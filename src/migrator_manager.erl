%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc
%%
%% Our startup and replacement strategy is as follows:
%%
%% ALL orgs: org_names = undefined, org_count_remaining < 0
%%   Startup: Fetch max_worker_count orgs and start a worker for each.
%%   Ongoing: As each org is processed*, fetch next org from state
%%            tracker and start replacement worker. This ensures we're
%%            constantly running max_worker_count workers*
%%
%%            When state tracker returns no next org for processing,
%%            no more replacement workers will be started.
%%
%%            State returned to 'ready' when last worker terminates.
%%
%% COUNT orgs: org_names = undefined, org_count_remaining = Count
%%   Startup: As for "ALL orgs"
%%   Ongoing: As each org is processed*, fetch next org from state
%%            tracker and start replacement worker. Decrement
%%            max_org_count as orgs are processed.
%%
%%            Once max_org_count reaches 0, OR state
%%            tracker returns no more orgs (whichever comes first), no
%%            more replacement workers will be spawned and we will
%%            monitor remaining workers to completion.
%%
%%            State returned to 'ready' when last worker terminates.
%%
%% ORG NAME LIST: org_names != undefined
%%   Startup: Start one worker per org.
%%   Ongoing: Once all orgs have been processed* (and associated workers
%%            have died), update state to ready.  Because we start
%%            all workers up front, we do not
%%
%% Some notes:
%%
%% * in this context a 'processed' org means that we have attempted to
%%   migrate an org and its associated worker has died after a successful
%%   intialization.
%% * a risk in this implementation is that if startup of a worker FSM fails,
%%   we 'lose' that worker.  If we attempt to start 5 workers, and only
%%   4 succeed, we will AT most only ever have 4 workers.
%%
%%   Similarly if a worker fails to start after its predecessor goes
%%   down, we'll never recover that worker.
%%
%%   Scenarios:
%%    - out of memory, can't spawn process
%%    - lose DB link, can't persist state in FSM 'init'.
%%
%%   These are both fatal - while we can't halt inflight migrations, we
%%   is it worth it to stop new ones from starting once this occurS?
%%
%% @end

-module(migrator_manager).

-compile([{parse_transform, lager_transform}]).

-record(state, { org_names,
                 live_worker_count = 0,
                 max_worker_count = 0,
                 org_count_remaining = 0, % Number of orgs left to
                                          %   attempt to migrate
                 halt = false % indicates a problem occurred and we
                              % should allow only in-flight migrations to
                              %  attempt to complete
             }).


% API Exports
-export([ start_link/0,
          migrate/1,
          migrate/2 ]).

%% gen_fsm callbacks
-export([ init/1,
          handle_event/3,
          handle_sync_event/4,
          handle_info/3,
          terminate/3,
          code_change/4 ]).
%% states
-export([ ready/2,
          ready/3,
          starting/2,
          starting/3,
          working/3 ]).

-define(SERVER, ?MODULE).

-include_lib("moser/include/moser_state.hrl").

%%
%% API
%%

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate(OrgNames) ->
    gen_fsm:sync_send_event(?SERVER, {start, OrgNames}).

migrate(all, NumWorkers) ->
    gen_fsm:sync_send_event(?SERVER, {start, -1, NumWorkers});
migrate(NumOrgs, NumWorkers) when NumWorkers > NumOrgs ->
    {error, more_workers_than_orgs};
migrate(NumOrgs, NumWorkers) ->
    gen_fsm:sync_send_event(?SERVER, {start, NumOrgs, NumWorkers}).

init([]) ->
    { ok, ready, #state{}, 0 }.

%%
%% States
%%

ready(timeout, State) ->
    {next_state, ready, State}.

ready({start, NumOrgs, NumWorkers}, _From, State) ->
    State1 = State#state{ org_names = undefined,
                          max_worker_count = NumWorkers,
                          org_count_remaining = NumOrgs },

    {reply, {ok, starting}, starting, State1, 0};
ready({start, OrgNames}, _From, State) ->
    State1 = State#state{ org_names = OrgNames,
                          max_worker_count = 0,
                          org_count_remaining = 0 },
    {reply, {ok, starting}, starting, State1, 0}.

starting(timeout, #state { org_names = undefined,
                           max_worker_count = MaxWorkers } = State) ->
    {ok, OrgList} = moser_state_tracker:fetch_pending_org_batch(MaxWorkers),
    State1 = start_org_migrators(OrgList, State),
    next_state_for_workers(State1);
starting(timeout, #state { org_names = OrgNames } = State) ->
    {ok, OrgList} = moser_state_tracker:fetch_states_by_names(OrgNames),
    State1 = start_org_migrators(OrgList, State),
    next_state_for_workers(State1).

starting({start, _, _}, _From, State) ->
    {reply, {error, busy}, starting, State};
starting({start, _}, _From, State) ->
    {reply, {error, busy}, starting, State}.

working({start, _}, _From, State) ->
    {reply, {error, busy}, working, State};
working({start, _, _}, _From, State) ->
    {reply, {error, busy}, working, State}.

handle_info({'DOWN', _MRef, process, _Pid, _Reason}, _FSMState,
        #state{live_worker_count = Live} = State) ->
    worker_down(State#state{live_worker_count = (Live - 1) });
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%
%%% Internal Functions
%%%
start_org_migrators([], State) ->
    State;
start_org_migrators([O|OrgList], State) ->
    State1 = start_org_migrator(O, State),
    start_org_migrators(OrgList, State1).

start_org_migrator_for_next_org(State) ->
    case moser_state_tracker:next_pending_org() of
        not_found -> State;
        {ok, OrgState} -> start_org_migrator(OrgState, State)
    end.

start_org_migrator(OrgState, #state{live_worker_count = Live,
                                    org_count_remaining = Count} = State) ->
    case mover_org_migrator_sup:start_org_migrator(OrgState) of
        {ok, Pid} ->
            monitor(process, Pid),
            State#state{ live_worker_count = (Live + 1),
                         org_count_remaining = (Count - 1) };
        _AnythingElse ->
            lager:error(?MS_LOG_META(OrgState), "Failed to start migrator!"),
            State#state{ halt = true }
    end.

% failure occurred in trying to start a worker
% Allow inflight orgs to complete if possible, but do not start
% more workers.
worker_down(#state{halt = true} = State) ->
    next_state_for_workers(State);
% we're working from a list of names - all workers started in this case.
worker_down(#state{max_worker_count = 0} = State) ->
    next_state_for_workers(State);
% "all orgs" - keep spawning replcaement workers until no more orgs
% are returned from moser_state_tracker
worker_down(#state{org_names = undefined,
                   org_count_remaining = Count} = State) when Count < 0 ->
    State2 = start_org_migrator_for_next_org(State),
    next_state_for_workers(State2);
% We have processed all orgs we were asked to - we're done.
worker_down(#state{org_names = undefined,
                   org_count_remaining = 0} = State) ->
    next_state_for_workers(State);
% Keep replacing workers until we have processed the number of orgs
% we've been asked to process
worker_down(#state{org_names = undefined,
                   org_count_remaining = _Remain} = State) ->
    State2 = start_org_migrator_for_next_org(State),
    next_state_for_workers(State2).

% Next state is driven by the number of live workers remaining
% If it's zero, we go back to 'ready'
next_state_for_workers(#state{live_worker_count = 0} = State) ->
    % Workers are done, so is our work. Back to ready state.
    lager:info("Workers are done, returning to 'ready' state"),
    {next_state, ready, State, 0};
next_state_for_workers(#state{live_worker_count = Live} = State) ->
    lager:info("Current live worker count: ~p", [Live]),
    % This org is done, but we still have more to do. Keep waiting for
    % the other workers.
    {next_state, working, State}.

