%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that migrates a single org from end to end.
%%


-module(mover_org_migrator).
-behaviour(gen_fsm).

-compile([{parse_transform, lager_transform}]).

% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

% states
-export([disable_org_access/2,
         migrate_org/2,
         verify_org/2,
         set_org_to_sql/2,
         enable_org_access/2,
         complete_migration/2,
         abort_migration/2 ]).

% api
-export([start_link/1]).

-include_lib("moser/include/moser_state.hrl").

-define(PHASE_2_MIGRATION_COMPONENTS,
            [checksums, cookbooks, environments, roles, data]).

-record(state, { account_info :: term(),   % TODO #account_info for moser, provided via init config
                 ms :: #migration_state{}, % migration state persistance record
                 start_time :: term(),     % timestamp indicating start of migration operation
                 error :: term()           % the error that has placed us in failure state.
               }).

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init(#migration_state{} = MS) ->
    MS2 = persist_state(MS#migration_state{state = migrating}),
    {ok, disable_org_access, #state{ms = MS2, start_time = os:timestamp()}, 0}.


disable_org_access(timeout, #state{ms = MS} = State) ->
    lager:info(?MS_LOG_META(MS), "Placing organization into maintenance mode", []),
    case mover_org_darklaunch:disable_org(MS) of
        {ok, MS2} ->
            {next_state, migrate_org, State#state{ms = MS2}, 0};
        {{error, Error}, MS2} ->
            MS3 = persist_state(MS#migration_state{state = pending}),
            lager:error(?MS_LOG_META(MS2),
                        "Failed to place org into maintenance mode, skipping it: ~p", [Error]),
            {stop, {error, Error}, State#state{ms = MS3}}
    end.

migrate_org(timeout, #state{ms = MS} = State) ->
    lager:info(?MS_LOG_META(MS), "Migrating organization data", []),
    case moser_converter:convert_org(MS) of
        [{ok, _}] ->
            {next_state, verify_org, State, 0};
        {error, Error} ->
            lager:error(?MS_LOG_META(MS), "Failed to migrate org: ~p", [Error]),
            {next_state, abort_migration, #state{error = Error} = State, 0}
    end.

verify_org(timeout, #state{ms = MS} = State) ->
    lager:info(?MS_LOG_META(MS), "Validating organizaton migration (placeholder)", []),
    % TODO do we have any validation we can run?
    % case moser_validator:validate_org(MS) of
    case ok of
        ok ->
            {next_state, set_org_to_sql, State, 0};
        {error, Error} ->
            lager:error(?MS_LOG_META(MS), "Failed to verify org: ~p", [Error]),
            {next_state, abort_migration, #state{error = Error} = State, 0}
    end.


set_org_to_sql(timeout, #state{ms = MS} = State) ->
    lager:info(?MS_LOG_META(MS), "Setting organization to SQL mode", []),
    case mover_org_darklaunch:org_to_sql(MS, ?PHASE_2_MIGRATION_COMPONENTS) of
        {ok, MS2} ->
            {next_state, enable_org_access, State#state{ms = MS2}, 0};
        {{error, Error}, MS2} ->
            lager:error(?MS_LOG_META(MS2), "Failed to update org darklaunch to sql: ~p", [Error]),
            {next_state, abort_migration, #state{error = Error, ms = MS2} = State, 0}
    end.

enable_org_access(timeout, #state{ms = MS, start_time = Start} = State) ->
    lager:info(?MS_LOG_META(MS), "Removing organization from maintenance mode, enabling access", []),
    case mover_org_darklaunch:enable_org(MS) of
        {ok, MS2} ->
            {next_state, complete_migration, State#state{ms = MS2}, 0};
        {{error, Error}, MS2} ->
            % Everything's done but we couldn't un-503 the org. Don't
            % undo the migration, it won't help. This needs
            % intervention...
            Time = timer:now_diff(os:timestamp(), Start),
            MS3 = persist_state(MS2#migration_state{state = disabled,
                                                    migration_duration = Time}),
            lager:error(?MS_LOG_META(MS2),  "Failed to take org out of maint mode: ~p", [Error]),
            {stop, {error, Error}, State#state{ms = MS3}}
    end.

complete_migration(timeout, #state{ms = MS, start_time = Start} = State) ->
    lager:info(?MS_LOG_META(MS), "Migration completed", []),
    StateTime = timer:now_diff(os:timestamp(), Start),
    Time = moser_utils:us_to_secs(StateTime),
    MS2 = persist_state(MS#migration_state{state = complete,
                                           migration_duration = StateTime}),
    lager:info(?MS_LOG_META(MS2), "Migration completed successfully in ~.3f seconds.", [Time]),
    {stop, normal, State#state{ms = MS2}}.

abort_migration(timeout, #state{ms = MS, error = Error} = State) ->
    MS2 = persist_state(MS#migration_state{state = aborted}),
    lager:error(?MS_LOG_META(MS2), "Migration aborted, rolling back", []),
    {stop, {shutdown, {error, Error}}, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% Internal
%%
persist_state(#migration_state{} = MS) ->
    {_, MS2} = moser_state_tracker:update_state(MS),
    MS2.

