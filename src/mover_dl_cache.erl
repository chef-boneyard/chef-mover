%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.

%% @oc Provides an interface to check and non-destructively set org migration state

-module(mover_dl_cache).

-behavior(gen_server).

% gen_server behavior
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% API
-export([start_link/1,
         set_org_down_mode/2,
         update_dl_state/1,
         fetch_dl_state/1]).

% TODO move to hrl
% TODO necessary wwwor is undefined valid for any field value and bool() is usable? 
-type maybe_bool() :: undefined | true | false.
-type target() :: {org, string()} | default | override.

-record(dl_state, {
            target :: target(),
            downtime_enabled = false :: maybe_bool(),
            couchdb_checksums :: maybe_bool(),
            couchdb_clients :: maybe_bool(),
            couchdb_cookbooks :: maybe_bool(),
            couchdb_environments :: maybe_bool(),
            couchdb_roles :: maybe_bool(),
            couchdb_data :: maybe_bool()}).
% end TODO

% Internal State
-record(state, {redis,
                dl_default :: #dl_state{},
                dl_override :: #dl_state{} } ).

%%
%% API
%%

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [Config]).

%% Update dl state for a given org, or default/override (resolved based on #dl_state.target)
-spec update_dl_state(DLState :: #dl_state{}) -> { ok, #dl_state{} } | { error, term() }.
update_dl_state(#dl_state{} = DLState) ->
    gen_server:call(?MODULE, {update_dl_state, DLState}).

%% Retrieve current state for the specified target.  If the target is an org, this 
%% will be merged in the sequence of default + org + override
-spec fetch_dl_state(Target :: target()) -> { ok, #dl_state{} } | { error, term() }.
fetch_dl_state(Target) -> 
    gen_server:call(?MODULE, {fetch_dl_state, Target}).

% Helper function to easily put an org into or out of downtime
-spec set_org_down_mode(Target :: target(), EnableDowntime :: boolean()) -> term().
set_org_down_mode(Target, EnableDowntime) -> 
    update_dl_state(#dl_state{ target = Target, downtime_enabled = EnableDowntime } ).

%% Callback Functions
init(_Config) -> 
   {ok, Redis} = eredis:start_link(), % TODO we'll need eredis:start_link/4 or /5
   Default = fetch_dl_state(default),
   Override = fetch_dl_state(override),
   {ok, #state{redis = Redis, dl_default = Default, dl_override = Override }}.

handle_call({update_dl_state, #dl_state{ target = Target} = DLState }, _From, #state{redis = _Redis} = State ) ->
    %% TODO record -> json mapping
    _Key = make_key(Target), % clear warning for now
    %% Result = eredis:q(Redis, [ "SET", make_key(Target), NewState ]),
    Result = DLState,
    {reply, Result, State};
handle_call({fetch_dl_state, Target}, _From, #state{dl_default = D, dl_override = O, redis = _Redis} = State ) ->
    %% TODO json -> record mapping
    %% {ok, NewDLState} = eredis:q(Redis, [ "GET", make_key(Target) ]),
    NewDLState = #dl_state{},
    FinalDLState = merge_dl_states(D, #dl_state{target = Target} = NewDLState, O),
    {reply, FinalDLState, State};
handle_call(_Request, _From, State ) ->
    {noreply, State}.

handle_cast(_Request, State) -> 
    {reply, ok, State}.

handle_info(_Info, State) -> 
    {reply, ok, State}.

terminate(Reason, State) -> 
    {Reason, State}.

code_change(_Old, State, _Extra) -> 
    {ok, State}.

%% Internal Functions
make_key(default) -> 
    "dl_default";
make_key(override) -> 
    "dl_override";
make_key({org, OrgName}) when is_binary(OrgName) ->
    make_key({org, binary_to_list(OrgName)});
make_key({org, OrgName}) when is_list(OrgName) ->
    lists:flatten(["dl_org_", OrgName]).

merge_dl_states(Default, New, Override) -> 
    State1 = merge_dl_state(Default, New),
    merge_dl_state(State1, Override).

%% Merges two #dl_state records, with the right taking priority over left.
merge_dl_state(#dl_state{target = Target,
                         downtime_enabled = Downtime, 
                         couchdb_checksums = Checksums,
                         couchdb_clients = Clients,
                         couchdb_cookbooks = Cookbooks,
                         couchdb_environments = Environments,
                         couchdb_roles = Roles,
                         couchdb_data = Data},
               #dl_state{
                         downtime_enabled = RDowntime,
                         couchdb_checksums = RChecksums,
                         couchdb_clients = RClients,
                         couchdb_cookbooks = RCookbooks,
                         couchdb_environments = REnvironments,
                         couchdb_roles = RRoles,
                         couchdb_data = RData}) -> 
    #dl_state{ target = Target,
               downtime_enabled = default_value(Downtime, RDowntime),
               couchdb_checksums = default_value(Checksums, RChecksums),
               couchdb_clients = default_value(Clients, RClients),
               couchdb_cookbooks = default_value(Cookbooks, RCookbooks),
               couchdb_environments = default_value(Environments, REnvironments),
               couchdb_roles = default_value(Roles, RRoles),
               couchdb_data = default_value(Data, RData)}.

default_value(undefined, undefined) ->
    undefined;
default_value(undefined, Right) -> 
    Right;
default_value(Left, undefined) -> 
    Left;
default_value(_Left, Right) -> 
    Right.
