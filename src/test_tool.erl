%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc  Just some things I got tired of retyping
%%

-module(test_tool).

-export([reset_orgs/1]).

-include_lib("moser/include/moser_state.hrl").

% Fetch orgs of a given state
% Nuke any sql records for them
% Reset their persisted state to pending
reset_orgs(State) ->
    {ok, Orgs} = moser_state_tracker:fetch_state_list(State, 1000000, 0),
    full_reset_orgs(Orgs).

full_reset_orgs([]) ->
    ok;
full_reset_orgs( [#migration_state{org_id = OrgId, org_name = Name} |T]) ->
    moser_chef_converter:cleanup_orgid(OrgId),
    moser_state_tracker:reset_state(Name),
    full_reset_orgs(T).
