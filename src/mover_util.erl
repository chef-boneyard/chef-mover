%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright (C) 2013, Opscode Inc.
%%
%% @doc
%% Some simple org migration utilities.
%% @end

-module(mover_util).

-export([reset_org/1,
         reset_orgs/1,
         reset_orgs_from_file/1]).

%% @doc delete any SQL data for the named org and reset its state
%% to indicate it's ready to migrate.
reset_org(OrgName) when is_list(OrgName) ->
    reset_org(iolist_to_binary(OrgName));
reset_org(OrgName) when is_binary(OrgName) ->
    moser_state_tracker:ready_migration(OrgName),
    Acct = moser_acct_processor:open_account(),
    GUID = moser_acct_processor:get_org_guid_by_name(OrgName, Acct),
    moser_chef_converter:cleanup_orgid(GUID).

%% @doc Reset each org in the provided list of orgs.
reset_orgs(Orgs) when is_list(Orgs) ->
    [ reset_org(X) || X <- Orgs ].

%% @doc Reset each org in the provided file, which must contain
%% one org per line.
reset_orgs_from_file(FileName) ->
    {ok, Dev} = file:open(FileName, [read]),
    process_lines(Dev, fun reset_org/1).

process_lines(Dev, Processor) ->
    case io:get_line(Dev, "") of
        eof ->
            file:close(Dev),
            ok;
        "\n" ->
            process_lines(Dev, Processor);
        Line ->
            Processor(string:strip(Line, right, $\n)),
            process_lines(Dev, Processor)
    end.
