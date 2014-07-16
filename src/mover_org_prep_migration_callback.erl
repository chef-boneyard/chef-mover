-module(mover_org_prep_migration_callback).

-export([
	 migration_init/0,
	 migration_type/0,
	 supervisor/0,
	 migration_start_worker_args/2,
	 error_halts_migration/0,
	 reconfigure_object/2,
	 migration_action/2,
	 next_object/0
	]).

-include("mover.hrl").
-include_lib("moser/include/moser.hrl").

migration_init() ->
    % initialize a transient queue with all orgs, we will fail in migration_action
    % if the org has state for org_migration
    AcctInfo = moser_acct_processor:open_account(),
    AllOrgs = moser_acct_processor:all_orgs(AcctInfo),
    OrgNames = [ OrgName || #org_info{org_name=OrgName} <- AllOrgs],
    mover_transient_migration_queue:initialize_queue(?MODULE, OrgNames).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    OrgInfo = moser_acct_processor:expand_org_info(#org_info{org_name = OrgName, account_info = AcctInfo}),
    OrgId = OrgInfo#org_info.org_id,

    case moser_state_tracker:insert_one_org(OrgInfo, mover_org_migration_callback:migration_type()) of
	% org did not have org_migration set in state table,
	% darklaunch it and set org_migration to holding
	ok ->
	    moser_state_tracker:force_org_to_state(OrgName, mover_org_migration_callback:migration_type(), <<"holding">>),
	    ok;
	% org already exited for org_migration in state table, do nothing
	{error, {error, error, <<"23505">>, _Msg, _Detail}} ->
	    lager:warning([{org_name,OrgName}, {org_id, OrgId}], "org already exists, ignoring"),
	    ok;
	{error, Reason} ->
	    lager:error([{org_name,OrgName}, {org_id, OrgId}],
			"stopping - failed to capture state because: ~p", [Reason])
    end.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"org_prep_migration">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.
