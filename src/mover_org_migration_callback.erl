-module(mover_org_migration_callback).

-include("mover.hrl").
-include_lib("moser/include/moser.hrl").

-export([
	 migration_start_worker_args/2,
	 migration_action/2,
	 migration_type/0,
	 supervisor/0,
	 error_halts_migration/0,
	 reconfigure_object/2
	 ]).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    {Guid, AuthzId, LastUpdatedBy, RawObject} = moser_acct_processor:get_parsed_org_object_by_name(AcctInfo, OrgName),
    moser_org_converter:insert_org(Guid, AuthzId, LastUpdatedBy, RawObject),
    [{ok, done}].

migration_type() ->
    <<"org_migration">>.

supervisor() ->
    mover_org_migrator_sup.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.

error_halts_migration() ->
    true.
