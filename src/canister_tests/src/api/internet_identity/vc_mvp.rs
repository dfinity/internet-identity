use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{call_candid_as, query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, GetIdAliasRequest, IdAliasCredentials, PrepareIdAliasError,
    PrepareIdAliasRequest, PreparedIdAlias,
};

pub fn prepare_id_alias(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    prepare_id_alias_req: PrepareIdAliasRequest,
) -> Result<Result<PreparedIdAlias, PrepareIdAliasError>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "prepare_id_alias",
        (prepare_id_alias_req,),
    )
    .map(|(x,)| x)
}

pub fn get_id_alias(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    get_id_alias_req: GetIdAliasRequest,
) -> Result<Result<IdAliasCredentials, GetIdAliasError>, CallError> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_id_alias",
        (get_id_alias_req,),
    )
    .map(|(x,)| x)
}
