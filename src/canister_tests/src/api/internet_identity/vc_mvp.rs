use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{call_candid_as, query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::attribute_sharing_mvp::{
    GetIdAliasResponse, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::IdentityNumber;

pub fn prepare_id_alias(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    relying_party: &str,
    issuer: &str,
) -> Result<Option<PrepareIdAliasResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "prepare_id_alias",
        (identity_number, relying_party, issuer),
    )
    .map(|(x,)| x)
}

pub fn get_id_alias(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    relying_party: &str,
    issuer: &str,
) -> Result<Option<GetIdAliasResponse>, CallError> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_id_alias",
        (identity_number, relying_party, issuer),
    )
    .map(|(x,)| x)
}
