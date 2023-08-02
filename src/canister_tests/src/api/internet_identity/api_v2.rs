use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{call_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodAddResponse, AuthnMethodData, IdentityInfoResponse, IdentityMetadataReplaceResponse,
    IdentityNumber, MetadataEntry,
};
use std::collections::HashMap;

pub fn identity_info(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
) -> Result<Option<IdentityInfoResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_info",
        (identity_number,),
    )
    .map(|(x,)| x)
}

pub fn authn_method_add(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    authn_method: &AuthnMethodData,
) -> Result<Option<AuthnMethodAddResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "authn_method_add",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn identity_metadata_replace(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    metadata: &HashMap<String, MetadataEntry>,
) -> Result<Option<IdentityMetadataReplaceResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_metadata_replace",
        (identity_number, metadata),
    )
    .map(|(x,)| x)
}
