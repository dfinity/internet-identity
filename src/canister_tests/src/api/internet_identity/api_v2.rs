use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{
    call_candid, call_candid_as, query_candid, CallError, StateMachine,
};
use internet_identity_interface::internet_identity::types::*;
use std::collections::HashMap;

pub fn captcha_create(
    env: &StateMachine,
    canister_id: CanisterId,
) -> Result<Result<Challenge, ()>, CallError> {
    call_candid(env, canister_id, "captcha_create", ()).map(|(x,)| x)
}

pub fn identity_register(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    authn_method: &AuthnMethodData,
    challenge_attempt: &ChallengeAttempt,
    temp_key: Option<Principal>,
) -> Result<Result<IdentityNumber, IdentityRegisterError>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_register",
        (authn_method, challenge_attempt, temp_key),
    )
    .map(|(x,)| x)
}

pub fn identity_authn_info(
    env: &StateMachine,
    canister_id: CanisterId,
    identity_number: IdentityNumber,
) -> Result<Result<IdentityAuthnInfo, ()>, CallError> {
    query_candid(env, canister_id, "identity_authn_info", (identity_number,)).map(|(x,)| x)
}

pub fn identity_info(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
) -> Result<Result<IdentityInfo, ()>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_info",
        (identity_number,),
    )
    .map(|(x,)| x)
}

pub fn identity_metadata_replace(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    metadata: &HashMap<String, MetadataEntryV2>,
) -> Result<Result<(), ()>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_metadata_replace",
        (identity_number, metadata),
    )
    .map(|(x,)| x)
}

pub fn authn_method_add(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    authn_method: &AuthnMethodData,
) -> Result<Result<(), AuthnMethodAddError>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "authn_method_add",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_remove(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
) -> Result<Result<(), ()>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "authn_method_remove",
        (identity_number, public_key),
    )
    .map(|(x,)| x)
}
