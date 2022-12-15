use crate::framework;
use crate::framework::CallError;
use candid::Principal;
use ic_state_machine_tests::{CanisterId, PrincipalId, StateMachine};
use internet_identity_interface as types;
use serde_bytes::ByteBuf;

/** The functions here are derived (manually) from Internet Identity's Candid file */

/// A fake "health check" method that just checks the canister is alive a well.
pub fn health_check(env: &StateMachine, canister_id: CanisterId) {
    let user_number: types::UserNumber = 0;
    // XXX: we use "IDLValue" because we're just checking that the canister is sending
    // valid data, but we don't care about the actual data.
    let _: (candid::parser::value::IDLValue,) =
        framework::call_candid(env, canister_id, "lookup", (user_number,)).unwrap();
}

pub fn http_request(
    env: &StateMachine,
    canister_id: CanisterId,
    http_request: types::HttpRequest,
) -> Result<types::HttpResponse, CallError> {
    framework::query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}

pub fn create_challenge(
    env: &StateMachine,
    canister_id: CanisterId,
) -> Result<types::Challenge, CallError> {
    framework::call_candid(env, canister_id, "create_challenge", ()).map(|(x,)| x)
}

pub fn register(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    device_data: &types::DeviceData,
    challenge_attempt: types::ChallengeAttempt,
) -> Result<types::RegisterResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "register",
        (device_data, challenge_attempt),
    )
    .map(|(x,)| x)
}

pub fn prepare_delegation(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
    session_key: types::SessionKey,
    max_time_to_live: Option<u64>,
) -> Result<(types::UserKey, types::Timestamp), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "prepare_delegation",
        (
            user_number,
            frontend_hostname,
            session_key,
            max_time_to_live,
        ),
    )
}

pub fn init_salt(env: &StateMachine, canister_id: CanisterId) -> Result<(), CallError> {
    framework::call_candid(env, canister_id, "init_salt", ())
}

pub fn get_delegation(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
    session_key: types::SessionKey,
    timestamp: u64,
) -> Result<types::GetDelegationResponse, CallError> {
    framework::query_candid_as(
        env,
        canister_id,
        sender,
        "get_delegation",
        (user_number, frontend_hostname, session_key, timestamp),
    )
    .map(|(x,)| x)
}

pub fn get_principal(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
) -> Result<Principal, CallError> {
    framework::query_candid_as(
        env,
        canister_id,
        sender,
        "get_principal",
        (user_number, frontend_hostname),
    )
    .map(|(x,)| x)
}

pub fn lookup(
    env: &StateMachine,
    canister_id: CanisterId,
    user_number: types::UserNumber,
) -> Result<Vec<types::DeviceData>, CallError> {
    framework::query_candid(env, canister_id, "lookup", (user_number,)).map(|(x,)| x)
}

pub fn add(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_data: types::DeviceData,
) -> Result<(), CallError> {
    framework::call_candid_as(env, canister_id, sender, "add", (user_number, device_data))
}

pub fn update(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_key: types::PublicKey,
    device_data: types::DeviceData,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "update",
        (user_number, device_key, device_data),
    )
}

pub fn remove(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_key: types::PublicKey,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "remove",
        (user_number, device_key),
    )
}

pub fn get_anchor_info(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<types::IdentityAnchorInfo, CallError> {
    framework::call_candid_as(env, canister_id, sender, "get_anchor_info", (user_number,))
        .map(|(x,)| x)
}

pub fn enter_device_registration_mode(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<types::Timestamp, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "enter_device_registration_mode",
        (user_number,),
    )
    .map(|(x,)| x)
}

pub fn exit_device_registration_mode(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "exit_device_registration_mode",
        (user_number,),
    )
}

pub fn add_tentative_device(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_data: types::DeviceData,
) -> Result<types::AddTentativeDeviceResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "add_tentative_device",
        (user_number, device_data),
    )
    .map(|(x,)| x)
}

pub fn verify_tentative_device(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    verification_code: types::DeviceVerificationCode,
) -> Result<types::VerifyTentativeDeviceResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "verify_tentative_device",
        (user_number, verification_code),
    )
    .map(|(x,)| x)
}

pub fn deploy_archive(
    env: &StateMachine,
    canister_id: CanisterId,
    wasm: ByteBuf,
) -> Result<types::DeployArchiveResult, CallError> {
    framework::call_candid(env, canister_id, "deploy_archive", (wasm,)).map(|(x,)| x)
}

pub fn stats(
    env: &StateMachine,
    canister_id: CanisterId,
) -> Result<types::InternetIdentityStats, CallError> {
    framework::query_candid(env, canister_id, "stats", ()).map(|(x,)| x)
}

/// A "compatibility" module for the previous version of II to handle API changes.
pub mod compat {
    use super::*;
    use candid::{CandidType, Deserialize};

    #[derive(Clone, Debug, CandidType, Deserialize)]
    pub struct InternetIdentityStats {
        pub assigned_user_number_range: (types::UserNumber, types::UserNumber),
        pub users_registered: u64,
    }

    pub fn stats(
        env: &StateMachine,
        canister_id: CanisterId,
    ) -> Result<InternetIdentityStats, CallError> {
        framework::query_candid(env, canister_id, "stats", ()).map(|(x,)| x)
    }
}
