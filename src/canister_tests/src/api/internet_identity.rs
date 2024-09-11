use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::archive::types::BufferedEntry;
use internet_identity_interface::internet_identity::types;
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid, call_candid_as, query_candid, query_candid_as, CallError, PocketIc};

/// The experimental v2 API
pub mod api_v2;

// API of verifiable credentials MVP.
pub mod vc_mvp;

/** The functions here are derived (manually) from Internet Identity's Candid file */

/// A fake "health check" method that just checks the canister is alive a well.
pub fn health_check(env: &PocketIc, canister_id: CanisterId) {
    let user_number: types::AnchorNumber = 0;
    // XXX: we use "IDLValue" because we're just checking that the canister is sending
    // valid data, but we don't care about the actual data.
    let _: () = call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "lookup",
        (user_number,),
    )
    .unwrap();
}

pub fn create_challenge(
    env: &PocketIc,
    canister_id: CanisterId,
) -> Result<types::Challenge, CallError> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "create_challenge",
        (),
    )
    .map(|(x,)| x)
}

pub fn register(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    device_data: &types::DeviceData,
    challenge_attempt: &types::ChallengeAttempt,
    temp_key: Option<Principal>,
) -> Result<types::RegisterResponse, CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "register",
        (device_data, challenge_attempt, temp_key),
    )
    .map(|(x,)| x)
}

pub fn prepare_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    frontend_hostname: &str,
    session_key: &types::SessionKey,
    max_time_to_live: Option<u64>,
) -> Result<(types::UserKey, types::Timestamp), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_delegation",
        (
            anchor_number,
            frontend_hostname,
            session_key,
            max_time_to_live,
        ),
    )
}

pub fn init_salt(env: &PocketIc, canister_id: CanisterId) -> Result<(), CallError> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "init_salt",
        (),
    )
}

pub fn get_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    frontend_hostname: &str,
    session_key: &types::SessionKey,
    timestamp: u64,
) -> Result<types::GetDelegationResponse, CallError> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_delegation",
        (anchor_number, frontend_hostname, session_key, timestamp),
    )
    .map(|(x,)| x)
}

pub fn get_principal(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    frontend_hostname: &str,
) -> Result<Principal, CallError> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_principal",
        (anchor_number, frontend_hostname),
    )
    .map(|(x,)| x)
}

pub fn lookup(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor_number: types::AnchorNumber,
) -> Result<Vec<types::DeviceData>, CallError> {
    query_candid(env, canister_id, "lookup", (anchor_number,)).map(|(x,)| x)
}

pub fn get_anchor_credentials(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor_number: types::AnchorNumber,
) -> Result<types::AnchorCredentials, CallError> {
    query_candid(env, canister_id, "get_anchor_credentials", (anchor_number,)).map(|(x,)| x)
}

pub fn add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    device_data: &types::DeviceData,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "add",
        (anchor_number, device_data),
    )
}

pub fn update(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    device_key: &types::PublicKey,
    device_data: &types::DeviceData,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "update",
        (anchor_number, device_key, device_data),
    )
}

pub fn replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    device_key: &types::PublicKey,
    device_data: &types::DeviceData,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "replace",
        (anchor_number, device_key, device_data),
    )
}

pub fn remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    device_key: &types::PublicKey,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "remove",
        (anchor_number, device_key),
    )
}

pub fn get_anchor_info(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
) -> Result<types::IdentityAnchorInfo, CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "get_anchor_info",
        (anchor_number,),
    )
    .map(|(x,)| x)
}

pub fn enter_device_registration_mode(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
) -> Result<types::Timestamp, CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "enter_device_registration_mode",
        (anchor_number,),
    )
    .map(|(x,)| x)
}

pub fn exit_device_registration_mode(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "exit_device_registration_mode",
        (anchor_number,),
    )
}

pub fn add_tentative_device(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor_number: types::AnchorNumber,
    device_data: &types::DeviceData,
) -> Result<types::AddTentativeDeviceResponse, CallError> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "add_tentative_device",
        (anchor_number, device_data),
    )
    .map(|(x,)| x)
}

pub fn verify_tentative_device(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    verification_code: &str,
) -> Result<types::VerifyTentativeDeviceResponse, CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "verify_tentative_device",
        (anchor_number, verification_code),
    )
    .map(|(x,)| x)
}

pub fn deploy_archive(
    env: &PocketIc,
    canister_id: CanisterId,
    wasm: &Vec<u8>,
) -> Result<types::DeployArchiveResult, CallError> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "deploy_archive",
        (wasm,),
    )
    .map(|(x,)| x)
}

pub fn stats(
    env: &PocketIc,
    canister_id: CanisterId,
) -> Result<types::InternetIdentityStats, CallError> {
    query_candid(env, canister_id, "stats", ()).map(|(x,)| x)
}

pub fn fetch_entries(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
) -> Result<Vec<BufferedEntry>, CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "fetch_entries",
        (),
    )
    .map(|(x,)| x)
}

pub fn acknowledge_entries(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    sequence_number: u64,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "acknowledge_entries",
        (sequence_number,),
    )
}

pub fn config(
    env: &PocketIc,
    canister_id: CanisterId,
) -> Result<types::InternetIdentityInit, CallError> {
    call_candid(env, canister_id, RawEffectivePrincipal::None, "config", ()).map(|(x,)| x)
}

/// A "compatibility" module for the previous version of II to handle API changes.
pub mod compat {}
