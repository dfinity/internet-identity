//! The functions here are derived (manually) from Internet Identity's Candid file
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::archive::types::BufferedEntry;
use internet_identity_interface::internet_identity::types::{
    self, CredentialId, DeviceKeyWithAnchor, IdentityNumber, OpenIdCredentialKey,
};
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{
    call_candid, call_candid_as, query_candid, query_candid_as, PocketIc, RejectResponse,
};

/// The experimental v2 API
pub mod api_v2;

// API of verifiable credentials MVP.
pub mod vc_mvp;

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
) -> Result<types::Challenge, RejectResponse> {
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
) -> Result<types::RegisterResponse, RejectResponse> {
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
) -> Result<(types::UserKey, types::Timestamp), RejectResponse> {
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

pub fn init_salt(env: &PocketIc, canister_id: CanisterId) -> Result<(), RejectResponse> {
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
) -> Result<types::GetDelegationResponse, RejectResponse> {
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
) -> Result<Principal, RejectResponse> {
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
) -> Result<Vec<types::DeviceData>, RejectResponse> {
    query_candid(env, canister_id, "lookup", (anchor_number,)).map(|(x,)| x)
}

pub fn get_anchor_credentials(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor_number: types::AnchorNumber,
) -> Result<types::AnchorCredentials, RejectResponse> {
    query_candid(env, canister_id, "get_anchor_credentials", (anchor_number,)).map(|(x,)| x)
}

pub fn add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: types::AnchorNumber,
    device_data: &types::DeviceData,
) -> Result<(), RejectResponse> {
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
) -> Result<(), RejectResponse> {
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
) -> Result<(), RejectResponse> {
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
) -> Result<(), RejectResponse> {
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
) -> Result<types::IdentityAnchorInfo, RejectResponse> {
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
) -> Result<types::Timestamp, RejectResponse> {
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
) -> Result<(), RejectResponse> {
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
) -> Result<types::AddTentativeDeviceResponse, RejectResponse> {
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
) -> Result<types::VerifyTentativeDeviceResponse, RejectResponse> {
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
) -> Result<types::DeployArchiveResult, RejectResponse> {
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
) -> Result<types::InternetIdentityStats, RejectResponse> {
    query_candid(env, canister_id, "stats", ()).map(|(x,)| x)
}

pub fn fetch_entries(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
) -> Result<Vec<BufferedEntry>, RejectResponse> {
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
) -> Result<(), RejectResponse> {
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
) -> Result<types::InternetIdentityInit, RejectResponse> {
    call_candid(env, canister_id, RawEffectivePrincipal::None, "config", ()).map(|(x,)| x)
}

pub fn openid_prepare_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
) -> Result<
    Result<types::OpenIdPrepareDelegationResponse, types::OpenIdDelegationError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_prepare_delegation",
        (jwt, salt, session_key),
    )
    .map(|(x,)| x)
}

pub fn openid_get_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
    expiration: &types::Timestamp,
) -> Result<Result<types::SignedDelegation, types::OpenIdDelegationError>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "openid_get_delegation",
        (jwt, salt, session_key, expiration),
    )
    .map(|(x,)| x)
}

pub fn openid_credential_add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    jwt: &str,
    salt: &[u8; 32],
) -> Result<Result<(), types::OpenIdCredentialAddError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_credential_add",
        (identity_number, jwt, salt),
    )
    .map(|(x,)| x)
}

pub fn openid_credential_remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    openid_credential_key: &OpenIdCredentialKey,
) -> Result<Result<(), types::OpenIdCredentialRemoveError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_credential_remove",
        (identity_number, openid_credential_key),
    )
    .map(|(x,)| x)
}
pub fn lookup_device_key(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    credential_id: &CredentialId,
) -> Result<Option<DeviceKeyWithAnchor>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "lookup_device_key",
        (credential_id,),
    )
    .map(|(x,)| x)
}

pub fn prepare_attributes(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::PrepareAttributeRequest,
) -> Result<
    Result<types::attributes::PrepareAttributeResponse, types::attributes::PrepareAttributeError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_attributes",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn get_attributes(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::GetAttributesRequest,
) -> Result<
    Result<types::attributes::CertifiedAttributes, types::attributes::GetAttributesError>,
    RejectResponse,
> {
    query_candid_as(env, canister_id, sender, "get_attributes", (request,)).map(|(x,)| x)
}

/// A "compatibility" module for the previous version of II to handle API changes.
pub mod compat {}
