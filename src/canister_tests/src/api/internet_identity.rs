//! The functions here are derived (manually) from Internet Identity's Candid file
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::archive::types::BufferedEntry;
use internet_identity_interface::internet_identity::types::{
    self, CredentialId, DeviceKeyWithAnchor, IdentityNumber, OpenIdCredentialKey,
};
use pocket_ic::common::rest::{RawEffectivePrincipal, RawSenderInfo};
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

pub fn discover_sso(
    env: &PocketIc,
    canister_id: CanisterId,
    domain: &str,
) -> Result<(), RejectResponse> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "discover_sso",
        (domain,),
    )
}

pub fn get_sso_discovery(
    env: &PocketIc,
    canister_id: CanisterId,
    domain: &str,
) -> Result<types::SsoDiscoveryStatus, RejectResponse> {
    get_sso_discovery_for_origin(env, canister_id, domain, None)
}

/// Like [`get_sso_discovery`] but supplies the target dapp origin so the
/// resolved config reports the per-origin `resolved_client_id`.
pub fn get_sso_discovery_for_origin(
    env: &PocketIc,
    canister_id: CanisterId,
    domain: &str,
    origin: Option<&str>,
) -> Result<types::SsoDiscoveryStatus, RejectResponse> {
    let request = types::GetSsoDiscoveryStatusRequest {
        org_domain: domain.to_string(),
        target_app_origin: origin.map(str::to_string),
    };
    query_candid(env, canister_id, "get_sso_discovery_status", (request,)).map(|(x,)| x)
}

#[allow(clippy::too_many_arguments)]
pub fn sso_prepare_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
    discovery_domain: &str,
    origin: &str,
) -> Result<
    types::OpenIdResult<types::SsoPrepareDelegationResponse, types::OpenIdDelegationError>,
    RejectResponse,
> {
    let request = types::SsoPrepareDelegationRequest {
        jwt: jwt.to_string(),
        salt: *salt,
        session_key: session_key.clone(),
        org_domain: discovery_domain.to_string(),
        target_app_origin: origin.to_string(),
    };
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "sso_prepare_delegation",
        (request,),
    )
    .map(|(x,)| x)
}

#[allow(clippy::too_many_arguments)]
pub fn sso_get_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
    expiration: &types::Timestamp,
    discovery_domain: &str,
    origin: &str,
    sso_attr_bundle: &[u8],
) -> Result<
    types::OpenIdResult<types::SsoGetDelegationResponse, types::OpenIdDelegationError>,
    RejectResponse,
> {
    let request = types::SsoGetDelegationRequest {
        jwt: jwt.to_string(),
        salt: *salt,
        session_key: session_key.clone(),
        expiration: *expiration,
        org_domain: discovery_domain.to_string(),
        target_app_origin: origin.to_string(),
        sso_attr_bundle: serde_bytes::ByteBuf::from(sso_attr_bundle.to_vec()),
    };
    query_candid_as(env, canister_id, sender, "sso_get_delegation", (request,)).map(|(x,)| x)
}

/// Collapse an [`OpenIdResult`](types::OpenIdResult) to a plain `Result` for
/// the OpenID JWT test helpers. They only ever drive configured providers
/// (Google / Microsoft / Apple), whose JWKs are always warm, so `Pending` —
/// the SSO-discovery retry signal — cannot occur; treat it as a test failure.
pub(crate) fn settled<T, E>(result: types::OpenIdResult<T, E>) -> Result<T, E> {
    match result {
        types::OpenIdResult::Ok(value) => Ok(value),
        types::OpenIdResult::Err(error) => Err(error),
        types::OpenIdResult::Pending => {
            panic!("OpenID JWT call returned Pending for a configured provider")
        }
    }
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
    openid_prepare_delegation_with_discovery(env, canister_id, sender, jwt, salt, session_key, None)
        .map(settled)
}

/// Like [`openid_prepare_delegation`] but takes the SSO discovery domain and
/// returns the raw [`OpenIdResult`](types::OpenIdResult) so callers exercising
/// the SSO path can observe (and poll on) the `Pending` cache-warming arm.
pub fn openid_prepare_delegation_with_discovery(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
    discovery_domain: Option<&str>,
) -> Result<
    types::OpenIdResult<types::OpenIdPrepareDelegationResponse, types::OpenIdDelegationError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_prepare_delegation",
        (jwt, salt, session_key, discovery_domain),
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
    openid_get_delegation_with_discovery(
        env,
        canister_id,
        sender,
        jwt,
        salt,
        session_key,
        expiration,
        None,
    )
    .map(settled)
}

/// Like [`openid_get_delegation`] but takes the SSO discovery domain and returns
/// the raw [`OpenIdResult`](types::OpenIdResult) so SSO-path callers can observe
/// the `Pending` arm.
#[allow(clippy::too_many_arguments)]
pub fn openid_get_delegation_with_discovery(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    jwt: &str,
    salt: &[u8; 32],
    session_key: &types::SessionKey,
    expiration: &types::Timestamp,
    discovery_domain: Option<&str>,
) -> Result<
    types::OpenIdResult<types::SignedDelegation, types::OpenIdDelegationError>,
    RejectResponse,
> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "openid_get_delegation",
        (jwt, salt, session_key, expiration, discovery_domain),
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
    openid_credential_add_with_discovery(env, canister_id, sender, identity_number, jwt, salt, None)
        .map(settled)
}

/// Like [`openid_credential_add`] but takes the SSO discovery domain and returns
/// the raw [`OpenIdResult`](types::OpenIdResult) so SSO-path callers can observe
/// the `Pending` arm.
pub fn openid_credential_add_with_discovery(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: Option<&str>,
) -> Result<types::OpenIdResult<(), types::OpenIdCredentialAddError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_credential_add",
        (identity_number, jwt, salt, discovery_domain),
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

pub fn prepare_icrc3_attributes(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::PrepareIcrc3AttributeRequest,
) -> Result<
    Result<
        types::attributes::PrepareIcrc3AttributeResponse,
        types::attributes::PrepareIcrc3AttributeError,
    >,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_icrc3_attributes",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn get_icrc3_attributes(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::GetIcrc3AttributeRequest,
) -> Result<
    Result<types::attributes::GetIcrc3AttributeResponse, types::attributes::GetIcrc3AttributeError>,
    RejectResponse,
> {
    query_candid_as(env, canister_id, sender, "get_icrc3_attributes", (request,)).map(|(x,)| x)
}

/// Like [`prepare_icrc3_attributes`], but attaches a `sender_info` (SSO
/// attribute bundle `info` + `signer`) to the call.
///
/// PocketIC does not verify the `sender_info` canister signature, so only the
/// canister-observable checks (signer, expiry, origin) are exercised here.
#[allow(clippy::too_many_arguments)]
pub fn prepare_icrc3_attributes_with_bundle(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::PrepareIcrc3AttributeRequest,
    bundle_info: &[u8],
    signer: Principal,
) -> Result<
    Result<
        types::attributes::PrepareIcrc3AttributeResponse,
        types::attributes::PrepareIcrc3AttributeError,
    >,
    RejectResponse,
> {
    let payload = candid::encode_one(request).expect("encode PrepareIcrc3AttributeRequest");
    let sender_info = RawSenderInfo {
        info: bundle_info.to_vec(),
        signer: signer.as_slice().to_vec(),
    };
    let bytes = env.update_call_with_sender_info(
        canister_id,
        sender,
        "prepare_icrc3_attributes",
        payload,
        sender_info,
    )?;
    let (result,) = candid::decode_args(&bytes).expect("decode prepare_icrc3_attributes response");
    Ok(result)
}

pub type ListAvailableAttributesResult =
    Result<Vec<(String, Vec<u8>)>, types::attributes::ListAvailableAttributesError>;

pub fn list_available_attributes(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::ListAvailableAttributesRequest,
) -> Result<ListAvailableAttributesResult, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "list_available_attributes",
        (request,),
    )
    .map(|(x,)| x)
}

/// Like [`list_available_attributes`], but attaches a `sender_info` (SSO
/// attribute bundle `info` + `signer`) to the query. See
/// [`prepare_icrc3_attributes_with_bundle`] for the PocketIC signature caveat.
pub fn list_available_attributes_with_bundle(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: types::attributes::ListAvailableAttributesRequest,
    bundle_info: &[u8],
    signer: Principal,
) -> Result<ListAvailableAttributesResult, RejectResponse> {
    let payload = candid::encode_one(request).expect("encode ListAvailableAttributesRequest");
    let sender_info = RawSenderInfo {
        info: bundle_info.to_vec(),
        signer: signer.as_slice().to_vec(),
    };
    let bytes = env.query_call_with_sender_info(
        canister_id,
        sender,
        "list_available_attributes",
        payload,
        sender_info,
    )?;
    let (result,) = candid::decode_args(&bytes).expect("decode list_available_attributes response");
    Ok(result)
}

// --- Email recovery ---

pub fn email_recovery_credential_prepare_add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    dns_input: types::email_challenge::EmailChallengeDnsInput,
) -> Result<
    Result<types::email_challenge::EmailChallenge, types::email_challenge::EmailChallengeError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "email_recovery_credential_prepare_add",
        (identity_number, dns_input),
    )
    .map(|(x,)| x)
}

pub fn email_challenge_status(
    env: &PocketIc,
    canister_id: CanisterId,
    nonce: &str,
) -> Result<types::email_challenge::EmailChallengeStatus, RejectResponse> {
    query_candid(env, canister_id, "email_challenge_status", (nonce,)).map(|(x,)| x)
}

pub fn email_challenge_diagnostics(
    env: &PocketIc,
    canister_id: CanisterId,
    nonce: &str,
) -> Result<Option<types::email_challenge::EmailChallengeDiagnostics>, RejectResponse> {
    query_candid(env, canister_id, "email_challenge_diagnostics", (nonce,)).map(|(x,)| x)
}

pub fn email_recovery_prepare_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    dns_input: types::email_challenge::EmailChallengeDnsInput,
    session_key: types::SessionKey,
) -> Result<
    Result<types::email_challenge::EmailChallenge, types::email_challenge::EmailChallengeError>,
    RejectResponse,
> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "email_recovery_prepare_delegation",
        (dns_input, session_key),
    )
    .map(|(x,)| x)
}

pub fn email_challenge_submit_dkim_leaf(
    env: &PocketIc,
    canister_id: CanisterId,
    arg: types::email_challenge::EmailChallengeSubmitDkimLeafArg,
) -> Result<Result<(), types::email_challenge::EmailChallengeError>, RejectResponse> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "email_challenge_submit_dkim_leaf",
        (arg,),
    )
    .map(|(x,)| x)
}

pub fn email_challenge_resolve_via_doh(
    env: &PocketIc,
    canister_id: CanisterId,
    nonce: &str,
) -> Result<Result<(), types::email_challenge::EmailChallengeError>, RejectResponse> {
    let arg = types::email_challenge::EmailChallengeResolveViaDohArg {
        nonce: nonce.to_string(),
    };
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "email_challenge_resolve_via_doh",
        (arg,),
    )
    .map(|(x,)| x)
}

pub fn email_recovery_get_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    args: types::email_recovery::EmailRecoveryGetDelegationArgs,
) -> Result<
    Result<types::SignedDelegation, types::email_challenge::EmailChallengeError>,
    RejectResponse,
> {
    query_candid(env, canister_id, "email_recovery_get_delegation", (args,)).map(|(x,)| x)
}

pub fn email_recovery_credential_remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    address: &str,
) -> Result<Result<(), types::email_challenge::EmailChallengeError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "email_recovery_credential_remove",
        (identity_number, address),
    )
    .map(|(x,)| x)
}

pub fn verified_email_prepare_add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    dns_input: types::email_challenge::EmailChallengeDnsInput,
) -> Result<
    Result<types::email_challenge::EmailChallenge, types::email_challenge::EmailChallengeError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "verified_email_prepare_add",
        (identity_number, dns_input),
    )
    .map(|(x,)| x)
}

pub fn verified_email_remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    address: &str,
) -> Result<Result<(), types::email_challenge::EmailChallengeError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "verified_email_remove",
        (identity_number, address),
    )
    .map(|(x,)| x)
}

pub fn smtp_request(
    env: &PocketIc,
    canister_id: CanisterId,
    request: &types::smtp::SmtpRequest,
) -> Result<types::smtp::SmtpResponse, RejectResponse> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "smtp_request",
        (request,),
    )
    .map(|(x,)| x)
}

/// A "compatibility" module for the previous version of II to handle API changes.
pub mod compat {}
