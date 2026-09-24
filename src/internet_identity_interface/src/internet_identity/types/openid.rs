use crate::internet_identity::types::{
    AnchorNumber, FrontendHostname, MetadataEntryV2, SessionKey, SignedDelegation, Timestamp,
    UserKey,
};
use candid::{CandidType, Deserialize, Principal};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

/// Types for OpenID credentials, used for OpenID sign in

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct OpenIdCredentialData {
    pub iss: String,
    pub sub: String,
    pub aud: String,
    // Must be optional to differentiate between linked anchor and used anchor,
    // it's not possible to keep track of registrations in bookkeeping
    // authn method stats if this value is already set to any value.
    pub last_usage_timestamp: Option<Timestamp>,
    pub metadata: HashMap<String, MetadataEntryV2>,
    /// SSO discovery domain this credential was verified through. `None` for
    /// direct-provider credentials (Google / Apple / Microsoft).
    pub sso_domain: Option<String>,
    /// Human-readable SSO name served alongside `client_id` at
    /// `{sso_domain}/.well-known/ii-openid-configuration`. `None` when
    /// the domain doesn't publish one — callers that want a label should
    /// fall back to `sso_domain` on the frontend side.
    pub sso_name: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum OpenIdCredentialAddError {
    Unauthorized(Principal),
    JwtVerificationFailed,
    OpenIdCredentialAlreadyRegistered,
    InternalCanisterError(String),
    JwtExpired,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum OpenIdCredentialRemoveError {
    Unauthorized(Principal),
    OpenIdCredentialNotFound,
    InternalCanisterError(String),
}

#[derive(CandidType, Debug, Deserialize)]
pub struct OpenIdPrepareDelegationResponse {
    pub user_key: UserKey,
    pub expiration: Timestamp,
    pub anchor_number: AnchorNumber,
}

/// Request for `sso_prepare_delegation`.
#[derive(CandidType, Debug, Deserialize)]
pub struct SsoPrepareDelegationRequest {
    pub jwt: String,
    pub salt: [u8; 32],
    pub session_key: SessionKey,
    pub org_domain: String,
    pub target_app_origin: FrontendHostname,
}

/// Request for `sso_get_delegation`.
#[derive(CandidType, Debug, Deserialize)]
pub struct SsoGetDelegationRequest {
    pub jwt: String,
    pub salt: [u8; 32],
    pub session_key: SessionKey,
    pub expiration: Timestamp,
    pub org_domain: String,
    pub target_app_origin: FrontendHostname,
    pub sso_attr_bundle: ByteBuf,
}

/// Response of `sso_prepare_delegation`.
#[derive(CandidType, Debug, Deserialize)]
pub struct SsoPrepareDelegationResponse {
    pub user_key: UserKey,
    pub expiration: Timestamp,
    pub anchor_number: AnchorNumber,
    /// SSO attribute bundle message bytes (encodes `sso_domain`, `origin`, `expiry`).
    pub sso_attr_bundle: ByteBuf,
}

/// Response of `sso_get_delegation`.
#[derive(CandidType, Debug, Deserialize)]
pub struct SsoGetDelegationResponse {
    pub signed_delegation: SignedDelegation,
    /// Canister signature over the `sso_attr_bundle` message.
    pub sso_attr_bundle_signature: ByteBuf,
}

#[derive(CandidType, Debug, Deserialize, Serialize)]
pub enum OpenIdDelegationError {
    NoSuchAnchor,
    NoSuchDelegation,
    JwtVerificationFailed,
    JwtExpired,
}

/// Result of an OpenID call whose verification may need SSO discovery / JWKS
/// that isn't cached yet.
///
/// `Pending` is a retry signal, not an error: the canister kicked off the
/// fetch in the background, so the caller should poll the same method again
/// shortly. It is only ever returned for SSO discovery domains — configured
/// providers (Google / Microsoft / Apple) keep their keys warm and resolve to
/// `Ok` or `Err` directly.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum OpenIdResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}

/// Map a settled `Result` onto an `OpenIdResult`. Callers that have already
/// handled the `Pending` retry signal upstream can collapse the final
/// `Ok`/`Err` match into `result.into()`.
impl<T, E> From<Result<T, E>> for OpenIdResult<T, E> {
    fn from(result: Result<T, E>) -> Self {
        match result {
            Ok(value) => OpenIdResult::Ok(value),
            Err(err) => OpenIdResult::Err(err),
        }
    }
}

pub type OpenIdCredentialKey = (Iss, Sub, Aud);
pub type Iss = String;
pub type Sub = String;
pub type Aud = String;

// Issuer found in configuration NOT credential/jwt e.g. https://login.microsoftonline.com/{tid}/v2.0
pub type ConfigIss = String;
