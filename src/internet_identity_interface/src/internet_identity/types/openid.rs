use crate::internet_identity::types::{AnchorNumber, MetadataEntryV2, Timestamp, UserKey};
use candid::{CandidType, Deserialize, Principal};
use serde::Serialize;
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
    /// SSO discovery / JWKS for this domain isn't cached yet; the canister
    /// kicked off the fetch. Retry the call shortly. Never returned for
    /// configured providers (Google / Microsoft / Apple).
    Pending,
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

#[derive(CandidType, Debug, Deserialize, Serialize)]
pub enum OpenIdDelegationError {
    NoSuchAnchor,
    NoSuchDelegation,
    JwtVerificationFailed,
    JwtExpired,
    /// SSO discovery / JWKS for this domain isn't cached yet. `prepare` kicks
    /// off the fetch; poll `get` until the delegation is ready, re-calling
    /// `prepare` if `get` reports `Pending`. Never returned for configured
    /// providers.
    Pending,
}

pub type OpenIdCredentialKey = (Iss, Sub, Aud);
pub type Iss = String;
pub type Sub = String;
pub type Aud = String;

// Issuer found in configuration NOT credential/jwt e.g. https://login.microsoftonline.com/{tid}/v2.0
pub type ConfigIss = String;
