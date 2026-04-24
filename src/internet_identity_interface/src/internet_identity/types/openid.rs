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
    /// Two-hop SSO provenance for credentials linked via
    /// `add_discoverable_oidc_config`. Looked up on demand by `(iss, aud)`
    /// from current canister state. `None` for direct-provider credentials
    /// (Google / Apple / Microsoft) and for SSO credentials whose provider
    /// is no longer registered on the canister.
    pub sso_configuration: Option<SsoConfiguration>,
}

/// Per-credential SSO provenance returned alongside an
/// `OpenIdCredentialData`. Grouped into one optional struct (rather than
/// two independent optional fields) so "is this credential SSO?" is a
/// single presence check.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SsoConfiguration {
    /// The `discovery_domain` the user entered (always present for SSO
    /// credentials).
    pub domain: String,
    /// Human-readable name served alongside `client_id` at
    /// `{domain}/.well-known/ii-openid-configuration`. `None` when the
    /// domain doesn't publish one — callers that want a label should
    /// fall back to `domain` on the frontend side. Kept separate from
    /// `domain` so callers can render the two cases differently.
    pub name: Option<String>,
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

#[derive(CandidType, Debug, Deserialize, Serialize)]
pub enum OpenIdDelegationError {
    NoSuchAnchor,
    NoSuchDelegation,
    JwtVerificationFailed,
    JwtExpired,
}

pub type OpenIdCredentialKey = (Iss, Sub, Aud);
pub type Iss = String;
pub type Sub = String;
pub type Aud = String;

// Issuer found in configuration NOT credential/jwt e.g. https://login.microsoftonline.com/{tid}/v2.0
pub type ConfigIss = String;
