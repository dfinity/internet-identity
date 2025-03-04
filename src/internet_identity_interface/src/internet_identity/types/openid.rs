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
    pub last_usage_timestamp: Timestamp,
    pub metadata: HashMap<String, MetadataEntryV2>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum OpenIdCredentialAddError {
    Unauthorized(Principal),
    JwtVerificationFailed,
    OpenIdCredentialAlreadyRegistered,
    InternalCanisterError(String),
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
}

pub type OpenIdCredentialKey = (Iss, Sub);
pub type Iss = String;
pub type Sub = String;
pub type Aud = String;
