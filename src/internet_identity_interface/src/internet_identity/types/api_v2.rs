use crate::internet_identity::types::{CredentialId, PublicKey, Timestamp};
use candid::{CandidType, Deserialize, Principal};
use serde_bytes::ByteBuf;
use std::collections::HashMap;

pub type IdentityNumber = u64;

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum MetadataEntryV2 {
    String(String),
    Bytes(ByteBuf),
    Map(HashMap<String, MetadataEntryV2>),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodProtection {
    Protected,
    Unprotected,
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum AuthnMethodPurpose {
    Recovery,
    Authentication,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PublicKeyAuthn {
    pub pubkey: PublicKey,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct WebAuthn {
    pub pubkey: PublicKey,
    pub credential_id: CredentialId,
}

/// Supported authentication methods
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethod {
    WebAuthn(WebAuthn),
    PubKey(PublicKeyAuthn),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AuthnMethodSecuritySettings {
    pub protection: AuthnMethodProtection,
    pub purpose: AuthnMethodPurpose,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AuthnMethodData {
    pub authn_method: AuthnMethod,
    pub security_settings: AuthnMethodSecuritySettings,
    // contains now the following fields
    // - alias
    // - origin
    // - key_type: reduced to "platform", "cross_platform" on migration
    pub metadata: HashMap<String, MetadataEntryV2>,
    // last usage timestamp cannot be written and will always be ignored on write
    pub last_authentication: Option<Timestamp>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AuthnMethodRegistration {
    pub expiration: Timestamp,
    pub authn_method: Option<AuthnMethodData>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdentityAuthnInfo {
    pub authn_methods: Vec<AuthnMethod>,
    pub recovery_authn_methods: Vec<AuthnMethod>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdentityInfo {
    pub authn_methods: Vec<AuthnMethodData>,
    pub authn_method_registration: Option<AuthnMethodRegistration>,
    pub metadata: HashMap<String, MetadataEntryV2>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum IdentityInfoError {
    Unauthorized(Principal),
    InternalCanisterError(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum IdentityRegisterError {
    CanisterFull,
    BadCaptcha,
    InvalidMetadata(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodAddError {
    InvalidMetadata(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodReplaceError {
    InvalidMetadata(String),
    AuthnMethodNotFound,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodMetadataReplaceError {
    InvalidMetadata(String),
    AuthnMethodNotFound,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodSecuritySettingsReplaceError {
    AuthnMethodNotFound,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct RegistrationModeInfo {
    pub expiration: Timestamp,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AuthnMethodConfirmationCode {
    pub confirmation_code: String,
    pub expiration: Timestamp,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodRegisterError {
    RegistrationModeOff,
    RegistrationAlreadyInProgress,
    InvalidMetadata(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum AuthnMethodConfirmationError {
    WrongCode { retries_left: u8 },
    RegistrationModeOff,
    NoAuthnMethodToConfirm,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum IdentityMetadataReplaceError {
    Unauthorized(Principal),
    StorageSpaceExceeded {
        space_available: u64,
        space_required: u64,
    },
    InternalCanisterError(String),
}
