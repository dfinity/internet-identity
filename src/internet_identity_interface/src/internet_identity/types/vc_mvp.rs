use crate::internet_identity::types::{CanisterSigPublicKeyDer, FrontendHostname, IdentityNumber};
use candid::{CandidType, Deserialize, Principal};

/// Types for requesting ID-alias credentials, used for attribute sharing feature.

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SignedIdAlias {
    pub id_alias: Principal,
    pub id_dapp: Principal,
    pub credential_jws: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PrepareIdAliasRequest {
    pub identity_number: IdentityNumber,
    pub relying_party: FrontendHostname,
    pub issuer: FrontendHostname,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PreparedIdAlias {
    pub canister_sig_pk_der: CanisterSigPublicKeyDer,
    pub rp_id_alias_signing_input: String,
    pub issuer_id_alias_signing_input: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum PrepareIdAliasError {
    Unauthorized(Principal),
    InternalCanisterError(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdAliasCredentials {
    pub rp_id_alias_credential: SignedIdAlias,
    pub issuer_id_alias_credential: SignedIdAlias,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct GetIdAliasRequest {
    pub identity_number: IdentityNumber,
    pub relying_party: FrontendHostname,
    pub issuer: FrontendHostname,
    pub rp_id_alias_signing_input: String,
    pub issuer_id_alias_signing_input: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum GetIdAliasError {
    Unauthorized(Principal),
    NoSuchCredentials(String),
    InternalCanisterError(String),
}
