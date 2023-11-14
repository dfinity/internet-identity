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
    #[serde(rename = "identity_number")]
    pub identity_number: IdentityNumber,
    #[serde(rename = "relying_party")]
    pub relying_party: FrontendHostname,
    #[serde(rename = "issuer")]
    pub issuer: FrontendHostname,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PreparedIdAlias {
    #[serde(rename = "canister_sig_pk_der")]
    pub canister_sig_pk_der: CanisterSigPublicKeyDer,
    #[serde(rename = "rp_id_alias_jwt")]
    pub rp_id_alias_jwt: String,
    #[serde(rename = "issuer_id_alias_jwt")]
    pub issuer_id_alias_jwt: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum PrepareIdAliasResponse {
    #[serde(rename = "ok")]
    Ok(PreparedIdAlias),
    #[serde(rename = "authentication_failed")]
    AuthenticationFailed(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdAliasCredentials {
    pub rp_id_alias_credential: SignedIdAlias,
    pub issuer_id_alias_credential: SignedIdAlias,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct GetIdAliasRequest {
    #[serde(rename = "identity_number")]
    pub identity_number: IdentityNumber,
    #[serde(rename = "relying_party")]
    pub relying_party: FrontendHostname,
    #[serde(rename = "issuer")]
    pub issuer: FrontendHostname,
    #[serde(rename = "rp_id_alias_jwt")]
    pub rp_id_alias_jwt: String,
    #[serde(rename = "issuer_id_alias_jwt")]
    pub issuer_id_alias_jwt: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum GetIdAliasResponse {
    #[serde(rename = "ok")]
    Ok(IdAliasCredentials),
    #[serde(rename = "authentication_failed")]
    AuthenticationFailed(String),
    #[serde(rename = "no_such_credentials")]
    NoSuchCredentials(String),
}

pub mod issuer {
    use super::*;
    use serde_bytes::ByteBuf;
    use std::collections::HashMap;
    use std::fmt::{Display, Formatter};

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct PrepareCredentialRequest {
        pub signed_id_alias: SignedIdAlias,
        pub credential_spec: CredentialSpec,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum PrepareCredentialResponse {
        #[serde(rename = "ok")]
        Ok(PreparedCredentialData),
        #[serde(rename = "err")]
        Err(IssueCredentialError),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum IssueCredentialError {
        #[serde(rename = "unknown_subject")]
        UnknownSubject(String),
        #[serde(rename = "unauthorized_subject")]
        UnauthorizedSubject(String),
        #[serde(rename = "invalid_id_alias")]
        InvalidIdAlias(String),
        #[serde(rename = "signature_not_found")]
        SignatureNotFound(String),
        #[serde(rename = "internal")]
        Internal(String),
        #[serde(rename = "unsupported_credential_spec")]
        UnsupportedCredentialSpec(String),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct GetCredentialRequest {
        pub signed_id_alias: SignedIdAlias,
        pub credential_spec: CredentialSpec,
        pub prepared_context: Option<ByteBuf>,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum GetCredentialResponse {
        #[serde(rename = "ok")]
        Ok(IssuedCredentialData),
        #[serde(rename = "err")]
        Err(IssueCredentialError),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct PreparedCredentialData {
        pub prepared_context: Option<ByteBuf>,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct IssuedCredentialData {
        pub vc_jws: String,
    }

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub enum ArgumentValue {
        #[serde(rename = "string")]
        String(String),
        #[serde(rename = "int")]
        Int(i32),
    }

    impl Display for ArgumentValue {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                ArgumentValue::String(s) => write!(f, "'{}'", s),
                ArgumentValue::Int(i) => write!(f, "{}", i),
            }
        }
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct CredentialSpec {
        pub credential_name: String,
        pub arguments: Option<HashMap<String, ArgumentValue>>,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ManifestRequest {}

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum ManifestResponse {
        #[serde(rename = "ok")]
        Ok(ManifestData),
        #[serde(rename = "err")]
        Err(String),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ManifestData {}

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21VcConsentMessageRequest {
        pub credential_spec: CredentialSpec,
        pub preferences: Icrc21ConsentPreferences,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21ConsentPreferences {
        pub language: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21ErrorInfo {
        pub error_code: u64,
        pub description: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum Icrc21Error {
        #[serde(rename = "forbidden")]
        Forbidden(Icrc21ErrorInfo),
        #[serde(rename = "malformed_call")]
        MalformedCall(Icrc21ErrorInfo),
        #[serde(rename = "not_supported")]
        NotSupported(Icrc21ErrorInfo),
        #[serde(rename = "generic_error")]
        GenericError(Icrc21ErrorInfo),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21ConsentInfo {
        pub consent_message: String,
        pub language: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum Icrc21ConsentMessageResponse {
        #[serde(rename = "ok")]
        Ok(Icrc21ConsentInfo),
        #[serde(rename = "err")]
        Err(Icrc21Error),
    }
}
