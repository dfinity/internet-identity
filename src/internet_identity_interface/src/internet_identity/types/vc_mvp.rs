use crate::internet_identity::types::{CanisterSigKey, FrontendHostname, IdentityNumber};
use candid::{CandidType, Deserialize, Principal};

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
    #[serde(rename = "canister_sig_pk")]
    pub canister_sig_pk: CanisterSigKey,
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
pub struct SignedIdAlias {
    pub id_alias: Principal,
    pub id_dapp: Principal,
    pub credential_jws: String,
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

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct PrepareCredentialRequest {
        pub signed_id_alias: SignedIdAlias,
        pub credential_spec: CredentialSpec,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum PrepareCredentialResponse {
        Ok(PreparedCredentialData),
        Err(IssueCredentialError),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum IssueCredentialError {
        UnknownSubject,
        UnauthorizedSubject,
        InvalidIdAlias,
        SignatureNotFound,
        Internal(String),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct GetCredentialRequest {
        pub signed_id_alias: SignedIdAlias,
        pub credential_spec: CredentialSpec,
        pub vc_jwt: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum GetCredentialResponse {
        Ok(IssuedCredentialData),
        Err(IssueCredentialError),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct PreparedCredentialData {
        pub vc_jwt: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct IssuedCredentialData {
        pub vc_jws: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct CredentialSpec {
        pub info: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ManifestRequest {}

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum ManifestResponse {
        Ok(ManifestData),
        Err(String),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ManifestData {}

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21ConsentMessageRequest {
        pub method: String,
        pub arg: ByteBuf,
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
        Forbidden(Icrc21ErrorInfo),
        MalformedCall(Icrc21ErrorInfo),
        NotSupported(Icrc21ErrorInfo),
        GenericError(Icrc21ErrorInfo),
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct Icrc21ConsentInfo {
        pub consent_message: String,
        pub language: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub enum Icrc21ConsentMessageResponse {
        Ok(Icrc21ConsentInfo),
        Err(Icrc21Error),
    }
}
