use crate::internet_identity::types::{CanisterSigKey, Signature};
use candid::{CandidType, Deserialize, Principal};

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum PrepareIdAliasResponse {
    #[serde(rename = "ok")]
    Ok(CanisterSigKey),
    #[serde(rename = "authentication_failed")]
    AuthenticationFailed(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SignedIdAlias {
    pub id_alias: Principal,
    pub id_dapp: Principal,
    pub signature: Signature,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdAliasCredentials {
    pub rp_id_alias_credential: SignedIdAlias,
    pub issuer_id_alias_credential: SignedIdAlias,
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
    use std::collections::HashMap;

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct IssueCredentialRequest {
        pub signed_id_alias: SignedIdAlias,
        pub credential_spec: CredentialSpec,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct IssueCredentialResponse {
        pub credential: VerifiableCredential,
        pub consent_info: ConsentInfo,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct CredentialSpec {
        pub info: String,
        pub consent_message_request: ConsentMessageRequest,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ConsentPreferences {
        pub language: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ConsentMessageRequest {
        pub preferences: ConsentPreferences,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct ConsentInfo {
        pub consent_message: String,
        pub language: String,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct CredentialSubject {
        pub id: String,
        pub property_set: HashMap<String, String>,
    }

    #[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
    pub struct VerifiableCredential {
        pub issuer: String,
        pub credential_subject: CredentialSubject,
        pub proof: String,
    }
}
