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
