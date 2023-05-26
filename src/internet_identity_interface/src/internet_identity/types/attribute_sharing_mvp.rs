use candid::{CandidType, Deserialize};

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum PreparePrincipalLinkResponse {
    #[serde(rename = "ok")]
    Ok,
    #[serde(rename = "authentication_failed")]
    AuthenticationFailed(String),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PrincipalLinkCredentials {
    pub rp_link_credential: String,
    pub issuer_link_credential: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum GetPrincipalLinkResponse {
    #[serde(rename = "ok")]
    Ok(PrincipalLinkCredentials),
    #[serde(rename = "authentication_failed")]
    AuthenticationFailed(String),
    #[serde(rename = "no_such_credentials")]
    NoSuchCredentials(String),
}
