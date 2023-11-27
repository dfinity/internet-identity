use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

/// API to be implemented by an issuer of verifiable credentials.
/// (cf. https://github.com/dfinity/internet-identity/blob/main/docs/vc-spec.md)
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SignedIdAlias {
    pub credential_jws: String,
}

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
    UnknownSubject(String),
    UnauthorizedSubject(String),
    InvalidIdAlias(String),
    SignatureNotFound(String),
    Internal(String),
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
    Ok(IssuedCredentialData),
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
    String(String),
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
    pub credential_type: String,
    pub arguments: Option<HashMap<String, ArgumentValue>>,
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
    UnsupportedCanisterCall(Icrc21ErrorInfo),
    ConsentMessageUnavailable(Icrc21ErrorInfo),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_display_argument_values() {
        assert_eq!("42", format!("{}", ArgumentValue::Int(42)));
        assert_eq!("0", format!("{}", ArgumentValue::Int(0)));
        assert_eq!("-7", format!("{}", ArgumentValue::Int(-7)));
        assert_eq!("''", format!("{}", ArgumentValue::String("".to_string())));
        assert_eq!(
            "'some string'",
            format!("{}", ArgumentValue::String("some string".to_string()))
        );
    }
}
