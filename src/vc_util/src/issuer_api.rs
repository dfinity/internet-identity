use candid::{CandidType, Deserialize, Nat};
use serde_bytes::ByteBuf;
use serde_json::{Number, Value};
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

impl From<ArgumentValue> for Value {
    fn from(argument_value: ArgumentValue) -> Self {
        match argument_value {
            ArgumentValue::String(s) => Value::String(s),
            ArgumentValue::Int(i) => Value::Number(Number::from(i)),
        }
    }
}

impl PartialEq<serde_json::Value> for ArgumentValue {
    fn eq(&self, other: &Value) -> bool {
        match self {
            ArgumentValue::String(ls) => {
                if let Some(rs) = other.as_str() {
                    ls.eq(rs)
                } else {
                    false
                }
            }
            ArgumentValue::Int(li) => {
                if let Some(ri) = other.as_i64() {
                    (*li as i64) == ri
                } else {
                    false
                }
            }
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
    pub description: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum Icrc21Error {
    UnsupportedCanisterCall(Icrc21ErrorInfo),
    ConsentMessageUnavailable(Icrc21ErrorInfo),
    GenericError {
        error_code: Nat,
        description: String,
    },
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct Icrc21ConsentInfo {
    pub consent_message: String,
    pub language: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DerivationOriginRequest {
    pub frontend_hostname: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DerivationOriginData {
    pub origin: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum DerivationOriginError {
    UnsupportedOrigin(String),
    Internal(String),
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

    #[test]
    fn should_correctly_compare_argument_values() {
        assert_eq!(ArgumentValue::Int(42), Value::from(42));
        assert_eq!(ArgumentValue::Int(123456789), Value::from(123456789));
        assert_eq!(ArgumentValue::Int(0), Value::from(0));

        assert_ne!(ArgumentValue::Int(42), Value::from(11));
        assert_ne!(ArgumentValue::Int(42), Value::from("some string"));
        assert_ne!(ArgumentValue::Int(42), Value::from(true));
        assert_ne!(ArgumentValue::Int(42), Value::from(vec![1, 2, 3]));

        assert_eq!(
            ArgumentValue::String("same string".to_string()),
            Value::from("same string")
        );
        let long_string = "this is a bit longer string just for testing purposes";
        assert_eq!(
            ArgumentValue::String(long_string.to_string()),
            Value::from(long_string)
        );
        assert_eq!(ArgumentValue::String("".to_string()), Value::from(""));

        assert_ne!(
            ArgumentValue::String("some string".to_string()),
            Value::from("different")
        );
        assert_ne!(
            ArgumentValue::String("a string".to_string()),
            Value::from(42)
        );
        assert_ne!(
            ArgumentValue::String("a string".to_string()),
            Value::from(true)
        );
        assert_ne!(
            ArgumentValue::String("a string".to_string()),
            Value::from(vec![1, 2, 3])
        );
    }
}
