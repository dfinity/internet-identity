use crate::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, GetAccountError, Timestamp,
};
use candid::{CandidType, Principal};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

// TODO: Refer to the same constant as in `internet_identity::delegation::check_frontend_length`
pub const FRONTEND_HOSTNAME_LIMIT: usize = 255;

pub const MAX_ATTRIBUTES_PER_REQUEST: usize = 100;

pub const MAX_ATTRIBUTE_VALUE_LENGTH: usize = 50_000;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub enum AttributeName {
    Email,
    Name,
}

impl TryFrom<&str> for AttributeName {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "email" => Ok(AttributeName::Email),
            "name" => Ok(AttributeName::Name),
            _ => Err(format!("Unknown attribute: {}", value)),
        }
    }
}

impl std::fmt::Display for AttributeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeName::Email => write!(f, "email"),
            AttributeName::Name => write!(f, "name"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub enum AttributeScope {
    OpenId { issuer: String },
}

impl std::fmt::Display for AttributeScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeScope::OpenId { issuer } => write!(f, "openid:{}", issuer),
        }
    }
}

impl TryFrom<&str> for AttributeScope {
    type Error = String;

    /// Parses an attribute scope string by splitting on the first `':'`.
    ///
    /// Currently, only scopes of the form `openid:<issuer>` are supported.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut parts = value.splitn(2, ':');
        let scope_str = parts
            .next()
            .ok_or_else(|| format!("Invalid attribute request: {}", value))?;
        match scope_str {
            "openid" => {
                let issuer = parts
                    .next()
                    .ok_or_else(|| format!("Missing issuer in attribute scope: {}", value))?
                    .to_string();
                if issuer.is_empty() {
                    return Err(format!("Missing issuer in attribute scope: {}", value));
                }
                Ok(AttributeScope::OpenId { issuer })
            }
            _ => Err(format!("Unknown attribute scope: {}", scope_str)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub struct AttributeKey {
    /// E.g., `Some("openid:google.com")` in "openid:google.com:email" or `None` in "name".
    pub scope: Option<AttributeScope>,

    /// E.g., "email", "name"
    pub attribute_name: AttributeName,
}

impl TryFrom<String> for AttributeKey {
    type Error = String;

    /// Splits by ':', setting the attribute name to the last component, and setting
    /// the scope to the union of all preceding components, if any.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut parts = value.rsplitn(2, ':');

        let key = parts
            .next()
            .ok_or_else(|| format!("Invalid attribute request: {}", value))?;

        let key = AttributeName::try_from(key)?;

        let scope = parts.next().map(AttributeScope::try_from).transpose()?;

        Ok(AttributeKey {
            scope,
            attribute_name: key,
        })
    }
}

impl std::fmt::Display for AttributeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(scope) = &self.scope {
            write!(f, "{}:", scope)?;
        }
        write!(f, "{}", self.attribute_name)
    }
}

#[derive(CandidType, Deserialize)]
pub struct PrepareAttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub attribute_keys: Vec<String>,
}

#[derive(Debug)]
pub struct ValidatedPrepareAttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub attribute_keys: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>>,
}

impl TryFrom<PrepareAttributeRequest> for ValidatedPrepareAttributeRequest {
    type Error = PrepareAttributeError;

    fn try_from(value: PrepareAttributeRequest) -> Result<Self, Self::Error> {
        let PrepareAttributeRequest {
            identity_number: anchor_number,
            origin,
            account_number,
            attribute_keys: unparsed_attributes,
        } = value;

        let mut problems = Vec::new();

        if origin.len() > FRONTEND_HOSTNAME_LIMIT {
            problems.push(format!(
                "Frontend hostname length {} exceeds limit of {} bytes",
                origin.len(),
                FRONTEND_HOSTNAME_LIMIT
            ));
        }

        if unparsed_attributes.len() > MAX_ATTRIBUTES_PER_REQUEST {
            problems.push(format!(
                "Number of attributes {} exceeds limit of {}",
                unparsed_attributes.len(),
                MAX_ATTRIBUTES_PER_REQUEST
            ));
        }

        let mut attribute_keys = BTreeMap::new();

        for unparsed_attribute in unparsed_attributes {
            let AttributeKey {
                scope,
                attribute_name: key,
            } = match unparsed_attribute.try_into() {
                Ok(attr) => attr,
                Err(err) => {
                    problems.push(err);
                    continue;
                }
            };
            attribute_keys
                .entry(scope)
                .or_insert_with(BTreeSet::new)
                .insert(key);
        }

        if !problems.is_empty() {
            return Err(PrepareAttributeError::ValidationError { problems });
        }

        Ok(Self {
            identity_number: anchor_number,
            origin,
            account_number,
            attribute_keys,
        })
    }
}

#[derive(CandidType, Serialize, Deserialize)]
pub struct PrepareAttributeResponse {
    pub issued_at_timestamp_ns: Timestamp,
    pub attributes: Vec<(String, String)>,
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum PrepareAttributeError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
    GetAccountError(GetAccountError),
}

#[derive(CandidType, Deserialize)]
pub struct GetAttributesRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub issued_at_timestamp_ns: Timestamp,
    pub attributes: Vec<(String, String)>,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub struct Attribute {
    pub key: AttributeKey,
    pub value: String,
}

impl TryFrom<(String, String)> for Attribute {
    type Error = String;

    fn try_from(value: (String, String)) -> Result<Self, Self::Error> {
        let (key, value) = value;

        let key = AttributeKey::try_from(key)?;

        if value.len() > MAX_ATTRIBUTE_VALUE_LENGTH {
            return Err(format!(
                "Attribute value length {} exceeds limit of {} bytes",
                value.len(),
                MAX_ATTRIBUTE_VALUE_LENGTH
            ));
        }

        Ok(Attribute { key, value })
    }
}

#[derive(Debug)]
pub struct ValidatedGetAttributesRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub issued_at_timestamp_ns: Timestamp,
    pub attributes: BTreeMap<Option<AttributeScope>, BTreeSet<Attribute>>,
}

impl TryFrom<GetAttributesRequest> for ValidatedGetAttributesRequest {
    type Error = GetAttributesError;

    fn try_from(value: GetAttributesRequest) -> Result<Self, Self::Error> {
        let GetAttributesRequest {
            identity_number,
            origin,
            account_number,
            issued_at_timestamp_ns,
            attributes: unparsed_attributes,
        } = value;

        let mut problems = Vec::new();

        if origin.len() > FRONTEND_HOSTNAME_LIMIT {
            problems.push(format!(
                "Frontend hostname length {} exceeds limit of {} bytes",
                origin.len(),
                FRONTEND_HOSTNAME_LIMIT
            ));
        }

        if unparsed_attributes.len() > MAX_ATTRIBUTES_PER_REQUEST {
            problems.push(format!(
                "Number of attributes {} exceeds limit of {}",
                unparsed_attributes.len(),
                MAX_ATTRIBUTES_PER_REQUEST
            ));
        }

        let mut attributes = BTreeMap::new();

        for unparsed_attribute in unparsed_attributes {
            let attribute: Attribute = match unparsed_attribute.try_into() {
                Ok(attr) => attr,
                Err(err) => {
                    problems.push(err);
                    continue;
                }
            };
            attributes
                .entry(attribute.key.scope.clone())
                .or_insert_with(BTreeSet::new)
                .insert(attribute);
        }

        if !problems.is_empty() {
            return Err(GetAttributesError::ValidationError { problems });
        }

        Ok(Self {
            identity_number,
            origin,
            account_number,
            issued_at_timestamp_ns,
            attributes,
        })
    }
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize, Eq, PartialOrd, Ord)]
pub struct CertifiedAttribute {
    pub key: String,
    pub value: String,
    pub signature: Vec<u8>,
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub struct CertifiedAttributes {
    pub certified_attributes: Vec<CertifiedAttribute>,
    pub expires_at_timestamp_ns: Timestamp,
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum GetAttributesError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
    GetAccountError(GetAccountError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq as pretty_assert_eq;

    mod attribute_key_tests {
        use super::*;

        #[test]
        fn test_try_from_str_email() {
            let result = AttributeName::try_from("email");
            assert_eq!(result, Ok(AttributeName::Email));
        }

        #[test]
        fn test_try_from_str_name() {
            let result = AttributeName::try_from("name");
            assert_eq!(result, Ok(AttributeName::Name));
        }

        #[test]
        fn test_try_from_str_unknown() {
            let result = AttributeName::try_from("unknown");
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Unknown attribute: unknown");
        }

        #[test]
        fn test_try_from_str_empty() {
            let result = AttributeName::try_from("");
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Unknown attribute: ");
        }

        #[test]
        fn test_display_email() {
            assert_eq!(AttributeName::Email.to_string(), "email");
        }

        #[test]
        fn test_display_name() {
            assert_eq!(AttributeName::Name.to_string(), "name");
        }

        #[test]
        fn test_ordering() {
            assert!(AttributeName::Email < AttributeName::Name);
        }
    }

    // AttributeScope Tests
    mod attribute_scope_tests {
        use super::*;

        #[test]
        fn test_try_from_str_openid() {
            let result = AttributeScope::try_from("openid:google.com");
            assert_eq!(
                result,
                Ok(AttributeScope::OpenId {
                    issuer: "google.com".to_string()
                })
            );
        }

        #[test]
        fn test_try_from_str_openid_complex_issuer() {
            let result = AttributeScope::try_from("openid:accounts.google.com");
            assert_eq!(
                result,
                Ok(AttributeScope::OpenId {
                    issuer: "accounts.google.com".to_string()
                })
            );
        }

        #[test]
        fn test_try_from_str_openid_with_colon_in_issuer() {
            let result = AttributeScope::try_from("openid:issuer:with:colons");
            assert_eq!(
                result,
                Ok(AttributeScope::OpenId {
                    issuer: "issuer:with:colons".to_string()
                })
            );
        }

        #[test]
        fn test_try_from_str_openid_missing_issuer() {
            let result = AttributeScope::try_from("openid:");
            assert!(result.is_err());
            assert!(result.unwrap_err().contains("Missing issuer"));
        }

        #[test]
        fn test_try_from_str_openid_no_colon() {
            let result = AttributeScope::try_from("openid");
            assert!(result.is_err());
            assert!(result.unwrap_err().contains("Missing issuer"));
        }

        #[test]
        fn test_try_from_str_unknown_scope() {
            let result = AttributeScope::try_from("unknown:issuer");
            assert!(result.is_err());
            assert!(result.unwrap_err().contains("Unknown attribute scope"));
        }

        #[test]
        fn test_display_openid() {
            let scope = AttributeScope::OpenId {
                issuer: "google.com".to_string(),
            };
            assert_eq!(scope.to_string(), "openid:google.com");
        }

        #[test]
        fn test_ordering() {
            let scope1 = AttributeScope::OpenId {
                issuer: "a.com".to_string(),
            };
            let scope2 = AttributeScope::OpenId {
                issuer: "b.com".to_string(),
            };
            assert!(scope1 < scope2);
        }
    }

    // AttributeRequest Tests
    mod attribute_request_tests {
        use super::*;

        #[test]
        fn test_try_from_string_key_only() {
            let result = AttributeKey::try_from("email".to_string());
            assert_eq!(
                result,
                Ok(AttributeKey {
                    scope: None,
                    attribute_name: AttributeName::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_scope() {
            let result = AttributeKey::try_from("openid:google.com:email".to_string());
            assert_eq!(
                result,
                Ok(AttributeKey {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "google.com".to_string()
                    }),
                    attribute_name: AttributeName::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_complex_issuer() {
            let result = AttributeKey::try_from("openid:accounts.google.com:name".to_string());
            assert_eq!(
                result,
                Ok(AttributeKey {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "accounts.google.com".to_string()
                    }),
                    attribute_name: AttributeName::Name,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_issuer_containing_colons() {
            let result = AttributeKey::try_from("openid:issuer:with:colons:email".to_string());
            assert_eq!(
                result,
                Ok(AttributeKey {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "issuer:with:colons".to_string()
                    }),
                    attribute_name: AttributeName::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_invalid_key() {
            let result = AttributeKey::try_from("openid:google.com:invalid".to_string());
            assert_eq!(result, Err("Unknown attribute: invalid".to_string()));
        }

        #[test]
        fn test_try_from_string_invalid_scope() {
            let result = AttributeKey::try_from("unknown:issuer:email".to_string());
            assert_eq!(result, Err("Unknown attribute scope: unknown".to_string()));
        }

        #[test]
        fn test_try_from_string_empty() {
            let result = AttributeKey::try_from("".to_string());
            assert_eq!(result, Err("Unknown attribute: ".to_string()));
        }

        #[test]
        fn test_display_key_only() {
            let req = AttributeKey {
                scope: None,
                attribute_name: AttributeName::Email,
            };
            assert_eq!(req.to_string(), "email");
        }

        #[test]
        fn test_display_with_scope() {
            let req = AttributeKey {
                scope: Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                attribute_name: AttributeName::Email,
            };
            assert_eq!(req.to_string(), "openid:google.com:email");
        }

        #[test]
        fn test_round_trip_conversion_key_only() {
            let original = "name".to_string();
            let req = AttributeKey::try_from(original.clone()).unwrap();
            assert_eq!(req.to_string(), original);
        }

        #[test]
        fn test_round_trip_conversion_with_scope() {
            let original = "openid:google.com:email".to_string();
            let req = AttributeKey::try_from(original.clone()).unwrap();
            assert_eq!(req.to_string(), original);
        }

        #[test]
        fn test_ordering() {
            let req1 = AttributeKey {
                scope: None,
                attribute_name: AttributeName::Email,
            };
            let req2 = AttributeKey {
                scope: None,
                attribute_name: AttributeName::Name,
            };
            let req3 = AttributeKey {
                scope: Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                attribute_name: AttributeName::Email,
            };
            assert!(req1 < req2);
            assert!(req1 < req3);
        }
    }

    mod attribute_tests {
        use super::*;

        #[test]
        fn test_attribute_try_from_valid() {
            let attribute =
                Attribute::try_from(("email".to_string(), "user@example.com".to_string())).unwrap();
            let expected_key = AttributeKey::try_from("email".to_string()).unwrap();

            pretty_assert_eq!(
                attribute,
                Attribute {
                    key: expected_key,
                    value: "user@example.com".to_string(),
                }
            );
        }

        #[test]
        fn test_attribute_try_from_invalid_key() {
            let result = Attribute::try_from(("invalid".to_string(), "value".to_string()));
            pretty_assert_eq!(result, Err("Unknown attribute: invalid".to_string()));
        }

        #[test]
        fn test_attribute_try_from_value_too_long() {
            let long_value = "x".repeat(MAX_ATTRIBUTE_VALUE_LENGTH + 1);
            let length = long_value.len();
            let result = Attribute::try_from(("email".to_string(), long_value));
            pretty_assert_eq!(
                result,
                Err(format!(
                    "Attribute value length {} exceeds limit of {} bytes",
                    length, MAX_ATTRIBUTE_VALUE_LENGTH
                ))
            );
        }
    }

    mod validated_get_attributes_request_tests {
        use super::*;
        use std::collections::{BTreeMap, BTreeSet};

        #[test]
        fn test_try_from_valid_request_multiple_scopes() {
            let request = GetAttributesRequest {
                identity_number: 987,
                origin: "example.com".to_string(),
                account_number: Some(7),
                issued_at_timestamp_ns: 42,
                attributes: vec![
                    ("email".to_string(), "user@example.com".to_string()),
                    (
                        "openid:google.com:email".to_string(),
                        "google@example.com".to_string(),
                    ),
                ],
            };

            let validated = ValidatedGetAttributesRequest::try_from(request).unwrap();
            pretty_assert_eq!(validated.identity_number, 987);
            pretty_assert_eq!(validated.account_number, Some(7));
            pretty_assert_eq!(validated.issued_at_timestamp_ns, 42);

            let mut expected = BTreeMap::new();

            let mut default_scope = BTreeSet::new();
            default_scope.insert(
                Attribute::try_from(("email".to_string(), "user@example.com".to_string())).unwrap(),
            );
            expected.insert(None, default_scope);

            let mut google_scope = BTreeSet::new();
            google_scope.insert(
                Attribute::try_from((
                    "openid:google.com:email".to_string(),
                    "google@example.com".to_string(),
                ))
                .unwrap(),
            );
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                google_scope,
            );

            pretty_assert_eq!(validated.attributes, expected);
        }

        #[test]
        fn test_try_from_deduplicates_attributes() {
            let request = GetAttributesRequest {
                identity_number: 111,
                origin: "example.com".to_string(),
                account_number: None,
                issued_at_timestamp_ns: 1,
                attributes: vec![
                    ("email".to_string(), "alias".to_string()),
                    ("email".to_string(), "alias".to_string()),
                ],
            };

            let validated = ValidatedGetAttributesRequest::try_from(request).unwrap();

            let mut expected = BTreeMap::new();
            let mut attrs = BTreeSet::new();
            attrs.insert(Attribute::try_from(("email".to_string(), "alias".to_string())).unwrap());
            expected.insert(None, attrs);

            pretty_assert_eq!(validated.attributes, expected);
        }

        #[test]
        fn test_try_from_validation_errors_combined() {
            let long_origin = "x".repeat(FRONTEND_HOSTNAME_LIMIT + 1);
            let long_value = "y".repeat(MAX_ATTRIBUTE_VALUE_LENGTH + 1);
            let long_value_len = long_value.len();

            let request = GetAttributesRequest {
                identity_number: 222,
                origin: long_origin.clone(),
                account_number: None,
                issued_at_timestamp_ns: 2,
                attributes: vec![
                    ("invalid".to_string(), "value".to_string()),
                    ("email".to_string(), long_value),
                ],
            };

            let err = ValidatedGetAttributesRequest::try_from(request).unwrap_err();
            match err {
                GetAttributesError::ValidationError { problems } => {
                    pretty_assert_eq!(
                        problems,
                        vec![
                            format!(
                                "Frontend hostname length {} exceeds limit of {} bytes",
                                long_origin.len(),
                                FRONTEND_HOSTNAME_LIMIT
                            ),
                            "Unknown attribute: invalid".to_string(),
                            format!(
                                "Attribute value length {} exceeds limit of {} bytes",
                                long_value_len, MAX_ATTRIBUTE_VALUE_LENGTH
                            ),
                        ],
                    );
                }
                other => panic!("Expected validation error, got {other:?}"),
            }
        }

        #[test]
        fn test_try_from_too_many_attributes() {
            let attributes = (0..=MAX_ATTRIBUTES_PER_REQUEST)
                .map(|i| ("email".to_string(), format!("value-{i}")))
                .collect::<Vec<_>>();

            let request = GetAttributesRequest {
                identity_number: 333,
                origin: "example.com".to_string(),
                account_number: None,
                issued_at_timestamp_ns: 3,
                attributes,
            };

            let err = ValidatedGetAttributesRequest::try_from(request).unwrap_err();
            match err {
                GetAttributesError::ValidationError { problems } => {
                    assert!(problems.iter().any(|p| {
                        p == &format!(
                            "Number of attributes {} exceeds limit of {}",
                            MAX_ATTRIBUTES_PER_REQUEST + 1,
                            MAX_ATTRIBUTES_PER_REQUEST
                        )
                    }));
                }
                other => panic!("Expected validation error, got {other:?}"),
            }
        }
    }

    // ValidatedPrepareAttributeRequest Tests
    mod validated_prepare_attribute_request_tests {
        use super::*;

        #[test]
        fn test_try_from_single_attribute() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                attribute_keys: vec!["email".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");
            assert_eq!(validated.identity_number, 12345);
            assert_eq!(validated.origin, "example.com");
            assert_eq!(validated.account_number, None);

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeName::Email);
            expected.insert(None, s);

            assert_eq!(validated.attribute_keys, expected);
        }

        #[test]
        fn test_try_from_multiple_attributes_same_scope() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: Some(1),
                attribute_keys: vec!["email".to_string(), "name".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeName::Email);
            s.insert(AttributeName::Name);
            expected.insert(None, s);

            assert_eq!(validated.attribute_keys, expected);
        }

        #[test]
        fn test_try_from_multiple_attributes_different_scopes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                attribute_keys: vec!["email".to_string(), "openid:google.com:email".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut default_set = BTreeSet::new();
            default_set.insert(AttributeName::Email);
            expected.insert(None, default_set);

            let mut google_set = BTreeSet::new();
            google_set.insert(AttributeName::Email);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                google_set,
            );

            assert_eq!(validated.attribute_keys, expected);
        }

        #[test]
        fn test_try_from_duplicate_attributes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                attribute_keys: vec!["email".to_string(), "email".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeName::Email);
            expected.insert(None, s);

            assert_eq!(validated.attribute_keys, expected);
        }

        #[test]
        fn test_try_from_invalid_attribute() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,

                attribute_keys: vec!["invalid".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let err = result.unwrap_err();
            pretty_assert_eq!(
                err,
                PrepareAttributeError::ValidationError {
                    problems: vec!["Unknown attribute: invalid".to_string()]
                }
            );
        }

        #[test]
        fn test_try_from_multiple_invalid_attributes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,

                attribute_keys: vec!["invalid1".to_string(), "invalid2".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let err = result.unwrap_err();
            pretty_assert_eq!(
                err,
                PrepareAttributeError::ValidationError {
                    problems: vec![
                        "Unknown attribute: invalid1".to_string(),
                        "Unknown attribute: invalid2".to_string()
                    ]
                }
            );
        }

        #[test]
        fn test_try_from_mixed_valid_and_invalid() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,

                attribute_keys: vec!["email".to_string(), "invalid".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let err = result.unwrap_err();
            pretty_assert_eq!(
                err,
                PrepareAttributeError::ValidationError {
                    problems: vec!["Unknown attribute: invalid".to_string()]
                }
            );
        }

        #[test]
        fn test_try_from_empty_attributes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,

                attribute_keys: vec![],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let expected: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>> =
                BTreeMap::new();
            assert_eq!(validated.attribute_keys, expected);
        }

        #[test]
        fn test_try_from_complex_scenario() {
            let req = PrepareAttributeRequest {
                identity_number: 67890,
                origin: "app.example.com".to_string(),
                account_number: Some(42),
                attribute_keys: vec![
                    "name".to_string(),
                    "openid:google.com:email".to_string(),
                    "openid:google.com:name".to_string(),
                    "openid:github.com:email".to_string(),
                ],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");
            assert_eq!(validated.identity_number, 67890);
            assert_eq!(validated.account_number, Some(42));

            let mut expected = BTreeMap::new();
            let mut default_set = BTreeSet::new();
            default_set.insert(AttributeName::Name);
            expected.insert(None, default_set);

            let mut google_set = BTreeSet::new();
            google_set.insert(AttributeName::Email);
            google_set.insert(AttributeName::Name);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                google_set,
            );

            let mut github_set = BTreeSet::new();
            github_set.insert(AttributeName::Email);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "github.com".to_string(),
                }),
                github_set,
            );

            assert_eq!(validated.attribute_keys, expected);
        }
    }
}
