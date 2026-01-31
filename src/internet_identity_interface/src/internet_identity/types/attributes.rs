use std::collections::{BTreeMap, BTreeSet};

use candid::{CandidType, Principal};
use serde::{Deserialize, Serialize};

use crate::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, SessionKey, Timestamp,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttributeField {
    Email,
    Name,
}

impl TryFrom<&str> for AttributeField {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "email" => Ok(AttributeField::Email),
            "name" => Ok(AttributeField::Name),
            _ => Err(format!("Unknown attribute: {}", value)),
        }
    }
}

impl std::fmt::Display for AttributeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeField::Email => write!(f, "email"),
            AttributeField::Name => write!(f, "name"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    /// Splits by ':', setting the attribute scope to the first component and processing
    /// possible additional attributes as needed.
    ///
    /// The special value `AttributeScope::Default` cannot be constructed via this method
    /// and needs to be constructed directly.
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AttributeRequest {
    /// E.g., `Some("openid:google.com")` in "openid:google.com:email" or `None` in "name".
    pub scope: Option<AttributeScope>,

    /// E.g., "email", "name"
    pub field: AttributeField,
}

impl TryFrom<String> for AttributeRequest {
    type Error = String;

    /// Splits by ':', setting the attribute name to the last component, and setting
    /// the scope to the union of all preceding components, if any.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut parts = value.rsplitn(2, ':');

        let field = parts
            .next()
            .ok_or_else(|| format!("Invalid attribute request: {}", value))?;

        let field = AttributeField::try_from(field)?;

        let scope = parts.next().map(AttributeScope::try_from).transpose()?;

        Ok(AttributeRequest { scope, field })
    }
}

impl std::fmt::Display for AttributeRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(scope) = &self.scope {
            write!(f, "{}:", scope)?;
        }
        write!(f, "{}", self.field)
    }
}

#[derive(CandidType, Deserialize)]
pub struct PrepareAttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
    pub requested_attributes: Vec<String>,
}

#[derive(Debug)]
pub struct ValidatedPrepareAttributeRequest {
    pub identity_number: AnchorNumber,
    pub session_key: SessionKey,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeField>>,
}

impl TryFrom<PrepareAttributeRequest> for ValidatedPrepareAttributeRequest {
    type Error = PrepareAttributeError;

    fn try_from(value: PrepareAttributeRequest) -> Result<Self, Self::Error> {
        let PrepareAttributeRequest {
            identity_number: anchor_number,
            origin,
            account_number,
            session_key,
            requested_attributes: unparsed_attributes,
        } = value;

        let mut attributes = BTreeMap::new();
        let mut problems = Vec::new();

        for unparsed_attribute in unparsed_attributes {
            let AttributeRequest { scope, field } = match unparsed_attribute.try_into() {
                Ok(attr) => attr,
                Err(err) => {
                    problems.push(err);
                    continue;
                }
            };
            attributes
                .entry(scope)
                .or_insert_with(BTreeSet::new)
                .insert(field);
        }

        if !problems.is_empty() {
            return Err(PrepareAttributeError::ValidationError { problems });
        }

        Ok(Self {
            identity_number: anchor_number,
            origin,
            account_number,
            session_key,
            requested_attributes: attributes,
        })
    }
}

#[derive(CandidType, Serialize)]
pub struct PrepareAttributeResponse {
    pub issued_at_timestamp_ns: Timestamp,
    pub certified_attributes: Vec<String>,
}

#[derive(Debug, PartialEq, CandidType, Serialize)]
pub enum PrepareAttributeError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq as pretty_assert_eq;
    use serde_bytes::ByteBuf;

    // AttributeField Tests
    mod attribute_field_tests {
        use super::*;

        #[test]
        fn test_try_from_str_email() {
            let result = AttributeField::try_from("email");
            assert_eq!(result, Ok(AttributeField::Email));
        }

        #[test]
        fn test_try_from_str_name() {
            let result = AttributeField::try_from("name");
            assert_eq!(result, Ok(AttributeField::Name));
        }

        #[test]
        fn test_try_from_str_unknown() {
            let result = AttributeField::try_from("unknown");
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Unknown attribute: unknown");
        }

        #[test]
        fn test_try_from_str_empty() {
            let result = AttributeField::try_from("");
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Unknown attribute: ");
        }

        #[test]
        fn test_display_email() {
            assert_eq!(AttributeField::Email.to_string(), "email");
        }

        #[test]
        fn test_display_name() {
            assert_eq!(AttributeField::Name.to_string(), "name");
        }

        #[test]
        fn test_ordering() {
            assert!(AttributeField::Email < AttributeField::Name);
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
        fn test_try_from_string_field_only() {
            let result = AttributeRequest::try_from("email".to_string());
            assert_eq!(
                result,
                Ok(AttributeRequest {
                    scope: None,
                    field: AttributeField::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_scope() {
            let result = AttributeRequest::try_from("openid:google.com:email".to_string());
            assert_eq!(
                result,
                Ok(AttributeRequest {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "google.com".to_string()
                    }),
                    field: AttributeField::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_complex_issuer() {
            let result = AttributeRequest::try_from("openid:accounts.google.com:name".to_string());
            assert_eq!(
                result,
                Ok(AttributeRequest {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "accounts.google.com".to_string()
                    }),
                    field: AttributeField::Name,
                })
            );
        }

        #[test]
        fn test_try_from_string_with_issuer_containing_colons() {
            let result = AttributeRequest::try_from("openid:issuer:with:colons:email".to_string());
            assert_eq!(
                result,
                Ok(AttributeRequest {
                    scope: Some(AttributeScope::OpenId {
                        issuer: "issuer:with:colons".to_string()
                    }),
                    field: AttributeField::Email,
                })
            );
        }

        #[test]
        fn test_try_from_string_invalid_field() {
            let result = AttributeRequest::try_from("openid:google.com:invalid".to_string());
            assert_eq!(result, Err("Unknown attribute: invalid".to_string()));
        }

        #[test]
        fn test_try_from_string_invalid_scope() {
            let result = AttributeRequest::try_from("unknown:issuer:email".to_string());
            assert_eq!(result, Err("Unknown attribute scope: unknown".to_string()));
        }

        #[test]
        fn test_try_from_string_empty() {
            let result = AttributeRequest::try_from("".to_string());
            assert_eq!(result, Err("Unknown attribute: ".to_string()));
        }

        #[test]
        fn test_display_field_only() {
            let req = AttributeRequest {
                scope: None,
                field: AttributeField::Email,
            };
            assert_eq!(req.to_string(), "email");
        }

        #[test]
        fn test_display_with_scope() {
            let req = AttributeRequest {
                scope: Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                field: AttributeField::Email,
            };
            assert_eq!(req.to_string(), "openid:google.com:email");
        }

        #[test]
        fn test_round_trip_conversion_field_only() {
            let original = "name".to_string();
            let req = AttributeRequest::try_from(original.clone()).unwrap();
            assert_eq!(req.to_string(), original);
        }

        #[test]
        fn test_round_trip_conversion_with_scope() {
            let original = "openid:google.com:email".to_string();
            let req = AttributeRequest::try_from(original.clone()).unwrap();
            assert_eq!(req.to_string(), original);
        }

        #[test]
        fn test_ordering() {
            let req1 = AttributeRequest {
                scope: None,
                field: AttributeField::Email,
            };
            let req2 = AttributeRequest {
                scope: None,
                field: AttributeField::Name,
            };
            let req3 = AttributeRequest {
                scope: Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                field: AttributeField::Email,
            };
            assert!(req1 < req2);
            assert!(req1 < req3);
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
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["email".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");
            assert_eq!(validated.identity_number, 12345);
            assert_eq!(validated.origin, "example.com");
            assert_eq!(validated.account_number, None);
            assert_eq!(validated.session_key, ByteBuf::from(vec![1, 2, 3]));

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeField::Email);
            expected.insert(None, s);

            assert_eq!(validated.requested_attributes, expected);
        }

        #[test]
        fn test_try_from_multiple_attributes_same_scope() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: Some(1),
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["email".to_string(), "name".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeField::Email);
            s.insert(AttributeField::Name);
            expected.insert(None, s);

            assert_eq!(validated.requested_attributes, expected);
        }

        #[test]
        fn test_try_from_multiple_attributes_different_scopes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                session_key: vec![1, 2, 3].into(),
                requested_attributes: vec![
                    "email".to_string(),
                    "openid:google.com:email".to_string(),
                ],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut default_set = BTreeSet::new();
            default_set.insert(AttributeField::Email);
            expected.insert(None, default_set);

            let mut google_set = BTreeSet::new();
            google_set.insert(AttributeField::Email);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                google_set,
            );

            assert_eq!(validated.requested_attributes, expected);
        }

        #[test]
        fn test_try_from_duplicate_attributes() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["email".to_string(), "email".to_string()],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let mut expected = BTreeMap::new();
            let mut s = BTreeSet::new();
            s.insert(AttributeField::Email);
            expected.insert(None, s);

            assert_eq!(validated.requested_attributes, expected);
        }

        #[test]
        fn test_try_from_invalid_attribute() {
            let req = PrepareAttributeRequest {
                identity_number: 12345,
                origin: "example.com".to_string(),
                account_number: None,
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["invalid".to_string()],
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
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["invalid1".to_string(), "invalid2".to_string()],
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
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec!["email".to_string(), "invalid".to_string()],
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
                session_key: ByteBuf::from(vec![1, 2, 3]),
                requested_attributes: vec![],
            };

            let result = ValidatedPrepareAttributeRequest::try_from(req);
            let validated = result.expect("Should successfully validate");

            let expected: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeField>> =
                BTreeMap::new();
            assert_eq!(validated.requested_attributes, expected);
        }

        #[test]
        fn test_try_from_complex_scenario() {
            let req = PrepareAttributeRequest {
                identity_number: 67890,
                origin: "app.example.com".to_string(),
                account_number: Some(42),
                session_key: ByteBuf::from(vec![4, 5, 6, 7, 8]),
                requested_attributes: vec![
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
            default_set.insert(AttributeField::Name);
            expected.insert(None, default_set);

            let mut google_set = BTreeSet::new();
            google_set.insert(AttributeField::Email);
            google_set.insert(AttributeField::Name);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "google.com".to_string(),
                }),
                google_set,
            );

            let mut github_set = BTreeSet::new();
            github_set.insert(AttributeField::Email);
            expected.insert(
                Some(AttributeScope::OpenId {
                    issuer: "github.com".to_string(),
                }),
                github_set,
            );

            assert_eq!(validated.requested_attributes, expected);
        }
    }
}
