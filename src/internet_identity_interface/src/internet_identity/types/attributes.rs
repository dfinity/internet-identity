use crate::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, GetAccountError, Timestamp,
};
use candid::{CandidType, Principal};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

// TODO: Refer to the same constant as in `internet_identity::delegation::check_frontend_length`
pub const FRONTEND_HOSTNAME_MAX_BYTES: usize = 255;

pub const MAX_ATTRIBUTES_PER_REQUEST: usize = 100;

pub const ATTRIBUTE_VALUE_MAX_BYTES: usize = 50_000;

pub const OPENID_ISSUER_MAX_BYTES: usize = 1024;

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

/// Returns a possibly modified version of `s` that fits within the specified bounds (in terms of
/// the number of bytes, not UTF-8 characters).
///
/// More precisely, end characters are removed such that the return value has at most `max_bytes`
/// bytes. Some examples:
/// ```
/// println!("{}", ellipsized("abcdef", 5));   // ab...
/// println!("{}", ellipsized("abcde", 5));    // abcde
/// println!("{}", ellipsized("abcd", 5));     // abcd
/// println!("{}", ellipsized("y̆zy̆", 4));      // y̆zy
/// println!("{}", ellipsized("y̆zy̆ooooo", 4)); // y...
/// ```
///
/// **Note**: This function respects UTF-8 character boundaries to ensure valid UTF-8 output,
/// but does not respect grapheme cluster boundaries. This means combining diacritics or other
/// multi-codepoint characters may be split, resulting in output that is not a visual prefix of
/// the input (e.g., "y̆" may be truncated to just "y").
pub fn ellipsized(s: &str, max_bytes: usize) -> String {
    // Fast path: already fits.
    if s.len() <= max_bytes {
        return s.to_owned();
    }

    // Edge case: not enough room for "..."
    if max_bytes <= 3 {
        return ".".repeat(max_bytes);
    }

    let keep_bytes = max_bytes - 3;

    // Find the largest prefix ≤ keep_bytes that ends on a UTF-8 boundary.
    let mut end = keep_bytes;
    while end > 0 && !s.is_char_boundary(end) {
        end -= 1;
    }

    let mut out = String::with_capacity(max_bytes);
    out.push_str(&s[..end]);
    out.push_str("...");
    out
}

/// https://openid.net/specs/openid-4-verifiable-credential-issuance-1_0.html#section-12.2.1
fn validate_openid_credential_issuer_identifier(issuer: &str) -> Result<(), String> {
    let mut problems = vec![];

    if issuer.is_empty() {
        problems.push("empty issuer".to_string());
    }

    if issuer.len() > OPENID_ISSUER_MAX_BYTES {
        problems.push(format!(
            "must not exceed {} bytes (got {} bytes)",
            OPENID_ISSUER_MAX_BYTES,
            issuer.len(),
        ));
    }

    if !issuer.starts_with("https://") {
        problems.push("must start with `https://`".to_string());
    }

    // This check is overly strict, but we keep it for now to avoid pulling in a URL parser.
    if issuer.contains("?") {
        problems.push("must not contain '?'".to_string());
    }

    // This check is overly strict, but we keep it for now to avoid pulling in a URL parser.
    if issuer.contains("#") {
        problems.push("must not contain '#'".to_string());
    }

    if !problems.is_empty() {
        return Err(problems.join(", "));
    }

    Ok(())
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

                validate_openid_credential_issuer_identifier(&issuer).map_err(|err| {
                    format!(
                        "Invalid issuer `{}` in attribute scope: {}",
                        ellipsized(&issuer, OPENID_ISSUER_MAX_BYTES),
                        err
                    )
                })?;

                Ok(AttributeScope::OpenId { issuer })
            }
            _ => Err(format!("Unknown attribute scope: {}", scope_str)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub struct AttributeKey {
    /// E.g., `Some("openid:https://google.com")` in "openid:https://google.com:email" or `None` in "name".
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

        if origin.len() > FRONTEND_HOSTNAME_MAX_BYTES {
            problems.push(format!(
                "Frontend hostname length {} exceeds limit of {} bytes",
                origin.len(),
                FRONTEND_HOSTNAME_MAX_BYTES
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
    pub attributes: Vec<(String, Vec<u8>)>,
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
    pub attributes: Vec<(String, Vec<u8>)>,
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub struct Attribute {
    pub key: AttributeKey,
    pub value: Vec<u8>,
}

impl TryFrom<(String, Vec<u8>)> for Attribute {
    type Error = String;

    fn try_from(value: (String, Vec<u8>)) -> Result<Self, Self::Error> {
        let (key, value) = value;

        let key = AttributeKey::try_from(key)?;

        if value.len() > ATTRIBUTE_VALUE_MAX_BYTES {
            return Err(format!(
                "Attribute value length {} exceeds limit of {} bytes",
                value.len(),
                ATTRIBUTE_VALUE_MAX_BYTES
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

        if origin.len() > FRONTEND_HOSTNAME_MAX_BYTES {
            problems.push(format!(
                "Frontend hostname length {} exceeds limit of {} bytes",
                origin.len(),
                FRONTEND_HOSTNAME_MAX_BYTES
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
    pub value: Vec<u8>,
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

    mod ellipsized_tests {
        use super::*;

        #[test]
        fn test_ellipsized() {
            let test_cases = vec![
                // Fast path: already fits
                ("already fits", "abcde", 5, "abcde"),
                ("shorter than limit", "abcd", 5, "abcd"),
                ("empty string", "", 5, ""),
                ("empty string with zero limit", "", 0, ""),
                // Needs truncation
                ("basic truncation", "abcdef", 5, "ab..."),
                ("truncate long string", "abcdefghij", 8, "abcde..."),
                // Edge cases with max_bytes <= 3
                ("max_bytes 0", "abc", 0, ""),
                ("max_bytes 1", "abc", 1, "."),
                ("max_bytes 2", "abc", 2, ".."),
                ("max_bytes 3", "abc", 3, "abc"),
                ("max_bytes 3 long string", "abcdef", 3, "..."),
                // UTF-8 boundary handling (combining diacritics)
                // y̆ is 'y' (1 byte) + combining breve U+0306 (2 bytes) = 3 bytes total
                // "y̆zy̆" = 7 bytes total
                ("utf8 boundary case 1", "y̆zy̆", 4, "y..."), // Only 'y' (1 byte) + '...' fits
                ("utf8 boundary case 2", "y̆zy̆", 7, "y̆zy̆"),  // Exactly fits
                ("utf8 boundary case 3", "y̆zy̆ooooo", 10, "y̆zy̆..."), // "y̆zy̆" (7 bytes) + "..." (3 bytes)
                // More UTF-8 tests
                ("emoji", "😀😀😀😀", 8, "😀..."), // Each emoji is 4 bytes
                ("mixed ascii and emoji", "hi😀😀", 6, "hi..."),
                ("japanese", "こんにちは", 9, "こん..."), // Each char is 3 bytes
                // Boundary condition at exactly the limit
                ("exactly at limit", "12345", 5, "12345"),
                ("one over limit", "123456", 5, "12..."),
            ];

            for (label, input, max_bytes, expected) in test_cases {
                let result = ellipsized(input, max_bytes);
                pretty_assert_eq!(
                    result,
                    expected,
                    "Failed test case: {} (input: {:?}, max_bytes: {})",
                    label,
                    input,
                    max_bytes
                );
            }
        }

        #[test]
        fn test_ellipsized_preserves_utf8() {
            // Ensure we never produce invalid UTF-8
            let test_strings = vec![
                "hello world",
                "こんにちは世界",
                "Hello 世界 🌍",
                "y̆zy̆",
                "Ñoño",
                "Zürich",
            ];

            for s in test_strings {
                for max_bytes in 0..=s.len() + 5 {
                    let result = ellipsized(s, max_bytes);
                    // Should always be valid UTF-8
                    assert!(
                        result.is_ascii() || std::str::from_utf8(result.as_bytes()).is_ok(),
                        "ellipsized produced invalid UTF-8 for input: {:?}, max_bytes: {}",
                        s,
                        max_bytes
                    );
                    // Should not exceed max_bytes
                    assert!(
                        result.len() <= max_bytes,
                        "ellipsized exceeded max_bytes: {} > {} for input: {:?}",
                        result.len(),
                        max_bytes,
                        s
                    );
                }
            }
        }
    }

    mod attribute_key_tests {
        use super::*;

        #[test]
        fn test_attribute_name_conversions() {
            let test_cases = vec![
                ("email", "email", Ok(AttributeName::Email)),
                ("name", "name", Ok(AttributeName::Name)),
                (
                    "unknown",
                    "unknown",
                    Err("Unknown attribute: unknown".to_string()),
                ),
                ("empty", "", Err("Unknown attribute: ".to_string())),
            ];

            for (label, input, expected) in test_cases {
                let result = AttributeName::try_from(input);
                pretty_assert_eq!(result, expected, "Failed test case: {}", label);
            }
        }

        #[test]
        fn test_attribute_name_display() {
            let test_cases = vec![
                ("email", AttributeName::Email, "email"),
                ("name", AttributeName::Name, "name"),
            ];

            for (label, input, expected) in test_cases {
                pretty_assert_eq!(input.to_string(), expected, "Failed test case: {}", label);
            }
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
        fn test_attribute_scope_conversions() {
            // Create owned strings for dynamic test cases to avoid lifetime issues
            let max_length_issuer = format!("https://{}", "a".repeat(OPENID_ISSUER_MAX_BYTES - 8));
            let max_length_input = format!("openid:{}", max_length_issuer);

            let too_long_issuer = format!("https://{}", "a".repeat(OPENID_ISSUER_MAX_BYTES - 7));
            let too_long_input = format!("openid:{}", too_long_issuer);
            let too_long_error = format!(
                "Invalid issuer `{}...` in attribute scope: must not exceed 1024 bytes (got 1025 bytes)",
                &too_long_issuer[..OPENID_ISSUER_MAX_BYTES - "...".len()],
            );

            let test_cases: Vec<(&str, &str, Result<AttributeScope, String>)> = vec![
                (
                    "openid",
                    "openid:https://google.com",
                    Ok(AttributeScope::OpenId {
                        issuer: "https://google.com".to_string(),
                    }),
                ),
                (
                    "openid complex issuer",
                    "openid:https://accounts.google.com",
                    Ok(AttributeScope::OpenId {
                        issuer: "https://accounts.google.com".to_string(),
                    }),
                ),
                (
                    "openid with colon in issuer",
                    "openid:https://issuer:with:colons",
                    Ok(AttributeScope::OpenId {
                        issuer: "https://issuer:with:colons".to_string(),
                    }),
                ),
                (
                    "openid extra colon",
                    "openid::",
                    Err("Invalid issuer `:` in attribute scope: must start with `https://`".to_string()),
                ),
                (
                    "openid missing issuer",
                    "openid:",
                    Err("Invalid issuer `` in attribute scope: empty issuer, must start with `https://`".to_string()),
                ),
                (
                    "openid no colon",
                    "openid",
                    Err("Missing issuer in attribute scope: openid".to_string()),
                ),
                (
                    "unknown scope",
                    "unknown:issuer",
                    Err("Unknown attribute scope: unknown".to_string()),
                ),
                // Test https:// requirement
                (
                    "http instead of https",
                    "openid:http://google.com",
                    Err("Invalid issuer `http://google.com` in attribute scope: must start with `https://`".to_string()),
                ),
                (
                    "no protocol",
                    "openid:google.com",
                    Err("Invalid issuer `google.com` in attribute scope: must start with `https://`".to_string()),
                ),
                // Test query parameter rejection
                (
                    "issuer with query parameter",
                    "openid:https://google.com?param=value",
                    Err("Invalid issuer `https://google.com?param=value` in attribute scope: must not contain '?'".to_string()),
                ),
                (
                    "issuer with multiple query parameters",
                    "openid:https://google.com?foo=bar&baz=qux",
                    Err("Invalid issuer `https://google.com?foo=bar&baz=qux` in attribute scope: must not contain '?'".to_string()),
                ),
                // Test fragment rejection
                (
                    "issuer with fragment",
                    "openid:https://google.com#section",
                    Err("Invalid issuer `https://google.com#section` in attribute scope: must not contain '#'".to_string()),
                ),
                (
                    "issuer with query and fragment",
                    "openid:https://google.com?param=value#section",
                    Err("Invalid issuer `https://google.com?param=value#section` in attribute scope: must not contain '?', must not contain '#'".to_string()),
                ),
                // Test IPv6 address support
                (
                    "issuer with IPv6 address",
                    "openid:https://[2001:db8::1]:8080",
                    Ok(AttributeScope::OpenId {
                        issuer: "https://[2001:db8::1]:8080".to_string(),
                    }),
                ),
                (
                    "issuer with IPv6 localhost",
                    "openid:https://[::1]",
                    Ok(AttributeScope::OpenId {
                        issuer: "https://[::1]".to_string(),
                    }),
                ),
                // Test length limit
                (
                    "issuer at max length",
                    &max_length_input,
                    Ok(AttributeScope::OpenId {
                        issuer: max_length_issuer.clone(),
                    }),
                ),
                (
                    "issuer exceeds max length",
                    &too_long_input,
                    Err(too_long_error.clone()),
                ),
            ];

            for (label, input, expected) in test_cases {
                let result = AttributeScope::try_from(input);
                pretty_assert_eq!(result, expected, "Failed test case: {}", label);
            }
        }

        #[test]
        fn test_attribute_scope_display() {
            let scope = AttributeScope::OpenId {
                issuer: "https://google.com".to_string(),
            };
            pretty_assert_eq!(scope.to_string(), "openid:https://google.com");
        }

        #[test]
        fn test_ordering() {
            let scope1 = AttributeScope::OpenId {
                issuer: "https://a.com".to_string(),
            };
            let scope2 = AttributeScope::OpenId {
                issuer: "https://b.com".to_string(),
            };
            assert!(scope1 < scope2);
        }
    }

    // AttributeRequest Tests
    mod attribute_request_tests {
        use super::*;

        #[test]
        fn test_attribute_key_conversions() {
            let test_cases = vec![
                (
                    "key only",
                    "email",
                    Ok(AttributeKey {
                        scope: None,
                        attribute_name: AttributeName::Email,
                    }),
                ),
                (
                    "with scope",
                    "openid:https://google.com:email",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::OpenId {
                            issuer: "https://google.com".to_string(),
                        }),
                        attribute_name: AttributeName::Email,
                    }),
                ),
                (
                    "complex issuer",
                    "openid:https://accounts.google.com:name",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::OpenId {
                            issuer: "https://accounts.google.com".to_string(),
                        }),
                        attribute_name: AttributeName::Name,
                    }),
                ),
                (
                    "issuer with colons",
                    "openid:https://issuer:with:colons:email",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::OpenId {
                            issuer: "https://issuer:with:colons".to_string(),
                        }),
                        attribute_name: AttributeName::Email,
                    }),
                ),
                (
                    "invalid key",
                    "openid:https://google.com:invalid",
                    Err("Unknown attribute: invalid".to_string()),
                ),
                (
                    "invalid scope",
                    "unknown:https://issuer:email",
                    Err("Unknown attribute scope: unknown".to_string()),
                ),
                ("empty", "", Err("Unknown attribute: ".to_string())),
            ];

            for (label, input, expected) in test_cases {
                let result = AttributeKey::try_from(input.to_string());
                pretty_assert_eq!(result, expected, "Failed test case: {}", label);
            }
        }

        #[test]
        fn test_attribute_key_display_and_round_trip() {
            let test_cases = vec![
                ("key only", "name"),
                ("with scope", "openid:https://google.com:email"),
            ];

            for (label, input) in test_cases {
                let key = AttributeKey::try_from(input.to_string()).unwrap();
                pretty_assert_eq!(key.to_string(), input, "Failed test case: {}", label);
            }
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
                    issuer: "https://google.com".to_string(),
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
        fn test_attribute_conversions() {
            let long_value = "x".repeat(ATTRIBUTE_VALUE_MAX_BYTES + 1);
            let long_value_len = long_value.len();

            let test_cases = vec![
                (
                    "valid",
                    ("email".to_string(), b"user@example.com".to_vec()),
                    Ok(Attribute {
                        key: AttributeKey::try_from("email".to_string()).unwrap(),
                        value: b"user@example.com".to_vec(),
                    }),
                ),
                (
                    "invalid key",
                    ("invalid".to_string(), b"value".to_vec()),
                    Err("Unknown attribute: invalid".to_string()),
                ),
                (
                    "value too long",
                    ("email".to_string(), long_value.into_bytes()),
                    Err(format!(
                        "Attribute value length {} exceeds limit of {} bytes",
                        long_value_len, ATTRIBUTE_VALUE_MAX_BYTES
                    )),
                ),
            ];

            for (label, input, expected) in test_cases {
                let result = Attribute::try_from(input);
                pretty_assert_eq!(result, expected, "Failed test case: {}", label);
            }
        }
    }

    mod validated_get_attributes_request_tests {
        use super::*;

        #[test]
        fn test_try_from_valid_get_attributes_requests() {
            let test_cases = vec![
                (
                    "multiple scopes",
                    GetAttributesRequest {
                        identity_number: 987,
                        origin: "example.com".to_string(),
                        account_number: Some(7),
                        issued_at_timestamp_ns: 42,
                        attributes: vec![
                            ("email".to_string(), b"user@example.com".to_vec()),
                            (
                                "openid:https://google.com:email".to_string(),
                                b"google@example.com".to_vec(),
                            ),
                        ],
                    },
                    (987, Some(7), 42, {
                        let mut m = BTreeMap::new();
                        m.insert(None, {
                            let mut s = BTreeSet::new();
                            s.insert(
                                Attribute::try_from((
                                    "email".to_string(),
                                    b"user@example.com".to_vec(),
                                ))
                                .unwrap(),
                            );
                            s
                        });
                        m.insert(
                            Some(AttributeScope::OpenId {
                                issuer: "https://google.com".to_string(),
                            }),
                            {
                                let mut s = BTreeSet::new();
                                s.insert(
                                    Attribute::try_from((
                                        "openid:https://google.com:email".to_string(),
                                        b"google@example.com".to_vec(),
                                    ))
                                    .unwrap(),
                                );
                                s
                            },
                        );
                        m
                    }),
                ),
                (
                    "deduplicates attributes",
                    GetAttributesRequest {
                        identity_number: 111,
                        origin: "example.com".to_string(),
                        account_number: None,
                        issued_at_timestamp_ns: 1,
                        attributes: vec![
                            ("email".to_string(), b"alias".to_vec()),
                            ("email".to_string(), b"alias".to_vec()),
                        ],
                    },
                    (111, None, 1, {
                        let mut m = BTreeMap::new();
                        let mut attrs = BTreeSet::new();
                        attrs.insert(
                            Attribute::try_from(("email".to_string(), b"alias".to_vec())).unwrap(),
                        );
                        m.insert(None, attrs);
                        m
                    }),
                ),
            ];

            for (label, input, expected) in test_cases {
                let validated = ValidatedGetAttributesRequest::try_from(input).expect(label);
                pretty_assert_eq!(
                    (
                        validated.identity_number,
                        validated.account_number,
                        validated.issued_at_timestamp_ns,
                        validated.attributes
                    ),
                    expected,
                    "Failed test case: {}",
                    label
                );
            }
        }

        #[test]
        fn test_try_from_invalid_get_attributes_requests() {
            let long_origin = "x".repeat(FRONTEND_HOSTNAME_MAX_BYTES + 1);
            let long_value = "y".repeat(ATTRIBUTE_VALUE_MAX_BYTES + 1);
            let long_value_len = long_value.len();

            let test_cases = vec![
                (
                    "validation errors combined",
                    GetAttributesRequest {
                        identity_number: 222,
                        origin: long_origin.clone(),
                        account_number: None,
                        issued_at_timestamp_ns: 2,
                        attributes: vec![
                            ("invalid".to_string(), b"value".to_vec()),
                            ("email".to_string(), long_value.into_bytes()),
                        ],
                    },
                    vec![
                        format!(
                            "Frontend hostname length {} exceeds limit of {} bytes",
                            long_origin.len(),
                            FRONTEND_HOSTNAME_MAX_BYTES
                        ),
                        "Unknown attribute: invalid".to_string(),
                        format!(
                            "Attribute value length {} exceeds limit of {} bytes",
                            long_value_len, ATTRIBUTE_VALUE_MAX_BYTES
                        ),
                    ],
                ),
                (
                    "too many attributes",
                    GetAttributesRequest {
                        identity_number: 333,
                        origin: "example.com".to_string(),
                        account_number: None,
                        issued_at_timestamp_ns: 3,
                        attributes: (0..=MAX_ATTRIBUTES_PER_REQUEST)
                            .map(|i| ("email".to_string(), format!("value-{i}").into_bytes()))
                            .collect::<Vec<_>>(),
                    },
                    vec![format!(
                        "Number of attributes {} exceeds limit of {}",
                        MAX_ATTRIBUTES_PER_REQUEST + 1,
                        MAX_ATTRIBUTES_PER_REQUEST
                    )],
                ),
            ];

            for (label, input, expected_problems) in test_cases {
                let err = ValidatedGetAttributesRequest::try_from(input).unwrap_err();
                match err {
                    GetAttributesError::ValidationError { problems } => {
                        // For the "too many attributes" case, we might have more problems if the values were also invalid,
                        // but here we just check for inclusion or exact match.
                        if label == "too many attributes" {
                            assert!(
                                problems.iter().any(|p| p == &expected_problems[0]),
                                "Failed test case: {}",
                                label
                            );
                        } else {
                            pretty_assert_eq!(
                                problems,
                                expected_problems,
                                "Failed test case: {}",
                                label
                            );
                        }
                    }
                    other => panic!("Expected validation error for {}, got {:?}", label, other),
                }
            }
        }
    }

    // ValidatedPrepareAttributeRequest Tests
    mod validated_prepare_attribute_request_tests {
        use super::*;

        #[test]
        fn test_try_from_valid_prepare_attribute_requests() {
            let test_cases = vec![
                (
                    "single attribute",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["email".to_string()],
                    },
                    (12345, "example.com".to_string(), None, {
                        let mut m = BTreeMap::new();
                        let mut s = BTreeSet::new();
                        s.insert(AttributeName::Email);
                        m.insert(None, s);
                        m
                    }),
                ),
                (
                    "multiple attributes same scope",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: Some(1),
                        attribute_keys: vec!["email".to_string(), "name".to_string()],
                    },
                    (12345, "example.com".to_string(), Some(1), {
                        let mut m = BTreeMap::new();
                        let mut s = BTreeSet::new();
                        s.insert(AttributeName::Email);
                        s.insert(AttributeName::Name);
                        m.insert(None, s);
                        m
                    }),
                ),
                (
                    "multiple attributes different scopes",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec![
                            "email".to_string(),
                            "openid:https://google.com:email".to_string(),
                        ],
                    },
                    (12345, "example.com".to_string(), None, {
                        let mut m = BTreeMap::new();
                        let mut default_set = BTreeSet::new();
                        default_set.insert(AttributeName::Email);
                        m.insert(None, default_set);
                        let mut google_set = BTreeSet::new();
                        google_set.insert(AttributeName::Email);
                        m.insert(
                            Some(AttributeScope::OpenId {
                                issuer: "https://google.com".to_string(),
                            }),
                            google_set,
                        );
                        m
                    }),
                ),
                (
                    "duplicate attributes",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["email".to_string(), "email".to_string()],
                    },
                    (12345, "example.com".to_string(), None, {
                        let mut m = BTreeMap::new();
                        let mut s = BTreeSet::new();
                        s.insert(AttributeName::Email);
                        m.insert(None, s);
                        m
                    }),
                ),
                (
                    "empty attributes",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec![],
                    },
                    (12345, "example.com".to_string(), None, BTreeMap::new()),
                ),
                (
                    "complex scenario",
                    PrepareAttributeRequest {
                        identity_number: 67890,
                        origin: "app.example.com".to_string(),
                        account_number: Some(42),
                        attribute_keys: vec![
                            "name".to_string(),
                            "openid:https://google.com:email".to_string(),
                            "openid:https://google.com:name".to_string(),
                            "openid:https://github.com:email".to_string(),
                        ],
                    },
                    (67890, "app.example.com".to_string(), Some(42), {
                        let mut m = BTreeMap::new();
                        let mut default_set = BTreeSet::new();
                        default_set.insert(AttributeName::Name);
                        m.insert(None, default_set);
                        let mut google_set = BTreeSet::new();
                        google_set.insert(AttributeName::Email);
                        google_set.insert(AttributeName::Name);
                        m.insert(
                            Some(AttributeScope::OpenId {
                                issuer: "https://google.com".to_string(),
                            }),
                            google_set,
                        );
                        let mut github_set = BTreeSet::new();
                        github_set.insert(AttributeName::Email);
                        m.insert(
                            Some(AttributeScope::OpenId {
                                issuer: "https://github.com".to_string(),
                            }),
                            github_set,
                        );
                        m
                    }),
                ),
            ];

            for (label, input, expected) in test_cases {
                let validated = ValidatedPrepareAttributeRequest::try_from(input).expect(label);
                pretty_assert_eq!(
                    (
                        validated.identity_number,
                        validated.origin,
                        validated.account_number,
                        validated.attribute_keys
                    ),
                    expected,
                    "Failed test case: {}",
                    label
                );
            }
        }

        #[test]
        fn test_try_from_invalid_prepare_attribute_requests() {
            let test_cases = vec![
                (
                    "invalid attribute",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["invalid".to_string()],
                    },
                    vec!["Unknown attribute: invalid".to_string()],
                ),
                (
                    "multiple invalid attributes",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["invalid1".to_string(), "invalid2".to_string()],
                    },
                    vec![
                        "Unknown attribute: invalid1".to_string(),
                        "Unknown attribute: invalid2".to_string(),
                    ],
                ),
                (
                    "mixed valid and invalid",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["email".to_string(), "invalid".to_string()],
                    },
                    vec!["Unknown attribute: invalid".to_string()],
                ),
            ];

            for (label, input, expected_problems) in test_cases {
                let err = ValidatedPrepareAttributeRequest::try_from(input).unwrap_err();
                match err {
                    PrepareAttributeError::ValidationError { problems } => {
                        pretty_assert_eq!(
                            problems,
                            expected_problems,
                            "Failed test case: {}",
                            label
                        );
                    }
                    other => panic!("Expected validation error for {}, got {:?}", label, other),
                }
            }
        }
    }
}
