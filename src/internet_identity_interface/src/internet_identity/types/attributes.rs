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

/// Upper bound on the `<domain>` component of an `sso:<domain>` attribute
/// scope. 253 mirrors the RFC 1035 DNS name length; we do not require a
/// fully-qualified DNS name, only a reasonable cap.
pub const SSO_DOMAIN_MAX_BYTES: usize = 253;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub enum AttributeName {
    Email,
    Name,
    VerifiedEmail,
}

impl AttributeName {
    /// Returns all known attribute name variants.
    pub const fn all() -> &'static [AttributeName] {
        &[
            AttributeName::Email,
            AttributeName::Name,
            AttributeName::VerifiedEmail,
        ]
    }
}

impl TryFrom<&str> for AttributeName {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "email" => Ok(AttributeName::Email),
            "name" => Ok(AttributeName::Name),
            "verified_email" => Ok(AttributeName::VerifiedEmail),
            _ => Err(format!("Unknown attribute: {}", value)),
        }
    }
}

impl std::fmt::Display for AttributeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeName::Email => write!(f, "email"),
            AttributeName::Name => write!(f, "name"),
            AttributeName::VerifiedEmail => write!(f, "verified_email"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub enum AttributeScope {
    OpenId { issuer: String },
    Sso { domain: String },
}

impl std::fmt::Display for AttributeScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeScope::OpenId { issuer } => write!(f, "openid:{}", issuer),
            AttributeScope::Sso { domain } => write!(f, "sso:{}", domain),
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

    if !issuer.starts_with("https://") && !issuer.starts_with("http://localhost:") {
        problems.push("must start with `https://` or `http://localhost:`".to_string());
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

/// Format-only validation for the `<domain>` component of an `sso:<domain>`
/// attribute scope. Intentionally not coupled to the SSO canary allowlist
/// (`allowed_discovery_domains()` in the canister): a credential whose SSO
/// domain is no longer allowed should still be parseable, and the scope
/// parser has no access to canister runtime state anyway. A scope that does
/// not match any registered SSO provider simply yields no attributes.
fn validate_sso_domain(domain: &str) -> Result<(), String> {
    let mut problems = vec![];

    if domain.is_empty() {
        problems.push("empty domain".to_string());
    }

    if domain.len() > SSO_DOMAIN_MAX_BYTES {
        problems.push(format!(
            "must not exceed {} bytes (got {} bytes)",
            SSO_DOMAIN_MAX_BYTES,
            domain.len(),
        ));
    }

    // Disallow ':' so that the `rsplitn(2, ':')` attribute-key parser can
    // unambiguously split scope from attribute name.
    if domain.contains(':') {
        problems.push("must not contain ':'".to_string());
    }

    if domain.chars().any(|c| c.is_whitespace()) {
        problems.push("must not contain whitespace".to_string());
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
    /// Supported forms:
    /// - `openid:<issuer>`  (e.g. `openid:https://accounts.google.com`)
    /// - `sso:<domain>`     (e.g. `sso:dfinity.org`)
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
            "sso" => {
                let domain = parts
                    .next()
                    .ok_or_else(|| format!("Missing domain in attribute scope: {}", value))?
                    .to_string();

                validate_sso_domain(&domain).map_err(|err| {
                    format!(
                        "Invalid domain `{}` in attribute scope: {}",
                        ellipsized(&domain, SSO_DOMAIN_MAX_BYTES),
                        err
                    )
                })?;

                // SSO domains are DNS hostnames — normalize to lowercase so
                // `sso:DFINITY.ORG:email` and `sso:dfinity.org:email` match
                // the same credential. This also aligns with
                // `openid::generic::is_allowed_discovery_domain`, which
                // already compares case-insensitively.
                let domain = domain.to_ascii_lowercase();

                Ok(AttributeScope::Sso { domain })
            }
            _ => Err(format!("Unknown attribute scope: {}", scope_str)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize)]
pub struct AttributeKey {
    /// E.g., `Some("openid:https://google.com")` in `"openid:https://google.com:email"`,
    /// `Some("sso:dfinity.org")` in `"sso:dfinity.org:email"`, or `None` in `"name"`.
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

#[derive(CandidType, Debug, Deserialize)]
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

/// Problem message appended by the legacy attribute-sharing validators when a
/// request carries an `sso:<domain>` scoped key.
///
/// The legacy flow (`prepare_attributes`, `get_attributes`,
/// `list_available_attributes`) is deprecated and will not gain new
/// functionality. All `sso:<domain>` attribute requests must go through the
/// ICRC-3 flow (`prepare_icrc3_attributes` / `get_icrc3_attributes`) instead.
pub const LEGACY_SSO_SCOPE_REJECTION: &str =
    "sso:<domain> attribute scope is not supported by the legacy attribute-sharing flow; \
     use the ICRC-3 attribute flow (prepare_icrc3_attributes / get_icrc3_attributes) instead";

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
            if matches!(scope, Some(AttributeScope::Sso { .. })) {
                problems.push(LEGACY_SSO_SCOPE_REJECTION.to_string());
                continue;
            }
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

#[derive(CandidType, Debug, Deserialize)]
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
            if matches!(attribute.key.scope, Some(AttributeScope::Sso { .. })) {
                problems.push(LEGACY_SSO_SCOPE_REJECTION.to_string());
                continue;
            }
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

#[derive(Clone, Debug, PartialEq, CandidType, Serialize, Deserialize, Eq, PartialOrd, Ord)]
pub struct CertifiedAttribute {
    pub key: String,
    pub value: Vec<u8>,
    pub signature: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, CandidType, Serialize, Deserialize)]
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

// ==================== ICRC-3 attribute sharing types ====================

/// Maximum length (in bytes) of an `AttributeName` when rendered as a string.
///
/// Currently, this is the length of `"VerifiedEmail"`, the longest variant.
/// If new, longer variants are added to `AttributeName`, this constant must be
/// updated accordingly.
pub const ATTRIBUTE_NAME_MAX_BYTES: usize = "VerifiedEmail".len();

/// Additional bytes added when an attribute name is scoped as an OpenID-style
/// key of the form: `"openid:" + <issuer> + ":" + <attribute_name>`.
///
/// This includes:
///  * the `"openid:"` prefix,
///  * one separator `":"` between the issuer and the attribute name,
///  * the attribute name itself (bounded by `ATTRIBUTE_NAME_MAX_BYTES`).
pub const OPENID_ATTRIBUTE_KEY_OVERHEAD_BYTES: usize =
    "openid:".len() + 1 + ATTRIBUTE_NAME_MAX_BYTES;

/// Maximum size of an attribute key in the ICRC-3 message.
///
/// Attribute keys may include issuer strings (bounded by
/// `OPENID_ISSUER_MAX_BYTES`) and additional OpenID scoping overhead
/// (`"openid:"` prefix, separators, and the attribute name). This constant
/// provides a true upper bound for fully-scoped attribute keys.
pub const ICRC3_ATTRIBUTE_KEY_MAX_BYTES: usize =
    OPENID_ISSUER_MAX_BYTES + OPENID_ATTRIBUTE_KEY_OVERHEAD_BYTES;

/// Approximate Candid encoding overhead per attribute (length prefixes,
/// type information, variants, etc.). This is a conservative upper bound:
/// it does not need to be tight, only large enough so that
/// `ICRC3_MESSAGE_MAX_BYTES` is a sound limit.
pub const ICRC3_ATTRIBUTE_CANDID_OVERHEAD_BYTES: usize = 128;

/// Approximate Candid encoding overhead for the whole ICRC-3 message map
/// (type table, map header, etc.).
pub const ICRC3_MESSAGE_CANDID_OVERHEAD_BYTES: usize = 1024;

/// Maximum size of the ICRC-3 message blob (Candid-encoded ICRC-3 Value map).
///
/// This bound accounts for:
///  * up to `MAX_ATTRIBUTES_PER_REQUEST` attributes
///  * each attribute's key (up to `ICRC3_ATTRIBUTE_KEY_MAX_BYTES` bytes)
///  * each attribute's value (up to `ATTRIBUTE_VALUE_MAX_BYTES` bytes)
///  * per-attribute and per-message Candid encoding overhead.
pub const ICRC3_MESSAGE_MAX_BYTES: usize = ICRC3_MESSAGE_CANDID_OVERHEAD_BYTES
    + MAX_ATTRIBUTES_PER_REQUEST
        * (ICRC3_ATTRIBUTE_KEY_MAX_BYTES
            + ATTRIBUTE_VALUE_MAX_BYTES
            + ICRC3_ATTRIBUTE_CANDID_OVERHEAD_BYTES);

/// A specification of an attribute to be certified.
#[derive(CandidType, Debug, Deserialize, Clone)]
pub struct AttributeSpec {
    /// `attribute_scope:attribute_name`, e.g. `openid:https://accounts.google.com:email`
    pub key: String,
    /// If set, only certify if the current value matches.
    pub value: Option<Vec<u8>>,
    /// Whether to omit the scope prefix in the certified attribute key.
    pub omit_scope: bool,
}

/// Validated and parsed attribute spec for internal use.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValidatedAttributeSpec {
    pub key: AttributeKey,
    pub value: Option<Vec<u8>>,
    pub omit_scope: bool,
}

pub const ICRC3_NONCE_BYTES: usize = 32;

#[derive(CandidType, Debug, Deserialize)]
pub struct PrepareIcrc3AttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub attributes: Vec<AttributeSpec>,
    pub nonce: Vec<u8>,
}

#[derive(Debug)]
pub struct ValidatedPrepareIcrc3AttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub attributes: Vec<ValidatedAttributeSpec>,
    pub nonce: Vec<u8>,
}

impl TryFrom<PrepareIcrc3AttributeRequest> for ValidatedPrepareIcrc3AttributeRequest {
    type Error = PrepareIcrc3AttributeError;

    fn try_from(value: PrepareIcrc3AttributeRequest) -> Result<Self, Self::Error> {
        let PrepareIcrc3AttributeRequest {
            identity_number,
            origin,
            account_number,
            attributes: unparsed_attributes,
            nonce,
        } = value;

        let mut problems = Vec::new();

        if nonce.len() != ICRC3_NONCE_BYTES {
            problems.push(format!(
                "Nonce must be exactly {} bytes, got {}",
                ICRC3_NONCE_BYTES,
                nonce.len()
            ));
        }

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

        let mut attributes = Vec::new();

        for spec in unparsed_attributes {
            let key = match AttributeKey::try_from(spec.key.clone()) {
                Ok(key) => key,
                Err(err) => {
                    problems.push(err);
                    continue;
                }
            };

            if let Some(ref value) = spec.value {
                if value.len() > ATTRIBUTE_VALUE_MAX_BYTES {
                    problems.push(format!(
                        "Attribute value length {} exceeds limit of {} bytes",
                        value.len(),
                        ATTRIBUTE_VALUE_MAX_BYTES
                    ));
                    continue;
                }
            }

            attributes.push(ValidatedAttributeSpec {
                key,
                value: spec.value,
                omit_scope: spec.omit_scope,
            });
        }

        if !problems.is_empty() {
            return Err(PrepareIcrc3AttributeError::ValidationError { problems });
        }

        Ok(Self {
            identity_number,
            origin,
            account_number,
            attributes,
            nonce,
        })
    }
}

#[derive(Clone, Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub struct PrepareIcrc3AttributeResponse {
    pub message: Vec<u8>,
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum PrepareIcrc3AttributeError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
    GetAccountError(GetAccountError),
    AttributeMismatch { problems: Vec<String> },
}

#[derive(CandidType, Debug, Deserialize)]
pub struct GetIcrc3AttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub message: Vec<u8>,
}

#[derive(Debug)]
pub struct ValidatedGetIcrc3AttributeRequest {
    pub identity_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub message: Vec<u8>,
}

impl TryFrom<GetIcrc3AttributeRequest> for ValidatedGetIcrc3AttributeRequest {
    type Error = GetIcrc3AttributeError;

    fn try_from(value: GetIcrc3AttributeRequest) -> Result<Self, Self::Error> {
        let GetIcrc3AttributeRequest {
            identity_number,
            origin,
            account_number,
            message,
        } = value;

        let mut problems = Vec::new();

        if origin.len() > FRONTEND_HOSTNAME_MAX_BYTES {
            problems.push(format!(
                "Frontend hostname length {} exceeds limit of {} bytes",
                origin.len(),
                FRONTEND_HOSTNAME_MAX_BYTES
            ));
        }

        if message.len() > ICRC3_MESSAGE_MAX_BYTES {
            problems.push(format!(
                "Message length {} exceeds limit of {} bytes",
                message.len(),
                ICRC3_MESSAGE_MAX_BYTES
            ));
        }

        if !problems.is_empty() {
            return Err(GetIcrc3AttributeError::ValidationError { problems });
        }

        Ok(Self {
            identity_number,
            origin,
            account_number,
            message,
        })
    }
}

#[derive(Clone, Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub struct GetIcrc3AttributeResponse {
    pub signature: Vec<u8>,
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum GetIcrc3AttributeError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
    GetAccountError(GetAccountError),
    NoSuchSignature,
}

// ── List available attributes ───────────────────────────────────────────────

#[derive(CandidType, Debug, Deserialize)]
pub struct ListAvailableAttributesRequest {
    pub identity_number: AnchorNumber,
    pub attributes: Option<Vec<String>>,
}

#[derive(Debug)]
pub struct ValidatedListAvailableAttributesRequest {
    pub identity_number: AnchorNumber,
    pub attributes: Option<Vec<AttributeKey>>,
}

impl TryFrom<ListAvailableAttributesRequest> for ValidatedListAvailableAttributesRequest {
    type Error = ListAvailableAttributesError;

    fn try_from(request: ListAvailableAttributesRequest) -> Result<Self, Self::Error> {
        let mut problems = Vec::new();

        let attributes = match request.attributes {
            None => None,
            Some(keys) => {
                if keys.len() > MAX_ATTRIBUTES_PER_REQUEST {
                    problems.push(format!(
                        "Too many attributes: {} (max {})",
                        keys.len(),
                        MAX_ATTRIBUTES_PER_REQUEST
                    ));
                }
                let mut parsed = Vec::with_capacity(keys.len().min(MAX_ATTRIBUTES_PER_REQUEST));
                for key in keys {
                    match AttributeKey::try_from(key) {
                        Ok(k) => {
                            if matches!(k.scope, Some(AttributeScope::Sso { .. })) {
                                problems.push(LEGACY_SSO_SCOPE_REJECTION.to_string());
                                continue;
                            }
                            parsed.push(k);
                        }
                        Err(e) => problems.push(e),
                    }
                }
                if !problems.is_empty() {
                    return Err(ListAvailableAttributesError::ValidationError { problems });
                }
                Some(parsed)
            }
        };

        Ok(ValidatedListAvailableAttributesRequest {
            identity_number: request.identity_number,
            attributes,
        })
    }
}

#[derive(Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum ListAvailableAttributesError {
    ValidationError { problems: Vec<String> },
    AuthorizationError(Principal),
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
                    Err("Invalid issuer `:` in attribute scope: must start with `https://` or `http://localhost:`".to_string()),
                ),
                (
                    "openid missing issuer",
                    "openid:",
                    Err("Invalid issuer `` in attribute scope: empty issuer, must start with `https://` or `http://localhost:`".to_string()),
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
                (
                    "http instead of https",
                    "openid:http://google.com",
                    Err("Invalid issuer `http://google.com` in attribute scope: must start with `https://` or `http://localhost:`".to_string()),
                ),
                (
                    "no protocol",
                    "openid:google.com",
                    Err("Invalid issuer `google.com` in attribute scope: must start with `https://` or `http://localhost:`".to_string()),
                ),
                (
                    "http localhost should be allowed",
                    "openid:http://localhost:8080",
                    Ok(AttributeScope::OpenId {
                        issuer: "http://localhost:8080".to_string(),
                    }),
                ),
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
        fn test_attribute_scope_sso_conversions() {
            let max_length_domain = "a".repeat(SSO_DOMAIN_MAX_BYTES);
            let max_length_input = format!("sso:{}", max_length_domain);

            let too_long_domain = "a".repeat(SSO_DOMAIN_MAX_BYTES + 1);
            let too_long_input = format!("sso:{}", too_long_domain);
            let too_long_error = format!(
                "Invalid domain `{}...` in attribute scope: must not exceed 253 bytes (got 254 bytes)",
                &too_long_domain[..SSO_DOMAIN_MAX_BYTES - "...".len()],
            );

            let test_cases: Vec<(&str, &str, Result<AttributeScope, String>)> = vec![
                (
                    "sso basic",
                    "sso:dfinity.org",
                    Ok(AttributeScope::Sso {
                        domain: "dfinity.org".to_string(),
                    }),
                ),
                (
                    "sso beta",
                    "sso:beta.dfinity.org",
                    Ok(AttributeScope::Sso {
                        domain: "beta.dfinity.org".to_string(),
                    }),
                ),
                (
                    "sso subdomain",
                    "sso:accounts.dfinity.org",
                    Ok(AttributeScope::Sso {
                        domain: "accounts.dfinity.org".to_string(),
                    }),
                ),
                (
                    // DNS hostnames are case-insensitive; the parser
                    // normalizes to lowercase so `sso:DFINITY.ORG` and
                    // `sso:dfinity.org` match the same credential.
                    "sso uppercase normalizes to lowercase",
                    "sso:DFINITY.ORG",
                    Ok(AttributeScope::Sso {
                        domain: "dfinity.org".to_string(),
                    }),
                ),
                (
                    "sso mixed case normalizes to lowercase",
                    "sso:Beta.DFinity.Org",
                    Ok(AttributeScope::Sso {
                        domain: "beta.dfinity.org".to_string(),
                    }),
                ),
                (
                    "sso missing domain",
                    "sso:",
                    Err("Invalid domain `` in attribute scope: empty domain".to_string()),
                ),
                (
                    "sso no colon",
                    "sso",
                    Err("Missing domain in attribute scope: sso".to_string()),
                ),
                (
                    "sso with whitespace",
                    "sso:bad domain",
                    Err(
                        "Invalid domain `bad domain` in attribute scope: must not contain whitespace"
                            .to_string(),
                    ),
                ),
                (
                    "sso at max length",
                    &max_length_input,
                    Ok(AttributeScope::Sso {
                        domain: max_length_domain.clone(),
                    }),
                ),
                (
                    "sso exceeds max length",
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

            let sso = AttributeScope::Sso {
                domain: "dfinity.org".to_string(),
            };
            pretty_assert_eq!(sso.to_string(), "sso:dfinity.org");
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
                (
                    "sso scope with email",
                    "sso:dfinity.org:email",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::Sso {
                            domain: "dfinity.org".to_string(),
                        }),
                        attribute_name: AttributeName::Email,
                    }),
                ),
                (
                    "sso scope with name",
                    "sso:dfinity.org:name",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::Sso {
                            domain: "dfinity.org".to_string(),
                        }),
                        attribute_name: AttributeName::Name,
                    }),
                ),
                (
                    // verified_email under sso: parses fine — it is silently
                    // dropped downstream by prepare_sso_attributes.
                    "sso scope with verified_email parses",
                    "sso:dfinity.org:verified_email",
                    Ok(AttributeKey {
                        scope: Some(AttributeScope::Sso {
                            domain: "dfinity.org".to_string(),
                        }),
                        attribute_name: AttributeName::VerifiedEmail,
                    }),
                ),
                (
                    "sso scope with invalid attribute",
                    "sso:dfinity.org:bogus",
                    Err("Unknown attribute: bogus".to_string()),
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
                (
                    "sso scope rejected by legacy flow",
                    GetAttributesRequest {
                        identity_number: 444,
                        origin: "example.com".to_string(),
                        account_number: None,
                        issued_at_timestamp_ns: 4,
                        attributes: vec![(
                            "sso:dfinity.org:email".to_string(),
                            b"user@dfinity.org".to_vec(),
                        )],
                    },
                    vec![LEGACY_SSO_SCOPE_REJECTION.to_string()],
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
                (
                    // The legacy flow rejects sso: scopes; they must go
                    // through the ICRC-3 flow instead.
                    "sso scope rejected by legacy flow",
                    PrepareAttributeRequest {
                        identity_number: 12345,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attribute_keys: vec!["sso:dfinity.org:email".to_string()],
                    },
                    vec![LEGACY_SSO_SCOPE_REJECTION.to_string()],
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

    // ICRC-3 Validation Tests
    mod validated_prepare_icrc3_attribute_request_tests {
        use super::*;

        fn make_spec(key: &str, value: Option<&[u8]>, omit_scope: bool) -> AttributeSpec {
            AttributeSpec {
                key: key.to_string(),
                value: value.map(|v| v.to_vec()),
                omit_scope,
            }
        }

        #[test]
        fn test_valid_requests() {
            let test_cases = vec![
                (
                    "single attribute with value",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![make_spec(
                            "openid:https://google.com:email",
                            Some(b"user@example.com"),
                            false,
                        )],
                        nonce: vec![0u8; 32],
                    },
                    1,
                ),
                (
                    "attribute without value",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![make_spec("openid:https://google.com:email", None, false)],
                        nonce: vec![0u8; 32],
                    },
                    1,
                ),
                (
                    "attribute with omit_scope",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![make_spec("openid:https://google.com:email", None, true)],
                        nonce: vec![0u8; 32],
                    },
                    1,
                ),
                (
                    "multiple attributes mixed options",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: Some(42),
                        attributes: vec![
                            make_spec(
                                "openid:https://google.com:email",
                                Some(b"user@example.com"),
                                true,
                            ),
                            make_spec("openid:https://google.com:name", None, false),
                        ],
                        nonce: vec![0u8; 32],
                    },
                    2,
                ),
                (
                    "empty attributes",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![],
                        nonce: vec![0u8; 32],
                    },
                    0,
                ),
            ];

            for (label, input, expected_count) in test_cases {
                let validated =
                    ValidatedPrepareIcrc3AttributeRequest::try_from(input).expect(label);
                pretty_assert_eq!(
                    validated.attributes.len(),
                    expected_count,
                    "Failed test case: {}",
                    label
                );
            }
        }

        #[test]
        fn test_omit_scope_preserved() {
            let request = PrepareIcrc3AttributeRequest {
                identity_number: 123,
                origin: "example.com".to_string(),
                account_number: None,
                attributes: vec![
                    make_spec("openid:https://google.com:email", None, true),
                    make_spec("openid:https://google.com:name", None, false),
                ],
                nonce: vec![0u8; 32],
            };
            let validated = ValidatedPrepareIcrc3AttributeRequest::try_from(request).unwrap();
            assert!(validated.attributes[0].omit_scope);
            assert!(!validated.attributes[1].omit_scope);
        }

        #[test]
        fn test_value_preserved() {
            let request = PrepareIcrc3AttributeRequest {
                identity_number: 123,
                origin: "example.com".to_string(),
                account_number: None,
                attributes: vec![
                    make_spec(
                        "openid:https://google.com:email",
                        Some(b"user@example.com"),
                        false,
                    ),
                    make_spec("openid:https://google.com:name", None, false),
                ],
                nonce: vec![0u8; 32],
            };
            let validated = ValidatedPrepareIcrc3AttributeRequest::try_from(request).unwrap();
            pretty_assert_eq!(
                validated.attributes[0].value,
                Some(b"user@example.com".to_vec())
            );
            pretty_assert_eq!(validated.attributes[1].value, None);
        }

        #[test]
        fn test_invalid_requests() {
            let long_origin = "x".repeat(FRONTEND_HOSTNAME_MAX_BYTES + 1);
            let long_value = vec![0u8; ATTRIBUTE_VALUE_MAX_BYTES + 1];

            let test_cases: Vec<(&str, PrepareIcrc3AttributeRequest, Vec<String>)> = vec![
                (
                    "origin too long",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: long_origin.clone(),
                        account_number: None,
                        attributes: vec![],
                        nonce: vec![0u8; 32],
                    },
                    vec![format!(
                        "Frontend hostname length {} exceeds limit of {} bytes",
                        long_origin.len(),
                        FRONTEND_HOSTNAME_MAX_BYTES
                    )],
                ),
                (
                    "too many attributes",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: (0..=MAX_ATTRIBUTES_PER_REQUEST)
                            .map(|_| make_spec("openid:https://google.com:email", None, false))
                            .collect(),
                        nonce: vec![0u8; 32],
                    },
                    vec![format!(
                        "Number of attributes {} exceeds limit of {}",
                        MAX_ATTRIBUTES_PER_REQUEST + 1,
                        MAX_ATTRIBUTES_PER_REQUEST
                    )],
                ),
                (
                    "invalid attribute key",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![make_spec("invalid_key", None, false)],
                        nonce: vec![0u8; 32],
                    },
                    vec!["Unknown attribute: invalid_key".to_string()],
                ),
                (
                    "value too long",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        attributes: vec![AttributeSpec {
                            key: "openid:https://google.com:email".to_string(),
                            value: Some(long_value.clone()),
                            omit_scope: false,
                        }],
                        nonce: vec![0u8; 32],
                    },
                    vec![format!(
                        "Attribute value length {} exceeds limit of {} bytes",
                        long_value.len(),
                        ATTRIBUTE_VALUE_MAX_BYTES
                    )],
                ),
                (
                    "multiple errors combined",
                    PrepareIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: long_origin.clone(),
                        account_number: None,
                        attributes: vec![make_spec("bad_key", None, false)],
                        nonce: vec![0u8; 32],
                    },
                    vec![
                        format!(
                            "Frontend hostname length {} exceeds limit of {} bytes",
                            long_origin.len(),
                            FRONTEND_HOSTNAME_MAX_BYTES
                        ),
                        "Unknown attribute: bad_key".to_string(),
                    ],
                ),
            ];

            for (label, input, expected_problems) in test_cases {
                let err = ValidatedPrepareIcrc3AttributeRequest::try_from(input).unwrap_err();
                match err {
                    PrepareIcrc3AttributeError::ValidationError { problems } => {
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

    mod validated_get_icrc3_attribute_request_tests {
        use super::*;

        #[test]
        fn test_valid_requests() {
            let test_cases = vec![
                (
                    "normal request",
                    GetIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        message: vec![1, 2, 3],
                    },
                ),
                (
                    "with account number",
                    GetIcrc3AttributeRequest {
                        identity_number: 456,
                        origin: "app.example.com".to_string(),
                        account_number: Some(7),
                        message: vec![0; 1000],
                    },
                ),
                (
                    "empty message",
                    GetIcrc3AttributeRequest {
                        identity_number: 789,
                        origin: "test.com".to_string(),
                        account_number: None,
                        message: vec![],
                    },
                ),
            ];

            for (label, input) in test_cases {
                let validated = ValidatedGetIcrc3AttributeRequest::try_from(input).expect(label);
                assert!(validated.identity_number > 0, "Failed test case: {}", label);
            }
        }

        #[test]
        fn test_invalid_requests() {
            let long_origin = "x".repeat(FRONTEND_HOSTNAME_MAX_BYTES + 1);

            let test_cases = vec![
                (
                    "origin too long",
                    GetIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: long_origin.clone(),
                        account_number: None,
                        message: vec![1, 2, 3],
                    },
                    vec![format!(
                        "Frontend hostname length {} exceeds limit of {} bytes",
                        long_origin.len(),
                        FRONTEND_HOSTNAME_MAX_BYTES
                    )],
                ),
                (
                    "message too large",
                    GetIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: "example.com".to_string(),
                        account_number: None,
                        message: vec![0; ICRC3_MESSAGE_MAX_BYTES + 1],
                    },
                    vec![format!(
                        "Message length {} exceeds limit of {} bytes",
                        ICRC3_MESSAGE_MAX_BYTES + 1,
                        ICRC3_MESSAGE_MAX_BYTES
                    )],
                ),
                (
                    "both errors combined",
                    GetIcrc3AttributeRequest {
                        identity_number: 123,
                        origin: long_origin.clone(),
                        account_number: None,
                        message: vec![0; ICRC3_MESSAGE_MAX_BYTES + 1],
                    },
                    vec![
                        format!(
                            "Frontend hostname length {} exceeds limit of {} bytes",
                            long_origin.len(),
                            FRONTEND_HOSTNAME_MAX_BYTES
                        ),
                        format!(
                            "Message length {} exceeds limit of {} bytes",
                            ICRC3_MESSAGE_MAX_BYTES + 1,
                            ICRC3_MESSAGE_MAX_BYTES
                        ),
                    ],
                ),
            ];

            for (label, input, expected_problems) in test_cases {
                let err = ValidatedGetIcrc3AttributeRequest::try_from(input).unwrap_err();
                match err {
                    GetIcrc3AttributeError::ValidationError { problems } => {
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

    mod validated_list_available_attributes_request_tests {
        use super::*;

        #[test]
        fn test_valid_requests() {
            // None means all attributes
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: None,
            };
            let validated = ValidatedListAvailableAttributesRequest::try_from(req).unwrap();
            assert!(validated.attributes.is_none());

            // Empty vec
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(vec![]),
            };
            let validated = ValidatedListAvailableAttributesRequest::try_from(req).unwrap();
            pretty_assert_eq!(validated.attributes.unwrap().len(), 0);

            // Scoped key
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(vec!["openid:https://accounts.google.com:email".to_string()]),
            };
            let validated = ValidatedListAvailableAttributesRequest::try_from(req).unwrap();
            let attrs = validated.attributes.unwrap();
            pretty_assert_eq!(attrs.len(), 1);
            assert!(attrs[0].scope.is_some());

            // Unscoped key — valid for list (unlike prepare)
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(vec!["email".to_string()]),
            };
            let validated = ValidatedListAvailableAttributesRequest::try_from(req).unwrap();
            let attrs = validated.attributes.unwrap();
            pretty_assert_eq!(attrs.len(), 1);
            assert!(attrs[0].scope.is_none());
            pretty_assert_eq!(attrs[0].attribute_name, AttributeName::Email);
        }

        #[test]
        fn test_invalid_requests() {
            // Too many attributes
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(
                    (0..MAX_ATTRIBUTES_PER_REQUEST + 1)
                        .map(|_| "email".to_string())
                        .collect(),
                ),
            };
            let err = ValidatedListAvailableAttributesRequest::try_from(req).unwrap_err();
            match err {
                ListAvailableAttributesError::ValidationError { problems } => {
                    assert!(problems[0].contains("Too many attributes"));
                }
                other => panic!("Expected ValidationError, got {:?}", other),
            }

            // Invalid key
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(vec!["unknown_attribute".to_string()]),
            };
            let err = ValidatedListAvailableAttributesRequest::try_from(req).unwrap_err();
            match err {
                ListAvailableAttributesError::ValidationError { problems } => {
                    assert!(problems[0].contains("Unknown attribute"));
                }
                other => panic!("Expected ValidationError, got {:?}", other),
            }

            // sso: scope is rejected by the legacy flow.
            let req = ListAvailableAttributesRequest {
                identity_number: 10000,
                attributes: Some(vec!["sso:dfinity.org:email".to_string()]),
            };
            let err = ValidatedListAvailableAttributesRequest::try_from(req).unwrap_err();
            match err {
                ListAvailableAttributesError::ValidationError { problems } => {
                    pretty_assert_eq!(problems, vec![LEGACY_SSO_SCOPE_REJECTION.to_string()]);
                }
                other => panic!("Expected ValidationError, got {:?}", other),
            }
        }
    }
}
