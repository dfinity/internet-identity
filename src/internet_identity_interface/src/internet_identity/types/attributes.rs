use std::collections::{BTreeMap, BTreeSet};

use candid::CandidType;
use serde::{Deserialize, Serialize};

use crate::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, SessionKey, Timestamp,
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttributeScope {
    Default,
    OpenId { issuer: String },
}

impl TryFrom<String> for AttributeScope {
    type Error = String;

    /// Splits by ':', setting the attribute scope to the first component and processing
    /// possible additional attributes as needed.
    ///
    /// The special value `AttributeScope::Default` cannot be constructed via this method
    /// and needs to be constructed directly.
    fn try_from(value: String) -> Result<Self, Self::Error> {
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
                Ok(AttributeScope::OpenId { issuer })
            }
            _ => Err(format!("Unknown attribute scope: {}", scope_str)),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct AttributeRequest {
    pub scope: AttributeScope,
    pub field: AttributeField,
}

impl TryFrom<String> for AttributeRequest {
    type Error = String;

    /// Splits by ':', setting the attribute scope to all components by the last, and setting
    /// the attribute name to the second component.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut parts = value.rsplitn(2, ':');
        let name = parts
            .next()
            .ok_or_else(|| format!("Invalid attribute request: {}", value))?
            .to_string();
        let name = AttributeField::try_from(name.as_str())?;
        let scope_str = parts
            .next()
            .ok_or_else(|| format!("Invalid attribute request: {}", value))?;
        let scope = AttributeScope::try_from(scope_str.to_string())?;

        Ok(AttributeRequest { scope, field: name })
    }
}

impl std::fmt::Display for AttributeRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.scope {
            AttributeScope::Default => write!(f, "{}", self.field),
            AttributeScope::OpenId { issuer } => {
                write!(f, "openid:{}:{}", issuer, self.field)
            }
        }
    }
}

#[derive(CandidType, Deserialize)]
pub struct PrepareAttributeRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
    pub attributes: Vec<String>,
}

pub struct ValidatedPrepareAttributeRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
    pub attributes: BTreeMap<AttributeScope, BTreeSet<AttributeField>>,
}

impl TryFrom<PrepareAttributeRequest> for ValidatedPrepareAttributeRequest {
    type Error = String;

    fn try_from(value: PrepareAttributeRequest) -> Result<Self, Self::Error> {
        let PrepareAttributeRequest {
            anchor_number,
            origin,
            account_number,
            session_key,
            attributes: unparsed_attributes,
        } = value;

        let mut attributes = BTreeMap::new();
        let mut errors = Vec::new();

        for unparsed_attribute in unparsed_attributes {
            let AttributeRequest { scope, field } = match unparsed_attribute.try_into() {
                Ok(attr) => attr,
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            };
            attributes
                .entry(scope)
                .or_insert_with(BTreeSet::new)
                .insert(field);
        }

        if !errors.is_empty() {
            return Err(errors.join(", "));
        }

        Ok(Self {
            anchor_number,
            origin,
            account_number,
            session_key,
            attributes,
        })
    }
}

#[derive(CandidType, Serialize)]
pub struct PrepareAttributeResponse {
    pub issued_at_timestamp_ns: Timestamp,
    pub certified_attributes: Vec<String>,
}
