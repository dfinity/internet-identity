use crate::internet_identity::types::{MetadataEntryV2, Timestamp};
use candid::{CandidType, Deserialize};
use std::collections::HashMap;

/// Types for OpenID credentials, used for OpenID sign in

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct OpenIdCredentialData {
    pub iss: String,
    pub sub: String,
    pub aud: String,
    pub last_usage_timestamp: Timestamp,
    pub metadata: HashMap<String, MetadataEntryV2>,
}
