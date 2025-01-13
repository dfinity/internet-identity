use candid::{Deserialize, Principal};
use identity_jose::jws::Decoder;
use internet_identity_interface::internet_identity::types::{MetadataEntryV2, Timestamp};
use std::collections::HashMap;

mod google;

#[derive(Debug, PartialEq)]
pub struct OpenIdCredential {
    pub iss: String,
    pub sub: String,
    pub aud: String,
    pub principal: Principal,
    pub last_usage_timestamp: Timestamp,
    pub metadata: HashMap<String, MetadataEntryV2>,
}

#[derive(Deserialize)]
struct PartialClaims {
    iss: String,
}

pub fn setup_timers() {
    google::setup_timers();
}

#[allow(unused)]
pub fn verify(
    jwt: &str,
    session_principal: &Principal,
    session_salt: &[u8; 32],
    timestamp: Timestamp,
) -> Result<OpenIdCredential, String> {
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| "Failed to decode JWT")?;
    let claims: PartialClaims =
        serde_json::from_slice(validation_item.claims()).map_err(|_| "Unable to decode claims")?;
    match claims.iss.as_str() {
        google::ISSUER => google::verify(jwt, session_principal, session_salt, timestamp),
        _ => Err(format!("Unsupported issuer: {}", claims.iss)),
    }
}
