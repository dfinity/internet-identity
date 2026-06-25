use crate::internet_identity::types::Timestamp;
use candid::{CandidType, Deserialize};
use serde::Serialize;

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct VerifiedEmail {
    /// Lowercased canonical form.
    pub address: String,
    /// Nanoseconds since the Unix epoch.
    pub verified_at: Timestamp,
}
