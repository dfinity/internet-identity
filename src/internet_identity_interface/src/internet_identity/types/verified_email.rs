use crate::internet_identity::types::Timestamp;
use candid::{CandidType, Deserialize};
use serde::Serialize;

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct VerifiedEmail {
    /// Lowercased canonical form.
    pub address: String,
    /// Nanoseconds since the Unix epoch.
    pub verified_at: Timestamp,
    /// `Some(true)` when the address is one of Apple's private relay
    /// addresses, which only take mail from senders registered with II's
    /// Apple developer team. Absent counts as not a relay address.
    pub is_private_relay: Option<bool>,
}
