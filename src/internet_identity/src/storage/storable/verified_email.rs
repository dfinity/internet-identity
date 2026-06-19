//! On-disk representation of a `VerifiedEmail`.
//!
//! Stored inline on the anchor (see `StorableAnchor`) — *not* in a
//! separate stable map. `minicbor-derive`'s `#[cbor(map)]` shape is
//! forward-compatible across optional-field additions, so an anchor
//! that doesn't yet carry any verified email decodes with
//! `verified_emails: None`.
//!
//! Verified emails are a **parallel** concept to recovery emails
//! (`StorableEmailRecoveryCredential`). They share the verification
//! primitive (DKIM + DMARC + DNSSEC/DoH, driven by the same
//! `email_recovery` machinery) but live in their own anchor field,
//! own their own user-facing wizard, use their own `II-Verify-`
//! Subject prefix, and serve a different purpose (attribute sources
//! for dapps — see Phase 2 of the design doc).

use internet_identity_interface::internet_identity::types::email_recovery::VerifiedEmail;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableVerifiedEmail {
    /// Lowercased canonical address — same normalisation as
    /// `StorableEmailRecoveryCredential::address`.
    #[n(0)]
    pub address: String,
    /// Nanoseconds since the Unix epoch. The moment the inbound
    /// challenge-email completed DKIM/DMARC verification.
    #[n(1)]
    pub verified_at: Timestamp,
}

impl From<VerifiedEmail> for StorableVerifiedEmail {
    fn from(value: VerifiedEmail) -> Self {
        Self {
            address: value.address,
            verified_at: value.verified_at,
        }
    }
}

impl From<StorableVerifiedEmail> for VerifiedEmail {
    fn from(value: StorableVerifiedEmail) -> Self {
        Self {
            address: value.address,
            verified_at: value.verified_at,
        }
    }
}
