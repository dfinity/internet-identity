//! Types for the verified-emails feature — a parallel anchor primitive
//! to [`email_recovery`][super::email_recovery].
//!
//! Verified emails share the inbound-DKIM verification primitive
//! (DKIM/DMARC/DNSSEC/DoH) with the recovery flow but live in their
//! own anchor field (`Anchor.verified_emails`), use their own
//! `II-Verify-` Subject prefix, and are intended as attribute sources
//! for dapps (see Phase 2 of `docs/design/verified-email-attributes.md`).
//! Adding an entry here does not affect the recovery email and vice
//! versa.
//!
//! The canister-side wrappers live in `crate::internet_identity::
//! types::email_recovery` for the shared challenge / status / DKIM-
//! leaf-submission types (`EmailChallenge`,
//! `EmailChallengeDnsInput`, `EmailChallengeStatus`,
//! `EmailChallengeError`, etc.) since those describe the inbound-DKIM
//! primitive both flows consume.

use crate::internet_identity::types::Timestamp;
use candid::{CandidType, Deserialize};
use serde::Serialize;

/// One verified email, owned by a single anchor.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct VerifiedEmail {
    /// Lowercased canonical form, same normalisation as
    /// `EmailRecoveryCredential::address`.
    pub address: String,
    /// Nanoseconds since the Unix epoch. The moment the inbound
    /// challenge-email completed DKIM/DMARC verification.
    pub verified_at: Timestamp,
}
