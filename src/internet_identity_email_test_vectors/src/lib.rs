//! Test vectors for the email-recovery verification stack.
//!
//! Captured from real DoH responses and DKIM-signed emails so the canister's
//! verifiers can be exercised against bytes the IC can't produce in unit tests.
//!
//! Currently empty: PR #1 only lands the scaffold. PR #1b adds the first batch
//! of DNSSEC chains; later PRs in Phase 0 add DKIM and DMARC vectors.

/// Returns the list of available DNSSEC chain test vectors.
///
/// TODO(PR #1b): replace with DoH captures (gmail.com, icloud.com,
/// outlook.com, fastmail.com, proton.me) plus deliberately-tampered
/// negatives.
pub fn dnssec_chains() -> Vec<&'static [u8]> {
    Vec::new()
}
