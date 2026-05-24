//! DKIM verifier (RFC 6376 + RFC 8463).
//!
//! Hand-rolled because the well-tested upstream alternative (Stalwart's
//! `mail-auth`) pulls a non-optional `hickory-resolver` dep that won't
//! compile to `wasm32-unknown-unknown`. Each sub-module corresponds to a
//! piece of RFC 6376 the verifier needs:
//!
//! - `parse` — `DKIM-Signature` header tag-list parser (§3.5).
//! - `dns_record` — DKIM TXT record parser (§3.6.2).
//! - `canonicalize` — relaxed header (§3.4.2) and body (§3.4.4) canonical
//!   forms; `simple/*` canonicalisation on the header side is rejected
//!   per design §5.2.
//! - `signature` — RSA-SHA256 (RFC 5702 / RFC 8301) and Ed25519-SHA256
//!   (RFC 8463) signature verification, on top of the existing `rsa`
//!   and `ed25519-dalek` workspace deps.
//! - `verify` — orchestration: multi-signature loop, tag enforcement,
//!   accept-on-first-pass.
//! - `tag_checks` — the **tag-contract facade** (two `enforce_*` umbrella
//!   functions) that both pipelines route their tag enforcement
//!   through. The facade is the single source of truth so the DoH and
//!   DNSSEC paths can't drift apart on which tag policies they enforce.
//!
//! ## Tag-contract call order
//!
//! Within each umbrella the order matters for diagnostics — the first
//! failing check is what surfaces upstream when an input triggers
//! multiple rejections. Documented here so a future contributor
//! reordering checks knows the rationale and doesn't undo it:
//!
//! - `enforce_signature_header_tag_contract`: `x=` (expired) → `t=`
//!   (future-dated) → `Subject` ∈ `h=`. Cheapest-first against a
//!   parsed signature header.
//! - `enforce_dns_record_tag_contract`: `t=y` (testing) → `i=` AUID
//!   alignment. `t=y` is deliberately first: a key the signer has
//!   marked non-production invalidates the signature regardless of
//!   any other tag state, so surfacing `TestingMode` ahead of (e.g.)
//!   `AuidMisaligned` or the structural `AlgorithmKeyTypeMismatch`
//!   that runs immediately after the umbrella gives the most useful
//!   diagnostic.
//!
//! The verifier consumes a DKIM TXT record (sourced either from a
//! DNSSEC-verified `DnsProofBundle` cached at prepare time, or via
//! `crate::doh::fetch_txt` at email-arrival time) plus a parsed
//! `SmtpRequest`. It does not make any DNS calls itself; the caller
//! is responsible for delivering the trusted public-key bytes.

// `crate::email_recovery::smtp::verify_setup_email` is the in-canister
// consumer; some less-used items in the public surface aren't yet
// referenced. Suppress dead-code warnings until those land.
#![allow(dead_code)]

mod canonicalize;
mod dns_record;
mod parse;
mod signature;
mod tag_checks;
#[cfg(test)]
mod test_vectors;
mod types;
mod verify;

#[allow(unused_imports)]
pub use types::{
    Algorithm, BodyCanon, DkimCheck, DkimCheckName, DkimCheckStatus, DkimVerifyResult, HeaderCanon,
    SignedSmtpMessage, VerificationFailReason,
};
// Re-exported as `verify_dkim` so downstream callers (the dmarc layer)
// don't have to deal with both a `dkim::verify` and `dmarc::verify`
// in scope at the same time.
#[allow(unused_imports)]
pub use verify::verify as verify_dkim;

// Building blocks consumed by the email-recovery two-phase pipeline
// (`crate::email_recovery::smtp` parses the signature and computes
// the headers digest at email-arrival time, then
// `crate::email_recovery::submit_leaf` admits the DKIM TXT and
// finishes the signature check).
#[allow(unused_imports)]
pub(crate) use canonicalize::relaxed_body;
#[allow(unused_imports)]
pub(crate) use dns_record::{parse_dkim_txt, DkimDnsRecord, KeyType};
#[allow(unused_imports)]
pub(crate) use parse::{parse_dkim_signature, DkimSignature};
#[allow(unused_imports)]
pub(crate) use signature::{body_hash_sha256, verify_signature, VerifyOutcome};
#[allow(unused_imports)]
pub(crate) use tag_checks::{
    // The two umbrellas (`enforce_*_tag_contract`) are the facade
    // both verification pipelines call into; the individual
    // `check_*` helpers remain reachable for fine-grained unit tests.
    check_auid_aligned,
    check_dns_not_testing,
    check_signature_not_expired,
    check_signature_not_from_future,
    check_subject_signed,
    enforce_dns_record_tag_contract,
    enforce_signature_header_tag_contract,
};
#[allow(unused_imports)]
pub(crate) use verify::{build_header_hash_input, simple_body};
