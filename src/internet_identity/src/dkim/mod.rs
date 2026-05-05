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
//!
//! The verifier consumes a `DnsProofBundle`-verified DKIM TXT record (or
//! a DoH-fetched one in PR 4) plus a parsed `SmtpRequest`. It does not
//! make any DNS calls itself; the caller is responsible for delivering
//! the trusted public-key bytes.

// PR 2 lands the verifier without a canister-side caller (PR 8 will wire
// it into `smtp_request`). Suppress dead-code warnings for the public
// surface until consumers land.
#![allow(dead_code)]

mod canonicalize;
mod dns_record;
mod parse;
mod signature;
mod types;

#[allow(unused_imports)]
pub use types::{
    Algorithm, BodyCanon, DkimCheck, DkimCheckName, DkimCheckStatus,
    EmailVerificationStatus, HeaderCanon, VerificationFailReason,
};
