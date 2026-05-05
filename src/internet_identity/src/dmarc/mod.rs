//! DMARC alignment check (RFC 7489) + the combined DKIM + DMARC
//! email verifier entry point.
//!
//! This module sits one layer above `dkim`: it consumes a parsed
//! `SmtpRequest` plus the (already-trusted) DKIM TXT record bytes and
//! the DMARC TXT record bytes, runs DKIM verification, then checks
//! that the DKIM-signed `d=` aligns with the From-header domain under
//! the published `adkim=` mode. The public entry point is
//! [`verify::verify_email`].
//!
//! The submodules are:
//! - `types` — `DmarcOutcome`, `DmarcPolicy`, `AlignmentMode`,
//!   `DmarcRecord`, plus the combined `EmailVerificationStatus`.
//! - `parse` — DMARC TXT record parser (RFC 7489 §6.3).
//! - `from_header` — RFC 5322 single-mailbox From-header parser.
//! - `alignment` — strict / relaxed alignment check (no PSL — see
//!   design doc §6.4).
//! - `verify` — orchestration; the public entry point lives here.

// PR 3 lands the verifier without an in-canister consumer (PR 8's
// `smtp_request` dispatch is the first caller). Suppress dead-code
// warnings for the public surface until consumers land.
#![allow(dead_code)]

mod alignment;
mod from_header;
mod parse;
#[cfg(test)]
mod test_vectors;
mod types;
mod verify;

#[allow(unused_imports)]
pub use types::{AlignmentMode, DmarcOutcome, DmarcPolicy, DmarcRecord, EmailVerificationStatus};
#[allow(unused_imports)]
pub use verify::verify_email;
