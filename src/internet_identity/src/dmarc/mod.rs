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
//!
//! # Security model
//!
//! Three intentional deviations from "stock" DMARC, each documented in
//! the design doc and worth surfacing here for auditors:
//!
//! - **No Public Suffix List.** Relaxed alignment uses a label-anchored
//!   suffix check (see `alignment::is_subdomain_of`) instead of computing
//!   the PSL-defined organizational domain. This prevents suffix
//!   attacks (`evilexample.com` masquerading as `example.com`) and
//!   removes a large, slowly-changing trust dependency from the
//!   canister. The cost is that legitimate multi-domain orgs
//!   (`gmail.com` ↔ `googlemail.com`) fail closed; that's the safe
//!   direction for an account-recovery surface. Design doc §6.4.
//!
//! - **No SPF.** The recovery flow proves *mailbox control*, not
//!   *path-of-delivery*. DKIM gives us a cryptographic binding from the
//!   message content to the signing domain; SPF would only tell us the
//!   message came from an IP the domain authorised, which we can't
//!   meaningfully check in-canister anyway (no source IP in the gateway
//!   payload). Design doc §6.5.
//!
//! - **Fail-closed everywhere.** Every malformed input, every
//!   unrecognised value, every step that can't produce a positive
//!   answer collapses to `Unverified`. We never quarantine, never
//!   downgrade — a recovery attempt either proves mailbox control or
//!   it doesn't run.

// `crate::email_recovery::smtp::verify_setup_email` is the in-canister
// consumer; some less-used variants in the public surface (e.g.
// fine-grained `EmailVerificationStatus` reasons) aren't yet
// pattern-matched. Suppress dead-code warnings until those land.
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

// Building blocks the email-recovery submit-leaf path needs to admit
// a DMARC record cached at prepare time and re-check alignment
// without re-running the full `verify_email` pipeline (which expects
// the message body the body has already been dropped).
pub(crate) use alignment::aligns;
pub(crate) use parse::parse_dmarc_txt;
