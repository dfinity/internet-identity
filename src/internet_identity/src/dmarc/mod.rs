//! DMARC alignment primitives (RFC 7489).
//!
//! Orchestration moved to [`crate::email_recovery::typestate`] — the
//! canister consumes a `VerifiedSmtpRequest` produced by stage 3 of
//! the typestate pipeline, which calls into the alignment helpers
//! here directly. What stays in this module are the parsing and
//! alignment primitives:
//!
//! - `types` — `DmarcOutcome`, `DmarcPolicy`, `AlignmentMode`,
//!   `DmarcRecord`.
//! - `parse` — DMARC TXT record parser (RFC 7489 §6.3).
//! - `from_header` — RFC 5322 single-mailbox From-header parser.
//! - `alignment` — strict / relaxed alignment check (no PSL — see
//!   design doc §6.4).
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

// `crate::email_recovery::typestate` is the in-canister consumer;
// some less-used variants in the public surface aren't yet
// pattern-matched. Suppress dead-code warnings until those land.
#![allow(dead_code)]

mod alignment;
mod from_header;
mod parse;
#[cfg(test)]
mod test_vectors;
mod types;

#[allow(unused_imports)]
pub use types::{AlignmentMode, DmarcOutcome, DmarcPolicy, DmarcRecord};

// Building blocks the email-recovery typestate (stage 3) and the
// submit-leaf path consume to admit a DMARC record cached at prepare
// time and re-check alignment without re-running the full DKIM+DMARC
// pipeline.
pub(crate) use alignment::aligns;
pub(crate) use from_header::extract_from_domain;
pub(crate) use parse::parse_dmarc_txt;
