//! DNSSEC verifier for caller-supplied DNS proof bundles.
//!
//! Inputs are a `DnsProofBundle` (a leaf RRset plus the DNSKEY/DS chain
//! linking it to the IANA root) and the canister's currently-trusted root
//! anchors. Verification is fully deterministic — every byte the verifier
//! consumes is supplied as a canister-call argument or baked in via the
//! init/upgrade arg. There are no HTTP outcalls anywhere in the
//! email-recovery stack (see `docs/ongoing/email-recovery.md` §7).
//!
//! PR #1 lands the scaffold (types + a stub `verify` returning
//! `NotImplemented`). The real RRSIG/DS/DNSKEY checks land in PR #1b — see
//! `verify.rs` for the per-step TODO list.
// PR 1 lands the scaffold without callers (PR 2's DKIM verifier and PRs
// 6+'s recovery methods are the first consumers). Allow `dead_code` on this
// module until those land so the wasm build stays warning-clean. Narrow on
// purpose: anything else (e.g. `unused_imports`) should still warn so a
// real issue isn't masked when the verifier implementation lands.
#![allow(dead_code)]

mod types;
mod verify;

#[allow(unused_imports)]
pub use types::{
    DelegationLink, DnsName, DnsProofBundle, DnssecError, Rrsig, SignedRRset, VerifiedRecord,
};
#[allow(unused_imports)]
pub use verify::verify;
