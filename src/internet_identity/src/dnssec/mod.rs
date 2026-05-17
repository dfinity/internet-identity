//! DNSSEC verifier for caller-supplied DNS proof bundles.
//!
//! Inputs are a [`DnsProofBundle`] (a leaf RRset plus the DNSKEY / DS
//! chain linking it back to the IANA root) and the canister's
//! currently-trusted root anchors. Verification of a bundle is fully
//! deterministic — every byte the verifier consumes is supplied as a
//! canister-call argument or baked in via the init / upgrade arg, and
//! this module makes no HTTP outcalls. Domains that don't publish
//! DNSSEC are handled separately by the DoH-quorum fallback (landing
//! in PR 4 of the email-recovery stack; see design doc §7.6); this
//! module is the no-outcall path.
//!
//! ## Spec references
//!
//! - RFC 4033 — DNSSEC introduction and requirements.
//! - RFC 4034 — DNSSEC resource records (DNSKEY, RRSIG, DS, NSEC). The
//!   canonical-form rules and signed-data construction live here.
//! - RFC 4035 — Protocol modifications for DNSSEC; defines the
//!   validation procedure we follow end-to-end.
//! - RFC 5702 — RSA / SHA-256 in DNSSEC (algorithm 8).
//! - RFC 6605 — ECDSA P-256 / SHA-256 in DNSSEC (algorithm 13).
//! - RFC 8080 — Ed25519 in DNSSEC (algorithm 15).
//! - RFC 8624 — Algorithm implementation requirements; defines the
//!   MUST set `{8, 13, 15}` we accept and the MUST-NOT set (RSA-SHA1
//!   etc.) we reject.
//!
//! ## Module layout
//!
//! - [`wire`] — RFC-derived field offsets / lengths shared across
//!   modules. Single-use offsets live next to their function.
//! - [`types`] — `DnsProofBundle` and friends; the canister-side
//!   shape of the verifier's input.
//! - [`canonical`] — canonical-form serialisation (RFC 4034 §6.2,
//!   §6.3) and signed-data assembly (RFC 4034 §3.1.8.1, §5.1.4).
//! - [`signature`] — algorithm dispatch + signature checks for the
//!   three MUST algorithms plus DS digest matching.
//! - [`verify`] — top-level entry point implementing the four-step
//!   algorithm in `docs/ongoing/email-recovery.md` §7.3.

// No callers in this PR (the DKIM verifier in PR 2 and the recovery
// methods in PR 6+ are the first consumers). Suppress dead-code
// warnings for the public surface until the consumers land.
#![allow(dead_code)]

mod canonical;
mod signature;
#[cfg(test)]
mod test_vectors;
pub(crate) mod types;
mod verify;
pub(crate) mod wire;

#[allow(unused_imports)]
pub use types::{
    DelegationChain, DelegationLink, DnsName, DnsProofBundle, DnssecError, Rrsig, SignedRRset,
    VerifiedRecord, ZoneKeysMap, TYPE_DNSKEY, TYPE_DS, TYPE_TXT,
};
#[allow(unused_imports)]
pub use verify::{
    verify_bundle, verify_bundle_with_clock, verify_chain_with_clock,
    verify_extra_chains_with_clock, verify_hops_with_clock, verify_root_dnskey_with_clock,
    MAX_CNAME_HOPS,
};
