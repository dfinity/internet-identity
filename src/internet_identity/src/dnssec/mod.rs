//! DNSSEC verifier for caller-supplied DNS proof bundles.
//!
//! Inputs are a `DnsProofBundle` (a leaf RRset plus the DNSKEY/DS chain
//! linking it to the IANA root) and the canister's currently-trusted root
//! anchors. Verification is fully deterministic — every byte the verifier
//! consumes is supplied as a canister-call argument or baked in via the
//! init/upgrade arg. There are no HTTP outcalls anywhere in the
//! email-recovery stack (see `docs/ongoing/email-recovery.md` §7).
//!
//! Algorithm coverage: RFC 8624 MUST set — alg 8 (RSA-SHA256), alg 13
//! (ECDSA-P256-SHA256), alg 15 (Ed25519). Anything else is rejected
//! with `DnssecError::UnsupportedAlgorithm`.

// No callers in this PR (PR 2's DKIM verifier and PR 6+'s recovery
// methods are the first consumers). Suppress dead-code warnings for
// the public surface until consumers land.
#![allow(dead_code)]

mod canonical;
mod signature;
#[cfg(test)]
mod test_vectors;
mod types;
mod verify;

#[allow(unused_imports)]
pub use types::{
    DelegationLink, DnsName, DnsProofBundle, DnssecError, Rrsig, SignedRRset, VerifiedRecord,
};
#[allow(unused_imports)]
pub use verify::{verify, verify_with_clock};
