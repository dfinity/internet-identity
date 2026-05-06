use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;

// =========================================================================
// Trust-anchor config (`init` / `post_upgrade` arg).
// =========================================================================

/// Canister-wide DNSSEC verification configuration. Set on every `init` /
/// `post_upgrade`. Held in `PersistentState` so it survives upgrades when an
/// upgrade arg omits the field.
///
/// Not specific to email recovery. Any feature that needs DNSSEC-verified
/// DNS records (the email-recovery DKIM/DMARC flow today, future DANE/ACME
/// hooks, etc.) consumes the same set of trust anchors.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct DnssecConfig {
    /// IANA root KSK trust anchors. Multiple are accepted simultaneously so
    /// rollover is a one-line change in the next upgrade arg — during a
    /// transition both the retiring and incoming KSK digests live here.
    pub root_anchors: Vec<DnssecRootAnchor>,
}

/// A single IANA root KSK trust anchor, in the same shape IANA publishes at
/// `data.iana.org/root-anchors/root-anchors.xml`.
///
/// We only ship `digest_type = 2` (SHA-256) — the legacy SHA-1 form is
/// declined at the boundary by the verifier (see PR #1b TODO list).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DnssecRootAnchor {
    pub key_tag: u16,
    pub algorithm: u8,
    pub digest_type: u8,
    pub digest: ByteBuf,
}

// =========================================================================
// Caller-supplied DNSSEC proof bundle — used by the email-recovery prepare
// flow and any future feature that takes a DNSSEC chain as input.
//
// Mirrors the canister-side types in `crate::dnssec::types` (which are kept
// CandidType-free so the verifier can stay focused on cryptographic checks).
// `From<>` impls on the canister side convert at the API boundary.
//
// All names are wire-format (length-prefixed labels). Canonicalisation per
// RFC 4034 §6.2 happens inside the verifier.
// =========================================================================

/// One signed RRset — RDATA collection plus the RRSIG that authenticates
/// it. Both come from the same DoH response.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SignedRRset {
    /// Wire-format owner name: a sequence of length-prefixed labels.
    pub name: ByteBuf,
    /// `TYPE` per IANA registry — TXT (16), DNSKEY (48), DS (43), …
    pub rtype: u16,
    /// RDATA per record, in receipt order.
    pub rdata: Vec<ByteBuf>,
    pub ttl: u32,
    pub rrsig: Rrsig,
}

/// An RRSIG resource record per RFC 4034 §3.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct Rrsig {
    pub type_covered: u16,
    pub algorithm: u8,
    pub labels: u8,
    pub original_ttl: u32,
    /// Unix-seconds (matches the wire-format RRSIG field, **not** the
    /// canister-wide `Timestamp` nanosecond convention).
    pub expiration: u32,
    /// Unix-seconds — same encoding as `expiration`.
    pub inception: u32,
    pub key_tag: u16,
    /// Wire-format signer name.
    pub signer_name: ByteBuf,
    pub signature: ByteBuf,
}

/// One link in the DNSSEC delegation walk: parent zone publishes a DS
/// RRset for the child, child publishes its own DNSKEY RRset self-signed
/// by the KSK whose digest matches the parent's DS.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DelegationLink {
    pub child_ds: SignedRRset,
    pub child_dnskey: SignedRRset,
}

/// A DNSSEC proof: one delegation chain rooted at the IANA KSK plus
/// at most one signed leaf RRset that the chain authenticates.
///
/// The two-phase email-recovery flow only ever needs one leaf per
/// call — at `prepare_add` it's the optional DMARC TXT, at
/// `submit_dkim_leaf` it's the DKIM TXT — so `leaf` is `Option`,
/// not `Vec`. A `None` leaf means "validate the chain only";
/// callers cache the deepest-zone DNSKEY and use it to admit a leaf
/// in a follow-up call (see `crate::dnssec::verify_chain_with_clock`).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DnsProofBundle {
    pub leaf: Option<SignedRRset>,
    pub root_dnskey: SignedRRset,
    pub chain: Vec<DelegationLink>,
}
