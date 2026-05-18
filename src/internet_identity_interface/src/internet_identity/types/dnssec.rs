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
/// `data.iana.org/root-anchors/root-anchors.xml`. Only `digest_type = 2`
/// (SHA-256) is accepted; the legacy SHA-1 form is rejected at the verifier
/// boundary.
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

/// A delegation walk from the root zone to a single zone's DNSKEY.
/// Each link carries the child zone's DS RRset (signed by the parent)
/// plus the child zone's DNSKEY RRset (self-signed). The last link's
/// `child_dnskey` is the zone whose DNSKEY ends up in the verifier's
/// `(zone_name → DNSKEY RRset)` lookup table.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DelegationChain {
    pub links: Vec<DelegationLink>,
}

/// A DNSSEC proof: a set of delegation chains rooted at the IANA KSK
/// plus the signed RRsets being authenticated. See design doc §7.2.
///
/// Single-zone direct case (e.g. Gmail-style DKIM): one chain, one TXT
/// hop. Cross-zone CNAME case (Proton's `proton.me` → `proton.ch`,
/// Tutanota's `tutanota.com` → `tutanota.de`, M365 custom domains, …):
/// one chain per signing zone touched, one hop per RRset in the
/// resolution sequence (CNAME, CNAME, …, TXT).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DnsProofBundle {
    pub root_dnskey: SignedRRset,
    /// One delegation chain per signing zone touched by this bundle.
    /// At least one is required; duplicates (two chains landing at
    /// the same zone) are rejected.
    pub chains: Vec<DelegationChain>,
    /// The RRsets being authenticated, in CNAME-resolution order.
    /// Each hop's `rrsig.signer_name` identifies which zone signed
    /// it; the verifier looks that name up in the table built from
    /// `chains` and validates the RRset under that zone's DNSKEY.
    pub hops: Vec<SignedRRset>,
}
