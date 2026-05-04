//! Type definitions for DNSSEC proof bundles.
//!
//! Mirrors the shape described in `docs/ongoing/email-recovery.md` §7.2.
//! These types are the canister-side representation of what the FE assembles
//! by walking DoH responses from a public resolver.

/// A DNS owner name in wire form (a sequence of length-prefixed labels). All
/// canonicalisation (lowercasing per RFC 4034 §6.2) happens inside the
/// verifier; callers are expected to supply names exactly as the DoH
/// resolver returned them.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DnsName(pub Vec<u8>);

/// One signed RRset — the RDATA collection plus the RRSIG that authenticates
/// it. Both come from the same DoH response.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedRRset {
    pub name: DnsName,
    /// `TYPE` per IANA registry — TXT (16), DNSKEY (48), DS (43), …
    pub rtype: u16,
    /// RDATA per record, in the receipt order returned by the resolver. The
    /// verifier canonicalises (RFC 4034 §6.3) before hashing.
    pub rdata: Vec<Vec<u8>>,
    pub ttl: u32,
    pub rrsig: Rrsig,
}

/// An RRSIG resource record per RFC 4034 §3.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rrsig {
    pub type_covered: u16,
    pub algorithm: u8,
    pub labels: u8,
    pub original_ttl: u32,
    /// Unix-seconds.
    pub expiration: u32,
    /// Unix-seconds.
    pub inception: u32,
    pub key_tag: u16,
    pub signer_name: DnsName,
    pub signature: Vec<u8>,
}

/// One link in the DNSSEC delegation walk: the parent zone publishes a DS
/// RRset for the child, and the child publishes its own DNSKEY RRset
/// self-signed by the KSK whose digest matches the parent's DS.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DelegationLink {
    pub child_ds: SignedRRset,
    pub child_dnskey: SignedRRset,
}

/// The full proof: leaf RRset + delegation chain to the root + the root
/// DNSKEY RRset (which is in turn validated against the canister's trust
/// anchors).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DnsProofBundle {
    pub leaf: SignedRRset,
    pub root_dnskey: SignedRRset,
    pub chain: Vec<DelegationLink>,
}

/// A successfully-verified leaf record. Currently just echoes the leaf's
/// canonical form; richer return types (parsed TXT chunks, etc.) can be
/// layered on later without breaking callers.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedRecord {
    pub name: DnsName,
    pub rtype: u16,
    pub rdata: Vec<Vec<u8>>,
}

/// Verification failure modes. Granular enough that a UI message can map
/// straight from the variant.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DnssecError {
    /// No trust anchors are configured. The deploy/upgrade arg must include
    /// `dnssec_config.root_anchors` before any verification can succeed.
    NoTrustAnchors,

    /// The bundle's root DNSKEY RRset doesn't include any KSK that hashes
    /// to a configured trust-anchor digest.
    RootAnchorMismatch,

    /// A DS RRset published by the parent zone doesn't match any KSK in the
    /// child zone's DNSKEY RRset.
    DsMismatch,

    /// An RRSIG didn't validate against the DNSKEY claimed to have signed it.
    BadSignature,

    /// `now` falls outside an RRSIG's `[inception, expiration]` window
    /// (with the configured clock-skew tolerance).
    StaleOrFutureSignature,

    /// The signature algorithm is not in our supported set (RFC 8624 MUST:
    /// 8 = RSA-SHA256, 13 = ECDSA-P256-SHA256, 15 = Ed25519). RSA-SHA1
    /// (alg 5) and friends fall here.
    UnsupportedAlgorithm(u8),

    /// Bundle is structurally malformed — e.g. RDATA bytes don't decode
    /// per the RFC, label lengths exceed 63, name exceeds 255 bytes.
    Malformed(&'static str),
}
