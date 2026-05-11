//! Type definitions for DNSSEC proof bundles.
//!
//! Mirrors the shape described in `docs/ongoing/email-recovery.md`
//! §7.2. These types are the canister-side representation of what the
//! front-end assembles by walking DoH responses from a public
//! resolver. The verifier treats them as untrusted input — all
//! canonicalisation, sorting, and signature checks happen on the
//! canister side.

/// A DNS owner name in wire form: a sequence of `(length, label)`
/// tuples terminated by a zero-length label (RFC 1035 §3.1).
///
/// Canonicalisation (RFC 4034 §6.2 — lower-case every ASCII label
/// byte) happens inside the verifier; callers are expected to supply
/// names exactly as the DoH resolver returned them. The verifier
/// rejects compression pointers (RFC 1035 §4.1.4 / RFC 4034 §6.2)
/// implicitly by failing the downstream signature check.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DnsName(pub Vec<u8>);

/// One signed RRset — the RDATA collection plus the RRSIG that
/// authenticates it. Both come from the same DoH response.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedRRset {
    pub name: DnsName,
    /// `TYPE` per the IANA Resource Record Types registry — A (1),
    /// TXT (16), DS (43), DNSKEY (48), etc. (RFC 1035 §3.2.2 +
    /// later assignments).
    pub rtype: u16,
    /// One entry per RR in canonical RDATA form. The verifier
    /// re-sorts these per RFC 4034 §6.3 before hashing, so input
    /// order does not need to be canonical.
    pub rdata: Vec<Vec<u8>>,
    /// Currently-cached TTL on the wire. The verifier substitutes
    /// `rrsig.original_ttl` when building canonical form
    /// (RFC 4034 §6.2 step 3), so this field is informational only.
    pub ttl: u32,
    pub rrsig: Rrsig,
}

/// An RRSIG resource record (RFC 4034 §3.1).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rrsig {
    /// The TYPE this RRSIG signs (RFC 4034 §3.1.1).
    pub type_covered: u16,
    /// IANA DNSSEC algorithm number (RFC 4034 §3.1.2; we support
    /// `{8, 13, 15}`, RFC 8624 §3.1).
    pub algorithm: u8,
    /// Label count of the signer's name, excluding the root label
    /// (RFC 4034 §3.1.3). Used as a wildcard-expansion signal; the
    /// verifier currently does not synthesise wildcard responses
    /// (email-recovery uses only fully-qualified leaves).
    pub labels: u8,
    /// TTL stamped into the signed canonical form, regardless of
    /// what `SignedRRset.ttl` currently caches (RFC 4034 §3.1.4 /
    /// §6.2 step 3).
    pub original_ttl: u32,
    /// Latest Unix-seconds time at which the signature is valid
    /// (RFC 4034 §3.1.5).
    pub expiration: u32,
    /// Earliest Unix-seconds time at which the signature is valid
    /// (RFC 4034 §3.1.5).
    pub inception: u32,
    /// 16-bit checksum of the signing DNSKEY's RDATA, used to
    /// quickly pick the candidate signer in the DNSKEY RRset
    /// (RFC 4034 §3.1.6 + Appendix B).
    pub key_tag: u16,
    /// Owner name of the zone that signed this RRset, in wire form
    /// (RFC 4034 §3.1.7).
    pub signer_name: DnsName,
    /// Algorithm-specific signature bytes (RFC 4034 §3.1.8;
    /// per-algorithm encoding in RFCs 5702 / 6605 / 8080).
    pub signature: Vec<u8>,
}

/// One link in the DNSSEC delegation walk: the parent zone publishes
/// a DS RRset for the child (RFC 4034 §5), and the child publishes
/// its own DNSKEY RRset which is self-signed by the KSK whose digest
/// matches one of the parent's DS records (RFC 4035 §5.2).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DelegationLink {
    pub child_ds: SignedRRset,
    pub child_dnskey: SignedRRset,
}

/// The full proof: leaf RRset + delegation chain to the root + the
/// root DNSKEY RRset, which is in turn validated against the
/// canister's trust anchors (RFC 4035 §5).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DnsProofBundle {
    /// The leaf RRset (typically a TXT for DKIM / DMARC).
    pub leaf: SignedRRset,
    /// The DNS root's DNSKEY RRset; validated against
    /// [`DnssecRootAnchor`] entries in the canister's config.
    ///
    /// [`DnssecRootAnchor`]: internet_identity_interface::internet_identity::types::DnssecRootAnchor
    pub root_dnskey: SignedRRset,
    /// Delegation links top-down: `[root → TLD, TLD → 2LD, …]`
    /// ending at the parent of the leaf.
    pub chain: Vec<DelegationLink>,
}

/// A successfully-verified leaf record. Currently echoes the leaf's
/// canonical form so callers don't have to re-derive it; richer
/// return types (e.g. parsed TXT chunks) can be layered on later
/// without breaking callers.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedRecord {
    pub name: DnsName,
    pub rtype: u16,
    pub rdata: Vec<Vec<u8>>,
}

/// Verification failure modes. Each variant is granular enough that
/// a UI message can map straight from the variant without consulting
/// any free-form context, except for `Malformed` which carries a
/// short static reason for log aggregation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DnssecError {
    /// No trust anchors are configured. The deploy / upgrade arg
    /// must include `dnssec_config.root_anchors` before any
    /// verification can succeed.
    NoTrustAnchors,

    /// The bundle's root DNSKEY RRset doesn't include any KSK that
    /// hashes to a configured trust-anchor digest (RFC 4035 §5).
    RootAnchorMismatch,

    /// A DS RRset published by the parent zone doesn't match any
    /// KSK in the child zone's DNSKEY RRset (RFC 4035 §5.2).
    DsMismatch,

    /// An RRSIG didn't validate against the DNSKEY claimed to have
    /// signed it (RFC 4035 §5.3.3).
    BadSignature,

    /// `now` falls outside an RRSIG's `[inception, expiration]`
    /// window after applying the configured clock-skew tolerance
    /// (RFC 4034 §3.1.5 / RFC 4035 §5.3.1).
    StaleOrFutureSignature,

    /// The signature algorithm is not in our supported set
    /// (RFC 8624 §3.1 MUST: 8 = RSA-SHA256, 13 = ECDSA-P256-SHA256,
    /// 15 = Ed25519). RSA-SHA1 (algorithm 5) etc. land here.
    UnsupportedAlgorithm(u8),

    /// Bundle is structurally malformed — e.g. RDATA bytes don't
    /// decode per the relevant RFC, an Ed25519 public key is not
    /// 32 bytes, a length octet exceeds 63 (RFC 1035 §2.3.4), or a
    /// name exceeds 255 bytes.
    Malformed(&'static str),
}
