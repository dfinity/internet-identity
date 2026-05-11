//! Type definitions for DNSSEC proof bundles.
//!
//! Mirrors the shape described in `docs/ongoing/email-recovery.md`
//! §7.2. These types are the canister-side representation of what the
//! front-end assembles by walking DoH responses from a public
//! resolver. The verifier treats them as untrusted input — all
//! canonicalisation, sorting, and signature checks happen on the
//! canister side.

/// IANA DNS resource record `TYPE` codes the verifier and its
/// callers reference by name. Full registry at
/// <https://www.iana.org/assignments/dns-parameters/>; these are the
/// only ones the email-recovery flow currently consumes (RFC 1035
/// §3.2.2 + later assignments).
pub const TYPE_TXT: u16 = 16;
pub const TYPE_DS: u16 = 43;
pub const TYPE_DNSKEY: u16 = 48;

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
    /// later assignments). See [`TYPE_TXT`] / [`TYPE_DS`] /
    /// [`TYPE_DNSKEY`] for the values the verifier references.
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
    /// RFC 8624 §3.1's MUST set: RSA-SHA256, ECDSA-P256-SHA256,
    /// Ed25519).
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

/// A delegation walk from the DNS root to one zone's DNSKEY RRset.
/// The chain's last link's `child_dnskey` is the validated DNSKEY
/// RRset for the zone — that's what gets inserted into the
/// [`ZoneKeysMap`] for subsequent per-hop signature checks.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DelegationChain {
    pub links: Vec<DelegationLink>,
}

/// The full DNSSEC proof: a root DNSKEY RRset, one or more
/// delegation chains rooted at the root DNSKEY, and an ordered list
/// of signed RRsets (`hops`) representing CNAME resolution from the
/// requested name to the final RRset.
///
/// Single-zone direct case (e.g. `cloudflare.com TXT`):
/// `chains.len() == 1`, `hops.len() == 1`.
///
/// Cross-zone CNAME case (e.g. `proton.me` → `proton.ch` for DKIM):
/// one chain per signing zone touched, one hop per RRset in the
/// resolution sequence (intermediate CNAMEs, then the final TXT).
///
/// See design doc §7.2 for the operational shape, and
/// [`crate::dnssec::verify::verify_bundle_with_clock`] for the
/// validation algorithm.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DnsProofBundle {
    /// The DNS root's DNSKEY RRset; validated against
    /// [`DnssecRootAnchor`] entries in the canister's config.
    ///
    /// [`DnssecRootAnchor`]: internet_identity_interface::internet_identity::types::DnssecRootAnchor
    pub root_dnskey: SignedRRset,
    /// One delegation chain per signing zone the bundle touches.
    /// Each chain is walked top-down from `root_dnskey`; its
    /// deepest-zone DNSKEY RRset goes into the
    /// `(zone_name → DNSKEY RRset)` lookup table that the hop
    /// checks consult. Duplicate landing-zones are rejected.
    pub chains: Vec<DelegationChain>,
    /// The RRsets being authenticated, in CNAME-resolution order.
    /// Each hop's `rrsig.signer_name` identifies the zone that
    /// signed it; the verifier looks that name up in the table
    /// built from `chains` and checks the RRset's RRSIG under that
    /// zone's DNSKEY. The first hop's owner must match the
    /// caller-supplied requested name; intermediate hops must be
    /// CNAMEs whose target equals the next hop's owner; the final
    /// hop's RTYPE must match the caller-supplied requested type.
    pub hops: Vec<SignedRRset>,
}

/// A successfully-verified leaf record. Currently echoes the leaf's
/// canonical form so callers don't have to re-derive it; richer
/// return types (parsed TXT chunks, etc.) can be layered on later
/// without breaking callers.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedRecord {
    pub name: DnsName,
    pub rtype: u16,
    pub rdata: Vec<Vec<u8>>,
}

/// A map from canonical (lowercased, length-prefixed wire-format)
/// zone name to that zone's validated DNSKEY RRset.
///
/// The verifier builds this from the supplied delegation chains.
/// Two-phase callers can also cache it across prepare/submit calls
/// so a follow-up submission only needs to validate any *new*
/// chains the resolution crossed into.
///
/// Backed by a `Vec<(DnsName, SignedRRset)>` rather than a
/// `HashMap` so iteration order is deterministic and so size can be
/// bounded with a single `len()` check (callers cap the cache).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ZoneKeysMap {
    entries: Vec<(DnsName, SignedRRset)>,
}

impl ZoneKeysMap {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Insert `(zone, dnskey_rrset)`. Returns
    /// [`DnssecError::DuplicateZone`] if a chain to the same zone
    /// is already present — defence against bundles that try to
    /// inflate the cache or smuggle a second, attacker-chosen
    /// DNSKEY for an already-validated zone.
    pub fn insert(&mut self, zone: DnsName, dnskey_rrset: SignedRRset) -> Result<(), DnssecError> {
        if self.entries.iter().any(|(n, _)| n == &zone) {
            return Err(DnssecError::DuplicateZone);
        }
        self.entries.push((zone, dnskey_rrset));
        Ok(())
    }

    pub fn get(&self, zone: &DnsName) -> Option<&SignedRRset> {
        self.entries
            .iter()
            .find(|(n, _)| n == zone)
            .map(|(_, k)| k)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Iterate the validated zones in insertion order, which matches
    /// the order the delegation chains were verified in.
    pub fn iter(&self) -> impl Iterator<Item = (&DnsName, &SignedRRset)> {
        self.entries.iter().map(|(n, k)| (n, k))
    }
}

/// Verification failure modes. Each variant is granular enough that
/// a UI message can map straight from the variant without consulting
/// any free-form context, except for `Malformed` and `BadCnameChain`
/// which carry a short static reason for log aggregation.
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

    /// An RRSIG didn't validate against any DNSKEY in the rrset
    /// claimed to have signed it (RFC 4035 §5.3.3).
    BadSignature,

    /// `now` falls outside an RRSIG's `[inception, expiration]`
    /// window after applying the configured clock-skew tolerance
    /// (RFC 4034 §3.1.5 / RFC 4035 §5.3.1).
    StaleOrFutureSignature,

    /// The signature algorithm is not in our supported set
    /// (RFC 8624 §3.1 MUST: RSA-SHA256, ECDSA-P256-SHA256,
    /// Ed25519). RSA-SHA1 etc. land here.
    UnsupportedAlgorithm(u8),

    /// Bundle is structurally malformed — e.g. RDATA bytes don't
    /// decode per the relevant RFC, an Ed25519 public key is not
    /// 32 bytes, a label length exceeds 63 (RFC 1035 §2.3.4), or a
    /// chain has no links.
    Malformed(&'static str),

    /// Two delegation chains in the same bundle land at the same
    /// zone. Either the bundle has redundant data (caller bug) or
    /// it's trying to smuggle a second, attacker-chosen DNSKEY for
    /// an already-validated zone.
    DuplicateZone,

    /// A hop's `RRSIG.signer_name` references a zone that isn't in
    /// the validated zone-keys map — i.e. the bundle didn't
    /// include a delegation chain for the zone the hop claims to
    /// be signed by.
    UnknownSigningZone,

    /// A hop's owner name doesn't lie under its RRSIG's
    /// `signer_name` — e.g. an RRset claims to be signed by
    /// `example.com.` but its owner is `unrelated.org.`. Defends
    /// against valid-but-misplaced signatures.
    HopOwnerOutsideZone,

    /// The hop sequence doesn't form a coherent CNAME → … →
    /// final-type resolution (intermediate hop isn't a CNAME,
    /// target doesn't match the next hop's owner, hop list is
    /// empty, sequence forms a loop, …).
    BadCnameChain(&'static str),

    /// Bundle exceeds the per-bundle hop cap. See `MAX_CNAME_HOPS`
    /// in [`crate::dnssec::verify`].
    TooManyHops,
}

// =========================================================================
// Conversions from the Candid-friendly mirror types in
// `internet_identity_interface::types::dnssec`. Used at the canister
// method boundary so callers (FE, integration tests) can pass a
// `DnsProofBundle` over Candid without us having to add CandidType
// derives everywhere in the verifier.
// =========================================================================

mod interface_conversions {
    use super::{
        DelegationChain, DelegationLink, DnsName, DnsProofBundle, Rrsig, SignedRRset,
    };
    use internet_identity_interface::internet_identity::types as i_types;

    impl From<i_types::SignedRRset> for SignedRRset {
        fn from(v: i_types::SignedRRset) -> Self {
            SignedRRset {
                name: DnsName(v.name.into_vec()),
                rtype: v.rtype,
                rdata: v.rdata.into_iter().map(|b| b.into_vec()).collect(),
                ttl: v.ttl,
                rrsig: v.rrsig.into(),
            }
        }
    }

    impl From<i_types::Rrsig> for Rrsig {
        fn from(v: i_types::Rrsig) -> Self {
            Rrsig {
                type_covered: v.type_covered,
                algorithm: v.algorithm,
                labels: v.labels,
                original_ttl: v.original_ttl,
                expiration: v.expiration,
                inception: v.inception,
                key_tag: v.key_tag,
                signer_name: DnsName(v.signer_name.into_vec()),
                signature: v.signature.into_vec(),
            }
        }
    }

    impl From<i_types::DelegationLink> for DelegationLink {
        fn from(v: i_types::DelegationLink) -> Self {
            DelegationLink {
                child_ds: v.child_ds.into(),
                child_dnskey: v.child_dnskey.into(),
            }
        }
    }

    impl From<i_types::DelegationChain> for DelegationChain {
        fn from(v: i_types::DelegationChain) -> Self {
            DelegationChain {
                links: v.links.into_iter().map(Into::into).collect(),
            }
        }
    }

    impl From<i_types::DnsProofBundle> for DnsProofBundle {
        fn from(v: i_types::DnsProofBundle) -> Self {
            DnsProofBundle {
                root_dnskey: v.root_dnskey.into(),
                chains: v.chains.into_iter().map(Into::into).collect(),
                hops: v.hops.into_iter().map(Into::into).collect(),
            }
        }
    }
}
