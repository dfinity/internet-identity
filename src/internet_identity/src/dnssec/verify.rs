//! DNSSEC verification entry point.
//!
//! PR #1 — scaffold only. The body returns `Err(DnssecError::NotImplemented)`
//! and exists so consumers (the DKIM verifier in PR #2, the
//! `email_recovery_*` methods in PR #6+) can compile against the real type
//! signature while PR #1b carries the cryptographic implementation.

use super::types::{DnsProofBundle, DnssecError, VerifiedRecord};
use internet_identity_interface::internet_identity::types::DnssecRootAnchor;

/// Verify a caller-supplied DNS proof bundle against the configured trust
/// anchors and return the canonical leaf record.
///
/// Algorithm (per `docs/ongoing/email-recovery.md` §7.3) is implemented in
/// PR #1b. The TODO list below is the per-step plan:
///
/// - TODO(PR #1b): step 1 — pick the root KSK whose digest matches one of
///   `trust_anchors`, then verify `bundle.root_dnskey.rrsig` over its RRset
///   using that KSK. Reject with `RootAnchorMismatch` / `BadSignature`.
/// - TODO(PR #1b): step 2 — walk `bundle.chain` top-down. For each link:
///   verify `child_ds.rrsig` against the parent zone's DNSKEY RRset, then
///   verify `child_dnskey.rrsig` self-signature, then check that some KSK
///   in `child_dnskey.rdata` hashes to one of the DS digests in
///   `child_ds.rdata` (`DsMismatch` if none match).
/// - TODO(PR #1b): step 3 — verify `bundle.leaf.rrsig` against the deepest
///   zone's DNSKEY RRset. `BadSignature` on failure.
/// - TODO(PR #1b): step 4 — freshness. Every RRSIG must satisfy
///   `inception <= now + skew` and `expiration >= now - skew`. Use
///   `ic_cdk::api::time()` (nanos) divided to seconds. `StaleOrFutureSignature`
///   on out-of-window.
/// - TODO(PR #1b): algorithm dispatch. Support RFC 8624 MUST set:
///   8 (RSA-SHA256, via existing `rsa` + `sha2` workspace deps),
///   13 (ECDSA-P256-SHA256, new `p256` dep), and
///   15 (Ed25519, new `ed25519-dalek` dep).
///   Any other algorithm (5, 7, …) returns `UnsupportedAlgorithm`.
/// - TODO(PR #1b): canonical RDATA encoding per RFC 4034 §6.2 + §6.3
///   (lowercase owner names, sort RDATA per the type's defined ordering)
///   shared between RRSIG verification and DS hashing.
pub fn verify(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
) -> Result<VerifiedRecord, DnssecError> {
    let _ = bundle;
    if trust_anchors.is_empty() {
        return Err(DnssecError::NoTrustAnchors);
    }
    Err(DnssecError::NotImplemented)
}

#[cfg(test)]
mod tests {
    use super::super::types::{DnsName, Rrsig, SignedRRset};
    use super::*;
    use serde_bytes::ByteBuf;

    /// Returns a single throwaway trust anchor — enough to get past the
    /// `NoTrustAnchors` check so we exercise the actual verify path.
    fn dummy_trust_anchor() -> DnssecRootAnchor {
        DnssecRootAnchor {
            key_tag: 20326,
            algorithm: 8,
            digest_type: 2,
            digest: ByteBuf::from(vec![0u8; 32]),
        }
    }

    fn dummy_signed_rrset() -> SignedRRset {
        SignedRRset {
            name: DnsName(vec![0]),
            rtype: 16,
            rdata: vec![],
            ttl: 0,
            rrsig: Rrsig {
                type_covered: 16,
                algorithm: 8,
                labels: 0,
                original_ttl: 0,
                expiration: 0,
                inception: 0,
                key_tag: 0,
                signer_name: DnsName(vec![0]),
                signature: vec![],
            },
        }
    }

    #[test]
    fn verify_returns_not_implemented_with_anchors() {
        let bundle = DnsProofBundle {
            leaf: dummy_signed_rrset(),
            root_dnskey: dummy_signed_rrset(),
            chain: vec![],
        };
        assert_eq!(
            verify(&bundle, &[dummy_trust_anchor()]),
            Err(DnssecError::NotImplemented)
        );
    }

    #[test]
    fn verify_rejects_empty_anchor_list() {
        let bundle = DnsProofBundle {
            leaf: dummy_signed_rrset(),
            root_dnskey: dummy_signed_rrset(),
            chain: vec![],
        };
        assert_eq!(verify(&bundle, &[]), Err(DnssecError::NoTrustAnchors));
    }
}
