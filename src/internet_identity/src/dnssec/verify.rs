//! DNSSEC verification entry point.
//!
//! Implements the four-step algorithm from §7.3 of
//! `docs/ongoing/email-recovery.md`:
//!
//!   1. Validate the bundle's root DNSKEY RRset against a configured
//!      trust anchor (one of `trust_anchors`).
//!   2. Walk `chain` top-down: each link's DS RRset verifies under the
//!      parent's DNSKEY, the child's DNSKEY RRset is self-signed by a
//!      KSK whose digest matches one of the DS records, and the child's
//!      DNSKEY RRset's RRSIG verifies under that same KSK.
//!   3. Verify the leaf RRset's RRSIG under the deepest zone's DNSKEY.
//!   4. Freshness — every RRSIG's `[inception, expiration]` window must
//!      contain `now ± clock_skew`.

use super::canonical::build_signed_data;
use super::signature::{ds_matches_dnskey, verify_signature_for_alg};
use super::types::{DelegationLink, DnsProofBundle, DnssecError, SignedRRset, VerifiedRecord};
use internet_identity_interface::internet_identity::types::DnssecRootAnchor;

/// Tolerance applied around the canister clock when comparing RRSIG
/// inception / expiration timestamps. 60 seconds matches BIND's default
/// and is the operational sweet spot — wide enough to absorb subnet
/// clock jitter, narrow enough that a stale signature never validates
/// for more than a minute past its declared expiration.
const CLOCK_SKEW_SECS: u64 = 60;

/// DNSKEY RDATA: Flags (2) | Protocol (1) | Algorithm (1) | Public Key (...).
/// The KSK flag is bit 0 of the 2-byte Flags field (= numeric value 1
/// when the field is interpreted as big-endian); the secure-entry-point
/// (SEP) bit per RFC 4034 §2.1.1.
const DNSKEY_FLAG_SEP: u16 = 0x0001;

/// Verify a caller-supplied DNS proof bundle against the configured trust
/// anchors and return the canonical leaf record.
///
/// `now` is in Unix seconds. In production this is `ic_cdk::api::time()
/// / 1_000_000_000`; tests pass a frozen value to keep RRSIG freshness
/// stable as captured signatures age past their expiration window.
pub fn verify_with_clock(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
    now: u64,
) -> Result<VerifiedRecord, DnssecError> {
    if trust_anchors.is_empty() {
        return Err(DnssecError::NoTrustAnchors);
    }

    // Step 1: the root DNSKEY RRset must contain a KSK whose DS digest
    // matches one of our configured anchors, and that DNSKEY must verify
    // the RRSIG over the entire root DNSKEY RRset.
    verify_root_dnskey(&bundle.root_dnskey, trust_anchors)?;
    check_freshness(&bundle.root_dnskey, now)?;

    // Step 2: walk the chain top-down. After each link, `parent_keys`
    // is the validated DNSKEY RRset for the *current* zone and is used
    // to verify the next link.
    let mut parent_keys: &SignedRRset = &bundle.root_dnskey;
    for link in &bundle.chain {
        verify_link(link, parent_keys, now)?;
        parent_keys = &link.child_dnskey;
    }

    // Step 3: the leaf RRset's RRSIG verifies under the deepest zone's
    // DNSKEY RRset (= parent_keys after the walk).
    verify_rrsig_under_dnskey_rrset(&bundle.leaf, parent_keys)?;
    check_freshness(&bundle.leaf, now)?;

    Ok(VerifiedRecord {
        name: bundle.leaf.name.clone(),
        rtype: bundle.leaf.rtype,
        rdata: bundle.leaf.rdata.clone(),
    })
}

/// Production wrapper that uses the canister's wall clock.
pub fn verify(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
) -> Result<VerifiedRecord, DnssecError> {
    verify_with_clock(bundle, trust_anchors, current_time_secs())
}

#[cfg(all(
    target_arch = "wasm32",
    target_vendor = "unknown",
    target_os = "unknown"
))]
fn current_time_secs() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

#[cfg(not(all(
    target_arch = "wasm32",
    target_vendor = "unknown",
    target_os = "unknown"
)))]
fn current_time_secs() -> u64 {
    // On non-wasm targets (unit tests, doc-tests) we don't have access
    // to the IC system API. Return 0; tests should call
    // `verify_with_clock` directly with a frozen `now` instead.
    0
}

fn verify_root_dnskey(
    root_dnskey: &SignedRRset,
    trust_anchors: &[DnssecRootAnchor],
) -> Result<(), DnssecError> {
    // Find a (DNSKEY, anchor) pair whose digest matches.
    for dnskey_rdata in &root_dnskey.rdata {
        for anchor in trust_anchors {
            if anchor.digest_type != 2 {
                // We only implement SHA-256 DS digests; ignore others.
                continue;
            }
            if anchor_matches_dnskey(anchor, &root_dnskey.name.0, dnskey_rdata) {
                // Found a candidate KSK. Verify the entire DNSKEY RRset
                // is signed by it.
                return verify_rrsig_under_dnskey(
                    root_dnskey,
                    dnskey_rdata,
                    root_dnskey.rrsig.algorithm,
                );
            }
        }
    }
    Err(DnssecError::RootAnchorMismatch)
}

fn anchor_matches_dnskey(
    anchor: &DnssecRootAnchor,
    owner_name: &[u8],
    dnskey_rdata: &[u8],
) -> bool {
    // The DS-equivalent of an anchor: key_tag + algorithm + digest_type
    // + digest. Reconstruct synthetic DS RDATA so we can reuse the same
    // ds_matches_dnskey function.
    let mut synth_ds = Vec::with_capacity(4 + anchor.digest.len());
    synth_ds.extend_from_slice(&anchor.key_tag.to_be_bytes());
    synth_ds.push(anchor.algorithm);
    synth_ds.push(anchor.digest_type);
    synth_ds.extend_from_slice(&anchor.digest);

    // Cheap pre-filter on algorithm + key_tag before computing SHA-256.
    if dnskey_rdata.len() < 4 || dnskey_rdata[3] != anchor.algorithm {
        return false;
    }
    if dnskey_key_tag(dnskey_rdata) != anchor.key_tag {
        return false;
    }
    ds_matches_dnskey(owner_name, dnskey_rdata, &synth_ds)
}

fn verify_link(
    link: &DelegationLink,
    parent_dnskey_rrset: &SignedRRset,
    now: u64,
) -> Result<(), DnssecError> {
    // 2a: the DS RRset is signed by the parent zone's DNSKEY RRset.
    verify_rrsig_under_dnskey_rrset(&link.child_ds, parent_dnskey_rrset)?;
    check_freshness(&link.child_ds, now)?;

    // 2b: at least one of the child's DNSKEY KSKs has a digest matching
    // one of the parent's DS records.
    let matched_ksk = pick_matching_ksk(&link.child_dnskey, &link.child_ds.rdata)?;

    // 2c: that KSK signed the child's entire DNSKEY RRset.
    verify_rrsig_under_dnskey(
        &link.child_dnskey,
        matched_ksk,
        link.child_dnskey.rrsig.algorithm,
    )?;
    check_freshness(&link.child_dnskey, now)?;
    Ok(())
}

fn pick_matching_ksk<'a>(
    child_dnskey_rrset: &'a SignedRRset,
    parent_ds_rdata: &[Vec<u8>],
) -> Result<&'a [u8], DnssecError> {
    for dnskey in &child_dnskey_rrset.rdata {
        // Only KSKs (SEP bit set) can be referenced by a DS in the parent.
        if !is_sep(dnskey) {
            continue;
        }
        for ds in parent_ds_rdata {
            if ds.len() < 4 {
                continue;
            }
            if ds_matches_dnskey(&child_dnskey_rrset.name.0, dnskey, ds) {
                return Ok(dnskey);
            }
        }
    }
    Err(DnssecError::DsMismatch)
}

fn is_sep(dnskey_rdata: &[u8]) -> bool {
    if dnskey_rdata.len() < 2 {
        return false;
    }
    let flags = u16::from_be_bytes([dnskey_rdata[0], dnskey_rdata[1]]);
    flags & DNSKEY_FLAG_SEP != 0
}

/// Verify `rrset.rrsig` against any DNSKEY in `dnskey_rrset` whose
/// algorithm and key_tag match the RRSIG.
fn verify_rrsig_under_dnskey_rrset(
    rrset: &SignedRRset,
    dnskey_rrset: &SignedRRset,
) -> Result<(), DnssecError> {
    let mut last_err = DnssecError::BadSignature;
    for dnskey_rdata in &dnskey_rrset.rdata {
        if dnskey_rdata.len() < 4 {
            continue;
        }
        if dnskey_rdata[3] != rrset.rrsig.algorithm {
            continue;
        }
        if dnskey_key_tag(dnskey_rdata) != rrset.rrsig.key_tag {
            continue;
        }
        match verify_rrsig_under_dnskey(rrset, dnskey_rdata, rrset.rrsig.algorithm) {
            Ok(()) => return Ok(()),
            Err(e) => last_err = e,
        }
    }
    Err(last_err)
}

fn verify_rrsig_under_dnskey(
    rrset: &SignedRRset,
    dnskey_rdata: &[u8],
    algorithm: u8,
) -> Result<(), DnssecError> {
    let signed_data = build_signed_data(rrset);
    verify_signature_for_alg(
        algorithm,
        &signed_data,
        &rrset.rrsig.signature,
        dnskey_rdata,
    )
}

/// RFC 4034 Appendix B: the DNSKEY's "key tag" is a 16-bit checksum over
/// the DNSKEY RDATA. For non-revoked algorithms (everything we support)
/// it's the standard arithmetic checksum. We compute it once and cache
/// at the call site by short-circuiting in the loops above.
fn dnskey_key_tag(dnskey_rdata: &[u8]) -> u16 {
    let mut acc: u32 = 0;
    for (i, &b) in dnskey_rdata.iter().enumerate() {
        if i & 1 == 0 {
            acc = acc.wrapping_add((b as u32) << 8);
        } else {
            acc = acc.wrapping_add(b as u32);
        }
    }
    acc = acc.wrapping_add((acc >> 16) & 0xFFFF);
    (acc & 0xFFFF) as u16
}

fn check_freshness(rrset: &SignedRRset, now: u64) -> Result<(), DnssecError> {
    let inception = rrset.rrsig.inception as u64;
    let expiration = rrset.rrsig.expiration as u64;
    if inception > now.saturating_add(CLOCK_SKEW_SECS) {
        return Err(DnssecError::StaleOrFutureSignature);
    }
    if expiration < now.saturating_sub(CLOCK_SKEW_SECS) {
        return Err(DnssecError::StaleOrFutureSignature);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::test_vectors::{
        load_anchors, load_bundle, CLOUDFLARE_COM_CHAIN_JSON, IANA_ROOT_ANCHORS_JSON,
    };
    use super::*;

    /// `now` frozen at the capture time of the cloudflare-com chain so
    /// the freshness check uses the validity window the captured RRSIGs
    /// were signed against.
    fn frozen_now() -> u64 {
        let bundle: serde_json::Value = serde_json::from_str(CLOUDFLARE_COM_CHAIN_JSON).unwrap();
        bundle["_meta"]["captured_at_unix"].as_u64().unwrap()
    }

    #[test]
    fn key_tag_matches_root_ksk_2017() {
        // The IANA-assigned key_tag for the 2017 root KSK is 20326. The
        // synthetic DNSKEY RDATA we hard-code in the anchor file's
        // public-key field would only match if the captured root_dnskey
        // contains that KSK with the 20326 tag.
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let mut found_2017 = false;
        for rdata in &bundle.root_dnskey.rdata {
            if dnskey_key_tag(rdata) == 20326 {
                found_2017 = true;
            }
        }
        assert!(found_2017, "captured root DNSKEY missing 2017 KSK");
    }

    #[test]
    fn verifies_real_cloudflare_chain() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        let result = verify_with_clock(&bundle, &anchors, frozen_now());
        match result {
            Ok(rec) => {
                assert_eq!(rec.rtype, 16, "expected TXT (16)");
                assert!(!rec.rdata.is_empty(), "leaf rdata must not be empty");
            }
            Err(e) => panic!("expected Ok, got Err({:?})", e),
        }
    }

    #[test]
    fn rejects_empty_anchor_list() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        assert_eq!(
            verify_with_clock(&bundle, &[], frozen_now()),
            Err(DnssecError::NoTrustAnchors)
        );
    }

    #[test]
    fn rejects_flipped_root_dnskey() {
        let mut bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Flip a byte deep inside the public-key field of the first KSK
        // (skip the 4-byte DNSKEY header so we don't accidentally hit
        // the algorithm and trigger UnsupportedAlgorithm before the
        // signature check). Position 100 lands inside the modulus.
        bundle.root_dnskey.rdata[0][100] ^= 0x01;
        match verify_with_clock(&bundle, &anchors, frozen_now()) {
            Err(DnssecError::RootAnchorMismatch) | Err(DnssecError::BadSignature) => {}
            other => panic!(
                "expected RootAnchorMismatch or BadSignature, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn rejects_flipped_leaf_signature() {
        let mut bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Flip a byte in the leaf signature.
        bundle.leaf.rrsig.signature[10] ^= 0x01;
        assert_eq!(
            verify_with_clock(&bundle, &anchors, frozen_now()),
            Err(DnssecError::BadSignature)
        );
    }

    #[test]
    fn rejects_wrong_trust_anchor() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        // A trust anchor with an arbitrary digest no real DNSKEY hashes to.
        let bad = vec![DnssecRootAnchor {
            key_tag: 9999,
            algorithm: 8,
            digest_type: 2,
            digest: serde_bytes::ByteBuf::from(vec![0xAA; 32]),
        }];
        assert_eq!(
            verify_with_clock(&bundle, &bad, frozen_now()),
            Err(DnssecError::RootAnchorMismatch)
        );
    }

    #[test]
    fn rejects_stale_signature() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Advance the clock far past every captured RRSIG's expiration.
        let very_late = frozen_now() + 365 * 24 * 3600 * 10;
        assert_eq!(
            verify_with_clock(&bundle, &anchors, very_late),
            Err(DnssecError::StaleOrFutureSignature)
        );
    }
}
