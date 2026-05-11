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
use super::wire::{DNSKEY_RDATA_HEADER_LEN, DS_DIGEST_TYPE_SHA256, DS_RDATA_HEADER_LEN};
use internet_identity_interface::internet_identity::types::DnssecRootAnchor;

/// Byte offset of the Algorithm field within DNSKEY RDATA
/// (RFC 4034 §2.1: Flags 2 bytes, Protocol 1 byte, then Algorithm).
const DNSKEY_ALGORITHM_OFFSET: usize = 3;

/// Secure Entry Point flag in DNSKEY Flags — RFC 4034 §2.1.1 /
/// RFC 3757. Set on DNSKEYs that are meant to be referenced from the
/// parent zone's DS RRset (i.e. KSKs). Only KSKs are candidates when
/// matching a child DNSKEY against a parent DS.
const DNSKEY_FLAG_SEP: u16 = 0x0001;

/// Tolerance applied around the canister wall clock when comparing
/// RRSIG inception / expiration timestamps. 60 seconds matches BIND's
/// long-standing default and is the operational sweet spot: wide
/// enough to absorb any plausible IC subnet clock jitter, narrow
/// enough that a stale signature never validates for more than a
/// minute past its declared expiration. RFC 4034 §3.1.5 leaves the
/// tolerance up to the validator; we pick a small constant deliberately.
const CLOCK_SKEW_SECS: u64 = 60;

/// Verify a caller-supplied DNS proof bundle against the configured
/// trust anchors and return the canonical leaf record.
///
/// `now` is in Unix seconds. In production this is the canister wall
/// clock; tests pass a frozen value so RRSIG freshness checks remain
/// stable as captured signatures age past their original expiration
/// window.
pub fn verify_with_clock(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
    now: u64,
) -> Result<VerifiedRecord, DnssecError> {
    if trust_anchors.is_empty() {
        return Err(DnssecError::NoTrustAnchors);
    }

    // Step 1: the root DNSKEY RRset must contain a KSK whose DS digest
    // matches one of our configured anchors, and that DNSKEY must
    // verify the RRSIG over the entire root DNSKEY RRset.
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
    // DNSKEY RRset (= `parent_keys` after the walk).
    verify_rrsig_under_dnskey_rrset(&bundle.leaf, parent_keys)?;
    check_freshness(&bundle.leaf, now)?;

    Ok(VerifiedRecord {
        name: bundle.leaf.name.clone(),
        rtype: bundle.leaf.rtype,
        rdata: bundle.leaf.rdata.clone(),
    })
}

/// Production wrapper that reads `now` from the IC system API.
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
    /// The IC system API returns time in nanoseconds; RRSIG fields
    /// are 32-bit Unix-seconds (RFC 4034 §3.1.5).
    const NANOS_PER_SEC: u64 = 1_000_000_000;
    ic_cdk::api::time() / NANOS_PER_SEC
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

/// Step 1 of the verification algorithm. The root DNSKEY RRset must
/// contain at least one KSK whose DS-style digest matches a configured
/// trust anchor, and the RRSIG covering the RRset must verify under
/// that KSK (RFC 4035 §5).
///
/// During a KSK rollover the operator may configure multiple anchors
/// at once — the rolling-out KSK plus the rolling-in pre-published
/// KSK (RFC 5011 §2). Only one of them is currently being used to
/// sign the DNSKEY RRset, so a "first digest match wins" strategy
/// risks short-circuiting onto the inactive key and never trying the
/// active one. We therefore attempt every matching candidate before
/// reporting failure.
fn verify_root_dnskey(
    root_dnskey: &SignedRRset,
    trust_anchors: &[DnssecRootAnchor],
) -> Result<(), DnssecError> {
    let mut had_digest_match = false;
    let mut last_signature_err: Option<DnssecError> = None;

    for dnskey_rdata in &root_dnskey.rdata {
        for anchor in trust_anchors {
            // We only implement SHA-256 DS digests (RFC 4509 /
            // RFC 8624 §3.3). Anchors with any other digest type
            // are skipped — and because the IANA root anchors are
            // SHA-256 anyway, an anchor list with only non-SHA-256
            // entries falls through to `RootAnchorMismatch` below.
            if anchor.digest_type != DS_DIGEST_TYPE_SHA256 {
                continue;
            }
            if !anchor_matches_dnskey(anchor, &root_dnskey.name.0, dnskey_rdata) {
                continue;
            }
            had_digest_match = true;
            match verify_rrsig_under_dnskey(
                root_dnskey,
                dnskey_rdata,
                root_dnskey.rrsig.algorithm,
            ) {
                Ok(()) => return Ok(()),
                Err(e) => last_signature_err = Some(e),
            }
        }
    }

    // If at least one anchor matched a DNSKEY by digest but every
    // signature check failed, surface the cryptographic error
    // (typically `BadSignature`) rather than the generic anchor
    // mismatch — the operator-facing distinction is important when
    // diagnosing a misconfigured anchor vs. a stale capture.
    if had_digest_match {
        Err(last_signature_err.unwrap_or(DnssecError::RootAnchorMismatch))
    } else {
        Err(DnssecError::RootAnchorMismatch)
    }
}

/// Check whether a root DNSKEY matches a configured trust anchor.
///
/// A trust anchor is a DS-RDATA-shaped record without the wrapping
/// owner / TTL: `key_tag | algorithm | digest_type | digest` per
/// RFC 4034 §5.1. We synthesise the DS-RDATA bytes from the anchor
/// fields and reuse `ds_matches_dnskey` so there is a single
/// implementation of the DS-vs-DNSKEY digest comparison.
fn anchor_matches_dnskey(
    anchor: &DnssecRootAnchor,
    owner_name: &[u8],
    dnskey_rdata: &[u8],
) -> bool {
    // Cheap pre-filter on algorithm + key_tag before computing the
    // SHA-256 digest of the DNSKEY RDATA.
    if dnskey_rdata.len() < DNSKEY_RDATA_HEADER_LEN {
        return false;
    }
    if dnskey_rdata[DNSKEY_ALGORITHM_OFFSET] != anchor.algorithm {
        return false;
    }
    if dnskey_key_tag(dnskey_rdata) != anchor.key_tag {
        return false;
    }

    let mut synth_ds = Vec::with_capacity(DS_RDATA_HEADER_LEN + anchor.digest.len());
    synth_ds.extend_from_slice(&anchor.key_tag.to_be_bytes());
    synth_ds.push(anchor.algorithm);
    synth_ds.push(anchor.digest_type);
    synth_ds.extend_from_slice(&anchor.digest);

    ds_matches_dnskey(owner_name, dnskey_rdata, &synth_ds)
}

/// Step 2 of the verification algorithm. Validate one delegation link:
/// the parent zone's DNSKEY RRset signs the child's DS RRset, that DS
/// RRset references a KSK in the child's DNSKEY RRset, and that KSK
/// signs the child's DNSKEY RRset (RFC 4035 §5).
fn verify_link(
    link: &DelegationLink,
    parent_dnskey_rrset: &SignedRRset,
    now: u64,
) -> Result<(), DnssecError> {
    // 2a — the DS RRset is signed by the parent zone's DNSKEY RRset.
    verify_rrsig_under_dnskey_rrset(&link.child_ds, parent_dnskey_rrset)?;
    check_freshness(&link.child_ds, now)?;

    // 2b — at least one of the child's DNSKEY KSKs has a digest
    //      matching one of the parent's DS records.
    let matched_ksk = pick_matching_ksk(&link.child_dnskey, &link.child_ds.rdata)?;

    // 2c — that KSK signed the child's entire DNSKEY RRset.
    verify_rrsig_under_dnskey(
        &link.child_dnskey,
        matched_ksk,
        link.child_dnskey.rrsig.algorithm,
    )?;
    check_freshness(&link.child_dnskey, now)?;
    Ok(())
}

/// Find a KSK in `child_dnskey_rrset` whose DS-style digest matches
/// one of the parent zone's published DS records (RFC 4035 §5.2).
fn pick_matching_ksk<'a>(
    child_dnskey_rrset: &'a SignedRRset,
    parent_ds_rdata: &[Vec<u8>],
) -> Result<&'a [u8], DnssecError> {
    for dnskey in &child_dnskey_rrset.rdata {
        // Only KSKs (SEP bit set, RFC 4034 §2.1.1) can be referenced
        // by a DS in the parent. ZSKs are excluded structurally.
        if !is_sep(dnskey) {
            continue;
        }
        for ds in parent_ds_rdata {
            if ds.len() < DS_RDATA_HEADER_LEN {
                continue;
            }
            if ds_matches_dnskey(&child_dnskey_rrset.name.0, dnskey, ds) {
                return Ok(dnskey);
            }
        }
    }
    Err(DnssecError::DsMismatch)
}

/// True iff this DNSKEY has the Secure Entry Point flag set
/// (RFC 4034 §2.1.1).
fn is_sep(dnskey_rdata: &[u8]) -> bool {
    /// DNSKEY Flags is a u16 — two bytes (RFC 4034 §2.1).
    const FLAGS_LEN: usize = 2;
    if dnskey_rdata.len() < FLAGS_LEN {
        return false;
    }
    let flags = u16::from_be_bytes([dnskey_rdata[0], dnskey_rdata[1]]);
    flags & DNSKEY_FLAG_SEP != 0
}

/// Verify `rrset.rrsig` against any DNSKEY in `dnskey_rrset` whose
/// algorithm and key_tag match the RRSIG (RFC 4034 §3.1.6).
///
/// A DNSKEY RRset may legitimately contain several keys (KSK + one or
/// more ZSKs; rollovers temporarily double up keys for the same role)
/// so we try every candidate and only return the last error if none
/// worked.
fn verify_rrsig_under_dnskey_rrset(
    rrset: &SignedRRset,
    dnskey_rrset: &SignedRRset,
) -> Result<(), DnssecError> {
    let mut last_err = DnssecError::BadSignature;
    for dnskey_rdata in &dnskey_rrset.rdata {
        if dnskey_rdata.len() < DNSKEY_RDATA_HEADER_LEN {
            continue;
        }
        if dnskey_rdata[DNSKEY_ALGORITHM_OFFSET] != rrset.rrsig.algorithm {
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

/// Verify a single RRSIG against a single DNSKEY: build the
/// canonical signed-data bytes and dispatch to the algorithm-specific
/// signature check (RFC 4034 §3.1.8.1).
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

/// Compute a DNSKEY's "key tag" per RFC 4034 Appendix B.1 — a 16-bit
/// checksum of the DNSKEY RDATA computed by summing even-offset bytes
/// shifted left by 8 and odd-offset bytes as-is, then folding any
/// carry back into the low 16 bits.
///
/// This is *not* a cryptographic hash; it's a hint used by validators
/// to skip DNSKEYs that obviously don't match an RRSIG's `key_tag`
/// field before doing the expensive signature check.
///
/// RFC 4034 Appendix B defines a different algorithm for the historic
/// RSA-MD5 (algorithm 1), but we don't accept that algorithm at all —
/// `verify_signature_for_alg` rejects everything outside the RFC 8624
/// MUST set — so only the "all other algorithms" branch is implemented.
fn dnskey_key_tag(dnskey_rdata: &[u8]) -> u16 {
    /// Mask used to fold the carry-out of the 32-bit accumulator
    /// back into the lower 16 bits per RFC 4034 Appendix B.1.
    const U16_MASK: u32 = 0xFFFF;
    /// Number of bits to shift the carry-out down to the low 16 bits.
    const CARRY_SHIFT: u32 = 16;

    let mut acc: u32 = 0;
    for (i, &b) in dnskey_rdata.iter().enumerate() {
        if i & 1 == 0 {
            // Even-indexed byte: contributes to the high octet of the
            // 16-bit accumulator (RFC 4034 Appendix B.1).
            acc = acc.wrapping_add((b as u32) << 8);
        } else {
            // Odd-indexed byte: contributes to the low octet.
            acc = acc.wrapping_add(b as u32);
        }
    }
    // Fold the high 16 bits of accumulated carries back in.
    acc = acc.wrapping_add((acc >> CARRY_SHIFT) & U16_MASK);
    (acc & U16_MASK) as u16
}

/// Step 4 of the verification algorithm. Reject an RRSIG whose
/// validity window has not yet started or has already expired,
/// allowing a small skew on either side (RFC 4034 §3.1.5).
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
        load_anchors, load_bundle, CLOUDFLARE_COM_CHAIN_JSON, ED25519_NL_CHAIN_JSON,
        IANA_ROOT_ANCHORS_JSON, PROTONMAIL_COM_CHAIN_JSON, PROTON_ME_CHAIN_JSON,
        TUTANOTA_COM_CHAIN_JSON,
    };
    use super::*;

    /// Numeric `TYPE` value for TXT records (RFC 1035 §3.2.2).
    const TYPE_TXT: u16 = 16;
    /// Numeric `TYPE` value for A records (RFC 1035 §3.2.2).
    const TYPE_A: u16 = 1;

    /// `now` frozen at the capture time of each chain so the freshness
    /// check uses the validity window the captured RRSIGs were signed
    /// against. RRSIGs are typically valid for one to two weeks, so
    /// without freezing the clock the tests would start failing as
    /// soon as the captures rolled out of their expiration windows.
    fn frozen_now(chain_json: &str) -> u64 {
        let bundle: serde_json::Value = serde_json::from_str(chain_json).unwrap();
        bundle["_meta"]["captured_at_unix"].as_u64().unwrap()
    }

    #[test]
    fn key_tag_matches_root_ksk_2017() {
        /// The 2017 IANA root KSK has key tag 20326. Sanity-check
        /// that our captured root DNSKEY actually includes the 2017
        /// KSK — otherwise the trust-anchor file would be mismatched
        /// against the captured chain.
        const ROOT_KSK_2017_KEY_TAG: u16 = 20326;
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let found = bundle
            .root_dnskey
            .rdata
            .iter()
            .any(|rdata| dnskey_key_tag(rdata) == ROOT_KSK_2017_KEY_TAG);
        assert!(found, "captured root DNSKEY missing 2017 KSK");
    }

    fn assert_verifies(chain_json: &str, expected_rtype: u16) {
        let bundle = load_bundle(chain_json);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        match verify_with_clock(&bundle, &anchors, frozen_now(chain_json)) {
            Ok(rec) => {
                assert_eq!(rec.rtype, expected_rtype);
                assert!(!rec.rdata.is_empty(), "leaf rdata must not be empty");
            }
            Err(e) => panic!("expected Ok, got Err({:?})", e),
        }
    }

    #[test]
    fn verifies_cloudflare_com_chain() {
        // ECDSA-P256-SHA256 leaf under ECDSA-P256-SHA256 com under
        // RSA-SHA256 root.
        assert_verifies(CLOUDFLARE_COM_CHAIN_JSON, TYPE_TXT);
    }

    #[test]
    fn verifies_proton_me_chain() {
        // RSA-SHA256 end-to-end — covers the RSA leaf path which is
        // otherwise only exercised at the root in the cloudflare chain.
        assert_verifies(PROTON_ME_CHAIN_JSON, TYPE_TXT);
    }

    #[test]
    fn verifies_protonmail_com_chain() {
        // ECDSA-P256-SHA256 leaf — email-recovery target.
        assert_verifies(PROTONMAIL_COM_CHAIN_JSON, TYPE_TXT);
    }

    #[test]
    fn verifies_tutanota_com_chain() {
        // ECDSA-P256-SHA256 leaf — email-recovery target.
        assert_verifies(TUTANOTA_COM_CHAIN_JSON, TYPE_TXT);
    }

    #[test]
    fn verifies_ed25519_nl_chain() {
        // Ed25519 leaf — real-data coverage for the rarest RFC 8624
        // MUST algorithm. Chain: RSA-SHA256 root → ECDSA-P256-SHA256
        // nl → Ed25519 leaf.
        assert_verifies(ED25519_NL_CHAIN_JSON, TYPE_A);
    }

    #[test]
    fn rejects_empty_anchor_list() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        assert_eq!(
            verify_with_clock(&bundle, &[], frozen_now(CLOUDFLARE_COM_CHAIN_JSON)),
            Err(DnssecError::NoTrustAnchors)
        );
    }

    #[test]
    fn rejects_flipped_root_dnskey() {
        // Offset chosen to land well past the DNSKEY header so we
        // don't accidentally hit the algorithm byte (which would
        // short-circuit to UnsupportedAlgorithm before the signature
        // check).
        const FLIP_OFFSET: usize = DNSKEY_RDATA_HEADER_LEN + 96;
        let mut bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        bundle.root_dnskey.rdata[0][FLIP_OFFSET] ^= 0x01;
        match verify_with_clock(&bundle, &anchors, frozen_now(CLOUDFLARE_COM_CHAIN_JSON)) {
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
        // Flip a byte well inside the signature field, past any
        // algorithm-specific header bytes.
        bundle.leaf.rrsig.signature[10] ^= 0x01;
        assert_eq!(
            verify_with_clock(&bundle, &anchors, frozen_now(CLOUDFLARE_COM_CHAIN_JSON)),
            Err(DnssecError::BadSignature)
        );
    }

    #[test]
    fn rejects_wrong_trust_anchor() {
        /// Numeric DNSSEC algorithm for RSA-SHA256 (RFC 5702). We
        /// match a real anchor's algorithm so the early pre-filter
        /// doesn't reject our synthetic anchor on a triviality —
        /// the test must hit the digest-mismatch path, not the
        /// algorithm-mismatch path.
        const ALG_RSA_SHA256: u8 = 8;
        /// Length in bytes of the SHA-256 digest stored in a DS RR
        /// (RFC 4509 §2.1).
        const SHA256_DIGEST_LEN: usize = 32;

        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let bad = vec![DnssecRootAnchor {
            key_tag: 9999,
            algorithm: ALG_RSA_SHA256,
            digest_type: DS_DIGEST_TYPE_SHA256,
            digest: serde_bytes::ByteBuf::from(vec![0xAA; SHA256_DIGEST_LEN]),
        }];
        assert_eq!(
            verify_with_clock(&bundle, &bad, frozen_now(CLOUDFLARE_COM_CHAIN_JSON)),
            Err(DnssecError::RootAnchorMismatch)
        );
    }

    #[test]
    fn rejects_stale_signature() {
        /// Advance the clock ten years past every captured RRSIG's
        /// expiration — well outside the `CLOCK_SKEW_SECS` tolerance.
        const TEN_YEARS_SECS: u64 = 10 * 365 * 24 * 3600;
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        let very_late = frozen_now(CLOUDFLARE_COM_CHAIN_JSON) + TEN_YEARS_SECS;
        assert_eq!(
            verify_with_clock(&bundle, &anchors, very_late),
            Err(DnssecError::StaleOrFutureSignature)
        );
    }
}
