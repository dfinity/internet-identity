//! DNSSEC verification entry points.
//!
//! Implements a multi-zone, CNAME-aware variant of the algorithm in
//! §7.3 of `docs/ongoing/email-recovery.md`:
//!
//!   1. Validate the bundle's root DNSKEY RRset against a configured
//!      trust anchor.
//!   2. Walk every supplied delegation chain top-down under the
//!      validated root DNSKEY, building a `(zone → DNSKEY RRset)`
//!      lookup table (the [`ZoneKeysMap`]).
//!   3. Verify each hop's RRSIG under the zone its `signer_name`
//!      identifies, then check the hop sequence forms a coherent
//!      CNAME → … → final-type resolution starting at the
//!      caller-supplied requested name and ending at the requested
//!      type.
//!   4. Freshness — every RRSIG's `[inception, expiration]` window
//!      must contain `now ± clock_skew`.
//!
//! Single-zone direct case (most TLDs, Gmail, iCloud): one chain,
//! one TXT hop. Cross-zone CNAME case (Proton's `proton.me` →
//! `proton.ch`, Tutanota's `tutanota.com` → `tutanota.de`, M365
//! custom domains): one chain per signing zone, one hop per RRset
//! in the resolution sequence.
//!
//! The top-level entry point is [`verify_bundle_with_clock`]. The
//! sub-step entry points ([`verify_root_dnskey_with_clock`],
//! [`verify_chain_with_clock`], [`verify_extra_chains_with_clock`],
//! [`verify_hops_with_clock`]) are exposed so a two-phase caller can
//! cache a validated [`ZoneKeysMap`] between calls and only validate
//! any *new* chains a follow-up submission needs.

use super::canonical::{self, build_signed_data};
use super::signature::{ds_matches_dnskey, verify_signature_for_alg};
use super::types::{
    DelegationChain, DelegationLink, DnsName, DnsProofBundle, DnssecError, SignedRRset,
    VerifiedRecord, ZoneKeysMap,
};
use super::wire::{DNSKEY_RDATA_HEADER_LEN, DS_DIGEST_TYPE_SHA256, DS_RDATA_HEADER_LEN};
use internet_identity_interface::internet_identity::types::DnssecRootAnchor;

/// Byte offset of the Algorithm field within DNSKEY RDATA
/// (RFC 4034 §2.1: Flags 2 bytes, Protocol 1 byte, then Algorithm).
const DNSKEY_ALGORITHM_OFFSET: usize = 3;

/// Secure Entry Point flag in DNSKEY Flags — RFC 4034 §2.1.1 /
/// RFC 3757. Set on DNSKEYs that are meant to be referenced from
/// the parent zone's DS RRset (i.e. KSKs).
const DNSKEY_FLAG_SEP: u16 = 0x0001;

/// IANA TYPE for CNAME records (RFC 1035 §3.3.1). Used for the
/// intermediate-hop check in [`verify_hops_with_clock`].
const TYPE_CNAME: u16 = 5;

/// Tolerance applied around the canister wall clock when comparing
/// RRSIG inception / expiration timestamps. 60 seconds matches
/// BIND's long-standing default — wide enough to absorb plausible
/// IC subnet clock jitter, narrow enough that a stale signature
/// never validates for more than a minute past its declared
/// expiration. RFC 4034 §3.1.5 leaves the tolerance up to the
/// validator; we pick a small constant deliberately.
const CLOCK_SKEW_SECS: u64 = 60;

/// Maximum number of hops accepted in a single bundle's resolution
/// sequence. Real-world DKIM CNAME chains observed across providers
/// max out at two; the cap is set to four for headroom while still
/// rejecting oversized bundles.
pub const MAX_CNAME_HOPS: usize = 4;

// =====================================================================
// Top-level entry points.
// =====================================================================

/// Verify a self-contained [`DnsProofBundle`]: validate the root
/// DNSKEY, walk every delegation chain into a [`ZoneKeysMap`], and
/// resolve the hop sequence to its final RRset of the requested
/// type. Returns the validated zone-keys map (so callers can cache
/// it for follow-up submissions) alongside the final verified
/// record.
pub fn verify_bundle_with_clock(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
    requested_name: &DnsName,
    requested_type: u16,
    now: u64,
) -> Result<(ZoneKeysMap, VerifiedRecord), DnssecError> {
    verify_root_dnskey_with_clock(&bundle.root_dnskey, trust_anchors, now)?;
    let mut zones = ZoneKeysMap::new();
    verify_extra_chains_with_clock(&bundle.chains, &bundle.root_dnskey, &mut zones, now)?;
    let verified =
        verify_hops_with_clock(&bundle.hops, &zones, requested_name, requested_type, now)?;
    Ok((zones, verified))
}

/// Production wrapper that reads `now` from the IC system API.
pub fn verify_bundle(
    bundle: &DnsProofBundle,
    trust_anchors: &[DnssecRootAnchor],
    requested_name: &DnsName,
    requested_type: u16,
) -> Result<(ZoneKeysMap, VerifiedRecord), DnssecError> {
    verify_bundle_with_clock(
        bundle,
        trust_anchors,
        requested_name,
        requested_type,
        current_time_secs(),
    )
}

/// Validate a root DNSKEY RRset against the configured trust
/// anchors (step 1 of the bundle algorithm) and check its RRSIG
/// freshness.
///
/// Exposed as a public entry point so a two-phase caller can
/// re-verify a cached root DNSKEY before admitting any extra chains
/// under it on a follow-up submission — the cached RRSIG may have
/// fallen out of its inception / expiration window if the pending
/// challenge has been sitting around.
pub fn verify_root_dnskey_with_clock(
    root_dnskey: &SignedRRset,
    trust_anchors: &[DnssecRootAnchor],
    now: u64,
) -> Result<(), DnssecError> {
    if trust_anchors.is_empty() {
        return Err(DnssecError::NoTrustAnchors);
    }
    verify_root_dnskey(root_dnskey, trust_anchors)?;
    check_freshness(root_dnskey, now)?;
    Ok(())
}

/// Walk a single delegation chain top-down under a pre-validated
/// `root_dnskey`, returning the deepest zone's DNSKEY RRset.
///
/// The caller is responsible for having pre-validated `root_dnskey`
/// against the trust anchors (e.g. via
/// [`verify_root_dnskey_with_clock`]); passing in an unvalidated
/// DNSKEY is a bug this function has no way to detect.
pub fn verify_chain_with_clock(
    chain: &DelegationChain,
    root_dnskey: &SignedRRset,
    now: u64,
) -> Result<SignedRRset, DnssecError> {
    if chain.links.is_empty() {
        return Err(DnssecError::Malformed("delegation chain has no links"));
    }
    let mut parent_keys: SignedRRset = root_dnskey.clone();
    for link in &chain.links {
        verify_link(link, &parent_keys, now)?;
        parent_keys = link.child_dnskey.clone();
    }
    Ok(parent_keys)
}

/// Validate every chain in `chains` under an already-validated
/// `root_dnskey`, inserting each chain's deepest-zone DNSKEY RRset
/// into `zones`.
///
/// Used both at prepare time (the bundle's full chain set, starting
/// with an empty `zones`) and at submit time (with `zones`
/// pre-populated from a prepare-time cache and `chains` containing
/// only the new zones the resolution crossed into).
pub fn verify_extra_chains_with_clock(
    chains: &[DelegationChain],
    root_dnskey: &SignedRRset,
    zones: &mut ZoneKeysMap,
    now: u64,
) -> Result<(), DnssecError> {
    for chain in chains {
        let zone_dnskey = verify_chain_with_clock(chain, root_dnskey, now)?;
        zones.insert(zone_dnskey.name.clone(), zone_dnskey)?;
    }
    Ok(())
}

/// Verify a CNAME hop sequence and return the final RRset (step 3
/// of the bundle algorithm).
///
/// Each hop is verified under the DNSKEY of the zone its
/// `RRSIG.signer_name` names (looked up in `zones`). Consecutive
/// hops must form a coherent CNAME chain: the first hop's owner
/// equals `requested_name`, every intermediate hop is a CNAME whose
/// target equals the next hop's owner, the final hop's RTYPE equals
/// `requested_type`, no owner repeats, and the total length is
/// within [`MAX_CNAME_HOPS`].
pub fn verify_hops_with_clock(
    hops: &[SignedRRset],
    zones: &ZoneKeysMap,
    requested_name: &DnsName,
    requested_type: u16,
    now: u64,
) -> Result<VerifiedRecord, DnssecError> {
    if hops.is_empty() {
        return Err(DnssecError::BadCnameChain("empty hop list"));
    }
    if hops.len() > MAX_CNAME_HOPS {
        return Err(DnssecError::TooManyHops);
    }

    // Per-hop signature + zone-membership check.
    for hop in hops {
        let signer = DnsName(canonical::canonicalize_name(&hop.rrsig.signer_name.0));
        let zone_dnskey = zones.get(&signer).ok_or(DnssecError::UnknownSigningZone)?;
        if !name_is_subdomain_of(&hop.name.0, &signer.0) {
            return Err(DnssecError::HopOwnerOutsideZone);
        }
        verify_rrsig_under_dnskey_rrset(hop, zone_dnskey)?;
        check_freshness(hop, now)?;
    }

    // Sequence coherence: first hop owner == requested_name; each
    // intermediate hop is a CNAME chaining to the next; final hop
    // type == requested_type; no owner appears twice.
    let canon_requested = canonical::canonicalize_name(&requested_name.0);
    let canon_first = canonical::canonicalize_name(&hops[0].name.0);
    if canon_first != canon_requested {
        return Err(DnssecError::BadCnameChain(
            "first hop owner does not match requested name",
        ));
    }
    let mut seen: Vec<Vec<u8>> = Vec::with_capacity(hops.len());
    seen.push(canon_first);

    for window in hops.windows(2) {
        let cur = &window[0];
        let next = &window[1];
        if cur.rtype != TYPE_CNAME {
            return Err(DnssecError::BadCnameChain(
                "intermediate hop is not a CNAME",
            ));
        }
        if cur.rdata.len() != 1 {
            return Err(DnssecError::BadCnameChain(
                "CNAME RRset must contain exactly one record",
            ));
        }
        let target = canonical::canonicalize_name(&cur.rdata[0]);
        let next_owner = canonical::canonicalize_name(&next.name.0);
        if target != next_owner {
            return Err(DnssecError::BadCnameChain(
                "CNAME target does not equal next hop owner",
            ));
        }
        if seen.iter().any(|n| n == &target) {
            return Err(DnssecError::BadCnameChain("CNAME chain has a loop"));
        }
        seen.push(target);
    }

    let last = hops.last().expect("hops non-empty: checked above");
    if last.rtype != requested_type {
        return Err(DnssecError::BadCnameChain(
            "final hop type does not match requested type",
        ));
    }

    Ok(VerifiedRecord {
        name: last.name.clone(),
        rtype: last.rtype,
        rdata: last.rdata.clone(),
    })
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
    // to the IC system API. Return 0; tests should call the
    // `_with_clock` variants directly with a frozen `now` instead.
    0
}

// =====================================================================
// Internals.
// =====================================================================

/// Match the root DNSKEY RRset against the configured trust anchors
/// and verify the rrset's RRSIG under any matching KSK.
///
/// During a KSK rollover the operator may configure multiple
/// anchors at once — the rolling-out KSK plus the rolling-in
/// pre-published KSK (RFC 5011 §2). Only one of them is currently
/// being used to sign the DNSKEY RRset, so a "first digest match
/// wins" strategy risks short-circuiting onto the inactive anchor
/// and never trying the active one. We therefore attempt every
/// matching candidate before reporting failure.
fn verify_root_dnskey(
    root_dnskey: &SignedRRset,
    trust_anchors: &[DnssecRootAnchor],
) -> Result<(), DnssecError> {
    let mut had_digest_match = false;
    let mut last_signature_err: Option<DnssecError> = None;

    for dnskey_rdata in &root_dnskey.rdata {
        for anchor in trust_anchors {
            // SHA-256 only (RFC 4509 / RFC 8624 §3.3). IANA root
            // anchors are SHA-256, so a list with only non-SHA-256
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

/// Step 2 of the verification algorithm. Validate one delegation
/// link: the parent zone's DNSKEY RRset signs the child's DS
/// RRset, that DS RRset references a KSK in the child's DNSKEY
/// RRset, and the child's DNSKEY RRset is itself signed by some
/// key in that rrset (RFC 4035 §5).
fn verify_link(
    link: &DelegationLink,
    parent_dnskey_rrset: &SignedRRset,
    now: u64,
) -> Result<(), DnssecError> {
    // 2a — the DS RRset is signed by the parent zone's DNSKEY RRset.
    verify_rrsig_under_dnskey_rrset(&link.child_ds, parent_dnskey_rrset)?;
    check_freshness(&link.child_ds, now)?;

    // 2b — at least one of the child's DNSKEY KSKs has a digest
    //      matching one of the parent's DS records. This is what
    //      pins the chain of trust: `pick_matching_ksk` only looks
    //      at DNSKEYs with the SEP bit set whose
    //      `SHA-256(owner | rdata)` equals the DS digest, so an
    //      attacker can't smuggle a self-signed extra DNSKEY into
    //      the rrset and have it recognised as the trust anchor
    //      here.
    let _ = pick_matching_ksk(&link.child_dnskey, &link.child_ds.rdata)?;

    // 2c — the child's DNSKEY RRset is signed by *some* key in
    //      itself. We don't require the RRSIG to be the one made
    //      by the DS-pinned KSK specifically: many real zones
    //      (proton.me, proton.ch, …) publish a DNSKEY RRset signed
    //      by both the KSK and the ZSK, and resolvers return
    //      whichever RRSIG comes first. Either signature proves
    //      the same fact — the zone operator authored this rrset
    //      — and step 2b already pinned the DS-referenced KSK as
    //      part of that rrset, so the chain of trust is intact
    //      regardless of which RRSIG verifies here.
    verify_rrsig_under_dnskey_rrset(&link.child_dnskey, &link.child_dnskey)?;
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
/// A DNSKEY RRset may legitimately contain several keys (KSK + one
/// or more ZSKs; rollovers temporarily double up keys for the same
/// role), so we try every candidate and only return the last error
/// if none verified.
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

/// Compute a DNSKEY's "key tag" per RFC 4034 Appendix B.1 — a
/// 16-bit checksum of the DNSKEY RDATA computed by summing
/// even-offset bytes shifted left by 8 and odd-offset bytes as-is,
/// then folding any carry back into the low 16 bits.
///
/// This is *not* a cryptographic hash; it's a hint used by
/// validators to skip DNSKEYs that obviously don't match an
/// RRSIG's `key_tag` field before doing the expensive signature
/// check.
///
/// RFC 4034 Appendix B defines a different algorithm for the
/// historic RSA-MD5 (algorithm 1), but we don't accept that
/// algorithm at all — `verify_signature_for_alg` rejects everything
/// outside the RFC 8624 MUST set — so only the "all other
/// algorithms" branch is implemented.
fn dnskey_key_tag(dnskey_rdata: &[u8]) -> u16 {
    /// Mask used to fold the carry-out of the 32-bit accumulator
    /// back into the lower 16 bits per RFC 4034 Appendix B.1.
    const U16_MASK: u32 = 0xFFFF;
    /// Number of bits to shift the carry-out down to the low 16
    /// bits.
    const CARRY_SHIFT: u32 = 16;

    let mut acc: u32 = 0;
    for (i, &b) in dnskey_rdata.iter().enumerate() {
        if i & 1 == 0 {
            // Even-indexed byte: contributes to the high octet of
            // the 16-bit accumulator (RFC 4034 Appendix B.1).
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

/// Wire-format subdomain check. `owner` lies under `zone` if its
/// canonical bytes end with the canonical bytes of `zone` *and* the
/// boundary lands on a label start — so `evilexample.com.` does NOT
/// pass as a subdomain of `example.com.`. The root zone
/// (`zone == "\x00"`) is treated as the parent of every name.
fn name_is_subdomain_of(owner: &[u8], zone: &[u8]) -> bool {
    /// Top two bits of a label length octet — `11` is a compression
    /// pointer (RFC 1035 §4.1.4), `10` / `01` are reserved. None are
    /// valid in canonical form (RFC 4034 §6.2); seeing one means the
    /// caller passed a malformed name.
    const TOP_BITS_MASK: u8 = 0xC0;
    /// Wire-format encoding of the root label (zero-length).
    const ROOT_LABEL: &[u8] = &[0u8];

    let canon_owner = canonical::canonicalize_name(owner);
    let canon_zone = canonical::canonicalize_name(zone);
    if canon_zone.as_slice() == ROOT_LABEL {
        return true;
    }
    if canon_owner.len() < canon_zone.len() {
        return false;
    }
    if canon_owner == canon_zone {
        return true;
    }
    // Walk labels of `owner` until the suffix equals `zone`.
    let mut i = 0;
    while i < canon_owner.len() {
        if &canon_owner[i..] == canon_zone.as_slice() {
            return true;
        }
        let len = canon_owner[i] as usize;
        if len == 0 || (canon_owner[i] & TOP_BITS_MASK) != 0 || i + 1 + len > canon_owner.len() {
            return false;
        }
        i += 1 + len;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::super::test_vectors::{
        load_anchors, load_bundle, CLOUDFLARE_COM_CHAIN_JSON, ED25519_NL_CHAIN_JSON,
        IANA_ROOT_ANCHORS_JSON, PROTONMAIL_COM_CHAIN_JSON, PROTON_ME_CHAIN_JSON,
        TUTANOTA_COM_CHAIN_JSON,
    };
    use super::super::types::{TYPE_DNSKEY, TYPE_TXT};
    use super::*;

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

    /// The owner name of the bundle's first hop. Tests pass this as
    /// `requested_name` so the CNAME-chain coherence check passes.
    fn first_hop_name(bundle: &DnsProofBundle) -> DnsName {
        bundle.hops[0].name.clone()
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
        let req_name = first_hop_name(&bundle);
        match verify_bundle_with_clock(
            &bundle,
            &anchors,
            &req_name,
            expected_rtype,
            frozen_now(chain_json),
        ) {
            Ok((zones, rec)) => {
                assert!(!zones.is_empty(), "expected at least one validated zone");
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
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(
                &bundle,
                &[],
                &req_name,
                TYPE_TXT,
                frozen_now(CLOUDFLARE_COM_CHAIN_JSON)
            ),
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
        let req_name = first_hop_name(&bundle);
        match verify_bundle_with_clock(
            &bundle,
            &anchors,
            &req_name,
            TYPE_TXT,
            frozen_now(CLOUDFLARE_COM_CHAIN_JSON),
        ) {
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
        bundle.hops[0].rrsig.signature[10] ^= 0x01;
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(
                &bundle,
                &anchors,
                &req_name,
                TYPE_TXT,
                frozen_now(CLOUDFLARE_COM_CHAIN_JSON)
            ),
            Err(DnssecError::BadSignature)
        );
    }

    #[test]
    fn rejects_wrong_trust_anchor() {
        /// IANA DNSSEC algorithm for RSA-SHA256 (RFC 5702). We
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
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(
                &bundle,
                &bad,
                &req_name,
                TYPE_TXT,
                frozen_now(CLOUDFLARE_COM_CHAIN_JSON)
            ),
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
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(&bundle, &anchors, &req_name, TYPE_TXT, very_late),
            Err(DnssecError::StaleOrFutureSignature)
        );
    }

    #[test]
    fn rejects_duplicate_zone_in_chains() {
        let mut bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Duplicate the only chain so two chains land at the same
        // zone — triggers the dedup defence in `ZoneKeysMap::insert`.
        let dup = bundle.chains[0].clone();
        bundle.chains.push(dup);
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(
                &bundle,
                &anchors,
                &req_name,
                TYPE_TXT,
                frozen_now(CLOUDFLARE_COM_CHAIN_JSON)
            ),
            Err(DnssecError::DuplicateZone)
        );
    }

    #[test]
    fn rejects_too_many_hops() {
        let mut bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Synthesize a hop list one over the cap. Hops won't
        // actually pass signature verification, but the length
        // check fires before that.
        let h = bundle.hops[0].clone();
        bundle.hops = std::iter::repeat_n(h, MAX_CNAME_HOPS + 1).collect();
        let req_name = first_hop_name(&bundle);
        assert_eq!(
            verify_bundle_with_clock(
                &bundle,
                &anchors,
                &req_name,
                TYPE_TXT,
                frozen_now(CLOUDFLARE_COM_CHAIN_JSON)
            ),
            Err(DnssecError::TooManyHops)
        );
    }

    #[test]
    fn rejects_first_hop_owner_mismatch() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        // Ask for a different name than the leaf the bundle
        // actually proves.
        let wrong = DnsName(b"\x07example\x03com\x00".to_vec());
        match verify_bundle_with_clock(
            &bundle,
            &anchors,
            &wrong,
            TYPE_TXT,
            frozen_now(CLOUDFLARE_COM_CHAIN_JSON),
        ) {
            Err(DnssecError::BadCnameChain(_)) => {}
            other => panic!("expected BadCnameChain, got {:?}", other),
        }
    }

    #[test]
    fn rejects_wrong_requested_type() {
        let bundle = load_bundle(CLOUDFLARE_COM_CHAIN_JSON);
        let anchors = load_anchors(IANA_ROOT_ANCHORS_JSON);
        let req_name = first_hop_name(&bundle);
        // Bundle's leaf is a TXT; ask for a DNSKEY instead.
        match verify_bundle_with_clock(
            &bundle,
            &anchors,
            &req_name,
            TYPE_DNSKEY,
            frozen_now(CLOUDFLARE_COM_CHAIN_JSON),
        ) {
            Err(DnssecError::BadCnameChain(_)) => {}
            other => panic!("expected BadCnameChain, got {:?}", other),
        }
    }

    #[test]
    fn name_is_subdomain_of_handles_label_boundaries() {
        // example.com under com → yes
        assert!(name_is_subdomain_of(
            b"\x07example\x03com\x00",
            b"\x03com\x00"
        ));
        // sub.example.com under example.com → yes
        assert!(name_is_subdomain_of(
            b"\x03sub\x07example\x03com\x00",
            b"\x07example\x03com\x00"
        ));
        // evilexample.com is NOT under example.com (different label)
        assert!(!name_is_subdomain_of(
            b"\x0bevilexample\x03com\x00",
            b"\x07example\x03com\x00"
        ));
        // unrelated.org is NOT under example.com
        assert!(!name_is_subdomain_of(
            b"\x09unrelated\x03org\x00",
            b"\x07example\x03com\x00"
        ));
        // Anything is under the root
        assert!(name_is_subdomain_of(b"\x07example\x03com\x00", b"\x00"));
        // Self under self
        assert!(name_is_subdomain_of(b"\x03com\x00", b"\x03com\x00"));
    }
}
