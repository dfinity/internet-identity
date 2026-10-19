//! Canonical-form encoding for DNSSEC verification.
//!
//! The verifier checks `signature == sign(signed_data)`, where
//! `signed_data` is constructed per RFC 4034 §3.1.8.1:
//!
//! ```text
//! signed_data = RRSIG_RDATA | RR(1) | RR(2) | ... | RR(n)
//! ```
//!
//! - `RRSIG_RDATA` is the RRSIG's RDATA fields *excluding* the
//!   signature itself, with the signer's name in canonical wire form.
//! - Each `RR(i)` is the canonical form of one resource record
//!   (RFC 4034 §6.2): canonical owner name, fixed-width header
//!   (`TYPE | CLASS | original_TTL | RDLENGTH`), and the per-type
//!   canonicalised RDATA.
//! - The RRs are sorted into canonical RDATA order (RFC 4034 §6.3)
//!   before concatenation.
//!
//! Our `SignedRRset.rdata` field is already populated with the
//! canonical per-record RDATA bytes — in production by the front-end
//! (which uses dnspython's `to_wire(canonicalize=True)` semantics),
//! in tests by the captured fixtures in `test_vectors/dnssec/` — and
//! every owner and signer name is a [`DnsName`], so this module's
//! responsibilities are:
//!
//! 1. sort the RDATAs (RFC 4034 §6.3),
//! 2. assemble the byte sequence that the RRSIG signs (RFC 4034
//!    §3.1.8.1), and
//! 3. assemble the byte sequence that the DS digest is computed over
//!    (RFC 4034 §5.1.4).

use super::types::{DnsName, Rrsig, SignedRRset};

/// Serialise the RRSIG's fixed RDATA fields followed by the canonical
/// form of the signer's name. This is the `RRSIG_RDATA` prefix of
/// `signed_data` defined in RFC 4034 §3.1.8.1 — i.e. the RRSIG RDATA
/// with the trailing signature field omitted.
///
/// RFC 4034 §3.1 RRSIG RDATA layout:
///
/// ```text
/// Type Covered (u16) | Algorithm (u8) | Labels (u8) | Original TTL (u32)
///   | Signature Expiration (u32) | Signature Inception (u32) | Key Tag (u16)
///   | Signer's Name (variable)   | Signature (variable, NOT included here)
/// ```
pub fn rrsig_rdata_for_signing(rrsig: &Rrsig) -> Vec<u8> {
    /// Sum of the eight fixed-width fields preceding the variable
    /// signer-name field in RRSIG RDATA (RFC 4034 §3.1):
    /// 2 + 1 + 1 + 4 + 4 + 4 + 2 = 18.
    const FIXED_FIELDS_LEN: usize = 18;

    let canonical_signer = rrsig.signer_name.as_wire();
    let mut out = Vec::with_capacity(FIXED_FIELDS_LEN + canonical_signer.len());
    out.extend_from_slice(&rrsig.type_covered.to_be_bytes());
    out.push(rrsig.algorithm);
    out.push(rrsig.labels);
    out.extend_from_slice(&rrsig.original_ttl.to_be_bytes());
    out.extend_from_slice(&rrsig.expiration.to_be_bytes());
    out.extend_from_slice(&rrsig.inception.to_be_bytes());
    out.extend_from_slice(&rrsig.key_tag.to_be_bytes());
    out.extend_from_slice(canonical_signer);
    debug_assert_eq!(
        out.len(),
        FIXED_FIELDS_LEN + canonical_signer.len(),
        "FIXED_FIELDS_LEN out of sync with rrsig_rdata_for_signing layout",
    );
    out
}

/// Serialise one resource record in canonical form (RFC 4034 §6.2):
///
/// ```text
/// owner_name | TYPE (u16) | CLASS (u16) | original_TTL (u32) | RDLENGTH (u16) | RDATA
/// ```
///
/// `rdata` is expected to already be in canonical form, from
/// whatever produced the `SignedRRset` (production: front-end;
/// tests: captured fixtures). The `original_ttl` from the RRSIG is
/// used instead of the RRset's currently-cached TTL, because
/// RFC 4034 §6.2 step 3 requires the TTL used in canonical form to
/// be the one stamped into the RRSIG at signing time (a resolver may
/// have decremented the cached TTL between then and now).
fn rr_canonical(name: &DnsName, rtype: u16, original_ttl: u32, rdata: &[u8]) -> Vec<u8> {
    /// Sum of the four fixed-width fields between the owner name and
    /// the RDATA in canonical form (TYPE | CLASS | TTL | RDLENGTH):
    /// 2 + 2 + 4 + 2 = 10 (RFC 1035 §3.2.1 + RFC 4034 §6.2).
    const FIXED_HEADER_LEN: usize = 10;
    /// CLASS IN (Internet) per RFC 1035 §3.2.4. DNSSEC signatures
    /// cover only records of a specific class (RFC 4034 §6.2), and
    /// every record we look at is class IN.
    const CLASS_IN: u16 = 1;

    let name_canonical = name.as_wire();
    let mut out = Vec::with_capacity(name_canonical.len() + FIXED_HEADER_LEN + rdata.len());
    out.extend_from_slice(name_canonical);
    out.extend_from_slice(&rtype.to_be_bytes());
    out.extend_from_slice(&CLASS_IN.to_be_bytes());
    out.extend_from_slice(&original_ttl.to_be_bytes());
    let rdlength = u16::try_from(rdata.len()).expect("RDATA longer than 65535 bytes");
    out.extend_from_slice(&rdlength.to_be_bytes());
    out.extend_from_slice(rdata);
    debug_assert_eq!(
        out.len(),
        name_canonical.len() + FIXED_HEADER_LEN + rdata.len(),
        "FIXED_HEADER_LEN out of sync with rr_canonical layout",
    );
    out
}

/// Assemble the byte sequence that an RRSIG's signature is computed
/// over (RFC 4034 §3.1.8.1):
///
/// ```text
/// signed_data = RRSIG_RDATA | RR(1) | RR(2) | ... | RR(n)
/// ```
///
/// where the `RR(i)` are sorted into canonical RDATA order
/// (RFC 4034 §6.3 — left-justified octet comparison, shorter sorts
/// before longer when one is a prefix of the other).
pub fn build_signed_data(rrset: &SignedRRset) -> Vec<u8> {
    // RFC 4034 §6.3 canonical RR ordering. Rust's `Vec<u8>` cmp is
    // already left-justified octet comparison with the right
    // tie-breaker (shorter before longer), so a direct sort matches
    // the RFC exactly.
    let mut canonical_rdatas: Vec<&Vec<u8>> = rrset.rdata.iter().collect();
    canonical_rdatas.sort_by(|a, b| a.as_slice().cmp(b.as_slice()));

    let mut out = rrsig_rdata_for_signing(&rrset.rrsig);
    for rdata in canonical_rdatas {
        out.extend_from_slice(&rr_canonical(
            &rrset.name,
            rrset.rtype,
            rrset.rrsig.original_ttl,
            rdata,
        ));
    }
    out
}

/// Assemble the byte sequence that a DS RR's digest is computed over
/// (RFC 4034 §5.1.4):
///
/// ```text
/// digest = digest_algorithm( canonical_owner_name | DNSKEY_RDATA )
/// ```
///
/// `child_zone_name` is the owner of the child zone's DNSKEY RRset
/// (which is also the owner of the DS RRset in the parent zone, by
/// definition). `dnskey_rdata` is the entire DNSKEY RDATA, i.e.
/// `Flags | Protocol | Algorithm | Public Key`.
pub fn ds_digest_input(child_zone_name: &DnsName, dnskey_rdata: &[u8]) -> Vec<u8> {
    let canonical_name = child_zone_name.as_wire();
    let mut out = Vec::with_capacity(canonical_name.len() + dnskey_rdata.len());
    out.extend_from_slice(canonical_name);
    out.extend_from_slice(dnskey_rdata);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rr_canonical_layout_matches_rfc_4034() {
        // owner=root, type=DNSKEY(48), TTL=3600, RDATA="abc".
        let root = DnsName::from_wire(b"\x00").unwrap();
        let rr = rr_canonical(&root, 48, 3600, b"abc");
        let mut expected = Vec::new();
        expected.extend_from_slice(b"\x00"); // canonical owner name
        expected.extend_from_slice(&48u16.to_be_bytes()); // TYPE = DNSKEY
        expected.extend_from_slice(&1u16.to_be_bytes()); // CLASS = IN
        expected.extend_from_slice(&3600u32.to_be_bytes()); // original TTL
        expected.extend_from_slice(&3u16.to_be_bytes()); // RDLENGTH
        expected.extend_from_slice(b"abc"); // RDATA
        assert_eq!(rr, expected);
    }
}
