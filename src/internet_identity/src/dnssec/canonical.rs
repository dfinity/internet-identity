//! Canonical encoding helpers for DNSSEC verification.
//!
//! The verifier checks `signature == sign(signed_data)`, where
//! `signed_data` is constructed per RFC 4034 §3.1.8.1:
//!
//! ```text
//! signed_data = RRSIG_RDATA | RR(1) | RR(2) | ... | RR(n)
//! ```
//!
//! - `RRSIG_RDATA` is the RRSIG's RDATA fields *excluding* the signature
//!   itself, with the signer's name in canonical wire form.
//! - Each `RR(i)` is the canonical form of one resource record:
//!   `owner_name | type | class | original_ttl | rdlength | rdata` where
//!   the owner name is canonical (lowercase labels, no compression) and
//!   the RDATA is canonicalized per the record-type-specific rules in
//!   RFC 4034 §6.2.
//! - The RRs are sorted in canonical octet order (RFC 4034 §6.3) before
//!   concatenation.
//!
//! Our `SignedRRset.rdata` field is already populated with canonical
//! per-record RDATA bytes by the FE (in production) and by
//! `scripts/capture-dnssec-chain.py` (in tests), so this module's job
//! is mainly: (a) lowercase the owner name, (b) sort the RDATAs, and
//! (c) build the right header bytes around them.

use super::types::{Rrsig, SignedRRset};

const CLASS_IN: u16 = 1;

/// Lowercase every byte that is part of a label content (i.e. anything
/// that isn't a length octet starting a label). In wire form an owner
/// name is a sequence of `(len, label_bytes...)` tuples terminated by a
/// zero-length label. Labels can be at most 63 bytes; a length byte with
/// the top two bits set is a compression pointer (RFC 1035 §4.1.4) which
/// must NOT appear in canonical form, so we treat its presence as an
/// invalid input.
pub fn canonicalize_name(wire: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(wire.len());
    let mut i = 0;
    while i < wire.len() {
        let len = wire[i];
        if len & 0xC0 != 0 {
            // Compression pointer — should not be present in a canonical
            // owner name. Push as-is; downstream signature check will
            // fail if this slipped past us, and we'd rather fail closed
            // than silently rewrite.
            out.extend_from_slice(&wire[i..]);
            return out;
        }
        out.push(len);
        if len == 0 {
            return out;
        }
        let label_end = i + 1 + (len as usize);
        if label_end > wire.len() {
            // Truncated input — pass through; signature check will fail.
            out.extend_from_slice(&wire[i + 1..]);
            return out;
        }
        for &b in &wire[i + 1..label_end] {
            out.push(b.to_ascii_lowercase());
        }
        i = label_end;
    }
    out
}

/// Build the RRSIG RDATA *minus* the signature itself, with the signer's
/// name canonicalised. This is the prefix of `signed_data` in RFC 4034
/// §3.1.8.1.
pub fn rrsig_rdata_for_signing(rrsig: &Rrsig) -> Vec<u8> {
    let mut out = Vec::with_capacity(18 + rrsig.signer_name.0.len());
    out.extend_from_slice(&rrsig.type_covered.to_be_bytes());
    out.push(rrsig.algorithm);
    out.push(rrsig.labels);
    out.extend_from_slice(&rrsig.original_ttl.to_be_bytes());
    out.extend_from_slice(&rrsig.expiration.to_be_bytes());
    out.extend_from_slice(&rrsig.inception.to_be_bytes());
    out.extend_from_slice(&rrsig.key_tag.to_be_bytes());
    out.extend_from_slice(&canonicalize_name(&rrsig.signer_name.0));
    out
}

/// Build the canonical-form serialization of one RR (used for both
/// signed-data construction and DS digest input).
fn rr_canonical(
    name_canonical: &[u8],
    rtype: u16,
    original_ttl: u32,
    rdata: &[u8],
) -> Vec<u8> {
    let mut out =
        Vec::with_capacity(name_canonical.len() + 2 + 2 + 4 + 2 + rdata.len());
    out.extend_from_slice(name_canonical);
    out.extend_from_slice(&rtype.to_be_bytes());
    out.extend_from_slice(&CLASS_IN.to_be_bytes());
    out.extend_from_slice(&original_ttl.to_be_bytes());
    out.extend_from_slice(&(rdata.len() as u16).to_be_bytes());
    out.extend_from_slice(rdata);
    out
}

/// Construct the byte sequence that the RRSIG's signature is computed
/// over (RFC 4034 §3.1.8.1 + §6.3).
///
/// The `original_ttl` from the RRSIG is used for every RR in the RRset,
/// not the RRset's currently-cached TTL — important when a resolver has
/// decremented the TTL between the time the zone was signed and the
/// time we're verifying.
pub fn build_signed_data(rrset: &SignedRRset) -> Vec<u8> {
    let canon_name = canonicalize_name(&rrset.name.0);

    // §6.3: sort RRs by their canonical RDATA, treated as left-justified
    // octet streams. Equal prefixes sort by length (shorter first).
    let mut sorted: Vec<&Vec<u8>> = rrset.rdata.iter().collect();
    sorted.sort_by(|a, b| a.as_slice().cmp(b.as_slice()));

    let mut out = rrsig_rdata_for_signing(&rrset.rrsig);
    for rdata in sorted {
        out.extend_from_slice(&rr_canonical(
            &canon_name,
            rrset.rtype,
            rrset.rrsig.original_ttl,
            rdata,
        ));
    }
    out
}

/// Build the input to the DS digest computation per RFC 4034 §5.1.4:
///
/// ```text
/// digest = digest(canonical_owner_name | DNSKEY_RDATA)
/// ```
///
/// where the DNSKEY RDATA is the candidate KSK's RDATA (the public-key
/// resource record we're trying to validate against the DS).
pub fn ds_digest_input(child_zone_name: &[u8], dnskey_rdata: &[u8]) -> Vec<u8> {
    let canon = canonicalize_name(child_zone_name);
    let mut out = Vec::with_capacity(canon.len() + dnskey_rdata.len());
    out.extend_from_slice(&canon);
    out.extend_from_slice(dnskey_rdata);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonicalize_name_lowercases_labels() {
        // "Example.COM." in wire form: 7 'E' 'x' 'a' 'm' 'p' 'l' 'e' 3 'C' 'O' 'M' 0
        let wire = b"\x07Example\x03COM\x00";
        assert_eq!(canonicalize_name(wire), b"\x07example\x03com\x00".to_vec());
    }

    #[test]
    fn canonicalize_name_root_stays_root() {
        assert_eq!(canonicalize_name(b"\x00"), vec![0u8]);
    }

    #[test]
    fn rr_canonical_layout() {
        // owner=root, type=DNSKEY(48), class=IN(1), TTL=3600, RDATA="abc"
        let rr = rr_canonical(b"\x00", 48, 3600, b"abc");
        let mut expected = Vec::new();
        expected.extend_from_slice(b"\x00"); // name
        expected.extend_from_slice(&48u16.to_be_bytes()); // type
        expected.extend_from_slice(&1u16.to_be_bytes()); // class IN
        expected.extend_from_slice(&3600u32.to_be_bytes()); // TTL
        expected.extend_from_slice(&3u16.to_be_bytes()); // RDLENGTH
        expected.extend_from_slice(b"abc"); // RDATA
        assert_eq!(rr, expected);
    }
}
