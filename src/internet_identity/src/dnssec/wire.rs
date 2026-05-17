//! Wire-format constants and helpers shared across DNS-touching
//! modules.
//!
//! The constants are RDATA layout values used by both `verify.rs`
//! (chain walk / anchor matching) and `signature.rs` (DS digest
//! matching / DNSKEY public-key extraction).
//!
//! The helpers are RFC 1035 §3 wire-format primitives that the
//! DNSSEC-verified email-recovery path needs:
//!
//! - [`parse_txt_rdata`] flattens TXT RDATA byte chunks (character-
//!   strings each prefixed by one length octet) into the concatenated
//!   bytes the DKIM / DMARC verifier expects.
//! - [`encode_dns_name_lowercase`] turns a dotted ASCII name into wire
//!   format with every label lower-cased — used to build the canonical
//!   FQDN that a verified RRset's owner name must round-trip to.
//! - [`decode_dns_name_lowercase`] is the inverse, used to read a
//!   verified leaf's owner name back out for comparison.
//!
//! Keeping these in one place stops the email-recovery callers
//! (`prepare_add` for the DMARC leaf, `submit_dkim_leaf` for the DKIM
//! leaf) from each carrying their own copy that could drift apart on
//! a bounds-check or canonicalisation fix.

/// Length of the fixed DNSKEY RDATA header (RFC 4034 §2.1):
///
/// ```text
/// Flags (u16) | Protocol (u8) | Algorithm (u8) | Public Key (variable)
/// ```
///
/// Used to bounds-check RDATA before reading the algorithm byte or
/// slicing off the public-key sub-field.
pub const DNSKEY_RDATA_HEADER_LEN: usize = 4;

/// Length of the fixed DS RDATA header (RFC 4034 §5.1):
///
/// ```text
/// Key Tag (u16) | Algorithm (u8) | Digest Type (u8) | Digest (variable)
/// ```
pub const DS_RDATA_HEADER_LEN: usize = 4;

/// DS Digest Type 2 = SHA-256 (RFC 4509). The only digest type we
/// accept — SHA-1 (Digest Type 1) is MUST NOT for new deployments
/// per RFC 8624 §3.3, and the IANA root anchors are SHA-256 anyway.
pub const DS_DIGEST_TYPE_SHA256: u8 = 2;

/// A TXT RDATA chunk's length octet ran past the end of the chunk —
/// see [`parse_txt_rdata`]. Marker type rather than a stringly-typed
/// error so callers map it to whichever module-specific error they
/// want (e.g. `EmailRecoveryError::EmailVerificationFailed`).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TxtRdataTruncated;

/// The dotted DNS name handed to [`encode_dns_name_lowercase`] could
/// not be expressed in wire format because some label was >63 bytes
/// or the total wire length exceeded 255 bytes (RFC 1035 §2.3.4 /
/// §3.1). Marker type, like [`TxtRdataTruncated`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InvalidDnsName;

/// Concatenate one or more TXT character-strings into the bytes the
/// DKIM / DMARC verifier expects.
///
/// `rdata` is `&[Vec<u8>]` because a TXT RRset can carry multiple
/// records — but for DKIM/DMARC there's exactly one record made of
/// multiple chunks. Each chunk is one or more `<character-string>`s
/// (RFC 1035 §3.3.14), each prefixed by a one-octet length. We strip
/// the length octets and emit only the body bytes.
///
/// Returns [`TxtRdataTruncated`] if any chunk's length octet runs
/// past the end of the chunk — the canonical fail-closed response,
/// since silently truncating would let a malformed RDATA verify as a
/// different (shorter) TXT.
pub(crate) fn parse_txt_rdata(rdata: &[Vec<u8>]) -> Result<Vec<u8>, TxtRdataTruncated> {
    let mut out = Vec::new();
    for rec in rdata {
        let mut i = 0;
        while i < rec.len() {
            let len = rec[i] as usize;
            i += 1;
            if i + len > rec.len() {
                return Err(TxtRdataTruncated);
            }
            out.extend_from_slice(&rec[i..i + len]);
            i += len;
        }
    }
    Ok(out)
}

/// Encode a dotted ASCII DNS name (with or without a trailing dot)
/// into wire format: a sequence of length-prefixed labels terminated
/// by a zero-length root label. Labels are lowercased on the way in.
///
/// Returns [`InvalidDnsName`] when the input violates RFC 1035 §3.1
/// limits — a single label longer than 63 bytes, or an encoded name
/// longer than 255 bytes total including the root terminator.
/// Silently truncating an oversized label would mutate the name into
/// a different DNS name, which is a correctness bug at best and a
/// cache-poisoning vector at worst.
pub(crate) fn encode_dns_name_lowercase(dotted: &str) -> Result<Vec<u8>, InvalidDnsName> {
    let trimmed = dotted.strip_suffix('.').unwrap_or(dotted);
    let mut out = Vec::with_capacity(trimmed.len() + 2);
    for label in trimmed.split('.') {
        if label.is_empty() {
            continue;
        }
        let bytes = label.as_bytes();
        if bytes.len() > 63 {
            return Err(InvalidDnsName);
        }
        out.push(bytes.len() as u8);
        for &b in bytes {
            out.push(b.to_ascii_lowercase());
        }
    }
    out.push(0);
    if out.len() > 255 {
        return Err(InvalidDnsName);
    }
    Ok(out)
}

/// Decode a wire-format DNS name (length-prefixed labels) into a
/// dotted ASCII-lowercased string with a trailing dot. The root
/// terminator (`\x00`) is *not* an extra label — the trailing dot
/// comes from the final non-root label; reaching the terminator just
/// stops the walk.
///
/// Truncated input is treated as fail-open: whatever labels we
/// already consumed get returned. The caller compares against an
/// expected name, so truncation always surfaces as a mismatch.
pub(crate) fn decode_dns_name_lowercase(wire: &[u8]) -> String {
    let mut out = String::new();
    let mut i = 0;
    while i < wire.len() {
        let len = wire[i] as usize;
        i += 1;
        if len == 0 {
            break;
        }
        if i + len > wire.len() {
            return out;
        }
        for &b in &wire[i..i + len] {
            out.push(b.to_ascii_lowercase() as char);
        }
        out.push('.');
        i += len;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_txt_rdata_single_chunk_round_trips() {
        let rdata = vec![b"\x0bhello world".to_vec()];
        assert_eq!(parse_txt_rdata(&rdata).unwrap(), b"hello world");
    }

    #[test]
    fn parse_txt_rdata_concatenates_chunks() {
        // A real DKIM TXT often comes back as multiple short strings.
        let rdata = vec![b"\x05first\x06second".to_vec()];
        assert_eq!(parse_txt_rdata(&rdata).unwrap(), b"firstsecond");
    }

    #[test]
    fn parse_txt_rdata_handles_multiple_records() {
        let rdata = vec![b"\x03foo".to_vec(), b"\x03bar".to_vec()];
        assert_eq!(parse_txt_rdata(&rdata).unwrap(), b"foobar");
    }

    #[test]
    fn parse_txt_rdata_rejects_truncated_chunk() {
        // Length octet says 10 bytes follow, only 3 do.
        let rdata = vec![b"\x0aabc".to_vec()];
        assert_eq!(parse_txt_rdata(&rdata).unwrap_err(), TxtRdataTruncated);
    }

    #[test]
    fn encode_dns_name_simple_round_trip() {
        let wire = encode_dns_name_lowercase("example.com").unwrap();
        assert_eq!(wire, b"\x07example\x03com\x00");
    }

    #[test]
    fn encode_dns_name_lowercases_input() {
        let wire = encode_dns_name_lowercase("EXAMPLE.com").unwrap();
        assert_eq!(wire, b"\x07example\x03com\x00");
    }

    #[test]
    fn encode_dns_name_handles_trailing_dot() {
        // RFC 1035 allows an optional trailing dot to mark the root.
        let with_dot = encode_dns_name_lowercase("example.com.").unwrap();
        let without_dot = encode_dns_name_lowercase("example.com").unwrap();
        assert_eq!(with_dot, without_dot);
    }

    #[test]
    fn encode_dns_name_rejects_oversized_label() {
        let label = "a".repeat(64);
        let name = format!("{label}.example.com");
        assert_eq!(
            encode_dns_name_lowercase(&name).unwrap_err(),
            InvalidDnsName
        );
    }

    #[test]
    fn encode_dns_name_rejects_total_length_overflow() {
        // 5 × 50-char labels + length octets + terminator → wire > 255.
        let label = "a".repeat(50);
        let name = (0..5).map(|_| label.as_str()).collect::<Vec<_>>().join(".");
        assert_eq!(
            encode_dns_name_lowercase(&name).unwrap_err(),
            InvalidDnsName
        );
    }

    #[test]
    fn decode_dns_name_returns_dotted_lowercase() {
        let name = decode_dns_name_lowercase(b"\x07Example\x03COM\x00");
        assert_eq!(name, "example.com.");
    }

    #[test]
    fn decode_dns_name_fails_open_on_truncation() {
        // Label says 9 bytes follow, only "example" (7) is there.
        let name = decode_dns_name_lowercase(b"\x09example");
        // We returned whatever we'd consumed — empty in this case (the
        // truncation hit before any label completed).
        assert_eq!(name, "");
    }

    #[test]
    fn encode_then_decode_round_trips() {
        let wire = encode_dns_name_lowercase("Selector1._domainkey.GMAIL.com").unwrap();
        assert_eq!(
            decode_dns_name_lowercase(&wire),
            "selector1._domainkey.gmail.com."
        );
    }
}
