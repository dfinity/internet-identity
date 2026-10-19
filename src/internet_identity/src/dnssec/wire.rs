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
//!
//! Keeping it in one place stops the email-recovery callers
//! (`prepare_add` for the DMARC leaf, `submit_dkim_leaf` for the DKIM
//! leaf) from each carrying their own copy that could drift apart on
//! a bounds-check fix.

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
/// want (e.g. `EmailChallengeError::EmailVerificationFailed`).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TxtRdataTruncated;

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
}
