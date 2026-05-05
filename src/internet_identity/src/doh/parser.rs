//! Minimal DNS wire-format helpers focused on TXT record extraction.
//!
//! We use wire-format DoH (RFC 8484) rather than JSON DoH because:
//! - Every public resolver supports it; JSON DoH is mostly Google +
//!   Cloudflare specific.
//! - Comparing two responses for quorum is bytes-in / bytes-out — no
//!   JSON dialect quirks to normalise.
//! - The parsing surface we need is tiny: header skip, question
//!   skip, walk the answer section, find TXT RDATA.
//!
//! We do **not** implement a full DNS message parser. The verifier
//! never asks for record types it doesn't know how to handle, and a
//! malformed response just surfaces as `ParseError` and gets thrown
//! out by the quorum.

/// DNS query type for TXT records (RFC 1035 §3.2.2).
pub const TYPE_TXT: u16 = 16;

/// DNS class IN — the only class we handle.
pub const CLASS_IN: u16 = 1;

/// DNS message header is fixed at 12 bytes (RFC 1035 §4.1.1).
const HEADER_LEN: usize = 12;

/// Cap on label-pointer chase depth. RFC 1035 doesn't define an
/// upper bound, but a real DNS message can't have meaningful chains
/// longer than ~30; anything past that is almost certainly a
/// hand-crafted compression-pointer loop.
const MAX_LABEL_HOPS: usize = 32;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    Truncated,
    BadFlags,
    BadResponseCode(u8),
    PointerLoop,
    /// The response carried no answer records of the requested type
    /// (NoData) or returned an explicit "not found" RCODE.
    NoAnswer,
}

/// Build a wire-format DNS query for `<name> IN TXT`.
///
/// `name` is given in dotted form (e.g. `selector1._domainkey.gmail.com`,
/// optional trailing dot). The returned bytes are the body to send as
/// `application/dns-message` over DoH.
///
/// Transaction ID is fixed at zero — the same canister-internal value
/// every time. DoH responses echo the ID back; the IC's HTTP-outcall
/// transform function strips header bytes anyway, so a fixed ID makes
/// for cleaner replica-consensus byte equality.
pub fn build_txt_query(name: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(HEADER_LEN + name.len() + 16);
    // Header (12 bytes).
    // ID (2): 0
    out.extend_from_slice(&0u16.to_be_bytes());
    // Flags (2): RD=1 (recursion desired). All other bits 0.
    out.extend_from_slice(&0x0100u16.to_be_bytes());
    // QDCOUNT=1, ANCOUNT=NSCOUNT=ARCOUNT=0
    out.extend_from_slice(&1u16.to_be_bytes());
    out.extend_from_slice(&0u16.to_be_bytes());
    out.extend_from_slice(&0u16.to_be_bytes());
    out.extend_from_slice(&0u16.to_be_bytes());

    // Question section: QNAME (length-prefixed labels + 0), QTYPE, QCLASS.
    encode_name(name, &mut out);
    out.extend_from_slice(&TYPE_TXT.to_be_bytes());
    out.extend_from_slice(&CLASS_IN.to_be_bytes());
    out
}

/// Parse a DoH response and return the concatenated TXT RDATA bytes
/// for the first matching answer record. The returned bytes are the
/// raw `<character-string>` segments concatenated *without* the
/// length octets (RFC 1035 §3.3.14): a typical DKIM TXT comes back
/// like `b"v=DKIM1; k=rsa; p=..."` ready to feed straight into
/// [`crate::dkim::dns_record::parse_dkim_txt`].
///
/// If the response carries multiple TXT records for the same name
/// (rare in practice; selectors usually publish one), we return the
/// first one only — the verifier doesn't merge them.
pub fn parse_txt_response(bytes: &[u8]) -> Result<Vec<u8>, ParseError> {
    if bytes.len() < HEADER_LEN {
        return Err(ParseError::Truncated);
    }
    let flags = u16::from_be_bytes([bytes[2], bytes[3]]);
    // QR=1 must be set (this is a response).
    if flags & 0x8000 == 0 {
        return Err(ParseError::BadFlags);
    }
    let rcode = (flags & 0x000f) as u8;
    if rcode != 0 {
        return Err(ParseError::BadResponseCode(rcode));
    }
    let qdcount = u16::from_be_bytes([bytes[4], bytes[5]]);
    let ancount = u16::from_be_bytes([bytes[6], bytes[7]]);

    // Skip the question section.
    let mut pos = HEADER_LEN;
    for _ in 0..qdcount {
        pos = skip_name(bytes, pos)?;
        // QTYPE (2) + QCLASS (2)
        if pos + 4 > bytes.len() {
            return Err(ParseError::Truncated);
        }
        pos += 4;
    }

    // Walk the answer section, find the first TXT record.
    for _ in 0..ancount {
        pos = skip_name(bytes, pos)?;
        if pos + 10 > bytes.len() {
            return Err(ParseError::Truncated);
        }
        let rtype = u16::from_be_bytes([bytes[pos], bytes[pos + 1]]);
        // class (2) + ttl (4)
        let rdlength =
            u16::from_be_bytes([bytes[pos + 8], bytes[pos + 9]]) as usize;
        pos += 10;
        if pos + rdlength > bytes.len() {
            return Err(ParseError::Truncated);
        }
        if rtype == TYPE_TXT {
            return parse_txt_rdata(&bytes[pos..pos + rdlength]);
        }
        pos += rdlength;
    }

    Err(ParseError::NoAnswer)
}

/// Decode a TXT RDATA blob: a sequence of `<character-string>`s, each
/// length-prefixed by one byte. Returns the concatenated string bodies
/// (length octets stripped). RFC 1035 §3.3.14 / RFC 6376 §3.6.2.2.
fn parse_txt_rdata(rdata: &[u8]) -> Result<Vec<u8>, ParseError> {
    let mut out = Vec::with_capacity(rdata.len());
    let mut i = 0;
    while i < rdata.len() {
        let len = rdata[i] as usize;
        i += 1;
        if i + len > rdata.len() {
            return Err(ParseError::Truncated);
        }
        out.extend_from_slice(&rdata[i..i + len]);
        i += len;
    }
    Ok(out)
}

/// Skip a (possibly-compressed) DNS name and return the position
/// immediately after it. Compression pointers (top two bits of the
/// length byte = 11) jump elsewhere in the message; we never actually
/// follow the pointer here because `skip_name` only needs to know
/// where the *current* name ends — for a compressed name, that's two
/// bytes after the start.
fn skip_name(bytes: &[u8], mut pos: usize) -> Result<usize, ParseError> {
    let mut hops = 0;
    loop {
        if pos >= bytes.len() {
            return Err(ParseError::Truncated);
        }
        let len = bytes[pos];
        if len == 0 {
            // End of name.
            return Ok(pos + 1);
        }
        if len & 0xC0 == 0xC0 {
            // Compression pointer — 2 bytes total, regardless of where
            // it points.
            if pos + 2 > bytes.len() {
                return Err(ParseError::Truncated);
            }
            return Ok(pos + 2);
        }
        if len & 0xC0 != 0 {
            // Reserved bit pattern (10 / 01) — RFC 1035 §4.1.4.
            return Err(ParseError::BadFlags);
        }
        let next = pos + 1 + len as usize;
        if next > bytes.len() {
            return Err(ParseError::Truncated);
        }
        pos = next;
        hops += 1;
        if hops > MAX_LABEL_HOPS {
            return Err(ParseError::PointerLoop);
        }
    }
}

/// Encode a dotted name as a sequence of length-prefixed labels
/// terminated by a zero-length label.
fn encode_name(name: &str, out: &mut Vec<u8>) {
    for label in name.trim_end_matches('.').split('.') {
        if label.is_empty() {
            continue;
        }
        let bytes = label.as_bytes();
        let len = bytes.len().min(63) as u8;
        out.push(len);
        out.extend_from_slice(&bytes[..len as usize]);
    }
    out.push(0);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_query_for_simple_name() {
        let bytes = build_txt_query("example.com");
        // Header: ID=0, flags=0x0100, QDCOUNT=1, others=0
        assert_eq!(&bytes[0..2], &[0, 0]);
        assert_eq!(&bytes[2..4], &[0x01, 0x00]);
        assert_eq!(&bytes[4..6], &[0, 1]);
        // Question: 7example3com0 + type 16 + class 1
        assert_eq!(
            &bytes[12..],
            b"\x07example\x03com\x00\x00\x10\x00\x01"
        );
    }

    #[test]
    fn build_query_handles_trailing_dot() {
        let with_dot = build_txt_query("example.com.");
        let without_dot = build_txt_query("example.com");
        assert_eq!(with_dot, without_dot);
    }

    #[test]
    fn build_query_handles_long_name() {
        let bytes = build_txt_query("selector1._domainkey.gmail.com");
        // Header (12) + name (32 = 1+9 + 1+10 + 1+5 + 1+3 + 1) + qtype(2) + qclass(2)
        assert_eq!(bytes.len(), 12 + 1 + 9 + 1 + 10 + 1 + 5 + 1 + 3 + 1 + 4);
    }

    /// Hand-built minimal DoH response carrying one TXT answer.
    /// Header: ID=0, flags=0x8180 (QR + RD + RA, RCODE=0), QDCOUNT=1,
    /// ANCOUNT=1.
    /// Question: example.com TXT IN
    /// Answer: example.com TXT IN TTL=300, RDLENGTH=12, RDATA="\x0bhello world"
    fn fake_response(rdata: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        // Header
        out.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]);
        // Question
        out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
        // Answer name (use a compression pointer to offset 12)
        out.extend_from_slice(&[0xC0, 0x0C]);
        // type=TXT, class=IN, ttl=300
        out.extend_from_slice(&TYPE_TXT.to_be_bytes());
        out.extend_from_slice(&CLASS_IN.to_be_bytes());
        out.extend_from_slice(&300u32.to_be_bytes());
        // RDLENGTH and RDATA
        out.extend_from_slice(&(rdata.len() as u16).to_be_bytes());
        out.extend_from_slice(rdata);
        out
    }

    #[test]
    fn parse_simple_txt() {
        let resp = fake_response(b"\x0bhello world");
        let txt = parse_txt_response(&resp).unwrap();
        assert_eq!(txt, b"hello world");
    }

    #[test]
    fn parse_concatenates_multiple_character_strings() {
        // A real DKIM TXT often spans multiple <character-string>s.
        // RDATA: "\x05first\x06second"
        let resp = fake_response(b"\x05first\x06second");
        let txt = parse_txt_response(&resp).unwrap();
        assert_eq!(txt, b"firstsecond");
    }

    #[test]
    fn rejects_when_qr_bit_unset() {
        let mut resp = fake_response(b"\x05hello");
        // Flip QR bit off
        resp[2] = 0x01;
        assert_eq!(parse_txt_response(&resp), Err(ParseError::BadFlags));
    }

    #[test]
    fn rejects_nxdomain() {
        let mut resp = fake_response(b"\x05hello");
        // Flip RCODE to NXDOMAIN (3).
        resp[3] = 0x83;
        assert_eq!(
            parse_txt_response(&resp),
            Err(ParseError::BadResponseCode(3))
        );
    }

    #[test]
    fn rejects_truncated_header() {
        let resp = vec![0u8; 5];
        assert_eq!(parse_txt_response(&resp), Err(ParseError::Truncated));
    }

    #[test]
    fn rejects_when_no_answer_section() {
        // Build a response with ANCOUNT=0.
        let mut out = Vec::new();
        out.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 0, 0, 0, 0, 0]);
        out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
        assert_eq!(parse_txt_response(&out), Err(ParseError::NoAnswer));
    }
}
