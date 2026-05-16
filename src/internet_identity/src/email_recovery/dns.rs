//! Shared DNS wire-format parsing for the email-recovery DNSSEC path.
//!
//! `prepare_add` (DMARC leaf) and `submit_dkim_leaf` (DKIM leaf) both
//! consume verified RRsets from `crate::dnssec::verify_hops_with_clock`
//! and need the same two operations: flatten the TXT `RDATA` byte
//! chunks and lowercase the wire-format leaf name. Keeping both
//! helpers in one place prevents the two copies from drifting (e.g.
//! one getting a truncation-bounds fix the other doesn't).

use internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryError;

/// Concatenate one or more TXT character-strings (each prefixed by a
/// length octet) into the bytes the DKIM / DMARC verifier expects.
/// `rdata` is `Vec<Vec<u8>>` because TXT RRsets can carry multiple
/// records — but for DKIM/DMARC there's exactly one record made of
/// multiple chunks.
pub(super) fn parse_txt_rdata(rdata: &[Vec<u8>]) -> Result<Vec<u8>, EmailRecoveryError> {
    let mut txt_bytes = Vec::new();
    for rec in rdata {
        let mut i = 0;
        while i < rec.len() {
            let len = rec[i] as usize;
            i += 1;
            if i + len > rec.len() {
                return Err(EmailRecoveryError::EmailVerificationFailed(
                    "DNSSEC TXT RDATA truncated".into(),
                ));
            }
            txt_bytes.extend_from_slice(&rec[i..i + len]);
            i += len;
        }
    }
    Ok(txt_bytes)
}

/// Decode a wire-format DNS name (length-prefixed labels) into a
/// dotted ASCII-lowercased string with a trailing dot. The root
/// terminator (`\x00`) is *not* an extra label — the trailing dot
/// comes from the final non-root label; reaching the terminator just
/// stops the walk.
pub(super) fn decode_dns_name_lowercase(wire: &[u8]) -> String {
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
