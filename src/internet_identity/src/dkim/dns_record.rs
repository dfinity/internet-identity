//! DKIM DNS record parser (RFC 6376 §3.6.2).
//!
//! A DKIM key record is published as a DNS TXT record at
//! `<selector>._domainkey.<domain>`. RFC 6376 §3.6.2.1 defines the
//! tag-value grammar:
//!
//! - `v=DKIM1` — version, optional but if present must be the first tag.
//! - `g=` — granularity (deprecated by RFC 6376 §3.6.1; ignored).
//! - `h=` — supported hash algorithms (`sha256`, default `*`).
//! - `k=` — key type: `rsa` (default) or `ed25519`.
//! - `n=` — human-readable notes; ignored.
//! - `p=` — public key, base64. **Required.** Empty string means revoked.
//! - `s=` — service types (`email` or `*`, default `*`).
//! - `t=` — flags. We honour `y` (testing mode) and `s` (strict identity).
//!
//! Practical wrinkles called out in the PoC PR review and RFC §3.6.2.2:
//!
//! - The TXT RDATA can be split across multiple `<character-string>`s
//!   (each ≤ 255 bytes). We treat the input as the concatenated content;
//!   the gateway/DNSSEC layer is responsible for joining the chunks.
//! - Whitespace is allowed almost anywhere; we collapse it.
//! - Tag names are case-insensitive — the PoC initially mis-handled
//!   `P=` versus `p=`, hence `eq_ignore_ascii_case` here.
//!
//! Public key parsing for RSA uses RFC 3447 SubjectPublicKeyInfo
//! (PKCS#1-DER inside an X.509 wrapper) — the format DKIM publishes
//! per RFC 6376 §3.6.2.1. Ed25519 keys are 32 raw bytes.

use super::types::Algorithm;
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;

/// Parsed DKIM DNS TXT record.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DkimDnsRecord {
    /// `k=` — public-key algorithm. Defaults to RSA per §3.6.2.1.
    pub key_type: KeyType,
    /// `p=` — the raw public key bytes (base64-decoded). Empty when the
    /// key has been revoked (the record is published with `p=`).
    pub public_key: Vec<u8>,
    /// `t=y` — testing mode. The verifier should treat any signature
    /// associated with such a record as inconclusive.
    pub testing: bool,
    /// `t=s` — strict identity. When set, `i=` in the signature must
    /// match `d=` exactly; subdomains are *not* permitted.
    pub strict_auid: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KeyType {
    Rsa,
    Ed25519,
}

impl KeyType {
    pub fn matches_signature_alg(&self, alg: Algorithm) -> bool {
        match (self, alg) {
            (KeyType::Rsa, Algorithm::RsaSha256) => true,
            (KeyType::Ed25519, Algorithm::Ed25519Sha256) => true,
            _ => false,
        }
    }
}

/// Parse a DKIM TXT record (the concatenated RDATA bytes). Returns
/// `Err(human-readable-reason)` on malformed input.
pub fn parse_dkim_txt(record: &str) -> Result<DkimDnsRecord, String> {
    let mut k_type = KeyType::Rsa;
    let mut p_bytes: Option<Vec<u8>> = None;
    let mut testing = false;
    let mut strict_auid = false;

    let tags = split_record_tags(record)?;

    // §3.6.2.1: if `v=` is present it MUST be the first tag and equal
    // to `DKIM1`. Absent is acceptable for backward compatibility.
    if let Some((idx, val)) = tags
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name.eq_ignore_ascii_case("v"))
    {
        if idx != 0 {
            return Err("v= must be the first tag if present".into());
        }
        if !val.1.eq_ignore_ascii_case("DKIM1") {
            return Err(format!("unsupported DKIM version v={}", val.1));
        }
    }

    for (name, raw_value) in &tags {
        match name.to_ascii_lowercase().as_str() {
            "v" => {} // already handled
            "k" => {
                k_type = match raw_value.to_ascii_lowercase().as_str() {
                    "rsa" => KeyType::Rsa,
                    "ed25519" => KeyType::Ed25519,
                    other => return Err(format!("unsupported k={other}")),
                };
            }
            "p" => {
                let stripped: String =
                    raw_value.chars().filter(|c| !c.is_whitespace()).collect();
                let decoded = BASE64
                    .decode(stripped.as_bytes())
                    .map_err(|e| format!("invalid base64 in p=: {e}"))?;
                p_bytes = Some(decoded);
            }
            "t" => {
                // Colon-separated flags per §3.6.2.1. Other flags are
                // ignored.
                for flag in raw_value.split(':') {
                    match flag.trim().to_ascii_lowercase().as_str() {
                        "y" => testing = true,
                        "s" => strict_auid = true,
                        _ => {}
                    }
                }
            }
            // h=, g=, n=, s= — accepted, not enforced for v1.
            "h" | "g" | "n" | "s" => {}
            _ => {
                // §3.6.2.1: unknown tags MUST be ignored.
            }
        }
    }

    let public_key = p_bytes.ok_or("missing required tag p=")?;
    Ok(DkimDnsRecord {
        key_type: k_type,
        public_key,
        testing,
        strict_auid,
    })
}

/// Split a record string into `(name, value)` pairs. Like
/// [`super::parse::parse_dkim_signature`]'s splitter but more permissive
/// in two ways: tag values may contain `=` (split only on the *first*
/// `=`), and we tolerate non-alphanumeric characters in unknown tag
/// names (which §3.6.2.1 says we MUST ignore).
fn split_record_tags(record: &str) -> Result<Vec<(String, String)>, String> {
    let mut out: Vec<(String, String)> = Vec::new();
    for part in record.split(';') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        let (name, value) = match part.split_once('=') {
            Some(p) => p,
            None => return Err(format!("tag without '=' separator: {part}")),
        };
        let name = name.trim().to_string();
        if name.is_empty() {
            return Err("empty tag name".into());
        }
        let value = value.trim().to_string();
        // Duplicate tag check is not strictly required by RFC; later
        // occurrences silently shadow earlier ones in §3.6.2.1, but
        // production records don't repeat tags. Reject duplicates so
        // a cryptic "second p=" can't be smuggled past us.
        if out
            .iter()
            .any(|(existing, _)| existing.eq_ignore_ascii_case(&name))
        {
            return Err(format!("duplicate tag {name}"));
        }
        out.push((name, value));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minimal_rsa_record() {
        let r = "v=DKIM1; k=rsa; p=YWJjZGVm";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.key_type, KeyType::Rsa);
        assert_eq!(parsed.public_key, b"abcdef");
        assert!(!parsed.testing);
        assert!(!parsed.strict_auid);
    }

    #[test]
    fn k_defaults_to_rsa() {
        let r = "v=DKIM1; p=YWJjZGVm";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.key_type, KeyType::Rsa);
    }

    #[test]
    fn parses_ed25519() {
        let r = "v=DKIM1; k=ed25519; p=YWJjZGVm";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.key_type, KeyType::Ed25519);
    }

    #[test]
    fn case_insensitive_p_tag() {
        // The PoC PR review flagged a bug where `P=` was not recognised.
        let r = "v=DKIM1; P=YWJjZGVm";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.public_key, b"abcdef");
    }

    #[test]
    fn t_flag_y_marks_testing() {
        let r = "v=DKIM1; p=YWJjZGVm; t=y";
        let parsed = parse_dkim_txt(r).unwrap();
        assert!(parsed.testing);
        assert!(!parsed.strict_auid);
    }

    #[test]
    fn t_flag_s_marks_strict_auid() {
        let r = "v=DKIM1; p=YWJjZGVm; t=s";
        let parsed = parse_dkim_txt(r).unwrap();
        assert!(parsed.strict_auid);
    }

    #[test]
    fn t_flags_can_combine_with_colon() {
        let r = "v=DKIM1; p=YWJjZGVm; t=y:s";
        let parsed = parse_dkim_txt(r).unwrap();
        assert!(parsed.testing);
        assert!(parsed.strict_auid);
    }

    #[test]
    fn unknown_tags_are_ignored() {
        let r = "v=DKIM1; p=YWJjZGVm; n=human readable note; futurething=ok";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.public_key, b"abcdef");
    }

    #[test]
    fn rejects_missing_p() {
        let r = "v=DKIM1; k=rsa";
        let err = parse_dkim_txt(r).unwrap_err();
        assert!(err.contains("p="));
    }

    #[test]
    fn rejects_v_not_first() {
        let r = "k=rsa; v=DKIM1; p=YWJjZGVm";
        let err = parse_dkim_txt(r).unwrap_err();
        assert!(err.contains("v="));
    }

    #[test]
    fn rejects_unsupported_v() {
        let r = "v=DKIM2; p=YWJjZGVm";
        let err = parse_dkim_txt(r).unwrap_err();
        assert!(err.contains("DKIM2"));
    }

    #[test]
    fn rejects_unsupported_k() {
        let r = "v=DKIM1; k=dsa; p=YWJjZGVm";
        let err = parse_dkim_txt(r).unwrap_err();
        assert!(err.contains("k=dsa"));
    }

    #[test]
    fn rejects_duplicate_p() {
        let r = "v=DKIM1; p=YWJjZGVm; p=Zm9v";
        let err = parse_dkim_txt(r).unwrap_err();
        assert!(err.contains("duplicate"));
    }

    #[test]
    fn whitespace_inside_p_is_stripped() {
        // Real DNS TXT records can split across `<character-string>`s; the
        // gateway concatenates and may leave whitespace at the boundaries.
        let r = "v=DKIM1; p=YWJj ZGVm";
        let parsed = parse_dkim_txt(r).unwrap();
        assert_eq!(parsed.public_key, b"abcdef");
    }

    #[test]
    fn empty_p_means_revoked() {
        let r = "v=DKIM1; p=";
        let parsed = parse_dkim_txt(r).unwrap();
        assert!(parsed.public_key.is_empty());
    }

    #[test]
    fn key_type_matches_signature_alg() {
        assert!(KeyType::Rsa.matches_signature_alg(Algorithm::RsaSha256));
        assert!(KeyType::Ed25519.matches_signature_alg(Algorithm::Ed25519Sha256));
        assert!(!KeyType::Rsa.matches_signature_alg(Algorithm::Ed25519Sha256));
        assert!(!KeyType::Ed25519.matches_signature_alg(Algorithm::RsaSha256));
    }
}
