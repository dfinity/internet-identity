//! `DKIM-Signature` header parser (RFC 6376 §3.5).
//!
//! The header value is a list of `tag=value` pairs separated by `;`. The
//! grammar is more permissive than a naïve `split(';')` / `split('=')`
//! suggests — see §3.2 ("Tag=Value Lists"):
//!
//! - Tag names are 1–8 alphanumerics, case-insensitive.
//! - Whitespace (FWS) is allowed *before and after* every tag name and
//!   value, including across line folds (CRLF + WSP).
//! - Tag values are everything between `=` and the next `;` (or end),
//!   *minus* surrounding FWS. The value itself can contain `=` and other
//!   delimiters as long as they're not the structural one.
//! - The same tag MUST NOT appear twice; we reject if it does.
//! - Trailing `;` is allowed (and common).
//!
//! The PoC PR's review specifically called out that a naïve splitter
//! would mis-parse a header where the `b=` (signature) value happens to
//! contain a literal `b=` substring inside its base64 — that's why we
//! parse structurally first (split on tags) and only then look up by
//! name.

use super::types::{Algorithm, BodyCanon, HeaderCanon};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DkimSignature {
    /// `v=` (must be `1`).
    pub version: u32,
    /// `a=` (only `rsa-sha256` and `ed25519-sha256` accepted).
    pub algorithm: Algorithm,
    /// `d=` — the signing domain.
    pub d: String,
    /// `s=` — the selector.
    pub s: String,
    /// `h=` — names of the signed header fields, in signing order. Stored
    /// lowercased; selection from the message uses case-insensitive name
    /// match per RFC 6376 §3.5.
    pub h: Vec<String>,
    /// `b=` — the signature itself, decoded from base64.
    pub b: Vec<u8>,
    /// `bh=` — the body hash, decoded from base64.
    pub bh: Vec<u8>,
    /// `c=` — header / body canonicalisation, defaults to `simple/simple`
    /// per RFC 6376 §3.5 (we map this to (Simple, Simple); the verifier
    /// then rejects on the header-side `Simple` per design §5.2).
    pub c_header: HeaderCanon,
    pub c_body: BodyCanon,
    /// `l=` — body length to hash, in bytes. `None` means "all of it".
    pub l: Option<u64>,
    /// `t=` — signature timestamp (Unix seconds). Optional.
    pub t: Option<u64>,
    /// `x=` — signature expiration (Unix seconds). Optional.
    pub x: Option<u64>,
    /// `i=` — Agent or User Identifier on whose behalf the message was
    /// signed. Defaults to `@d=` per §3.5. Always lowercased.
    pub i: String,
}

/// Parse a `DKIM-Signature` header value into structured tags.
///
/// `value` is the content of the header *after* the `:` separator —
/// gateway-supplied, possibly containing folded whitespace.
pub fn parse_dkim_signature(value: &str) -> Result<DkimSignature, String> {
    let tags = split_tag_list(value)?;

    let get = |name: &str| -> Option<&str> {
        tags.iter()
            .find(|(k, _)| k == name)
            .map(|(_, v)| v.as_str())
    };
    let require = |name: &str| -> Result<&str, String> {
        get(name).ok_or_else(|| format!("missing required tag {name}"))
    };

    // v= MUST be 1.
    let version: u32 = require("v")?
        .parse()
        .map_err(|_| "v= is not a number".to_string())?;
    if version != 1 {
        return Err(format!("unsupported DKIM version v={version}"));
    }

    // a= must be one of our supported algorithms.
    let algorithm = match require("a")?.to_ascii_lowercase().as_str() {
        "rsa-sha256" => Algorithm::RsaSha256,
        "ed25519-sha256" => Algorithm::Ed25519Sha256,
        other => return Err(format!("unsupported a={other}")),
    };

    let d = require("d")?.to_ascii_lowercase();
    let s = require("s")?.to_ascii_lowercase();

    // h= is a colon-separated list of header field names, case-
    // insensitive (RFC 6376 §3.5). Folded whitespace inside it is
    // stripped by the tag-list splitter; we still strip per-element
    // whitespace because the splitter only handles structural folding.
    let h: Vec<String> = require("h")?
        .split(':')
        .map(|name| name.trim().to_ascii_lowercase())
        .filter(|name| !name.is_empty())
        .collect();
    if h.is_empty() {
        return Err("h= must list at least one signed header".to_string());
    }

    // Base64 values may contain whitespace (folding), strip it before
    // decoding.
    let b = decode_b64_with_whitespace(require("b")?, "b")?;
    let bh = decode_b64_with_whitespace(require("bh")?, "bh")?;

    // c= defaults to (Simple, Simple) per §3.5.
    let (c_header, c_body) = match get("c") {
        None => (HeaderCanon::Simple, BodyCanon::Simple),
        Some(v) => parse_canon(v)?,
    };

    let l = match get("l") {
        None => None,
        Some(v) => Some(
            v.trim()
                .parse::<u64>()
                .map_err(|_| format!("l= is not a number: {v}"))?,
        ),
    };

    let t = match get("t") {
        None => None,
        Some(v) => Some(
            v.trim()
                .parse::<u64>()
                .map_err(|_| format!("t= is not a number: {v}"))?,
        ),
    };

    let x = match get("x") {
        None => None,
        Some(v) => Some(
            v.trim()
                .parse::<u64>()
                .map_err(|_| format!("x= is not a number: {v}"))?,
        ),
    };

    // i= defaults to "@<d=>" per §3.5.
    let i = match get("i") {
        None => format!("@{d}"),
        Some(v) => v.trim().to_ascii_lowercase(),
    };

    Ok(DkimSignature {
        version,
        algorithm,
        d,
        s,
        h,
        b,
        bh,
        c_header,
        c_body,
        l,
        t,
        x,
        i,
    })
}

fn parse_canon(v: &str) -> Result<(HeaderCanon, BodyCanon), String> {
    let v = v.trim();
    let (h_part, b_part) = match v.split_once('/') {
        Some((h, b)) => (h.trim(), b.trim()),
        None => (v, "simple"),
    };
    let h = match h_part.to_ascii_lowercase().as_str() {
        "simple" => HeaderCanon::Simple,
        "relaxed" => HeaderCanon::Relaxed,
        other => return Err(format!("unknown header canon: {other}")),
    };
    let b = match b_part.to_ascii_lowercase().as_str() {
        "simple" => BodyCanon::Simple,
        "relaxed" => BodyCanon::Relaxed,
        other => return Err(format!("unknown body canon: {other}")),
    };
    Ok((h, b))
}

fn decode_b64_with_whitespace(src: &str, label: &str) -> Result<Vec<u8>, String> {
    let stripped: String = src.chars().filter(|c| !c.is_whitespace()).collect();
    BASE64
        .decode(stripped.as_bytes())
        .map_err(|e| format!("invalid base64 in {label}=: {e}"))
}

/// Split a tag-value list into `(name_lowercased, value)` pairs.
///
/// Per RFC 6376 §3.2: the list is `;`-separated, each element is
/// `<name>=<value>` with FWS allowed around the `=` and around each
/// element. Names are 1–8 alphanumerics, lowercase. The FIRST `=` in
/// each element separates the name from the value — subsequent `=`s
/// belong to the value.
///
/// Returns `Err` on:
/// - duplicate tag names (each tag MUST appear at most once),
/// - element with no `=` separator,
/// - empty tag name.
fn split_tag_list(value: &str) -> Result<Vec<(String, String)>, String> {
    let mut tags: Vec<(String, String)> = Vec::new();
    for part in value.split(';') {
        let part = part.trim();
        if part.is_empty() {
            // Trailing or empty element — RFC allows this.
            continue;
        }
        let (name, val) = match part.split_once('=') {
            Some(p) => p,
            None => return Err(format!("tag without '=' separator: {part}")),
        };
        let name = name.trim().to_ascii_lowercase();
        if name.is_empty() {
            return Err("empty tag name".to_string());
        }
        // Tag names are alphanumeric per the ABNF.
        if !name.chars().all(|c| c.is_ascii_alphanumeric()) {
            return Err(format!("invalid characters in tag name: {name}"));
        }
        if tags.iter().any(|(n, _)| n == &name) {
            return Err(format!("duplicate tag {name}"));
        }
        let val = val.trim().to_string();
        tags.push((name, val));
    }
    Ok(tags)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn happy_value() -> &'static str {
        // A representative DKIM-Signature value (truncated).
        "v=1; a=rsa-sha256; d=example.com; s=mail; \
         c=relaxed/relaxed; q=dns/txt; h=From:To:Subject:Date; \
         bh=MTIzNDU2; b=YWJjZGVm"
    }

    #[test]
    fn parses_minimal_required_tags() {
        let s = parse_dkim_signature(happy_value()).unwrap();
        assert_eq!(s.version, 1);
        assert_eq!(s.algorithm, Algorithm::RsaSha256);
        assert_eq!(s.d, "example.com");
        assert_eq!(s.s, "mail");
        assert_eq!(s.h, vec!["from", "to", "subject", "date"]);
        assert_eq!(s.bh, b"123456");
        assert_eq!(s.b, b"abcdef");
        assert_eq!(s.c_header, HeaderCanon::Relaxed);
        assert_eq!(s.c_body, BodyCanon::Relaxed);
        // i= defaults to @d=
        assert_eq!(s.i, "@example.com");
    }

    #[test]
    fn case_insensitive_tag_names() {
        let v = "V=1; A=rsa-sha256; D=Example.COM; S=mail; \
                 H=From; bh=MTIzNDU2; B=YWJjZGVm";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.algorithm, Algorithm::RsaSha256);
        assert_eq!(s.d, "example.com");
    }

    #[test]
    fn folded_whitespace_in_b_tag_is_tolerated() {
        // Real senders fold long base64 values across multiple lines.
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; \
                 bh=MTIzNDU2; \
                 b=YWJj\r\n\tZGVm\r\n  ";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.b, b"abcdef");
    }

    #[test]
    fn b_tag_value_containing_b_equals_substring_is_not_misparsed() {
        // The literal `b=`-like prefix inside the bh= value must not
        // be misread as the start of a new tag. This is the bug class
        // the PoC PR review flagged.
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; \
                 bh=Yj1ub3RhdGFn; b=YWJjZGVm";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.bh, b"b=notatag");
        assert_eq!(s.b, b"abcdef");
    }

    #[test]
    fn rejects_v_other_than_1() {
        let v = "v=2; a=rsa-sha256; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("v=2"));
    }

    #[test]
    fn rejects_duplicate_tag() {
        let v = "v=1; v=1; a=rsa-sha256; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("duplicate"));
    }

    #[test]
    fn rejects_unsupported_algorithm() {
        let v = "v=1; a=rsa-sha1; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("unsupported"));
    }

    #[test]
    fn parses_ed25519_sha256() {
        let v = "v=1; a=ed25519-sha256; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.algorithm, Algorithm::Ed25519Sha256);
    }

    #[test]
    fn parses_l_t_x_tags() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm; \
                 l=1234; t=1700000000; x=1800000000";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.l, Some(1234));
        assert_eq!(s.t, Some(1700000000));
        assert_eq!(s.x, Some(1800000000));
    }

    #[test]
    fn parses_explicit_i_tag() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; \
                 i=alice@example.com; bh=MTIzNDU2; b=YWJjZGVm";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.i, "alice@example.com");
    }

    #[test]
    fn defaults_canon_when_c_missing() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; bh=MTIzNDU2; b=YWJjZGVm";
        let s = parse_dkim_signature(v).unwrap();
        assert_eq!(s.c_header, HeaderCanon::Simple);
        assert_eq!(s.c_body, BodyCanon::Simple);
    }

    #[test]
    fn rejects_h_without_any_header() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=; bh=MTIzNDU2; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("h="));
    }

    #[test]
    fn rejects_missing_required_tag() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; bh=MTIzNDU2; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("missing required tag h"));
    }

    #[test]
    fn rejects_bad_base64() {
        let v = "v=1; a=rsa-sha256; d=example.com; s=mail; h=from; bh=!!!; b=YWJjZGVm";
        let err = parse_dkim_signature(v).unwrap_err();
        assert!(err.contains("invalid base64 in bh"));
    }
}
