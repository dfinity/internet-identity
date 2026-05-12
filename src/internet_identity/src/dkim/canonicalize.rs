//! Canonicalisation per RFC 6376 §3.4.
//!
//! DKIM verification compares hashes computed over the canonicalised
//! form of the signed material — both the body (for the `bh=` tag) and
//! the listed headers (for `b=`). We implement only the **relaxed**
//! algorithms (§3.4.2 for headers, §3.4.4 for body); `simple/*` on the
//! header side is rejected at the verifier orchestration layer per
//! design doc §5.2.
//!
//! The PoC PR's review specifically called out that a verifier that
//! re-emits headers from parsed `(name, value)` pairs cannot replicate
//! `simple` canonicalization byte-for-byte — but it *can* replicate
//! `relaxed` canonicalisation, because every transformation `relaxed`
//! applies (lowercase names, unfold, collapse WSP, strip WSP around
//! colon and at end of value) is destructive of information the parser
//! has *also* discarded. So the canonical form we compute on
//! synthesised input matches the canonical form the signer computed on
//! the original wire bytes, modulo non-printable edge cases.

/// Canonicalise a single header (relaxed). Per RFC 6376 §3.4.2:
///
/// 1. Convert the header name to lowercase.
/// 2. Unfold continuation lines (replace `CRLF + WSP` with `WSP`).
/// 3. Collapse any sequence of one or more WSP characters in the value
///    to a single SP.
/// 4. Strip trailing WSP from the value.
/// 5. Strip WSP immediately before and after the `:` separator.
/// 6. Emit `name:value\r\n`.
///
/// The `value` we receive from the gateway has already had any folding
/// resolved; we still apply unfolding here in case the gateway delivers
/// folded content.
pub fn relaxed_header(name: &str, value: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(name.len() + value.len() + 4);
    for b in name.bytes() {
        out.push(b.to_ascii_lowercase());
    }
    out.push(b':');
    let canon_value = relaxed_header_value(value);
    out.extend_from_slice(canon_value.as_bytes());
    out.extend_from_slice(b"\r\n");
    out
}

/// Apply steps 2-5 of §3.4.2 to a header value.
fn relaxed_header_value(value: &str) -> String {
    // Step 2: unfold. Walk the bytes, drop any `\r\n` that's followed by
    // WSP. For our synthesised input this is usually a no-op; we still
    // do it because the gateway may pass values that contain CRLF + WSP
    // sequences (folded headers).
    //
    // Step 3 + 4 + 5 done in the same pass: track whether we just emitted
    // a SP so we don't emit a second one (collapsing); skip leading WSP
    // before the first non-WSP byte (handles step 5's "WSP after colon"
    // case since the colon is consumed by the caller); strip trailing
    // WSP at the end.
    let bytes = value.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    let mut last_was_sp = true; // true so leading WSP is dropped (step 5)
    while i < bytes.len() {
        let b = bytes[i];
        // Unfold: `\r\n` followed by WSP → drop the CRLF, keep the WSP
        // (which then collapses with neighbouring WSP).
        if b == b'\r' && i + 2 < bytes.len() && bytes[i + 1] == b'\n' && is_wsp(bytes[i + 2]) {
            i += 2;
            continue;
        }
        if is_wsp(b) {
            if !last_was_sp {
                out.push(b' ');
                last_was_sp = true;
            }
            i += 1;
            continue;
        }
        out.push(b);
        last_was_sp = false;
        i += 1;
    }
    // Step 4: strip trailing WSP. Above loop already collapses runs to
    // a single SP, so at most one trailing SP can exist now.
    if out.last() == Some(&b' ') {
        out.pop();
    }
    String::from_utf8(out).expect("relaxed_header_value preserves UTF-8 from its &str input")
}

/// Whitespace test per RFC 6376 §2.8 (citing RFC 5234): SP (0x20) and
/// HTAB (0x09).
fn is_wsp(b: u8) -> bool {
    b == b' ' || b == b'\t'
}

/// Canonicalise a body (relaxed). Per RFC 6376 §3.4.4:
///
/// - Strip trailing WSP from every line.
/// - Collapse runs of WSP within a line to a single SP.
/// - Strip empty lines from the end of the body.
/// - If the body is non-empty but does not end with CRLF, append CRLF.
///
/// Lines are CRLF-separated. A line consisting solely of `\r\n` after
/// the per-line WSP cleanup is "empty" for the purpose of the final
/// trailing-empty-lines strip.
pub fn relaxed_body(body: &[u8]) -> Vec<u8> {
    // Split into lines (each line is the bytes between consecutive CRLFs;
    // a non-CRLF-terminated tail is its own final line).
    let mut lines: Vec<Vec<u8>> = Vec::new();
    let mut start = 0;
    while start < body.len() {
        let mut end = start;
        let mut had_crlf = false;
        while end < body.len() {
            if body[end] == b'\r' && end + 1 < body.len() && body[end + 1] == b'\n' {
                had_crlf = true;
                break;
            }
            end += 1;
        }
        lines.push(canon_body_line(&body[start..end]));
        start = if had_crlf { end + 2 } else { end };
    }

    // Strip trailing empty lines.
    while lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }

    if lines.is_empty() {
        return Vec::new();
    }

    // Join with CRLF terminators; the algorithm guarantees a single
    // trailing CRLF (§3.4.4 tail clause).
    let mut out: Vec<u8> = Vec::with_capacity(body.len());
    for line in &lines {
        out.extend_from_slice(line);
        out.extend_from_slice(b"\r\n");
    }
    out
}

/// Apply the per-line transformations from §3.4.4 step 1: collapse runs
/// of WSP within the line to a single SP, then strip trailing WSP. The
/// returned `Vec<u8>` contains only the line content (no CRLF).
fn canon_body_line(line: &[u8]) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::with_capacity(line.len());
    let mut last_was_sp = false;
    for &b in line {
        if is_wsp(b) {
            if !last_was_sp {
                out.push(b' ');
                last_was_sp = true;
            }
        } else {
            out.push(b);
            last_was_sp = false;
        }
    }
    if out.last() == Some(&b' ') {
        out.pop();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(bytes: &[u8]) -> String {
        String::from_utf8_lossy(bytes).into_owned()
    }

    // --- Header tests ---

    #[test]
    fn relaxed_header_lowercases_name() {
        let h = relaxed_header("Subject", "hello");
        assert_eq!(s(&h), "subject:hello\r\n");
    }

    #[test]
    fn relaxed_header_preserves_value_case() {
        let h = relaxed_header("Subject", "Hello World");
        assert_eq!(s(&h), "subject:Hello World\r\n");
    }

    #[test]
    fn relaxed_header_strips_leading_value_wsp() {
        // The gateway's parser usually strips this, but tolerate it if
        // present. Per step 5: no WSP between `:` and the value.
        let h = relaxed_header("Subject", "   hello");
        assert_eq!(s(&h), "subject:hello\r\n");
    }

    #[test]
    fn relaxed_header_strips_trailing_value_wsp() {
        let h = relaxed_header("Subject", "hello   ");
        assert_eq!(s(&h), "subject:hello\r\n");
    }

    #[test]
    fn relaxed_header_collapses_internal_wsp_runs() {
        let h = relaxed_header("Subject", "hello   world\t\tfoo");
        assert_eq!(s(&h), "subject:hello world foo\r\n");
    }

    #[test]
    fn relaxed_header_unfolds_continuation_with_tab() {
        let h = relaxed_header("Subject", "long\r\n\tcontinuation");
        assert_eq!(s(&h), "subject:long continuation\r\n");
    }

    #[test]
    fn relaxed_header_unfolds_continuation_with_space() {
        let h = relaxed_header("X-Note", "first\r\n  second");
        assert_eq!(s(&h), "x-note:first second\r\n");
    }

    #[test]
    fn relaxed_header_empty_value() {
        let h = relaxed_header("X-Empty", "");
        assert_eq!(s(&h), "x-empty:\r\n");
    }

    #[test]
    fn relaxed_header_only_whitespace_value() {
        let h = relaxed_header("X-Empty", "   \t  ");
        assert_eq!(s(&h), "x-empty:\r\n");
    }

    // --- Body tests ---

    #[test]
    fn relaxed_body_empty_stays_empty() {
        assert_eq!(relaxed_body(b""), b"");
    }

    #[test]
    fn relaxed_body_strips_trailing_empty_lines() {
        let body = b"hello\r\nworld\r\n\r\n\r\n";
        assert_eq!(relaxed_body(body), b"hello\r\nworld\r\n");
    }

    #[test]
    fn relaxed_body_strips_trailing_wsp_per_line() {
        let body = b"hello   \r\nworld\t\r\n";
        assert_eq!(relaxed_body(body), b"hello\r\nworld\r\n");
    }

    #[test]
    fn relaxed_body_collapses_internal_wsp() {
        let body = b"a   b\r\nc\t\td\r\n";
        assert_eq!(relaxed_body(body), b"a b\r\nc d\r\n");
    }

    #[test]
    fn relaxed_body_appends_crlf_when_missing_at_eof() {
        let body = b"hello world";
        assert_eq!(relaxed_body(body), b"hello world\r\n");
    }

    #[test]
    fn relaxed_body_lines_with_only_wsp_become_empty() {
        // A line of only whitespace becomes an empty line; if it's the
        // last non-empty line, it's stripped per §3.4.4 step 3.
        let body = b"hello\r\n   \t  \r\n";
        assert_eq!(relaxed_body(body), b"hello\r\n");
    }

    #[test]
    fn relaxed_body_keeps_internal_empty_lines() {
        // Empty lines in the middle are preserved.
        let body = b"a\r\n\r\nb\r\n";
        assert_eq!(relaxed_body(body), b"a\r\n\r\nb\r\n");
    }

    #[test]
    fn relaxed_body_single_line_no_eol() {
        // A non-empty body with no CRLF at all: the algorithm appends
        // exactly one trailing CRLF.
        let body = b"x";
        assert_eq!(relaxed_body(body), b"x\r\n");
    }

    #[test]
    fn relaxed_body_only_whitespace_is_empty() {
        // The whole body is one WSP-only line; it canonicalises to
        // empty (no trailing CRLF).
        let body = b"   \r\n";
        assert_eq!(relaxed_body(body), b"");
    }
}
