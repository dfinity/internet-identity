//! Extract the From-header domain for DMARC alignment.
//!
//! RFC 5322 §3.6.2 defines `From:` as an `address-list`. RFC 7489 §3.1.1
//! adds an extra constraint for DMARC: the message MUST have *exactly
//! one* `From:` header containing *exactly one* mailbox. Multiple
//! addresses, group syntax, or a missing/duplicated header all map to
//! a verifier failure.
//!
//! This module is a deliberately minimal parser: enough to spot the
//! valid single-mailbox case and reject everything else. We do *not*
//! try to decode RFC 2047 encoded-words, comments, or quoted-pair
//! sequences inside display names — those are display concerns and
//! don't affect the domain we extract for alignment.
//!
//! Reads run against [`crate::dkim::SignedSmtpMessage`] rather than
//! the raw `SmtpMessage`: the view exposes only the From value DKIM
//! hashed (per RFC 6376 §5.4 bottom-up selection), so a duplicate
//! From header that the DKIM verifier didn't sign cannot influence
//! the domain we align against. The verifier itself separately
//! rejects messages with two From headers (RFC 5322 §3.6 + RFC 6376
//! §8.15), so reaching here with `header("From")` returning `Some`
//! already implies a single signed From.

use crate::dkim::SignedSmtpMessage;

const FROM_HEADER: &str = "From";

/// Resolve the `From:` domain from the DKIM-signed projection of the
/// message and return it lowercased.
///
/// Returns `Err` with a human-readable reason on:
/// - no signed `From:` (signer didn't list it in `h=`, or no From in
///   the message - both mean DKIM doesn't vouch for any From value);
/// - a header value that's empty / has no `@` / has trailing list
///   syntax / uses RFC 5322 group syntax (`name:addrs;`);
/// - a domain that's empty after `@`.
pub fn extract_from_domain(signed: &SignedSmtpMessage) -> Result<String, String> {
    let from_value = signed
        .header(FROM_HEADER)
        .ok_or_else(|| "no DKIM-signed From: header".to_string())?;
    parse_single_mailbox_domain(from_value)
}

/// Parse a single-mailbox `From:` value and return the domain
/// lowercased. Tolerates an optional display name and angle-bracket
/// `<addr-spec>`; rejects address-lists and group syntax.
fn parse_single_mailbox_domain(value: &str) -> Result<String, String> {
    let value = value.trim();
    if value.is_empty() {
        return Err("empty From: value".to_string());
    }

    // Reject group syntax: `name: a@x, b@y;`. The `:` followed by `;`
    // somewhere later is the giveaway. We also reject any unquoted top-
    // level `,` because that would be an address-list with multiple
    // entries — DMARC requires exactly one.
    let (address_part, in_angle_brackets) = match find_address_spec(value)? {
        Some(spec) => spec,
        None => return Err("could not locate address-spec".to_string()),
    };

    // Bare addr-spec (no angle brackets) must be a single token: no
    // whitespace at all. With angle brackets, the inside has already
    // been carved out so anything outside is display-name territory.
    // Without them, `"Alice alice@example.com"` or `"alice @example.com"`
    // would otherwise slip through with domain == example.com.
    if !in_angle_brackets && address_part.chars().any(char::is_whitespace) {
        return Err("From: addr-spec contains whitespace".to_string());
    }

    let (_, domain) = address_part
        .split_once('@')
        .ok_or_else(|| format!("From: value missing '@': {value}"))?;
    let domain = domain.trim().trim_end_matches('.').to_ascii_lowercase();
    if domain.is_empty() {
        return Err("From: value has empty domain".to_string());
    }
    if domain.contains(char::is_whitespace) {
        return Err("From: domain contains whitespace".to_string());
    }
    Ok(domain)
}

/// Walk the value looking for the address-spec. The grammar we accept:
///
/// - `local@domain`                  — bare addr-spec
/// - `<local@domain>`                — angle-addr (display name absent)
/// - `Display Name <local@domain>`   — name-addr
/// - `"Quoted, Name" <local@domain>` — name-addr with quoted display
///
/// Returns `(addr-spec, in_angle_brackets)`. `in_angle_brackets` is
/// `true` when the addr-spec came from inside `<...>` — the caller uses
/// that to relax whitespace rules outside the brackets (display name)
/// while still rejecting whitespace inside the bare addr-spec form.
///
/// Rejects on an unquoted top-level `,` (address-list), any `:` outside
/// angle brackets / quotes (group syntax), or a `>` without a preceding
/// `<`.
fn find_address_spec(value: &str) -> Result<Option<(&str, bool)>, String> {
    let bytes = value.as_bytes();
    let mut in_quotes = false;
    let mut angle_start: Option<usize> = None;
    let mut angle_end: Option<usize> = None;
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        // RFC 5322 §3.2.4: inside a quoted-string a backslash escapes
        // the next character (notably an embedded `"`). Without this
        // we'd close the quoted region prematurely on something like
        // `"Alice \"Ops, Inc\"" <alice@example.com>` and then
        // misinterpret the comma as an address-list separator.
        if in_quotes && b == b'\\' && i + 1 < bytes.len() {
            i += 2;
            continue;
        }
        if b == b'"' {
            in_quotes = !in_quotes;
            i += 1;
            continue;
        }
        if in_quotes {
            i += 1;
            continue;
        }
        if b == b'<' {
            if angle_start.is_some() {
                return Err("nested '<' in From: value".to_string());
            }
            if angle_end.is_some() {
                return Err("'<' after '>' in From: value".to_string());
            }
            angle_start = Some(i + 1);
            i += 1;
            continue;
        }
        if b == b'>' {
            if angle_start.is_none() {
                return Err("'>' without matching '<' in From: value".to_string());
            }
            if angle_end.is_some() {
                return Err("multiple '>' in From: value".to_string());
            }
            angle_end = Some(i);
            i += 1;
            continue;
        }
        if angle_start.is_some() && angle_end.is_none() {
            // Inside angle brackets — anything goes (no further checks).
            i += 1;
            continue;
        }
        // Outside angle brackets (or after the closing `>`): only
        // whitespace is permitted as trailing content. Anything else
        // after `>` would be a malformed mailbox like `<a@b> garbage`.
        if angle_end.is_some() && !b.is_ascii_whitespace() {
            return Err("trailing content after '>' in From: value".to_string());
        }
        if b == b',' {
            return Err("From: contains an address list, not a single mailbox".to_string());
        }
        if b == b':' {
            return Err("From: contains group syntax, not a single mailbox".to_string());
        }
        i += 1;
    }

    if let (Some(start), Some(end)) = (angle_start, angle_end) {
        if start > end {
            return Err("malformed angle-bracket pair in From:".to_string());
        }
        return Ok(Some((value[start..end].trim(), true)));
    }
    if angle_start.is_some() {
        return Err("unclosed '<' in From: value".to_string());
    }
    Ok(Some((value.trim(), false)))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a `SignedSmtpMessage` containing only a signed `From:`
    /// value, mirroring what the DKIM verifier would produce for a
    /// message whose `h=` listed `From` and supplied the given value.
    /// Empty `from_values` produces a view with no signed From, used
    /// to exercise the "DKIM didn't sign From" rejection path.
    fn view_with_from(from_values: &[&str]) -> SignedSmtpMessage {
        use internet_identity_interface::internet_identity::types::smtp::SmtpHeader;
        let picked: Vec<SmtpHeader> = from_values
            .iter()
            .map(|v| SmtpHeader {
                name: "From".into(),
                value: (*v).into(),
            })
            .collect();
        SignedSmtpMessage::from_signed_walk(picked)
    }

    #[test]
    fn bare_addr_spec() {
        let v = view_with_from(&["alice@example.com"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn angle_bracketed() {
        let v = view_with_from(&["<alice@example.com>"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn name_addr() {
        let v = view_with_from(&["Alice Smith <alice@example.com>"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn quoted_display_with_comma_in_name() {
        // A `,` inside quotes is part of the display name and must not
        // be mistaken for an address-list separator.
        let v = view_with_from(&["\"Smith, Alice\" <alice@example.com>"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn quoted_display_with_colon_in_name() {
        let v = view_with_from(&["\"Note: subject\" <alice@example.com>"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn lowercases_domain() {
        let v = view_with_from(&["alice@EXAMPLE.COM"]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    #[test]
    fn strips_trailing_dot() {
        let v = view_with_from(&["alice@example.com."]);
        assert_eq!(extract_from_domain(&v).unwrap(), "example.com");
    }

    /// No signed From - the view's `header("From")` returns `None`,
    /// meaning the signer either didn't list From in `h=` or the
    /// message didn't have one. Both states mean DKIM doesn't vouch
    /// for any From value and the verifier must refuse alignment.
    #[test]
    fn rejects_no_signed_from() {
        let v = view_with_from(&[]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("no DKIM-signed From"));
    }

    #[test]
    fn rejects_address_list() {
        let v = view_with_from(&["alice@example.com, bob@example.com"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("address list"));
    }

    #[test]
    fn rejects_group_syntax() {
        let v = view_with_from(&["recipients: alice@example.com;"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("group"));
    }

    #[test]
    fn rejects_missing_at() {
        let v = view_with_from(&["alice"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("'@'"));
    }

    #[test]
    fn rejects_empty_value() {
        let v = view_with_from(&[""]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("empty"));
    }

    #[test]
    fn rejects_empty_domain_after_at() {
        let v = view_with_from(&["alice@"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("empty domain"));
    }

    #[test]
    fn rejects_unclosed_angle_bracket() {
        let v = view_with_from(&["<alice@example.com"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("unclosed"));
    }

    #[test]
    fn rejects_nested_angle_brackets() {
        let v = view_with_from(&["<<alice@example.com>>"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("nested"));
    }

    #[test]
    fn rejects_whitespace_in_bare_addr_spec() {
        // No angle brackets → the whole value is the addr-spec, so a
        // leading display name with a space would otherwise sneak the
        // last token through as a fake mailbox.
        let v = view_with_from(&["Alice alice@example.com"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("whitespace"));
    }

    #[test]
    fn rejects_space_around_at_in_bare_addr_spec() {
        let v = view_with_from(&["alice @example.com"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("whitespace"));
    }

    #[test]
    fn rejects_stray_closing_angle_without_opening() {
        let v = view_with_from(&["alice@example.com>"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("'>' without matching '<'"));
    }

    #[test]
    fn rejects_content_after_closing_angle() {
        let v = view_with_from(&["<alice@example.com> garbage"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("trailing content"));
    }

    #[test]
    fn rejects_multiple_closing_angles() {
        let v = view_with_from(&["<alice@example.com>>"]);
        let err = extract_from_domain(&v).unwrap_err();
        assert!(err.contains("multiple '>'"));
    }
}
