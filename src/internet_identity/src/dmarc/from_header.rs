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

use internet_identity_interface::internet_identity::types::smtp::SmtpMessage;

const FROM_HEADER: &str = "From";

/// Locate exactly one `From:` header in `message`, parse the single
/// mailbox out of its value, and return the lowercased domain.
///
/// Returns `Err` with a human-readable reason on:
/// - zero or more than one `From:` header,
/// - a header value that's empty / has no `@` / has trailing list
///   syntax / uses RFC 5322 group syntax (`name:addrs;`),
/// - a domain that's empty after `@`.
pub fn extract_from_domain(message: &SmtpMessage) -> Result<String, String> {
    let mut iter = message
        .headers
        .iter()
        .filter(|h| h.name.eq_ignore_ascii_case(FROM_HEADER));
    let from = iter.next().ok_or_else(|| "no From: header".to_string())?;
    if iter.next().is_some() {
        return Err("multiple From: headers".to_string());
    }
    parse_single_mailbox_domain(&from.value)
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
    let address_part = match find_address_spec(value)? {
        Some(s) => s,
        None => return Err("could not locate address-spec".to_string()),
    };

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
/// Returns the inside of the angle brackets if any; otherwise the whole
/// trimmed value. Rejects on an unquoted top-level `,` (address-list)
/// or any `:` that appears outside angle brackets / quotes (group
/// syntax).
fn find_address_spec(value: &str) -> Result<Option<&str>, String> {
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
            angle_start = Some(i + 1);
            i += 1;
            continue;
        }
        if b == b'>' {
            if angle_start.is_none() {
                return Err("stray '>' without matching '<' in From: value".to_string());
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
        return Ok(Some(value[start..end].trim()));
    }
    if angle_start.is_some() {
        return Err("unclosed '<' in From: value".to_string());
    }
    Ok(Some(value.trim()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::smtp::SmtpHeader;
    use serde_bytes::ByteBuf;

    fn message_with(from_values: &[&str]) -> SmtpMessage {
        let mut headers: Vec<SmtpHeader> = vec![SmtpHeader {
            name: "Subject".into(),
            value: "x".into(),
        }];
        for v in from_values {
            headers.push(SmtpHeader {
                name: "From".into(),
                value: (*v).into(),
            });
        }
        SmtpMessage {
            headers,
            body: ByteBuf::from(b"x".to_vec()),
        }
    }

    #[test]
    fn bare_addr_spec() {
        let m = message_with(&["alice@example.com"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn angle_bracketed() {
        let m = message_with(&["<alice@example.com>"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn name_addr() {
        let m = message_with(&["Alice Smith <alice@example.com>"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn quoted_display_with_comma_in_name() {
        // A `,` inside quotes is part of the display name and must not
        // be mistaken for an address-list separator.
        let m = message_with(&["\"Smith, Alice\" <alice@example.com>"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn quoted_display_with_colon_in_name() {
        let m = message_with(&["\"Note: subject\" <alice@example.com>"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn lowercases_domain() {
        let m = message_with(&["alice@EXAMPLE.COM"]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn strips_trailing_dot() {
        let m = message_with(&["alice@example.com."]);
        assert_eq!(extract_from_domain(&m).unwrap(), "example.com");
    }

    #[test]
    fn rejects_no_from_header() {
        let m = message_with(&[]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("no From"));
    }

    #[test]
    fn rejects_multiple_from_headers() {
        let m = message_with(&["alice@example.com", "bob@example.com"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("multiple"));
    }

    #[test]
    fn rejects_address_list() {
        let m = message_with(&["alice@example.com, bob@example.com"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("address list"));
    }

    #[test]
    fn rejects_group_syntax() {
        let m = message_with(&["recipients: alice@example.com;"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("group"));
    }

    #[test]
    fn rejects_missing_at() {
        let m = message_with(&["alice"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("'@'"));
    }

    #[test]
    fn rejects_empty_value() {
        let m = message_with(&[""]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("empty"));
    }

    #[test]
    fn rejects_empty_domain_after_at() {
        let m = message_with(&["alice@"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("empty domain"));
    }

    #[test]
    fn rejects_unclosed_angle_bracket() {
        let m = message_with(&["<alice@example.com"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("unclosed"));
    }

    #[test]
    fn rejects_nested_angle_brackets() {
        let m = message_with(&["<<alice@example.com>>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("nested"));
    }

    #[test]
    fn rejects_stray_close_angle_bracket() {
        let m = message_with(&["alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("stray '>'"));
    }

    #[test]
    fn rejects_multiple_close_angle_brackets() {
        let m = message_with(&["<alice@example.com>>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("multiple '>'"));
    }
}
