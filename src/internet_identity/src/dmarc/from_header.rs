//! Extract the From-header domain for DMARC alignment.
//!
//! RFC 5322 §3.6.2 defines `From:` as an `address-list`. RFC 7489 §3.1.1
//! adds an extra constraint for DMARC: the message MUST have *exactly
//! one* `From:` header containing *exactly one* mailbox. Multiple
//! addresses, group syntax, or a missing/duplicated header all map to
//! a verifier failure.
//!
//! Rather than implement RFC 5322 in general, this module matches the
//! `From:` value against the three mailbox forms the email-recovery
//! flow actually receives:
//!
//! ```text
//! local@domain
//! Display Name <local@domain>
//! "Quoted Display Name" <local@domain>
//! ```
//!
//! An absent display name (`<local@domain>`) is the degenerate case of
//! the latter two. Everything else — comments, quoted-pair escapes,
//! RFC 2047 encoded-words, address lists, group syntax — is refused
//! rather than interpreted.

use internet_identity_interface::internet_identity::types::smtp::SmtpMessage;

const FROM_HEADER: &str = "From";

/// Locate exactly one `From:` header in `message`, parse the single
/// mailbox out of its value, and return the lowercased domain.
///
/// Returns `Err` with a human-readable reason on:
/// - zero or more than one `From:` header,
/// - a header value that's empty / has no `@` / has more than one `@`
///   / has trailing list syntax / uses RFC 5322 group syntax
///   (`name:addrs;`),
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
    let address_part = parse_single_mailbox_addr_spec(value)?;
    let (_, domain) = address_part
        .split_once('@')
        .ok_or_else(|| format!("From: value missing '@': {address_part}"))?;
    let domain = domain.trim().trim_end_matches('.').to_ascii_lowercase();
    if domain.is_empty() {
        return Err("From: value has empty domain".to_string());
    }
    if domain.contains(char::is_whitespace) {
        return Err("From: domain contains whitespace".to_string());
    }
    Ok(domain)
}

/// Parse an RFC 5322 `From:` value down to its single `addr-spec`
/// (`local@domain`, no angle brackets, no display name).
///
/// This is the shared front half of [`parse_single_mailbox_domain`]:
/// it matches one of the supported mailbox forms and returns the whole
/// `local@domain` slice instead of only the domain. Callers that need
/// the full mailbox — the email-recovery inbound path, which matches
/// the verified sender against the anchor's registered address —
/// reuse this rather than re-implementing the mailbox scan.
///
/// The returned slice borrows from `value` (trimmed); it still
/// contains the `@`, and the caller is responsible for any further
/// splitting / length bounds.
pub(crate) fn parse_single_mailbox_addr_spec(value: &str) -> Result<&str, String> {
    let value = value.trim();
    if value.is_empty() {
        return Err("empty From: value".to_string());
    }
    let (address_part, in_angle_brackets) = find_address_spec(value)?;
    validate_addr_spec(address_part, in_angle_brackets)?;
    Ok(address_part)
}

/// Characters that only occur in the RFC 5322 constructs this module
/// declines to implement: quoted strings, comments, and quoted-pair
/// escapes.
const UNSUPPORTED_CHARS: [char; 4] = ['"', '(', ')', '\\'];

fn reject_unsupported(part: &str, label: &str) -> Result<(), String> {
    match part.chars().find(|c| UNSUPPORTED_CHARS.contains(c)) {
        Some(c) => Err(format!("From: {label} uses unsupported syntax '{c}'")),
        None => Ok(()),
    }
}

fn reject_list_or_group(part: &str) -> Result<(), String> {
    if part.contains(',') {
        return Err("From: contains an address list, not a single mailbox".to_string());
    }
    if part.contains(':') {
        return Err("From: contains group syntax, not a single mailbox".to_string());
    }
    Ok(())
}

/// Split `value` into display name and `addr-spec` at the angle
/// brackets, returning `(addr-spec, in_angle_brackets)`.
///
/// The brackets are counted over the whole value rather than tracked
/// against quoting state, so a `<` or `>` inside a quoted display name
/// is refused alongside a genuinely nested one.
fn find_address_spec(value: &str) -> Result<(&str, bool), String> {
    let opens = value.matches('<').count();
    let closes = value.matches('>').count();
    if opens > 1 {
        return Err("nested '<' in From: value".to_string());
    }
    if closes > 1 {
        return Err("multiple '>' in From: value".to_string());
    }
    match (opens, closes) {
        (0, 0) => Ok((value, false)),
        (0, _) => Err("'>' without matching '<' in From: value".to_string()),
        (_, 0) => Err("unclosed '<' in From: value".to_string()),
        _ => {
            let start = value.find('<').expect("counted one '<'");
            let end = value.find('>').expect("counted one '>'");
            if start > end {
                return Err("malformed angle-bracket pair in From:".to_string());
            }
            if !value[end + 1..].trim().is_empty() {
                return Err("trailing content after '>' in From: value".to_string());
            }
            validate_display_name(value[..start].trim())?;
            Ok((value[start + 1..end].trim(), true))
        }
    }
}

/// A display name is absent, one quoted string, or one plain run. `,`
/// and `:` are ordinary text inside quotes; unquoted they would make
/// the value an address list or a group, so they are refused there.
fn validate_display_name(display: &str) -> Result<(), String> {
    if display.is_empty() {
        return Ok(());
    }
    match display.strip_prefix('"') {
        Some(rest) => {
            let inner = rest
                .strip_suffix('"')
                .ok_or_else(|| "malformed quoted display name in From:".to_string())?;
            reject_unsupported(inner, "display name")
        }
        None => {
            reject_list_or_group(display)?;
            reject_unsupported(display, "display name")
        }
    }
}

/// The `addr-spec`: one whitespace-free token carrying exactly one `@`
/// and none of the unsupported syntax. `in_angle_brackets` only shapes
/// the whitespace diagnostic — the rule itself is the same either way.
fn validate_addr_spec(addr: &str, in_angle_brackets: bool) -> Result<(), String> {
    reject_list_or_group(addr)?;
    if addr.chars().any(char::is_whitespace) {
        return Err(if in_angle_brackets {
            "From: addr-spec contains whitespace inside '<...>'".to_string()
        } else {
            "From: addr-spec contains whitespace".to_string()
        });
    }
    // Exactly one `@`, so "the domain" is the same slice no matter
    // which side a caller splits from. RFC 5322 does permit a quoted
    // local-part to carry an `@` (`"a@b"@example.com`), but accepting
    // one means a first-`@` split and a last-`@` split disagree about
    // the domain — and this parser feeds both.
    if addr.matches('@').count() != 1 {
        return Err("From: addr-spec must contain exactly one '@'".to_string());
    }
    reject_unsupported(addr, "addr-spec")
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
    fn rejects_whitespace_in_bare_addr_spec() {
        // No angle brackets → the whole value is the addr-spec, so a
        // leading display name with a space would otherwise sneak the
        // last token through as a fake mailbox.
        let m = message_with(&["Alice alice@example.com"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("whitespace"));
    }

    #[test]
    fn rejects_space_around_at_in_bare_addr_spec() {
        let m = message_with(&["alice @example.com"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("whitespace"));
    }

    #[test]
    fn rejects_second_at_in_bare_addr_spec() {
        let m = message_with(&["alice@example.com@example.org"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("exactly one '@'"));
    }

    #[test]
    fn rejects_second_at_in_quoted_local_part() {
        // Legal RFC 5322, deliberately refused: a left split and a right
        // split disagree about the domain, and both happen downstream.
        let m = message_with(&["\"alice@example.com\"@example.org"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("exactly one '@'"));
    }

    #[test]
    fn rejects_second_at_inside_angle_brackets() {
        let m = message_with(&["Alice <alice@example.com@example.org>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("exactly one '@'"));
    }

    #[test]
    fn rejects_stray_closing_angle_without_opening() {
        let m = message_with(&["alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("'>' without matching '<'"));
    }

    #[test]
    fn rejects_content_after_closing_angle() {
        let m = message_with(&["<alice@example.com> garbage"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("trailing content"));
    }

    #[test]
    fn rejects_multiple_closing_angles() {
        let m = message_with(&["<alice@example.com>>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("multiple '>'"));
    }

    #[test]
    fn accepts_the_supported_mailbox_forms() {
        for value in [
            "alice@example.com",
            "<alice@example.com>",
            "Alice Smith <alice@example.com>",
            "\"Smith, Alice\" <alice@example.com>",
        ] {
            let m = message_with(&[value]);
            assert_eq!(
                extract_from_domain(&m).unwrap(),
                "example.com",
                "should accept {value}"
            );
        }
    }

    #[test]
    fn rejects_parenthesized_comment() {
        let m = message_with(&["Alice (Ops) <alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("unsupported syntax"));
    }

    #[test]
    fn rejects_quoted_pair_escape() {
        let m = message_with(&["\"Alice \\\"Ops\\\"\" <alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("unsupported syntax"));
    }

    #[test]
    fn rejects_unterminated_quoted_display_name() {
        let m = message_with(&["\"Alice <alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("malformed quoted display name"));
    }

    #[test]
    fn rejects_trailing_content_after_quoted_display_name() {
        let m = message_with(&["\"Alice\"Smith <alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("malformed quoted display name"));
    }

    #[test]
    fn rejects_address_list_before_angle_addr() {
        // The unquoted display name is where a second mailbox would sit
        // if the value were an address list rather than one mailbox.
        let m = message_with(&["alice@example.com, Bob <bob@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("address list"));
    }

    #[test]
    fn rejects_group_syntax_before_angle_addr() {
        let m = message_with(&["Recipients: Bob <bob@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("group"));
    }

    #[test]
    fn rejects_whitespace_inside_angle_brackets() {
        let m = message_with(&["<alice @example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("whitespace"));
    }

    #[test]
    fn rejects_angle_bracket_inside_quoted_display_name() {
        let m = message_with(&["\"Alice <other@example.org>\" <alice@example.com>"]);
        let err = extract_from_domain(&m).unwrap_err();
        assert!(err.contains("nested '<'"));
    }
}
