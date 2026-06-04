//! SMTP gateway protocol types (Candid).
//!
//! The off-chain SMTP gateway forwards every inbound message to the canister
//! by calling `smtp_request(SmtpRequest) -> SmtpResponse`. The shape mirrors
//! the PoC #3760 surface so the existing gateway can target this canister
//! without changes — see `docs/ongoing/email-recovery.md` §5.2 for the
//! decision to keep the parsed-headers contract instead of moving the
//! gateway to raw bytes.
//!
//! These types are _just_ the wire surface plus input-bound validation.
//! DKIM verification (PR 2) and the canister-side dispatch (PR 8) consume
//! `SmtpRequest` from elsewhere in the canister.
//!
//! Carried forward from PoC PR #3760 with the postbox-specific bits removed:
//! - `PostboxEmail`, `ValidatedSmtpRequest`, the `to.user → anchor_number`
//!   parser, and the per-anchor mailbox storage are all out of scope here
//!   (design §2 non-goals).
//! - The validation helpers come along unchanged because the bounds and
//!   the multi-byte-UTF-8-safe truncation were the result of a careful
//!   review pass on the PoC PR.

use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;

// --- Bounds ---

pub const MAX_EMAIL_USER_BYTES: usize = 64;
pub const MAX_EMAIL_DOMAIN_BYTES: usize = 255;
pub const MAX_SUBJECT_BYTES: usize = 256;
pub const MAX_BODY_BYTES: usize = 5_000;
/// Cap on the number of headers in one inbound message. Real-world mail
/// that has traversed several hops accumulates a long run of trace and
/// authentication headers (`Received`, `Authentication-Results`, `ARC-*`,
/// and provider-specific `X-MS-Exchange-*` / `X-Microsoft-*` fields), so a
/// legitimate message routed through Outlook/Exchange routinely carries far
/// more than a hand-authored one. The original bound of 30 was too tight —
/// it rejected genuine recovery emails (e.g. 39 headers from Outlook) — so
/// it is raised to 100 to leave comfortable headroom while still keeping
/// worst-case allocation on the open `smtp_request` endpoint bounded.
pub const MAX_HEADERS: usize = 100;
pub const MAX_HEADER_NAME_BYTES: usize = 256;
pub const MAX_HEADER_VALUE_BYTES: usize = 8_192;
/// Cap on the optional gateway-supplied `message_id` correlation id.
/// `smtp_request` and `smtp_request_validate` are both open (anyone can
/// call them), so — like every other inbound string — it is length-bounded
/// to keep worst-case allocation on an open endpoint in check. 256 bytes
/// comfortably fits an RFC 5322 `Message-ID` or a gateway-assigned tracking
/// id; oversize values are rejected with code 555.
pub const MAX_MESSAGE_ID_BYTES: usize = 256;

/// Headers that RFC 5322 §3.6 requires to appear exactly once in a
/// message. Counted case-insensitively.
const REQUIRED_EXACTLY_ONCE: &[&str] = &["From", "Date"];

/// Headers that RFC 5322 §3.6 allows zero or one of, but never
/// duplicates. Reading code in this canister (`extract_from_address`,
/// `extract_nonce_from_subject`, DKIM-Signature lookup) uses `.find()`
/// and would silently pick the first occurrence — a duplicate header
/// is a header-smuggling vector against DKIM signature coverage, so
/// we reject the whole message at the input-bounds layer instead.
const ALLOWED_AT_MOST_ONCE: &[&str] = &[
    "Sender",
    "Reply-To",
    "To",
    "Cc",
    "Bcc",
    "Subject",
    "Message-ID",
    "In-Reply-To",
    "References",
];

/// Cap on `envelope.to` length. `smtp_request` and
/// `smtp_request_validate` are both open (anyone can call them), so
/// without an explicit upper bound an attacker could pad the
/// recipient list out to the ingress size limit and force per-message
/// allocation + per-recipient case-insensitive dispatch on every
/// call. RFC 5321 §4.5.3.1 caps a single SMTP session at 100
/// recipients, which is far more than the gateway will ever batch in
/// practice (the canister only handles two reserved mailboxes), so
/// using the RFC ceiling here just kicks the obvious adversarial case
/// out without constraining any legitimate gateway flow.
pub const MAX_RECIPIENTS: usize = 100;

// --- SMTP error codes (RFC 5321) ---

/// 550 — "Requested action not taken: mailbox unavailable" (RFC 5321
/// §4.2.2). Used here for the "No such user here" case: the envelope
/// has exactly one recipient but it's not a mailbox this canister
/// handles (`register@<d>` / `recover@<d>`).
pub const SMTP_ERR_MAILBOX_UNAVAILABLE: u64 = 550;
/// 551 — "User not local; please try `<forward-path>`" (RFC 5321
/// §4.2.2). Used here for envelopes whose **shape** doesn't fit the
/// recovery flows we serve: an envelope must carry exactly one
/// recipient, so empty-`to` and multi-recipient envelopes get 551
/// (a distinct code from 550 so the gateway can tell "we don't know
/// this user" from "this envelope shape isn't accepted for us").
pub const SMTP_ERR_USER_NOT_LOCAL: u64 = 551;
/// 555 — "Syntax error" (RFC 5321 §4.2.2). Used here for malformed
/// request payloads (missing envelope, bounds violations on
/// address/header/body lengths, or recipient lists that blow past
/// `MAX_RECIPIENTS`).
pub const SMTP_ERR_SYNTAX_ERROR: u64 = 555;

// --- Candid wire types ---

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpHeader {
    pub name: String,
    pub value: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpMessage {
    pub headers: Vec<SmtpHeader>,
    pub body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpAddress {
    pub user: String,
    pub domain: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpEnvelope {
    pub from: SmtpAddress,
    /// SMTP allows multiple `RCPT TO` recipients per envelope, so this
    /// is a `Vec`. The canister-side dispatcher picks the first
    /// recognised recipient to decide which flow to run; unknown
    /// recipients in the same vec are ignored. The wire-level
    /// `smtp_request_validate` query refuses the SMTP transaction
    /// (550 — "No such user here") only when *no* recipient is
    /// recognised, mirroring how `RCPT TO` interacts with the gateway.
    pub to: Vec<SmtpAddress>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpRequest {
    pub message: Option<SmtpMessage>,
    pub envelope: Option<SmtpEnvelope>,
    pub gateway_flags: Option<Vec<String>>,
    /// Optional gateway-supplied correlation id for one inbound message
    /// (e.g. the RFC 5322 `Message-ID` or a gateway-assigned tracking id).
    /// The canister does not interpret it; it exists purely so a reported
    /// case can be lined up across the SMTP gateway logs and the canister's
    /// production logs during support investigations. Length-bounded by
    /// [`MAX_MESSAGE_ID_BYTES`] since the entrypoints are open.
    pub message_id: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SmtpRequestError {
    pub code: u64,
    pub message: String,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum SmtpResponse {
    Ok {},
    Err(SmtpRequestError),
}

// --- Helpers ---

pub fn smtp_err(code: u64, message: impl Into<String>) -> SmtpResponse {
    SmtpResponse::Err(SmtpRequestError {
        code,
        message: message.into(),
    })
}

/// Render an address as `user@domain` with both parts ASCII-lowercased so
/// it is a stable lookup key regardless of envelope casing. The validators
/// below are case-insensitive (`eq_ignore_ascii_case` / `to_lowercase`),
/// so without this canonicalisation the same logical mailbox could be
/// stored under multiple keys by varying case.
pub fn format_address(addr: &SmtpAddress) -> String {
    format!(
        "{}@{}",
        addr.user.to_ascii_lowercase(),
        addr.domain.to_ascii_lowercase()
    )
}

/// Clamp `s` to at most `max_bytes`, on the previous UTF-8 char boundary.
///
/// `String::truncate` panics when the byte index splits a codepoint;
/// callers handling untrusted input (subject lines, body decode results)
/// must use this helper instead. PoC PR #3760's review caught a multi-byte
/// subject case that would have trapped the canister.
pub fn truncate_at_char_boundary(s: &mut String, max_bytes: usize) {
    if s.len() <= max_bytes {
        return;
    }
    let mut end = max_bytes;
    while end > 0 && !s.is_char_boundary(end) {
        end -= 1;
    }
    s.truncate(end);
}

// --- Validation ---

pub fn validate_address_bounds(addr: &SmtpAddress, label: &str) -> Result<(), SmtpResponse> {
    if addr.user.len() > MAX_EMAIL_USER_BYTES {
        return Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!("{label} user part exceeds {MAX_EMAIL_USER_BYTES} bytes"),
        ));
    }
    if addr.domain.len() > MAX_EMAIL_DOMAIN_BYTES {
        return Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!("{label} domain exceeds {MAX_EMAIL_DOMAIN_BYTES} bytes"),
        ));
    }
    Ok(())
}

pub fn validate_envelope(envelope: &SmtpEnvelope) -> Result<(), SmtpResponse> {
    validate_address_bounds(&envelope.from, "Sender")?;
    if envelope.to.len() > MAX_RECIPIENTS {
        return Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!(
                "Too many recipients: {} (max {MAX_RECIPIENTS})",
                envelope.to.len()
            ),
        ));
    }
    for to in &envelope.to {
        validate_address_bounds(to, "Recipient")?;
    }
    Ok(())
}

pub fn validate_message(message: &SmtpMessage) -> Result<(), SmtpResponse> {
    if message.headers.len() > MAX_HEADERS {
        return Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!(
                "Too many headers: {} (max {MAX_HEADERS})",
                message.headers.len()
            ),
        ));
    }
    for header in &message.headers {
        if header.name.len() > MAX_HEADER_NAME_BYTES {
            return Err(smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!("Header name exceeds {MAX_HEADER_NAME_BYTES} bytes"),
            ));
        }
        if header.value.len() > MAX_HEADER_VALUE_BYTES {
            return Err(smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!(
                    "Header '{}' value exceeds {MAX_HEADER_VALUE_BYTES} bytes",
                    header.name
                ),
            ));
        }
    }
    if message.body.len() > MAX_BODY_BYTES {
        return Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!(
                "Body size {} exceeds limit of {MAX_BODY_BYTES} bytes",
                message.body.len()
            ),
        ));
    }
    validate_header_occurrences(&message.headers)?;
    Ok(())
}

/// Enforce RFC 5322 §3.6 header occurrence rules: `From` and `Date`
/// must appear exactly once; `Sender`, `Reply-To`, `To`, `Cc`, `Bcc`,
/// `Subject`, `Message-ID`, `In-Reply-To`, and `References` must not
/// appear more than once. Case-insensitive per RFC 5322 §1.2.2.
///
/// Headers not listed (trace headers, `Resent-*`, `Comments`,
/// `Keywords`, and any `X-*`/optional-field) may repeat freely under
/// the RFC and are not checked here.
pub fn validate_header_occurrences(headers: &[SmtpHeader]) -> Result<(), SmtpResponse> {
    let count = |name: &str| {
        headers
            .iter()
            .filter(|h| h.name.eq_ignore_ascii_case(name))
            .count()
    };
    for &name in REQUIRED_EXACTLY_ONCE {
        let c = count(name);
        if c != 1 {
            return Err(smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!("Header '{name}' must appear exactly once (RFC 5322 §3.6), found {c}"),
            ));
        }
    }
    for &name in ALLOWED_AT_MOST_ONCE {
        let c = count(name);
        if c > 1 {
            return Err(smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!("Header '{name}' must appear at most once (RFC 5322 §3.6), found {c}"),
            ));
        }
    }
    Ok(())
}

/// Bounds-check an `SmtpRequest`: the envelope (always), the message body
/// if present, and the optional `message_id`. Per the PoC review, the
/// validate path must accept envelope-only requests for
/// `smtp_request_validate`.
pub fn validate_smtp_request(request: &SmtpRequest) -> Result<(), SmtpResponse> {
    let envelope = request
        .envelope
        .as_ref()
        .ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"))?;
    validate_envelope(envelope)?;
    if let Some(message) = &request.message {
        validate_message(message)?;
    }
    if let Some(message_id) = &request.message_id {
        if message_id.len() > MAX_MESSAGE_ID_BYTES {
            return Err(smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!("message_id exceeds {MAX_MESSAGE_ID_BYTES} bytes"),
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.into(),
            domain: domain.into(),
        }
    }

    fn err_code(resp: &SmtpResponse) -> u64 {
        match resp {
            SmtpResponse::Err(e) => e.code,
            SmtpResponse::Ok {} => panic!("expected Err"),
        }
    }

    fn err_msg(resp: &SmtpResponse) -> &str {
        match resp {
            SmtpResponse::Err(e) => &e.message,
            SmtpResponse::Ok {} => panic!("expected Err"),
        }
    }

    #[test]
    fn format_address_lowercases_both_parts() {
        let a = addr("Alice", "GMAIL.COM");
        assert_eq!(format_address(&a), "alice@gmail.com");
    }

    #[test]
    fn truncate_at_char_boundary_short_string_unchanged() {
        let mut s = "hi".to_string();
        truncate_at_char_boundary(&mut s, 100);
        assert_eq!(s, "hi");
    }

    #[test]
    fn truncate_at_char_boundary_clamps_on_codepoint() {
        // "naïve": n(1) a(1) ï(2 bytes) v(1) e(1) = 6 bytes.
        // Truncating to 3 bytes lands inside 'ï' (which spans bytes 2..4).
        // We must fall back to byte 2, leaving "na".
        let mut s = "naïve".to_string();
        truncate_at_char_boundary(&mut s, 3);
        assert_eq!(s, "na");
    }

    #[test]
    fn truncate_at_char_boundary_lands_on_boundary() {
        // "ééé": each é is 2 bytes, total 6. Truncating to 3 bytes lands
        // mid-second-é; we should clamp back to byte 2.
        let mut s = "ééé".to_string();
        truncate_at_char_boundary(&mut s, 3);
        assert_eq!(s, "é");
    }

    #[test]
    fn validate_envelope_rejects_oversized_user() {
        let env = SmtpEnvelope {
            from: addr(&"x".repeat(MAX_EMAIL_USER_BYTES + 1), "ok.com"),
            to: vec![addr("a", "ok.com")],
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("Sender user"));
    }

    #[test]
    fn validate_envelope_rejects_oversized_domain() {
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: vec![addr("a", &"d".repeat(MAX_EMAIL_DOMAIN_BYTES + 1))],
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert!(err_msg(&resp).contains("Recipient domain"));
    }

    #[test]
    fn validate_envelope_rejects_any_oversized_recipient() {
        // Bounds checking must run over every recipient in the vec —
        // not just the first — otherwise a malformed RCPT TO further
        // down the list could slip past.
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: vec![
                addr("recover", "id.ai"),
                addr(&"x".repeat(MAX_EMAIL_USER_BYTES + 1), "ok.com"),
            ],
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("Recipient user"));
    }

    #[test]
    fn validate_envelope_rejects_too_many_recipients() {
        // `smtp_request` is open; without a cap an attacker could pad
        // the recipient list out to the ingress size limit. The bound
        // here keeps worst-case validation/dispatch costs bounded.
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: (0..(MAX_RECIPIENTS + 1))
                .map(|_| addr("recover", "id.ai"))
                .collect(),
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("Too many recipients"));
    }

    #[test]
    fn validate_envelope_accepts_max_recipients() {
        // Exactly at the cap must still pass — defines the inclusive
        // upper bound.
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: (0..MAX_RECIPIENTS)
                .map(|_| addr("recover", "id.ai"))
                .collect(),
        };
        assert!(validate_envelope(&env).is_ok());
    }

    #[test]
    fn validate_envelope_accepts_empty_to_vec() {
        // Bounds-only check: an empty `to` vec is shape-valid here.
        // The recipient-dispatch logic in `email_recovery::smtp`
        // separately rejects "no known recipient" with 550 — that
        // semantic isn't this helper's job.
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: vec![],
        };
        assert!(validate_envelope(&env).is_ok());
    }

    #[test]
    fn validate_message_rejects_too_many_headers() {
        let msg = SmtpMessage {
            headers: (0..(MAX_HEADERS + 1))
                .map(|i| SmtpHeader {
                    name: format!("X-{i}"),
                    value: "v".into(),
                })
                .collect(),
            body: ByteBuf::from(b"".to_vec()),
        };
        let resp = validate_message(&msg).unwrap_err();
        assert!(err_msg(&resp).contains("Too many headers"));
    }

    #[test]
    fn validate_message_accepts_max_headers() {
        // Exactly at the cap must still pass — defines the inclusive upper
        // bound, and guards the motivating case: a message that has picked
        // up a long run of trace/authentication headers en route (e.g.
        // through Outlook/Exchange) must not be rejected. From and Date
        // appear exactly once; the remainder are repeatable trace headers so
        // the occurrence rules in `validate_header_occurrences` are satisfied.
        let mut headers = minimal_headers();
        headers.extend(
            (0..(MAX_HEADERS - headers.len())).map(|i| header("Received", &format!("hop {i}"))),
        );
        assert_eq!(headers.len(), MAX_HEADERS);
        assert!(validate_message(&message_with(headers)).is_ok());
    }

    #[test]
    fn validate_message_rejects_oversize_body() {
        let msg = SmtpMessage {
            headers: vec![],
            body: ByteBuf::from(vec![0u8; MAX_BODY_BYTES + 1]),
        };
        let resp = validate_message(&msg).unwrap_err();
        assert!(err_msg(&resp).contains("Body size"));
    }

    #[test]
    fn validate_smtp_request_missing_envelope_is_syntax() {
        let req = SmtpRequest {
            envelope: None,
            message: None,
            gateway_flags: None,
            message_id: None,
        };
        let resp = validate_smtp_request(&req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("envelope"));
    }

    fn header(name: &str, value: &str) -> SmtpHeader {
        SmtpHeader {
            name: name.into(),
            value: value.into(),
        }
    }

    fn message_with(headers: Vec<SmtpHeader>) -> SmtpMessage {
        SmtpMessage {
            headers,
            body: ByteBuf::from(b"hi".to_vec()),
        }
    }

    /// Minimal RFC-5322-conformant header set: From and Date present
    /// exactly once, everything optional absent.
    fn minimal_headers() -> Vec<SmtpHeader> {
        vec![
            header("From", "alice@example.com"),
            header("Date", "Mon, 26 May 2026 12:00:00 +0000"),
        ]
    }

    #[test]
    fn validate_header_occurrences_minimal_ok() {
        assert!(validate_header_occurrences(&minimal_headers()).is_ok());
    }

    #[test]
    fn validate_header_occurrences_rejects_missing_from() {
        let headers = vec![header("Date", "Mon, 26 May 2026 12:00:00 +0000")];
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("'From' must appear exactly once"));
        assert!(err_msg(&resp).contains("found 0"));
    }

    #[test]
    fn validate_header_occurrences_rejects_missing_date() {
        let headers = vec![header("From", "alice@example.com")];
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'Date' must appear exactly once"));
    }

    #[test]
    fn validate_header_occurrences_rejects_duplicate_from() {
        let mut headers = minimal_headers();
        headers.push(header("From", "mallory@example.com"));
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'From' must appear exactly once"));
        assert!(err_msg(&resp).contains("found 2"));
    }

    #[test]
    fn validate_header_occurrences_rejects_duplicate_date() {
        let mut headers = minimal_headers();
        headers.push(header("Date", "Tue, 27 May 2026 12:00:00 +0000"));
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'Date' must appear exactly once"));
        assert!(err_msg(&resp).contains("found 2"));
    }

    #[test]
    fn validate_header_occurrences_rejects_duplicate_subject() {
        // The motivating case: nonce extraction in
        // `extract_nonce_from_subject` uses `.find()` and would silently
        // pick the first Subject. A duplicate Subject lets a sender
        // sign one Subject under DKIM and present another to the
        // canister's nonce lookup. We refuse the whole message.
        let mut headers = minimal_headers();
        headers.push(header("Subject", "first"));
        headers.push(header("Subject", "second"));
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'Subject' must appear at most once"));
        assert!(err_msg(&resp).contains("found 2"));
    }

    #[test]
    fn validate_header_occurrences_rejects_duplicate_to() {
        let mut headers = minimal_headers();
        headers.push(header("To", "recover@id.ai"));
        headers.push(header("To", "register@id.ai"));
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'To' must appear at most once"));
    }

    #[test]
    fn validate_header_occurrences_case_insensitive() {
        // "FROM" and "from" must be counted as the same header.
        let headers = vec![
            header("FROM", "alice@example.com"),
            header("from", "mallory@example.com"),
            header("Date", "Mon, 26 May 2026 12:00:00 +0000"),
        ];
        let resp = validate_header_occurrences(&headers).unwrap_err();
        assert!(err_msg(&resp).contains("'From' must appear exactly once"));
        assert!(err_msg(&resp).contains("found 2"));
    }

    #[test]
    fn validate_header_occurrences_allows_repeated_optional() {
        // Optional / extension headers (X-*) may repeat freely per
        // RFC 5322 §3.6.8.
        let mut headers = minimal_headers();
        headers.push(header("X-Custom", "one"));
        headers.push(header("X-Custom", "two"));
        headers.push(header("Comments", "a"));
        headers.push(header("Comments", "b"));
        assert!(validate_header_occurrences(&headers).is_ok());
    }

    #[test]
    fn validate_header_occurrences_allows_repeated_trace() {
        // Received and Resent-* may repeat per RFC 5322 §3.6.6 / §3.6.7.
        let mut headers = minimal_headers();
        headers.push(header("Received", "from a"));
        headers.push(header("Received", "from b"));
        headers.push(header("Resent-From", "x@example.com"));
        headers.push(header("Resent-From", "y@example.com"));
        assert!(validate_header_occurrences(&headers).is_ok());
    }

    #[test]
    fn validate_header_occurrences_all_singletons_ok() {
        // Every at-most-once header present exactly once is legal.
        let headers = vec![
            header("From", "alice@example.com"),
            header("Date", "Mon, 26 May 2026 12:00:00 +0000"),
            header("Sender", "alice@example.com"),
            header("Reply-To", "alice@example.com"),
            header("To", "recover@id.ai"),
            header("Cc", "bob@example.com"),
            header("Bcc", "carol@example.com"),
            header("Subject", "II-Recovery-nonce"),
            header("Message-ID", "<id@example.com>"),
            header("In-Reply-To", "<prev@example.com>"),
            header("References", "<prev@example.com>"),
        ];
        assert!(validate_header_occurrences(&headers).is_ok());
    }

    #[test]
    fn validate_message_rejects_duplicate_subject() {
        // End-to-end through validate_message: duplicate Subject must
        // be rejected by the canister entrypoint via validate_message,
        // not just by the standalone helper.
        let msg = message_with(vec![
            header("From", "alice@example.com"),
            header("Date", "Mon, 26 May 2026 12:00:00 +0000"),
            header("Subject", "first"),
            header("Subject", "second"),
        ]);
        let resp = validate_message(&msg).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("'Subject' must appear at most once"));
    }

    #[test]
    fn validate_smtp_request_envelope_only_ok() {
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "gmail.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: None,
            gateway_flags: None,
            message_id: None,
        };
        assert!(validate_smtp_request(&req).is_ok());
    }

    fn envelope_only_request(message_id: Option<String>) -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "gmail.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: None,
            gateway_flags: None,
            message_id,
        }
    }

    #[test]
    fn validate_smtp_request_accepts_within_bound_message_id() {
        // A gateway-supplied correlation id within the cap is accepted; the
        // canister doesn't interpret it, only bounds its length.
        let req = envelope_only_request(Some("<abc123@gateway.example>".into()));
        assert!(validate_smtp_request(&req).is_ok());
    }

    #[test]
    fn validate_smtp_request_accepts_message_id_at_cap() {
        // Exactly at the cap must still pass — defines the inclusive bound.
        let req = envelope_only_request(Some("x".repeat(MAX_MESSAGE_ID_BYTES)));
        assert!(validate_smtp_request(&req).is_ok());
    }

    #[test]
    fn validate_smtp_request_rejects_oversize_message_id() {
        // `smtp_request` is open, so an over-long correlation id is refused
        // at the input-bounds layer with a syntax error (555), like every
        // other oversize field.
        let req = envelope_only_request(Some("x".repeat(MAX_MESSAGE_ID_BYTES + 1)));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("message_id"));
    }
}
