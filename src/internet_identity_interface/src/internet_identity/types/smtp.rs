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
pub const MAX_HEADERS: usize = 30;
pub const MAX_HEADER_NAME_BYTES: usize = 256;
pub const MAX_HEADER_VALUE_BYTES: usize = 8_192;
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
    Ok(())
}

/// Validate both halves of an `SmtpRequest`: the envelope (always)
/// and the message body if present. Per the PoC review, the validate
/// path must accept envelope-only requests for `smtp_request_validate`.
///
/// Covers two layers in one pass so callers don't have to compose them
/// themselves:
///
/// 1. **Bounds** — address/header/body/recipient-count caps, plus
///    envelope presence.
/// 2. **RFC 5322 §3.6 header uniqueness** (only when a message is
///    present): `From`/`Date`/`Subject` exactly once,
///    `Sender`/`Reply-To`/`To`/`Cc`/`Bcc`/`Message-ID`/`In-Reply-To`/
///    `References` at most once, `DKIM-Signature` at least once.
pub fn validate_smtp_request(request: &SmtpRequest) -> Result<(), SmtpResponse> {
    let envelope = request
        .envelope
        .as_ref()
        .ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"))?;
    validate_envelope(envelope)?;
    if let Some(message) = &request.message {
        validate_message(message)?;
        validate_message_header_uniqueness(message)?;
    }
    Ok(())
}

/// Header-count constraint for the RFC 5322 §3.6 well-formedness pass.
/// Private to this module — only `validate_message_header_uniqueness`
/// needs the type.
#[derive(Clone, Copy)]
struct HeaderCount {
    min: usize,
    max: Option<usize>,
}

impl HeaderCount {
    const EXACTLY_ONE: Self = Self {
        min: 1,
        max: Some(1),
    };
    const AT_MOST_ONE: Self = Self {
        min: 0,
        max: Some(1),
    };
    const AT_LEAST_ONE: Self = Self { min: 1, max: None };

    fn accepts(self, count: usize) -> bool {
        count >= self.min && self.max.map(|m| count <= m).unwrap_or(true)
    }
}

impl std::fmt::Display for HeaderCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.min, self.max) {
            (1, Some(1)) => write!(f, "exactly once"),
            (0, Some(1)) => write!(f, "at most once"),
            (1, None) => write!(f, "at least once"),
            (min, Some(max)) => write!(f, "between {min} and {max} times"),
            (min, None) => write!(f, "at least {min} times"),
        }
    }
}

fn validate_message_header_uniqueness(message: &SmtpMessage) -> Result<(), SmtpResponse> {
    check_header_count(&message.headers, "From", HeaderCount::EXACTLY_ONE)?;
    check_header_count(&message.headers, "Date", HeaderCount::EXACTLY_ONE)?;
    check_header_count(&message.headers, "Subject", HeaderCount::EXACTLY_ONE)?;
    for name in [
        "Sender",
        "Reply-To",
        "To",
        "Cc",
        "Bcc",
        "Message-ID",
        "In-Reply-To",
        "References",
    ] {
        check_header_count(&message.headers, name, HeaderCount::AT_MOST_ONE)?;
    }
    check_header_count(&message.headers, "DKIM-Signature", HeaderCount::AT_LEAST_ONE)?;
    Ok(())
}

fn check_header_count(
    headers: &[SmtpHeader],
    name: &'static str,
    expected: HeaderCount,
) -> Result<(), SmtpResponse> {
    let found = headers
        .iter()
        .filter(|h| h.name.eq_ignore_ascii_case(name))
        .count();
    if expected.accepts(found) {
        Ok(())
    } else {
        Err(smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!(
                "RFC 5322 §3.6 violation: header '{name}' must appear {expected} (found {found})"
            ),
        ))
    }
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
        };
        let resp = validate_smtp_request(&req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("envelope"));
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
        };
        assert!(validate_smtp_request(&req).is_ok());
    }

    // --- RFC 5322 §3.6 header uniqueness ---

    fn header(name: &str, value: &str) -> SmtpHeader {
        SmtpHeader {
            name: name.into(),
            value: value.into(),
        }
    }

    /// Build a minimal RFC §3.6-compliant `SmtpRequest` for the
    /// uniqueness tests. Extra headers can be appended on top of the
    /// returned value.
    fn well_formed_request() -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    header("From", "alice@example.com"),
                    header("Date", "Mon, 1 Jan 2024 00:00:00 +0000"),
                    header("Subject", "II-Recovery-deadbeefcafe1234"),
                    header(
                        "DKIM-Signature",
                        "v=1; a=rsa-sha256; d=example.com; s=mail; \
                         c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
                    ),
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        }
    }

    #[test]
    fn well_formed_request_passes_validation() {
        assert!(validate_smtp_request(&well_formed_request()).is_ok());
    }

    #[test]
    fn rejects_duplicate_from() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("From", "evil@elsewhere.example"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("'From'"));
        assert!(err_msg(&resp).contains("exactly once"));
        assert!(err_msg(&resp).contains("found 2"));
    }

    #[test]
    fn rejects_missing_from() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("From"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'From'"));
        assert!(err_msg(&resp).contains("found 0"));
    }

    #[test]
    fn rejects_duplicate_date() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Date", "Tue, 2 Jan 2024 00:00:00 +0000"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'Date'"));
    }

    #[test]
    fn rejects_duplicate_subject() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Subject", "another"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'Subject'"));
    }

    #[test]
    fn rejects_duplicate_message_id() {
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers.push(header("Message-ID", "<a@example.com>"));
        msg.headers.push(header("Message-ID", "<b@example.com>"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'Message-ID'"));
        assert!(err_msg(&resp).contains("at most once"));
    }

    #[test]
    fn at_most_once_headers_accept_zero_or_one() {
        // Reply-To absent is fine.
        assert!(validate_smtp_request(&well_formed_request()).is_ok());
        // Reply-To once is fine.
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Reply-To", "alice-reply@example.com"));
        assert!(validate_smtp_request(&req).is_ok());
    }

    #[test]
    fn rejects_missing_dkim_signature_when_message_present() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'DKIM-Signature'"));
        assert!(err_msg(&resp).contains("at least once"));
    }

    #[test]
    fn header_uniqueness_check_is_case_insensitive() {
        let mut req = well_formed_request();
        // Lowercase "from" must still trip the duplicate-From check.
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("from", "another@example.com"));
        let resp = validate_smtp_request(&req).unwrap_err();
        assert!(err_msg(&resp).contains("'From'"));
    }
}
