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

// --- SMTP error codes (RFC 5321) ---

pub const SMTP_ERR_MAILBOX_UNAVAILABLE: u64 = 550;
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
    pub to: SmtpAddress,
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
    validate_address_bounds(&envelope.to, "Recipient")?;
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

/// Bounds-check both halves of an `SmtpRequest`: the envelope (always)
/// and the message body if present. Per the PoC review, the validate
/// path must accept envelope-only requests for `smtp_request_validate`.
pub fn validate_smtp_request(request: &SmtpRequest) -> Result<(), SmtpResponse> {
    let envelope = request
        .envelope
        .as_ref()
        .ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"))?;
    validate_envelope(envelope)?;
    if let Some(message) = &request.message {
        validate_message(message)?;
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
            to: addr("a", "ok.com"),
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_msg(&resp).contains("Sender user"));
    }

    #[test]
    fn validate_envelope_rejects_oversized_domain() {
        let env = SmtpEnvelope {
            from: addr("ok", "ok.com"),
            to: addr("a", &"d".repeat(MAX_EMAIL_DOMAIN_BYTES + 1)),
        };
        let resp = validate_envelope(&env).unwrap_err();
        assert!(err_msg(&resp).contains("Recipient domain"));
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
                to: addr("recover", "id.ai"),
            }),
            message: None,
            gateway_flags: None,
        };
        assert!(validate_smtp_request(&req).is_ok());
    }
}
