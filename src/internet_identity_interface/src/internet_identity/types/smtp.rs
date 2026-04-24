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
pub const MAX_EMAILS_PER_USER: usize = 10;

// --- SMTP error codes ---

const SMTP_ERR_MAILBOX_UNAVAILABLE: u64 = 550;
const SMTP_ERR_SYNTAX_ERROR: u64 = 555;

// --- API types (Candid) ---

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpHeader {
    pub name: String,
    pub value: String,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpMessage {
    pub headers: Vec<SmtpHeader>,
    pub body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpAddress {
    pub user: String,
    pub domain: String,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpEnvelope {
    pub from: SmtpAddress,
    pub to: SmtpAddress,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpRequest {
    pub message: Option<SmtpMessage>,
    pub envelope: Option<SmtpEnvelope>,
    pub gateway_flags: Option<Vec<String>>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SmtpRequestError {
    pub code: u64,
    pub message: String,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum SmtpResponse {
    Ok {},
    Err(SmtpRequestError),
}

// --- DKIM verification ---

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum DkimCheckName {
    DkimSignaturePresent,
    SignatureParsed,
    AlgorithmSupported,
    RequiredHeadersSigned,
    BodyHashValid,
    PublicKeyFetched,
    SignatureValid,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum DkimCheckStatus {
    Pass,
    Fail,
    Skipped,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct DkimCheck {
    pub name: DkimCheckName,
    pub status: DkimCheckStatus,
    pub detail: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum DkimVerificationStatus {
    Verified { checks: Vec<DkimCheck> },
    Unverified { checks: Vec<DkimCheck> },
    Pending,
}

// --- Postbox query types ---

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct PostboxEmail {
    pub sender: String,
    pub recipient: String,
    pub subject: String,
    pub body: String,
    pub dkim_status: Option<DkimVerificationStatus>,
}

// --- Validated internal types ---

#[derive(Clone, Debug)]
pub struct ValidatedSmtpRequest {
    pub anchor_number: u64,
    pub sender: String,
    pub recipient: String,
    pub subject: String,
    pub body: String,
    pub headers: Vec<SmtpHeader>,
    pub raw_body: Vec<u8>,
}

// --- Helpers ---

fn smtp_err(code: u64, message: impl Into<String>) -> SmtpResponse {
    SmtpResponse::Err(SmtpRequestError {
        code,
        message: message.into(),
    })
}

/// Renders an address as `user@domain` with both parts lowercased so it can
/// be used as a stable stable-map key. Envelope validation is already
/// case-insensitive (`eq_ignore_ascii_case`); without canonicalization the
/// same logical mailbox could be stored under multiple keys by varying
/// case, bypassing per-user pruning.
fn format_address(addr: &SmtpAddress) -> String {
    format!(
        "{}@{}",
        addr.user.to_ascii_lowercase(),
        addr.domain.to_ascii_lowercase()
    )
}

fn validate_address_bounds(addr: &SmtpAddress, label: &str) -> Result<(), SmtpResponse> {
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

fn validate_envelope(envelope: &SmtpEnvelope) -> Result<u64, SmtpResponse> {
    validate_address_bounds(&envelope.from, "Sender")?;
    validate_address_bounds(&envelope.to, "Recipient")?;

    let anchor_number = envelope.to.user.parse::<u64>().map_err(|_| {
        smtp_err(
            SMTP_ERR_MAILBOX_UNAVAILABLE,
            "Recipient user must be a valid anchor number",
        )
    })?;

    Ok(anchor_number)
}

fn validate_message(message: &SmtpMessage) -> Result<(), SmtpResponse> {
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

fn extract_subject(headers: &[SmtpHeader]) -> String {
    headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("subject"))
        .map(|h| {
            let mut s = h.value.clone();
            truncate_at_char_boundary(&mut s, MAX_SUBJECT_BYTES);
            s
        })
        .unwrap_or_default()
}

/// `String::truncate` panics when the byte index is mid-codepoint. Subjects
/// with multi-byte UTF-8 characters right around [`MAX_SUBJECT_BYTES`] would
/// crash the canister on a crafted email. Clamp down to the previous char
/// boundary instead, which is safe even for arbitrary input.
fn truncate_at_char_boundary(s: &mut String, max_bytes: usize) {
    if s.len() <= max_bytes {
        return;
    }
    let mut end = max_bytes;
    while end > 0 && !s.is_char_boundary(end) {
        end -= 1;
    }
    s.truncate(end);
}

// --- TryFrom ---

impl TryFrom<SmtpRequest> for ValidatedSmtpRequest {
    type Error = SmtpResponse;

    fn try_from(request: SmtpRequest) -> Result<Self, Self::Error> {
        let envelope = request
            .envelope
            .as_ref()
            .ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"))?;

        let anchor_number = validate_envelope(envelope)?;

        let message = request
            .message
            .as_ref()
            .ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing message"))?;

        validate_message(message)?;

        // `from_utf8_lossy` replaces each invalid byte with U+FFFD (3 bytes),
        // so a body that passed the `MAX_BODY_BYTES` check above on byte
        // length can still expand past the storage bound. Clamp the decoded
        // string to `MAX_BODY_BYTES` on a char boundary so the insertion
        // into stable storage cannot trap.
        let mut body = String::from_utf8_lossy(&message.body).into_owned();
        truncate_at_char_boundary(&mut body, MAX_BODY_BYTES);

        Ok(ValidatedSmtpRequest {
            anchor_number,
            sender: format_address(&envelope.from),
            recipient: format_address(&envelope.to),
            subject: extract_subject(&message.headers),
            headers: message.headers.clone(),
            raw_body: message.body.to_vec(),
            body,
        })
    }
}

/// Validates only the envelope portion of an SMTP request.
/// Used by `smtp_request_validate` when no message is present.
pub fn validate_envelope_only(request: &SmtpRequest) -> Result<(), SmtpResponse> {
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
    use serde_bytes::ByteBuf;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.to_string(),
            domain: domain.to_string(),
        }
    }

    fn envelope(anchor: u64) -> SmtpEnvelope {
        SmtpEnvelope {
            from: addr("sender", "example.com"),
            to: addr(&anchor.to_string(), "id.ai"),
        }
    }

    fn ok_message() -> SmtpMessage {
        SmtpMessage {
            headers: vec![SmtpHeader {
                name: "Subject".into(),
                value: "hello".into(),
            }],
            body: ByteBuf::from(b"body".to_vec()),
        }
    }

    fn err_message(resp: &SmtpResponse) -> &str {
        match resp {
            SmtpResponse::Err(e) => &e.message,
            SmtpResponse::Ok {} => panic!("expected Err, got Ok"),
        }
    }

    fn err_code(resp: &SmtpResponse) -> u64 {
        match resp {
            SmtpResponse::Err(e) => e.code,
            SmtpResponse::Ok {} => panic!("expected Err, got Ok"),
        }
    }

    #[test]
    fn missing_envelope_is_syntax_error() {
        let req = SmtpRequest {
            envelope: None,
            message: Some(ok_message()),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_message(&resp).contains("envelope"));
    }

    #[test]
    fn missing_message_is_syntax_error() {
        let req = SmtpRequest {
            envelope: Some(envelope(42)),
            message: None,
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_message(&resp).contains("message"));
    }

    #[test]
    fn non_numeric_user_is_mailbox_unavailable() {
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("sender", "example.com"),
                to: addr("alice", "id.ai"),
            }),
            message: Some(ok_message()),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_MAILBOX_UNAVAILABLE);
    }

    #[test]
    fn too_many_headers_is_syntax_error() {
        let headers: Vec<SmtpHeader> = (0..MAX_HEADERS + 1)
            .map(|i| SmtpHeader {
                name: format!("X-Header-{i}"),
                value: "v".into(),
            })
            .collect();
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: Some(SmtpMessage {
                headers,
                body: ByteBuf::from(b"".to_vec()),
            }),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
        assert!(err_message(&resp).contains("headers"));
    }

    #[test]
    fn oversize_header_name_is_syntax_error() {
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: Some(SmtpMessage {
                headers: vec![SmtpHeader {
                    name: "X".repeat(MAX_HEADER_NAME_BYTES + 1),
                    value: "v".into(),
                }],
                body: ByteBuf::from(b"".to_vec()),
            }),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
    }

    #[test]
    fn oversize_header_value_is_syntax_error() {
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: Some(SmtpMessage {
                headers: vec![SmtpHeader {
                    name: "X-Big".into(),
                    value: "a".repeat(MAX_HEADER_VALUE_BYTES + 1),
                }],
                body: ByteBuf::from(b"".to_vec()),
            }),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
    }

    #[test]
    fn oversize_body_is_syntax_error() {
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: Some(SmtpMessage {
                headers: vec![],
                body: ByteBuf::from(vec![b'a'; MAX_BODY_BYTES + 1]),
            }),
            gateway_flags: None,
        };
        let resp = ValidatedSmtpRequest::try_from(req).unwrap_err();
        assert_eq!(err_code(&resp), SMTP_ERR_SYNTAX_ERROR);
    }

    #[test]
    fn non_utf8_body_does_not_panic() {
        // A byte sequence that is NOT valid UTF-8 but fits in MAX_BODY_BYTES.
        // `from_utf8_lossy` will replace invalid bytes with U+FFFD (3 bytes
        // each) so the decoded string would otherwise exceed the bound; the
        // truncation helper must keep us safe.
        let body = vec![0xFF_u8; MAX_BODY_BYTES];
        let req = SmtpRequest {
            envelope: Some(envelope(7)),
            message: Some(SmtpMessage {
                headers: vec![],
                body: ByteBuf::from(body),
            }),
            gateway_flags: None,
        };
        let validated = ValidatedSmtpRequest::try_from(req).unwrap();
        assert!(validated.body.len() <= MAX_BODY_BYTES);
    }

    #[test]
    fn multibyte_subject_truncates_on_char_boundary() {
        // Pad "а" (Cyrillic, 2 bytes) up to just past MAX_SUBJECT_BYTES and
        // confirm `extract_subject` doesn't panic and returns a string
        // shorter than the limit (possibly by 1 byte to land on a boundary).
        // ~2 × MAX_SUBJECT_BYTES bytes total.
        let subject: String = "а".repeat(MAX_SUBJECT_BYTES);
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: Some(SmtpMessage {
                headers: vec![SmtpHeader {
                    name: "Subject".into(),
                    value: subject,
                }],
                body: ByteBuf::from(b"".to_vec()),
            }),
            gateway_flags: None,
        };
        let validated = ValidatedSmtpRequest::try_from(req).unwrap();
        assert!(validated.subject.len() <= MAX_SUBJECT_BYTES);
        // A valid String: must be re-encodable as UTF-8 (implicit).
        // Drop bytes if needed so we don't split a codepoint mid-sequence.
        assert!(validated.subject.is_char_boundary(validated.subject.len()));
    }

    #[test]
    fn format_address_lowercases_both_parts() {
        let lower = format_address(&addr("ALICE", "EXAMPLE.COM"));
        assert_eq!(lower, "alice@example.com");
    }

    #[test]
    fn accepted_message_roundtrips() {
        let req = SmtpRequest {
            envelope: Some(envelope(12345)),
            message: Some(ok_message()),
            gateway_flags: None,
        };
        let validated = ValidatedSmtpRequest::try_from(req).unwrap();
        assert_eq!(validated.anchor_number, 12345);
        assert_eq!(validated.recipient, "12345@id.ai");
        assert_eq!(validated.sender, "sender@example.com");
        assert_eq!(validated.subject, "hello");
        assert_eq!(validated.body, "body");
    }

    #[test]
    fn validate_envelope_only_accepts_envelope_alone() {
        let req = SmtpRequest {
            envelope: Some(envelope(1)),
            message: None,
            gateway_flags: None,
        };
        validate_envelope_only(&req).unwrap();
    }

    #[test]
    fn validate_envelope_only_rejects_missing_envelope() {
        let req = SmtpRequest {
            envelope: None,
            message: None,
            gateway_flags: None,
        };
        let err = validate_envelope_only(&req).unwrap_err();
        assert_eq!(err_code(&err), SMTP_ERR_SYNTAX_ERROR);
    }
}
