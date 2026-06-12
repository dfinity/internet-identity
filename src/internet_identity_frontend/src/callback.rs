//! OAuth `response_mode=form_post` callback translator.
//!
//! The IdP POSTs `{id_token, state}` (or `{error, error_description, state}`
//! per RFC 6749 §4.1.2.1) as a form body to `/callback`. The POST arrives
//! anonymously — the IdP submits the form, not the user's session — so this
//! handler cannot redeem the JWT itself: the salt + nonce + `caller()`
//! binding requires a signed ingress message from the user's session. It is
//! a transport translator only: parse the form body, return certified HTML
//! that hands the payload to the frontend via `BroadcastChannel` (popup
//! flow) or `sessionStorage` (same-tab flow), where the existing JWT
//! redemption flow takes over.
//!
//! The handler runs in update mode (`http_request` upgrades the POST) so the
//! response is certified via consensus — an uncertified dynamic HTML response
//! would be rejected by the HTTP gateway.

use crate::dynamic_response_headers;
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_http_certification::{HeaderField, HttpRequest, HttpResponse, Method, StatusCode};
use sha2::Digest;
use std::borrow::Cow;

pub const CALLBACK_PATH: &str = "/callback";

/// Upper bound on the accepted form body. The largest legitimate body is an
/// `id_token` near its own cap plus the small `state` and `code` fields;
/// anything bigger is rejected before parsing.
const MAX_BODY_BYTES: usize = 16 * 1024;
const MAX_ID_TOKEN_BYTES: usize = 8192;
const MAX_STATE_BYTES: usize = 64;
const MAX_ERROR_BYTES: usize = 256;
const MAX_ERROR_DESCRIPTION_BYTES: usize = 1024;

/// The single executable inline script of the landing page. Constant so its
/// CSP hash is constant; the per-request payload lives in a non-executing
/// `<script type="application/json">` data block instead.
const CALLBACK_SCRIPT: &str = r#"(function () {
  var data = JSON.parse(document.getElementById("cb").textContent);
  if (sessionStorage.getItem("ii-openid-authorize-state") !== null) {
    // 1-click flow: the authorize page navigated this tab to the IdP after
    // stashing its state marker; stash the payload and resume in-app.
    // `window.opener` can't discriminate the flows here — in the authorize
    // flow this tab is itself a popup opened by the relying party.
    sessionStorage.setItem("ii-openid-callback-data", JSON.stringify(data));
    window.location.replace("/authorize?flow=openid-resume");
  } else {
    // Popup flow: deliver to the tab that opened us and close.
    var channel = new BroadcastChannel("redirect_callback");
    channel.postMessage(data);
    channel.close();
    window.close();
  }
})();"#;

/// Payload delivered to the frontend, mirroring what the fragment used to
/// carry. The error variant exists because RFC 6749 IdPs report failures
/// (e.g. `access_denied`, `unsupported_response_type`) through the same
/// callback; the frontend surfaces them as `OAuthProviderError` after its
/// CSRF state check.
#[derive(Debug, PartialEq, Eq)]
pub enum CallbackPayload {
    Token {
        id_token: String,
        state: String,
    },
    ProviderError {
        error: String,
        error_description: Option<String>,
        state: String,
    },
}

/// Whether the request is the IdP's form_post callback.
pub fn is_callback_post(request: &HttpRequest) -> bool {
    request.method() == Method::POST && request.get_path().is_ok_and(|path| path == CALLBACK_PATH)
}

/// Translate the form_post body into the landing page. Malformed bodies get
/// a static error page; this endpoint is anonymously reachable, so all
/// validation is cheap and bounded.
pub fn handle_form_post_callback(body: &[u8]) -> HttpResponse<'static> {
    match parse_form_post(body) {
        Ok(payload) => render_callback_landing(&payload),
        Err(reason) => render_error_page(reason),
    }
}

/// Parse an `application/x-www-form-urlencoded` body into a
/// [`CallbackPayload`]. Unknown fields (e.g. the `code` that accompanies our
/// `response_type=code id_token` request) are ignored; for duplicate keys the
/// first occurrence wins. All accepted fields are validated against the
/// charset and length the OAuth/JWT specs allow, so the payload can't carry
/// anything able to break out of the JSON data block it is embedded in.
fn parse_form_post(body: &[u8]) -> Result<CallbackPayload, &'static str> {
    if body.len() > MAX_BODY_BYTES {
        return Err("form body too large");
    }
    let mut id_token: Option<Cow<str>> = None;
    let mut state: Option<Cow<str>> = None;
    let mut error: Option<Cow<str>> = None;
    let mut error_description: Option<Cow<str>> = None;
    for (key, value) in form_urlencoded::parse(body) {
        let slot = match key.as_ref() {
            "id_token" => &mut id_token,
            "state" => &mut state,
            "error" => &mut error,
            "error_description" => &mut error_description,
            _ => continue,
        };
        if slot.is_none() {
            *slot = Some(value);
        }
    }

    let Some(state) = state else {
        return Err("missing state");
    };
    if !is_state_charset(&state) || state.len() > MAX_STATE_BYTES {
        return Err("invalid state");
    }

    if let Some(id_token) = id_token {
        if !is_jwt_charset(&id_token) || id_token.len() > MAX_ID_TOKEN_BYTES {
            return Err("invalid id_token");
        }
        return Ok(CallbackPayload::Token {
            id_token: id_token.into_owned(),
            state: state.into_owned(),
        });
    }

    if let Some(error) = error {
        if !is_rfc6749_charset(&error) || error.is_empty() || error.len() > MAX_ERROR_BYTES {
            return Err("invalid error");
        }
        let error_description = match error_description {
            Some(description) => {
                if !is_rfc6749_charset(&description)
                    || description.len() > MAX_ERROR_DESCRIPTION_BYTES
                {
                    return Err("invalid error_description");
                }
                Some(description.into_owned())
            }
            None => None,
        };
        return Ok(CallbackPayload::ProviderError {
            error: error.into_owned(),
            error_description,
            state: state.into_owned(),
        });
    }

    Err("missing id_token")
}

/// JWT compact serialization: base64url segments joined by `.` (plus `=`
/// padding some encoders emit).
fn is_jwt_charset(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'=' | b'.' | b'-'))
}

/// The frontend generates `state` as unpadded base64url; accept padding for
/// robustness.
fn is_state_charset(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-' | b'='))
}

/// RFC 6749 `error` / `error_description` charset: printable ASCII except
/// `"` (0x22) and `\` (0x5C).
fn is_rfc6749_charset(value: &str) -> bool {
    value
        .bytes()
        .all(|byte| matches!(byte, 0x20..=0x21 | 0x23..=0x5B | 0x5D..=0x7E))
}

/// CSP hash of [`CALLBACK_SCRIPT`], in the same `sha384-<base64>` form the
/// asset pipeline pins inline scripts with.
pub fn callback_script_hash() -> String {
    let hash = sha2::Sha384::digest(CALLBACK_SCRIPT.as_bytes());
    format!("sha384-{}", BASE64.encode(hash))
}

/// Serialize the payload for embedding in the JSON data block. `serde_json`
/// escapes `"` and `\`; `<`, `>` and `&` are additionally escaped so the
/// serialized form can never contain `</script` even if a validation bug
/// lets a hostile value through.
fn payload_json(payload: &CallbackPayload) -> String {
    let json = match payload {
        CallbackPayload::Token { id_token, state } => serde_json::json!({
            "id_token": id_token,
            "state": state,
        }),
        CallbackPayload::ProviderError {
            error,
            error_description,
            state,
        } => serde_json::json!({
            "error": error,
            "error_description": error_description,
            "state": state,
        }),
    };
    json.to_string()
        .replace('&', "\\u0026")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e")
}

/// The certified HTML page the IdP receives as the POST response and the
/// browser renders inside the popup (or tab). Its inline script delivers the
/// payload to the frontend and is hash-pinned in the response CSP.
fn render_callback_landing(payload: &CallbackPayload) -> HttpResponse<'static> {
    let json = payload_json(payload);
    let html = format!(
        r#"<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Internet Identity</title>
</head>
<body>
<script type="application/json" id="cb">{json}</script>
<script>{CALLBACK_SCRIPT}</script>
</body>
</html>"#
    );
    // Scope the CSP to this page instead of inheriting the SPA's permissive
    // `script-src 'self' 'unsafe-inline' 'unsafe-eval'` (needed for SvelteKit
    // + agent-js wasm). This page runs exactly one known inline script, so
    // pinning to its hash with no `unsafe-inline`/`unsafe-eval`/`'self'`
    // fallback makes the hash genuinely load-bearing rather than relying on
    // the CSP3 rule that a hash makes browsers ignore `unsafe-inline`.
    let csp = format!(
        "default-src 'none'; script-src '{}'; base-uri 'none'; frame-ancestors 'none'",
        callback_script_hash()
    );
    html_response(StatusCode::OK, html, csp)
}

/// Static page for bodies that can't be translated. `reason` is always one
/// of the fixed strings from [`parse_form_post`], never attacker-controlled.
fn render_error_page(reason: &str) -> HttpResponse<'static> {
    let html = format!(
        r#"<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Internet Identity</title>
</head>
<body>
<p>Sign-in could not be completed: {reason}. Close this window and try again.</p>
</body>
</html>"#
    );
    // No inline script on the error page, so deny scripts entirely.
    html_response(
        StatusCode::BAD_REQUEST,
        html,
        "default-src 'none'; base-uri 'none'; frame-ancestors 'none'".to_string(),
    )
}

fn html_response(
    status_code: StatusCode,
    html: String,
    content_security_policy: String,
) -> HttpResponse<'static> {
    // Take the shared security headers, then swap in this page's own CSP in
    // place of the SPA-wide one (see `render_callback_landing`).
    let mut headers: Vec<HeaderField> = dynamic_response_headers(vec![
        ("content-type".to_string(), "text/html".to_string()),
        // The payload is single-use and session-bound; never cache it.
        ("cache-control".to_string(), "no-store".to_string()),
    ])
    .into_iter()
    .filter(|(name, _)| !name.eq_ignore_ascii_case("content-security-policy"))
    .collect();
    headers.push((
        "Content-Security-Policy".to_string(),
        content_security_policy,
    ));
    HttpResponse::builder()
        .with_status_code(status_code)
        .with_headers(headers)
        .with_body(html.into_bytes())
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn encode_form(pairs: &[(&str, &str)]) -> Vec<u8> {
        let mut serializer = form_urlencoded::Serializer::new(String::new());
        for (key, value) in pairs {
            serializer.append_pair(key, value);
        }
        serializer.finish().into_bytes()
    }

    const ID_TOKEN: &str = "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJ0ZXN0In0.c2lnbmF0dXJl";
    const STATE: &str = "Y2FsbGJhY2stc3RhdGU";

    #[test]
    fn parses_token_payload_and_ignores_unknown_fields() {
        let body = encode_form(&[
            ("code", "4/abc-def"),
            ("id_token", ID_TOKEN),
            ("state", STATE),
            ("session_state", "ignored"),
        ]);
        assert_eq!(
            parse_form_post(&body).unwrap(),
            CallbackPayload::Token {
                id_token: ID_TOKEN.to_string(),
                state: STATE.to_string(),
            }
        );
    }

    #[test]
    fn parses_provider_error_payload() {
        let body = encode_form(&[
            ("error", "unsupported_response_type"),
            (
                "error_description",
                "The response type is not supported by the authorization server.",
            ),
            ("state", STATE),
        ]);
        assert_eq!(
            parse_form_post(&body).unwrap(),
            CallbackPayload::ProviderError {
                error: "unsupported_response_type".to_string(),
                error_description: Some(
                    "The response type is not supported by the authorization server.".to_string()
                ),
                state: STATE.to_string(),
            }
        );
    }

    #[test]
    fn parses_provider_error_without_description() {
        let body = encode_form(&[("error", "access_denied"), ("state", STATE)]);
        assert_eq!(
            parse_form_post(&body).unwrap(),
            CallbackPayload::ProviderError {
                error: "access_denied".to_string(),
                error_description: None,
                state: STATE.to_string(),
            }
        );
    }

    #[test]
    fn token_wins_over_error_when_both_present() {
        // A spec-violating body carrying both: treat as success, matching the
        // frontend's order of checking the token first only after the state.
        let body = encode_form(&[
            ("id_token", ID_TOKEN),
            ("error", "access_denied"),
            ("state", STATE),
        ]);
        assert!(matches!(
            parse_form_post(&body).unwrap(),
            CallbackPayload::Token { .. }
        ));
    }

    #[test]
    fn first_occurrence_wins_for_duplicate_keys() {
        let body = encode_form(&[
            ("id_token", ID_TOKEN),
            ("id_token", "second.token.ignored"),
            ("state", STATE),
        ]);
        assert_eq!(
            parse_form_post(&body).unwrap(),
            CallbackPayload::Token {
                id_token: ID_TOKEN.to_string(),
                state: STATE.to_string(),
            }
        );
    }

    #[test]
    fn rejects_missing_state() {
        let body = encode_form(&[("id_token", ID_TOKEN)]);
        assert_eq!(parse_form_post(&body), Err("missing state"));
    }

    #[test]
    fn rejects_missing_token_and_error() {
        let body = encode_form(&[("state", STATE), ("code", "4/abc")]);
        assert_eq!(parse_form_post(&body), Err("missing id_token"));
    }

    #[test]
    fn rejects_invalid_token_charset() {
        let body = encode_form(&[("id_token", "ey<script>"), ("state", STATE)]);
        assert_eq!(parse_form_post(&body), Err("invalid id_token"));
    }

    #[test]
    fn rejects_oversized_token() {
        let token = "a".repeat(MAX_ID_TOKEN_BYTES + 1);
        let body = encode_form(&[("id_token", &token), ("state", STATE)]);
        assert_eq!(parse_form_post(&body), Err("invalid id_token"));
    }

    #[test]
    fn rejects_invalid_state_charset() {
        let body = encode_form(&[("id_token", ID_TOKEN), ("state", "bad state!")]);
        assert_eq!(parse_form_post(&body), Err("invalid state"));
    }

    #[test]
    fn rejects_oversized_state() {
        let state = "a".repeat(MAX_STATE_BYTES + 1);
        let body = encode_form(&[("id_token", ID_TOKEN), ("state", &state)]);
        assert_eq!(parse_form_post(&body), Err("invalid state"));
    }

    #[test]
    fn rejects_error_with_forbidden_charset() {
        let body = encode_form(&[("error", "access\"denied"), ("state", STATE)]);
        assert_eq!(parse_form_post(&body), Err("invalid error"));
    }

    #[test]
    fn rejects_oversized_error_description() {
        let description = "a".repeat(MAX_ERROR_DESCRIPTION_BYTES + 1);
        let body = encode_form(&[
            ("error", "access_denied"),
            ("error_description", &description),
            ("state", STATE),
        ]);
        assert_eq!(parse_form_post(&body), Err("invalid error_description"));
    }

    #[test]
    fn rejects_oversized_body_before_parsing() {
        let token = "a".repeat(MAX_BODY_BYTES);
        let body = encode_form(&[("id_token", &token), ("state", STATE)]);
        assert_eq!(parse_form_post(&body), Err("form body too large"));
    }

    #[test]
    fn payload_json_escapes_html_significant_characters() {
        // The charset checks make these unreachable from a real form body;
        // this pins the defense-in-depth escaping of the embedding itself.
        let payload = CallbackPayload::ProviderError {
            error: "access_denied".to_string(),
            error_description: Some("</script><b>&amp;".to_string()),
            state: STATE.to_string(),
        };
        let json = payload_json(&payload);
        assert!(!json.contains('<'));
        assert!(!json.contains('>'));
        assert!(!json.contains('&'));
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(
            parsed["error_description"].as_str().unwrap(),
            "</script><b>&amp;"
        );
    }

    #[test]
    fn payload_json_round_trips_token_payload() {
        let payload = CallbackPayload::Token {
            id_token: ID_TOKEN.to_string(),
            state: STATE.to_string(),
        };
        let parsed: serde_json::Value = serde_json::from_str(&payload_json(&payload)).unwrap();
        assert_eq!(parsed["id_token"].as_str().unwrap(), ID_TOKEN);
        assert_eq!(parsed["state"].as_str().unwrap(), STATE);
    }

    #[test]
    fn landing_page_embeds_payload_and_pins_script_hash() {
        let response =
            handle_form_post_callback(&encode_form(&[("id_token", ID_TOKEN), ("state", STATE)]));
        assert_eq!(response.status_code(), StatusCode::OK);
        let body = std::str::from_utf8(response.body()).unwrap();
        assert!(body.contains(ID_TOKEN));
        assert!(body.contains(CALLBACK_SCRIPT));
        let csp_headers: Vec<&str> = response
            .headers()
            .iter()
            .filter(|(name, _)| name.eq_ignore_ascii_case("content-security-policy"))
            .map(|(_, value)| value.as_str())
            .collect();
        // Exactly one CSP (the SPA-wide one is replaced, not appended), and it
        // pins the inline script by hash with no permissive fallback — so the
        // hash actually governs execution.
        assert_eq!(csp_headers.len(), 1);
        let csp = csp_headers[0];
        assert!(csp.contains(&format!("script-src '{}'", callback_script_hash())));
        assert!(!csp.contains("unsafe-inline"));
        assert!(!csp.contains("unsafe-eval"));
        let cache_control = response
            .headers()
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("cache-control"))
            .map(|(_, value)| value.as_str())
            .unwrap();
        assert_eq!(cache_control, "no-store");
    }

    #[test]
    fn malformed_body_gets_error_page() {
        let response = handle_form_post_callback(b"not&a=valid#form");
        assert_eq!(response.status_code(), StatusCode::BAD_REQUEST);
        let body = std::str::from_utf8(response.body()).unwrap();
        assert!(body.contains("Sign-in could not be completed"));
        assert!(!body.contains("<script>"));
    }
}
