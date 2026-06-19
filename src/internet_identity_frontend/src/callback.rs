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
use ic_http_certification::{HttpRequest, HttpResponse, Method, StatusCode};
use serde::Serialize;
use sha2::Digest;
use std::borrow::Cow;

pub const CALLBACK_PATH: &str = "/callback";

/// In-SPA route the browser is redirected to when the body can't be
/// translated. The page renders the styled, localized error UI.
const CALLBACK_ERROR_PATH: &str = "/callback-error";

/// Field length caps. OAuth/OIDC define no byte limits for these fields, so
/// the bounds are practical ceilings chosen to fit any legitimate value while
/// keeping the anonymously-reachable parser's work bounded:
/// - `id_token`: a JWT. Real OIDC id_tokens are ~1-2 KiB; 8 KiB clears even
///   token-heavy providers and matches the common 8 KiB ceiling servers put on
///   a single header/line.
/// - `state`: we generate it ourselves as unpadded base64url (see
///   [`is_state_charset`]); 64 bytes covers our own value with margin.
/// - `error` / `error_description`: RFC 6749 §4.1.2.1 text. 256 / 1024 bytes
///   hold any realistic provider message.
const MAX_ID_TOKEN_BYTES: usize = 8192;
const MAX_STATE_BYTES: usize = 64;
const MAX_ERROR_BYTES: usize = 256;
const MAX_ERROR_DESCRIPTION_BYTES: usize = 1024;

/// Upper bound on the whole form body, checked before parsing. A legitimate
/// body is dominated by the `id_token`; doubling [`MAX_ID_TOKEN_BYTES`] leaves
/// ample room for the remaining small fields without admitting an unbounded
/// body.
const MAX_BODY_BYTES: usize = 2 * MAX_ID_TOKEN_BYTES;

/// The single executable inline script of the landing page. Constant so its
/// CSP hash is constant; the per-request payload lives in a non-executing
/// `<script type="application/json">` data block instead.
const CALLBACK_SCRIPT: &str = r#"(function () {
  var data = JSON.parse(document.getElementById("callback-payload").textContent);
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

/// Payload delivered to the frontend: either the token or the IdP's error
/// report, plus the CSRF `state` in both cases. The error variant exists
/// because RFC 6749 IdPs report failures (e.g. `access_denied`,
/// `unsupported_response_type`) through the same callback; the frontend
/// surfaces them as `OAuthProviderError` after its CSRF state check.
#[derive(Debug, PartialEq, Eq, Serialize)]
// Untagged so each variant serializes to a flat object of just its fields —
// `{id_token, state}` or `{error, error_description, state}` — which is the
// shape the frontend's `CallbackPayloadSchema` expects. `error_description`
// is serialized as JSON `null` when absent (not omitted), which that schema
// accepts and normalizes to `undefined`.
#[serde(untagged)]
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

/// Translate the form_post body into the landing page. Malformed bodies are
/// redirected to the in-SPA error page; this endpoint is anonymously
/// reachable, so all validation is cheap and bounded.
pub fn handle_form_post_callback(body: &[u8]) -> HttpResponse<'static> {
    match parse_form_post(body) {
        Ok(payload) => render_callback_landing(&payload),
        Err(reason) => redirect_to_error_page(&reason),
    }
}

/// Parse an `application/x-www-form-urlencoded` body into a
/// [`CallbackPayload`]. Unknown fields (e.g. the `code` that accompanies our
/// `response_type=code id_token` request) are ignored; for duplicate keys the
/// first occurrence wins.
///
/// This endpoint is reachable anonymously by anyone (the IdP submits the form
/// without the user's session), so the body is untrusted and the parsed values
/// are echoed straight into an HTML page. Two concrete risks are closed here:
///
/// - HTML/script-context breakout: the values land inside a
///   `<script type="application/json">` data block (see
///   [`render_callback_landing`]). A value containing `</script>` or angle
///   brackets could close that block early and inject markup. Restricting each
///   field to its OAuth/JWT charset rejects `<`, `>`, `"` and `\` outright;
///   [`payload_json`] then escapes them again as defence in depth.
/// - Unbounded work: every field has a length cap and the whole body is
///   size-checked before parsing, so a hostile client can't force large
///   allocations on this free-to-call endpoint.
///
/// Validation here is purely about safe transport — authenticity of the
/// `id_token` is still established later by the signed-ingress JWT redemption.
fn parse_form_post(body: &[u8]) -> Result<CallbackPayload, String> {
    if body.len() > MAX_BODY_BYTES {
        return Err(too_large("form body", body.len(), MAX_BODY_BYTES));
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

    // Precondition: every callback carries a valid `state`.
    let Some(state) = state else {
        return Err("missing state".to_string());
    };
    if !is_state_charset(&state) {
        return Err("state is not valid base64url".to_string());
    }
    if state.len() > MAX_STATE_BYTES {
        return Err(too_large("state", state.len(), MAX_STATE_BYTES));
    }
    let state = state.into_owned();

    // A token is the success outcome and wins over a co-present error report
    // (a spec-violating body); validate it and we're done.
    if let Some(id_token) = id_token {
        if !is_jwt_charset(&id_token) {
            return Err("id_token is not valid JWT compact serialization".to_string());
        }
        if id_token.len() > MAX_ID_TOKEN_BYTES {
            return Err(too_large("id_token", id_token.len(), MAX_ID_TOKEN_BYTES));
        }
        return Ok(CallbackPayload::Token {
            id_token: id_token.into_owned(),
            state,
        });
    }

    // Precondition: with no token, the body must carry an error report to be
    // a usable callback at all.
    let Some(error) = error else {
        return Err("missing id_token".to_string());
    };
    if error.is_empty() {
        return Err("empty error".to_string());
    }
    if !is_rfc6749_charset(&error) {
        return Err("error contains characters outside the RFC 6749 charset".to_string());
    }
    if error.len() > MAX_ERROR_BYTES {
        return Err(too_large("error", error.len(), MAX_ERROR_BYTES));
    }
    let error_description = match error_description {
        Some(description) => {
            if !is_rfc6749_charset(&description) {
                return Err(
                    "error_description contains characters outside the RFC 6749 charset"
                        .to_string(),
                );
            }
            if description.len() > MAX_ERROR_DESCRIPTION_BYTES {
                return Err(too_large(
                    "error_description",
                    description.len(),
                    MAX_ERROR_DESCRIPTION_BYTES,
                ));
            }
            Some(description.into_owned())
        }
        None => None,
    };
    Ok(CallbackPayload::ProviderError {
        error: error.into_owned(),
        error_description,
        state,
    })
}

/// Error string for a field that exceeds its byte cap, naming the observed
/// size and the limit so a misconfigured IdP is easy to diagnose.
fn too_large(field: &str, observed: usize, limit: usize) -> String {
    format!("{field} too large: {observed} bytes exceeds limit of {limit}")
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
fn payload_json(payload: &CallbackPayload) -> Result<String, serde_json::Error> {
    Ok(serde_json::to_string(payload)?
        .replace('&', "\\u0026")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e"))
}

/// The certified HTML page the IdP receives as the POST response and the
/// browser renders inside the popup (or tab). Its inline script delivers the
/// payload to the frontend and is hash-pinned in the response CSP.
fn render_callback_landing(payload: &CallbackPayload) -> HttpResponse<'static> {
    // The payload is owned strings, so serialization cannot fail in practice;
    // fall back to the error page rather than trapping if it ever does.
    let Ok(json) = payload_json(payload) else {
        return redirect_to_error_page("could not encode callback");
    };
    let html = format!(
        r#"<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Internet Identity</title>
</head>
<body>
<script type="application/json" id="callback-payload">{json}</script>
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
    let headers = dynamic_response_headers(
        Some(csp),
        vec![
            ("content-type".to_string(), "text/html".to_string()),
            // The payload is single-use and session-bound; never cache it.
            ("cache-control".to_string(), "no-store".to_string()),
        ],
    );
    HttpResponse::builder()
        .with_status_code(StatusCode::OK)
        .with_headers(headers)
        .with_body(html.into_bytes())
        .build()
}

/// Redirect the browser to the in-SPA error page for bodies that can't be
/// translated, so the styled, localized error UI is rendered by the SPA on the
/// follow-up GET. `reason` is always one of the fixed strings from
/// [`parse_form_post`], never attacker-controlled; it travels as a query param
/// to aid debugging a misconfigured IdP. The bodyless redirect keeps the
/// SPA-wide CSP, so no override is needed.
fn redirect_to_error_page(reason: &str) -> HttpResponse<'static> {
    let reason_param: String = form_urlencoded::byte_serialize(reason.as_bytes()).collect();
    let location = format!("{CALLBACK_ERROR_PATH}?reason={reason_param}");
    let headers = dynamic_response_headers(
        None,
        vec![
            ("location".to_string(), location),
            // The redirect is single-use and session-bound; never cache it.
            ("cache-control".to_string(), "no-store".to_string()),
        ],
    );
    HttpResponse::builder()
        .with_status_code(StatusCode::SEE_OTHER)
        .with_headers(headers)
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

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
        assert_eq!(parse_form_post(&body).unwrap_err(), "missing state");
    }

    #[test]
    fn rejects_missing_token_and_error() {
        let body = encode_form(&[("state", STATE), ("code", "4/abc")]);
        assert_eq!(parse_form_post(&body).unwrap_err(), "missing id_token");
    }

    #[test]
    fn rejects_invalid_token_charset() {
        let body = encode_form(&[("id_token", "ey<script>"), ("state", STATE)]);
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            "id_token is not valid JWT compact serialization"
        );
    }

    #[test]
    fn rejects_oversized_token() {
        let token = "a".repeat(MAX_ID_TOKEN_BYTES + 1);
        let body = encode_form(&[("id_token", &token), ("state", STATE)]);
        // The message names the observed size and the limit for debugging.
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            format!(
                "id_token too large: {} bytes exceeds limit of {MAX_ID_TOKEN_BYTES}",
                MAX_ID_TOKEN_BYTES + 1
            )
        );
    }

    #[test]
    fn rejects_invalid_state_charset() {
        let body = encode_form(&[("id_token", ID_TOKEN), ("state", "bad state!")]);
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            "state is not valid base64url"
        );
    }

    #[test]
    fn rejects_oversized_state() {
        let state = "a".repeat(MAX_STATE_BYTES + 1);
        let body = encode_form(&[("id_token", ID_TOKEN), ("state", &state)]);
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            format!(
                "state too large: {} bytes exceeds limit of {MAX_STATE_BYTES}",
                MAX_STATE_BYTES + 1
            )
        );
    }

    #[test]
    fn rejects_error_with_forbidden_charset() {
        let body = encode_form(&[("error", "access\"denied"), ("state", STATE)]);
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            "error contains characters outside the RFC 6749 charset"
        );
    }

    #[test]
    fn rejects_oversized_error_description() {
        let description = "a".repeat(MAX_ERROR_DESCRIPTION_BYTES + 1);
        let body = encode_form(&[
            ("error", "access_denied"),
            ("error_description", &description),
            ("state", STATE),
        ]);
        assert_eq!(
            parse_form_post(&body).unwrap_err(),
            format!(
                "error_description too large: {} bytes exceeds limit of {MAX_ERROR_DESCRIPTION_BYTES}",
                MAX_ERROR_DESCRIPTION_BYTES + 1
            )
        );
    }

    #[test]
    fn rejects_oversized_body_before_parsing() {
        let token = "a".repeat(MAX_BODY_BYTES);
        let body = encode_form(&[("id_token", &token), ("state", STATE)]);
        // Rejected on total body size before any field parsing.
        assert!(parse_form_post(&body)
            .unwrap_err()
            .starts_with("form body too large:"));
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
        let json = payload_json(&payload).unwrap();
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
        let parsed: serde_json::Value =
            serde_json::from_str(&payload_json(&payload).unwrap()).unwrap();
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
    fn malformed_body_redirects_to_error_page() {
        let response = handle_form_post_callback(b"not&a=valid#form");
        assert_eq!(response.status_code(), StatusCode::SEE_OTHER);
        let location = response
            .headers()
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("location"))
            .map(|(_, value)| value.as_str())
            .expect("redirect must carry a Location header");
        // The reason is carried as a query param for debugging; it is always
        // one of the fixed parser strings, percent-encoded.
        assert!(location.starts_with("/callback-error?reason="));
        assert!(response.body().is_empty());
    }

    // Characters that could break out of the `<script type="application/json">`
    // data block or the JSON string the payload sits in.
    const EMBEDDING_DANGEROUS: [char; 5] = ['<', '>', '"', '\\', '&'];

    proptest! {
        // The JWT charset is the front-line guard for the embedded `id_token`,
        // so anything it accepts must be free of every embedding-dangerous
        // character (the JSON escaping is only defence in depth for it).
        #[test]
        fn accepted_jwt_charset_is_embedding_safe(value in ".*") {
            if is_jwt_charset(&value) {
                prop_assert!(!value.contains(EMBEDDING_DANGEROUS));
            }
        }

        // Same guarantee for the `state` charset.
        #[test]
        fn accepted_state_charset_is_embedding_safe(value in ".*") {
            if is_state_charset(&value) {
                prop_assert!(!value.contains(EMBEDDING_DANGEROUS));
            }
        }

        // The RFC 6749 charset deliberately permits `<`, `>` and `&` (escaped
        // later by `payload_json`), but must always reject the JSON string
        // delimiters and any control or non-ASCII byte.
        #[test]
        fn accepted_rfc6749_charset_excludes_json_delimiters_and_control(value in ".*") {
            if is_rfc6749_charset(&value) {
                prop_assert!(!value.contains(['"', '\\']));
                prop_assert!(value.bytes().all(|byte| (0x20..=0x7e).contains(&byte)));
            }
        }

        // End to end, whatever the field values, the serialized JSON block can
        // never contain a raw `<`, `>` or `&` — so it cannot close the
        // surrounding `<script>` tag — and it still round-trips to the inputs.
        // This is the property that actually closes `</script>` breakout for
        // the error fields, which the charset above lets `<`/`>`/`&` through.
        #[test]
        fn provider_error_json_is_embedding_safe_and_round_trips(
            error in ".*",
            error_description in proptest::option::of(".*"),
            state in ".*",
        ) {
            let payload = CallbackPayload::ProviderError {
                error: error.clone(),
                error_description: error_description.clone(),
                state: state.clone(),
            };
            let json = payload_json(&payload).unwrap();
            prop_assert!(!json.contains(['<', '>', '&']));
            let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
            prop_assert_eq!(parsed["error"].as_str().unwrap(), &error);
            prop_assert_eq!(parsed["state"].as_str().unwrap(), &state);
            match error_description {
                Some(description) => {
                    prop_assert_eq!(parsed["error_description"].as_str().unwrap(), &description)
                }
                None => prop_assert!(parsed["error_description"].is_null()),
            }
        }
    }
}
