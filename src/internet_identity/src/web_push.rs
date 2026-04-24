//! Web Push notifications for postbox emails.
//!
//! Sends VAPID-signed (RFC 8292) HTTP POSTs to push service endpoints when a
//! new email arrives. V1 sends empty payloads only — the service worker shows
//! a generic "you have a new email" notification and the user navigates to the
//! postbox. Payload encryption (RFC 8291) is deferred.
//!
//! The canister generates and stores an ECDSA P-256 VAPID key pair lazily on
//! first subscription. Subscriptions are stored per anchor.
//!
//! **Future optimization:** Push messages are self-authenticating via VAPID,
//! so a batch of outgoing pushes could be forwarded through a single non-IC
//! relay in one outcall rather than one outcall per subscription. Also, once
//! the project upgrades `ic-cdk` past a version that exposes the
//! `is_replicated: false` flag, switch to non-replicated outcalls — push
//! dispatch is a fire-and-forget side-effect and does not need consensus.

#[cfg(not(test))]
use crate::state;
#[cfg(not(test))]
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
#[cfg(not(test))]
use candid::Principal;
#[cfg(not(test))]
use ic_cdk::api::call::call;
#[cfg(not(test))]
use ic_cdk::api::management_canister::http_request::{
    http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod, HttpResponse,
};
#[cfg(not(test))]
use internet_identity_interface::internet_identity::types::AnchorNumber;
#[cfg(not(test))]
use p256::{
    ecdsa::{signature::Signer, Signature, SigningKey},
    elliptic_curve::sec1::ToEncodedPoint,
    pkcs8::{DecodePrivateKey, EncodePrivateKey},
    SecretKey,
};

#[cfg(not(test))]
const PUSH_CALL_CYCLES: u128 = 30_000_000_000;
/// VAPID JWT validity window: 12 hours. Push services reject JWTs with `exp`
/// more than 24 hours in the future.
#[cfg(not(test))]
const VAPID_JWT_TTL_SECONDS: u64 = 12 * 60 * 60;
/// How long push services should retain an undelivered message (seconds).
#[cfg(not(test))]
const PUSH_TTL_SECONDS: u64 = 24 * 60 * 60;
/// `sub` claim in the VAPID JWT — a contact URI in case the push service
/// needs to reach us about abusive traffic.
#[cfg(not(test))]
const VAPID_CONTACT: &str = "mailto:noreply@internetcomputer.org";
/// `Topic` header value (RFC 8030 §5.4) shared by every new-email push. Push
/// services use this to collapse older undelivered messages of the same topic
/// for a subscription — so a user whose device was offline during a burst of
/// arrivals receives only the most recent one when it comes back online.
#[cfg(not(test))]
const PUSH_TOPIC: &str = "ii-new-email";

// ---------------------------------------------------------------------------
// VAPID key management
// ---------------------------------------------------------------------------

/// Returns the stored VAPID private key, generating one on first call.
#[cfg(not(test))]
pub async fn ensure_vapid_key() -> Result<Vec<u8>, String> {
    if let Some(key) = state::persistent_state(|ps| ps.vapid_key.clone()) {
        return Ok(key);
    }

    let random_bytes: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((bytes,)) => bytes,
        Err((_, err)) => return Err(format!("raw_rand call failed: {err}")),
    };

    if random_bytes.len() < 32 {
        return Err("raw_rand returned fewer than 32 bytes".to_string());
    }

    let secret_key = SecretKey::from_slice(&random_bytes[..32])
        .map_err(|e| format!("invalid P-256 secret key: {e}"))?;
    let der = secret_key
        .to_pkcs8_der()
        .map_err(|e| format!("PKCS#8 encoding failed: {e}"))?;
    let key_bytes = der.as_bytes().to_vec();

    state::persistent_state_mut(|ps| ps.vapid_key = Some(key_bytes.clone()));
    state::save_persistent_state();
    Ok(key_bytes)
}

/// Returns the VAPID public key as 65 uncompressed SEC-1 bytes
/// (`0x04 || X || Y`), or `None` if the key hasn't been generated yet.
#[cfg(not(test))]
pub fn vapid_public_key_bytes() -> Option<Vec<u8>> {
    let der = state::persistent_state(|ps| ps.vapid_key.clone())?;
    let sk = SecretKey::from_pkcs8_der(&der).ok()?;
    let encoded_point = sk.public_key().to_encoded_point(false);
    Some(encoded_point.as_bytes().to_vec())
}

// ---------------------------------------------------------------------------
// VAPID JWT construction
// ---------------------------------------------------------------------------

/// Extracts the origin (`scheme://host[:port]`) from a URL. Returns `None`
/// for malformed URLs.
#[cfg(not(test))]
fn extract_origin(endpoint: &str) -> Option<String> {
    let after_scheme = endpoint.split_once("://")?;
    let scheme = after_scheme.0;
    if scheme != "https" && scheme != "http" {
        return None;
    }
    let rest = after_scheme.1;
    let host = match rest.find('/') {
        Some(idx) => &rest[..idx],
        None => rest,
    };
    if host.is_empty() {
        return None;
    }
    Some(format!("{scheme}://{host}"))
}

#[cfg(not(test))]
fn base64url(input: &[u8]) -> String {
    URL_SAFE_NO_PAD.encode(input)
}

/// Builds a VAPID JWT (RFC 8292) for the given push service audience (origin).
#[cfg(not(test))]
fn create_vapid_jwt(audience: &str, vapid_key_der: &[u8]) -> Result<String, String> {
    let sk =
        SecretKey::from_pkcs8_der(vapid_key_der).map_err(|e| format!("invalid VAPID key: {e}"))?;

    let header_b64 = base64url(br#"{"typ":"JWT","alg":"ES256"}"#);
    let now_secs = ic_cdk::api::time() / 1_000_000_000;
    let claims = format!(
        r#"{{"aud":"{}","exp":{},"sub":"{}"}}"#,
        audience,
        now_secs + VAPID_JWT_TTL_SECONDS,
        VAPID_CONTACT
    );
    let claims_b64 = base64url(claims.as_bytes());
    let signing_input = format!("{header_b64}.{claims_b64}");

    let signing_key = SigningKey::from(sk);
    let signature: Signature = signing_key.sign(signing_input.as_bytes());
    // Push services expect raw `r || s` (64 bytes), not DER.
    let sig_b64 = base64url(&signature.to_bytes());

    Ok(format!("{signing_input}.{sig_b64}"))
}

// ---------------------------------------------------------------------------
// HTTP outcall
// ---------------------------------------------------------------------------

/// Outcome of a single push attempt.
#[cfg(not(test))]
#[derive(Debug)]
enum PushError {
    /// Endpoint returned 404/410 — subscription is gone, caller should remove.
    SubscriptionGone,
    /// Any other failure (transient or permanent). Caller should log and keep
    /// the subscription.
    Other(String),
}

#[cfg(not(test))]
async fn send_push_to_endpoint(endpoint: &str, vapid_key_der: &[u8]) -> Result<(), PushError> {
    let audience = extract_origin(endpoint)
        .ok_or_else(|| PushError::Other(format!("malformed endpoint URL: {endpoint}")))?;
    let jwt = create_vapid_jwt(&audience, vapid_key_der).map_err(PushError::Other)?;

    let public_key_bytes =
        vapid_public_key_bytes().ok_or_else(|| PushError::Other("VAPID key missing".into()))?;
    let public_key_b64 = base64url(&public_key_bytes);

    let request = CanisterHttpRequestArgument {
        url: endpoint.to_string(),
        method: HttpMethod::POST,
        body: Some(vec![]),
        max_response_bytes: Some(1024),
        transform: None,
        headers: vec![
            HttpHeader {
                name: "Authorization".into(),
                value: format!("vapid t={jwt}, k={public_key_b64}"),
            },
            HttpHeader {
                name: "TTL".into(),
                value: PUSH_TTL_SECONDS.to_string(),
            },
            HttpHeader {
                name: "Topic".into(),
                value: PUSH_TOPIC.to_string(),
            },
            HttpHeader {
                name: "Content-Length".into(),
                value: "0".into(),
            },
        ],
    };

    let (response,) = http_request_with_closure(request, PUSH_CALL_CYCLES, transform_push_response)
        .await
        .map_err(|(_, err)| PushError::Other(err))?;

    // `response.status` is a `candid::Nat`. Compare against a few well-known
    // HTTP codes; treat 2xx as success.
    let status_str = response.status.0.to_string();
    let status: u16 = status_str.parse().unwrap_or(0);
    match status {
        200..=299 => Ok(()),
        404 | 410 => Err(PushError::SubscriptionGone),
        _ => Err(PushError::Other(format!(
            "push service returned status {status}"
        ))),
    }
}

/// Deterministic transform for the push service response.
///
/// Push service response bodies (e.g. FCM message IDs) and timestamp headers
/// vary per replica, which would break consensus. We discard them and keep
/// only the status code — that's all we need to decide success/failure.
#[cfg(not(test))]
fn transform_push_response(response: HttpResponse) -> HttpResponse {
    HttpResponse {
        status: response.status,
        headers: vec![],
        body: vec![],
    }
}

// ---------------------------------------------------------------------------
// Dispatch orchestrator
// ---------------------------------------------------------------------------

/// Sends a push notification to every subscription registered for `anchor`.
///
/// Called from [`crate::smtp::handle_smtp_request`] via `ic_cdk::spawn`. Any
/// subscription that returns 404/410 is removed from storage — those endpoints
/// will never succeed again.
#[cfg(not(test))]
pub async fn dispatch_push_for_anchor(recipient_key: &str) {
    let anchor_number: AnchorNumber = match recipient_key.parse() {
        Ok(n) => n,
        Err(_) => return,
    };

    let vapid_key = match state::persistent_state(|ps| ps.vapid_key.clone()) {
        Some(k) => k,
        None => return, // No subscribers means no VAPID key exists yet.
    };

    let subscriptions = state::storage_borrow(|s| s.get_push_subscriptions(anchor_number));
    if subscriptions.is_empty() {
        return;
    }

    let mut gone_endpoints: Vec<String> = Vec::new();
    for sub in &subscriptions {
        match send_push_to_endpoint(&sub.endpoint, &vapid_key).await {
            Ok(()) => {}
            Err(PushError::SubscriptionGone) => {
                gone_endpoints.push(sub.endpoint.clone());
            }
            Err(PushError::Other(err)) => {
                // Transient — keep the subscription and let the next email
                // try again. No retry-now to bound cycle cost.
                ic_cdk::println!("push dispatch failed: {err}");
            }
        }
    }

    if !gone_endpoints.is_empty() {
        state::storage_borrow_mut(|s| {
            for endpoint in &gone_endpoints {
                s.remove_push_subscription(anchor_number, endpoint);
            }
        });
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    // Most of this module is gated on `#[cfg(not(test))]` because it depends
    // on `ic_cdk::api::time()` and the management canister. Pure helpers like
    // `extract_origin` are exercised indirectly via integration tests of the
    // smtp flow.
}
