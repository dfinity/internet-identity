//! VAPID — Voluntary Application Server Identification (RFC 8292).
//!
//! Every Web Push request the canister makes carries an ECDSA-P256-SHA256
//! JWT signed with a keypair the browser has pre-registered via
//! `pushManager.subscribe({applicationServerKey})`. Relays (FCM/APNs/
//! Mozilla) reject unsigned requests — this module is what unblocks the
//! outcall.
//!
//! # Key management
//!
//! The private key is a random 32-byte P-256 scalar generated via
//! `raw_rand` on first use and persisted in `PersistentState`. Same
//! shape as II's anchor salt, and unlike the previous PoC form it never
//! ships in the WASM binary. The public key is derived on demand from
//! the private key.

#![allow(deprecated)]

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use p256::ecdsa::{signature::Signer, Signature, SigningKey};

const VAPID_SUBJECT: &str = "mailto:security@identity.internetcomputer.org";

pub const VAPID_JWT_MAX_LIFETIME_SECS: u64 = 12 * 60 * 60;

/// Load the VAPID signing key from persistent state, or generate + persist
/// one on first use. Uses `raw_rand` as the entropy source and rejection-
/// samples against the P-256 curve order — the rejection probability per
/// draw is on the order of 2^-128, so the loop terminates in one
/// iteration with overwhelming probability.
pub async fn get_or_init_signing_key() -> Result<SigningKey, String> {
    if let Some(bytes) = crate::state::persistent_state(|s| s.push_vapid_private_key) {
        return SigningKey::from_slice(&bytes)
            .map_err(|e| format!("stored VAPID key is not a valid P-256 scalar: {e}"));
    }
    for _ in 0..16 {
        let seed = crate::random_salt().await;
        if let Ok(sk) = SigningKey::from_slice(&seed) {
            crate::state::persistent_state_mut(|s| s.push_vapid_private_key = Some(seed));
            crate::state::save_persistent_state();
            return Ok(sk);
        }
    }
    Err("failed to generate a valid P-256 scalar after 16 tries".into())
}

pub fn public_key_uncompressed(sk: &SigningKey) -> [u8; 65] {
    let point = sk.verifying_key().to_encoded_point(false);
    let src = point.as_bytes();
    let mut out = [0u8; 65];
    let n = src.len().min(65);
    out[..n].copy_from_slice(&src[..n]);
    out
}

pub fn public_key_base64url(sk: &SigningKey) -> String {
    URL_SAFE_NO_PAD.encode(public_key_uncompressed(sk))
}

/// Sign a VAPID JWT for the given push relay.
///
/// - `audience` is the relay's origin (`scheme://host`, no path) — e.g.
///   `https://fcm.googleapis.com`. Extract with [`origin_of_endpoint`].
/// - `expires_at_secs` is the JWT `exp` claim. Must be within 12 hours
///   of "now" per RFC 8292 §2.
pub fn sign_jwt(sk: &SigningKey, audience: &str, expires_at_secs: u64) -> String {
    let header_json = r#"{"typ":"JWT","alg":"ES256"}"#;
    let payload_json =
        format!(r#"{{"aud":"{audience}","exp":{expires_at_secs},"sub":"{VAPID_SUBJECT}"}}"#);

    let header_b64 = URL_SAFE_NO_PAD.encode(header_json.as_bytes());
    let payload_b64 = URL_SAFE_NO_PAD.encode(payload_json.as_bytes());
    let signing_input = format!("{header_b64}.{payload_b64}");

    let signature: Signature = sk.sign(signing_input.as_bytes());
    let sig_b64 = URL_SAFE_NO_PAD.encode(signature.to_bytes().as_slice());

    format!("{signing_input}.{sig_b64}")
}

/// Extract the origin (scheme + host, no trailing slash, no path) from
/// a push relay endpoint URL. Returns an error for anything that isn't
/// an `https://...` URL.
pub fn origin_of_endpoint(endpoint: &str) -> Result<String, String> {
    let rest = endpoint
        .strip_prefix("https://")
        .ok_or_else(|| format!("endpoint must be https, got: {endpoint}"))?;
    let host_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
    if host_end == 0 {
        return Err("endpoint has empty host".to_string());
    }
    Ok(format!("https://{}", &rest[..host_end]))
}
