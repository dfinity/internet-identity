//! VAPID — Voluntary Application Server Identification (RFC 8292).
//!
//! Every Web Push request the canister makes carries an ECDSA-P256-SHA256
//! JWT signed with a keypair the browser has pre-registered via
//! `pushManager.subscribe({applicationServerKey})`. Relays (FCM/APNs/
//! Mozilla) reject unsigned requests — this module is what unblocks the
//! outcall.
//!
//! # PoC keypair
//!
//! The keypair below was generated once via `openssl ecparam -name
//! prime256v1 -genkey`. Hardcoding is fine for a PoC — the *private*
//! half never leaves the wasm binary at runtime, but it's not secret vs.
//! anyone with source access. Rotate (and move into a stable-memory
//! region seeded at `canister_init`) before this ships to prod.

// Silence deprecation warnings from the RustCrypto ecosystem's ongoing
// migration to `generic-array` 1.x — the replacement APIs aren't stable
// across the versions II pins. Same rationale as `push::rfc8291`.
#![allow(deprecated)]

use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine};
use p256::ecdsa::{signature::Signer, Signature, SigningKey};

/// P-256 private scalar (32 bytes).
const VAPID_PRIVATE_KEY: [u8; 32] = [
    0xe2, 0xa8, 0xcb, 0x14, 0x7c, 0xbd, 0x00, 0x0c, 0x2b, 0x47, 0x19, 0xe2, 0x36, 0xf9, 0x4d, 0x22,
    0xa2, 0x8c, 0xff, 0x62, 0xef, 0x38, 0x10, 0xfd, 0xc4, 0x06, 0x4b, 0xc3, 0x8e, 0xa5, 0x11, 0xb8,
];

/// P-256 public key, uncompressed SEC1 encoding (65 bytes, `0x04 || X || Y`).
/// The frontend passes this as `applicationServerKey` to
/// `pushManager.subscribe()`; the relay also sees it via the `k=`
/// parameter on the `Authorization: vapid` header.
pub const VAPID_PUBLIC_KEY_UNCOMPRESSED: [u8; 65] = [
    0x04, 0x69, 0xc6, 0xe1, 0x88, 0x11, 0xf2, 0x4c, 0xd9, 0xc9, 0xb1, 0xfe, 0x86, 0x55, 0xf1, 0x62,
    0x02, 0x4b, 0xb5, 0xc4, 0x36, 0xbc, 0x6f, 0xb4, 0xde, 0x19, 0x18, 0x1d, 0x92, 0xd8, 0xa6, 0x0b,
    0x03, 0x69, 0xf1, 0x9f, 0xfa, 0x74, 0xb1, 0x7c, 0x02, 0x2f, 0x45, 0xd3, 0xfe, 0xfe, 0x13, 0xe1,
    0xbe, 0x29, 0x37, 0xbc, 0x8f, 0x35, 0x45, 0x7f, 0xf1, 0x24, 0xa9, 0x1d, 0xb9, 0xfc, 0x37, 0x53,
    0xe4,
];

/// The `sub` claim — a mailto the relay can use to reach us if it sees
/// abuse from this VAPID identity. PoC placeholder.
const VAPID_SUBJECT: &str = "mailto:security@identity.internetcomputer.org";

/// Maximum lifetime of a VAPID JWT per RFC 8292 §2. We stay well under.
pub const VAPID_JWT_MAX_LIFETIME_SECS: u64 = 12 * 60 * 60;

/// Sign a VAPID JWT for the given push relay.
///
/// - `audience` is the relay's origin (`scheme://host`, no path) — e.g.
///   `https://fcm.googleapis.com`. Extract with [`origin_of_endpoint`].
/// - `expires_at_secs` is the JWT `exp` claim (seconds since epoch).
///   Must be within 12 hours of "now" per RFC 8292 §2.
///
/// Returns the JWS compact serialization
/// (`<header>.<payload>.<signature>` with each part base64url-nopad).
pub fn sign_jwt(audience: &str, expires_at_secs: u64) -> String {
    // Header and payload are hand-formatted — the shapes are fixed and
    // reaching for `serde_json` just for two constant-shape objects
    // would inflate the wasm binary for no benefit.
    let header_json = r#"{"typ":"JWT","alg":"ES256"}"#;
    let payload_json =
        format!(r#"{{"aud":"{audience}","exp":{expires_at_secs},"sub":"{VAPID_SUBJECT}"}}"#);

    let header_b64 = URL_SAFE_NO_PAD.encode(header_json.as_bytes());
    let payload_b64 = URL_SAFE_NO_PAD.encode(payload_json.as_bytes());
    let signing_input = format!("{header_b64}.{payload_b64}");

    let signing_key =
        SigningKey::from_slice(&VAPID_PRIVATE_KEY).expect("hardcoded VAPID key is valid");
    let signature: Signature = signing_key.sign(signing_input.as_bytes());
    // Signature::to_bytes() returns the fixed-size 64-byte `r || s`
    // encoding used by JWS ES256 (not the ASN.1 DER form).
    let raw_sig = signature.to_bytes();
    let sig_b64 = URL_SAFE_NO_PAD.encode(raw_sig.as_slice());

    format!("{signing_input}.{sig_b64}")
}

/// Base64url-nopad-encoded uncompressed VAPID public key, ready for the
/// `k=` parameter of the `Authorization: vapid` header and for the FE's
/// `pushManager.subscribe({applicationServerKey})` call.
pub fn public_key_base64url() -> String {
    URL_SAFE_NO_PAD.encode(VAPID_PUBLIC_KEY_UNCOMPRESSED)
}

/// Extract the origin (scheme + host, no trailing slash, no path) from
/// a push relay endpoint URL. Returns an error for anything that isn't
/// an `https://...` URL — malformed subscriptions never should have
/// made it into storage but we still guard here defensively.
pub fn origin_of_endpoint(endpoint: &str) -> Result<String, String> {
    let rest = endpoint
        .strip_prefix("https://")
        .ok_or_else(|| format!("endpoint must be https, got: {endpoint}"))?;
    // Host ends at the first `/`, `?`, or `#`. RFC 3986: fragments and
    // queries can precede the path in theory, but push endpoints don't
    // use them — cheap safety.
    let host_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
    if host_end == 0 {
        return Err("endpoint has empty host".to_string());
    }
    Ok(format!("https://{}", &rest[..host_end]))
}
