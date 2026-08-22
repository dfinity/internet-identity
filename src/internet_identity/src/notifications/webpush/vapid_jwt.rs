//! Reassembles the compact VAPID JWT a device pre-signed. II holds no VAPID
//! key: the device signs the deterministic claims for each validity window and
//! uploads only the raw signatures, and the sender rebuilds
//! `base64url(header).base64url(payload).base64url(signature)` for the window
//! covering now. The header bytes, the claim order, and `WINDOW_NS` are a wire
//! contract with the frontend: change one side only and every signature stops
//! verifying at the relay.

use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use internet_identity_interface::internet_identity::types::Timestamp;

/// One signature per window; 30 windows cover ~30 days (see MAX_JWT_POOL_LEN).
const WINDOW_NS: u64 = 24 * 60 * 60 * 1_000_000_000;
/// Fixed ES256 header, base64url of `{"typ":"JWT","alg":"ES256"}`.
const HEADER_B64: &str = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9";
/// RFC 8292 `sub`: a stable contact the push service can reach II's operator at.
const VAPID_SUBJECT: &str = "https://id.ai";

/// The compact VAPID JWT authorizing a push to `relay_origin` at `now_ns`, or
/// `None` when the pool has no signature covering the current window (exhausted,
/// or minted in the future). `relay_origin` is the `scheme://host` the JWT's
/// `aud` is bound to.
pub fn assemble(
    pool: &StorableWebPushJwtPool,
    relay_origin: &str,
    now_ns: Timestamp,
) -> Option<String> {
    let index = (now_ns.checked_sub(pool.issued_at_ns)? / WINDOW_NS) as usize;
    let signature = pool.signatures.get(index)?;
    let exp_secs = (pool.issued_at_ns + (index as u64 + 1) * WINDOW_NS) / 1_000_000_000;

    let aud = serde_json::to_string(relay_origin).ok()?;
    let payload = format!(r#"{{"aud":{aud},"exp":{exp_secs},"sub":"{VAPID_SUBJECT}"}}"#);
    let payload_b64 = BASE64_URL_SAFE_NO_PAD.encode(payload);
    let signature_b64 = BASE64_URL_SAFE_NO_PAD.encode(signature);
    Some(format!("{HEADER_B64}.{payload_b64}.{signature_b64}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pool(count: usize, issued_at_ns: Timestamp) -> StorableWebPushJwtPool {
        StorableWebPushJwtPool {
            signatures: (0..count).map(|i| vec![i as u8; 64]).collect(),
            issued_at_ns,
        }
    }

    fn parts(jwt: &str) -> (String, serde_json::Value, Vec<u8>) {
        let segments: Vec<&str> = jwt.split('.').collect();
        assert_eq!(segments.len(), 3);
        let payload = BASE64_URL_SAFE_NO_PAD.decode(segments[1]).unwrap();
        (
            segments[0].to_string(),
            serde_json::from_slice(&payload).unwrap(),
            BASE64_URL_SAFE_NO_PAD.decode(segments[2]).unwrap(),
        )
    }

    #[test]
    fn header_is_the_fixed_es256_template() {
        let jwt = assemble(&pool(1, 0), "https://relay.example", 0).unwrap();
        let (header_b64, _, _) = parts(&jwt);
        let header = BASE64_URL_SAFE_NO_PAD.decode(header_b64).unwrap();
        assert_eq!(header, br#"{"typ":"JWT","alg":"ES256"}"#);
    }

    #[test]
    fn picks_the_signature_for_the_window_covering_now() {
        let issued = 1_000 * WINDOW_NS;
        let p = pool(5, issued);
        // Two-and-a-bit windows in resolves to window index 2.
        let jwt = assemble(&p, "https://relay.example", issued + 2 * WINDOW_NS + 123).unwrap();
        let (_, _, signature) = parts(&jwt);
        assert_eq!(signature, vec![2u8; 64]);
    }

    #[test]
    fn exp_is_the_window_end_in_seconds() {
        let issued = 1_000 * WINDOW_NS;
        let jwt = assemble(&pool(3, issued), "https://relay.example", issued).unwrap();
        let (_, claims, _) = parts(&jwt);
        assert_eq!(claims["exp"], (issued + WINDOW_NS) / 1_000_000_000);
        assert_eq!(claims["aud"], "https://relay.example");
        assert_eq!(claims["sub"], VAPID_SUBJECT);
    }

    #[test]
    fn claims_serialize_in_the_agreed_order() {
        let jwt = assemble(&pool(1, 0), "https://relay.example", 0).unwrap();
        let (_, _, _) = parts(&jwt);
        let payload_b64 = jwt.split('.').nth(1).unwrap();
        let payload =
            String::from_utf8(BASE64_URL_SAFE_NO_PAD.decode(payload_b64).unwrap()).unwrap();
        assert!(payload.starts_with(r#"{"aud":"https://relay.example","exp":"#));
        assert!(payload.ends_with(&format!(r#","sub":"{VAPID_SUBJECT}"}}"#)));
    }

    #[test]
    fn none_when_pool_exhausted() {
        let issued = 1_000 * WINDOW_NS;
        let p = pool(2, issued);
        assert!(assemble(&p, "https://relay.example", issued + 2 * WINDOW_NS).is_none());
    }

    #[test]
    fn none_when_minted_in_the_future() {
        assert!(assemble(&pool(3, 5 * WINDOW_NS), "https://relay.example", 0).is_none());
    }
}
