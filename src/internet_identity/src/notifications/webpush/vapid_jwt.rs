//! Rebuild VAPID JWTs from browser-signed claims; II holds no signing key.
//! Header bytes, claim order, and window duration must match `vapidPool.ts`.

use crate::storage::anchor::WebPushSubscription;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use internet_identity_interface::internet_identity::types::Timestamp;
use url::Url;

const SECOND_NS: u64 = 1_000_000_000;
const WINDOW_NS: u64 = 24 * 60 * 60 * SECOND_NS;
/// Base64url of `{"typ":"JWT","alg":"ES256"}`.
const HEADER_B64: &str = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9";
/// Operator contact for the RFC 8292 `sub` claim.
const VAPID_SUBJECT: &str = "https://id.ai";

/// Build a JWT for the current window, or return `None` if no signature covers it.
/// `relay_origin` must match the signed `aud` claim: `scheme://host[:port]`. Windows
/// are counted in whole seconds, as `exp` is, so a window's last partial second
/// already uses the next signature.
pub(crate) fn assemble(
    subscription: &WebPushSubscription,
    relay_origin: &str,
    now_ns: Timestamp,
) -> Option<String> {
    let issued_at_ns = subscription.jwt_issued_at_ns;
    now_ns.checked_sub(issued_at_ns)?;
    let index =
        ((now_ns / SECOND_NS - issued_at_ns / SECOND_NS) / (WINDOW_NS / SECOND_NS)) as usize;
    let signature = subscription.jwt_signatures.get(index)?;
    let exp_secs = (issued_at_ns + (index as u64 + 1) * WINDOW_NS) / SECOND_NS;

    let aud = serde_json::to_string(relay_origin).ok()?;
    let payload = format!(r#"{{"aud":{aud},"exp":{exp_secs},"sub":"{VAPID_SUBJECT}"}}"#);
    Some(format!(
        "{HEADER_B64}.{}.{}",
        BASE64_URL_SAFE_NO_PAD.encode(payload),
        BASE64_URL_SAFE_NO_PAD.encode(signature)
    ))
}

/// The `aud` the browser signed: `new URL(endpoint).origin`, serialized the same way.
pub(crate) fn relay_origin_of(endpoint: &str) -> Option<String> {
    let origin = Url::parse(endpoint).ok()?.origin();
    origin.is_tuple().then(|| origin.ascii_serialization())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn subscription(windows: usize, issued_at_ns: Timestamp) -> WebPushSubscription {
        WebPushSubscription {
            endpoint: "https://relay.example/wpush/abc".to_string(),
            created_at_ns: issued_at_ns,
            vapid_public_key: vec![4u8; 65],
            jwt_signatures: (0..windows).map(|window| vec![window as u8; 64]).collect(),
            jwt_issued_at_ns: issued_at_ns,
            notifications: Vec::new(),
        }
    }

    fn parts(jwt: &str) -> (Vec<u8>, serde_json::Value, Vec<u8>) {
        let segments: Vec<&str> = jwt.split('.').collect();
        assert_eq!(segments.len(), 3);
        (
            BASE64_URL_SAFE_NO_PAD.decode(segments[0]).unwrap(),
            serde_json::from_slice(&BASE64_URL_SAFE_NO_PAD.decode(segments[1]).unwrap()).unwrap(),
            BASE64_URL_SAFE_NO_PAD.decode(segments[2]).unwrap(),
        )
    }

    // These bytes must match the claims signed by the browser.
    #[test]
    fn the_header_is_the_fixed_es256_template() {
        let jwt = assemble(&subscription(1, 0), "https://relay.example", 0).unwrap();

        assert_eq!(parts(&jwt).0, br#"{"typ":"JWT","alg":"ES256"}"#);
    }

    #[test]
    fn the_signature_is_the_one_covering_now() {
        let issued = 1_000 * WINDOW_NS;
        let subscription = subscription(5, issued);

        let jwt = assemble(
            &subscription,
            "https://relay.example",
            issued + 2 * WINDOW_NS + 123,
        )
        .unwrap();

        assert_eq!(parts(&jwt).2, vec![2u8; 64]);
    }

    #[test]
    fn the_claims_are_the_window_end_the_relay_and_the_subject() {
        let issued = 1_000 * WINDOW_NS;

        let jwt = assemble(&subscription(3, issued), "https://relay.example", issued).unwrap();

        let claims = parts(&jwt).1;
        assert_eq!(claims["exp"], (issued + WINDOW_NS) / 1_000_000_000);
        assert_eq!(claims["aud"], "https://relay.example");
        assert_eq!(claims["sub"], VAPID_SUBJECT);
    }

    /// `exp` rounds down, so the old signature would already have expired.
    #[test]
    fn a_windows_last_partial_second_uses_the_next_signature() {
        let issued = 1_000 * WINDOW_NS + 700_000_000;
        let now = issued + WINDOW_NS - 200_000_000;

        let (_, claims, signature) =
            parts(&assemble(&subscription(3, issued), "https://relay.example", now).unwrap());

        assert!(claims["exp"].as_u64().unwrap() > now / SECOND_NS);
        assert_eq!(signature, vec![1u8; 64]);
    }

    #[test]
    fn a_spent_pool_assembles_nothing() {
        let issued = 1_000 * WINDOW_NS;
        let subscription = subscription(3, issued);

        assert!(assemble(
            &subscription,
            "https://relay.example",
            issued + 3 * WINDOW_NS
        )
        .is_none());
        // Future signature windows cannot be used early.
        assert!(assemble(&subscription, "https://relay.example", issued - 1).is_none());
    }

    #[test]
    fn the_relay_origin_is_the_endpoint_without_its_path() {
        assert_eq!(
            relay_origin_of("https://relay.example/wpush/abc"),
            Some("https://relay.example".to_string())
        );
        assert_eq!(
            relay_origin_of("https://relay.example:8443/wpush"),
            Some("https://relay.example:8443".to_string())
        );
        assert_eq!(
            relay_origin_of("https://relay.example"),
            Some("https://relay.example".to_string())
        );
        assert_eq!(relay_origin_of("relay.example/wpush"), None);
    }

    /// Anything else would sign different bytes than the browser did.
    #[test]
    fn the_relay_origin_is_serialized_as_a_browser_does() {
        for (endpoint, origin) in [
            ("https://relay.example?token=x", "https://relay.example"),
            ("https://relay.example#part", "https://relay.example"),
            ("https://relay.example:443/wpush", "https://relay.example"),
            ("https://Relay.EXAMPLE/wpush", "https://relay.example"),
            (
                "https://user:pass@relay.example/wpush",
                "https://relay.example",
            ),
        ] {
            assert_eq!(
                relay_origin_of(endpoint),
                Some(origin.to_string()),
                "{endpoint}"
            );
        }
    }
}
