//! Turning a caller's Web Push request into one the channel can act on: a deployment
//! that notifies at all, and fields the canister can deliver against.

use std::ops::RangeInclusive;
use url::Url;

/// Relay endpoints run ~200-300 bytes; capped at 512 so the row stays small.
const MAX_ENDPOINT_LEN: usize = 512;
/// VAPID application server key: uncompressed SEC1 P-256, 65 bytes.
const VAPID_PUBKEY_LEN: usize = 65;
/// Raw ECDSA P-256 signature: r||s, 32 bytes each.
pub(super) const JWT_SIG_LEN: usize = 64;
/// One JWT per validity window; 30 covers ~30 days. Bounded to cap the row.
const MAX_JWT_POOL_LEN: usize = 30;

/// A relay address a push can be posted to: 1..=512 bytes, a URL with a host, and a
/// scheme this deployment delivers over.
///
/// The scheme rule reads `notifications_allow_insecure_endpoint`, so what this
/// guarantees is that the address was deliverable when it was registered. That is the
/// decision the canister makes once and stores; an endpoint already on a subscription
/// is not re-judged when the flag moves.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PushEndpoint(String);

impl PushEndpoint {
    pub fn into_string(self) -> String {
        self.0
    }
}

impl TryFrom<&str> for PushEndpoint {
    type Error = String;

    fn try_from(endpoint: &str) -> Result<Self, Self::Error> {
        validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint")?;
        let Ok(url) = Url::parse(endpoint) else {
            return Err("endpoint is not a URL".to_string());
        };
        let local = url.scheme() == "http"
            && allow_insecure_endpoint()
            && crate::utils::is_loopback_host(url.host_str().unwrap_or_default());
        if url.scheme() != "https" && !local {
            return Err("endpoint must be an https:// URL".to_string());
        }
        if url.host().is_none() {
            return Err("endpoint has no host".to_string());
        }
        Ok(Self(endpoint.to_string()))
    }
}

/// The VAPID application server key, uncompressed SEC1 P-256.
///
/// Length plus curve validity. The key is echoed to the relay as VAPID `k=`, and a
/// relay rejects a push whose `k` it cannot parse.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VapidPublicKey(Vec<u8>);

impl VapidPublicKey {
    pub fn into_vec(self) -> Vec<u8> {
        self.0
    }
}

impl TryFrom<Vec<u8>> for VapidPublicKey {
    type Error = String;

    fn try_from(vapid_public_key: Vec<u8>) -> Result<Self, Self::Error> {
        validate_param_len(
            vapid_public_key.len(),
            VAPID_PUBKEY_LEN..=VAPID_PUBKEY_LEN,
            "vapid_public_key",
        )?;
        if p256::PublicKey::from_sec1_bytes(&vapid_public_key).is_err() {
            return Err("vapid_public_key is not a valid SEC1 P-256 point".to_string());
        }
        Ok(Self(vapid_public_key))
    }
}

/// One JWT signature per validity window, in window order.
///
/// The signatures themselves are not verified: garbage only breaks the uploader's own
/// delivery, and 30 ECDSA verifications per subscribe isn't worth it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JwtPool(Vec<Vec<u8>>);

impl JwtPool {
    pub fn into_vec(self) -> Vec<Vec<u8>> {
        self.0
    }
}

impl TryFrom<Vec<Vec<u8>>> for JwtPool {
    type Error = String;

    fn try_from(signatures: Vec<Vec<u8>>) -> Result<Self, Self::Error> {
        validate_param_len(signatures.len(), 1..=MAX_JWT_POOL_LEN, "jwt_pool")?;
        if let Some(bad) = signatures.iter().find(|sig| sig.len() != JWT_SIG_LEN) {
            return Err(format!(
                "each jwt signature must be {JWT_SIG_LEN} bytes, got {}",
                bad.len()
            ));
        }
        Ok(Self(signatures))
    }
}

fn validate_param_len(
    observed_len_bytes: usize,
    allowed_range_bytes: RangeInclusive<usize>,
    dbg_name: &str,
) -> Result<(), String> {
    if !allowed_range_bytes.contains(&observed_len_bytes) {
        let bounds = if allowed_range_bytes.start() == allowed_range_bytes.end() {
            format!("expected {}", allowed_range_bytes.start())
        } else {
            format!(
                "{}..={}",
                allowed_range_bytes.start(),
                allowed_range_bytes.end()
            )
        };
        return Err(format!(
            "{dbg_name} length {observed_len_bytes} out of range ({bounds})"
        ));
    }
    Ok(())
}

/// Whether this deployment lets a browser register a plain-http endpoint, which only
/// one whose relay is a local server ever should.
fn allow_insecure_endpoint() -> bool {
    crate::state::persistent_state(|s| s.notifications_allow_insecure_endpoint).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, SetWebPushSubscriptionRequest,
    };
    use serde_bytes::ByteBuf;
    const ANCHOR: AnchorNumber = 10_000;

    /// What `notifications_allow_insecure_endpoint` says, for a test that is about a
    /// deployment whose relay is a local server.
    fn allow_insecure(allow: bool) {
        crate::state::persistent_state_mut(|s| {
            s.notifications_allow_insecure_endpoint = Some(allow);
        });
    }

    /// Every field, the way the endpoint takes them: all three problems rather than the
    /// first.
    fn validate(request: SetWebPushSubscriptionRequest) -> Result<(), String> {
        let SetWebPushSubscriptionRequest {
            endpoint,
            vapid_public_key,
            jwt_signatures,
            ..
        } = request;
        let problems: Vec<String> = [
            PushEndpoint::try_from(endpoint.as_str()).err(),
            VapidPublicKey::try_from(vapid_public_key.into_vec()).err(),
            JwtPool::try_from(
                jwt_signatures
                    .into_iter()
                    .map(ByteBuf::into_vec)
                    .collect::<Vec<_>>(),
            )
            .err(),
        ]
        .into_iter()
        .flatten()
        .collect();
        if problems.is_empty() {
            return Ok(());
        }
        Err(problems.join("; "))
    }

    #[test]
    fn accepts_what_a_browser_sends() {
        setup();
        assert!(validate(request(ANCHOR, "https://relay.example/a", 0)).is_ok());
    }

    #[test]
    fn refuses_an_endpoint_nothing_can_be_sent_to_in_confidence() {
        setup();
        assert!(validate(request(ANCHOR, "http://relay.example/a", 0)).is_err());
    }

    /// A deployment whose relay is a local server has no certificate to offer.
    #[test]
    fn takes_a_loopback_one_where_the_deployment_says_it_has_no_relay() {
        setup();
        allow_insecure(true);
        let loopback = validate(request(ANCHOR, "http://127.0.0.1:4711/push", 0));
        let elsewhere = validate(request(ANCHOR, "http://relay.example/push", 0));
        allow_insecure(false);

        assert!(loopback.is_ok());
        assert!(elsewhere.is_err(), "only a local relay, not any plain host");
    }

    /// The canister POSTs to this to wake the browser, so a value it could never POST
    /// to is a row nothing can be delivered to.
    #[test]
    fn rejects_an_endpoint_that_is_not_an_https_url() {
        setup();
        for endpoint in [
            "",
            "x",
            "http://relay.example/a",
            "relay.example/a",
            "https://",
        ] {
            assert!(
                validate(request(ANCHOR, endpoint, 0)).is_err(),
                "{endpoint} must not be accepted"
            );
        }
        let too_long = format!("https://relay.example/{}", "x".repeat(MAX_ENDPOINT_LEN));
        assert!(validate(request(ANCHOR, &too_long, 0)).is_err());
    }

    #[test]
    fn vapid_key_must_be_a_real_point() {
        setup();
        for bad_key in [
            vec![4u8; 10],
            // Right length, not on the curve, so only the curve check rejects it.
            vec![4u8; VAPID_PUBKEY_LEN],
        ] {
            let mut sent = request(ANCHOR, "https://relay.example/a", 0);
            sent.vapid_public_key = ByteBuf::from(bad_key);
            assert!(validate(sent).is_err());
        }
    }

    #[test]
    fn rejects_a_pool_that_is_empty_over_long_or_mis_sized() {
        setup();
        for bad_pool in [
            vec![],
            vec![vec![3u8; JWT_SIG_LEN]; MAX_JWT_POOL_LEN + 1],
            vec![vec![3u8; 10]],
        ] {
            let mut sent = request(ANCHOR, "https://relay.example/a", 0);
            sent.jwt_signatures = bad_pool.into_iter().map(ByteBuf::from).collect();
            assert!(validate(sent).is_err());
        }
    }

    #[test]
    fn reports_every_invalid_field_at_once() {
        setup();
        let mut sent = request(ANCHOR, "", 0);
        sent.vapid_public_key = ByteBuf::from(vec![4u8; 10]);
        sent.jwt_signatures = vec![];

        let Err(problems) = validate(sent) else {
            panic!("three bad fields must be refused");
        };
        assert_eq!(problems.split("; ").count(), 3, "{problems}");
    }
}
