//! Turning a caller's Web Push request into one the channel can act on: a deployment
//! that notifies at all, and fields the canister can deliver against.

use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, RemoveWebPushSubscriptionError, RemoveWebPushSubscriptionRequest,
    SetWebPushSubscriptionError, SetWebPushSubscriptionRequest, Timestamp,
};
use serde_bytes::ByteBuf;
use std::ops::RangeInclusive;
use url::Url;

/// Relay endpoints run ~200-300 bytes; capped at 1 KiB.
const MAX_ENDPOINT_LEN: usize = 1024;
/// VAPID application server key: uncompressed SEC1 P-256, 65 bytes.
const VAPID_PUBKEY_LEN: usize = 65;
/// Raw ECDSA P-256 signature: r||s, 32 bytes each.
pub(super) const JWT_SIG_LEN: usize = 64;
/// One JWT per validity window; 30 covers ~30 days. Bounded to cap the row.
const MAX_JWT_POOL_LEN: usize = 30;

pub struct ValidatedSetWebPushSubscriptionRequest {
    pub anchor_number: AnchorNumber,
    pub endpoint: String,
    pub vapid_public_key: Vec<u8>,
    pub jwt_signatures: Vec<Vec<u8>>,
    pub jwt_issued_at_ns: Timestamp,
}

pub struct ValidatedRemoveWebPushSubscriptionRequest {
    pub anchor_number: AnchorNumber,
    pub browser_id: BrowserId,
}

impl TryFrom<SetWebPushSubscriptionRequest> for ValidatedSetWebPushSubscriptionRequest {
    type Error = SetWebPushSubscriptionError;

    fn try_from(
        SetWebPushSubscriptionRequest {
            anchor_number,
            endpoint,
            vapid_public_key,
            jwt_signatures,
            jwt_issued_at_ns,
        }: SetWebPushSubscriptionRequest,
    ) -> Result<Self, Self::Error> {
        check_enabled().map_err(SetWebPushSubscriptionError::InternalCanisterError)?;

        let vapid_public_key = vapid_public_key.into_vec();
        let jwt_signatures: Vec<Vec<u8>> =
            jwt_signatures.into_iter().map(ByteBuf::into_vec).collect();

        // Report every invalid field at once rather than failing on the first, so a
        // browser doesn't go round three times to learn what one answer can say.
        let problems: Vec<String> = [
            validate_endpoint(&endpoint),
            validate_vapid_public_key(&vapid_public_key),
            validate_jwt_pool(&jwt_signatures),
        ]
        .into_iter()
        .filter_map(Result::err)
        .collect();
        if !problems.is_empty() {
            return Err(SetWebPushSubscriptionError::InternalCanisterError(
                problems.join("; "),
            ));
        }

        Ok(Self {
            anchor_number,
            endpoint,
            vapid_public_key,
            jwt_signatures,
            jwt_issued_at_ns,
        })
    }
}

impl TryFrom<RemoveWebPushSubscriptionRequest> for ValidatedRemoveWebPushSubscriptionRequest {
    type Error = RemoveWebPushSubscriptionError;

    fn try_from(
        RemoveWebPushSubscriptionRequest {
            anchor_number,
            browser_id,
        }: RemoveWebPushSubscriptionRequest,
    ) -> Result<Self, Self::Error> {
        check_enabled().map_err(RemoveWebPushSubscriptionError::InternalCanisterError)?;
        Ok(Self {
            anchor_number,
            browser_id,
        })
    }
}

/// Server-side gate for the channel. Per deployment rather than per app: a subscription
/// belongs to a browser, not to one of the apps that may notify it.
fn check_enabled() -> Result<(), String> {
    if crate::notifications::notifications_enabled() {
        Ok(())
    } else {
        Err("notifications are not enabled".to_string())
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

/// Length plus an absolute `https://` URL. The canister POSTs to this to wake the
/// browser, so a value that is not one is a row nothing can ever be delivered to.
fn validate_endpoint(endpoint: &str) -> Result<(), String> {
    validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint")?;
    let Ok(url) = Url::parse(endpoint) else {
        return Err("endpoint is not a URL".to_string());
    };
    if url.scheme() != "https" {
        return Err("endpoint must be an https:// URL".to_string());
    }
    if url.host().is_none() {
        return Err("endpoint has no host".to_string());
    }
    Ok(())
}

/// Length plus curve validity. The key is echoed to the relay as VAPID `k=`, and a relay
/// rejects a push whose `k` it cannot parse.
fn validate_vapid_public_key(vapid_public_key: &[u8]) -> Result<(), String> {
    validate_param_len(
        vapid_public_key.len(),
        VAPID_PUBKEY_LEN..=VAPID_PUBKEY_LEN,
        "vapid_public_key",
    )?;
    if p256::PublicKey::from_sec1_bytes(vapid_public_key).is_err() {
        return Err("vapid_public_key is not a valid SEC1 P-256 point".to_string());
    }
    Ok(())
}

/// Not verified: garbage only breaks the uploader's own delivery, and 30
/// ECDSA verifications per subscribe isn't worth it.
fn validate_jwt_pool(signatures: &[Vec<u8>]) -> Result<(), String> {
    validate_param_len(signatures.len(), 1..=MAX_JWT_POOL_LEN, "jwt_pool")?;
    if let Some(bad) = signatures.iter().find(|sig| sig.len() != JWT_SIG_LEN) {
        return Err(format!(
            "each jwt signature must be {JWT_SIG_LEN} bytes, got {}",
            bad.len()
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;

    const ANCHOR: AnchorNumber = 10_000;

    fn enable() {
        crate::state::persistent_state_mut(|s| {
            s.notifications_enabled_origins = Some(vec!["https://app.example".to_string()]);
        });
    }

    fn validate(
        request: SetWebPushSubscriptionRequest,
    ) -> Result<ValidatedSetWebPushSubscriptionRequest, SetWebPushSubscriptionError> {
        request.try_into()
    }

    #[test]
    fn accepts_what_a_browser_sends() {
        enable();
        assert!(validate(request(ANCHOR, "https://relay.example/a", 0)).is_ok());
    }

    /// The channel is off until an operator enables an origin, and nothing may be
    /// written before then.
    #[test]
    fn refuses_every_request_while_the_channel_is_off() {
        crate::state::persistent_state_mut(|s| s.notifications_enabled_origins = None);

        assert!(validate(request(ANCHOR, "https://relay.example/a", 0)).is_err());
        assert!(ValidatedRemoveWebPushSubscriptionRequest::try_from(
            RemoveWebPushSubscriptionRequest {
                anchor_number: ANCHOR,
                browser_id: 1,
            }
        )
        .is_err());
    }

    /// The canister POSTs to this to wake the browser, so a value it could never POST
    /// to is a row nothing can be delivered to.
    #[test]
    fn rejects_an_endpoint_that_is_not_an_https_url() {
        enable();
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
        enable();
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
        enable();
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
        enable();
        let mut sent = request(ANCHOR, "", 0);
        sent.vapid_public_key = ByteBuf::from(vec![4u8; 10]);
        sent.jwt_signatures = vec![];

        let Err(SetWebPushSubscriptionError::InternalCanisterError(problems)) = validate(sent)
        else {
            panic!("three bad fields must be refused");
        };
        assert_eq!(problems.split("; ").count(), 3, "{problems}");
    }
}
