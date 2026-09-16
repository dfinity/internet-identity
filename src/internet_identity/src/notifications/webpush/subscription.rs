//! One subscription row per device, keyed `(anchor, sha256(endpoint))`.

use super::super::{authorize_update, check_enabled, NotificationError};
use super::{
    validate_jwt_pool, validate_param_len, validate_vapid_public_key, MAX_ENDPOINT_LEN,
    MAX_SUBSCRIPTIONS_PER_ANCHOR,
};
use crate::state::storage_borrow_mut;
use crate::storage::storable::notifications::webpush::endpoint_hash::StorableEndpointSha256;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};
use minicbor::bytes::ByteVec;
use serde_bytes::ByteBuf;

/// Everything a browser uploads when it registers for Web Push. A record rather
/// than positional arguments: the fields are same-typed blobs that would be
/// silently swappable in a call.
#[derive(candid::CandidType, serde::Deserialize, Clone, Debug)]
pub struct SubscribeDeviceRequest {
    pub anchor_number: AnchorNumber,
    pub endpoint: String,
    pub vapid_public_key: ByteBuf,
    pub jwt_signatures: Vec<ByteBuf>,
    pub jwt_issued_at_ns: Timestamp,
}

/// The same request once the candid byte wrappers are off.
pub(super) struct Subscription {
    pub anchor_number: AnchorNumber,
    pub endpoint: String,
    pub vapid_public_key: Vec<u8>,
    pub jwt_signatures: Vec<Vec<u8>>,
    pub jwt_issued_at_ns: Timestamp,
}

impl From<SubscribeDeviceRequest> for Subscription {
    fn from(request: SubscribeDeviceRequest) -> Self {
        Self {
            anchor_number: request.anchor_number,
            endpoint: request.endpoint,
            vapid_public_key: request.vapid_public_key.into_vec(),
            jwt_signatures: request
                .jwt_signatures
                .into_iter()
                .map(ByteBuf::into_vec)
                .collect(),
            jwt_issued_at_ns: request.jwt_issued_at_ns,
        }
    }
}

pub(super) fn add_subscription(
    subscription: Subscription,
    now_ns: Timestamp,
) -> Result<(), Vec<String>> {
    let Subscription {
        anchor_number,
        endpoint,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
    } = subscription;

    // Report every invalid field at once rather than failing on the first.
    let errors: Vec<String> = [
        validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint"),
        validate_vapid_public_key(&vapid_public_key),
        validate_jwt_pool(&jwt_signatures),
    ]
    .into_iter()
    .filter_map(Result::err)
    .collect();
    if !errors.is_empty() {
        return Err(errors);
    }

    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    let row = StorableWebPushSubscription {
        anchor: anchor_number,
        endpoint,
        created_at_ns: now_ns,
        vapid_public_key,
        jwt_pool: Some(StorableWebPushJwtPool {
            signatures: jwt_signatures.into_iter().map(ByteVec::from).collect(),
            issued_at_ns: jwt_issued_at_ns,
        }),
    };

    storage_borrow_mut(|storage| {
        let key = (anchor_number, endpoint_hash.clone());
        let is_new_device = storage.webpush_subscriptions_memory.get(&key).is_none();

        // A re-subscribe overwrites in place, so only a new device grows the count.
        if is_new_device {
            let range_start = (anchor_number, StorableEndpointSha256::MIN);
            let range_end = (anchor_number, StorableEndpointSha256::MAX);
            let existing_devices: Vec<(StorableEndpointSha256, Timestamp)> = storage
                .webpush_subscriptions_memory
                .range(range_start..=range_end)
                .map(|((_, hash), sub)| (hash, sub.created_at_ns))
                .collect();

            if existing_devices.len() as u64 >= MAX_SUBSCRIPTIONS_PER_ANCHOR {
                if let Some((oldest_hash, _)) =
                    existing_devices.into_iter().min_by_key(|(_, ts)| *ts)
                {
                    // The key came from the scan just above, so the removal must
                    // hit; a miss means the row vanished underneath us. The JWT
                    // pool rides on the same row and goes with it.
                    let removed = storage
                        .webpush_subscriptions_memory
                        .remove(&(anchor_number, oldest_hash));
                    debug_assert!(
                        removed.is_some(),
                        "evicted a subscription that was not present"
                    );
                }
            }
        }

        storage.webpush_subscriptions_memory.insert(key, row);
    });
    Ok(())
}

/// Drops the subscription, and with it the JWT pool on the same row. Idempotent.
pub(super) fn remove_subscription(anchor_number: AnchorNumber, endpoint: &str) {
    let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
    storage_borrow_mut(|storage| {
        storage
            .webpush_subscriptions_memory
            .remove(&(anchor_number, endpoint_hash));
    });
}

/// Idempotent: re-subscribing the same endpoint overwrites in place.
pub fn subscribe_device(request: SubscribeDeviceRequest) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(request.anchor_number)?;
    let now_ns = ic_cdk::api::time();
    add_subscription(request.into(), now_ns).map_err(NotificationError::InvalidSubscription)
}

/// Removes this device's subscription. Idempotent.
pub fn unsubscribe_device(
    anchor_number: AnchorNumber,
    endpoint: String,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    remove_subscription(anchor_number, &endpoint);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::super::{JWT_SIG_LEN, MAX_JWT_POOL_LEN};
    use super::*;
    use crate::notifications::test_setup as setup;
    use crate::state::storage_borrow;

    fn request(anchor: AnchorNumber, endpoint: &str) -> Subscription {
        Subscription {
            anchor_number: anchor,
            endpoint: endpoint.to_string(),
            vapid_public_key: valid_vapid_key(),
            jwt_signatures: valid_pool(),
            jwt_issued_at_ns: 0,
        }
    }

    fn subscription_count(anchor_number: AnchorNumber) -> u64 {
        let range_start = (anchor_number, StorableEndpointSha256::MIN);
        let range_end = (anchor_number, StorableEndpointSha256::MAX);
        storage_borrow(|s| {
            s.webpush_subscriptions_memory
                .range(range_start..=range_end)
                .count() as u64
        })
    }

    #[test]
    fn subscribe_then_unsubscribe_round_trips() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a".to_string();

        subscribe(anchor, &endpoint, 1_000).unwrap();
        assert_eq!(subscription_count(anchor), 1);

        remove_subscription(anchor, &endpoint);
        assert_eq!(subscription_count(anchor), 0);
    }

    #[test]
    fn resubscribing_same_endpoint_overwrites_not_duplicates() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a".to_string();

        subscribe(anchor, &endpoint, 1_000).unwrap();
        subscribe(anchor, &endpoint, 2_000).unwrap();

        assert_eq!(subscription_count(anchor), 1);
    }

    #[test]
    fn unsubscribing_twice_is_a_harmless_no_op() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a".to_string();

        remove_subscription(anchor, &endpoint);
        remove_subscription(anchor, &endpoint);

        assert_eq!(subscription_count(anchor), 0);
    }

    #[test]
    fn subscribe_rejects_malformed_input() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a";

        assert!(subscribe(anchor, "", 0).is_err());

        let long_endpoint = format!("https://relay.example/{}", "x".repeat(MAX_ENDPOINT_LEN));
        assert!(add_subscription(request(anchor, &long_endpoint), 0).is_err());

        for bad_key in [vec![4u8; 10], vec![4u8; 65]] {
            let mut subscription = request(anchor, endpoint);
            subscription.vapid_public_key = bad_key;
            assert!(add_subscription(subscription, 0).is_err());
        }

        for bad_pool in [
            vec![],
            vec![vec![3u8; JWT_SIG_LEN]; MAX_JWT_POOL_LEN + 1],
            vec![vec![3u8; 10]],
        ] {
            let mut subscription = request(anchor, endpoint);
            subscription.jwt_signatures = bad_pool;
            assert!(add_subscription(subscription, 0).is_err());
        }
    }

    #[test]
    fn aggregates_all_validation_errors() {
        setup();
        // Two bad fields should surface two errors, not just the first.
        let mut subscription = request(1, "https://relay.example/a");
        subscription.vapid_public_key = vec![4u8; 10];
        subscription.jwt_signatures = vec![];

        let errors = add_subscription(subscription, 0).unwrap_err();
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn evicts_the_oldest_subscription_on_the_21st() {
        setup();
        let anchor = 1;

        for i in 0..MAX_SUBSCRIPTIONS_PER_ANCHOR {
            subscribe(anchor, &format!("https://relay.example/{i}"), i).unwrap();
        }
        assert_eq!(subscription_count(anchor), MAX_SUBSCRIPTIONS_PER_ANCHOR);

        subscribe(anchor, "https://relay.example/new", 1_000).unwrap();

        assert_eq!(
            subscription_count(anchor),
            MAX_SUBSCRIPTIONS_PER_ANCHOR,
            "the cap must hold even after a 21st subscribe"
        );
        let oldest_hash = StorableEndpointSha256::from_endpoint("https://relay.example/0");
        let oldest_still_present = storage_borrow(|s| {
            s.webpush_subscriptions_memory
                .get(&(anchor, oldest_hash))
                .is_some()
        });
        assert!(
            !oldest_still_present,
            "the oldest subscription (lowest created_at_ns) should have been evicted"
        );
    }
}
