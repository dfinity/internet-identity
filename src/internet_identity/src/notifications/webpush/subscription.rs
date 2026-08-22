//! One subscription row per device, keyed `(anchor, sha256(endpoint))`.

use super::super::{authorize_update, check_enabled};
use super::seal::{drop_device_seals, reseal_device};
use super::{
    validate_jwt_pool, validate_p256dh, validate_param_len, AUTH_LEN, MAX_ENDPOINT_LEN,
    MAX_SUBSCRIPTIONS_PER_ANCHOR, VAPID_PUBKEY_LEN,
};
use crate::state::storage_borrow_mut;
use crate::storage::storable::notifications::webpush::endpoint_hash::StorableEndpointSha256;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

#[allow(clippy::too_many_arguments)]
pub(super) fn add_subscription(
    anchor_number: AnchorNumber,
    endpoint: String,
    p256dh: Vec<u8>,
    auth: Vec<u8>,
    vapid_public_key: Vec<u8>,
    jwt_signatures: Vec<Vec<u8>>,
    jwt_issued_at_ns: Timestamp,
    now_ns: Timestamp,
) -> Result<(), Vec<String>> {
    // Report every invalid field at once rather than failing on the first.
    let errors: Vec<String> = [
        validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint"),
        validate_p256dh(&p256dh),
        validate_param_len(auth.len(), AUTH_LEN..=AUTH_LEN, "auth"),
        validate_param_len(
            vapid_public_key.len(),
            VAPID_PUBKEY_LEN..=VAPID_PUBKEY_LEN,
            "vapid_public_key",
        ),
        validate_jwt_pool(&jwt_signatures),
    ]
    .into_iter()
    .filter_map(Result::err)
    .collect();
    if !errors.is_empty() {
        return Err(errors);
    }

    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    let subscription = StorableWebPushSubscription {
        anchor: anchor_number,
        endpoint,
        p256dh,
        auth,
        created_at_ns: now_ns,
        vapid_public_key,
        jwt_pool: Some(StorableWebPushJwtPool {
            signatures: jwt_signatures,
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
                    // pool rides on the same row, and the device's seals go too.
                    let removed = storage
                        .webpush_subscriptions_memory
                        .remove(&(anchor_number, oldest_hash.clone()));
                    debug_assert!(
                        removed.is_some(),
                        "evicted a subscription that was not present"
                    );
                    storage.remove_webpush_seals_for_device(anchor_number, &oldest_hash);
                }
            }
        }

        storage
            .webpush_subscriptions_memory
            .insert(key, subscription);
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
#[allow(clippy::too_many_arguments)]
pub async fn subscribe_device(
    anchor_number: AnchorNumber,
    endpoint: String,
    p256dh: Vec<u8>,
    auth: Vec<u8>,
    vapid_public_key: Vec<u8>,
    jwt_signatures: Vec<Vec<u8>>,
    jwt_issued_at_ns: Timestamp,
) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    let now_ns = ic_cdk::api::time();
    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    add_subscription(
        anchor_number,
        endpoint,
        p256dh.clone(),
        auth.clone(),
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
        now_ns,
    )
    .map_err(|errors| errors.join("; "))?;
    reseal_device(anchor_number, endpoint_hash, &p256dh, &auth, now_ns).await;
    Ok(())
}

/// Removes this device's subscription. Idempotent.
pub fn unsubscribe_device(anchor_number: AnchorNumber, endpoint: String) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    remove_subscription(anchor_number, &endpoint);
    drop_device_seals(anchor_number, &endpoint_hash);
    Ok(())
}

/// Whether the push channel can currently reach this anchor.
pub fn has_any_subscription(anchor_number: AnchorNumber) -> bool {
    let range_start = (anchor_number, StorableEndpointSha256::MIN);
    let range_end = (anchor_number, StorableEndpointSha256::MAX);
    crate::state::storage_borrow(|storage| {
        storage
            .webpush_subscriptions_memory
            .range(range_start..=range_end)
            .next()
            .is_some()
    })
}

/// Whether this specific device is currently subscribed.
pub fn has_subscription(anchor_number: AnchorNumber, endpoint: &str) -> bool {
    let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
    crate::state::storage_borrow(|storage| {
        storage
            .webpush_subscriptions_memory
            .contains_key(&(anchor_number, endpoint_hash))
    })
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::super::{JWT_SIG_LEN, MAX_JWT_POOL_LEN};
    use super::*;
    use crate::notifications::test_setup as setup;
    use crate::state::storage_borrow;

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

        let ep = "https://relay.example/a".to_string();
        // empty endpoint
        assert!(subscribe(anchor, "", 0).is_err());
        // wrong p256dh length
        assert!(add_subscription(
            anchor,
            ep.clone(),
            vec![4u8; 10],
            valid_auth(),
            valid_vapid_key(),
            valid_pool(),
            0,
            0
        )
        .is_err());
        // wrong auth length
        assert!(add_subscription(
            anchor,
            ep.clone(),
            valid_p256dh(),
            vec![9u8; 4],
            valid_vapid_key(),
            valid_pool(),
            0,
            0
        )
        .is_err());
        // wrong VAPID public key length
        assert!(add_subscription(
            anchor,
            ep.clone(),
            valid_p256dh(),
            valid_auth(),
            vec![4u8; 10],
            valid_pool(),
            0,
            0
        )
        .is_err());
        // empty pool
        assert!(add_subscription(
            anchor,
            ep.clone(),
            valid_p256dh(),
            valid_auth(),
            valid_vapid_key(),
            vec![],
            0,
            0
        )
        .is_err());
        // oversized pool
        assert!(add_subscription(
            anchor,
            ep.clone(),
            valid_p256dh(),
            valid_auth(),
            valid_vapid_key(),
            vec![vec![3u8; JWT_SIG_LEN]; MAX_JWT_POOL_LEN + 1],
            0,
            0
        )
        .is_err());
        // wrong signature length
        assert!(add_subscription(
            anchor,
            ep,
            valid_p256dh(),
            valid_auth(),
            valid_vapid_key(),
            vec![vec![3u8; 10]],
            0,
            0
        )
        .is_err());
    }

    #[test]
    fn aggregates_all_validation_errors() {
        setup();
        // Two bad fields should surface two errors, not just the first.
        let errors = add_subscription(
            1,
            "https://relay.example/a".to_string(),
            vec![4u8; 10],
            vec![9u8; 4],
            valid_vapid_key(),
            valid_pool(),
            0,
            0,
        )
        .unwrap_err();
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

    #[test]
    fn eviction_drops_the_evicted_device_seals() {
        use crate::storage::storable::application::StorableOriginSha256;
        use crate::storage::storable::notifications::webpush::seal::StorableWebPushSeal;

        setup();
        let anchor = 1;
        let oldest_endpoint = "https://relay.example/0";

        for i in 0..MAX_SUBSCRIPTIONS_PER_ANCHOR {
            subscribe(anchor, &format!("https://relay.example/{i}"), i).unwrap();
        }
        // Seal the oldest device for an origin, as consent would have.
        let oldest_hash = StorableEndpointSha256::from_endpoint(oldest_endpoint);
        let origin_hash = StorableOriginSha256::from_origin(&"https://app.example".to_string());
        storage_borrow_mut(|s| {
            s.add_webpush_seal(
                (anchor, oldest_hash.clone(), origin_hash.clone()),
                StorableWebPushSeal {
                    blob: vec![1u8; 10],
                    created_at_ns: 0,
                },
            );
        });

        subscribe(anchor, "https://relay.example/new", 1_000).unwrap();

        let seal_survived = storage_borrow(|s| {
            s.webpush_seal_memory
                .contains_key(&(anchor, oldest_hash, origin_hash))
        });
        assert!(
            !seal_survived,
            "the evicted device's seals must be dropped with its subscription"
        );
    }
}
