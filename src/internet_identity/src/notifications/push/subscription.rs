//! One subscription row per device, keyed `(anchor, sha256(endpoint))`.

use super::super::{authorize_update, ensure_enabled};
use super::{
    validate_jwt_pool, validate_param_len, AUTH_LEN, MAX_ENDPOINT_LEN,
    MAX_SUBSCRIPTIONS_PER_ANCHOR, P256DH_LEN, VAPID_PUBKEY_LEN,
};
use crate::state::storage_borrow_mut;
use crate::storage::storable::notifications::push::endpoint_hash::StorableEndpointSha256;
use crate::storage::storable::notifications::push::jwt_pool::StorablePushJwtPool;
use crate::storage::storable::notifications::push::subscription::StorablePushSubscription;
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
) -> Result<(), String> {
    validate_param_len("endpoint", endpoint.len(), 1..=MAX_ENDPOINT_LEN)?;
    validate_param_len("p256dh", p256dh.len(), P256DH_LEN..=P256DH_LEN)?;
    validate_param_len("auth", auth.len(), AUTH_LEN..=AUTH_LEN)?;
    validate_param_len(
        "vapid_public_key",
        vapid_public_key.len(),
        VAPID_PUBKEY_LEN..=VAPID_PUBKEY_LEN,
    )?;
    validate_jwt_pool(&jwt_signatures)?;

    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    let subscription = StorablePushSubscription {
        anchor: anchor_number,
        endpoint,
        p256dh,
        auth,
        created_at_ns: now_ns,
        vapid_public_key,
    };
    let pool = StorablePushJwtPool {
        signatures: jwt_signatures,
        issued_at_ns: jwt_issued_at_ns,
    };

    storage_borrow_mut(|storage| {
        let key = (anchor_number, endpoint_hash.clone());
        let is_new_device = storage.push_subscriptions_memory.get(&key).is_none();

        // A re-subscribe overwrites in place, so only a new device grows the count.
        if is_new_device {
            let range_start = (anchor_number, StorableEndpointSha256::MIN);
            let range_end = (anchor_number, StorableEndpointSha256::MAX);
            let existing: Vec<(StorableEndpointSha256, Timestamp)> = storage
                .push_subscriptions_memory
                .range(range_start..=range_end)
                .map(|((_, hash), sub)| (hash, sub.created_at_ns))
                .collect();

            if existing.len() as u64 >= MAX_SUBSCRIPTIONS_PER_ANCHOR {
                if let Some((oldest_hash, _)) = existing.into_iter().min_by_key(|(_, ts)| *ts) {
                    let evicted = (anchor_number, oldest_hash);
                    storage.push_subscriptions_memory.remove(&evicted);
                    // Drop the pool too — it's useless without its subscription.
                    storage.push_jwt_pool_memory.remove(&evicted);
                }
            }
        }

        storage
            .push_subscriptions_memory
            .insert(key.clone(), subscription);
        storage.push_jwt_pool_memory.insert(key, pool);
    });
    Ok(())
}

/// Drops the subscription and its JWT pool together. Idempotent.
pub(super) fn remove_subscription(anchor_number: AnchorNumber, endpoint: &str) {
    let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
    let key = (anchor_number, endpoint_hash);
    storage_borrow_mut(|storage| {
        storage.push_subscriptions_memory.remove(&key);
        storage.push_jwt_pool_memory.remove(&key);
    });
}

/// Idempotent: re-subscribing the same endpoint overwrites in place.
#[allow(clippy::too_many_arguments)]
pub fn subscribe_device(
    anchor_number: AnchorNumber,
    endpoint: String,
    p256dh: Vec<u8>,
    auth: Vec<u8>,
    vapid_public_key: Vec<u8>,
    jwt_signatures: Vec<Vec<u8>>,
    jwt_issued_at_ns: Timestamp,
) -> Result<(), String> {
    ensure_enabled()?;
    authorize_update(anchor_number)?;
    add_subscription(
        anchor_number,
        endpoint,
        p256dh,
        auth,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
        ic_cdk::api::time(),
    )
}

/// Removes this device's subscription. Idempotent.
pub fn unsubscribe_device(anchor_number: AnchorNumber, endpoint: String) -> Result<(), String> {
    ensure_enabled()?;
    authorize_update(anchor_number)?;
    remove_subscription(anchor_number, &endpoint);
    Ok(())
}

/// Whether the push channel can currently reach this anchor.
pub fn has_any_subscription(anchor_number: AnchorNumber) -> bool {
    let range_start = (anchor_number, StorableEndpointSha256::MIN);
    let range_end = (anchor_number, StorableEndpointSha256::MAX);
    crate::state::storage_borrow(|storage| {
        storage
            .push_subscriptions_memory
            .range(range_start..=range_end)
            .next()
            .is_some()
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
            s.push_subscriptions_memory
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
            s.push_subscriptions_memory
                .get(&(anchor, oldest_hash))
                .is_some()
        });
        assert!(
            !oldest_still_present,
            "the oldest subscription (lowest created_at_ns) should have been evicted"
        );
    }
}
