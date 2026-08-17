//! The device-signed VAPID JWT pool. II holds no VAPID private key: the device
//! signs one JWT per validity window and uploads the raw signatures; the send
//! path spends the one covering "now". Only signatures are stored, so the
//! signing-input byte layout is a wire contract with the frontend.

use super::super::{authorize_query, authorize_update, ensure_enabled, feature_enabled};
use super::validate_jwt_pool;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::notifications::push::endpoint_hash::StorableEndpointSha256;
use crate::storage::storable::notifications::push::jwt_pool::StorablePushJwtPool;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

/// What a device still has signed, so the frontend knows when to top up.
#[derive(candid::CandidType, serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub struct JwtPoolStatus {
    /// Windows still signed and unspent.
    pub remaining: u32,
    /// When the device minted this pool; window `i` expires at
    /// `issued_at_ns + (i + 1) * window`.
    pub issued_at_ns: Timestamp,
}

fn replace_jwt_pool(
    anchor_number: AnchorNumber,
    endpoint: &str,
    jwt_signatures: Vec<Vec<u8>>,
    jwt_issued_at_ns: Timestamp,
) -> Result<(), String> {
    validate_jwt_pool(&jwt_signatures)?;
    let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
    let key = (anchor_number, endpoint_hash);

    storage_borrow_mut(|storage| {
        // Refuse to strand a pool with no subscription to spend it on.
        if storage.push_subscriptions_memory.get(&key).is_none() {
            return Err("no subscription for that endpoint".to_string());
        }
        storage.push_jwt_pool_memory.insert(
            key,
            StorablePushJwtPool {
                signatures: jwt_signatures,
                issued_at_ns: jwt_issued_at_ns,
            },
        );
        Ok(())
    })
}

fn jwt_pool_status(anchor_number: AnchorNumber, endpoint: &str) -> Option<(u32, Timestamp)> {
    let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
    storage_borrow(|storage| {
        storage
            .push_jwt_pool_memory
            .get(&(anchor_number, endpoint_hash))
            .map(|pool| (pool.signatures.len() as u32, pool.issued_at_ns))
    })
}

pub fn refresh_jwts(
    anchor_number: AnchorNumber,
    endpoint: String,
    jwt_signatures: Vec<Vec<u8>>,
    jwt_issued_at_ns: Timestamp,
) -> Result<(), String> {
    ensure_enabled()?;
    authorize_update(anchor_number)?;
    replace_jwt_pool(anchor_number, &endpoint, jwt_signatures, jwt_issued_at_ns)
}

/// Remaining signed windows and when the pool was minted.
pub fn jwt_pool_state(anchor_number: AnchorNumber, endpoint: String) -> Option<JwtPoolStatus> {
    if !feature_enabled() || !authorize_query(anchor_number) {
        return None;
    }
    jwt_pool_status(anchor_number, &endpoint).map(|(remaining, issued_at_ns)| JwtPoolStatus {
        remaining,
        issued_at_ns,
    })
}

#[cfg(test)]
pub(crate) mod test_support {
    use super::*;

    pub(crate) fn pool_len(anchor: AnchorNumber, endpoint: &str) -> Option<u32> {
        jwt_pool_status(anchor, endpoint).map(|(len, _)| len)
    }
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::super::JWT_SIG_LEN;
    use super::test_support::pool_len;
    use super::*;
    use crate::notifications::push::subscription::remove_subscription;
    use crate::notifications::push::MAX_SUBSCRIPTIONS_PER_ANCHOR;
    use crate::notifications::test_setup as setup;

    #[test]
    fn subscribing_stores_the_jwt_pool_and_unsubscribing_drops_it() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a";

        subscribe(anchor, endpoint, 1_000).unwrap();
        assert_eq!(pool_len(anchor, endpoint), Some(3));

        // The pool is worthless without its subscription, so it must not
        // outlive one.
        remove_subscription(anchor, endpoint);
        assert_eq!(pool_len(anchor, endpoint), None);
    }

    #[test]
    fn refreshing_replaces_the_pool_rather_than_appending() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/a";
        subscribe(anchor, endpoint, 1_000).unwrap();

        replace_jwt_pool(anchor, endpoint, vec![vec![5u8; JWT_SIG_LEN]; 7], 2_000).unwrap();

        assert_eq!(pool_len(anchor, endpoint), Some(7));
        assert_eq!(
            jwt_pool_status(anchor, endpoint).map(|(_, at)| at),
            Some(2_000)
        );
    }

    #[test]
    fn refreshing_an_unknown_endpoint_is_refused() {
        setup();
        // Otherwise a caller could park pools for endpoints that never existed.
        assert!(replace_jwt_pool(1, "https://relay.example/nope", valid_pool(), 0).is_err());
    }

    #[test]
    fn evicting_the_oldest_subscription_also_drops_its_pool() {
        setup();
        let anchor = 1;
        for i in 0..MAX_SUBSCRIPTIONS_PER_ANCHOR {
            subscribe(anchor, &format!("https://relay.example/{i}"), i).unwrap();
        }
        subscribe(anchor, "https://relay.example/new", 1_000).unwrap();

        assert_eq!(pool_len(anchor, "https://relay.example/0"), None);
        assert_eq!(pool_len(anchor, "https://relay.example/new"), Some(3));
    }
}
