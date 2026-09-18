use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// One browser's push subscription, keyed `(anchor, browser_id)` against the browser
/// registry the anchor already holds. The device's VAPID JWT pool rides on the same row,
/// sharing its key and lifetime.
///
/// Keying by the registry's id rather than by the endpoint is what makes the endpoint
/// mutable: a browser that re-creates its push subscription keeps its row.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableWebPushSubscription {
    /// Denormalised so a range scan's value carries its owner.
    #[n(0)]
    pub anchor: StorableAnchorNumber,
    /// Relay endpoint URL (≤512 bytes, validated at insert). Mutable: the browser
    /// rewrites it whenever the push service hands it a new one.
    #[n(1)]
    pub endpoint: String,
    /// Written per (re-)subscribe, so the manage screen can say since when.
    #[n(2)]
    pub created_at_ns: Timestamp,
    /// The `applicationServerKey` the browser minted this subscription with
    /// (uncompressed SEC1 P-256, 65 bytes). Sent as the relay's `k=`; the relay
    /// rejects a push whose `k` doesn't match.
    #[cbor(n(3), with = "minicbor::bytes")]
    pub vapid_public_key: Vec<u8>,
    /// The device's pre-signed VAPID JWT pool. `Option` so a future version can
    /// stop storing it (set to `None`) without a storable migration.
    #[n(4)]
    pub jwt_pool: Option<StorableWebPushJwtPool>,
}

impl Storable for StorableWebPushSubscription {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableWebPushSubscription");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableWebPushSubscription")
    }

    // Subscription (~600 bytes: endpoint ≤ 512 bytes + a 65-byte key + anchor +
    // timestamp) plus the embedded JWT pool (~2000 bytes: 30 signatures of 64
    // bytes + timestamp), CBOR-encoded. Worst case measured by
    // `the_largest_possible_subscription_fits_the_bound`.
    const BOUND: Bound = Bound::Bounded {
        max_size: 2688,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use minicbor::bytes::ByteVec;

    fn sample(jwt_pool: Option<StorableWebPushJwtPool>) -> StorableWebPushSubscription {
        StorableWebPushSubscription {
            anchor: 42,
            endpoint: "https://fcm.googleapis.com/fcm/send/abc".to_string(),
            created_at_ns: 1_234_567_890,
            vapid_public_key: vec![4u8; 65],
            jwt_pool,
        }
    }

    #[test]
    fn round_trips_through_storable() {
        for pool in [
            None,
            Some(StorableWebPushJwtPool {
                signatures: vec![ByteVec::from(vec![7u8; 64]), ByteVec::from(vec![8u8; 64])],
                issued_at_ns: 42,
            }),
        ] {
            let subscription = sample(pool);
            assert_eq!(
                StorableWebPushSubscription::from_bytes(subscription.to_bytes()),
                subscription
            );
        }
    }

    #[test]
    fn the_largest_possible_subscription_fits_the_bound() {
        let Bound::Bounded { max_size, .. } = StorableWebPushSubscription::BOUND else {
            panic!("StorableWebPushSubscription must stay bounded");
        };
        // Max endpoint, the VAPID key, and a full 30-signature pool (all
        // validated before storage).
        let subscription = StorableWebPushSubscription {
            anchor: u64::MAX,
            endpoint: "x".repeat(512),
            created_at_ns: u64::MAX,
            // High-entropy bytes (values >= 24) take ~2 bytes each under a CBOR
            // int-array, so this fails if the `minicbor::bytes` annotation is dropped.
            vapid_public_key: vec![0xABu8; 65],
            jwt_pool: Some(StorableWebPushJwtPool {
                signatures: vec![ByteVec::from(vec![0xABu8; 64]); 30],
                issued_at_ns: u64::MAX,
            }),
        };
        let encoded = subscription.to_bytes().len();
        assert!(
            encoded <= max_size as usize,
            "encoded {encoded} exceeds the bound {max_size}"
        );
    }
}
