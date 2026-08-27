use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// One browser's push subscription, keyed `(anchor, sha256(endpoint))` so a
/// re-subscribe overwrites in place. The device's VAPID JWT pool rides on the
/// same row: it shares this key and lifetime, so a single map is one write path
/// and no chance of the two drifting.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableWebPushSubscription {
    /// Denormalised so a range scan's value carries its owner.
    #[n(0)]
    pub anchor: StorableAnchorNumber,
    /// Relay endpoint URL (≤1 KiB, validated at insert).
    #[n(1)]
    pub endpoint: String,
    /// Device public key: uncompressed SEC1 P-256 (65 bytes).
    #[cbor(n(2), with = "minicbor::bytes")]
    pub p256dh: Vec<u8>,
    /// Auth secret from `subscription.getKey("auth")` (16 bytes).
    #[cbor(n(3), with = "minicbor::bytes")]
    pub auth: Vec<u8>,
    /// The `applicationServerKey` the browser minted this subscription with
    /// (uncompressed SEC1 P-256, 65 bytes). Sent as the relay's `k=`; the relay
    /// rejects a push whose `k` doesn't match.
    #[cbor(n(5), with = "minicbor::bytes")]
    pub vapid_public_key: Vec<u8>,
    /// Written per (re-)subscribe; the eviction tie-breaker at the cap.
    #[n(4)]
    pub created_at_ns: Timestamp,
    /// The device's pre-signed VAPID JWT pool. `Option` so a future version can
    /// stop storing it (set to `None`) without a storable migration.
    #[n(6)]
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

    // Subscription (~1200 bytes: endpoint ≤ 1 KiB + two 65-byte keys + a 16-byte
    // secret + anchor + timestamp) plus the embedded JWT pool (~2100 bytes: 30
    // signatures of 64 bytes + timestamp), CBOR-encoded.
    const BOUND: Bound = Bound::Bounded {
        max_size: 3700,
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
            p256dh: vec![4u8; 65],
            auth: vec![9u8; 16],
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
        // Max endpoint, both keys, and a full 30-signature pool (all validated
        // before storage).
        let subscription = StorableWebPushSubscription {
            anchor: u64::MAX,
            endpoint: "x".repeat(1024),
            // High-entropy bytes (values >= 24): under a CBOR int-array these
            // would take ~2 bytes each and blow the bound; as byte strings they
            // don't. Guards against dropping the `minicbor::bytes` annotation.
            p256dh: vec![0xABu8; 65],
            auth: vec![0xABu8; 16],
            created_at_ns: u64::MAX,
            vapid_public_key: vec![0xABu8; 65],
            jwt_pool: Some(StorableWebPushJwtPool {
                signatures: vec![ByteVec::from(vec![0xABu8; 64]); 30],
                issued_at_ns: u64::MAX,
            }),
        };
        assert!(subscription.to_bytes().len() <= max_size as usize);
    }
}
