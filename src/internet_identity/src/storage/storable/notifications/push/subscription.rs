use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// One browser's push subscription, keyed `(anchor, sha256(endpoint))` so a
/// re-subscribe overwrites in place.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorablePushSubscription {
    /// Denormalised so a range scan's value carries its owner.
    #[n(0)]
    pub anchor: StorableAnchorNumber,
    /// Relay endpoint URL (≤1 KiB, validated at insert).
    #[n(1)]
    pub endpoint: String,
    /// Device public key: uncompressed SEC1 P-256 (65 bytes).
    #[n(2)]
    pub p256dh: Vec<u8>,
    /// Auth secret from `subscription.getKey("auth")` (16 bytes).
    #[n(3)]
    pub auth: Vec<u8>,
    /// The `applicationServerKey` the browser minted this subscription with
    /// (uncompressed SEC1 P-256, 65 bytes). Sent as the relay's `k=`; the relay
    /// rejects a push whose `k` doesn't match.
    #[n(5)]
    pub vapid_public_key: Vec<u8>,
    /// Written per (re-)subscribe; the eviction tie-breaker at the cap.
    #[n(4)]
    pub created_at_ns: Timestamp,
}

impl Storable for StorablePushSubscription {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorablePushSubscription");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePushSubscription")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let subscription = StorablePushSubscription {
            anchor: 42,
            endpoint: "https://fcm.googleapis.com/fcm/send/abc".to_string(),
            p256dh: vec![4u8; 65],
            auth: vec![9u8; 16],
            created_at_ns: 1_234_567_890,
            vapid_public_key: vec![4u8; 65],
        };

        let decoded = StorablePushSubscription::from_bytes(subscription.to_bytes());

        assert_eq!(decoded, subscription);
    }
}
