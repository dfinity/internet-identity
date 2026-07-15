use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// One browser's push subscription — everything the canister needs to
/// deliver an encrypted Web Push message to it.
///
/// Endpoint is the relay URL the browser handed us at
/// `pushManager.subscribe()` time (Google/Apple/Mozilla). `p256dh` is the
/// device's SEC1-uncompressed P-256 public key (65 bytes; the browser
/// holds the private half in its Service Worker). `auth` is the 16-byte
/// shared secret that mixes into the RFC 8291 HKDF stage.
///
/// Keyed by `(anchor, endpoint_sha256)` in the parent map so re-subscribing
/// the same browser (same endpoint URL) overwrites in place rather than
/// duplicating.
#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorablePushSubscription {
    /// Anchor that owns this subscription. Denormalised so the value
    /// alone tells us the owner during iteration.
    #[n(0)]
    pub anchor: StorableAnchorNumber,
    /// The relay endpoint URL. Bounded at 1 KiB in the handler; the
    /// map stores whatever we received.
    #[n(1)]
    pub endpoint: String,
    /// Uncompressed SEC1 P-256 point (65 bytes). Validated on the
    /// handler side before insert.
    #[n(2)]
    pub p256dh: Vec<u8>,
    /// Auth secret from `subscription.getKey("auth")` (16 bytes).
    #[n(3)]
    pub auth: Vec<u8>,
    /// Wall-clock timestamp (ns) when the subscription was recorded.
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
