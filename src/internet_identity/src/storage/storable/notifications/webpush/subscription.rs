use crate::storage::storable::notifications::browser_queue::StorableQueuedNotification;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// Where a push notification goes, and the VAPID JWT pool it is authorized with.
///
/// Held by a browser entry, and also by a Home Screen app's record: an installed app has
/// one service worker registration however many identities it delivers for, so it has
/// one of these and they all deliver through it. What is *waiting* is not here, because
/// that belongs to the identity rather than to the address.
///
/// Keying by the holder rather than by the endpoint is what makes the endpoint mutable:
/// re-creating a push subscription keeps the entry.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableWebPushSubscription {
    /// Relay endpoint URL (≤512 bytes, validated at insert). Mutable: the browser
    /// rewrites it whenever the push service hands it a new one.
    #[n(0)]
    pub endpoint: String,
    /// Written per (re-)subscribe, so the manage screen can say since when.
    #[n(1)]
    pub created_at_ns: StorableTimestamp,
    /// The `applicationServerKey` the browser minted this subscription with
    /// (uncompressed SEC1 P-256, 65 bytes). Sent as the relay's `k=`; the relay
    /// rejects a push whose `k` doesn't match.
    #[cbor(n(2), with = "minicbor::bytes")]
    pub vapid_public_key: Vec<u8>,
    #[n(3)]
    pub jwt_pool: StorableWebPushJwtPool,
    /// A queue some records carry here. The entry holds the queue, so this is read and
    /// never written.
    #[n(4)]
    pub notifications: Option<Vec<StorableQueuedNotification>>,
    /// A re-wake stamp some records carry here, read and never written for the same
    /// reason.
    #[n(5)]
    pub woken_again_at_ns: Option<StorableTimestamp>,
}
