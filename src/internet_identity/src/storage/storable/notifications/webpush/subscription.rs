use crate::storage::storable::notifications::browser_queue::StorableQueuedNotification;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// Where a push notification goes, and the VAPID JWT pool it is authorized with. Held by a
/// browser entry, and by a Home Screen app's record, which has one for every identity.
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
    /// A queue some records carry here. The entry holds the queue, so this is read only.
    #[n(4)]
    pub notifications: Option<Vec<StorableQueuedNotification>>,
    /// A re-wake stamp some records carry here, read and never written.
    #[n(5)]
    pub woken_again_at_ns: Option<StorableTimestamp>,
}
