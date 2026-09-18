use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// One browser's push subscription, stored on the browser entry itself. The device's
/// VAPID JWT pool rides along, sharing its lifetime: signing the browser out or letting
/// the registry evict it takes both.
///
/// Keying by the browser rather than by the endpoint is what makes the endpoint
/// mutable: a browser that re-creates its push subscription keeps its entry.
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
}
