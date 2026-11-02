use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// Where a push notification goes, and what its payload is sealed against. Held by a
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
    /// RFC 8291 §3.4 input keying material, derived here from `subscriber_key`, `auth`
    /// and a sender key belonging to the pass that last refreshed it. Sealing a payload
    /// with it costs two HKDFs and one AES-GCM, so the ECDH runs only on a refresh.
    #[cbor(n(4), with = "minicbor::bytes")]
    pub payload_ikm: Vec<u8>,
    /// The public half of that sender key, carried as the `keyid` the browser derives its
    /// own half against (uncompressed SEC1 P-256, 65 bytes).
    #[cbor(n(5), with = "minicbor::bytes")]
    pub payload_keyid: Vec<u8>,
    /// The subscription's own `p256dh`, kept because every refresh runs the ECDH against
    /// it again.
    #[cbor(n(6), with = "minicbor::bytes")]
    pub subscriber_key: Vec<u8>,
    /// The subscription's `auth` secret, which arrived encrypted and is the salt of every
    /// derivation of `payload_ikm`.
    #[cbor(n(7), with = "minicbor::bytes")]
    pub auth: Vec<u8>,
    /// When `payload_ikm` was last derived. A pass refreshes the oldest stamps among its
    /// own destinations, so this is what puts one at the back of that queue.
    #[n(8)]
    pub last_rotated_ns: StorableTimestamp,
}
