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
    /// RFC 8291 §3.4 input keying material, which the browser derived from this
    /// subscription's own keys and an ephemeral pair of its own. Sealing a payload with it
    /// costs two HKDFs and one AES-GCM, so the ECDH never runs here.
    #[cbor(n(4), with = "minicbor::bytes")]
    pub payload_ikm: Vec<u8>,
    /// The browser's ephemeral public key, echoed as the `keyid` the browser derives
    /// against (uncompressed SEC1 P-256, 65 bytes).
    #[cbor(n(5), with = "minicbor::bytes")]
    pub payload_keyid: Vec<u8>,
}
