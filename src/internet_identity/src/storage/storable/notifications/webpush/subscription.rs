use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// Where a push notification goes, and what its payload is sealed against. Held by a
/// browser entry, and by a Home Screen app's record, which has one for every identity.
///
/// Two states, because a subscribe takes two calls. The first mints a key for the browser
/// to encrypt its `auth` secret to and leaves the private half here; the second spends it
/// and writes what can be delivered to. A browser replacing its subscription passes back
/// through `Pending`, which is why nothing outside this module reads a subscription
/// without asking for the ready one.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableWebPushSubscription {
    /// A key fetch waiting for the subscribe it was minted for. Expires on its own.
    #[n(0)]
    Pending {
        /// The private half of the key the browser encrypts `auth` to.
        #[cbor(n(0), with = "minicbor::bytes")]
        transport_scalar: Vec<u8>,
        /// Expanded into this subscription's first sender key once a subscribe arrives.
        #[cbor(n(1), with = "minicbor::bytes")]
        sender_seed: Vec<u8>,
        #[n(2)]
        minted_at_ns: StorableTimestamp,
        /// What was registered when this fetch arrived, so a re-subscribe at the same
        /// address keeps its age and its queue.
        #[n(3)]
        replacing: Option<Box<StorableWebPushSubscription>>,
    },
    /// Subscribed, and what a wake-up is posted and sealed against.
    #[n(1)]
    Ready {
        /// Relay endpoint URL (≤512 bytes, validated at insert). Mutable: the browser
        /// rewrites it whenever the push service hands it a new one.
        #[n(0)]
        endpoint: String,
        /// Written per (re-)subscribe, so the manage screen can say since when.
        #[n(1)]
        created_at_ns: StorableTimestamp,
        /// RFC 8291 §3.4 input keying material, derived here from `subscriber_key`,
        /// `auth` and a sender key belonging to the pass that last refreshed it. Sealing
        /// a payload with it costs two HKDFs and one AES-GCM, so the ECDH runs only on a
        /// refresh.
        #[cbor(n(2), with = "minicbor::bytes")]
        payload_ikm: Vec<u8>,
        /// The public half of that sender key, carried as the `keyid` the browser derives
        /// its own half against (uncompressed SEC1 P-256, 65 bytes).
        #[cbor(n(3), with = "minicbor::bytes")]
        payload_keyid: Vec<u8>,
        /// The subscription's own `p256dh`, kept because every refresh runs the ECDH
        /// against it again.
        #[cbor(n(4), with = "minicbor::bytes")]
        subscriber_key: Vec<u8>,
        /// The subscription's `auth` secret, which arrived encrypted and is the salt of
        /// every derivation of `payload_ikm`.
        #[cbor(n(5), with = "minicbor::bytes")]
        auth: Vec<u8>,
        /// When `payload_ikm` was last derived. A pass refreshes the oldest stamps among
        /// its own destinations, so this is what puts one at the back of that queue.
        #[n(6)]
        last_rotated_ns: StorableTimestamp,
    },
}
