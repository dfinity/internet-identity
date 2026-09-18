use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::bytes::ByteVec;
use minicbor::{Decode, Encode};

/// A device's pre-signed VAPID JWTs. II holds no VAPID private key: only the raw 64-byte
/// signatures are stored, and the sender reassembles each compact JWT by templating the
/// deterministic claims (`exp = issued_at_ns + (index + 1) * window`). That byte layout
/// is a wire contract with the frontend.
///
/// Lives inside [`super::subscription::StorableWebPushSubscription`].
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableWebPushJwtPool {
    /// Raw ECDSA P-256 signatures (r‖s, 64 bytes each), in window order.
    /// `ByteVec` so each signature encodes as a compact CBOR byte string rather
    /// than an array of integers (the latter ~doubles high-entropy bytes).
    #[n(0)]
    pub signatures: Vec<ByteVec>,
    /// Wall-clock time (ns) the device minted this pool. Window `i` expires at
    /// `issued_at_ns + (i + 1) * window_ns`.
    #[n(1)]
    pub issued_at_ns: StorableTimestamp,
}
