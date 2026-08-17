use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// A device's pre-signed VAPID JWTs. II holds no VAPID private key: only the
/// raw 64-byte signatures are stored; the sender reassembles each compact JWT
/// by templating the deterministic claims (`exp = issued_at_ns + (index + 1) *
/// window`). That byte layout is a wire contract with the frontend — change one
/// side only and every signature stops verifying.
///
/// Own map (not the subscription row) so listing devices skips ~2 KB/row.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorablePushJwtPool {
    /// Raw ECDSA P-256 signatures (r‖s, 64 bytes each), in window order.
    #[n(0)]
    pub signatures: Vec<Vec<u8>>,
    /// Wall-clock time (ns) the device minted this pool. Window `i` expires at
    /// `issued_at_ns + (i + 1) * window_ns`.
    #[n(1)]
    pub issued_at_ns: Timestamp,
}

impl Storable for StorablePushJwtPool {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorablePushJwtPool");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePushJwtPool")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let pool = StorablePushJwtPool {
            signatures: vec![vec![7u8; 64], vec![8u8; 64]],
            issued_at_ns: 1_234_567_890,
        };

        assert_eq!(StorablePushJwtPool::from_bytes(pool.to_bytes()), pool);
    }
}
