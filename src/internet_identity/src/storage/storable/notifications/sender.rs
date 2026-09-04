use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// A verified `(canister, origin)` sender binding — the value of a map keyed by
/// `(sender_principal, origin_sha256)`. The key's presence means that origin's
/// `/.well-known/ii-notification-senders` listed this canister (fetched at
/// consent). The send path additionally requires the caller to declare that same
/// origin, so trust is two-way and an origin can't authorize a canister it does
/// not own. Only the fetch time is stored.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableSenderBinding {
    #[n(0)]
    pub cached_at_ns: Timestamp,
}

impl Storable for StorableSenderBinding {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSenderBinding");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSenderBinding")
    }

    // A cbor map with one u64 field is at most ~11 bytes.
    const BOUND: Bound = Bound::Bounded {
        max_size: 16,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let value = StorableSenderBinding { cached_at_ns: 42 };
        assert_eq!(StorableSenderBinding::from_bytes(value.to_bytes()), value);
    }
}
