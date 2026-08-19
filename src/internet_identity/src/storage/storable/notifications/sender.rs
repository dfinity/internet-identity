use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The origin a sender canister is authorized to notify for, cached from the
/// dApp's `/.well-known/ii-notification-senders` at consent time. A canister
/// principal doesn't encode its web origin, so this reverse map is how a
/// `notification_send` caller is resolved to an origin.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableSenderOrigin {
    #[n(0)]
    pub origin: FrontendHostname,
    #[n(1)]
    pub cached_at_ns: Timestamp,
}

impl Storable for StorableSenderOrigin {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSenderOrigin");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSenderOrigin")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let value = StorableSenderOrigin {
            origin: "https://app.example".to_string(),
            cached_at_ns: 42,
        };
        assert_eq!(StorableSenderOrigin::from_bytes(value.to_bytes()), value);
    }
}
