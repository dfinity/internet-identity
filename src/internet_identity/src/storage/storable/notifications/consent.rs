use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Per-`(anchor, origin)` consent marker; presence means the user allowed
/// `origin` to notify this identity. `origin` is stored plaintext because the
/// map key hashes it and Settings needs to list origins back.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableNotificationConsent {
    #[n(0)]
    pub origin: FrontendHostname,
    #[n(1)]
    pub granted_at_ns: Timestamp,
}

impl Storable for StorableNotificationConsent {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableNotificationConsent");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableNotificationConsent")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let consent = StorableNotificationConsent {
            origin: "https://example.com".to_string(),
            granted_at_ns: 1_234_567_890,
        };

        let decoded = StorableNotificationConsent::from_bytes(consent.to_bytes());

        assert_eq!(decoded, consent);
    }
}
