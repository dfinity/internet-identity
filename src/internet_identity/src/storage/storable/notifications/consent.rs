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
    /// Last time a send to this app was accepted (ns). `None` = never sent.
    /// Updated throttled on the send path, so it's coarse, not exact.
    #[n(2)]
    pub last_sent_ns: Option<Timestamp>,
    /// The user muted this app without revoking it: consent stays, but the
    /// send path skips it. `None`/`Some(false)` = not muted.
    #[n(3)]
    pub muted: Option<bool>,
    /// The account the consent was granted from, so a device that subscribes
    /// later can mint the service worker's pull credential for the same account
    /// the app knows. `None` = the default account.
    #[n(4)]
    pub account_number: Option<u64>,
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

    // origin (≤ MAX_ORIGIN_LEN = 255) + a timestamp, two optional u64s and an
    // optional bool, CBOR-encoded: 291 bytes with every field at its maximum.
    const BOUND: Bound = Bound::Bounded {
        max_size: 448,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let consent = StorableNotificationConsent {
            origin: "https://example.com".to_string(),
            granted_at_ns: 1_234_567_890,
            last_sent_ns: Some(1_234_999_999),
            muted: Some(true),
            account_number: Some(7),
        };

        let decoded = StorableNotificationConsent::from_bytes(consent.to_bytes());

        assert_eq!(decoded, consent);
    }
}
