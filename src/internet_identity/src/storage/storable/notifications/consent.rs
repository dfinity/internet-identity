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
    // A CBOR map, so later work adds a field by taking the next free tag and nothing
    // already written has to be migrated. Tags 2, 3 and 4 are spoken for and must not
    // be reused for anything else: a send-path `last_sent_ns`, a mute flag, and the
    // account a grant came from. Each was removed because nothing reads it yet, and
    // each returns under its own number.
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

    // origin (≤ MAX_ORIGIN_LEN = 255, 2-byte header) and a timestamp, each behind a
    // 1-byte key, inside a map header. `max_size_holds` pins the worst case; the
    // headroom is for the fields listed above coming back.
    const BOUND: Bound = Bound::Bounded {
        max_size: 384,
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
        };

        let decoded = StorableNotificationConsent::from_bytes(consent.to_bytes());

        assert_eq!(decoded, consent);
    }

    /// `StableBTreeMap::insert` asserts the encoding fits the bound, and that
    /// assert is live in release, so an undersized bound traps rather than
    /// erroring. This fails at the bound instead.
    #[test]
    fn max_size_holds() {
        let consent = StorableNotificationConsent {
            origin: format!(
                "https://{}",
                "a".repeat(crate::notifications::MAX_ORIGIN_LEN - 8)
            ),
            granted_at_ns: u64::MAX,
        };
        assert_eq!(consent.origin.len(), crate::notifications::MAX_ORIGIN_LEN);

        let Bound::Bounded { max_size, .. } = StorableNotificationConsent::BOUND else {
            panic!("consent must stay bounded");
        };
        let encoded = consent.to_bytes().len();
        assert!(
            encoded <= max_size as usize,
            "encoded {encoded} bytes exceeds the {max_size}-byte bound"
        );
    }
}
