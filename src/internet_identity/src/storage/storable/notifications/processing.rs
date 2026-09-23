//! How a notification the canister has taken on is stored while it waits to be sent.

use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::timestamp::StorableTimestamp;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::AnchorNumber;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Deadline, recipient, origin hash, notification id.
const PROCESSING_KEY_SIZE: usize = 8 + 8 + 32 + 8;

/// Room for the entry's two fields plus CBOR framing, with slack for a third.
const PROCESSING_ENTRY_SIZE: u32 = 32;

/// Orders the queue by deadline, so the expiry sweep and the next batch to send are
/// both prefixes of one scan. Encoded big-endian, so the byte order the map sorts by
/// is the numeric order.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableProcessingKey {
    pub expires_at_ns: StorableTimestamp,
    pub recipient: AnchorNumber,
    pub origin: StorableOriginSha256,
    pub notification_id: u64,
}

impl Storable for StorableProcessingKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::with_capacity(PROCESSING_KEY_SIZE);
        buffer.extend(self.expires_at_ns.to_be_bytes());
        buffer.extend(self.recipient.to_be_bytes());
        buffer.extend(self.origin.to_bytes().as_ref());
        buffer.extend(self.notification_id.to_be_bytes());
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        let field = |from: usize| {
            u64::from_be_bytes(
                TryFrom::try_from(&bytes[from..from + 8])
                    .expect("failed to read a processing key field"),
            )
        };
        Self {
            expires_at_ns: field(0),
            recipient: field(8),
            origin: StorableOriginSha256::from_bytes(Cow::Borrowed(&bytes[16..48])),
            notification_id: field(48),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: PROCESSING_KEY_SIZE as u32,
        is_fixed_size: true,
    };
}

/// What the key does not already carry: when the app submitted the notification, and
/// the urgency the push has to go out with.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableProcessingEntry {
    #[n(0)]
    pub received_at_ns: StorableTimestamp,
    /// Priority index, lowest first. Out of range reads as the least urgent level.
    #[n(1)]
    pub urgency: u8,
}

impl Storable for StorableProcessingEntry {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableProcessingEntry");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableProcessingEntry")
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: PROCESSING_ENTRY_SIZE,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn key(
        expires_at_ns: u64,
        recipient: u64,
        origin: &str,
        notification_id: u64,
    ) -> StorableProcessingKey {
        StorableProcessingKey {
            expires_at_ns,
            recipient,
            origin: StorableOriginSha256::from_origin(&origin.to_string()),
            notification_id,
        }
    }

    #[test]
    fn a_key_round_trips_through_its_bytes() {
        let original = key(u64::MAX, 10_000, "https://app.example", 7);

        assert_eq!(
            StorableProcessingKey::from_bytes(original.to_bytes()),
            original
        );
    }

    /// The map sorts by encoded bytes, so a scan is only in deadline order if the
    /// encoding sorts the same way the field does.
    #[test]
    fn the_encoding_sorts_by_deadline_before_anything_else() {
        let sooner = key(1, u64::MAX, "https://z.example", u64::MAX);
        let later = key(2, 0, "https://a.example", 0);

        assert!(sooner.to_bytes() < later.to_bytes());
        assert!(sooner < later);
    }

    /// Two notifications sharing a deadline have to stay two entries.
    #[test]
    fn entries_sharing_a_deadline_keep_distinct_keys() {
        let deadline = 5;
        let distinct = [
            key(deadline, 1, "https://a.example", 1),
            key(deadline, 2, "https://a.example", 1),
            key(deadline, 1, "https://b.example", 1),
            key(deadline, 1, "https://a.example", 2),
        ];

        for (index, one) in distinct.iter().enumerate() {
            for other in &distinct[index + 1..] {
                assert_ne!(one.to_bytes(), other.to_bytes());
            }
        }
    }

    #[test]
    fn an_entry_round_trips_within_its_bound() {
        let original = StorableProcessingEntry {
            received_at_ns: u64::MAX,
            urgency: u8::MAX,
        };
        let bytes = original.to_bytes();

        assert!(
            bytes.len() as u32 <= PROCESSING_ENTRY_SIZE,
            "a largest-case entry encodes to {} bytes, over its bound",
            bytes.len()
        );
        assert_eq!(StorableProcessingEntry::from_bytes(bytes), original);
    }
}
