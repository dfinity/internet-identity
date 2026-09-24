//! Stable storage for pending notification deliveries.

use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::timestamp::StorableTimestamp;
use candid::Principal;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{AnchorNumber, NotificationId};
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Deadline, origin hash, notification ID, then the recipient principal.
const PROCESSING_KEY_MAX_SIZE: usize = 8 + 32 + 8 + Principal::MAX_LENGTH_IN_BYTES;

/// Three fields plus CBOR framing.
const PROCESSING_ENTRY_SIZE: u32 = 32;

/// Deadline-first ordering supports bounded expiry sweeps and dispatch batches.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableProcessingKey {
    pub expires_at_ns: StorableTimestamp,
    pub origin: StorableOriginSha256,
    pub notification_id: NotificationId,
    /// Last, so its variable length needs no prefix.
    pub recipient: Principal,
}

impl Storable for StorableProcessingKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::with_capacity(PROCESSING_KEY_MAX_SIZE);
        buffer.extend(self.expires_at_ns.to_be_bytes());
        buffer.extend(self.origin.to_bytes().as_ref());
        buffer.extend(self.notification_id.to_be_bytes());
        buffer.extend(self.recipient.as_slice());
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
            origin: StorableOriginSha256::from_bytes(Cow::Borrowed(&bytes[8..40])),
            notification_id: field(40),
            recipient: Principal::try_from_slice(&bytes[48..])
                .expect("failed to read a processing key recipient"),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: PROCESSING_KEY_MAX_SIZE as u32,
        is_fixed_size: false,
    };
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableProcessingEntry {
    #[n(0)]
    pub received_at_ns: StorableTimestamp,
    /// Urgency level, least urgent first. An unknown one reads as the least urgent.
    #[n(1)]
    pub urgency: u8,
    /// The identity behind the key's recipient, whose browsers are woken.
    #[n(2)]
    pub anchor_number: AnchorNumber,
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
        recipient: &[u8],
        origin: &str,
        notification_id: u64,
    ) -> StorableProcessingKey {
        StorableProcessingKey {
            expires_at_ns,
            origin: StorableOriginSha256::from_origin(&origin.to_string()),
            notification_id,
            recipient: Principal::from_slice(recipient),
        }
    }

    #[test]
    fn a_key_round_trips_through_its_bytes_within_its_bound() {
        for recipient in [&[][..], &[7], &[0xff; Principal::MAX_LENGTH_IN_BYTES]] {
            let original = key(u64::MAX, recipient, "https://app.example", 7);
            let bytes = original.to_bytes();

            assert!(bytes.len() <= PROCESSING_KEY_MAX_SIZE);
            assert_eq!(StorableProcessingKey::from_bytes(bytes), original);
        }
    }

    #[test]
    fn the_encoding_sorts_by_deadline_before_anything_else() {
        let sooner = key(1, &[0xff; 29], "https://z.example", u64::MAX);
        let later = key(2, &[], "https://a.example", 0);

        assert!(sooner.to_bytes() < later.to_bytes());
        assert!(sooner < later);
    }

    #[test]
    fn entries_sharing_a_deadline_keep_distinct_keys() {
        let deadline = 5;
        let distinct = [
            key(deadline, &[1], "https://a.example", 1),
            key(deadline, &[2], "https://a.example", 1),
            key(deadline, &[1, 0], "https://a.example", 1),
            key(deadline, &[1], "https://b.example", 1),
            key(deadline, &[1], "https://a.example", 2),
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
            anchor_number: u64::MAX,
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
