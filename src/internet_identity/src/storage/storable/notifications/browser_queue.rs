//! Stable storage for the notifications a browser's service worker has yet to take.

use crate::storage::storable::timestamp::StorableTimestamp;
use candid::Principal;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, NotificationId,
};
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Anchor, browser, then sequence.
const KEY_SIZE: usize = 8 + 4 + 8;

/// Deadline, then the queue key.
const EXPIRY_KEY_SIZE: usize = 8 + KEY_SIZE;

/// Map header, then the principal as a byte string and two maximal `u64`s, each behind
/// a one-byte field key.
const ENTRY_MAX_SIZE: u32 = 1 + (1 + 2 + Principal::MAX_LENGTH_IN_BYTES as u32) + 2 * (1 + 9);

/// One browser's queue is a contiguous range, oldest first.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableBrowserNotificationKey {
    pub anchor_number: AnchorNumber,
    pub browser_id: BrowserId,
    pub sequence: u64,
}

impl StorableBrowserNotificationKey {
    fn extend(&self, buffer: &mut Vec<u8>) {
        buffer.extend(self.anchor_number.to_be_bytes());
        buffer.extend(self.browser_id.to_be_bytes());
        buffer.extend(self.sequence.to_be_bytes());
    }

    fn read(bytes: &[u8]) -> Self {
        Self {
            anchor_number: u64::from_be_bytes(read_array(&bytes[0..8])),
            browser_id: u32::from_be_bytes(read_array(&bytes[8..12])),
            sequence: u64::from_be_bytes(read_array(&bytes[12..20])),
        }
    }
}

impl Storable for StorableBrowserNotificationKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::with_capacity(KEY_SIZE);
        self.extend(&mut buffer);
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        Self::read(&bytes)
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: KEY_SIZE as u32,
        is_fixed_size: true,
    };
}

/// Deadline first, so what has expired across every browser is a prefix.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableBrowserNotificationExpiry {
    pub expires_at_ns: StorableTimestamp,
    pub key: StorableBrowserNotificationKey,
}

impl Storable for StorableBrowserNotificationExpiry {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::with_capacity(EXPIRY_KEY_SIZE);
        buffer.extend(self.expires_at_ns.to_be_bytes());
        self.key.extend(&mut buffer);
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        Self {
            expires_at_ns: u64::from_be_bytes(read_array(&bytes[0..8])),
            key: StorableBrowserNotificationKey::read(&bytes[8..]),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: EXPIRY_KEY_SIZE as u32,
        is_fixed_size: true,
    };
}

fn read_array<const N: usize>(bytes: &[u8]) -> [u8; N] {
    TryFrom::try_from(bytes).expect("failed to read a browser notification key field")
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableBrowserNotification {
    /// The account principal the app addressed, resolved to its origin and account when
    /// the service worker takes it.
    #[cbor(n(0), with = "minicbor::bytes")]
    pub recipient: Vec<u8>,
    #[n(1)]
    pub notification_id: NotificationId,
    #[n(2)]
    pub expires_at_ns: StorableTimestamp,
}

impl Storable for StorableBrowserNotification {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableBrowserNotification");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableBrowserNotification")
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: ENTRY_MAX_SIZE,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn key(anchor_number: u64, browser_id: u32, sequence: u64) -> StorableBrowserNotificationKey {
        StorableBrowserNotificationKey {
            anchor_number,
            browser_id,
            sequence,
        }
    }

    #[test]
    fn a_key_round_trips_through_its_fixed_size() {
        let original = key(u64::MAX, u32::MAX, u64::MAX);
        let bytes = original.to_bytes();

        assert_eq!(bytes.len(), KEY_SIZE);
        assert_eq!(StorableBrowserNotificationKey::from_bytes(bytes), original);
    }

    #[test]
    fn a_browsers_queue_sorts_together_and_oldest_first() {
        let ordered = [
            key(1, 1, 1),
            key(1, 1, u64::MAX),
            key(1, 2, 0),
            key(2, 0, 0),
        ];

        for pair in ordered.windows(2) {
            assert!(pair[0].to_bytes() < pair[1].to_bytes());
            assert!(pair[0] < pair[1]);
        }
    }

    #[test]
    fn an_expiry_key_round_trips_and_sorts_by_deadline_first() {
        let sooner = StorableBrowserNotificationExpiry {
            expires_at_ns: 1,
            key: key(u64::MAX, u32::MAX, u64::MAX),
        };
        let later = StorableBrowserNotificationExpiry {
            expires_at_ns: 2,
            key: key(0, 0, 0),
        };

        assert_eq!(sooner.to_bytes().len(), EXPIRY_KEY_SIZE);
        assert_eq!(
            StorableBrowserNotificationExpiry::from_bytes(sooner.to_bytes()),
            sooner
        );
        assert!(sooner.to_bytes() < later.to_bytes());
        assert!(sooner < later);
    }

    #[test]
    fn the_largest_entry_fits_the_declared_bound() {
        let largest = StorableBrowserNotification {
            recipient: vec![0xff; Principal::MAX_LENGTH_IN_BYTES],
            notification_id: u64::MAX,
            expires_at_ns: u64::MAX,
        };
        let bytes = largest.to_bytes();

        assert!(
            bytes.len() as u32 <= ENTRY_MAX_SIZE,
            "a largest-case entry encodes to {} bytes, over its bound",
            bytes.len()
        );
        assert_eq!(StorableBrowserNotification::from_bytes(bytes), largest);
    }
}
