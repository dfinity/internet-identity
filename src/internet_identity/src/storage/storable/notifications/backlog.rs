//! How a queued notification is held while the canister upgrades.
//!
//! These rows exist only between `pre_upgrade` and `post_upgrade`. Nothing scans
//! them, so the key orders by sender first purely to group a sender's work together
//! as it is read back.

use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::timestamp::StorableTimestamp;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::AnchorNumber;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Origin hash, deadline, recipient, notification id.
const BACKLOG_KEY_SIZE: usize = 32 + 8 + 8 + 8;

/// Room for the entry's three fields plus CBOR framing.
const BACKLOG_ENTRY_SIZE: u32 = 48;

/// Identifies one queued notification. Big-endian throughout, so a sender's rows are
/// contiguous and come back in deadline order within it.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableBacklogKey {
    pub origin: StorableOriginSha256,
    pub expires_at_ns: StorableTimestamp,
    pub recipient: AnchorNumber,
    pub notification_id: u64,
}

impl Storable for StorableBacklogKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::with_capacity(BACKLOG_KEY_SIZE);
        buffer.extend(self.origin.to_bytes().as_ref());
        buffer.extend(self.expires_at_ns.to_be_bytes());
        buffer.extend(self.recipient.to_be_bytes());
        buffer.extend(self.notification_id.to_be_bytes());
        Cow::Owned(buffer)
    }

    /// Total, so a row of the wrong length costs the notification it holds rather
    /// than the upgrade reading it. The key is fixed-size and has no shape to
    /// change, so a short one means the row is corrupt either way.
    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        let field = |from: usize| {
            bytes
                .get(from..from + 8)
                .and_then(|slice| <[u8; 8]>::try_from(slice).ok())
                .map_or(0, u64::from_be_bytes)
        };
        Self {
            origin: StorableOriginSha256::from_bytes(Cow::Borrowed(
                bytes.get(0..32).unwrap_or(&[0u8; 32]),
            )),
            expires_at_ns: field(32),
            recipient: field(40),
            notification_id: field(48),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: BACKLOG_KEY_SIZE as u32,
        is_fixed_size: true,
    };
}

/// What the key does not already carry. The app's own expiry rides along even though
/// the deadline in the key already accounts for it, so an entry comes back as it was
/// rather than as something that merely behaves the same.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableBacklogEntry {
    #[n(0)]
    pub received_at_ns: StorableTimestamp,
    /// Urgency level, least urgent first. An unknown one reads as the least urgent.
    #[n(1)]
    pub urgency: u8,
    #[n(2)]
    pub expires_at_ns: Option<StorableTimestamp>,
}

/// A stored row. Holds `None` when the bytes do not decode, which is how a row an
/// older build wrote in a shape this one no longer understands costs the one
/// notification rather than the whole upgrade: a trap here would fail the upgrade,
/// and nothing that is only a wake-up is worth that.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BacklogRow(pub Option<StorableBacklogEntry>);

impl Storable for BacklogRow {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let Some(entry) = &self.0 else {
            return Cow::Owned(Vec::new());
        };
        let mut buffer = Vec::new();
        minicbor::encode(entry, &mut buffer).expect("failed to encode StorableBacklogEntry");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        Self(minicbor::decode(&bytes).ok())
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: BACKLOG_ENTRY_SIZE,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn key(origin: &str, expires_at_ns: u64) -> StorableBacklogKey {
        StorableBacklogKey {
            origin: StorableOriginSha256::from_origin(&origin.to_string()),
            expires_at_ns,
            recipient: 3,
            notification_id: 9,
        }
    }

    #[test]
    fn a_key_round_trips_through_its_bytes() {
        let original = StorableBacklogKey {
            recipient: u64::MAX,
            notification_id: u64::MAX,
            ..key("https://app.example", u64::MAX)
        };

        assert_eq!(
            StorableBacklogKey::from_bytes(original.to_bytes()),
            original
        );
    }

    /// A sender's rows have to be contiguous, or reading them back groups one
    /// sender's work under another.
    #[test]
    fn the_encoding_groups_by_sender_before_anything_else() {
        let origins = [
            "https://a.example",
            "https://b.example",
            "https://c.example",
        ];
        let mut rows: Vec<Vec<u8>> = origins
            .iter()
            .flat_map(|origin| [key(origin, 1), key(origin, 2)])
            .map(|key| key.to_bytes().into_owned())
            .collect();
        rows.sort();

        let senders: Vec<Vec<u8>> = rows.iter().map(|row| row[0..32].to_vec()).collect();
        let mut grouped = senders.clone();
        grouped.dedup();
        assert_eq!(grouped.len(), origins.len(), "a sender's rows are split up");
    }

    /// A trap in `pre_upgrade` fails the whole upgrade, so the largest entry the
    /// queue can hold has to encode inside the bound.
    #[test]
    fn a_largest_case_entry_round_trips_within_its_bound() {
        let original = BacklogRow(Some(StorableBacklogEntry {
            received_at_ns: u64::MAX,
            urgency: u8::MAX,
            expires_at_ns: Some(u64::MAX),
        }));
        let bytes = original.to_bytes();

        assert!(
            bytes.len() as u32 <= BACKLOG_ENTRY_SIZE,
            "a largest-case entry encodes to {} bytes, over its bound",
            bytes.len()
        );
        assert_eq!(BacklogRow::from_bytes(bytes), original);
    }

    #[test]
    fn an_entry_without_an_app_expiry_round_trips() {
        let original = BacklogRow(Some(StorableBacklogEntry {
            received_at_ns: 5,
            urgency: 0,
            expires_at_ns: None,
        }));

        assert_eq!(BacklogRow::from_bytes(original.to_bytes()), original);
    }

    /// Bytes this build cannot read must not trap: a trap here fails the upgrade,
    /// and the shape most likely to stop decoding is one an older build wrote.
    #[test]
    fn bytes_that_do_not_decode_read_as_no_entry() {
        for bytes in [vec![], vec![0xffu8; 8], b"not cbor at all".to_vec()] {
            assert_eq!(BacklogRow::from_bytes(Cow::Owned(bytes)), BacklogRow(None));
        }
    }

    /// Same for a key of the wrong length, which the fixed-size map should never
    /// hand back but which must not take the upgrade down if it does.
    #[test]
    fn a_key_of_the_wrong_length_reads_without_trapping() {
        let short = StorableBacklogKey::from_bytes(Cow::Owned(vec![1u8; 10]));

        assert_eq!(short.expires_at_ns, 0);
        assert_eq!(short.recipient, 0);
        assert_eq!(short.notification_id, 0);
    }
}
