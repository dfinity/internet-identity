//! The key for the entry map.
//!
//! A map is ordered by the bytes its key encodes to, so fields go big-endian
//! in the order they should sort by, and the leading byte separates the kinds
//! of row: what is waiting to be sent, what is waiting for a second attempt,
//! what Internet Identity accepted, and one row per notification saying which
//! of those it is in.

use ic_stable_structures::storable::{Bound, Storable};
use std::borrow::Cow;

pub const WAITING: u8 = 0;
pub const PARKED: u8 = 1;
pub const ACCEPTED: u8 = 2;
const SLOT: u8 = 3;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EntryKey {
    kind: u8,
    first: u64,
    second: u64,
    id: u64,
}

impl EntryKey {
    /// Waiting to be sent: ordered by lane, then soonest expiry.
    pub fn waiting(lane: u8, expires_at: u64, id: u64) -> Self {
        Self {
            kind: WAITING,
            first: u64::from(lane),
            second: expires_at,
            id,
        }
    }

    /// Waiting for a second attempt: ordered by when that attempt is due.
    pub fn parked(due_at: u64, id: u64) -> Self {
        Self {
            kind: PARKED,
            first: due_at,
            second: 0,
            id,
        }
    }

    /// Sent and accepted: ordered by expiry, which is when its content goes.
    pub fn accepted(expires_at: u64, id: u64) -> Self {
        Self {
            kind: ACCEPTED,
            first: expires_at,
            second: 0,
            id,
        }
    }

    /// Where this notification is, so a repeat of a key finds it in one look
    /// instead of a scan.
    pub fn slot(id: u64) -> Self {
        Self {
            kind: SLOT,
            first: 0,
            second: 0,
            id,
        }
    }

    pub fn lane_start(lane: u8) -> Self {
        Self::waiting(lane, 0, 0)
    }

    pub fn lane_end(lane: u8) -> Self {
        Self::waiting(lane, u64::MAX, u64::MAX)
    }

    pub fn parked_start() -> Self {
        Self::parked(0, 0)
    }

    pub fn parked_due_by(now: u64) -> Self {
        Self::parked(now, u64::MAX)
    }

    pub fn accepted_start() -> Self {
        Self::accepted(0, 0)
    }

    pub fn accepted_expired_by(now: u64) -> Self {
        Self::accepted(now, u64::MAX)
    }

    pub fn id(&self) -> u64 {
        self.id
    }
}

impl Storable for EntryKey {
    const BOUND: Bound = Bound::Bounded {
        max_size: 25,
        is_fixed_size: true,
    };

    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut bytes = Vec::with_capacity(25);
        bytes.push(self.kind);
        bytes.extend_from_slice(&self.first.to_be_bytes());
        bytes.extend_from_slice(&self.second.to_be_bytes());
        bytes.extend_from_slice(&self.id.to_be_bytes());
        Cow::Owned(bytes)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.to_bytes().into_owned()
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let word =
            |from: usize| u64::from_be_bytes(bytes[from..from + 8].try_into().expect("25 bytes"));
        Self {
            kind: bytes[0],
            first: word(1),
            second: word(9),
            id: word(17),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn encoded(key: EntryKey) -> Vec<u8> {
        key.to_bytes().into_owned()
    }

    #[test]
    fn waiting_sorts_by_lane_then_expiry_then_id() {
        let mut keys = vec![
            EntryKey::waiting(1, 5, 9),
            EntryKey::waiting(0, 9, 1),
            EntryKey::waiting(0, 5, 2),
            EntryKey::waiting(0, 5, 1),
        ];
        keys.sort_by_key(|key| encoded(*key));

        assert_eq!(
            keys,
            vec![
                EntryKey::waiting(0, 5, 1),
                EntryKey::waiting(0, 5, 2),
                EntryKey::waiting(0, 9, 1),
                EntryKey::waiting(1, 5, 9),
            ]
        );
    }

    #[test]
    fn the_kinds_sort_into_separate_ranges() {
        let waiting = encoded(EntryKey::waiting(3, u64::MAX, u64::MAX));
        let parked = encoded(EntryKey::parked(u64::MAX, u64::MAX));
        let accepted = encoded(EntryKey::accepted(u64::MAX, u64::MAX));

        assert!(waiting < encoded(EntryKey::parked(0, 0)));
        assert!(parked < encoded(EntryKey::accepted(0, 0)));
        assert!(accepted < encoded(EntryKey::slot(0)));
    }

    #[test]
    fn parked_sorts_by_when_it_is_due_and_accepted_by_expiry() {
        assert!(encoded(EntryKey::parked(5, 99)) < encoded(EntryKey::parked(6, 0)));
        assert!(encoded(EntryKey::accepted(5, 99)) < encoded(EntryKey::accepted(6, 0)));
    }

    #[test]
    fn byte_order_matches_numeric_order_across_a_carry() {
        assert!(encoded(EntryKey::parked(255, 0)) < encoded(EntryKey::parked(256, 0)));
    }

    #[test]
    fn keys_round_trip() {
        for key in [
            EntryKey::waiting(3, u64::MAX, 42),
            EntryKey::parked(7, u64::MAX),
            EntryKey::accepted(9, 42),
            EntryKey::slot(42),
        ] {
            assert_eq!(EntryKey::from_bytes(key.to_bytes()), key);
        }
    }
}
