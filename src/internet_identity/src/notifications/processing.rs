//! Stable queue for notifications taken from the backlog and awaiting delivery.
//! Entries are ordered by deadline for bounded expiry sweeps and dispatch batches.
//! A full queue stops backlog draining, causing senders to receive longer retry delays.
//! Resends with a different deadline may produce another wake-up.
// Used by the ticker and dispatcher in follow-up PRs.
#![allow(dead_code)]

use super::admission_queue::Taken;
use crate::notifications::backlog::PendingNotification;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::processing::{
    StorableProcessingEntry, StorableProcessingKey,
};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, NotificationId, Timestamp, Urgency,
};

pub(crate) const MAX_ENTRIES: u64 = 2_000;

/// Bound the work performed by a single scan.
pub(crate) const MAX_PER_MESSAGE: usize = 200;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Due {
    pub(crate) recipient: Principal,
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) origin: StorableOriginSha256,
    pub(crate) notification_id: NotificationId,
    pub(crate) urgency: Urgency,
    pub(crate) received_at_ns: Timestamp,
    pub(crate) expires_at_ns: Timestamp,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct DoesNotFit {
    pub(crate) free: usize,
    pub(crate) batch: usize,
}

/// Expired entries count toward capacity; sweep before checking available room.
pub(crate) fn free_space() -> usize {
    let stored = storage_borrow(|storage| storage.processing_notifications_len());
    MAX_ENTRIES.saturating_sub(stored) as usize
}

/// Store the entire batch or reject it without writes.
/// Runs synchronously so the backlog can restore a rejected batch.
pub(crate) fn store_batch(
    batch: &[Taken<StorableOriginSha256, PendingNotification>],
) -> Result<(), DoesNotFit> {
    let free = free_space();
    if batch.len() > free {
        return Err(DoesNotFit {
            free,
            batch: batch.len(),
        });
    }

    storage_borrow_mut(|storage| {
        for taken in batch {
            storage.add_processing_notification(
                storage_key_of(taken),
                StorableProcessingEntry {
                    received_at_ns: taken.entry.received_at_ns,
                    urgency: urgency_level(&taken.entry.item.urgency),
                    anchor_number: taken.entry.item.anchor_number,
                },
            );
        }
    });
    Ok(())
}

pub(crate) fn discard_expired(now_ns: Timestamp) -> usize {
    let expired: Vec<StorableProcessingKey> = storage_borrow(|storage| {
        storage
            .processing_notifications_by_deadline(MAX_PER_MESSAGE)
            .into_iter()
            .map(|(key, _)| key)
            .take_while(|key| key.expires_at_ns <= now_ns)
            .collect()
    });

    storage_borrow_mut(|storage| {
        for key in &expired {
            storage.remove_processing_notification(key);
        }
    });
    expired.len()
}

/// Read at most `limit` rows in deadline order, excluding expired entries.
/// May return fewer entries until the expiry sweep runs.
pub(crate) fn next_due(limit: usize, now_ns: Timestamp) -> Vec<Due> {
    storage_borrow(|storage| {
        storage
            .processing_notifications_by_deadline(limit.min(MAX_PER_MESSAGE))
            .into_iter()
            .filter(|(key, _)| key.expires_at_ns > now_ns)
            .map(|(key, entry)| Due {
                recipient: key.recipient,
                anchor_number: entry.anchor_number,
                origin: key.origin,
                notification_id: key.notification_id,
                urgency: urgency_from_level(entry.urgency),
                received_at_ns: entry.received_at_ns,
                expires_at_ns: key.expires_at_ns,
            })
            .collect()
    })
}

pub(crate) fn remove_due(due: &Due) -> bool {
    let key = StorableProcessingKey {
        expires_at_ns: due.expires_at_ns,
        origin: due.origin.clone(),
        notification_id: due.notification_id,
        recipient: due.recipient,
    };
    storage_borrow_mut(|storage| storage.remove_processing_notification(&key))
}

fn urgency_level(urgency: &Urgency) -> u8 {
    match urgency {
        Urgency::VeryLow => 0,
        Urgency::Low => 1,
        Urgency::Normal => 2,
        Urgency::High => 3,
    }
}

/// An unknown level reads as the least urgent, costing a row its urgency not the read.
fn urgency_from_level(level: u8) -> Urgency {
    match level {
        1 => Urgency::Low,
        2 => Urgency::Normal,
        3 => Urgency::High,
        _ => Urgency::VeryLow,
    }
}

fn storage_key_of(
    taken: &Taken<StorableOriginSha256, PendingNotification>,
) -> StorableProcessingKey {
    StorableProcessingKey {
        expires_at_ns: taken.entry.expires_at_ns,
        origin: taken.sender.clone(),
        notification_id: taken.entry.item.notification_id,
        recipient: taken.entry.item.recipient,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::admission_queue::Entry;
    use crate::notifications::test_setup as setup;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const OTHER: &str = "https://other.example";

    fn origin(host: &str) -> StorableOriginSha256 {
        StorableOriginSha256::from_origin(&host.to_string())
    }

    fn principal(seed: u64) -> Principal {
        Principal::from_slice(&seed.to_be_bytes())
    }

    fn taken(
        host: &str,
        anchor_number: AnchorNumber,
        notification_id: u64,
        expires_at_ns: Timestamp,
    ) -> Taken<StorableOriginSha256, PendingNotification> {
        Taken {
            sender: origin(host),
            entry: Entry {
                received_at_ns: 1,
                expires_at_ns,
                item: PendingNotification {
                    recipient: principal(anchor_number),
                    anchor_number,
                    notification_id,
                    urgency: Urgency::Normal,
                    expires_at_ns: Some(expires_at_ns),
                },
            },
        }
    }

    fn ids_due(limit: usize, now_ns: Timestamp) -> Vec<u64> {
        next_due(limit, now_ns)
            .iter()
            .map(|due| due.notification_id)
            .collect()
    }

    #[test]
    fn a_stored_notification_comes_back_with_everything_needed_to_send_it() {
        setup();
        let mut batch = taken(APP, 42, 7, 500);
        batch.entry.received_at_ns = 100;
        batch.entry.item.urgency = Urgency::VeryLow;

        store_batch(&[batch]).unwrap();

        assert_eq!(
            next_due(10, 200),
            vec![Due {
                recipient: principal(42),
                anchor_number: 42,
                origin: origin(APP),
                notification_id: 7,
                urgency: Urgency::VeryLow,
                received_at_ns: 100,
                expires_at_ns: 500,
            }]
        );
    }

    #[test]
    fn every_urgency_reads_back_as_the_level_it_was_stored_at() {
        for urgency in [
            Urgency::VeryLow,
            Urgency::Low,
            Urgency::Normal,
            Urgency::High,
        ] {
            assert_eq!(urgency_from_level(urgency_level(&urgency)), urgency);
        }
    }

    #[test]
    fn the_nearest_deadline_goes_out_first() {
        setup();
        store_batch(&[
            taken(APP, 1, 3, 300),
            taken(APP, 1, 1, 100),
            taken(APP, 1, 2, 200),
        ])
        .unwrap();

        assert_eq!(ids_due(10, 50), vec![1, 2, 3]);
    }

    // Partial storage would duplicate entries when the backlog retries the batch.
    #[test]
    fn a_batch_larger_than_the_room_left_stores_nothing() {
        setup();
        let full: Vec<_> = (0..MAX_ENTRIES)
            .map(|id| taken(APP, 1, id, 1_000))
            .collect();
        store_batch(&full).unwrap();

        assert_eq!(free_space(), 0);
        assert_eq!(
            store_batch(&[taken(OTHER, 2, 1, 1_000)]),
            Err(DoesNotFit { free: 0, batch: 1 })
        );
        assert_eq!(
            storage_borrow(|storage| storage.processing_notifications_len()),
            MAX_ENTRIES
        );
    }

    #[test]
    fn free_space_falls_as_the_queue_fills_and_returns_as_it_drains() {
        setup();
        assert_eq!(free_space(), MAX_ENTRIES as usize);

        store_batch(&[taken(APP, 1, 1, 100), taken(APP, 1, 2, 200)]).unwrap();
        assert_eq!(free_space(), MAX_ENTRIES as usize - 2);

        let due = next_due(1, 50).remove(0);
        assert!(remove_due(&due));
        assert_eq!(free_space(), MAX_ENTRIES as usize - 1);
    }

    #[test]
    fn a_notification_that_expired_while_it_waited_is_not_due() {
        setup();
        store_batch(&[taken(APP, 1, 1, 100), taken(APP, 1, 2, 300)]).unwrap();

        assert_eq!(ids_due(10, 200), vec![2]);
    }

    #[test]
    fn the_sweep_drops_what_has_passed_and_leaves_the_rest() {
        setup();
        store_batch(&[
            taken(APP, 1, 1, 100),
            taken(APP, 1, 2, 200),
            taken(APP, 1, 3, 300),
        ])
        .unwrap();

        assert_eq!(discard_expired(200), 2);
        assert_eq!(free_space(), MAX_ENTRIES as usize - 1);
        assert_eq!(ids_due(10, 250), vec![3]);
    }

    #[test]
    fn the_sweep_leaves_a_queue_with_nothing_expired_alone() {
        setup();
        store_batch(&[taken(APP, 1, 1, 100)]).unwrap();

        assert_eq!(discard_expired(50), 0);
        assert_eq!(ids_due(10, 50), vec![1]);
    }

    #[test]
    fn removing_a_delivered_notification_is_idempotent() {
        setup();
        store_batch(&[taken(APP, 1, 1, 100)]).unwrap();
        let due = next_due(1, 50).remove(0);

        assert!(remove_due(&due));
        assert!(!remove_due(&due));
        assert!(ids_due(10, 50).is_empty());
    }

    #[test]
    fn only_an_identical_notification_overwrites_one_already_here() {
        setup();
        store_batch(&[
            taken(APP, 1, 1, 100),
            taken(APP, 2, 1, 100),
            taken(OTHER, 1, 1, 100),
            taken(APP, 1, 2, 100),
        ])
        .unwrap();
        assert_eq!(free_space(), MAX_ENTRIES as usize - 4);

        store_batch(&[taken(APP, 1, 1, 100)]).unwrap();

        assert_eq!(free_space(), MAX_ENTRIES as usize - 4);
    }

    // The backlog no longer deduplicates a key after handing it over.
    #[test]
    fn a_resend_with_a_new_deadline_arrives_as_a_second_entry() {
        setup();
        store_batch(&[taken(APP, 1, 1, 100)]).unwrap();

        store_batch(&[taken(APP, 1, 1, 300)]).unwrap();

        assert_eq!(ids_due(10, 50), vec![1, 1]);
    }

    #[test]
    fn neither_the_sweep_nor_a_read_walks_more_than_one_message_worth() {
        setup();
        let expired: Vec<_> = (0..MAX_PER_MESSAGE as u64 + 10)
            .map(|id| taken(APP, 1, id, 100))
            .collect();
        store_batch(&expired).unwrap();

        assert_eq!(discard_expired(200), MAX_PER_MESSAGE);
        assert_eq!(discard_expired(200), 10);

        store_batch(&expired).unwrap();
        assert_eq!(next_due(usize::MAX, 50).len(), MAX_PER_MESSAGE);
    }
}
