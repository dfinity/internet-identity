//! Notifications the canister has taken on and not yet delivered.
//!
//! An entry arrives here by leaving the backlog, which is the last point a sender
//! hears anything. Nothing about this queue reaches an app directly. It reaches one
//! only as the backlog's stall backoff, once a full processing queue stops the ticker
//! taking. Entries live in stable memory rather than the heap because how long one
//! waits here depends on push relays II does not control, so an upgrade can land on a
//! queue holding far more than the backlog's tick-sized turnover.
//!
//! Keyed by deadline, so the entries closest to expiring are the first a scan reaches
//! and both the sweep and the next batch to send are prefixes of one walk. The backlog
//! never hands over anything already expired, but an entry can go stale waiting here,
//! so the deadline is checked again on the way out.
//!
//! A resend that reaches the backlog after its first copy was taken arrives here as a
//! second entry, since the backlog forgets a key once it hands it over. That costs one
//! extra wake-up, which carries no content, so it is left to whatever tracks delivery.
// The ticker and the dispatcher will use this in later PRs.
#![allow(dead_code)]

use crate::admission_queue::Taken;
use crate::notifications::backlog::{PendingNotification, Urgency};
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::processing::{
    StorableProcessingEntry, StorableProcessingKey,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

/// Sized against the backlog's 10,000. Much smaller and the backlog stalls whenever
/// delivery pauses; much larger and the backlog stops being the binding constraint.
pub(crate) const MAX_ENTRIES: u64 = 2_000;

/// Entries one message may walk. Keeps a sweep or a read bounded however long the
/// queue has grown.
pub(crate) const MAX_PER_MESSAGE: usize = 200;

/// A notification ready to send, holding everything needed to address it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Due {
    pub(crate) recipient: AnchorNumber,
    pub(crate) origin: StorableOriginSha256,
    pub(crate) notification_id: u64,
    pub(crate) urgency: Urgency,
    pub(crate) received_at_ns: Timestamp,
    pub(crate) expires_at_ns: Timestamp,
}

/// A batch larger than the room left. The sender never sees this: the backlog puts
/// the batch back and the next tick offers it again.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct DoesNotFit {
    pub(crate) free: usize,
    pub(crate) batch: usize,
}

/// How many more notifications the queue can take. The ticker reads this and passes
/// it as its take limit in the same message, so nothing can change it in between.
/// Expired entries still count, so sweep before reading it.
pub(crate) fn free_space() -> usize {
    let stored = storage_borrow(|storage| storage.processing_notifications_len());
    MAX_ENTRIES.saturating_sub(stored) as usize
}

/// Stores a batch the ticker took from the backlog. All of it or none of it, so a
/// batch that does not fit goes back to the backlog whole rather than in part.
///
/// Runs to completion without awaiting, which is what lets the backlog hand the batch
/// over and keep the removal only once this returns.
pub(crate) fn store(
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
                key_of(taken),
                StorableProcessingEntry {
                    received_at_ns: taken.entry.received_at_ns,
                    urgency: taken.entry.item.urgency as u8,
                },
            );
        }
    });
    Ok(())
}

/// Drops the notifications whose deadline passed while they waited, and returns how
/// many. Walks only the front of the queue, which is where they all are.
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

/// The notifications to send next, nearest deadline first. Reads `limit` entries and
/// leaves out any that expired while waiting, so it can return fewer than asked for
/// until [`discard_expired`] has run.
pub(crate) fn next_due(limit: usize, now_ns: Timestamp) -> Vec<Due> {
    storage_borrow(|storage| {
        storage
            .processing_notifications_by_deadline(limit.min(MAX_PER_MESSAGE))
            .into_iter()
            .filter(|(key, _)| key.expires_at_ns > now_ns)
            .map(|(key, entry)| Due {
                recipient: key.recipient,
                origin: key.origin,
                notification_id: key.notification_id,
                urgency: Urgency::at_priority(entry.urgency as usize),
                received_at_ns: entry.received_at_ns,
                expires_at_ns: key.expires_at_ns,
            })
            .collect()
    })
}

/// Forgets a notification, which is what delivering it does. Idempotent.
pub(crate) fn remove(due: &Due) -> bool {
    let key = StorableProcessingKey {
        expires_at_ns: due.expires_at_ns,
        recipient: due.recipient,
        origin: due.origin.clone(),
        notification_id: due.notification_id,
    };
    storage_borrow_mut(|storage| storage.remove_processing_notification(&key))
}

fn key_of(taken: &Taken<StorableOriginSha256, PendingNotification>) -> StorableProcessingKey {
    StorableProcessingKey {
        expires_at_ns: taken.entry.expires_at_ns,
        recipient: taken.entry.item.recipient,
        origin: taken.sender.clone(),
        notification_id: taken.entry.item.notification_id,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admission_queue::Entry;
    use crate::notifications::test_setup as setup;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const OTHER: &str = "https://other.example";

    fn origin(host: &str) -> StorableOriginSha256 {
        StorableOriginSha256::from_origin(&host.to_string())
    }

    fn taken(
        host: &str,
        recipient: AnchorNumber,
        notification_id: u64,
        expires_at_ns: Timestamp,
    ) -> Taken<StorableOriginSha256, PendingNotification> {
        Taken {
            sender: origin(host),
            entry: Entry {
                received_at_ns: 1,
                expires_at_ns,
                item: PendingNotification {
                    recipient,
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

        store(&[batch]).unwrap();

        assert_eq!(
            next_due(10, 200),
            vec![Due {
                recipient: 42,
                origin: origin(APP),
                notification_id: 7,
                urgency: Urgency::VeryLow,
                received_at_ns: 100,
                expires_at_ns: 500,
            }]
        );
    }

    #[test]
    fn the_nearest_deadline_goes_out_first() {
        setup();
        store(&[
            taken(APP, 1, 3, 300),
            taken(APP, 1, 1, 100),
            taken(APP, 1, 2, 200),
        ])
        .unwrap();

        assert_eq!(ids_due(10, 50), vec![1, 2, 3]);
    }

    /// The backlog puts a batch back whole, so a partial store would double-send the
    /// part that did fit.
    #[test]
    fn a_batch_larger_than_the_room_left_stores_nothing() {
        setup();
        let full: Vec<_> = (0..MAX_ENTRIES)
            .map(|id| taken(APP, 1, id, 1_000))
            .collect();
        store(&full).unwrap();

        assert_eq!(free_space(), 0);
        assert_eq!(
            store(&[taken(OTHER, 2, 1, 1_000)]),
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

        store(&[taken(APP, 1, 1, 100), taken(APP, 1, 2, 200)]).unwrap();
        assert_eq!(free_space(), MAX_ENTRIES as usize - 2);

        let due = next_due(1, 50).remove(0);
        assert!(remove(&due));
        assert_eq!(free_space(), MAX_ENTRIES as usize - 1);
    }

    /// The backlog hands over nothing expired, but delivery can fall behind while an
    /// entry sits here.
    #[test]
    fn a_notification_that_expired_while_it_waited_is_not_due() {
        setup();
        store(&[taken(APP, 1, 1, 100), taken(APP, 1, 2, 300)]).unwrap();

        assert_eq!(ids_due(10, 200), vec![2]);
    }

    #[test]
    fn the_sweep_drops_what_has_passed_and_leaves_the_rest() {
        setup();
        store(&[
            taken(APP, 1, 1, 100),
            taken(APP, 1, 2, 200),
            taken(APP, 1, 3, 300),
        ])
        .unwrap();

        assert_eq!(discard_expired(200), 2);
        assert_eq!(free_space(), MAX_ENTRIES as usize - 1);
        assert_eq!(ids_due(10, 250), vec![3]);
    }

    /// A deadline in the future is the whole queue's, so nothing is walked past it.
    #[test]
    fn the_sweep_leaves_a_queue_with_nothing_expired_alone() {
        setup();
        store(&[taken(APP, 1, 1, 100)]).unwrap();

        assert_eq!(discard_expired(50), 0);
        assert_eq!(ids_due(10, 50), vec![1]);
    }

    #[test]
    fn removing_a_delivered_notification_is_idempotent() {
        setup();
        store(&[taken(APP, 1, 1, 100)]).unwrap();
        let due = next_due(1, 50).remove(0);

        assert!(remove(&due));
        assert!(!remove(&due));
        assert!(ids_due(10, 50).is_empty());
    }

    /// Recipient, app and notification id each separate two entries, so nothing
    /// sharing a deadline overwrites anything else.
    #[test]
    fn only_an_identical_notification_overwrites_one_already_here() {
        setup();
        store(&[
            taken(APP, 1, 1, 100),
            taken(APP, 2, 1, 100),
            taken(OTHER, 1, 1, 100),
            taken(APP, 1, 2, 100),
        ])
        .unwrap();
        assert_eq!(free_space(), MAX_ENTRIES as usize - 4);

        store(&[taken(APP, 1, 1, 100)]).unwrap();

        assert_eq!(free_space(), MAX_ENTRIES as usize - 4);
    }

    /// The backlog folds a resend only while it still holds the key. One that arrives
    /// after the take is a second wake-up, which is accepted rather than deduped here.
    #[test]
    fn a_resend_with_a_new_deadline_arrives_as_a_second_entry() {
        setup();
        store(&[taken(APP, 1, 1, 100)]).unwrap();

        store(&[taken(APP, 1, 1, 300)]).unwrap();

        assert_eq!(ids_due(10, 50), vec![1, 1]);
    }

    /// However far the queue has fallen behind, one message walks a bounded prefix.
    #[test]
    fn neither_the_sweep_nor_a_read_walks_more_than_one_message_worth() {
        setup();
        let expired: Vec<_> = (0..MAX_PER_MESSAGE as u64 + 10)
            .map(|id| taken(APP, 1, id, 100))
            .collect();
        store(&expired).unwrap();

        assert_eq!(discard_expired(200), MAX_PER_MESSAGE);
        assert_eq!(discard_expired(200), 10);

        store(&expired).unwrap();
        assert_eq!(next_due(usize::MAX, 50).len(), MAX_PER_MESSAGE);
    }
}
