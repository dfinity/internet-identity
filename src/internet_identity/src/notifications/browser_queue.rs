//! What each browser's service worker has yet to show, one entry per wake-up sent.
//! An entry is added before its wake-up is posted and the service worker takes the
//! oldest on every wake-up, so wake-ups and shown notifications stay one to one.
// Filled by the dispatcher, which the submission endpoint kicks in a follow-up PR.
#![allow(dead_code)]

use crate::state::storage_borrow_mut;
use crate::storage::storable::notifications::browser_queue::{
    StorableBrowserNotification, StorableBrowserNotificationKey,
};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, NotificationId, Timestamp,
};
use std::cell::Cell;

/// Most entries one browser holds. A new one past it replaces the oldest.
pub(crate) const MAX_PER_BROWSER: usize = 100;

/// Most expired entries one sweep removes.
pub(crate) const MAX_PER_SWEEP: usize = 200;

thread_local! {
    static LAST_SEQUENCE: Cell<u64> = const { Cell::new(0) };
}

/// Where an added entry went.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Added {
    /// Its wake-up is still to be posted.
    Queued(StorableBrowserNotificationKey),
    /// The queue was full, so the oldest entry made way. The wake-up already on its way
    /// for that one covers this one.
    ReplacedOldest(StorableBrowserNotificationKey),
}

/// Queue a notification for one browser.
pub(crate) fn add(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    recipient: Principal,
    notification_id: NotificationId,
    expires_at_ns: Timestamp,
    now_ns: Timestamp,
) -> Added {
    let key = StorableBrowserNotificationKey {
        anchor_number,
        browser_id,
        sequence: next_sequence(now_ns),
    };
    storage_borrow_mut(|storage| {
        let queued = storage.browser_notifications(anchor_number, browser_id, MAX_PER_BROWSER);
        let full = queued.len() >= MAX_PER_BROWSER;
        if let (true, Some((oldest, _))) = (full, queued.first()) {
            storage.remove_browser_notification(oldest);
        }
        storage.add_browser_notification(
            key,
            StorableBrowserNotification {
                recipient: recipient.as_slice().to_vec(),
                notification_id,
                expires_at_ns,
            },
        );
        if full {
            Added::ReplacedOldest(key)
        } else {
            Added::Queued(key)
        }
    })
}

/// A failed post means no wake-up is coming, so one entry goes with it: its own, or the
/// oldest if a wake-up already took it. Nothing goes once it expired or the browser
/// registered elsewhere, since the sweep or the new registration may have dropped it.
pub(crate) fn remove_after_failed_wake_up(
    key: StorableBrowserNotificationKey,
    expires_at_ns: Timestamp,
    endpoint: &str,
    now_ns: Timestamp,
) {
    storage_borrow_mut(|storage| {
        if storage.remove_browser_notification(&key).is_some() || expires_at_ns <= now_ns {
            return;
        }
        let still_registered = storage.read(key.anchor_number).is_ok_and(|anchor| {
            anchor
                .webpush_subscription(key.browser_id)
                .is_some_and(|registered| registered.endpoint == endpoint)
        });
        if !still_registered {
            return;
        }
        if let Some((oldest, _)) = storage
            .browser_notifications(key.anchor_number, key.browser_id, 1)
            .pop()
        {
            storage.remove_browser_notification(&oldest);
        }
    });
}

/// Drop a browser's whole queue, for a registration that changed or went.
pub(crate) fn clear(anchor_number: AnchorNumber, browser_id: BrowserId) {
    storage_borrow_mut(|storage| {
        for (key, _) in storage.browser_notifications(anchor_number, browser_id, MAX_PER_BROWSER) {
            storage.remove_browser_notification(&key);
        }
    });
}

/// Remove up to [`MAX_PER_SWEEP`] expired entries across every browser.
pub(crate) fn discard_expired(now_ns: Timestamp) -> usize {
    storage_borrow_mut(|storage| {
        let expired = storage.expired_browser_notifications(now_ns, MAX_PER_SWEEP);
        for key in &expired {
            storage.remove_browser_notification(key);
        }
        expired.len()
    })
}

/// Canister time, bumped past the last one handed out, so sequences stay unique and
/// ordered within a message and across upgrades.
fn next_sequence(now_ns: Timestamp) -> u64 {
    LAST_SEQUENCE.with(|last| {
        let next = now_ns.max(last.get().saturating_add(1));
        last.set(next);
        next
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::storage_borrow;
    use pretty_assertions::assert_eq;

    const SECOND_NS: u64 = 1_000_000_000;
    const ANCHOR: AnchorNumber = 10_000;
    const BROWSER: BrowserId = 3;

    fn recipient() -> Principal {
        Principal::from_slice(&[7; 29])
    }

    fn queued(browser_id: BrowserId) -> Vec<NotificationId> {
        storage_borrow(|storage| {
            storage
                .browser_notifications(ANCHOR, browser_id, usize::MAX)
                .into_iter()
                .map(|(_, entry)| entry.notification_id)
                .collect()
        })
    }

    fn add_all(browser_id: BrowserId, ids: impl IntoIterator<Item = NotificationId>) -> Vec<Added> {
        ids.into_iter()
            .map(|id| {
                add(
                    ANCHOR,
                    browser_id,
                    recipient(),
                    id,
                    60 * SECOND_NS,
                    SECOND_NS,
                )
            })
            .collect()
    }

    fn key_of(added: Added) -> StorableBrowserNotificationKey {
        match added {
            Added::Queued(key) | Added::ReplacedOldest(key) => key,
        }
    }

    #[test]
    fn entries_added_in_one_message_keep_their_order() {
        crate::notifications::test_setup();
        let sequences: Vec<u64> = add_all(BROWSER, 1..=3)
            .into_iter()
            .map(|added| key_of(added).sequence)
            .collect();

        assert!(sequences.windows(2).all(|pair| pair[0] < pair[1]));
        assert_eq!(queued(BROWSER), vec![1, 2, 3]);
    }

    #[test]
    fn a_full_queue_replaces_its_oldest_and_needs_no_new_wake_up() {
        crate::notifications::test_setup();
        let added = add_all(BROWSER, 0..MAX_PER_BROWSER as u64 + 1);

        assert!(added[..MAX_PER_BROWSER]
            .iter()
            .all(|one| matches!(one, Added::Queued(_))));
        assert!(matches!(added[MAX_PER_BROWSER], Added::ReplacedOldest(_)));
        let left = queued(BROWSER);
        assert_eq!(left.len(), MAX_PER_BROWSER);
        assert_eq!(left.first(), Some(&1));
        assert_eq!(left.last(), Some(&(MAX_PER_BROWSER as u64)));
    }

    #[test]
    fn a_failed_wake_up_removes_its_own_entry() {
        crate::notifications::test_setup();
        let added = add_all(BROWSER, [1, 2]);

        remove_after_failed_wake_up(
            key_of(added[1]),
            60 * SECOND_NS,
            "https://relay.example/a",
            SECOND_NS,
        );

        assert_eq!(queued(BROWSER), vec![1]);
    }

    #[test]
    fn clearing_a_browser_leaves_the_others() {
        crate::notifications::test_setup();
        add_all(BROWSER, [1, 2]);
        add_all(BROWSER + 1, [3]);

        clear(ANCHOR, BROWSER);

        assert!(queued(BROWSER).is_empty());
        assert_eq!(queued(BROWSER + 1), vec![3]);
    }

    #[test]
    fn the_sweep_removes_expired_entries_from_every_browser() {
        crate::notifications::test_setup();
        add(ANCHOR, BROWSER, recipient(), 1, 2 * SECOND_NS, SECOND_NS);
        add(
            ANCHOR,
            BROWSER + 1,
            recipient(),
            2,
            2 * SECOND_NS,
            SECOND_NS,
        );
        add(ANCHOR, BROWSER, recipient(), 3, 60 * SECOND_NS, SECOND_NS);

        assert_eq!(discard_expired(2 * SECOND_NS), 2);

        assert_eq!(queued(BROWSER), vec![3]);
        assert!(queued(BROWSER + 1).is_empty());
        let still_indexed = storage_borrow(|storage| {
            storage
                .expired_browser_notifications(u64::MAX, usize::MAX)
                .len()
        });
        assert_eq!(still_indexed, 1);
    }

    #[test]
    fn the_sweep_stops_at_its_bound() {
        crate::notifications::test_setup();
        for browser_id in 0..3 {
            for id in 0..MAX_PER_BROWSER as u64 {
                add(
                    ANCHOR,
                    browser_id,
                    recipient(),
                    id,
                    2 * SECOND_NS,
                    SECOND_NS,
                );
            }
        }

        assert_eq!(discard_expired(2 * SECOND_NS), MAX_PER_SWEEP);
        assert_eq!(
            discard_expired(2 * SECOND_NS),
            3 * MAX_PER_BROWSER - MAX_PER_SWEEP
        );
    }
}
