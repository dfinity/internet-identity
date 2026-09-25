//! What each browser's service worker has yet to show, one entry per wake-up sent.
//! An entry is added before its wake-up is posted and the service worker takes the
//! oldest on every wake-up, so wake-ups and shown notifications stay one to one. The
//! queue lives on the browser entry, so it goes with the browser and its registration.
// Filled by the dispatcher, which the submission endpoint kicks in a follow-up PR.
#![allow(dead_code)]

use crate::state::storage_borrow_mut;
use crate::storage::anchor::{Anchor, QueuedNotification};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, NotificationToShow, Timestamp,
};

/// Most entries one browser holds. A new one past it replaces the oldest.
pub(crate) const MAX_PER_BROWSER: usize = 100;

/// Where an added entry went.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Added {
    /// Its wake-up is still to be posted.
    Queued,
    /// The queue was full, so the oldest entry made way. The wake-up already on its way
    /// for that one covers this one.
    ReplacedOldest,
}

/// Queue a notification behind the browser's newest, dropping what expired first.
pub(crate) fn add(
    queue: &mut Vec<QueuedNotification>,
    notification: QueuedNotification,
    now_ns: Timestamp,
) -> Added {
    queue.retain(|queued| queued.expires_at_ns > now_ns);
    let full = queue.len() >= MAX_PER_BROWSER;
    if full {
        queue.remove(0);
    }
    queue.push(notification);
    if full {
        Added::ReplacedOldest
    } else {
        Added::Queued
    }
}

/// A failed post means no wake-up is coming, so one entry goes with it: its own, or the
/// oldest if a wake-up already took it. Nothing goes once it expired or the browser
/// registered elsewhere, since expiry or the new registration may have dropped it.
pub(crate) fn remove_after_failed_wake_up(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    notification: &QueuedNotification,
    endpoint: &str,
    now_ns: Timestamp,
) {
    storage_borrow_mut(|storage| {
        let Ok(mut anchor) = storage.read(anchor_number) else {
            return;
        };
        if anchor
            .webpush_subscription(browser_id)
            .is_none_or(|registered| registered.endpoint != endpoint)
        {
            return;
        }
        let Some(queue) = anchor.notifications_mut(browser_id) else {
            return;
        };
        match queue.iter().position(|queued| queued == notification) {
            Some(own) => {
                queue.remove(own);
            }
            None if notification.expires_at_ns > now_ns && !queue.is_empty() => {
                queue.remove(0);
            }
            None => return,
        }
        if let Err(err) = storage.write(anchor) {
            ic_cdk::println!("Failed to drop a notification whose wake-up failed: {err}");
        }
    });
}

/// Take the oldest entry still worth showing, dropping on the way what expired or
/// belongs to an app that lost consent or that II no longer holds.
pub(crate) fn take_next(
    mut anchor: Anchor,
    browser_id: BrowserId,
    now_ns: Timestamp,
) -> Result<Option<NotificationToShow>, String> {
    let anchor_number = anchor.anchor_number();
    storage_borrow_mut(|storage| {
        let Some(queue) = anchor.notifications_mut(browser_id) else {
            return Ok(None);
        };
        if queue.is_empty() {
            return Ok(None);
        }
        let mut shown = None;
        while shown.is_none() && !queue.is_empty() {
            let queued = queue.remove(0);
            if queued.expires_at_ns <= now_ns {
                continue;
            }
            let Some(origin) =
                storage.lookup_origin_with_application_number(queued.application_number)
            else {
                continue;
            };
            let consented = storage
                .read_anchor_application_config(anchor_number, &origin)
                .and_then(|config| config.notifications_consented_at_ns)
                .is_some();
            if consented {
                shown = Some(NotificationToShow {
                    origin,
                    canister_id: queued.sender,
                    id: queued.notification_id,
                });
            }
        }
        storage.write(anchor).map_err(|err| format!("{err}"))?;
        Ok(shown)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::webpush::fixtures::{anchor, setup, subscribe};
    use crate::notifications::write_consent;
    use candid::Principal;
    use pretty_assertions::assert_eq;

    const SECOND_NS: u64 = 1_000_000_000;

    fn queued(notification_id: u64, expires_at_ns: Timestamp) -> QueuedNotification {
        QueuedNotification {
            application_number: 1,
            sender: Principal::from_slice(&[7; 10]),
            notification_id,
            expires_at_ns,
        }
    }

    fn ids(queue: &[QueuedNotification]) -> Vec<u64> {
        queue.iter().map(|one| one.notification_id).collect()
    }

    #[test]
    fn an_entry_is_added_behind_the_newest() {
        let mut queue = Vec::new();

        for id in 1..=3 {
            assert_eq!(
                add(&mut queue, queued(id, 60 * SECOND_NS), SECOND_NS),
                Added::Queued
            );
        }

        assert_eq!(ids(&queue), vec![1, 2, 3]);
    }

    #[test]
    fn adding_drops_what_expired_first() {
        let mut queue = vec![queued(1, 2 * SECOND_NS), queued(2, 60 * SECOND_NS)];

        add(&mut queue, queued(3, 60 * SECOND_NS), 2 * SECOND_NS);

        assert_eq!(ids(&queue), vec![2, 3]);
    }

    #[test]
    fn a_full_queue_replaces_its_oldest_and_needs_no_new_wake_up() {
        let mut queue: Vec<_> = (0..MAX_PER_BROWSER as u64)
            .map(|id| queued(id, 60 * SECOND_NS))
            .collect();

        let added = add(&mut queue, queued(1_000, 60 * SECOND_NS), SECOND_NS);

        assert_eq!(added, Added::ReplacedOldest);
        assert_eq!(queue.len(), MAX_PER_BROWSER);
        assert_eq!(queue.first().map(|one| one.notification_id), Some(1));
        assert_eq!(queue.last().map(|one| one.notification_id), Some(1_000));
    }

    /// A full queue of expired entries is a free one.
    #[test]
    fn a_full_queue_of_expired_entries_takes_a_new_one_with_a_wake_up() {
        let mut queue: Vec<_> = (0..MAX_PER_BROWSER as u64)
            .map(|id| queued(id, 2 * SECOND_NS))
            .collect();

        let added = add(&mut queue, queued(1_000, 60 * SECOND_NS), 2 * SECOND_NS);

        assert_eq!(added, Added::Queued);
        assert_eq!(ids(&queue), vec![1_000]);
    }

    const ORIGIN: &str = "https://app.example";
    const RELAY: &str = "https://relay.example/a";

    /// An identity signed in at `ORIGIN` from a registered browser, with the app
    /// allowed to notify it.
    fn signed_in() -> (AnchorNumber, BrowserId) {
        setup();
        let anchor_number = storage_borrow_mut(|storage| {
            let anchor = storage.allocate_anchor(0).expect("allocating an anchor");
            let anchor_number = anchor.anchor_number();
            storage.write(anchor).expect("writing the anchor");
            storage.sign_in_for_testing(anchor_number, &ORIGIN.to_string());
            anchor_number
        });
        let browser_id = anchor(anchor_number)
            .browsers()
            .first()
            .expect("signing in registers the browser")
            .id;
        subscribe(anchor_number, browser_id, RELAY, 0);
        write_consent(anchor_number, &ORIGIN.to_string(), Some(0), 0).expect("granting consent");
        (anchor_number, browser_id)
    }

    fn app() -> u64 {
        crate::state::storage_borrow(|storage| {
            storage.lookup_application_number_with_origin(&ORIGIN.to_string())
        })
        .expect("signing in stores the application")
    }

    fn from_app(notification_id: u64, expires_at_ns: Timestamp) -> QueuedNotification {
        QueuedNotification {
            application_number: app(),
            ..queued(notification_id, expires_at_ns)
        }
    }

    fn enqueue(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        notifications: impl IntoIterator<Item = QueuedNotification>,
    ) {
        let mut stored = anchor(anchor_number);
        stored
            .notifications_mut(browser_id)
            .expect("a listed browser")
            .extend(notifications);
        storage_borrow_mut(|storage| storage.write(stored)).expect("writing the anchor");
    }

    fn take(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<NotificationToShow> {
        take_next(anchor(anchor_number), browser_id, now_ns).expect("taking")
    }

    fn left_for(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        ids(anchor(anchor_number).notifications(browser_id))
    }

    #[test]
    fn the_service_worker_takes_the_oldest_first() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );

        let first = take(anchor_number, browser_id, 2 * SECOND_NS).expect("nothing to take");

        assert_eq!(
            first,
            NotificationToShow {
                origin: ORIGIN.to_string(),
                canister_id: Principal::from_slice(&[7; 10]),
                id: 7,
            }
        );
        assert_eq!(left_for(anchor_number, browser_id), vec![8]);
    }

    #[test]
    fn an_empty_queue_has_nothing_to_take() {
        let (anchor_number, browser_id) = signed_in();

        assert_eq!(take(anchor_number, browser_id, SECOND_NS), None);
    }

    #[test]
    fn taking_skips_what_expired() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(1, 2 * SECOND_NS), from_app(2, 60 * SECOND_NS)],
        );

        let taken = take(anchor_number, browser_id, 3 * SECOND_NS).expect("nothing to take");

        assert_eq!(taken.id, 2);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn taking_skips_an_app_ii_no_longer_holds() {
        let (anchor_number, browser_id) = signed_in();
        let unknown = QueuedNotification {
            application_number: u64::MAX,
            ..queued(1, 60 * SECOND_NS)
        };
        enqueue(
            anchor_number,
            browser_id,
            [unknown, from_app(2, 60 * SECOND_NS)],
        );

        let taken = take(anchor_number, browser_id, 2 * SECOND_NS).expect("nothing to take");

        assert_eq!(taken.id, 2);
    }

    #[test]
    fn taking_skips_an_app_that_lost_consent() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        write_consent(anchor_number, &ORIGIN.to_string(), None, SECOND_NS)
            .expect("revoking consent");

        assert_eq!(take(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn a_signed_out_browser_takes_nothing() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        storage_borrow_mut(|storage| {
            storage.revoke_browser_sessions(anchor_number, browser_id, SECOND_NS)
        })
        .expect("signing the browser out");

        assert_eq!(take(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }
}
