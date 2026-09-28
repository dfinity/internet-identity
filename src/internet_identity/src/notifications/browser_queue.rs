//! What each browser's service worker has yet to show, one entry per wake-up sent.
//! An entry is added before its wake-up is posted, and the service worker removes the
//! one it shows on every wake-up, so wake-ups and shown notifications stay one to one.
//! The queue lives on the browser's Web Push subscription, so it goes with it.
// Filled by the dispatcher, which the submission endpoint kicks in a follow-up PR.
#![allow(dead_code)]

use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, QueuedNotification};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, FrontendHostname, NotificationToShow, Timestamp,
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

/// Drop what an app queued on every browser of the identity, so consent granted again
/// later brings none of it back.
pub(crate) fn remove_app(anchor_number: AnchorNumber, origin: &FrontendHostname) {
    storage_borrow_mut(|storage| {
        let Some(application_number) = storage.lookup_application_number_with_origin(origin) else {
            return;
        };
        let Ok(mut anchor) = storage.read(anchor_number) else {
            return;
        };
        let browser_ids: Vec<BrowserId> =
            anchor.browsers().iter().map(|browser| browser.id).collect();
        let mut removed = false;
        for browser_id in browser_ids {
            if let Some(queue) = anchor.notifications_mut(browser_id) {
                let before = queue.len();
                queue.retain(|queued| queued.application_number != application_number);
                removed |= queue.len() < before;
            }
        }
        if !removed {
            return;
        }
        if let Err(err) = storage.write(anchor) {
            ic_cdk::println!("Failed to drop what an app without consent queued: {err}");
        }
    });
}

/// The oldest entry the browser has yet to show, passing over what the service worker
/// skips, what expired, and what belongs to an app that lost consent or that II no
/// longer holds.
pub(crate) fn next_to_show(
    anchor: &Anchor,
    browser_id: BrowserId,
    skip: &[NotificationToShow],
    now_ns: Timestamp,
) -> Option<NotificationToShow> {
    let anchor_number = anchor.anchor_number();
    storage_borrow(|storage| {
        anchor
            .notifications(browser_id)
            .iter()
            .filter(|queued| queued.expires_at_ns > now_ns)
            .find_map(|queued| {
                let origin =
                    storage.lookup_origin_with_application_number(queued.application_number)?;
                let notification = NotificationToShow {
                    origin,
                    account_number: queued.account_number,
                    canister_id: queued.sender,
                    id: queued.notification_id,
                };
                if skip.contains(&notification) {
                    return None;
                }
                storage
                    .read_anchor_application_config(anchor_number, &notification.origin)
                    .and_then(|config| config.notifications_consented_at_ns)
                    .map(|_| notification)
            })
    })
}

/// Remove the entry the service worker shows. One already gone is no error: expiry, a
/// full queue or a failed wake-up may have dropped it first.
pub(crate) fn remove_shown(
    mut anchor: Anchor,
    browser_id: BrowserId,
    shown: &NotificationToShow,
) -> Result<(), String> {
    storage_borrow_mut(|storage| {
        let Some(application_number) = storage.lookup_application_number_with_origin(&shown.origin)
        else {
            return Ok(());
        };
        let Some(queue) = anchor.notifications_mut(browser_id) else {
            return Ok(());
        };
        let Some(position) = queue.iter().position(|queued| {
            queued.application_number == application_number
                && queued.sender == shown.canister_id
                && queued.notification_id == shown.id
                && queued.account_number == shown.account_number
        }) else {
            return Ok(());
        };
        queue.remove(position);
        storage.write(anchor).map_err(|err| format!("{err}"))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::webpush::fixtures::{anchor, setup, subscribe};
    use crate::notifications::write_consent;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::NotificationRevokeConsentRequest;
    use pretty_assertions::assert_eq;

    const SECOND_NS: u64 = 1_000_000_000;

    fn queued(notification_id: u64, expires_at_ns: Timestamp) -> QueuedNotification {
        QueuedNotification {
            application_number: 1,
            sender: Principal::from_slice(&[7; 10]),
            notification_id,
            expires_at_ns,
            account_number: None,
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

    fn next(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        skip: &[NotificationToShow],
        now_ns: Timestamp,
    ) -> Option<NotificationToShow> {
        next_to_show(&anchor(anchor_number), browser_id, skip, now_ns)
    }

    fn next_id(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<u64> {
        next(anchor_number, browser_id, &[], now_ns).map(|one| one.id)
    }

    fn remove(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        notification: &NotificationToShow,
    ) {
        remove_shown(anchor(anchor_number), browser_id, notification).expect("removing");
    }

    fn left_for(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        ids(anchor(anchor_number).notifications(browser_id))
    }

    #[test]
    fn the_next_is_the_oldest_and_nothing_is_removed() {
        let (anchor_number, browser_id) = signed_in();
        let for_account = QueuedNotification {
            account_number: Some(3),
            ..from_app(7, 60 * SECOND_NS)
        };
        enqueue(
            anchor_number,
            browser_id,
            [for_account, from_app(8, 60 * SECOND_NS)],
        );

        assert_eq!(
            next(anchor_number, browser_id, &[], 2 * SECOND_NS),
            Some(NotificationToShow {
                origin: ORIGIN.to_string(),
                account_number: Some(3),
                canister_id: Principal::from_slice(&[7; 10]),
                id: 7,
            })
        );
        assert_eq!(left_for(anchor_number, browser_id), vec![7, 8]);
    }

    #[test]
    fn an_empty_queue_has_no_next() {
        let (anchor_number, browser_id) = signed_in();

        assert_eq!(next_id(anchor_number, browser_id, SECOND_NS), None);
    }

    #[test]
    fn what_the_service_worker_skips_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );
        let showing = next(anchor_number, browser_id, &[], 2 * SECOND_NS).expect("a next one");

        let after = next(anchor_number, browser_id, &[showing], 2 * SECOND_NS);

        assert_eq!(after.map(|one| one.id), Some(8));
    }

    #[test]
    fn skipping_one_account_passes_over_only_that_account() {
        let (anchor_number, browser_id) = signed_in();
        let for_account = QueuedNotification {
            account_number: Some(3),
            ..from_app(7, 60 * SECOND_NS)
        };
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), for_account],
        );
        let showing = next(anchor_number, browser_id, &[], 2 * SECOND_NS).expect("a next one");

        let after = next(anchor_number, browser_id, &[showing], 2 * SECOND_NS);

        assert_eq!(after.and_then(|one| one.account_number), Some(3));
    }

    #[test]
    fn what_expired_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(1, 2 * SECOND_NS), from_app(2, 60 * SECOND_NS)],
        );

        assert_eq!(next_id(anchor_number, browser_id, 3 * SECOND_NS), Some(2));
    }

    #[test]
    fn an_app_ii_no_longer_holds_is_passed_over() {
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

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(2));
    }

    #[test]
    fn an_app_that_lost_consent_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        write_consent(anchor_number, &ORIGIN.to_string(), None, SECOND_NS)
            .expect("revoking consent");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
    }

    #[test]
    fn consent_granted_again_brings_back_nothing_queued_before_it_was_revoked() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        let revoke = NotificationRevokeConsentRequest {
            anchor_number,
            origin: ORIGIN.to_string(),
        };
        crate::notifications::revoke_consent(
            revoke.try_into().expect("a valid request"),
            SECOND_NS,
        )
        .expect("revoking consent");
        write_consent(
            anchor_number,
            &ORIGIN.to_string(),
            Some(SECOND_NS),
            SECOND_NS,
        )
        .expect("granting consent again");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn a_signed_out_browser_has_no_next() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        storage_borrow_mut(|storage| {
            storage.revoke_browser_sessions(anchor_number, browser_id, SECOND_NS)
        })
        .expect("signing the browser out");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn removing_what_is_shown_leaves_the_rest() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );
        let eight = next(anchor_number, browser_id, &[], 2 * SECOND_NS)
            .and_then(|seven| next(anchor_number, browser_id, &[seven], 2 * SECOND_NS))
            .expect("a second one");

        remove(anchor_number, browser_id, &eight);

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }

    #[test]
    fn removing_tells_accounts_apart() {
        let (anchor_number, browser_id) = signed_in();
        let for_account = QueuedNotification {
            account_number: Some(3),
            ..from_app(7, 60 * SECOND_NS)
        };
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), for_account],
        );
        let for_account = next(anchor_number, browser_id, &[], 2 * SECOND_NS)
            .and_then(|first| next(anchor_number, browser_id, &[first], 2 * SECOND_NS))
            .expect("a second one");

        remove(anchor_number, browser_id, &for_account);

        assert_eq!(
            anchor(anchor_number).notifications(browser_id),
            [from_app(7, 60 * SECOND_NS)]
        );
    }

    #[test]
    fn removing_what_is_no_longer_queued_changes_nothing() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);
        let gone = NotificationToShow {
            origin: ORIGIN.to_string(),
            account_number: None,
            canister_id: Principal::from_slice(&[7; 10]),
            id: 8,
        };

        remove(anchor_number, browser_id, &gone);

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }
}
