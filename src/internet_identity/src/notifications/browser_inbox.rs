//! What each browser has yet to collect, one entry per wake-up sent.
//!
//! The canister deposits an entry before posting its wake-up, and the service worker
//! collects one on every wake-up, so wake-ups and collected notifications stay one to
//! one. What the worker renders afterwards is its own business, not this module's.
//!
//! The inbox lives on the browser's entry, and is cleared when its wake-up address
//! changes.

use crate::notifications::dispatch::DeliveryTarget;
use crate::origin::ApplicationOrigin;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, InboxNotification};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, NotificationToShow, Timestamp,
};

/// Most entries one inbox holds. A new one past it replaces the oldest.
pub(crate) const CAPACITY: usize = 100;

/// The origin Internet Identity's own notification is shown under: it is about this
/// identity rather than an app, so there is nothing to name and nothing is looked up.
pub(crate) const NO_APP_ORIGIN: &str = "";

/// Whether a deposited entry calls for a wake-up of its own.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum WakeUp {
    /// Nothing is on its way for this entry yet.
    Needed,
    /// The inbox was at [`CAPACITY`], so its oldest entry was dropped to fit this one.
    /// That entry's wake-up was posted and never answered, so it is still coming, and
    /// the worker will be handed this entry when it arrives.
    AlreadyComing,
}

/// Put a notification behind the inbox's newest, dropping what expired first.
pub(crate) fn deposit(
    inbox: &mut Vec<InboxNotification>,
    notification: InboxNotification,
    now_ns: Timestamp,
) -> WakeUp {
    inbox.retain(|entry| entry.expires_at_ns > now_ns);
    let full = inbox.len() >= CAPACITY;
    if full {
        inbox.remove(0);
    }
    inbox.push(notification);
    if full {
        WakeUp::AlreadyComing
    } else {
        WakeUp::Needed
    }
}

/// A failed post means no wake-up is coming, so one entry goes with it: its own, or the
/// oldest if a wake-up already took it. Nothing goes once it expired or whoever held the
/// posted address registered elsewhere, since expiry or the new registration may have
/// dropped it.
pub(crate) fn remove_after_failed_wake_up(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    target: DeliveryTarget,
    notification: &InboxNotification,
    endpoint: &str,
    now_ns: Timestamp,
) {
    storage_borrow_mut(|storage| {
        let Ok(mut anchor) = storage.read(anchor_number) else {
            return;
        };
        let posted_to = match target {
            DeliveryTarget::Browser => anchor
                .webpush_subscription(browser_id)
                .map(|registered| registered.endpoint.as_str().to_string()),
            DeliveryTarget::App(app_id) => {
                // The app's address stands for this entry only while the entry still
                // points at it. A switch to another record, or an install started, makes
                // the queue somebody else's, and a late failure from the old address
                // would drop a notification this entry can still show.
                let Some(browser) = anchor.browser(browser_id) else {
                    return;
                };
                if !browser.is_pointing_at(app_id) {
                    return;
                }
                storage
                    .notification_app(app_id)
                    .and_then(|app| app.webpush_subscription)
                    .map(|registered| registered.endpoint.into_string())
            }
        };
        if posted_to.is_none_or(|registered| registered != endpoint) {
            return;
        }
        let Some(inbox) = anchor.notifications_for_browser_mut(browser_id) else {
            return;
        };
        match inbox.iter().position(|entry| entry == notification) {
            Some(own) => {
                inbox.remove(own);
            }
            None if notification.expires_at_ns > now_ns && !inbox.is_empty() => {
                inbox.remove(0);
            }
            None => return,
        }
        if let Err(err) = storage.write(anchor) {
            ic_cdk::println!("Failed to drop a notification whose wake-up failed: {err}");
        }
    });
}

/// Drop what an app deposited in every browser's inbox, so consent granted again later
/// brings none of it back.
pub(crate) fn remove_app_entries(anchor_number: AnchorNumber, origin: &ApplicationOrigin) {
    let origin = &origin.as_str().to_string();
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
            if let Some(inbox) = anchor.notifications_for_browser_mut(browser_id) {
                let before = inbox.len();
                inbox.retain(|entry| entry.application_number != Some(application_number));
                removed |= inbox.len() < before;
            }
        }
        if !removed {
            return;
        }
        if let Err(err) = storage.write(anchor) {
            ic_cdk::println!("Failed to drop what an app without consent deposited: {err}");
        }
    });
}

/// The oldest entry the browser may still collect, passing over what it may not be handed.
pub(crate) fn next_to_collect(
    anchor: &Anchor,
    browser_id: BrowserId,
    now_ns: Timestamp,
) -> Option<NotificationToShow> {
    let anchor_number = anchor.anchor_number();
    storage_borrow(|storage| {
        anchor
            .notifications_for_browser(browser_id)
            .iter()
            .filter(|entry| entry.expires_at_ns > now_ns)
            .find_map(|entry| {
                if let Some(event) = entry.event.clone() {
                    return Some(NotificationToShow {
                        origin: NO_APP_ORIGIN.to_string(),
                        account_number: None,
                        canister_id: entry.sender,
                        id: entry.notification_id,
                        event: Some(event),
                    });
                }
                let origin =
                    storage.lookup_origin_with_application_number(entry.application_number?)?;
                let notification = NotificationToShow {
                    origin,
                    account_number: entry.account_number,
                    canister_id: entry.sender,
                    id: entry.notification_id,
                    event: None,
                };
                storage
                    .read_anchor_application_config(anchor_number, &notification.origin)
                    .filter(|config| config.notifications_allowed())
                    .map(|_| notification)
            })
    })
}

/// Remove the entry the service worker collected. One already gone is no error: expiry,
/// a full inbox or a failed wake-up may have dropped it first.
///
/// Which app the entry belongs to is read off the entry rather than off the request.
pub(crate) fn remove_collected(
    mut anchor: Anchor,
    browser_id: BrowserId,
    collected: &NotificationToShow,
    now_ns: Timestamp,
) -> Result<(), String> {
    storage_borrow_mut(|storage| {
        // Every part of what identifies an entry, origin included: a sender may queue
        // the same id for two applications, and one that is expired or belongs to
        // another one must not shadow the live entry being acknowledged.
        // `next_to_collect` hands out only unexpired entries, so this searches the same
        // set it does.
        let collected_from = storage.lookup_application_number_with_origin(&collected.origin);
        let Some(inbox) = anchor.notifications_for_browser_mut(browser_id) else {
            return Ok(());
        };
        let Some(position) = inbox.iter().position(|entry| {
            entry.sender == collected.canister_id
                && entry.notification_id == collected.id
                && entry.account_number == collected.account_number
                && entry.expires_at_ns > now_ns
                // Internet Identity's own is held with no application number and no origin.
                && entry
                    .application_number
                    .is_none_or(|application_number| Some(application_number) == collected_from)
        }) else {
            return Ok(());
        };
        inbox.remove(position);
        storage.write(anchor).map_err(|err| format!("{err}"))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::installed_app::NotificationAppId;
    use crate::notifications::webpush::fixtures::subscription_at;
    use crate::notifications::webpush::fixtures::{
        anchor, app_for_key, app_key, setup, subscribe, APP_RELAY,
    };
    use crate::notifications::write_notifications_decision;

    fn app_origin(origin: &str) -> ApplicationOrigin {
        ApplicationOrigin::try_from(origin).expect("a bare origin")
    }
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{
        IdentityForNotification, NotificationEvent,
    };
    use pretty_assertions::assert_eq;

    const SECOND_NS: u64 = 1_000_000_000;

    fn entry(notification_id: u64, expires_at_ns: Timestamp) -> InboxNotification {
        InboxNotification {
            application_number: Some(1),
            sender: Principal::from_slice(&[7; 10]),
            notification_id,
            expires_at_ns,
            account_number: None,
            event: None,
        }
    }

    fn ids(inbox: &[InboxNotification]) -> Vec<u64> {
        inbox.iter().map(|one| one.notification_id).collect()
    }

    #[test]
    fn an_entry_is_added_behind_the_newest() {
        let mut inbox = Vec::new();

        for id in 1..=3 {
            assert_eq!(
                deposit(&mut inbox, entry(id, 60 * SECOND_NS), SECOND_NS),
                WakeUp::Needed
            );
        }

        assert_eq!(ids(&inbox), vec![1, 2, 3]);
    }

    #[test]
    fn adding_drops_what_expired_first() {
        let mut inbox = vec![entry(1, 2 * SECOND_NS), entry(2, 60 * SECOND_NS)];

        deposit(&mut inbox, entry(3, 60 * SECOND_NS), 2 * SECOND_NS);

        assert_eq!(ids(&inbox), vec![2, 3]);
    }

    #[test]
    fn a_full_inbox_replaces_its_oldest_and_needs_no_new_wake_up() {
        let mut inbox: Vec<_> = (0..CAPACITY as u64)
            .map(|id| entry(id, 60 * SECOND_NS))
            .collect();

        let added = deposit(&mut inbox, entry(1_000, 60 * SECOND_NS), SECOND_NS);

        assert_eq!(added, WakeUp::AlreadyComing);
        assert_eq!(inbox.len(), CAPACITY);
        assert_eq!(inbox.first().map(|one| one.notification_id), Some(1));
        assert_eq!(inbox.last().map(|one| one.notification_id), Some(1_000));
    }

    /// A full inbox of expired entries is a free one.
    #[test]
    fn a_full_inbox_of_expired_entries_takes_a_new_one_with_a_wake_up() {
        let mut inbox: Vec<_> = (0..CAPACITY as u64)
            .map(|id| entry(id, 2 * SECOND_NS))
            .collect();

        let added = deposit(&mut inbox, entry(1_000, 60 * SECOND_NS), 2 * SECOND_NS);

        assert_eq!(added, WakeUp::Needed);
        assert_eq!(ids(&inbox), vec![1_000]);
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
        write_notifications_decision(anchor_number, &app_origin(ORIGIN), true, 0)
            .expect("granting consent");
        (anchor_number, browser_id)
    }

    fn app() -> u64 {
        crate::state::storage_borrow(|storage| {
            storage.lookup_application_number_with_origin(&ORIGIN.to_string())
        })
        .expect("signing in stores the application")
    }

    fn from_app(notification_id: u64, expires_at_ns: Timestamp) -> InboxNotification {
        InboxNotification {
            application_number: Some(app()),
            ..entry(notification_id, expires_at_ns)
        }
    }

    fn put(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        notifications: impl IntoIterator<Item = InboxNotification>,
    ) {
        let mut stored = anchor(anchor_number);
        stored
            .notifications_for_browser_mut(browser_id)
            .expect("a listed browser")
            .extend(notifications);
        storage_borrow_mut(|storage| storage.write(stored)).expect("writing the anchor");
    }

    fn next(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<NotificationToShow> {
        next_to_collect(&anchor(anchor_number), browser_id, now_ns)
    }

    fn next_id(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<u64> {
        next(anchor_number, browser_id, now_ns).map(|one| one.id)
    }

    fn remove(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        notification: &NotificationToShow,
        now_ns: Timestamp,
    ) {
        remove_collected(anchor(anchor_number), browser_id, notification, now_ns)
            .expect("removing");
    }

    fn left_for(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        ids(anchor(anchor_number).notifications_for_browser(browser_id))
    }

    /// The iOS case: the entry holds no registration of its own and is delivered through
    /// the app it points at.
    fn routed_through_an_app() -> (AnchorNumber, BrowserId, NotificationAppId) {
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
        // Outside the borrow below: it takes one of its own.
        let app = app_for_key(app_key(9), 0);
        let app_id = storage_borrow_mut(|storage| {
            storage
                .set_notification_app_subscription(
                    app.id,
                    Some(subscription_at(APP_RELAY, 0, 0)),
                    0,
                )
                .expect("the record is there");
            let mut stored = storage.read(anchor_number).expect("reading the anchor");
            stored.point_at_notification_app(browser_id, app.id);
            storage.write(stored).expect("pointing at the app");
            app.id
        });
        write_notifications_decision(anchor_number, &app_origin(ORIGIN), true, 0)
            .expect("granting consent");
        (anchor_number, browser_id, app_id)
    }

    #[test]
    fn a_failed_app_routed_wake_up_drops_its_entry() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        put(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);

        remove_after_failed_wake_up(
            anchor_number,
            browser_id,
            DeliveryTarget::App(app_id),
            &from_app(7, 60 * SECOND_NS),
            APP_RELAY,
            2 * SECOND_NS,
        );

        assert_eq!(left_for(anchor_number, browser_id), Vec::<u64>::new());
    }

    #[test]
    fn a_failed_app_routed_wake_up_drops_its_entry_though_delivery_was_switched_off() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        put(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);
        let mut stored = anchor(anchor_number);
        stored.disable_notification_app(browser_id);
        storage_borrow_mut(|storage| storage.write(stored)).expect("switching delivery off");

        remove_after_failed_wake_up(
            anchor_number,
            browser_id,
            DeliveryTarget::App(app_id),
            &from_app(7, 60 * SECOND_NS),
            APP_RELAY,
            2 * SECOND_NS,
        );

        assert_eq!(left_for(anchor_number, browser_id), Vec::<u64>::new());
    }

    /// A record another identity still points at keeps its address, so a failure posted
    /// for the app this entry has left still matches on endpoint. Only the entry's own
    /// route says whose queue this is.
    #[test]
    fn a_failed_wake_up_from_the_app_this_entry_left_removes_nothing() {
        use serde_bytes::ByteBuf;

        let (anchor_number, browser_id, left_behind) = routed_through_an_app();
        // Another identity keeps the record alive after this entry moves off it.
        storage_borrow_mut(|storage| {
            let other = storage
                .allocate_anchor(0)
                .expect("allocating a second anchor");
            let other_number = other.anchor_number();
            storage.write(other).expect("writing the second anchor");
            storage.sign_in_for_testing(other_number, &ORIGIN.to_string());
            let mut stored = storage.read(other_number).expect("reading it back");
            let other_browser = stored.browsers().first().expect("a browser").id;
            stored.point_at_notification_app(other_browser, left_behind);
            storage
                .write(stored)
                .expect("pointing the second identity at the app");
        });
        // Outside the borrow below: it takes one of its own.
        let app = app_for_key(ByteBuf::from(vec![8u8; 91]), 0);
        let moved_to = storage_borrow_mut(|storage| {
            storage
                .set_notification_app_subscription(
                    app.id,
                    Some(subscription_at("https://web.push.apple.com/second", 0, 0)),
                    0,
                )
                .expect("the record is there");
            let mut stored = storage.read(anchor_number).expect("reading the anchor");
            stored.point_at_notification_app(browser_id, app.id);
            storage.write(stored).expect("pointing at the second app");
            app.id
        });
        assert_ne!(left_behind, moved_to);
        assert_eq!(
            storage_borrow(|storage| storage.notification_app(left_behind))
                .and_then(|app| app.webpush_subscription)
                .map(|registered| registered.endpoint.into_string()),
            Some(APP_RELAY.to_string()),
            "the app this entry left should keep its address"
        );
        put(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);

        remove_after_failed_wake_up(
            anchor_number,
            browser_id,
            DeliveryTarget::App(left_behind),
            &from_app(9, 60 * SECOND_NS),
            APP_RELAY,
            2 * SECOND_NS,
        );

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }

    #[test]
    fn a_failed_app_routed_wake_up_for_an_old_endpoint_removes_nothing() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        put(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);

        remove_after_failed_wake_up(
            anchor_number,
            browser_id,
            DeliveryTarget::App(app_id),
            &from_app(7, 60 * SECOND_NS),
            "https://web.push.apple.com/b",
            2 * SECOND_NS,
        );

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }

    #[test]
    fn an_acknowledgement_takes_the_entry_it_names_and_not_the_one_its_origin_points_at() {
        let (anchor_number, browser_id) = signed_in();
        put(
            anchor_number,
            browser_id,
            [
                identity_event(1, 60 * SECOND_NS),
                from_app(1, 60 * SECOND_NS),
            ],
        );
        let shown = next(anchor_number, browser_id, 2 * SECOND_NS).expect("the oldest is shown");
        assert!(shown.event.is_some());

        let crafted = NotificationToShow {
            origin: ORIGIN.to_string(),
            event: None,
            ..shown
        };
        remove(anchor_number, browser_id, &crafted, 2 * SECOND_NS);

        assert_eq!(
            anchor(anchor_number).notifications_for_browser(browser_id),
            [from_app(1, 60 * SECOND_NS)]
        );
    }

    fn identity_event(notification_id: u64, expires_at_ns: Timestamp) -> InboxNotification {
        InboxNotification {
            application_number: None,
            event: Some(NotificationEvent::NotificationsOn(
                IdentityForNotification {
                    anchor_number: 10_000,
                    name: None,
                },
            )),
            ..entry(notification_id, expires_at_ns)
        }
    }

    #[test]
    fn an_identity_event_can_be_acknowledged_and_leaves_the_queue() {
        let (anchor_number, browser_id) = signed_in();
        put(
            anchor_number,
            browser_id,
            [
                identity_event(1, 60 * SECOND_NS),
                from_app(2, 60 * SECOND_NS),
            ],
        );

        let shown = next(anchor_number, browser_id, 2 * SECOND_NS).expect("the event is shown");
        assert_eq!(shown.id, 1);
        assert!(shown.event.is_some());
        assert_eq!(shown.origin, NO_APP_ORIGIN);

        remove(anchor_number, browser_id, &shown, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![2]);
        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(2));
    }

    #[test]
    fn the_next_is_the_oldest_and_nothing_is_removed() {
        let (anchor_number, browser_id) = signed_in();
        let for_account = InboxNotification {
            account_number: Some(3),
            ..from_app(7, 60 * SECOND_NS)
        };
        put(
            anchor_number,
            browser_id,
            [for_account, from_app(8, 60 * SECOND_NS)],
        );

        assert_eq!(
            next(anchor_number, browser_id, 2 * SECOND_NS),
            Some(NotificationToShow {
                origin: ORIGIN.to_string(),
                account_number: Some(3),
                canister_id: Principal::from_slice(&[7; 10]),
                id: 7,
                event: None,
            })
        );
        assert_eq!(left_for(anchor_number, browser_id), vec![7, 8]);
    }

    #[test]
    fn an_empty_inbox_has_no_next() {
        let (anchor_number, browser_id) = signed_in();

        assert_eq!(next_id(anchor_number, browser_id, SECOND_NS), None);
    }

    #[test]
    fn the_next_one_comes_once_what_was_collected_is_removed() {
        let (anchor_number, browser_id) = signed_in();
        put(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );
        let collected = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a next one");

        remove(anchor_number, browser_id, &collected, 2 * SECOND_NS);

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(8));
    }

    #[test]
    fn what_expired_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        put(
            anchor_number,
            browser_id,
            [from_app(1, 2 * SECOND_NS), from_app(2, 60 * SECOND_NS)],
        );

        assert_eq!(next_id(anchor_number, browser_id, 3 * SECOND_NS), Some(2));
    }

    #[test]
    fn an_app_ii_no_longer_holds_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        let unknown = InboxNotification {
            application_number: Some(u64::MAX),
            ..entry(1, 60 * SECOND_NS)
        };
        put(
            anchor_number,
            browser_id,
            [unknown, from_app(2, 60 * SECOND_NS)],
        );

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(2));
    }

    #[test]
    fn an_app_that_lost_consent_is_passed_over() {
        let (anchor_number, browser_id) = signed_in();
        put(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        write_notifications_decision(anchor_number, &app_origin(ORIGIN), false, SECOND_NS)
            .expect("revoking consent");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
    }

    #[test]
    fn consent_granted_again_brings_back_nothing_deposited_before_it_was_revoked() {
        let (anchor_number, browser_id) = signed_in();
        put(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        crate::notifications::revoke_consent(anchor_number, &app_origin(ORIGIN), SECOND_NS)
            .expect("revoking consent");
        write_notifications_decision(anchor_number, &app_origin(ORIGIN), true, SECOND_NS)
            .expect("granting consent again");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn a_signed_out_browser_has_no_next() {
        let (anchor_number, browser_id) = signed_in();
        put(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);
        storage_borrow_mut(|storage| {
            storage.revoke_browser_sessions(anchor_number, browser_id, SECOND_NS)
        })
        .expect("signing the browser out");

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), None);
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    #[test]
    fn removing_what_was_collected_leaves_the_rest() {
        let (anchor_number, browser_id) = signed_in();
        put(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );
        let seven = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a first one");

        remove(anchor_number, browser_id, &seven, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![8]);
    }

    #[test]
    fn removing_tells_accounts_apart() {
        let (anchor_number, browser_id) = signed_in();
        let for_account = InboxNotification {
            account_number: Some(3),
            ..from_app(7, 60 * SECOND_NS)
        };
        put(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), for_account],
        );
        let without_account = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a first one");

        remove(anchor_number, browser_id, &without_account, 2 * SECOND_NS);

        assert_eq!(
            anchor(anchor_number).notifications_for_browser(browser_id),
            [InboxNotification {
                account_number: Some(3),
                ..from_app(7, 60 * SECOND_NS)
            }]
        );
    }

    #[test]
    fn removing_what_is_no_longer_held_changes_nothing() {
        let (anchor_number, browser_id) = signed_in();
        put(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);
        let gone = NotificationToShow {
            origin: ORIGIN.to_string(),
            account_number: None,
            canister_id: Principal::from_slice(&[7; 10]),
            id: 8,
            event: None,
        };

        remove(anchor_number, browser_id, &gone, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }
}
