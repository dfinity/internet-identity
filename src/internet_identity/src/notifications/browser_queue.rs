//! What each browser's service worker has yet to show, one entry per wake-up sent.
//! An entry is added before its wake-up is posted, and the service worker removes the
//! one it shows on every wake-up, so wake-ups and shown notifications stay one to one.
//! The queue lives on the browser's entry, because an address can be the Home Screen
//! app's and shared with every identity pointing at it while what is waiting is one
//! identity's alone. It is cleared whenever the address its wake-ups were posted to goes
//! away or changes, whether that address is the entry's own or the app's.

use crate::authz_utils::NotificationCaller;
use crate::notifications::dispatch::DeliveryTarget;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, QueuedNotification};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, FrontendHostname, NotificationId, NotificationToShow, Timestamp,
};

/// Most entries one browser holds. A new one past it replaces the oldest.
pub(crate) const MAX_PER_BROWSER: usize = 100;

/// The origin Internet Identity's own notification is shown under. It is about this
/// identity rather than about an app, so there is no app to name, and the empty
/// authority is no origin at all: it resolves to no application and the service worker
/// reads the event instead. Nothing is looked up from it and nothing is matched on it.
pub(crate) const NO_APP_ORIGIN: &str = "";

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
/// oldest if a wake-up already took it. Nothing goes once it expired or whoever held the
/// posted address registered elsewhere, since expiry or the new registration may have
/// dropped it.
///
/// `target` says who held that address, so this asks only whether it is still registered
/// there. Whether the entry is routing through the app right now is a different
/// question, and answering it here would strand a queue entry with no wake-up coming
/// every time delivery was switched off or the identity signed out mid-flight.
pub(crate) fn remove_after_failed_wake_up(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    target: DeliveryTarget,
    notification: &QueuedNotification,
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
                .map(|registered| registered.endpoint.clone()),
            DeliveryTarget::App(app_id) => storage
                .notification_app(app_id)
                .and_then(|app| app.webpush_subscription)
                .map(|registered| registered.endpoint),
        };
        if posted_to.is_none_or(|registered| registered != endpoint) {
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
        anchor.clear_pending_probe_if_gone(browser_id);
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
                queue.retain(|queued| queued.application_number != Some(application_number));
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

/// The pending probe the caller may not answer, which is one a browser asked of the app
/// it points at.
///
/// The Safari tab and the Home Screen app resolve to the same entry and read the same
/// queue, so without this a browser worker could show the probe and acknowledge it, and
/// a probe answered by the surface it was not posted to reports an app alive on evidence
/// about the browser.
fn probe_only_the_app_may_answer(
    anchor: &Anchor,
    browser_id: BrowserId,
    caller: &NotificationCaller,
    now_ns: Timestamp,
) -> Option<NotificationId> {
    match caller {
        NotificationCaller::App(_) => None,
        NotificationCaller::Browser => anchor.pending_probe(browser_id, now_ns),
    }
}

/// Whether this entry is the probe `probe` names. Only Internet Identity's own carries
/// one, so an app's entry is never it: an app chooses its own notification ids while
/// ours come from a counter of its own, and the two spaces overlap from the first id an
/// app picks.
fn is_the_probe(queued: &QueuedNotification, probe: Option<NotificationId>) -> bool {
    queued.application_number.is_none() && Some(queued.notification_id) == probe
}

/// The oldest entry the browser has yet to show, passing over what expired, what
/// belongs to an app that lost consent or that II no longer holds, and a probe a
/// browser caller may not answer.
pub(crate) fn next_to_show(
    anchor: &Anchor,
    browser_id: BrowserId,
    caller: &NotificationCaller,
    now_ns: Timestamp,
) -> Option<NotificationToShow> {
    let anchor_number = anchor.anchor_number();
    let probe = probe_only_the_app_may_answer(anchor, browser_id, caller, now_ns);
    storage_borrow(|storage| {
        anchor
            .notifications(browser_id)
            .iter()
            .filter(|queued| queued.expires_at_ns > now_ns)
            .filter(|queued| !is_the_probe(queued, probe))
            .find_map(|queued| {
                // Internet Identity's own is about this identity rather than about an
                // app, so there is no consent to check and no origin to look up: it is
                // shown because the user did something here that is worth saying.
                if let Some(event) = queued.event.clone() {
                    return Some(NotificationToShow {
                        origin: NO_APP_ORIGIN.to_string(),
                        account_number: None,
                        canister_id: queued.sender,
                        id: queued.notification_id,
                        event: Some(event),
                    });
                }
                let origin =
                    storage.lookup_origin_with_application_number(queued.application_number?)?;
                let notification = NotificationToShow {
                    origin,
                    account_number: queued.account_number,
                    canister_id: queued.sender,
                    id: queued.notification_id,
                    event: None,
                };
                storage
                    .read_anchor_application_config(anchor_number, &notification.origin)
                    .filter(|config| config.notifications_allowed())
                    .map(|_| notification)
            })
    })
}

/// Remove the entry the service worker shows. One already gone is no error: expiry, a
/// full queue or a failed wake-up may have dropped it first. A probe a browser caller
/// may not answer is no candidate at all, so it is left where it is for the app it was
/// posted to, and an entry of the app's carrying the same id is still taken.
///
/// Which app the entry belongs to is read off the entry rather than off the request: a
/// caller saying it showed one of Internet Identity's own would otherwise decide that no
/// origin is looked up, and so decide which entries the search can match at all.
pub(crate) fn remove_shown(
    mut anchor: Anchor,
    browser_id: BrowserId,
    caller: &NotificationCaller,
    shown: &NotificationToShow,
    now_ns: Timestamp,
) -> Result<(), String> {
    let probe = probe_only_the_app_may_answer(&anchor, browser_id, caller, now_ns);
    storage_borrow_mut(|storage| {
        let Some(position) = anchor.notifications(browser_id).iter().position(|queued| {
            !is_the_probe(queued, probe)
                && queued.sender == shown.canister_id
                && queued.notification_id == shown.id
                && queued.account_number == shown.account_number
        }) else {
            return Ok(());
        };
        // Internet Identity's own belongs to no app: it is about this identity, so it is
        // queued with no application number and there is no origin to agree about.
        let names_another_app = anchor.notifications(browser_id)[position]
            .application_number
            .is_some_and(|application_number| {
                storage.lookup_application_number_with_origin(&shown.origin)
                    != Some(application_number)
            });
        if names_another_app {
            return Ok(());
        }
        let Some(queue) = anchor.notifications_mut(browser_id) else {
            return Ok(());
        };
        queue.remove(position);
        anchor.clear_pending_probe_if_gone(browser_id);
        storage.write(anchor).map_err(|err| format!("{err}"))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::app::NotificationAppId;
    use crate::notifications::webpush::fixtures::{anchor, setup, subscribe};
    use crate::notifications::write_notifications_decision;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{
        IdentityForNotification, NotificationEvent, NotificationRevokeConsentRequest,
    };
    use pretty_assertions::assert_eq;

    const SECOND_NS: u64 = 1_000_000_000;

    fn queued(notification_id: u64, expires_at_ns: Timestamp) -> QueuedNotification {
        QueuedNotification {
            application_number: Some(1),
            sender: Principal::from_slice(&[7; 10]),
            notification_id,
            expires_at_ns,
            account_number: None,
            event: None,
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
        write_notifications_decision(anchor_number, &ORIGIN.to_string(), true, 0)
            .expect("granting consent");
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
            application_number: Some(app()),
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

    /// What the Home Screen app signing as `app_id` reads and acknowledges.
    fn as_app(app_id: NotificationAppId) -> NotificationCaller {
        NotificationCaller::App(
            storage_borrow(|storage| storage.notification_app(app_id)).expect("a stored app"),
        )
    }

    fn next_as(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        caller: &NotificationCaller,
        now_ns: Timestamp,
    ) -> Option<NotificationToShow> {
        next_to_show(&anchor(anchor_number), browser_id, caller, now_ns)
    }

    fn next(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<NotificationToShow> {
        next_as(
            anchor_number,
            browser_id,
            &NotificationCaller::Browser,
            now_ns,
        )
    }

    fn next_id(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<u64> {
        next(anchor_number, browser_id, now_ns).map(|one| one.id)
    }

    fn remove_as(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        caller: &NotificationCaller,
        notification: &NotificationToShow,
        now_ns: Timestamp,
    ) {
        remove_shown(
            anchor(anchor_number),
            browser_id,
            caller,
            notification,
            now_ns,
        )
        .expect("removing");
    }

    fn remove(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        notification: &NotificationToShow,
        now_ns: Timestamp,
    ) {
        remove_as(
            anchor_number,
            browser_id,
            &NotificationCaller::Browser,
            notification,
            now_ns,
        );
    }

    fn left_for(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        ids(anchor(anchor_number).notifications(browser_id))
    }

    const APP_RELAY: &str = "https://web.push.apple.com/a";

    /// The iOS case: the entry holds no registration of its own and is delivered through
    /// the record of the Home Screen app it points at.
    fn routed_through_an_app() -> (AnchorNumber, BrowserId, NotificationAppId) {
        use crate::storage::anchor::WebPushSubscription;
        use serde_bytes::ByteBuf;

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
        let app_id = storage_borrow_mut(|storage| {
            let app = storage
                .notification_app_for_key(&ByteBuf::from(vec![9u8; 91]), 0)
                .expect("creating the app record");
            storage.set_notification_app_subscription(
                app.id,
                Some(WebPushSubscription {
                    endpoint: APP_RELAY.to_string(),
                    created_at_ns: 0,
                    vapid_public_key: vec![1; 65],
                    jwt_signatures: vec![vec![2; 64]],
                    jwt_issued_at_ns: 0,
                }),
                0,
            );
            let mut stored = storage.read(anchor_number).expect("reading the anchor");
            stored.point_at_notification_app(browser_id, app.id);
            storage.write(stored).expect("pointing at the app");
            app.id
        });
        write_notifications_decision(anchor_number, &ORIGIN.to_string(), true, 0)
            .expect("granting consent");
        (anchor_number, browser_id, app_id)
    }

    /// The app's registration is read under the borrow this already holds, so a post
    /// that failed on the app's endpoint drops its entry rather than trapping.
    #[test]
    fn a_failed_app_routed_wake_up_drops_its_entry() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        enqueue(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);

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

    /// Switching delivery off does not put the failed wake-up back on its way, so the
    /// entry it announced is still one nothing is coming for. Asking whether the app
    /// routes for this identity right now would leave it queued for its full hour,
    /// holding back the wake-up for everything behind it.
    #[test]
    fn a_failed_app_routed_wake_up_drops_its_entry_though_delivery_was_switched_off() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        enqueue(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);
        let mut stored = anchor(anchor_number);
        stored.set_notification_app_enabled(browser_id, false);
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

    /// A post that failed on an address the app no longer holds says nothing about what
    /// is queued now: the new registration may have dropped it already.
    #[test]
    fn a_failed_app_routed_wake_up_for_an_old_endpoint_removes_nothing() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        enqueue(anchor_number, browser_id, [from_app(7, 60 * SECOND_NS)]);

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

    /// A probe this browser asked of the app it points at, queued and remembered on the
    /// entry the way the dispatch leaves it.
    fn probe(anchor_number: AnchorNumber, browser_id: BrowserId, notification_id: u64) {
        enqueue(
            anchor_number,
            browser_id,
            [identity_event(notification_id, 60 * SECOND_NS)],
        );
        let mut stored = anchor(anchor_number);
        stored.set_pending_probe(browser_id, notification_id);
        storage_borrow_mut(|storage| storage.write(stored)).expect("recording the probe");
    }

    /// A failed relay post drops the probe's entry, so an empty pending probe says only
    /// that the entry is gone. It is not evidence that the app took anything, and what
    /// proves the app is alive is the stamp its own call leaves.
    #[test]
    fn a_probe_stops_being_pending_when_its_post_failed() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        probe(anchor_number, browser_id, 1);
        assert_eq!(
            anchor(anchor_number).pending_probe(browser_id, 2 * SECOND_NS),
            Some(1)
        );

        remove_after_failed_wake_up(
            anchor_number,
            browser_id,
            DeliveryTarget::App(app_id),
            &identity_event(1, 60 * SECOND_NS),
            APP_RELAY,
            2 * SECOND_NS,
        );

        assert_eq!(
            anchor(anchor_number).pending_probe(browser_id, 2 * SECOND_NS),
            None
        );
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    /// The Safari tab and the Home Screen app resolve to the same entry and read the
    /// same queue. A probe is posted to the app alone, so a browser worker shown it
    /// could answer a question about the app with evidence about the browser.
    #[test]
    fn only_the_app_is_shown_the_probe_it_was_sent() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        probe(anchor_number, browser_id, 1);
        enqueue(anchor_number, browser_id, [from_app(2, 60 * SECOND_NS)]);

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(2));
        assert_eq!(
            next_as(anchor_number, browser_id, &as_app(app_id), 2 * SECOND_NS).map(|one| one.id),
            Some(1)
        );
    }

    /// An app numbers its own notifications and starts wherever it likes, while ours come
    /// from a counter of this canister's starting at zero, so the two spaces overlap from
    /// the first id an app picks. A probe id left on the entry must veto the probe and
    /// nothing else: an app notification sharing that id would never be shown and could
    /// never be acknowledged, sitting queued until it expired while every re-wake fired
    /// for it again.
    #[test]
    fn an_app_notification_sharing_a_stale_probe_id_is_still_shown_and_taken() {
        let (anchor_number, browser_id, _) = routed_through_an_app();
        let mut stored = anchor(anchor_number);
        stored.set_pending_probe(browser_id, 1);
        storage_borrow_mut(|storage| storage.write(stored)).expect("recording the probe");
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);

        let shown = next(anchor_number, browser_id, 2 * SECOND_NS).expect("the app's own is shown");

        assert_eq!(shown.id, 1);
        assert_eq!(shown.event, None);

        remove(anchor_number, browser_id, &shown, 2 * SECOND_NS);

        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    /// The same, with the probe still queued: the veto is about the entry the probe
    /// names and not about its id, so the app's notification is shown to the browser
    /// while the probe stays where it is for the app.
    #[test]
    fn an_app_notification_sharing_a_queued_probe_id_is_shown_and_leaves_the_probe() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        probe(anchor_number, browser_id, 1);
        enqueue(anchor_number, browser_id, [from_app(1, 60 * SECOND_NS)]);

        let shown = next(anchor_number, browser_id, 2 * SECOND_NS).expect("the app's own is shown");
        assert_eq!(shown.id, 1);
        assert_eq!(shown.event, None);

        remove(anchor_number, browser_id, &shown, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![1]);
        assert_eq!(
            next_as(anchor_number, browser_id, &as_app(app_id), 2 * SECOND_NS).map(|one| one.id),
            Some(1)
        );
    }

    /// Which app an entry belongs to is the entry's own. A request saying it showed one
    /// of Internet Identity's own would otherwise decide that no origin is looked up, and
    /// so decide which entries the search can match at all: here the crafted origin would
    /// take the app's entry, which this browser was never shown.
    #[test]
    fn an_acknowledgement_takes_the_entry_it_names_and_not_the_one_its_origin_points_at() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
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
            anchor(anchor_number).notifications(browser_id),
            [from_app(1, 60 * SECOND_NS)]
        );
    }

    /// The probe is the one of Internet Identity's own a browser worker is not shown, so
    /// it is the one a crafted acknowledgement would take: the origin it carries is the
    /// empty one and the event is whatever it says it is.
    #[test]
    fn a_crafted_acknowledgement_leaves_the_probe_for_the_app() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        probe(anchor_number, browser_id, 1);
        let crafted = NotificationToShow {
            origin: NO_APP_ORIGIN.to_string(),
            account_number: None,
            canister_id: Principal::from_slice(&[7; 10]),
            id: 1,
            event: Some(NotificationEvent::NotificationsOn(
                IdentityForNotification {
                    anchor_number: 10_000,
                    name: None,
                },
            )),
        };

        remove(anchor_number, browser_id, &crafted, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![1]);
        let shown = next_as(anchor_number, browser_id, &as_app(app_id), 2 * SECOND_NS)
            .expect("the app is still shown the probe");
        remove_as(
            anchor_number,
            browser_id,
            &as_app(app_id),
            &shown,
            2 * SECOND_NS,
        );
        assert!(left_for(anchor_number, browser_id).is_empty());
    }

    /// An empty pending probe is read as the app having taken it, so a browser worker
    /// acknowledging one would report an app that is gone as alive.
    #[test]
    fn only_the_app_may_acknowledge_the_probe_it_was_sent() {
        let (anchor_number, browser_id, app_id) = routed_through_an_app();
        probe(anchor_number, browser_id, 1);
        let shown = next_as(anchor_number, browser_id, &as_app(app_id), 2 * SECOND_NS)
            .expect("the app is shown the probe");

        remove_as(
            anchor_number,
            browser_id,
            &NotificationCaller::Browser,
            &shown,
            2 * SECOND_NS,
        );
        assert_eq!(left_for(anchor_number, browser_id), vec![1]);

        remove_as(
            anchor_number,
            browser_id,
            &as_app(app_id),
            &shown,
            2 * SECOND_NS,
        );
        assert_eq!(left_for(anchor_number, browser_id), Vec::<u64>::new());
    }

    fn identity_event(notification_id: u64, expires_at_ns: Timestamp) -> QueuedNotification {
        QueuedNotification {
            application_number: None,
            event: Some(NotificationEvent::NotificationsOn(
                IdentityForNotification {
                    anchor_number: 10_000,
                    name: None,
                },
            )),
            ..queued(notification_id, expires_at_ns)
        }
    }

    /// Internet Identity's own belongs to no app, so it has no origin to resolve and no
    /// application number to match on. Without both of those the entry can never leave
    /// the queue, and being the oldest it is what every later wake-up returns, which
    /// starves everything queued behind it for its full hour.
    #[test]
    fn an_identity_event_can_be_acknowledged_and_leaves_the_queue() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
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
    fn an_empty_queue_has_no_next() {
        let (anchor_number, browser_id) = signed_in();

        assert_eq!(next_id(anchor_number, browser_id, SECOND_NS), None);
    }

    #[test]
    fn the_next_one_comes_once_what_was_shown_is_removed() {
        let (anchor_number, browser_id) = signed_in();
        enqueue(
            anchor_number,
            browser_id,
            [from_app(7, 60 * SECOND_NS), from_app(8, 60 * SECOND_NS)],
        );
        let shown = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a next one");

        remove(anchor_number, browser_id, &shown, 2 * SECOND_NS);

        assert_eq!(next_id(anchor_number, browser_id, 2 * SECOND_NS), Some(8));
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
            application_number: Some(u64::MAX),
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
        write_notifications_decision(anchor_number, &ORIGIN.to_string(), false, SECOND_NS)
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
        write_notifications_decision(anchor_number, &ORIGIN.to_string(), true, SECOND_NS)
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
        let seven = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a first one");

        remove(anchor_number, browser_id, &seven, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![8]);
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
        let without_account = next(anchor_number, browser_id, 2 * SECOND_NS).expect("a first one");

        remove(anchor_number, browser_id, &without_account, 2 * SECOND_NS);

        assert_eq!(
            anchor(anchor_number).notifications(browser_id),
            [QueuedNotification {
                account_number: Some(3),
                ..from_app(7, 60 * SECOND_NS)
            }]
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
            event: None,
        };

        remove(anchor_number, browser_id, &gone, 2 * SECOND_NS);

        assert_eq!(left_for(anchor_number, browser_id), vec![7]);
    }
}
