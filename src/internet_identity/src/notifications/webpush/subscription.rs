//! What a browser registered for Web Push, held on its entry in the identity's browser
//! registry.

use super::validation::{
    ValidatedRemoveWebPushSubscriptionRequest, ValidatedSetWebPushSubscriptionRequest,
};
use crate::authz_utils::NotificationCaller;
use crate::notifications::app::NotificationApp;
use crate::notifications::dispatch::{self, DeliveryTarget};
use crate::state;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{keeps_endpoint, Anchor, WebPushSubscription};
use crate::storage::Storage;
use ic_stable_structures::Memory;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, RemoveWebPushSubscriptionError, SetWebPushSubscriptionError,
    Timestamp, WebPushSubscriptionStatus,
};

/// Registers `browser_id` for Web Push, and is also how it replaces a pool that is
/// running out. Idempotent: a browser that re-subscribes overwrites what it registered
/// before, endpoint included.
pub fn set_subscription(
    anchor: Anchor,
    browser_id: BrowserId,
    caller: NotificationCaller,
    request: ValidatedSetWebPushSubscriptionRequest,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    // What the Home Screen app registers belongs to its record rather than to this entry.
    // The anchor named in the call is what authorized it, not where the subscription goes.
    if let NotificationCaller::App(app) = caller {
        return set_app_subscription(app, request, now_ns);
    }

    let ValidatedSetWebPushSubscriptionRequest {
        anchor_number: _,
        endpoint,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
        ..
    } = request;

    // The same endpoint is a pool replacement on a registration that is still live, so it
    // keeps its age and the pool has to move forward. Any other endpoint is a new one.
    let created_at_ns = match anchor.webpush_subscription(browser_id) {
        Some(registered) if registered.endpoint == endpoint => {
            if jwt_issued_at_ns <= registered.jwt_issued_at_ns {
                return Err(SetWebPushSubscriptionError::StaleJwtPool);
            }
            registered.created_at_ns
        }
        _ => now_ns,
    };

    write_subscription(
        anchor,
        browser_id,
        Some(WebPushSubscription {
            endpoint,
            created_at_ns,
            vapid_public_key,
            jwt_signatures,
            jwt_issued_at_ns,
        }),
    )
    .map_err(SetWebPushSubscriptionError::InternalCanisterError)
}

/// Stops notifications to one browser, and with them the JWT pool it registered.
/// Idempotent.
///
/// Takes the browser rather than reading it off the caller, since silencing one is done
/// from another.
pub fn remove_subscription(
    ValidatedRemoveWebPushSubscriptionRequest {
        anchor_number,
        browser_id,
        ..
    }: ValidatedRemoveWebPushSubscriptionRequest,
) -> Result<(), RemoveWebPushSubscriptionError> {
    clear_subscription(anchor_number, browser_id)
        .map_err(RemoveWebPushSubscriptionError::InternalCanisterError)
}

fn clear_subscription(anchor_number: AnchorNumber, browser_id: BrowserId) -> Result<(), String> {
    let mut anchor =
        storage_borrow(|storage| storage.read(anchor_number)).map_err(|err| format!("{err}"))?;
    // The reference stays, because it is which app this browser has rather than whether
    // it is routing.
    anchor.set_notification_app_enabled(browser_id, false);
    write_subscription(anchor, browser_id, None)
}

/// `target` says who held the posted endpoint, rather than whoever the entry points at by
/// the time the relay answers. A gone app endpoint means the app is gone, so the record goes.
pub(crate) fn clear_gone_subscription(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    target: DeliveryTarget,
    endpoint: &str,
) -> Result<(), String> {
    match target {
        DeliveryTarget::Browser => {
            let anchor = storage_borrow(|storage| storage.read(anchor_number))
                .map_err(|err| format!("{err}"))?;
            let Some(registered) = anchor.webpush_subscription(browser_id) else {
                return Ok(());
            };
            if registered.endpoint != endpoint {
                return Ok(());
            }
            write_subscription(anchor, browser_id, None)
        }
        DeliveryTarget::App(app_id) => {
            let gone = storage_borrow(|storage| {
                storage.notification_app(app_id).is_some_and(|app| {
                    app.webpush_subscription
                        .is_some_and(|subscription| subscription.endpoint == endpoint)
                })
            });
            if gone && !storage_borrow_mut(|storage| storage.forget_notification_app(app_id)) {
                return Err(format!(
                    "notification app {app_id} is gone but is still referenced"
                ));
            }
            Ok(())
        }
    }
}

/// What delivers this entry's notifications: its own registration, or the record of the
/// Home Screen app it routes through. On a phone the entry holds none of its own.
pub(crate) fn delivering_subscription<M: Memory + Clone>(
    storage: &Storage<M>,
    anchor: &Anchor,
    browser_id: BrowserId,
) -> Option<WebPushSubscription> {
    if let Some(own) = anchor.webpush_subscription(browser_id) {
        return Some(own.clone());
    }
    let browser = anchor
        .browsers()
        .iter()
        .find(|browser| browser.id == browser_id)?;
    dispatch::routed_app(storage, browser)?.webpush_subscription
}

/// What this browser is registered with, and how much of the pool it signed is left
/// to cover.
pub fn subscription_status(
    anchor: &Anchor,
    browser_id: BrowserId,
    caller: NotificationCaller,
) -> Option<WebPushSubscriptionStatus> {
    let registered = match caller {
        NotificationCaller::App(app) => app.webpush_subscription,
        NotificationCaller::Browser => anchor.webpush_subscription(browser_id).cloned(),
    };
    registered.map(|registered| WebPushSubscriptionStatus {
        endpoint: registered.endpoint.clone(),
        pool_len: registered.jwt_signatures.len() as u32,
        issued_at_ns: registered.jwt_issued_at_ns,
    })
}

/// Writes what this browser registered, and clears what was waiting where that address
/// is going away.
///
/// A queue is a list of wake-ups already posted to one endpoint. A registration that is
/// removed, or replaced by a different endpoint, leaves nothing that could show them, so
/// they go with it, and so does the re-wake stamp: it rations second wake-ups for an
/// entry still waiting, and this entry is waiting for nothing. A pool replacement on the
/// same endpoint keeps both.
fn write_subscription(
    mut anchor: Anchor,
    browser_id: BrowserId,
    subscription: Option<WebPushSubscription>,
) -> Result<(), String> {
    if !keeps_endpoint(
        anchor.webpush_subscription(browser_id),
        subscription.as_ref(),
    ) {
        if let Some(queue) = anchor.notifications_mut(browser_id) {
            queue.clear();
        }
        anchor.set_woken_again(browser_id, None);
    }
    anchor.set_webpush_subscription(browser_id, subscription);
    storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))
}

/// What the Home Screen app registered, which is its record's and not any anchor's.
fn set_app_subscription(
    app: NotificationApp,
    request: ValidatedSetWebPushSubscriptionRequest,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    let ValidatedSetWebPushSubscriptionRequest {
        anchor_number: _,
        endpoint,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
        ..
    } = request;

    let created_at_ns = match app.webpush_subscription.as_ref() {
        Some(registered) if registered.endpoint == endpoint => {
            if jwt_issued_at_ns <= registered.jwt_issued_at_ns {
                return Err(SetWebPushSubscriptionError::StaleJwtPool);
            }
            registered.created_at_ns
        }
        _ => now_ns,
    };

    let stored = state::storage_borrow_mut(|storage| {
        storage.set_notification_app_subscription(
            app.id,
            Some(WebPushSubscription {
                endpoint,
                created_at_ns,
                vapid_public_key,
                jwt_signatures,
                jwt_issued_at_ns,
            }),
            now_ns,
        )
    });
    if !stored {
        return Err(SetWebPushSubscriptionError::InternalCanisterError(
            "no notification app record".to_string(),
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use crate::notifications::app::NotificationAppRef;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use internet_identity_interface::internet_identity::types::RemoveWebPushSubscriptionRequest;

    const ENDPOINT: &str = "https://relay.example/a";
    const ROTATED: &str = "https://relay.example/b";

    fn status(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    ) -> Option<WebPushSubscriptionStatus> {
        subscription_status(
            &anchor(anchor_number),
            browser_id,
            NotificationCaller::Browser,
        )
    }

    fn queue_one(anchor_number: AnchorNumber, browser_id: BrowserId) {
        let mut stored = anchor(anchor_number);
        stored
            .notifications_mut(browser_id)
            .expect("a registered browser")
            .push(crate::storage::anchor::QueuedNotification {
                application_number: 1,
                sender: candid::Principal::from_slice(&[7; 10]),
                notification_id: 1,
                expires_at_ns: u64::MAX,
                account_number: None,
            });
        storage_borrow_mut(|storage| storage.write(stored)).expect("writing the anchor");
    }

    fn queued(anchor_number: AnchorNumber, browser_id: BrowserId) -> usize {
        anchor(anchor_number).notifications(browser_id).len()
    }

    fn remove(anchor_number: AnchorNumber, browser_id: BrowserId) {
        let request = RemoveWebPushSubscriptionRequest {
            anchor_number,
            browser_id,
        };
        remove_subscription(request.try_into().expect("the fixture request validates"))
            .expect("removing a subscription");
    }

    #[test]
    fn silencing_a_browser_switches_off_the_app_it_routes_through() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app = storage_borrow_mut(|storage| {
            storage
                .notification_app_for_key(&app_key(9), 0)
                .expect("creating the app record")
        });
        let mut stored = anchor(anchor_number);
        stored.point_at_notification_app(browsers[0], app.id);
        storage_borrow_mut(|storage| storage.write(stored)).expect("pointing at the app");

        remove(anchor_number, browsers[0]);

        assert_eq!(
            anchor(anchor_number).notification_app(browsers[0]),
            Some(NotificationAppRef::Linked {
                app_id: app.id,
                enabled: false,
            })
        );
    }

    #[test]
    fn a_new_endpoint_clears_the_re_wake_stamp() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);
        let mut stored = anchor(anchor_number);
        stored.set_woken_again(browsers[0], Some(1_000));
        storage_borrow_mut(|storage| storage.write(stored)).expect("stamping");

        subscribe(anchor_number, browsers[0], ROTATED, 2_000);

        assert_eq!(
            anchor(anchor_number)
                .browsers()
                .iter()
                .find(|browser| browser.id == browsers[0])
                .and_then(|browser| browser.woken_again_at_ns),
            None
        );
    }

    #[test]
    fn setting_then_removing_round_trips() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);

        subscribe(anchor_number, browsers[0], "https://relay.example/a", 1_000);
        assert_eq!(
            stored_endpoint(anchor_number, browsers[0]).as_deref(),
            Some("https://relay.example/a")
        );

        remove(anchor_number, browsers[0]);
        assert_eq!(stored_endpoint(anchor_number, browsers[0]), None);
    }

    #[test]
    fn a_browser_that_rotates_its_endpoint_keeps_one_subscription() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);

        subscribe(
            anchor_number,
            browsers[0],
            "https://relay.example/first",
            1_000,
        );
        subscribe(
            anchor_number,
            browsers[0],
            "https://relay.example/second",
            2_000,
        );

        assert_eq!(
            stored_endpoint(anchor_number, browsers[0]).as_deref(),
            Some("https://relay.example/second"),
            "a new endpoint for the same browser must overwrite, not accumulate"
        );
    }

    #[test]
    fn two_browsers_of_one_identity_register_on_their_own_entries() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(2);

        subscribe(
            anchor_number,
            browsers[0],
            "https://relay.example/one",
            1_000,
        );
        subscribe(
            anchor_number,
            browsers[1],
            "https://relay.example/other",
            1_000,
        );

        assert_eq!(
            stored_endpoint(anchor_number, browsers[0]).as_deref(),
            Some("https://relay.example/one")
        );
        assert_eq!(
            stored_endpoint(anchor_number, browsers[1]).as_deref(),
            Some("https://relay.example/other")
        );
    }

    #[test]
    fn removing_twice_is_a_harmless_no_op() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);

        remove(anchor_number, browsers[0]);
        remove(anchor_number, browsers[0]);

        assert_eq!(stored_endpoint(anchor_number, browsers[0]), None);
    }

    /// The pool is spent by elapsed time, so one no newer than the pool already stored
    /// would shorten the coverage the browser believes it has.
    #[test]
    fn a_pool_no_newer_than_the_stored_one_is_refused() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 5_000);

        for stale in [4_999, 5_000] {
            assert_eq!(
                try_subscribe(anchor_number, browsers[0], ENDPOINT, stale),
                Err(SetWebPushSubscriptionError::StaleJwtPool),
                "{stale} must not replace the pool minted at 5000"
            );
        }
        assert_eq!(
            stored_subscription(anchor_number, browsers[0]).map(|s| s.jwt_issued_at_ns),
            Some(5_000)
        );
    }

    /// Replacing a pool that is running out goes through this same call, so a top-up
    /// must leave the registration it lands on where it was.
    #[test]
    fn topping_up_the_pool_keeps_the_registration_it_lands_on() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);

        subscribe(anchor_number, browsers[0], ENDPOINT, 9_000);

        let stored = stored_subscription(anchor_number, browsers[0]).expect("a subscription");
        assert_eq!(stored.created_at_ns, 1_000);
        assert_eq!(stored.jwt_issued_at_ns, 9_000);
    }

    /// Another endpoint is another registration, so the pool it arrives with answers to
    /// nothing the one it replaces held.
    #[test]
    fn a_rotated_endpoint_starts_a_fresh_registration() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 5_000);

        try_subscribe(anchor_number, browsers[0], ROTATED, 1_000)
            .expect("a rotated endpoint is not a pool replacement");

        let stored = stored_subscription(anchor_number, browsers[0]).expect("a subscription");
        assert_eq!(stored.endpoint, ROTATED);
        assert_eq!(stored.created_at_ns, 1_000);
    }

    #[test]
    fn removing_a_registration_takes_its_browser_queue_with_it() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(2);
        for browser in &browsers {
            subscribe(anchor_number, *browser, ENDPOINT, 1_000);
            queue_one(anchor_number, *browser);
        }

        remove(anchor_number, browsers[0]);

        assert_eq!(queued(anchor_number, browsers[0]), 0);
        assert_eq!(queued(anchor_number, browsers[1]), 1);
    }

    /// Wake-ups for what was queued went to the old endpoint, so none reach the new one.
    #[test]
    fn a_rotated_endpoint_leaves_the_old_browser_queue_behind() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);
        queue_one(anchor_number, browsers[0]);

        subscribe(anchor_number, browsers[0], ROTATED, 2_000);

        assert_eq!(queued(anchor_number, browsers[0]), 0);
    }

    #[test]
    fn topping_up_the_pool_keeps_the_browser_queue() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);
        queue_one(anchor_number, browsers[0]);

        subscribe(anchor_number, browsers[0], ENDPOINT, 9_000);

        assert_eq!(queued(anchor_number, browsers[0]), 1);
    }

    #[test]
    fn the_status_reports_the_endpoint_and_the_pool_that_was_registered() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);

        assert_eq!(
            status(anchor_number, browsers[0]),
            Some(WebPushSubscriptionStatus {
                endpoint: ENDPOINT.to_string(),
                pool_len: valid_pool().len() as u32,
                issued_at_ns: 1_000,
            })
        );

        remove(anchor_number, browsers[0]);
        assert_eq!(status(anchor_number, browsers[0]), None);
    }

    /// Browsers register on their own entries, so reading one must not answer for
    /// whatever another happens to hold.
    #[test]
    fn a_status_answers_only_for_the_browser_it_was_asked_about() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(2);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);

        assert_eq!(status(anchor_number, browsers[1]), None);
    }

    /// The registration hangs off a browser entry, so one the registry does not list
    /// has nothing to hang it on.
    #[test]
    fn a_browser_the_registry_does_not_list_registers_nothing() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let unlisted = browsers[0] + 1;

        subscribe(anchor_number, unlisted, "https://relay.example/a", 1_000);

        assert_eq!(stored_endpoint(anchor_number, unlisted), None);
    }

    /// A seed no browser of a fixture anchor is registered under, so an app key is its own.
    const APP_KEY_SEED: u8 = 200;

    #[test]
    fn an_app_registers_on_its_record_rather_than_on_a_browser_entry() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(anchor_number, browsers[0], APP_KEY_SEED, 1_000);

        try_subscribe_as(
            app_caller(app_id),
            anchor_number,
            browsers[0],
            ENDPOINT,
            1_000,
        )
        .expect("an app registers its subscription");

        assert_eq!(
            app(app_id).webpush_subscription.map(|s| s.endpoint),
            Some(ENDPOINT.to_string())
        );
        assert_eq!(stored_endpoint(anchor_number, browsers[0]), None);
        assert_eq!(
            subscription_status(&anchor(anchor_number), browsers[0], app_caller(app_id)),
            Some(WebPushSubscriptionStatus {
                endpoint: ENDPOINT.to_string(),
                pool_len: 3,
                issued_at_ns: 1_000,
            })
        );
        // The browser is answered about its own entry, which the app's write left alone.
        assert_eq!(status(anchor_number, browsers[0]), None);
    }

    #[test]
    fn an_app_pool_no_newer_than_the_stored_one_is_refused() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(anchor_number, browsers[0], APP_KEY_SEED, 1_000);
        try_subscribe_as(
            app_caller(app_id),
            anchor_number,
            browsers[0],
            ENDPOINT,
            5_000,
        )
        .expect("an app registers its subscription");

        for stale in [4_999, 5_000] {
            assert_eq!(
                try_subscribe_as(
                    app_caller(app_id),
                    anchor_number,
                    browsers[0],
                    ENDPOINT,
                    stale
                ),
                Err(SetWebPushSubscriptionError::StaleJwtPool),
                "{stale} must not replace the pool minted at 5000"
            );
        }
        assert_eq!(
            app(app_id).webpush_subscription.map(|s| s.jwt_issued_at_ns),
            Some(5_000)
        );
    }

    #[test]
    fn an_app_rotating_its_endpoint_starts_a_fresh_registration() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(anchor_number, browsers[0], APP_KEY_SEED, 1_000);
        try_subscribe_as(
            app_caller(app_id),
            anchor_number,
            browsers[0],
            ENDPOINT,
            5_000,
        )
        .expect("an app registers its subscription");

        try_subscribe_as(
            app_caller(app_id),
            anchor_number,
            browsers[0],
            ROTATED,
            1_000,
        )
        .expect("a rotated endpoint is not a pool replacement");

        let stored = app(app_id).webpush_subscription.expect("a subscription");
        assert_eq!(stored.endpoint, ROTATED);
        assert_eq!(stored.created_at_ns, 1_000);
    }

    #[test]
    fn one_app_registration_answers_every_identity_pointing_at_it() {
        setup();
        let (first, first_browsers) = anchor_with_browsers(1);
        let (second, second_browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(first, first_browsers[0], APP_KEY_SEED, 1_000);
        point_at_app(second, second_browsers[0], app_id);
        subscribe(second, second_browsers[0], ROTATED, 1_000);

        try_subscribe_as(
            app_caller(app_id),
            first,
            first_browsers[0],
            ENDPOINT,
            2_000,
        )
        .expect("an app registers its subscription");

        for (anchor_number, browsers) in [(first, &first_browsers), (second, &second_browsers)] {
            assert_eq!(
                subscription_status(&anchor(anchor_number), browsers[0], app_caller(app_id))
                    .map(|s| s.endpoint),
                Some(ENDPOINT.to_string()),
                "every identity pointing at the app reads its one registration"
            );
        }
        assert_eq!(
            stored_endpoint(second, second_browsers[0]).as_deref(),
            Some(ROTATED)
        );
    }
}
