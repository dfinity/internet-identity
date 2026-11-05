//! What a browser registered for Web Push, held on its entry in the identity's browser
//! registry.

use super::validation::{PushEndpoint, SealedAuth, SubscriberKey};
use super::{payload, seed_pool, transport};
use crate::authz_utils::NotificationKey;
use crate::notifications::delivery_metrics::{self, Reason};
use crate::notifications::dispatch::{self, DeliveryTarget};
use crate::notifications::installed_app::NotificationAppRef;
use crate::notifications::installed_app::{NotificationApp, NotificationAppId};
use crate::state;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, PendingTransport, ReadySubscription, WebPushSubscription};
use crate::storage::Storage;
use ic_stable_structures::Memory;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, PrepareWebPushSubscriptionError, RemoveWebPushSubscriptionError,
    SetWebPushSubscriptionError, Timestamp, WebPushSubscriptionStatus,
};

/// Records the key a fetch just minted, against the registration that will spend it.
///
/// This replaces whatever was registered, so a browser with one in flight is not
/// delivered to until the subscribe lands. That is the whole gap: the browser asks for
/// these keys only when it is about to register an address the canister does not have.
pub fn start_subscription(
    key: NotificationKey,
    anchor_number: AnchorNumber,
    secret_key: [u8; 32],
    sender_seed: [u8; 32],
    now_ns: Timestamp,
) -> Result<(), PrepareWebPushSubscriptionError> {
    let pending = WebPushSubscription::Pending(PendingTransport {
        secret_key,
        sender_seed,
        minted_at_ns: now_ns,
    });
    match key {
        NotificationKey::App(app_id) => storage_borrow_mut(|storage| {
            storage.set_notification_app_subscription(app_id, Some(pending), now_ns)
        })
        .map_err(|err| PrepareWebPushSubscriptionError::InternalCanisterError(format!("{err}"))),
        NotificationKey::Browser(browser_id) => {
            let anchor = state::anchor(anchor_number);
            // This writes the entry's registration and empties its inbox, so it is
            // refused on the same ground `set_subscription` is: a browser the identity
            // signed out of stays signed out, whoever asks.
            if anchor
                .browser(browser_id)
                .is_none_or(|browser| browser.session_count == 0)
            {
                return Err(PrepareWebPushSubscriptionError::InvalidBrowserKey);
            }
            write_subscription(anchor, browser_id, Some(pending))
                .map_err(PrepareWebPushSubscriptionError::InternalCanisterError)
        }
    }
}

/// Registers `browser_id` for Web Push. Idempotent: a browser that re-subscribes
/// overwrites what it registered before, endpoint included.
///
/// `auth` arrives encrypted to the transport key this caller's key fetch minted, so the
/// first thing here is to open it, and the second is to derive what the subscription will
/// be sealed with. Deriving now rather than on the first dispatch pass is what keeps the
/// rotation budget a question of freshness: a pass that cannot afford to refresh a
/// subscription can still send to it, because it already has a key.
pub fn set_subscription(
    key: NotificationKey,
    anchor_number: AnchorNumber,
    endpoint: PushEndpoint,
    subscriber_key: SubscriberKey,
    browser_transport_key: SubscriberKey,
    sealed_auth: SealedAuth,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    // What the Home Screen app registers belongs to its record rather than to this entry.
    // The anchor named in the call is what authorized it, not where the subscription goes.
    let browser_id = match key {
        NotificationKey::App(app_id) => {
            return set_app_subscription(
                app_id,
                endpoint,
                subscriber_key,
                browser_transport_key,
                sealed_auth,
                now_ns,
            )
        }
        NotificationKey::Browser(browser_id) => browser_id,
    };
    let anchor = state::anchor(anchor_number);

    // Signing a browser out clears what it was registered with, and the entry keeps its
    // keys so that it can sign in again. Those keys must not be able to put the
    // registration back: a browser the identity signed out of stays signed out, whoever
    // asks. `routed_app` already refuses to deliver through an app on the same ground.
    if anchor
        .browser(browser_id)
        .is_none_or(|browser| browser.session_count == 0)
    {
        return Err(SetWebPushSubscriptionError::InvalidBrowserKey);
    }

    let pending = anchor
        .webpush_subscription(browser_id)
        .and_then(WebPushSubscription::pending)
        .ok_or(SetWebPushSubscriptionError::TransportKeyExpired)?
        .clone();
    let ready = spend_pending(
        &pending,
        &browser_transport_key,
        &sealed_auth,
        subscriber_key,
        endpoint,
        now_ns,
    )?;

    write_subscription(anchor, browser_id, Some(WebPushSubscription::Ready(ready)))
        .map_err(SetWebPushSubscriptionError::InternalCanisterError)
}

/// Opens `auth`, derives what the subscription will be sealed with, and answers the
/// registration to write.
///
/// The age and the queue follow the address: a browser re-registering where it already
/// was keeps both, which is why the fetch kept what it was replacing.
fn spend_pending(
    pending: &PendingTransport,
    browser_transport_key: &SubscriberKey,
    sealed_auth: &SealedAuth,
    subscriber_key: SubscriberKey,
    endpoint: PushEndpoint,
    now_ns: Timestamp,
) -> Result<ReadySubscription, SetWebPushSubscriptionError> {
    if now_ns.saturating_sub(pending.minted_at_ns) > transport::PENDING_TTL_NS {
        return Err(SetWebPushSubscriptionError::TransportKeyExpired);
    }
    let auth = transport::open(&pending.secret_key, browser_transport_key, sealed_auth)
        .map_err(|_| SetWebPushSubscriptionError::TransportKeyExpired)?;
    let sender = seed_pool::secret_key_from(&pending.sender_seed);
    let (payload_ikm, payload_keyid) = payload::derive_ikm(&sender, &subscriber_key, &auth)
        .map_err(SetWebPushSubscriptionError::InternalCanisterError)?;

    Ok(ReadySubscription {
        endpoint,
        created_at_ns: now_ns,
        payload_ikm,
        payload_keyid,
        subscriber_key,
        auth,
        last_rotated_ns: now_ns,
    })
}

/// Stops notifications to one browser. Idempotent.
///
/// Takes the browser rather than reading it off the caller, since silencing one is done
/// from another.
pub fn remove_subscription(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Result<(), RemoveWebPushSubscriptionError> {
    clear_subscription(anchor_number, browser_id)
        .map_err(RemoveWebPushSubscriptionError::InternalCanisterError)
}

fn clear_subscription(anchor_number: AnchorNumber, browser_id: BrowserId) -> Result<(), String> {
    let mut anchor =
        storage_borrow(|storage| storage.read(anchor_number)).map_err(|err| format!("{err}"))?;
    // The reference stays, because it is which app this browser has rather than whether
    // it is routing.
    anchor.disable_notification_app(browser_id);
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
            let Some(registered) = anchor
                .webpush_subscription(browser_id)
                .and_then(WebPushSubscription::ready)
            else {
                return Ok(());
            };
            if registered.endpoint.as_str() != endpoint {
                return Ok(());
            }
            write_subscription(anchor, browser_id, None)
        }
        DeliveryTarget::App(app_id) => {
            let gone = storage_borrow(|storage| {
                storage.notification_app(app_id).is_some_and(|app| {
                    app.webpush_subscription
                        .and_then(WebPushSubscription::into_ready)
                        .is_some_and(|subscription| subscription.endpoint.as_str() == endpoint)
                })
            });
            // Only where the record is the one the relay reported gone: dropping it
            // unconditionally would forget a record that is still delivering.
            if gone {
                storage_borrow_mut(|storage| storage.forget_notification_app(app_id))
                    .map_err(|err| format!("{err}"))?;
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
) -> Option<ReadySubscription> {
    let browser = anchor
        .browsers()
        .iter()
        .find(|browser| browser.id == browser_id)?;
    // A browser has one route, and an install nobody has opened yet is that route being
    // handed over: completing it drops whatever address this browser registered for
    // itself and clears what was waiting there. Nothing is delivered in between, and a
    // dispatch pass refuses the same browser on the same ground.
    if matches!(
        browser.notification_app,
        Some(NotificationAppRef::Pending { .. })
    ) {
        return None;
    }
    // Only the ready half: a registration still waiting on the subscribe that spends its
    // transport key has no endpoint to post to and nothing to seal with.
    if let Some(own) = anchor.webpush_subscription(browser_id) {
        return own.ready().cloned();
    }
    dispatch::routed_app(storage, browser)?
        .webpush_subscription
        .and_then(WebPushSubscription::into_ready)
}

/// The address this browser is registered at.
pub fn subscription_status(
    anchor_number: AnchorNumber,
    key: NotificationKey,
) -> Option<WebPushSubscriptionStatus> {
    let subscription = match key {
        NotificationKey::App(app_id) => {
            storage_borrow(|storage| storage.notification_app(app_id))?.webpush_subscription
        }
        NotificationKey::Browser(browser_id) => state::anchor(anchor_number)
            .webpush_subscription(browser_id)
            .cloned(),
    };
    // Only what can be delivered to. A registration waiting on its subscribe reports
    // nothing, which is what a caller deciding whether to register needs to hear.
    subscription
        .and_then(WebPushSubscription::into_ready)
        .map(|registered| WebPushSubscriptionStatus {
            endpoint: registered.endpoint.as_str().to_string(),
        })
}

/// Writes what this browser registered, and empties the inbox the old address leaves.
///
/// An inbox holds wake-ups already posted to one address. Every write here takes that
/// address away: a prepare replaces the registration with a pending one, a subscribe
/// spends that pending one, and signing out drops it. None can still show what was
/// announced before, and entries nothing will show would fill the inbox against what
/// arrives next.
fn write_subscription(
    mut anchor: Anchor,
    browser_id: BrowserId,
    subscription: Option<WebPushSubscription>,
) -> Result<(), String> {
    let cleared = anchor
        .notifications_for_browser_mut(browser_id)
        .is_some_and(|inbox| !std::mem::take(inbox).is_empty());
    let anchor_number = anchor.anchor_number();
    let left = anchor.set_webpush_subscription(browser_id, subscription);
    storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))?;
    if cleared {
        delivery_metrics::inbox_cleared(anchor_number, browser_id, Reason::SubscriptionReplaced);
    }
    // Read after the write, so the reference this browser just dropped is already gone:
    // a record the last browser left behind is one nothing can reach again.
    if let Some(app_id) = left {
        storage_borrow_mut(|storage| storage.delete_notification_app_if_unreferenced(app_id));
    }
    Ok(())
}

/// What the Home Screen app registered, which is its record's and not any anchor's.
fn set_app_subscription(
    app_id: NotificationAppId,
    endpoint: PushEndpoint,
    subscriber_key: SubscriberKey,
    browser_transport_key: SubscriberKey,
    sealed_auth: SealedAuth,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    // Read now rather than taking what authorizing the call happened to see: the key the
    // fetch left is on this record, and so is whatever it is replacing.
    let NotificationApp {
        webpush_subscription: registered,
        ..
    } = storage_borrow(|storage| storage.notification_app(app_id)).ok_or_else(|| {
        SetWebPushSubscriptionError::InternalCanisterError(format!("no notification app {app_id}"))
    })?;

    let pending = registered
        .as_ref()
        .and_then(WebPushSubscription::pending)
        .ok_or(SetWebPushSubscriptionError::TransportKeyExpired)?;
    let ready = spend_pending(
        pending,
        &browser_transport_key,
        &sealed_auth,
        subscriber_key,
        endpoint,
        now_ns,
    )?;

    state::storage_borrow_mut(|storage| {
        storage.set_notification_app_subscription(
            app_id,
            Some(WebPushSubscription::Ready(ready)),
            now_ns,
        )
    })
    .map_err(|err| SetWebPushSubscriptionError::InternalCanisterError(format!("{err}")))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use crate::notifications::installed_app::NotificationAppRef;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::AnchorNumber;

    const ENDPOINT: &str = "https://relay.example/a";
    const ROTATED: &str = "https://relay.example/b";

    /// A key fetch on its own leaves a registration that cannot be delivered to, and the
    /// subscribe that follows is what makes it one.
    #[test]
    fn a_key_fetch_alone_registers_nothing_deliverable() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        start_subscription(
            NotificationKey::Browser(browsers[0]),
            anchor_number,
            [1u8; 32],
            [2u8; 32],
            0,
        )
        .expect("the fetch records its key");

        assert!(
            stored_registration(anchor_number, browsers[0])
                .expect("a registration")
                .pending()
                .is_some(),
            "waiting on the subscribe that spends it"
        );
        assert!(status(anchor_number, browsers[0]).is_none());
        assert!(stored_subscription(anchor_number, browsers[0]).is_none());
    }

    /// A different address is a different registration, so what was announced to the old
    /// one goes with it.
    #[test]
    fn moving_address_drops_what_was_queued_for_the_old_one() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        try_subscribe(anchor_number, browsers[0], ENDPOINT, 500).expect("the first subscribe");
        queue_one(anchor_number, browsers[0]);

        try_subscribe(anchor_number, browsers[0], ROTATED, 9_000).expect("re-subscribing");

        let stored = stored_subscription(anchor_number, browsers[0]).expect("a subscription");
        assert_eq!(stored.created_at_ns, 9_000);
        assert_eq!(queued(anchor_number, browsers[0]), 0);
    }

    /// The key a fetch left is good for minutes, not forever.
    #[test]
    fn a_subscribe_past_the_window_is_refused() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        const SCALAR: [u8; 32] = [11u8; 32];
        start_subscription(
            NotificationKey::Browser(browsers[0]),
            anchor_number,
            SCALAR,
            [12u8; 32],
            0,
        )
        .expect("the fetch records its key");
        let (transport_key, sealed_auth) = super::super::transport::seal_auth(
            &super::super::transport::public_key_of(&SCALAR),
            &[13u8; 32],
            &valid_auth(),
        );

        let refused = set_subscription(
            NotificationKey::Browser(browsers[0]),
            anchor_number,
            validated(anchor_number, ENDPOINT),
            SubscriberKey::try_from(valid_sec1_point()).expect("a point"),
            transport_key,
            sealed_auth,
            transport::PENDING_TTL_NS + 1,
        );

        assert!(matches!(
            refused,
            Err(SetWebPushSubscriptionError::TransportKeyExpired)
        ));
    }

    /// Nothing to spend, so nothing to register.
    #[test]
    fn a_subscribe_without_a_key_fetch_is_refused() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let (transport_key, sealed_auth) = {
            let scalar = [11u8; 32];
            super::super::transport::seal_auth(
                &super::super::transport::public_key_of(&scalar),
                &[13u8; 32],
                &valid_auth(),
            )
        };
        let refused = set_subscription(
            NotificationKey::Browser(browsers[0]),
            anchor_number,
            validated(anchor_number, ENDPOINT),
            SubscriberKey::try_from(valid_sec1_point()).expect("a point"),
            transport_key,
            sealed_auth,
            0,
        );
        assert!(matches!(
            refused,
            Err(SetWebPushSubscriptionError::TransportKeyExpired)
        ));
    }

    fn status(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    ) -> Option<WebPushSubscriptionStatus> {
        subscription_status(anchor_number, NotificationKey::Browser(browser_id))
    }

    fn queue_one(anchor_number: AnchorNumber, browser_id: BrowserId) {
        let mut stored = anchor(anchor_number);
        stored
            .notifications_for_browser_mut(browser_id)
            .expect("a registered browser")
            .push(crate::storage::anchor::InboxNotification {
                inbox_notification_id: 0,
                application_number: Some(1),
                sender: candid::Principal::from_slice(&[7; 10]),
                notification_id: 1,
                expires_at_ns: u64::MAX,
                account_number: None,
                event: None,
            });
        storage_borrow_mut(|storage| storage.write(stored)).expect("writing the anchor");
    }

    fn queued(anchor_number: AnchorNumber, browser_id: BrowserId) -> usize {
        anchor(anchor_number)
            .notifications_for_browser(browser_id)
            .len()
    }

    fn remove(anchor_number: AnchorNumber, browser_id: BrowserId) {
        remove_subscription(anchor_number, browser_id).expect("removing a subscription");
    }

    #[test]
    fn silencing_a_browser_switches_off_the_app_it_routes_through() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app = storage_borrow_mut(|storage| {
            storage
                .ensure_notification_app(Principal::self_authenticating(app_key(9)), None, 0)
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

    /// Another address is another registration, so it answers to nothing the one it
    /// replaces held, its age included.
    #[test]
    fn a_rotated_endpoint_starts_a_fresh_registration() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 5_000);

        try_subscribe(anchor_number, browsers[0], ROTATED, 1_000)
            .expect("a rotated endpoint is a registration of its own");

        let stored = stored_subscription(anchor_number, browsers[0]).expect("a subscription");
        assert_eq!(stored.endpoint.as_str(), ROTATED);
        assert_eq!(stored.created_at_ns, 1_000);
    }

    #[test]
    fn removing_a_registration_takes_its_browser_inbox_with_it() {
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
    fn a_rotated_endpoint_leaves_the_old_browser_inbox_behind() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);
        queue_one(anchor_number, browsers[0]);

        subscribe(anchor_number, browsers[0], ROTATED, 2_000);

        assert_eq!(queued(anchor_number, browsers[0]), 0);
    }

    #[test]
    fn the_status_reports_the_endpoint_that_was_registered() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);

        assert_eq!(
            status(anchor_number, browsers[0]),
            Some(WebPushSubscriptionStatus {
                endpoint: ENDPOINT.to_string(),
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
    /// has nothing to hang it on and is told so.
    #[test]
    fn a_browser_the_registry_does_not_list_registers_nothing() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let unlisted = browsers[0] + 1;

        assert_eq!(
            try_subscribe(anchor_number, unlisted, "https://relay.example/a", 1_000),
            Err(SetWebPushSubscriptionError::InvalidBrowserKey)
        );
        assert_eq!(stored_endpoint(anchor_number, unlisted), None);
    }

    /// Signing a browser out clears what it was registered with, and its keys stay so it
    /// can sign in again. They must not be able to put the registration back.
    #[test]
    fn a_browser_signed_out_of_everything_registers_nothing() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        subscribe(anchor_number, browsers[0], ENDPOINT, 1_000);
        sign_browser_out(anchor_number, browsers[0]);

        // Both halves refuse. The fetch matters as much as the subscribe: it writes the
        // entry's registration and empties its inbox before any address is involved.
        assert_eq!(
            start_subscription(
                NotificationKey::Browser(browsers[0]),
                anchor_number,
                [11u8; 32],
                [12u8; 32],
                2_000,
            ),
            Err(PrepareWebPushSubscriptionError::InvalidBrowserKey)
        );
        assert_eq!(
            try_subscribe(anchor_number, browsers[0], ENDPOINT, 2_000),
            Err(SetWebPushSubscriptionError::InvalidBrowserKey)
        );
        assert_eq!(stored_endpoint(anchor_number, browsers[0]), None);
    }

    /// A seed no browser of a fixture anchor is registered under, so an app key is its own.
    const APP_KEY_SEED: u8 = 200;

    #[test]
    fn an_app_registers_on_its_record_rather_than_on_a_browser_entry() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(anchor_number, browsers[0], APP_KEY_SEED, 1_000);

        try_subscribe_as(app_authorization(app_id), anchor_number, ENDPOINT, 1_000)
            .expect("an app registers its subscription");

        assert_eq!(
            app(app_id)
                .webpush_subscription
                .and_then(WebPushSubscription::into_ready)
                .map(|s| s.endpoint.into_string()),
            Some(ENDPOINT.to_string())
        );
        assert_eq!(stored_endpoint(anchor_number, browsers[0]), None);
        assert_eq!(
            subscription_status(anchor_number, app_authorization(app_id)),
            Some(WebPushSubscriptionStatus {
                endpoint: ENDPOINT.to_string(),
            })
        );
        // The browser is answered about its own entry, which the app's write left alone.
        assert_eq!(status(anchor_number, browsers[0]), None);
    }

    #[test]
    fn an_app_rotating_its_endpoint_starts_a_fresh_registration() {
        setup();
        let (anchor_number, browsers) = anchor_with_browsers(1);
        let app_id = app_pointed_at_by(anchor_number, browsers[0], APP_KEY_SEED, 1_000);
        try_subscribe_as(app_authorization(app_id), anchor_number, ENDPOINT, 5_000)
            .expect("an app registers its subscription");

        try_subscribe_as(app_authorization(app_id), anchor_number, ROTATED, 1_000)
            .expect("a rotated endpoint is a registration of its own");

        let stored = app(app_id)
            .webpush_subscription
            .and_then(WebPushSubscription::into_ready)
            .expect("a subscription");
        assert_eq!(stored.endpoint.as_str(), ROTATED);
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

        try_subscribe_as(app_authorization(app_id), first, ENDPOINT, 2_000)
            .expect("an app registers its subscription");

        for anchor_number in [first, second] {
            assert_eq!(
                subscription_status(anchor_number, app_authorization(app_id)).map(|s| s.endpoint),
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
