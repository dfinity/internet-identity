//! Shared Web Push registration and dispatch test fixtures.

use super::validation::{
    AuthSecret, PayloadIkm, PayloadKeyId, PushEndpoint, SealedAuth, SubscriberKey,
};
use crate::authz_utils::NotificationKey;
use crate::notifications::installed_app::{NotificationApp, NotificationAppId};
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, ReadySubscription, WebPushSubscription};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, BrowserId, FormFactor, OperatingSystem,
    PublicKey, SetWebPushSubscriptionError, SetWebPushSubscriptionRequest, Timestamp,
};
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use std::collections::BTreeMap;

thread_local! {
    /// Which record each app principal was given. A real app names its own id on a later
    /// call, and without the principal index that is the only way back to its record, so
    /// a test has to remember what a key was handed the first time it was used.
    static APP_IDS: RefCell<BTreeMap<Principal, NotificationAppId>> =
        const { RefCell::new(BTreeMap::new()) };
}

/// The record the app holding `public_key` has, created the first time it is seen.
pub(crate) fn app_for_key(public_key: PublicKey, now_ns: Timestamp) -> NotificationApp {
    let principal = Principal::self_authenticating(public_key);
    let known = APP_IDS.with(|ids| ids.borrow().get(&principal).copied());
    let app = storage_borrow_mut(|storage| {
        storage
            .ensure_notification_app(principal, known, now_ns)
            .expect("creating the app record")
    });
    APP_IDS.with(|ids| {
        ids.borrow_mut().insert(principal, app.id);
    });
    app
}

/// One anchor with `count` browsers on it, as sign-ins leave them.
pub(crate) fn anchor_with_browsers(count: u8) -> (AnchorNumber, Vec<BrowserId>) {
    storage_borrow_mut(|storage| {
        let mut anchor = storage
            .allocate_anchor(0)
            .expect("the test anchor range has room");
        let anchor_number = anchor.anchor_number();
        let ids = (0..count)
            .map(|seed| {
                let (id, _) = anchor
                    .resolve_browser(key(seed * 2), key(seed * 2 + 1), chrome_on_a_mac(), 0)
                    .expect("an unseen key registers a browser");
                id
            })
            .collect();
        // Signed in, which is how a browser reaches any of this: a key alone registers
        // nothing, and signing out takes the registration with it.
        for browser in &mut anchor.browsers {
            browser.session_count = 1;
        }
        storage.write(anchor).expect("writing a fresh anchor");
        (anchor_number, ids)
    })
}

fn key(seed: u8) -> PublicKey {
    ByteBuf::from(vec![seed.wrapping_add(1); 91])
}

/// The key a Home Screen app generated in its own partition and presents on every call.
/// A registration whose every field passes the shape its newtype asserts, which is the
/// only kind storage can hand back.
pub(crate) fn subscription_at(endpoint: &str, created_at_ns: Timestamp) -> WebPushSubscription {
    WebPushSubscription::Ready(ReadySubscription {
        endpoint: PushEndpoint::try_from(endpoint).expect("a relay endpoint"),
        created_at_ns,
        payload_ikm: PayloadIkm::try_from(valid_payload_ikm()).expect("32 bytes"),
        payload_keyid: PayloadKeyId::try_from(valid_sec1_point()).expect("a SEC1 P-256 point"),
        subscriber_key: SubscriberKey::try_from(valid_sec1_point()).expect("a SEC1 P-256 point"),
        auth: AuthSecret::try_from(valid_auth()).expect("16 bytes"),
        last_rotated_ns: created_at_ns,
    })
}

pub(crate) fn app_key(seed: u8) -> PublicKey {
    ByteBuf::from(vec![seed; 91])
}

/// Where a Home Screen app's own registration sends its wake-ups.
pub(crate) const APP_RELAY: &str = "https://web.push.apple.com/a";
pub(crate) const OTHER_APP_RELAY: &str = "https://web.push.apple.com/b";

fn chrome_on_a_mac() -> BrowserDescription {
    BrowserDescription {
        brand: BrowserBrand::Chrome,
        os: OperatingSystem::Macos,
        form_factor: FormFactor::Desktop,
        model: None,
    }
}

/// An empty store on a deployment that notifies, which is the only state a Web Push
/// request validates in.
pub(crate) fn setup() {
    crate::notifications::test_setup();
    APP_IDS.with(|ids| ids.borrow_mut().clear());
    crate::state::persistent_state_mut(|s| s.notifications_enabled = Some(true));
}

pub(crate) fn anchor(anchor_number: AnchorNumber) -> Anchor {
    storage_borrow(|storage| storage.read(anchor_number)).expect("reading the test anchor")
}

/// A real, fixed SEC1 point: a length-only fixture cannot pass the curve check.
pub(crate) fn valid_sec1_point() -> Vec<u8> {
    use p256::elliptic_curve::sec1::ToEncodedPoint;
    let secret = p256::SecretKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid");
    secret
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec()
}

/// Derived on subscribe; a fixture only has to be the right length.
pub(crate) fn valid_payload_ikm() -> Vec<u8> {
    vec![9u8; 32]
}

/// The subscription's own `auth` secret, which RFC 8291 puts at 16 bytes.
pub(crate) fn valid_auth() -> Vec<u8> {
    vec![4u8; 16]
}

/// What a key fetch would have left behind, so a test subscribe has a transport key to
/// have been encrypted to.
fn pending_for(
    anchor_number: AnchorNumber,
    key: NotificationKey,
    now_ns: Timestamp,
) -> (SubscriberKey, SealedAuth) {
    const TRANSPORT_SCALAR: [u8; 32] = [11u8; 32];
    const SENDER_SEED: [u8; 32] = [12u8; 32];
    // A refusal is not an abort: the key fetch and the subscribe refuse the same callers,
    // so a test covering that refusal still needs what the browser would have sent. One
    // that did not expect it reads the missing registration as `TransportKeyExpired`.
    let _ = super::start_subscription(key, anchor_number, TRANSPORT_SCALAR, SENDER_SEED, now_ns);
    super::transport::seal_auth(
        &super::transport::public_key_of(&TRANSPORT_SCALAR),
        &[13u8; 32],
        &valid_auth(),
    )
}

/// Well-formed defaults, so a field added to the request doesn't touch every test.
pub(super) fn request(
    anchor_number: AnchorNumber,
    endpoint: &str,
) -> SetWebPushSubscriptionRequest {
    SetWebPushSubscriptionRequest {
        anchor_number,
        endpoint: endpoint.to_string(),
        p256dh: ByteBuf::from(valid_sec1_point()),
        transport_key: ByteBuf::from(valid_sec1_point()),
        sealed_auth: ByteBuf::from(vec![0u8; 32]),
    }
}

/// Only validation can build these, so the fixture goes through it.
pub(super) fn validated(anchor_number: AnchorNumber, endpoint: &str) -> PushEndpoint {
    let SetWebPushSubscriptionRequest { endpoint, .. } = request(anchor_number, endpoint);
    PushEndpoint::try_from(endpoint.as_str()).expect("the fixture endpoint validates")
}

/// `set_subscription` for a test that has an anchor number rather than the anchor.
pub(super) fn try_subscribe(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    try_subscribe_as(
        NotificationKey::Browser(browser_id),
        anchor_number,
        endpoint,
        now_ns,
    )
}

/// As [`try_subscribe`], for a call that may authorize as the Home Screen app.
pub(super) fn try_subscribe_as(
    key: NotificationKey,
    anchor_number: AnchorNumber,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    let (transport_key, sealed_auth) = pending_for(anchor_number, key, now_ns);
    super::set_subscription(
        key,
        anchor_number,
        validated(anchor_number, endpoint),
        SubscriberKey::try_from(valid_sec1_point()).expect("a SEC1 P-256 point"),
        transport_key,
        sealed_auth,
        now_ns,
    )
}

/// A Home Screen app record, with `anchor`'s browser `browser_id` pointing at it.
pub(super) fn app_pointed_at_by(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    key_seed: u8,
    now_ns: Timestamp,
) -> NotificationAppId {
    let app_id = app_for_key(key(key_seed), now_ns).id;
    point_at_app(anchor_number, browser_id, app_id);
    app_id
}

/// A second identity on the device pointing at the app the first installed.
pub(super) fn point_at_app(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    app_id: NotificationAppId,
) {
    let mut anchor = anchor(anchor_number);
    anchor.point_at_notification_app(browser_id, app_id);
    storage_borrow_mut(|storage| storage.write(anchor)).expect("pointing at the app");
}

/// The record as it stands, which is where an app's own subscription lives.
pub(crate) fn app(app_id: NotificationAppId) -> NotificationApp {
    storage_borrow(|storage| storage.notification_app(app_id)).expect("reading the app record")
}

/// What an app's own call authorizes as.
pub(super) fn app_authorization(app_id: NotificationAppId) -> NotificationKey {
    NotificationKey::App(app_id)
}

pub(crate) fn subscribe(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    endpoint: &str,
    now_ns: Timestamp,
) {
    try_subscribe(anchor_number, browser_id, endpoint, now_ns).expect("writing a subscription");
}

/// What this browser can be delivered to, which is what every test here asserts on. A
/// registration waiting on its subscribe reports nothing, as it does to any caller.
pub(crate) fn stored_subscription(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Option<ReadySubscription> {
    anchor(anchor_number)
        .webpush_subscription(browser_id)
        .and_then(WebPushSubscription::ready)
        .cloned()
}

/// The registration in whichever state it is in, for a test about the states themselves.
pub(crate) fn stored_registration(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Option<WebPushSubscription> {
    anchor(anchor_number)
        .webpush_subscription(browser_id)
        .cloned()
}

/// Signs `browser_id` out of everything, as revoking its sessions from another browser
/// does: the registration goes, the entry and its keys stay.
pub(super) fn sign_browser_out(anchor_number: AnchorNumber, browser_id: BrowserId) {
    storage_borrow_mut(|storage| {
        let mut anchor = storage.read(anchor_number).expect("the test anchor");
        anchor.set_webpush_subscription(browser_id, None);
        for browser in &mut anchor.browsers {
            if browser.id == browser_id {
                browser.session_count = 0;
            }
        }
        storage.write(anchor).expect("signing the browser out");
    });
}

pub(super) fn stored_endpoint(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Option<String> {
    stored_subscription(anchor_number, browser_id)
        .map(|subscription| subscription.endpoint.into_string())
}
