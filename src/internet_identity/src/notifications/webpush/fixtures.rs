//! Shared Web Push registration and dispatch test fixtures.

use super::validation::{ValidatedSetWebPushSubscriptionRequest, JWT_SIG_LEN};
use crate::authz_utils::NotificationCaller;
use crate::notifications::app::{NotificationApp, NotificationAppId};
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, WebPushSubscription};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, BrowserId, FormFactor, OperatingSystem,
    PublicKey, SetWebPushSubscriptionError, SetWebPushSubscriptionRequest, Timestamp,
};
use serde_bytes::ByteBuf;

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
        storage.write(anchor).expect("writing a fresh anchor");
        (anchor_number, ids)
    })
}

fn key(seed: u8) -> PublicKey {
    ByteBuf::from(vec![seed.wrapping_add(1); 91])
}

/// The key a Home Screen app generated in its own partition and presents on every call.
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
    crate::state::persistent_state_mut(|s| s.notifications_enabled = Some(true));
}

pub(crate) fn anchor(anchor_number: AnchorNumber) -> Anchor {
    storage_borrow(|storage| storage.read(anchor_number)).expect("reading the test anchor")
}

/// A real, fixed SEC1 point: a length-only fixture cannot pass the curve check.
pub(crate) fn valid_vapid_key() -> Vec<u8> {
    use p256::elliptic_curve::sec1::ToEncodedPoint;
    let secret = p256::SecretKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid");
    secret
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec()
}

pub(super) fn valid_pool() -> Vec<Vec<u8>> {
    vec![vec![3u8; JWT_SIG_LEN]; 3]
}

/// Well-formed defaults, so a field added to the request doesn't touch every test.
pub(super) fn request(
    anchor_number: AnchorNumber,
    endpoint: &str,
    jwt_issued_at_ns: Timestamp,
) -> SetWebPushSubscriptionRequest {
    SetWebPushSubscriptionRequest {
        anchor_number,
        endpoint: endpoint.to_string(),
        vapid_public_key: ByteBuf::from(valid_vapid_key()),
        jwt_signatures: valid_pool().into_iter().map(ByteBuf::from).collect(),
        jwt_issued_at_ns,
    }
}

/// Only validation can build one, so the fixture goes through it.
pub(super) fn validated(
    anchor_number: AnchorNumber,
    endpoint: &str,
    jwt_issued_at_ns: Timestamp,
) -> ValidatedSetWebPushSubscriptionRequest {
    request(anchor_number, endpoint, jwt_issued_at_ns)
        .try_into()
        .expect("the fixture request validates")
}

/// `set_subscription` for a test that has an anchor number rather than the anchor.
pub(super) fn try_subscribe(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    try_subscribe_as(
        NotificationCaller::Browser,
        anchor_number,
        browser_id,
        endpoint,
        now_ns,
    )
}

/// As [`try_subscribe`], for a caller that may be the Home Screen app.
pub(super) fn try_subscribe_as(
    caller: NotificationCaller,
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), SetWebPushSubscriptionError> {
    super::set_subscription(
        anchor(anchor_number),
        browser_id,
        caller,
        validated(anchor_number, endpoint, now_ns),
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
    let app_id = storage_borrow_mut(|storage| {
        storage
            .notification_app_for_key(&key(key_seed), now_ns)
            .expect("creating the app record")
            .id
    });
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
pub(super) fn app(app_id: NotificationAppId) -> NotificationApp {
    storage_borrow(|storage| storage.notification_app(app_id)).expect("reading the app record")
}

/// The caller an app's own call arrives as.
pub(super) fn app_caller(app_id: NotificationAppId) -> NotificationCaller {
    NotificationCaller::App(app(app_id))
}

pub(crate) fn subscribe(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    endpoint: &str,
    now_ns: Timestamp,
) {
    try_subscribe(anchor_number, browser_id, endpoint, now_ns).expect("writing a subscription");
}

pub(crate) fn stored_subscription(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Option<WebPushSubscription> {
    anchor(anchor_number)
        .webpush_subscription(browser_id)
        .cloned()
}

pub(super) fn stored_endpoint(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Option<String> {
    stored_subscription(anchor_number, browser_id).map(|subscription| subscription.endpoint)
}
