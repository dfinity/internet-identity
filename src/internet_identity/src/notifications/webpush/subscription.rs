//! One subscription row per browser, keyed `(anchor, browser_id)` against the
//! registry the anchor already holds.

use super::{validate_jwt_pool, validate_param_len, validate_vapid_public_key, MAX_ENDPOINT_LEN};
use crate::browser_key::verify_webpush_subscription;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
pub use internet_identity_interface::internet_identity::types::SubscribeDeviceRequest;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, PublicKey, SubscribeDeviceError, Timestamp,
};
use minicbor::bytes::ByteVec;
use serde_bytes::ByteBuf;

/// The same request once the browser is known and the candid wrappers are off.
pub(super) struct Subscription {
    pub anchor_number: AnchorNumber,
    pub browser_id: BrowserId,
    pub endpoint: String,
    pub vapid_public_key: Vec<u8>,
    pub jwt_signatures: Vec<Vec<u8>>,
    pub jwt_issued_at_ns: Timestamp,
}

impl Subscription {
    fn new(request: SubscribeDeviceRequest, browser_id: BrowserId) -> Self {
        Self {
            anchor_number: request.anchor_number,
            browser_id,
            endpoint: request.endpoint,
            vapid_public_key: request.vapid_public_key.into_vec(),
            jwt_signatures: request
                .jwt_signatures
                .into_iter()
                .map(ByteBuf::into_vec)
                .collect(),
            jwt_issued_at_ns: request.jwt_issued_at_ns,
        }
    }
}

/// The browser this key names, if the identity is signed in from it.
pub(super) fn browser_of_key(
    anchor_number: AnchorNumber,
    browser_key: &PublicKey,
) -> Result<BrowserId, SubscribeDeviceError> {
    storage_borrow(|storage| {
        storage
            .read(anchor_number)
            .ok()
            .and_then(|anchor| anchor.browser_by_key(browser_key))
    })
    .ok_or(SubscribeDeviceError::InvalidBrowserKey)
}

/// Whether the caller holds the key it named, over the subscription it is writing.
///
/// Every device of an identity passes the same authorization, so naming a browser is not
/// on its own evidence of being it.
pub(super) fn check_browser_proof(
    browser_key: &PublicKey,
    browser_key_signature: &[u8],
    endpoint: &str,
    jwt_issued_at_ns: Timestamp,
) -> Result<(), SubscribeDeviceError> {
    verify_webpush_subscription(
        browser_key,
        browser_key_signature,
        endpoint,
        jwt_issued_at_ns,
    )
    .then_some(())
    .ok_or(SubscribeDeviceError::InvalidBrowserKey)
}

pub(super) fn add_subscription(
    subscription: Subscription,
    now_ns: Timestamp,
) -> Result<(), Vec<String>> {
    let Subscription {
        anchor_number,
        browser_id,
        endpoint,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
    } = subscription;

    // Report every invalid field at once rather than failing on the first.
    let errors: Vec<String> = [
        validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint"),
        validate_vapid_public_key(&vapid_public_key),
        validate_jwt_pool(&jwt_signatures),
    ]
    .into_iter()
    .filter_map(Result::err)
    .collect();
    if !errors.is_empty() {
        return Err(errors);
    }

    let row = StorableWebPushSubscription {
        anchor: anchor_number,
        endpoint,
        created_at_ns: now_ns,
        vapid_public_key,
        jwt_pool: Some(StorableWebPushJwtPool {
            signatures: jwt_signatures.into_iter().map(ByteVec::from).collect(),
            issued_at_ns: jwt_issued_at_ns,
        }),
    };

    // No cap and no eviction: the key is a browser the registry already holds, so
    // `MAX_BROWSERS` bounds this map and the registry's own evictions empty it.
    storage_borrow_mut(|storage| {
        storage
            .webpush_subscriptions_memory
            .insert((anchor_number, browser_id), row);
    });
    Ok(())
}

/// Drops the subscription, and with it the JWT pool on the same row. Idempotent.
pub(super) fn remove_subscription(anchor_number: AnchorNumber, browser_id: BrowserId) {
    storage_borrow_mut(|storage| {
        storage
            .webpush_subscriptions_memory
            .remove(&(anchor_number, browser_id));
    });
}

/// Idempotent: a browser that re-subscribes overwrites its own row, endpoint included.
pub fn subscribe_device(
    request: SubscribeDeviceRequest,
    now_ns: Timestamp,
) -> Result<(), SubscribeDeviceError> {
    let browser_id = browser_of_key(request.anchor_number, &request.browser_key)?;
    check_browser_proof(
        &request.browser_key,
        &request.browser_key_signature,
        &request.endpoint,
        request.jwt_issued_at_ns,
    )?;
    add_subscription(Subscription::new(request, browser_id), now_ns)
        .map_err(|problems| SubscribeDeviceError::InternalCanisterError(problems.join("; ")))
}

/// Stops notifications to one browser. Idempotent.
///
/// Takes the browser rather than proving it, since turning one off is done from another.
pub fn unsubscribe_device(anchor_number: AnchorNumber, browser_id: BrowserId) {
    remove_subscription(anchor_number, browser_id);
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::super::{JWT_SIG_LEN, MAX_JWT_POOL_LEN};
    use super::*;
    use crate::notifications::test_setup as setup;

    #[test]
    fn subscribe_then_unsubscribe_round_trips() {
        setup();
        let browser = registered_browser(1);

        subscribe(&browser, "https://relay.example/a", 1_000).unwrap();
        assert_eq!(subscription_count(browser.anchor), 1);

        remove_subscription(browser.anchor, browser.id);
        assert_eq!(subscription_count(browser.anchor), 0);
    }

    #[test]
    fn a_browser_that_rotates_its_endpoint_keeps_one_row() {
        setup();
        let browser = registered_browser(1);

        subscribe(&browser, "https://relay.example/first", 1_000).unwrap();
        subscribe(&browser, "https://relay.example/second", 2_000).unwrap();

        assert_eq!(
            subscription_count(browser.anchor),
            1,
            "a new endpoint for the same browser must overwrite, not accumulate"
        );
        assert_eq!(
            stored_endpoint(browser.anchor, browser.id).as_deref(),
            Some("https://relay.example/second")
        );
    }

    #[test]
    fn two_browsers_of_one_identity_hold_their_own_rows() {
        setup();
        let (one, other) = two_registered_browsers();

        subscribe(&one, "https://relay.example/one", 1_000).unwrap();
        subscribe(&other, "https://relay.example/other", 1_000).unwrap();

        assert_eq!(subscription_count(one.anchor), 2);
    }

    #[test]
    fn unsubscribing_twice_is_a_harmless_no_op() {
        setup();
        let browser = registered_browser(1);

        remove_subscription(browser.anchor, browser.id);
        remove_subscription(browser.anchor, browser.id);

        assert_eq!(subscription_count(browser.anchor), 0);
    }

    #[test]
    fn an_unregistered_browser_key_is_refused() {
        setup();
        let browser = registered_browser(1);
        let stranger = unregistered_browser(9);

        let request = request_from(&stranger, browser.anchor, "https://relay.example/a", 0);
        assert!(matches!(
            subscribe_via_proof(request),
            Err(SubscribeDeviceError::InvalidBrowserKey)
        ));
        assert_eq!(subscription_count(browser.anchor), 0);
    }

    #[test]
    fn a_signature_over_another_endpoint_is_refused() {
        setup();
        let browser = registered_browser(1);

        // The browser's own key, signed over an endpoint other than the one it is
        // registering: lifting a signature onto a different endpoint must not work.
        let mut request = request_from(&browser, browser.anchor, "https://relay.example/a", 0);
        request.browser_key_signature = browser.sign("https://relay.example/elsewhere", 0);

        assert!(matches!(
            subscribe_via_proof(request),
            Err(SubscribeDeviceError::InvalidBrowserKey)
        ));
        assert_eq!(subscription_count(browser.anchor), 0);
    }

    #[test]
    fn one_browser_cannot_take_over_anothers_row() {
        setup();
        let (one, other) = two_registered_browsers();
        subscribe(&other, "https://relay.example/other", 1_000).unwrap();

        // `one` signs correctly, but for itself: the proof decides the row, so the worst
        // it can do is write its own.
        subscribe(&one, "https://relay.example/hijack", 2_000).unwrap();

        assert_eq!(
            stored_endpoint(other.anchor, other.id).as_deref(),
            Some("https://relay.example/other"),
            "the other browser's row must be untouched"
        );
    }

    #[test]
    fn subscribe_rejects_malformed_input() {
        setup();
        let browser = registered_browser(1);
        let endpoint = "https://relay.example/a";

        assert!(subscribe(&browser, "", 0).is_err());

        let long_endpoint = format!("https://relay.example/{}", "x".repeat(MAX_ENDPOINT_LEN));
        assert!(add_subscription(subscription_of(&browser, &long_endpoint), 0).is_err());

        for bad_key in [vec![4u8; 10], vec![4u8; 65]] {
            let mut subscription = subscription_of(&browser, endpoint);
            subscription.vapid_public_key = bad_key;
            assert!(add_subscription(subscription, 0).is_err());
        }

        for bad_pool in [
            vec![],
            vec![vec![3u8; JWT_SIG_LEN]; MAX_JWT_POOL_LEN + 1],
            vec![vec![3u8; 10]],
        ] {
            let mut subscription = subscription_of(&browser, endpoint);
            subscription.jwt_signatures = bad_pool;
            assert!(add_subscription(subscription, 0).is_err());
        }
    }

    #[test]
    fn aggregates_all_validation_errors() {
        setup();
        let browser = registered_browser(1);

        // Two bad fields should surface two errors, not just the first.
        let mut subscription = subscription_of(&browser, "https://relay.example/a");
        subscription.vapid_public_key = vec![4u8; 10];
        subscription.jwt_signatures = vec![];

        let errors = add_subscription(subscription, 0).unwrap_err();
        assert_eq!(errors.len(), 2);
    }
}
