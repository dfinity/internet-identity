//! The Web Push channel: what a browser registered, and the pool of VAPID JWTs that
//! browser signed for it.
//!
//! Callers reach this through `main.rs`, which validates and authorizes first, so
//! everything here acts on a browser the caller has already been shown to be.

mod subscription;
mod validation;

pub use subscription::{remove_subscription, set_subscription, subscription_status};
pub use validation::{
    ValidatedGetWebPushSubscriptionStatusRequest, ValidatedRemoveWebPushSubscriptionRequest,
    ValidatedSetWebPushSubscriptionRequest,
};

#[cfg(test)]
pub(crate) mod fixtures {
    use super::validation::{ValidatedSetWebPushSubscriptionRequest, JWT_SIG_LEN};
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

    fn chrome_on_a_mac() -> BrowserDescription {
        BrowserDescription {
            brand: BrowserBrand::Chrome,
            os: OperatingSystem::Macos,
            form_factor: FormFactor::Desktop,
            model: None,
        }
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

    pub(crate) fn valid_pool() -> Vec<Vec<u8>> {
        vec![vec![3u8; JWT_SIG_LEN]; 3]
    }

    /// Well-formed defaults, so a field added to the request doesn't touch every test.
    pub(crate) fn request(
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

    pub(crate) fn validated(
        anchor_number: AnchorNumber,
        endpoint: &str,
        jwt_issued_at_ns: Timestamp,
    ) -> ValidatedSetWebPushSubscriptionRequest {
        ValidatedSetWebPushSubscriptionRequest {
            anchor_number,
            endpoint: endpoint.to_string(),
            vapid_public_key: valid_vapid_key(),
            jwt_signatures: valid_pool(),
            jwt_issued_at_ns,
        }
    }

    /// `set_subscription` for a test that has an anchor number rather than the anchor.
    pub(crate) fn try_subscribe(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        endpoint: &str,
        now_ns: Timestamp,
    ) -> Result<(), SetWebPushSubscriptionError> {
        super::set_subscription(
            anchor(anchor_number),
            browser_id,
            validated(anchor_number, endpoint, now_ns),
            now_ns,
        )
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

    pub(crate) fn stored_endpoint(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    ) -> Option<String> {
        stored_subscription(anchor_number, browser_id).map(|subscription| subscription.endpoint)
    }
}
