//! One subscription row per browser, keyed `(anchor, browser_id)` against the
//! registry the anchor already holds.

use super::validation::{
    ValidatedRemoveWebPushSubscriptionRequest, ValidatedSetWebPushSubscriptionRequest,
};
use crate::state::storage_borrow_mut;
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use internet_identity_interface::internet_identity::types::{BrowserId, Timestamp};
use minicbor::bytes::ByteVec;

/// Registers `browser_id` for Web Push. Idempotent: a browser that re-subscribes
/// overwrites its own row, endpoint included.
pub fn set_subscription(
    request: ValidatedSetWebPushSubscriptionRequest,
    browser_id: BrowserId,
    now_ns: Timestamp,
) {
    let ValidatedSetWebPushSubscriptionRequest {
        anchor_number,
        endpoint,
        vapid_public_key,
        jwt_signatures,
        jwt_issued_at_ns,
    } = request;

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
        storage.write_webpush_subscription(anchor_number, browser_id, row)
    });
}

/// Stops notifications to one browser, and with them the JWT pool on the same row.
/// Idempotent.
///
/// Takes the browser rather than reading it off the caller, since silencing one is done
/// from another.
pub fn remove_subscription(
    ValidatedRemoveWebPushSubscriptionRequest {
        anchor_number,
        browser_id,
    }: ValidatedRemoveWebPushSubscriptionRequest,
) {
    storage_borrow_mut(|storage| storage.remove_webpush_subscription(anchor_number, browser_id));
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use crate::notifications::test_setup as setup;
    use internet_identity_interface::internet_identity::types::AnchorNumber;

    const ANCHOR: AnchorNumber = 10_000;

    fn remove(anchor_number: AnchorNumber, browser_id: BrowserId) {
        remove_subscription(ValidatedRemoveWebPushSubscriptionRequest {
            anchor_number,
            browser_id,
        });
    }

    #[test]
    fn setting_then_removing_round_trips() {
        setup();

        set_subscription(
            validated(ANCHOR, "https://relay.example/a", 1_000),
            1,
            1_000,
        );
        assert_eq!(
            stored_endpoint(ANCHOR, 1).as_deref(),
            Some("https://relay.example/a")
        );

        remove(ANCHOR, 1);
        assert_eq!(stored_endpoint(ANCHOR, 1), None);
    }

    #[test]
    fn a_browser_that_rotates_its_endpoint_keeps_one_row() {
        setup();

        set_subscription(
            validated(ANCHOR, "https://relay.example/first", 1_000),
            1,
            1_000,
        );
        set_subscription(
            validated(ANCHOR, "https://relay.example/second", 2_000),
            1,
            2_000,
        );

        assert_eq!(
            stored_endpoint(ANCHOR, 1).as_deref(),
            Some("https://relay.example/second"),
            "a new endpoint for the same browser must overwrite, not accumulate"
        );
    }

    #[test]
    fn two_browsers_of_one_identity_hold_their_own_rows() {
        setup();

        set_subscription(
            validated(ANCHOR, "https://relay.example/one", 1_000),
            1,
            1_000,
        );
        set_subscription(
            validated(ANCHOR, "https://relay.example/other", 1_000),
            2,
            1_000,
        );

        assert_eq!(
            stored_endpoint(ANCHOR, 1).as_deref(),
            Some("https://relay.example/one")
        );
        assert_eq!(
            stored_endpoint(ANCHOR, 2).as_deref(),
            Some("https://relay.example/other")
        );
    }

    #[test]
    fn removing_twice_is_a_harmless_no_op() {
        setup();

        remove(ANCHOR, 1);
        remove(ANCHOR, 1);

        assert_eq!(stored_endpoint(ANCHOR, 1), None);
    }
}
