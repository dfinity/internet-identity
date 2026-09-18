//! What a browser registered for Web Push, held on its entry in the identity's browser
//! registry.

use super::validation::{
    ValidatedRemoveWebPushSubscriptionRequest, ValidatedSetWebPushSubscriptionRequest,
};
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, WebPushSubscription};
use internet_identity_interface::internet_identity::types::{
    BrowserId, RemoveWebPushSubscriptionError, SetWebPushSubscriptionError, Timestamp,
};

/// Registers `browser_id` for Web Push, and is also how it replaces a pool that is
/// running out. Idempotent: a browser that re-subscribes overwrites what it registered
/// before, endpoint included.
pub fn set_subscription(
    anchor: Anchor,
    browser_id: BrowserId,
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

    // The same endpoint means this replaces the pool on a registration that is still
    // live, so the registration keeps its age and the pool has to move forward: one
    // signature covers one elapsed window, and an older pool would shorten the coverage
    // the browser believes it has. Any other endpoint is a new registration.
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
    let anchor = storage_borrow(|storage| storage.read(anchor_number))
        .map_err(|err| RemoveWebPushSubscriptionError::InternalCanisterError(format!("{err}")))?;
    write_subscription(anchor, browser_id, None)
        .map_err(RemoveWebPushSubscriptionError::InternalCanisterError)
}

fn write_subscription(
    mut anchor: Anchor,
    browser_id: BrowserId,
    subscription: Option<WebPushSubscription>,
) -> Result<(), String> {
    anchor.set_webpush_subscription(browser_id, subscription);
    storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use internet_identity_interface::internet_identity::types::RemoveWebPushSubscriptionRequest;

    const ENDPOINT: &str = "https://relay.example/a";
    const ROTATED: &str = "https://relay.example/b";

    fn remove(anchor_number: AnchorNumber, browser_id: BrowserId) {
        let request = RemoveWebPushSubscriptionRequest {
            anchor_number,
            browser_id,
        };
        remove_subscription(request.try_into().expect("the fixture request validates"))
            .expect("removing a subscription");
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
}
