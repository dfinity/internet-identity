//! Send content-free wake-up pushes to recipients' subscribed browsers.
//! Remove notifications before awaiting relay responses to prevent another pass
//! from sending them again. Failed pushes are not retried.
// The browser fetch endpoint is added in a follow-up PR.
#![allow(dead_code)]

use crate::notifications::processing::{self, Due};
use crate::notifications::webpush::{clear_subscription, vapid_jwt};
use crate::notifications::BROWSER_GONE_AFTER_NS;
use crate::state::storage_borrow;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, Timestamp, Urgency,
};

const SECOND_NS: u64 = 1_000_000_000;

const INTERVAL: std::time::Duration = std::time::Duration::from_secs(2);

/// Post budget per pass. Only the first recipient of a pass may exceed it.
const MAX_DELIVERIES: usize = 50;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Delivery {
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) browser_id: BrowserId,
    pub(crate) endpoint: String,
    pub(crate) vapid_public_key: Vec<u8>,
    pub(crate) jwt: String,
    pub(crate) urgency: Urgency,
    /// Relay retention time, capped by the notification's remaining lifetime.
    pub(crate) ttl_seconds: u64,
}

/// Prepare posts and remove their notifications, including those with no reachable browser.
///
/// A recipient's fan-out is all or nothing, so one that would overrun the budget waits
/// for the next pass. The first recipient of a pass always goes, or a recipient with
/// more browsers than the whole budget would never be served.
pub(crate) fn plan_deliveries(max_deliveries: usize, now_ns: Timestamp) -> Vec<Delivery> {
    let mut deliveries = Vec::new();
    for due in processing::next_due(max_deliveries, now_ns) {
        let mut planned = Vec::new();
        fan_out_to_browsers(&due, now_ns, &mut planned);
        if !deliveries.is_empty() && deliveries.len() + planned.len() > max_deliveries {
            break;
        }
        processing::remove_due(&due);
        deliveries.append(&mut planned);
        if deliveries.len() >= max_deliveries {
            break;
        }
    }
    deliveries
}

/// Prepare one post per subscribed browser with a valid signature window.
fn fan_out_to_browsers(due: &Due, now_ns: Timestamp, out: &mut Vec<Delivery>) {
    let Ok(anchor) = storage_borrow(|storage| storage.read(due.anchor_number)) else {
        return;
    };
    let ttl_seconds = due.expires_at_ns.saturating_sub(now_ns) / SECOND_NS;

    for browser in anchor.browsers() {
        if now_ns.saturating_sub(browser.last_used) >= BROWSER_GONE_AFTER_NS {
            continue;
        }
        let Some(subscription) = &browser.webpush_subscription else {
            continue;
        };
        let Some(relay_origin) = vapid_jwt::relay_origin_of(&subscription.endpoint) else {
            continue;
        };
        let Some(jwt) = vapid_jwt::assemble(subscription, &relay_origin, now_ns) else {
            continue;
        };
        out.push(Delivery {
            anchor_number: due.anchor_number,
            browser_id: browser.id,
            endpoint: subscription.endpoint.clone(),
            vapid_public_key: subscription.vapid_public_key.clone(),
            jwt,
            urgency: due.urgency.clone(),
            ttl_seconds,
        });
    }
}

/// Build retention, urgency, and VAPID authorization headers.
pub(crate) fn push_headers(delivery: &Delivery) -> Vec<(String, String)> {
    vec![
        ("TTL".to_string(), delivery.ttl_seconds.to_string()),
        (
            "Urgency".to_string(),
            urgency_header(&delivery.urgency).to_string(),
        ),
        (
            "Authorization".to_string(),
            format!(
                "vapid t={}, k={}",
                delivery.jwt,
                BASE64_URL_SAFE_NO_PAD.encode(&delivery.vapid_public_key)
            ),
        ),
    ]
}

fn urgency_header(urgency: &Urgency) -> &'static str {
    match urgency {
        Urgency::High => "high",
        Urgency::Normal => "normal",
        Urgency::Low => "low",
        Urgency::VeryLow => "very-low",
    }
}

/// Remove registrations whose relay returned 410 (subscription gone).
pub(crate) fn forget_gone_subscriptions(deliveries: &[Delivery], gone: &[bool]) {
    for (delivery, _) in deliveries.iter().zip(gone).filter(|(_, gone)| **gone) {
        if let Err(err) = clear_subscription(delivery.anchor_number, delivery.browser_id) {
            ic_cdk::println!("Failed to drop a push registration the relay called gone: {err}");
        }
    }
}

#[cfg(not(test))]
pub(crate) use outcalls::start;

#[cfg(test)]
pub(crate) fn start() {}

#[cfg(not(test))]
mod outcalls {
    use super::*;
    use crate::concurrency::{ConcurrencyLimiter, LimiterConfig};
    use candid::Principal;
    use ic_cdk::api::management_canister::http_request::{
        HttpHeader, HttpMethod, HttpResponse, TransformContext,
    };
    use std::cell::RefCell;

    /// Cycle budget per POST; unused cycles are refunded.
    const RELAY_CYCLES: u128 = 20_000_000_000;
    /// Allow room for relay headers even though only the status is used.
    const MAX_RESPONSE_BYTES: u64 = 8 * 1024;

    thread_local! {
        /// Prevent overlapping passes; reclaim after the outcall timeout.
        static PASS_LIMIT: RefCell<ConcurrencyLimiter> = RefCell::new(
            ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 1, max_age_secs: 120 }),
        );
    }

    /// This SDK lacks `is_replicated`; set it explicitly to avoid one POST per replica.
    #[derive(candid::CandidType)]
    struct NonReplicatedHttpRequest {
        url: String,
        max_response_bytes: Option<u64>,
        method: HttpMethod,
        headers: Vec<HttpHeader>,
        body: Option<Vec<u8>>,
        transform: Option<TransformContext>,
        is_replicated: Option<bool>,
    }

    /// Arm on install and upgrade; upgrades clear existing timers.
    pub(crate) fn start() {
        ic_cdk_timers::set_timer_interval(INTERVAL, || {
            ic_cdk::spawn(run_pass(ic_cdk::api::time()));
        });
    }

    pub(crate) async fn run_pass(now_ns: Timestamp) {
        let Some(_permit) = crate::concurrency::acquire(&PASS_LIMIT) else {
            return;
        };
        let deliveries = plan_deliveries(MAX_DELIVERIES, now_ns);
        let gone = futures::future::join_all(deliveries.iter().map(post_wake_up)).await;
        forget_gone_subscriptions(&deliveries, &gone);
    }

    /// Return whether the relay reported the subscription gone (410).
    async fn post_wake_up(delivery: &Delivery) -> bool {
        let request = NonReplicatedHttpRequest {
            url: delivery.endpoint.clone(),
            max_response_bytes: Some(MAX_RESPONSE_BYTES),
            method: HttpMethod::POST,
            headers: push_headers(delivery)
                .into_iter()
                .map(|(name, value)| HttpHeader { name, value })
                .collect(),
            body: None,
            transform: None,
            is_replicated: Some(false),
        };

        let result: ic_cdk::api::call::CallResult<(HttpResponse,)> =
            ic_cdk::api::call::call_with_payment128(
                Principal::management_canister(),
                "http_request",
                (request,),
                RELAY_CYCLES,
            )
            .await;
        matches!(result, Ok((response,)) if response.status == 410u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::admission_queue::{Entry, Taken};
    use crate::notifications::backlog::PendingNotification;
    use crate::notifications::webpush::fixtures::{
        anchor_with_browsers, setup, stored_subscription, subscribe,
    };
    use crate::storage::storable::application::StorableOriginSha256;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const RELAY: &str = "https://relay.example/wpush/abc";

    fn queue(anchor_number: AnchorNumber, notification_id: u64, expires_at_ns: Timestamp) {
        processing::store_batch(&[Taken {
            sender: StorableOriginSha256::from_origin(&APP.to_string()),
            entry: Entry {
                received_at_ns: 1,
                expires_at_ns,
                item: PendingNotification {
                    recipient: candid::Principal::from_slice(&anchor_number.to_be_bytes()),
                    anchor_number,
                    notification_id,
                    urgency: Urgency::Normal,
                    expires_at_ns: Some(expires_at_ns),
                },
            },
        }])
        .expect("room in the processing queue");
    }

    #[test]
    fn a_due_notification_becomes_one_post_per_registered_browser() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(2);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, 0);
        }
        queue(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_deliveries(MAX_DELIVERIES, SECOND_NS);

        assert_eq!(deliveries.len(), 2);
        assert_eq!(
            deliveries
                .iter()
                .map(|one| one.browser_id)
                .collect::<Vec<_>>(),
            browsers
        );
        assert!(deliveries.iter().all(|one| one.endpoint == RELAY));
        assert!(processing::next_due(10, SECOND_NS).is_empty());
    }

    #[test]
    fn a_browser_that_never_registered_gets_nothing() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(3);
        subscribe(recipient, browsers[1], RELAY, 0);
        queue(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_deliveries(MAX_DELIVERIES, SECOND_NS);

        assert_eq!(deliveries.len(), 1);
        assert_eq!(deliveries[0].browser_id, browsers[1]);
    }

    /// The wake-up leads to a fetch nobody is waiting for, so it is not worth an outcall.
    #[test]
    fn a_browser_unused_past_the_window_is_not_woken() {
        setup();
        let long_after = BROWSER_GONE_AFTER_NS + SECOND_NS;
        let (recipient, browsers) = anchor_with_browsers(2);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, long_after);
        }
        queue(recipient, 7, long_after + 10 * SECOND_NS);

        assert!(plan_deliveries(MAX_DELIVERIES, long_after).is_empty());
        // Removed all the same: nothing can deliver it.
        assert!(processing::next_due(10, long_after).is_empty());
    }

    #[test]
    fn a_pass_stops_at_its_budget_rather_than_splitting_a_fan_out() {
        setup();
        let (first, first_browsers) = anchor_with_browsers(2);
        let (second, second_browsers) = anchor_with_browsers(2);
        for browser in &first_browsers {
            subscribe(first, *browser, RELAY, 0);
        }
        for browser in &second_browsers {
            subscribe(second, *browser, RELAY, 0);
        }
        queue(first, 1, 10 * SECOND_NS);
        queue(second, 2, 20 * SECOND_NS);

        // Room for three, and the second recipient needs two.
        let deliveries = plan_deliveries(3, SECOND_NS);

        assert_eq!(deliveries.len(), 2);
        assert!(deliveries.iter().all(|one| one.anchor_number == first));
        assert_eq!(processing::next_due(10, SECOND_NS).len(), 1);
    }

    /// Otherwise a recipient with more browsers than the budget is never served.
    #[test]
    fn the_first_recipient_of_a_pass_goes_even_when_it_alone_overruns() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(3);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, 0);
        }
        queue(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_deliveries(2, SECOND_NS);

        assert_eq!(deliveries.len(), 3);
        assert!(processing::next_due(10, SECOND_NS).is_empty());
    }

    #[test]
    fn a_recipient_with_no_registration_still_frees_its_slot() {
        setup();
        let (recipient, _) = anchor_with_browsers(2);
        queue(recipient, 7, 10 * SECOND_NS);

        assert!(plan_deliveries(MAX_DELIVERIES, SECOND_NS).is_empty());

        assert!(processing::next_due(10, SECOND_NS).is_empty());
    }

    #[test]
    fn a_browser_whose_signed_pool_has_run_out_is_skipped() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        subscribe(recipient, browsers[0], RELAY, 0);
        let spent = 3 * 24 * 60 * 60 * SECOND_NS;
        queue(recipient, 7, spent + 10 * SECOND_NS);

        assert!(plan_deliveries(MAX_DELIVERIES, spent + SECOND_NS).is_empty());
    }

    #[test]
    fn a_pass_stops_at_its_delivery_budget() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        subscribe(recipient, browsers[0], RELAY, 0);
        for notification_id in 0..MAX_DELIVERIES as u64 + 10 {
            queue(recipient, notification_id, 10 * SECOND_NS);
        }

        assert_eq!(
            plan_deliveries(MAX_DELIVERIES, SECOND_NS).len(),
            MAX_DELIVERIES
        );

        assert_eq!(plan_deliveries(MAX_DELIVERIES, SECOND_NS).len(), 10);
    }

    #[test]
    fn the_headers_carry_retention_urgency_and_the_vapid_pair() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        subscribe(recipient, browsers[0], RELAY, 0);
        queue(recipient, 7, 61 * SECOND_NS);

        let delivery = plan_deliveries(MAX_DELIVERIES, SECOND_NS).remove(0);
        let headers = push_headers(&delivery);

        assert_eq!(delivery.ttl_seconds, 60);
        assert_eq!(headers[0], ("TTL".to_string(), "60".to_string()));
        assert_eq!(headers[1], ("Urgency".to_string(), "normal".to_string()));
        let (name, value) = &headers[2];
        assert_eq!(name, "Authorization");
        assert_eq!(
            *value,
            format!(
                "vapid t={}, k={}",
                delivery.jwt,
                BASE64_URL_SAFE_NO_PAD.encode(&delivery.vapid_public_key)
            )
        );
    }

    #[test]
    fn a_registration_the_relay_calls_gone_is_dropped() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(2);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, 0);
        }
        queue(recipient, 7, 10 * SECOND_NS);
        let deliveries = plan_deliveries(MAX_DELIVERIES, SECOND_NS);

        forget_gone_subscriptions(&deliveries, &[true, false]);

        assert!(stored_subscription(recipient, browsers[0]).is_none());
        assert!(stored_subscription(recipient, browsers[1]).is_some());
    }

    #[test]
    fn every_urgency_has_its_rfc_8030_spelling() {
        let spellings: Vec<&str> = [
            Urgency::High,
            Urgency::Normal,
            Urgency::Low,
            Urgency::VeryLow,
        ]
        .iter()
        .map(urgency_header)
        .collect();

        assert_eq!(spellings, vec!["high", "normal", "low", "very-low"]);
    }
}
