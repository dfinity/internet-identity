//! Send content-free wake-ups for backlog notifications, one per registered browser.
//! A pass queues each notification for the browser before posting its wake-up, and
//! spawns the posts rather than awaiting them. Nothing is retried; every outcome is counted.

use crate::notifications::admission_queue::Taken;
use crate::notifications::backlog::PendingNotification;
use crate::notifications::browser_queue::{self, Added};
use crate::notifications::webpush::{clear_gone_subscription, vapid_jwt};
use crate::notifications::{notifications_enabled, BROWSER_GONE_AFTER_NS};
use crate::state::{self, storage_borrow_mut};
use crate::storage::anchor::{Browser, QueuedNotification};
use crate::storage::storable::application::StorableOriginSha256;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, Timestamp, Urgency,
};
use std::cell::Cell;
use std::convert::Infallible;
use std::time::Duration;

const SECOND_NS: u64 = 1_000_000_000;

pub(crate) const INTERVAL: Duration = Duration::from_secs(1);

/// Wake-ups one pass posts. Only a pass's last recipient may take it over.
pub(crate) const MAX_POSTS_PER_PASS: usize = 65;

/// Notifications one pass takes, however few wake-ups they turn into.
pub(crate) const MAX_TAKEN_PER_PASS: usize = 200;

/// What became of one wake-up post.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PostOutcome {
    Sent,
    /// 404 or 410: the relay no longer holds the subscription.
    Gone,
    RateLimited,
    /// Any other answer from the relay.
    RelayError,
    /// The outcall failed, so the relay never answered.
    Rejected,
}

impl PostOutcome {
    pub(crate) fn of_status(status: u32) -> Self {
        match status {
            200..=299 => PostOutcome::Sent,
            404 | 410 => PostOutcome::Gone,
            429 => PostOutcome::RateLimited,
            _ => PostOutcome::RelayError,
        }
    }
}

/// Wake-up posts since the last upgrade, by outcome.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) struct PostOutcomes {
    pub(crate) sent: u64,
    pub(crate) gone: u64,
    pub(crate) rate_limited: u64,
    pub(crate) relay_error: u64,
    pub(crate) rejected: u64,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct PassSchedule {
    /// When the armed pass runs. A pass that trapped never cleared it, so one overdue
    /// by an interval counts as lost and is armed again.
    due_at_ns: Option<Timestamp>,
    last_started_ns: Option<Timestamp>,
    /// Counts every arming, so a timer armed before a re-arm runs no pass of its own.
    armed: u64,
}

thread_local! {
    static POST_OUTCOMES: Cell<PostOutcomes> = Cell::new(PostOutcomes::default());
    static PASS_SCHEDULE: Cell<PassSchedule> = Cell::new(PassSchedule::default());
}

pub(crate) fn post_outcomes() -> PostOutcomes {
    POST_OUTCOMES.with(Cell::get)
}

fn record_post_outcome(outcome: PostOutcome) {
    POST_OUTCOMES.with(|counts| {
        let mut tally = counts.get();
        let count = match outcome {
            PostOutcome::Sent => &mut tally.sent,
            PostOutcome::Gone => &mut tally.gone,
            PostOutcome::RateLimited => &mut tally.rate_limited,
            PostOutcome::RelayError => &mut tally.relay_error,
            PostOutcome::Rejected => &mut tally.rejected,
        };
        *count = count.saturating_add(1);
        counts.set(tally);
    });
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Delivery {
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) browser_id: BrowserId,
    /// The browser queue entry this wake-up announces.
    pub(crate) notification: QueuedNotification,
    pub(crate) endpoint: String,
    pub(crate) vapid_public_key: Vec<u8>,
    pub(crate) jwt: String,
    pub(crate) urgency: Urgency,
    /// Relay retention time, capped by the notification's remaining lifetime.
    pub(crate) ttl_seconds: u64,
}

/// Run a pass as soon as the budget allows: at once after a quiet spell, otherwise one
/// interval after the last pass started. Does nothing while a pass is already due.
pub(crate) fn schedule_pass(now_ns: Timestamp) {
    let delay = PASS_SCHEDULE.with(|schedule| {
        let current = schedule.get();
        let interval_ns = INTERVAL.as_nanos() as u64;
        if current
            .due_at_ns
            .is_some_and(|due_at_ns| now_ns <= due_at_ns.saturating_add(interval_ns))
        {
            return None;
        }
        let delay = delay_until_next_pass(current.last_started_ns, now_ns);
        let armed = current.armed.wrapping_add(1);
        schedule.set(PassSchedule {
            due_at_ns: Some(now_ns.saturating_add(delay.as_nanos() as u64)),
            armed,
            ..current
        });
        Some((delay, armed))
    });
    if let Some((delay, armed)) = delay {
        outcalls::arm(delay, armed);
    }
}

#[cfg(test)]
pub(crate) fn pass_due_at_for_testing() -> Option<Timestamp> {
    PASS_SCHEDULE.with(Cell::get).due_at_ns
}

#[cfg(test)]
pub(crate) fn reset_pass_schedule_for_testing() {
    PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));
}

fn delay_until_next_pass(last_started_ns: Option<Timestamp>, now_ns: Timestamp) -> Duration {
    let next_ns = last_started_ns.map_or(now_ns, |last| {
        last.saturating_add(INTERVAL.as_nanos() as u64)
    });
    Duration::from_nanos(next_ns.saturating_sub(now_ns))
}

/// Post this pass's wake-ups without waiting on them, then come back while work is left.
/// A timer armed before the latest arming runs nothing.
fn run_pass(armed: u64, now_ns: Timestamp) {
    let is_latest = PASS_SCHEDULE.with(|schedule| {
        let is_latest = schedule.get().armed == armed;
        if is_latest {
            schedule.set(PassSchedule {
                due_at_ns: None,
                last_started_ns: Some(now_ns),
                armed,
            });
        }
        is_latest
    });
    if !is_latest {
        return;
    }
    for delivery in plan_pass(MAX_POSTS_PER_PASS, now_ns) {
        outcalls::spawn_post(delivery);
    }
    let work_left =
        state::notification_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total > 0);
    if work_left {
        schedule_pass(now_ns);
    }
}

/// Take notifications in backlog order and queue each for its recipient's browsers,
/// returning the wake-ups to post. Taken one at a time, so a pass stops at its budget,
/// and never more than [`MAX_TAKEN_PER_PASS`], so ones that wake nothing cannot run on.
pub(crate) fn plan_pass(max_posts: usize, now_ns: Timestamp) -> Vec<Delivery> {
    if !notifications_enabled() {
        return Vec::new();
    }

    let mut deliveries = Vec::new();
    for _ in 0..MAX_TAKEN_PER_PASS {
        if deliveries.len() >= max_posts {
            break;
        }
        let taken = state::notification_backlog_mut(now_ns, |backlog| {
            backlog.take_batch(1, now_ns, |batch| Ok::<_, Infallible>(batch.to_vec()))
        })
        .unwrap_or_default();
        let Some(taken) = taken.first() else {
            break;
        };
        fan_out_to_browsers(taken, now_ns, &mut deliveries);
    }
    deliveries
}

/// Queue the notification for every browser that can be woken, preparing a wake-up
/// for each entry that needs one. A full queue's replaced entry already has one. The
/// queues live on the anchor, so one write covers every browser.
fn fan_out_to_browsers(
    taken: &Taken<StorableOriginSha256, PendingNotification>,
    now_ns: Timestamp,
    out: &mut Vec<Delivery>,
) {
    let pending = &taken.entry.item;
    let queued = QueuedNotification {
        application_number: pending.application_number,
        sender: pending.sender,
        notification_id: pending.notification_id,
        expires_at_ns: taken.entry.expires_at_ns,
        account_number: pending.account_number,
    };
    let ttl_seconds = queued.expires_at_ns.saturating_sub(now_ns) / SECOND_NS;
    storage_borrow_mut(|storage| {
        let Ok(mut anchor) = storage.read(pending.anchor_number) else {
            return;
        };
        let woken: Vec<Delivery> = anchor
            .browsers()
            .iter()
            .filter_map(|browser| {
                let subscription = browser.webpush_subscription.as_ref()?;
                let jwt = wake_up_jwt(browser, now_ns)?;
                Some(Delivery {
                    anchor_number: pending.anchor_number,
                    browser_id: browser.id,
                    notification: queued.clone(),
                    endpoint: subscription.endpoint.clone(),
                    vapid_public_key: subscription.vapid_public_key.clone(),
                    jwt,
                    urgency: pending.urgency.clone(),
                    ttl_seconds,
                })
            })
            .collect();
        if woken.is_empty() {
            return;
        }
        let mut to_post = Vec::new();
        for delivery in woken {
            let Some(queue) = anchor.notifications_mut(delivery.browser_id) else {
                continue;
            };
            if browser_queue::add(queue, queued.clone(), now_ns) == Added::Queued {
                to_post.push(delivery);
            }
        }
        match storage.write(anchor) {
            Ok(()) => out.extend(to_post),
            Err(err) => ic_cdk::println!("Failed to queue a notification for its browsers: {err}"),
        }
    });
}

/// The authorization a wake-up to `browser` carries now: none unless it was used within
/// [`BROWSER_GONE_AFTER_NS`], is registered, and its pool holds a signature for now.
pub(crate) fn wake_up_jwt(browser: &Browser, now_ns: Timestamp) -> Option<String> {
    if now_ns.saturating_sub(browser.last_used) >= BROWSER_GONE_AFTER_NS {
        return None;
    }
    let subscription = browser.webpush_subscription.as_ref()?;
    let relay_origin = vapid_jwt::relay_origin_of(&subscription.endpoint)?;
    vapid_jwt::assemble(subscription, &relay_origin, now_ns)
}

/// Count the outcome and keep the browser's queue in step with the wake-ups on their
/// way: a relay that lost the subscription takes the registration and the queue with
/// it, and any other failure removes an entry, since no wake-up is coming for it.
pub(crate) fn settle(delivery: &Delivery, outcome: PostOutcome, now_ns: Timestamp) {
    record_post_outcome(outcome);
    match outcome {
        PostOutcome::Sent => {}
        PostOutcome::Gone => {
            if let Err(err) = clear_gone_subscription(
                delivery.anchor_number,
                delivery.browser_id,
                &delivery.endpoint,
            ) {
                ic_cdk::println!("Failed to drop a push registration the relay called gone: {err}");
            }
        }
        PostOutcome::RateLimited | PostOutcome::RelayError | PostOutcome::Rejected => {
            browser_queue::remove_after_failed_wake_up(
                delivery.anchor_number,
                delivery.browser_id,
                &delivery.notification,
                &delivery.endpoint,
                now_ns,
            );
        }
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

#[cfg(test)]
mod outcalls {
    use super::Delivery;
    use std::time::Duration;

    pub(super) fn arm(_delay: Duration, _armed: u64) {}

    pub(super) fn spawn_post(_delivery: Delivery) {}
}

#[cfg(not(test))]
mod outcalls {
    use super::*;
    use candid::Principal;
    use ic_cdk::api::management_canister::http_request::{
        HttpHeader, HttpMethod, HttpResponse, TransformContext,
    };

    /// Cycle budget per POST; unused cycles are refunded.
    const RELAY_CYCLES: u128 = 20_000_000_000;
    /// Allow room for relay headers even though only the status is used.
    const MAX_RESPONSE_BYTES: u64 = 8 * 1024;

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

    pub(super) fn arm(delay: Duration, armed: u64) {
        ic_cdk_timers::set_timer(delay, move || run_pass(armed, ic_cdk::api::time()));
    }

    pub(super) fn spawn_post(delivery: Delivery) {
        ic_cdk::spawn(async move {
            let outcome = post_wake_up(&delivery).await;
            settle(&delivery, outcome, ic_cdk::api::time());
        });
    }

    async fn post_wake_up(delivery: &Delivery) -> PostOutcome {
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
        match result {
            Ok((response,)) => {
                PostOutcome::of_status(u32::try_from(&response.status.0).unwrap_or(0))
            }
            Err(_) => PostOutcome::Rejected,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::backlog::NOTIFICATION_BACKLOG;
    use crate::notifications::webpush::fixtures::{
        anchor, anchor_with_browsers, setup, stored_subscription, subscribe,
    };
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{AnchorNumber, BrowserId};
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const RELAY: &str = "https://relay.example/wpush/abc";
    const ROTATED: &str = "https://relay.example/wpush/def";

    fn sender() -> Principal {
        Principal::from_slice(&[7; 10])
    }

    fn submit(anchor_number: AnchorNumber, notification_id: u64, expires_at_ns: Timestamp) {
        state::notification_backlog_mut(0, |backlog| {
            backlog.admit(
                StorableOriginSha256::from_origin(&APP.to_string()),
                vec![PendingNotification {
                    recipient: Principal::from_slice(&anchor_number.to_be_bytes()),
                    anchor_number,
                    application_number: 1,
                    account_number: Some(2),
                    sender: sender(),
                    notification_id,
                    urgency: Urgency::Normal,
                    expires_at_ns: Some(expires_at_ns),
                }],
                0,
            );
        });
    }

    fn still_in_backlog(now_ns: Timestamp) -> usize {
        state::notification_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total)
    }

    fn queued(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        anchor(anchor_number)
            .notifications(browser_id)
            .iter()
            .map(|queued| queued.notification_id)
            .collect()
    }

    fn change_queue(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        change: impl FnOnce(&mut Vec<QueuedNotification>),
    ) {
        let mut stored = anchor(anchor_number);
        change(
            stored
                .notifications_mut(browser_id)
                .expect("a registered browser"),
        );
        storage_borrow_mut(|storage| storage.write(stored)).expect("writing the anchor");
    }

    /// What the service worker does on a wake-up.
    fn take_oldest(anchor_number: AnchorNumber, browser_id: BrowserId) {
        change_queue(anchor_number, browser_id, |queue| {
            queue.remove(0);
        });
    }

    /// An identity with `count` browsers, every one registered with the relay.
    fn subscribed_recipient(count: u8) -> (AnchorNumber, Vec<BrowserId>) {
        let (recipient, browsers) = anchor_with_browsers(count);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, 0);
        }
        (recipient, browsers)
    }

    #[test]
    fn a_notification_is_queued_and_woken_once_per_registered_browser() {
        setup();
        let (recipient, browsers) = subscribed_recipient(2);
        submit(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(
            deliveries
                .iter()
                .map(|one| one.browser_id)
                .collect::<Vec<_>>(),
            browsers
        );
        assert!(deliveries.iter().all(|one| one.endpoint == RELAY));
        for browser in &browsers {
            assert_eq!(queued(recipient, *browser), vec![7]);
        }
        assert_eq!(still_in_backlog(SECOND_NS), 0);
    }

    #[test]
    fn a_browser_that_never_registered_is_neither_queued_nor_woken() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(3);
        subscribe(recipient, browsers[1], RELAY, 0);
        submit(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(deliveries.len(), 1);
        assert_eq!(deliveries[0].browser_id, browsers[1]);
        assert!(queued(recipient, browsers[0]).is_empty());
    }

    /// The wake-up leads to a fetch nobody is waiting for, so it is not worth an outcall.
    #[test]
    fn a_browser_unused_past_the_window_is_not_woken() {
        setup();
        let long_after = BROWSER_GONE_AFTER_NS + SECOND_NS;
        let (recipient, browsers) = subscribed_recipient(2);
        submit(recipient, 7, long_after + 10 * SECOND_NS);

        assert!(plan_pass(MAX_POSTS_PER_PASS, long_after).is_empty());
        assert!(queued(recipient, browsers[0]).is_empty());
        // Taken all the same: nothing can deliver it.
        assert_eq!(still_in_backlog(long_after), 0);
    }

    #[test]
    fn a_recipient_with_no_registration_is_taken_all_the_same() {
        setup();
        let (recipient, _) = anchor_with_browsers(2);
        submit(recipient, 7, 10 * SECOND_NS);

        assert!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());

        assert_eq!(still_in_backlog(SECOND_NS), 0);
    }

    #[test]
    fn a_browser_whose_signed_pool_has_run_out_is_skipped() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        let spent = 3 * 24 * 60 * 60 * SECOND_NS;
        submit(recipient, 7, spent + 10 * SECOND_NS);

        assert!(plan_pass(MAX_POSTS_PER_PASS, spent + SECOND_NS).is_empty());
        assert!(queued(recipient, browsers[0]).is_empty());
    }

    #[test]
    fn a_pass_stops_once_its_budget_is_spent() {
        setup();
        // Spread over recipients to stay within the per-recipient cap.
        for _ in 0..4 {
            let (recipient, _) = subscribed_recipient(1);
            for notification_id in 0..20 {
                submit(recipient, notification_id, 10 * SECOND_NS);
            }
        }

        assert_eq!(
            plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).len(),
            MAX_POSTS_PER_PASS
        );
        assert_eq!(still_in_backlog(SECOND_NS), 80 - MAX_POSTS_PER_PASS);

        assert_eq!(
            plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).len(),
            80 - MAX_POSTS_PER_PASS
        );
    }

    #[test]
    fn a_pass_stops_taking_at_its_bound_when_nothing_is_woken() {
        setup();
        let per_recipient = NOTIFICATION_BACKLOG.max_pending_per_group as u64;
        let recipients = MAX_TAKEN_PER_PASS as u64 / per_recipient + 1;
        for _ in 0..recipients {
            let (recipient, _) = anchor_with_browsers(1);
            for notification_id in 0..per_recipient {
                submit(recipient, notification_id, 10 * SECOND_NS);
            }
        }

        assert!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());

        assert_eq!(
            still_in_backlog(SECOND_NS) as u64,
            recipients * per_recipient - MAX_TAKEN_PER_PASS as u64
        );
    }

    /// Otherwise a recipient with more browsers than the budget left is split across
    /// passes, and one with more than the whole budget is never served.
    #[test]
    fn the_last_recipient_of_a_pass_may_take_it_over_budget() {
        setup();
        let (first, _) = subscribed_recipient(2);
        let (second, _) = subscribed_recipient(2);
        submit(first, 1, 10 * SECOND_NS);
        submit(second, 2, 10 * SECOND_NS);

        let deliveries = plan_pass(3, SECOND_NS);

        assert_eq!(deliveries.len(), 4);
        assert_eq!(still_in_backlog(SECOND_NS), 0);
    }

    #[test]
    fn a_full_browser_queue_takes_the_notification_without_a_new_wake_up() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        change_queue(recipient, browsers[0], |queue| {
            queue.extend(
                (0..browser_queue::MAX_PER_BROWSER as u64).map(|notification_id| {
                    QueuedNotification {
                        application_number: 1,
                        sender: sender(),
                        notification_id,
                        expires_at_ns: 10 * SECOND_NS,
                        account_number: Some(2),
                    }
                }),
            )
        });
        submit(recipient, 1_000, 10 * SECOND_NS);

        assert!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());

        let left = queued(recipient, browsers[0]);
        assert_eq!(left.len(), browser_queue::MAX_PER_BROWSER);
        assert_eq!(left.last(), Some(&1_000));
    }

    #[test]
    fn a_pass_does_nothing_while_notifications_are_off() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        crate::state::persistent_state_mut(|s| s.notifications_enabled_origins = None);

        assert!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());

        assert_eq!(still_in_backlog(SECOND_NS), 1);
    }

    #[test]
    fn queuing_for_a_browser_drops_what_expired_in_its_queue() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        submit(recipient, 8, 20 * SECOND_NS);

        plan_pass(MAX_POSTS_PER_PASS, 10 * SECOND_NS);

        assert_eq!(queued(recipient, browsers[0]), vec![8]);
    }

    #[test]
    fn a_wake_up_carries_what_the_service_worker_fetches_with() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);

        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);

        assert_eq!(
            delivery.notification,
            QueuedNotification {
                application_number: 1,
                sender: sender(),
                notification_id: 7,
                expires_at_ns: 10 * SECOND_NS,
                account_number: Some(2),
            }
        );
    }

    #[test]
    fn the_headers_carry_retention_urgency_and_the_vapid_pair() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 61 * SECOND_NS);

        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
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
    fn a_sent_wake_up_leaves_its_entry_for_the_service_worker() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);

        settle(&delivery, PostOutcome::Sent, SECOND_NS);

        assert_eq!(queued(recipient, browsers[0]), vec![7]);
    }

    #[test]
    fn a_failed_wake_up_removes_the_entry_it_announced() {
        for outcome in [
            PostOutcome::RateLimited,
            PostOutcome::RelayError,
            PostOutcome::Rejected,
        ] {
            setup();
            let (recipient, browsers) = subscribed_recipient(1);
            submit(recipient, 7, 10 * SECOND_NS);
            submit(recipient, 8, 10 * SECOND_NS);
            let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

            settle(&deliveries[1], outcome, SECOND_NS);

            assert_eq!(queued(recipient, browsers[0]), vec![7], "{outcome:?}");
            assert!(stored_subscription(recipient, browsers[0]).is_some());
        }
    }

    #[test]
    fn a_relay_that_lost_the_subscription_takes_the_registration_and_queue_with_it() {
        setup();
        let (recipient, browsers) = subscribed_recipient(2);
        submit(recipient, 7, 10 * SECOND_NS);
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        settle(&deliveries[0], PostOutcome::Gone, SECOND_NS);

        assert!(stored_subscription(recipient, browsers[0]).is_none());
        assert!(queued(recipient, browsers[0]).is_empty());
        assert!(stored_subscription(recipient, browsers[1]).is_some());
        assert_eq!(queued(recipient, browsers[1]), vec![7]);
    }

    #[test]
    fn a_late_gone_for_an_old_endpoint_leaves_the_new_registration() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
        subscribe(recipient, browsers[0], ROTATED, SECOND_NS);

        settle(&delivery, PostOutcome::Gone, SECOND_NS);

        assert_eq!(
            stored_subscription(recipient, browsers[0]).map(|registered| registered.endpoint),
            Some(ROTATED.to_string())
        );
    }

    #[test]
    fn a_failed_wake_up_whose_entry_was_taken_removes_the_oldest() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        for notification_id in [7, 8, 9] {
            submit(recipient, notification_id, 10 * SECOND_NS);
        }
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        take_oldest(recipient, browsers[0]);

        settle(&deliveries[0], PostOutcome::RelayError, SECOND_NS);

        assert_eq!(queued(recipient, browsers[0]), vec![9]);
    }

    #[test]
    fn a_failed_wake_up_past_its_deadline_removes_nothing_more() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        submit(recipient, 8, 20 * SECOND_NS);
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        take_oldest(recipient, browsers[0]);

        settle(&deliveries[0], PostOutcome::RelayError, 10 * SECOND_NS);

        assert_eq!(queued(recipient, browsers[0]), vec![8]);
    }

    #[test]
    fn a_failed_wake_up_for_an_old_endpoint_removes_nothing_from_the_new_one() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
        subscribe(recipient, browsers[0], ROTATED, SECOND_NS);
        submit(recipient, 8, 10 * SECOND_NS);
        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        settle(&delivery, PostOutcome::RelayError, SECOND_NS);

        assert_eq!(queued(recipient, browsers[0]), vec![8]);
    }

    #[test]
    fn a_404_and_a_410_both_mean_the_subscription_is_gone() {
        assert_eq!(PostOutcome::of_status(404), PostOutcome::Gone);
        assert_eq!(PostOutcome::of_status(410), PostOutcome::Gone);
    }

    #[test]
    fn every_other_answer_lands_in_its_outcome() {
        for (status, outcome) in [
            (200, PostOutcome::Sent),
            (201, PostOutcome::Sent),
            (429, PostOutcome::RateLimited),
            (400, PostOutcome::RelayError),
            (403, PostOutcome::RelayError),
            (500, PostOutcome::RelayError),
            (503, PostOutcome::RelayError),
        ] {
            assert_eq!(PostOutcome::of_status(status), outcome, "status {status}");
        }
    }

    #[test]
    fn post_outcomes_are_tallied_by_kind() {
        let before = post_outcomes();

        for outcome in [
            PostOutcome::Sent,
            PostOutcome::Sent,
            PostOutcome::Gone,
            PostOutcome::RateLimited,
            PostOutcome::RelayError,
            PostOutcome::Rejected,
        ] {
            record_post_outcome(outcome);
        }

        assert_eq!(
            post_outcomes(),
            PostOutcomes {
                sent: before.sent + 2,
                gone: before.gone + 1,
                rate_limited: before.rate_limited + 1,
                relay_error: before.relay_error + 1,
                rejected: before.rejected + 1,
            }
        );
    }

    #[test]
    fn the_first_pass_after_a_quiet_spell_runs_at_once() {
        assert_eq!(delay_until_next_pass(None, 5 * SECOND_NS), Duration::ZERO);
        assert_eq!(
            delay_until_next_pass(Some(SECOND_NS), 5 * SECOND_NS),
            Duration::ZERO
        );
    }

    #[test]
    fn a_pass_follows_the_last_one_by_an_interval() {
        assert_eq!(
            delay_until_next_pass(Some(SECOND_NS), SECOND_NS + 300_000_000),
            Duration::from_millis(700)
        );
    }

    #[test]
    fn scheduling_while_a_pass_is_due_arms_nothing_more() {
        PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));

        schedule_pass(SECOND_NS);
        schedule_pass(SECOND_NS + 1);

        assert_eq!(
            PASS_SCHEDULE.with(Cell::get),
            PassSchedule {
                due_at_ns: Some(SECOND_NS),
                last_started_ns: None,
                armed: 1,
            }
        );
    }

    #[test]
    fn a_pass_that_never_ran_is_armed_again_once_overdue() {
        PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));
        schedule_pass(SECOND_NS);

        schedule_pass(3 * SECOND_NS);

        assert_eq!(PASS_SCHEDULE.with(Cell::get).due_at_ns, Some(3 * SECOND_NS));
    }

    /// Otherwise a timer that was only late runs beside the one that replaced it, and
    /// both keep arming.
    #[test]
    fn a_timer_armed_before_a_re_arm_runs_no_pass() {
        setup();
        PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        schedule_pass(SECOND_NS);
        schedule_pass(3 * SECOND_NS);

        run_pass(1, 3 * SECOND_NS);

        assert_eq!(still_in_backlog(3 * SECOND_NS), 1);
        assert_eq!(PASS_SCHEDULE.with(Cell::get).due_at_ns, Some(3 * SECOND_NS));

        run_pass(2, 3 * SECOND_NS);

        assert_eq!(still_in_backlog(3 * SECOND_NS), 0);
    }

    #[test]
    fn a_pass_that_leaves_work_behind_schedules_the_next_one() {
        setup();
        PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));
        for _ in 0..4 {
            let (recipient, _) = subscribed_recipient(1);
            for notification_id in 0..20 {
                submit(recipient, notification_id, 10 * SECOND_NS);
            }
        }

        run_pass(0, SECOND_NS);

        assert_eq!(
            PASS_SCHEDULE.with(Cell::get),
            PassSchedule {
                due_at_ns: Some(2 * SECOND_NS),
                last_started_ns: Some(SECOND_NS),
                armed: 1,
            }
        );
    }

    #[test]
    fn a_pass_that_empties_the_backlog_schedules_nothing() {
        setup();
        PASS_SCHEDULE.with(|schedule| schedule.set(PassSchedule::default()));
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);

        run_pass(0, SECOND_NS);

        assert_eq!(PASS_SCHEDULE.with(Cell::get).due_at_ns, None);
        assert_eq!(still_in_backlog(SECOND_NS), 0);
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
