//! Send content-free wake-ups for backlog notifications, one per registered browser.
//! A pass queues each notification for the browser before posting its wake-up, and
//! spawns the posts rather than awaiting them. Nothing is retried; every outcome is counted.

use crate::admission_queue::{Admission, Taken};
use crate::notifications::backlog::app::PendingNotification;
use crate::notifications::backlog::identity::PendingIdentityNotification;
use crate::notifications::browser_inbox;
use crate::notifications::installed_app::{NotificationApp, NotificationAppId, NotificationAppRef};
use crate::notifications::webpush::vapid_key;
use crate::notifications::webpush::{
    clear_gone_subscription, payload, vapid_jwt, PayloadIkm, PayloadKeyId,
};
use crate::notifications::{notifications_enabled, BROWSER_GONE_AFTER_NS};
use crate::single_flight_cache::{self, Cached};
use crate::state::{self, storage_borrow_mut};
use crate::storage::anchor::{Browser, InboxNotification, WebPushSubscription};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::Storage;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use ic_stable_structures::Memory;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, InboxNotificationId, NotificationEvent, NotificationId, Timestamp,
    Urgency,
};
use std::cell::{Cell, RefCell};
use std::convert::Infallible;
use std::time::Duration;

const SECOND_NS: u64 = 1_000_000_000;

pub(crate) const INTERVAL: Duration = Duration::from_secs(1);

/// Wake-ups one pass posts. Only a pass's last recipient may take it over.
pub(crate) const MAX_POSTS_PER_PASS: usize = 65;

/// Notifications one pass takes, however few wake-ups they turn into.
pub(crate) const MAX_TAKEN_PER_PASS: usize = 200;

/// Wake-ups of Internet Identity's own a pass posts before the app backlog gets the rest
/// of [`MAX_POSTS_PER_PASS`].
///
/// Half, so a flood of them can never take more than it leaves. A ceiling rather than a
/// ratio of turns: a ratio only binds while both queues are busy, and the only way this
/// one is busy is the flood the queue exists to contain, so a ratio favouring it would
/// hand an attacker most of the budget by design. Going first with a cap gives the
/// latency a ratio was wanted for while the queue is nearly empty, and the opposite
/// behaviour under load.
///
/// Measured against the pass rate rather than [`MAX_WAKEUP_OUTCALLS`]: that one is a
/// stall backstop sized at two passes' worth, so splitting it would split the backstop.
pub(crate) const MAX_IDENTITY_POSTS_PER_PASS: usize = MAX_POSTS_PER_PASS / 2;

/// Wake-up posts allowed in flight at once.
///
/// A pass spawns its posts rather than awaiting them, so without this the number in
/// flight is bounded by how long a relay takes rather than by the pass budget: at
/// [`MAX_POSTS_PER_PASS`] a second and a ~60s outcall timeout, a stalled relay would
/// put thousands of outcalls in flight.
///
/// Two passes' worth, because the budget a pass finds free is what trims it, and it
/// trims in whole passes: a budget of one pass would halve the rate the moment relays
/// hold a post past a single [`INTERVAL`], which is a large step for a small change
/// in a relay. At two the first step waits until they hold one past two intervals,
/// and takes the rate to a third rather than to a half. A healthy relay is nowhere
/// near either — 65 posts a second each lasting 200ms is 13 in flight.
const MAX_WAKEUP_OUTCALLS: usize = 2 * MAX_POSTS_PER_PASS;

thread_local! {
    /// The 90s reclaim age is above the ~60s outcall timeout, so a live call is never
    /// reclaimed early.
    static WAKEUP_OUTCALL_LIMIT: RefCell<crate::concurrency::ConcurrencyLimiter> =
        RefCell::new(crate::concurrency::ConcurrencyLimiter::new(
            crate::concurrency::LimiterConfig {
                max_concurrent: MAX_WAKEUP_OUTCALLS,
                max_age_secs: 90,
            },
        ));
}

/// How many more wake-ups may be posted before the budget is full.
fn post_capacity() -> usize {
    crate::concurrency::num_available_slots(&WAKEUP_OUTCALL_LIMIT)
}

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

/// Which registration a wake-up was posted to, carried with the post because the entry may
/// point somewhere else by the time an answer comes back.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DeliveryTarget {
    /// The entry's own registration.
    Browser,
    /// The app record the entry routes through, shared with every identity pointing at it.
    App(NotificationAppId),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Delivery {
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) browser_id: BrowserId,
    /// Which registration the endpoint below belongs to.
    pub(crate) target: DeliveryTarget,
    /// The browser queue entry this wake-up announces, or `None` where it announces
    /// nothing new: a second wake-up for an entry the service worker already has.
    pub(crate) notification: Option<InboxNotification>,
    pub(crate) endpoint: String,
    pub(crate) vapid_public_key: Vec<u8>,
    /// The sealed payload naming the entry above, absent where the wake-up names none.
    pub(crate) body: Option<Vec<u8>>,
    pub(crate) jwt: String,
    pub(crate) urgency: Urgency,
    /// Relay retention time, capped by the notification's remaining lifetime.
    pub(crate) ttl_seconds: u64,
}

thread_local! {
    /// One JWT per relay, signed ahead of the one in hand lapsing.
    static VAPID_JWTS: std::cell::RefCell<vapid_key::JwtCache> =
        std::cell::RefCell::new(vapid_key::new_jwt_cache());
}

/// The JWT a relay is woken with, where one is in hand. A relay we hold none for starts a
/// signature and answers nothing: those notifications keep their place and the next pass
/// asks again.
fn jwt_for(relay_origin: String) -> Option<String> {
    match single_flight_cache::get(&VAPID_JWTS, relay_origin.clone()) {
        Cached::Ready(jwt) => Some(jwt),
        Cached::Pending => {
            // Off-wasm nothing drives a detached fill, so a test drives it here and reads
            // the answer in the same pass rather than arranging a second one.
            #[cfg(test)]
            {
                single_flight_cache::run_detached();
                if let Cached::Ready(jwt) = single_flight_cache::get(&VAPID_JWTS, relay_origin) {
                    return Some(jwt);
                }
            }
            None
        }
    }
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
    // Internet Identity's own wake-ups go first, capped at half the pass, and the app
    // backlog plans against whatever is left of the budget.
    let identity = plan_identity_pass(MAX_IDENTITY_POSTS_PER_PASS, now_ns);
    let app = plan_pass(MAX_POSTS_PER_PASS.saturating_sub(identity.len()), now_ns);
    for delivery in identity.into_iter().chain(app) {
        outcalls::spawn_post(delivery);
    }
    let work_left =
        state::notification_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total > 0)
            || state::identity_backlog_mut(now_ns, |backlog| {
                backlog.stats(now_ns).stored_total > 0
            });
    if work_left {
        schedule_pass(now_ns);
    }
}

/// Take Internet Identity's own wake-ups in backlog order, expanding each into one post
/// per entry it names. Taken one at a time, so a pass stops at its budget.
///
/// Counts items rather than the posts they expand into, exactly as the app half does.
/// The real gate on over-committing is the permit a post has to acquire before it leaves,
/// which settles what it cannot get as rejected.
pub(crate) fn plan_identity_pass(max_posts: usize, now_ns: Timestamp) -> Vec<Delivery> {
    if !notifications_enabled() {
        return Vec::new();
    }
    let budget = max_posts.min(post_capacity());

    let mut deliveries = Vec::new();
    for _ in 0..MAX_TAKEN_PER_PASS {
        if deliveries.len() >= budget {
            break;
        }
        let taken = state::identity_backlog_mut(now_ns, |backlog| {
            backlog.take_batch(1, now_ns, |batch| Ok::<_, Infallible>(batch.to_vec()))
        })
        .unwrap_or_default();
        let Some(taken) = taken.first() else {
            break;
        };
        fan_out_to_destinations(&taken.entry.item, now_ns, &mut deliveries);
    }
    deliveries
}

/// One wake-up per entry the item names, at the address its kind reaches that entry by.
/// An absent destination is every entry of the identity.
fn fan_out_to_destinations(
    pending: &PendingIdentityNotification,
    now_ns: Timestamp,
    deliveries: &mut Vec<Delivery>,
) {
    // Every push names the key its subscription was made with, so without ours there is
    // nothing to post. Asked for once; the next pass finds it.
    let Some(vapid_public_key) = vapid_key::public_key() else {
        vapid_key::ensure_public_key();
        return;
    };
    storage_borrow_mut(|storage| {
        let Ok(mut anchor) = storage.read(pending.anchor_number) else {
            return;
        };
        let destinations: Vec<BrowserId> = match &pending.destination {
            Some(named) => named.clone(),
            None => anchor.browsers().iter().map(|browser| browser.id).collect(),
        };
        let expires_at_ns = now_ns.saturating_add(IDENTITY_NOTIFICATION_TTL_NS);
        let mut planned = Vec::new();
        for browser_id in destinations {
            let Some(browser) = anchor
                .browsers()
                .iter()
                .find(|browser| browser.id == browser_id)
            else {
                continue;
            };
            let app = routed_app(storage, browser);
            let Some((subscription, jwt, target)) = wake_up_address(browser, app.as_ref(), now_ns)
            else {
                continue;
            };
            let queued = InboxNotification {
                // Named by `deposit`, which is what draws from the inbox's own run of ids.
                inbox_notification_id: 0,
                application_number: None,
                // Ours, so the worker signs to us for it rather than fetching it from an app.
                sender: crate::storage::canister_id(),
                notification_id: pending.notification_id,
                expires_at_ns,
                account_number: None,
                event: Some(pending.event.clone()),
            };
            planned.push((
                browser_id,
                queued,
                Delivery {
                    anchor_number: pending.anchor_number,
                    browser_id,
                    target,
                    notification: None,
                    endpoint: subscription.endpoint.as_str().to_string(),
                    vapid_public_key: vapid_public_key.clone(),
                    body: None,
                    jwt,
                    urgency: pending.urgency.clone(),
                    ttl_seconds: IDENTITY_NOTIFICATION_TTL_NS / SECOND_NS,
                },
                subscription.payload_ikm.clone(),
                subscription.payload_keyid.clone(),
            ));
        }

        let mut wrote = false;
        for (browser_id, queued, mut delivery, payload_ikm, payload_keyid) in planned {
            let Some((queue, next_id)) = anchor.inbox_for_browser_mut(browser_id) else {
                continue;
            };
            wrote = true;
            let deposited = browser_inbox::deposit(queue, next_id, queued, now_ns);
            delivery.body = sealed_for(
                &payload_ikm,
                &payload_keyid,
                delivery.anchor_number,
                browser_id,
                &deposited,
            );
            delivery.notification = Some(deposited);
            deliveries.push(delivery);
        }
        if wrote {
            if let Err(err) = storage.write(anchor) {
                ic_cdk::println!("Failed to queue what Internet Identity has to say: {err}");
            }
        }
    });
}

/// Take notifications in backlog order and queue each for its recipient's browsers,
/// returning the wake-ups to post. Taken one at a time, so a pass stops at its budget,
/// and never more than [`MAX_TAKEN_PER_PASS`], so ones that wake nothing cannot run on.
pub(crate) fn plan_pass(max_posts: usize, now_ns: Timestamp) -> Vec<Delivery> {
    if !notifications_enabled() {
        return Vec::new();
    }

    // Planning past the outcall budget would queue entries for browsers whose wake-up
    // is then refused a slot, so the pass takes on no more than it can post.
    let budget = max_posts.min(post_capacity());

    let mut deliveries = Vec::new();
    for _ in 0..MAX_TAKEN_PER_PASS {
        if deliveries.len() >= budget {
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
///
/// Queued anywhere, the app has notified the identity, which is what its list of apps
/// shows as last notified: from here on only that browser decides whether it is shown.
fn fan_out_to_browsers(
    taken: &Taken<StorableOriginSha256, PendingNotification>,
    now_ns: Timestamp,
    out: &mut Vec<Delivery>,
) {
    // Every push names the key its subscription was made with, so without ours there is
    // nothing to post. Asked for once; the next pass finds it.
    let Some(vapid_public_key) = vapid_key::public_key() else {
        vapid_key::ensure_public_key();
        return;
    };
    let pending = &taken.entry.item;
    let queued = InboxNotification {
        // Named by `deposit`, once per browser this is deposited for.
        inbox_notification_id: 0,
        application_number: Some(pending.application_number),
        event: None,
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
        let apps: Vec<Option<NotificationApp>> = anchor
            .browsers()
            .iter()
            .map(|browser| routed_app(storage, browser))
            .collect();
        // The sealing keys travel with each wake-up: the deposit that names its entry
        // happens below, after this borrow of the browsers has ended.
        let woken: Vec<(Delivery, PayloadIkm, PayloadKeyId)> = anchor
            .browsers()
            .iter()
            .zip(apps.iter())
            .filter_map(|(browser, app)| {
                let (subscription, jwt, target) = wake_up_address(browser, app.as_ref(), now_ns)?;
                let payload_ikm = subscription.payload_ikm.clone();
                let payload_keyid = subscription.payload_keyid.clone();
                Some((
                    Delivery {
                        anchor_number: pending.anchor_number,
                        browser_id: browser.id,
                        target,
                        // Filled in below, where the deposit names the entry for this browser.
                        notification: None,
                        endpoint: subscription.endpoint.as_str().to_string(),
                        vapid_public_key: vapid_public_key.clone(),
                        body: None,
                        jwt,
                        urgency: pending.urgency.clone(),
                        ttl_seconds,
                    },
                    payload_ikm,
                    payload_keyid,
                ))
            })
            .collect();
        if woken.is_empty() {
            return;
        }
        let mut to_post = Vec::new();
        let mut queued_anywhere = false;
        for (mut delivery, payload_ikm, payload_keyid) in woken {
            let Some((queue, next_id)) = anchor.inbox_for_browser_mut(delivery.browser_id) else {
                continue;
            };
            queued_anywhere = true;
            let deposited = browser_inbox::deposit(queue, next_id, queued.clone(), now_ns);
            delivery.body = sealed_for(
                &payload_ikm,
                &payload_keyid,
                delivery.anchor_number,
                delivery.browser_id,
                &deposited,
            );
            delivery.notification = Some(deposited);
            to_post.push(delivery);
        }
        match storage.write(anchor) {
            Ok(()) => {
                out.extend(to_post);
                if queued_anywhere {
                    storage.record_notified(
                        pending.anchor_number,
                        pending.application_number,
                        now_ns,
                    );
                }
            }
            Err(err) => ic_cdk::println!("Failed to queue a notification for its browsers: {err}"),
        }
    });
}

/// It says something just happened, so one reaching a device a day later says nothing true.
const IDENTITY_NOTIFICATION_TTL_NS: u64 = 60 * 60 * SECOND_NS;

/// Submits one of Internet Identity's own wake-ups for one entry. Silent where the entry
/// cannot be woken or the identity's own share of the backlog is full.
///
/// What the entry is given to show is written when the pass expands this, as it is for
/// an app's notifications. A probe's own record of having been asked is written here,
/// because that is what the caller is answered with.
pub(crate) fn notify_identity(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    event: NotificationEvent,
    now_ns: Timestamp,
) -> Option<NotificationId> {
    if !notifications_enabled() {
        return None;
    }
    storage_borrow_mut(|storage| {
        let anchor = storage.read(anchor_number).ok()?;
        let browser = anchor
            .browsers()
            .iter()
            .find(|browser| browser.id == browser_id)?
            .clone();
        // Read for the refusal alone: an entry with no address has nothing to be told, so
        // nothing is queued for it. The address the post uses is resolved again when the
        // pass expands this.
        let app = routed_app(storage, &browser);
        wake_up_address(&browser, app.as_ref(), now_ns)?;

        // Asked before the event is handed over, which is the whole point of reading it
        // off the event rather than off a second argument.
        let proves_the_app_is_reachable = event.proves_the_app_is_reachable();
        let notification_id = storage.allocate_identity_notification_id();
        if !submit_identity_wake_up(
            PendingIdentityNotification {
                anchor_number,
                destination: Some(vec![browser_id]),
                notification_id,
                event,
                urgency: Urgency::Normal,
                expires_at_ns: Some(now_ns.saturating_add(IDENTITY_NOTIFICATION_TTL_NS)),
            },
            now_ns,
        ) {
            return None;
        }

        // The ask goes on the record the answer lands on, now rather than when the pass
        // posts it: a second ask inside the window is handed this one back instead of
        // sending another to the same address.
        if proves_the_app_is_reachable {
            storage.set_notification_app_probe(app?.id, notification_id, now_ns);
        }
        Some(notification_id)
    })
}

/// Puts one of Internet Identity's own wake-ups in the backlog, answering whether it was
/// admitted. Identities take turns there, and each has a quota per kind, which is what
/// stops one of them taking the outcall budget from the rest.
fn submit_identity_wake_up(pending: PendingIdentityNotification, now_ns: Timestamp) -> bool {
    let anchor_number = pending.anchor_number;
    let admitted = state::identity_backlog_mut(now_ns, |backlog| {
        backlog.admit(anchor_number, vec![pending], now_ns)
    });
    let accepted = admitted
        .iter()
        .any(|answer| matches!(answer.admission, Admission::Accepted | Admission::Folded));
    if accepted {
        schedule_pass(now_ns);
    }
    accepted
}

/// A browser that registered an address of its own is woken at it. Otherwise the app it
/// points at is, as long as this identity has it enabled and is still signed in here.
fn wake_up_address<'a>(
    browser: &'a Browser,
    app: Option<&'a NotificationApp>,
    now_ns: Timestamp,
) -> Option<(&'a WebPushSubscription, String, DeliveryTarget)> {
    // Only the flag is read here, never the session count: an entry that is merely signed
    // out is still an entry the user left switched on. An install that has not completed
    // has no app to reach and no flag to read.
    match browser.notification_app {
        Some(NotificationAppRef::Pending { .. }) => return None,
        Some(NotificationAppRef::Linked { enabled: false, .. }) => return None,
        Some(NotificationAppRef::Linked { .. }) | None => {}
    }
    let (subscription, target) = match browser.webpush_subscription.as_ref() {
        Some(own) => (own, DeliveryTarget::Browser),
        None => {
            let app = app?;
            (
                app.webpush_subscription.as_ref()?,
                DeliveryTarget::App(app.id),
            )
        }
    };
    let (subscription, jwt) = authorized_wake_up(browser, subscription, now_ns)?;
    Some((subscription, jwt, target))
}

/// Liveness is the entry's, whichever address this is: the app has none of its own.
fn authorized_wake_up<'a>(
    browser: &Browser,
    subscription: &'a WebPushSubscription,
    now_ns: Timestamp,
) -> Option<(&'a WebPushSubscription, String)> {
    if now_ns.saturating_sub(browser.last_used) >= BROWSER_GONE_AFTER_NS {
        return None;
    }
    let relay_origin = vapid_jwt::relay_origin_of(subscription.endpoint.as_str())?;
    // Nothing waits on a signature. A relay we hold no JWT for leaves its notifications
    // where they are and the next pass asks again, by which time the fill this started
    // has landed.
    let jwt = jwt_for(relay_origin)?;
    Some((subscription, jwt))
}

/// The app this entry routes through, where it has one and wants it. A reference alone is
/// not routing: it survives signing out and switching delivery off.
pub(crate) fn routed_app<M: Memory + Clone>(
    storage: &Storage<M>,
    browser: &Browser,
) -> Option<NotificationApp> {
    let Some(NotificationAppRef::Linked { app_id, enabled }) = browser.notification_app else {
        return None;
    };
    if !enabled || browser.session_count == 0 {
        return None;
    }
    storage.notification_app(app_id)
}

/// Whether anything could be woken for this entry right now.
pub(crate) fn can_wake<M: Memory + Clone>(
    storage: &Storage<M>,
    browser: &Browser,
    now_ns: Timestamp,
) -> bool {
    let app = routed_app(storage, browser);
    wake_up_address(browser, app.as_ref(), now_ns).is_some()
}

/// Count the outcome and keep the browser's queue in step with the wake-ups on their
/// way: a relay that lost the subscription takes the registration and the queue with
/// it, and any other failure removes an entry, since no wake-up is coming for it.
pub(crate) fn settle(delivery: &Delivery, outcome: PostOutcome) {
    record_post_outcome(outcome);
    match outcome {
        PostOutcome::Sent => {}
        PostOutcome::Gone => {
            if let Err(err) = clear_gone_subscription(
                delivery.anchor_number,
                delivery.browser_id,
                delivery.target,
                &delivery.endpoint,
            ) {
                ic_cdk::println!("Failed to drop a push registration the relay called gone: {err}");
            }
        }
        PostOutcome::RateLimited | PostOutcome::RelayError | PostOutcome::Rejected => {
            // A wake-up that announced nothing new leaves the queue alone: the entry it
            // was for is one the service worker has already been told about.
            let Some(notification) = delivery.notification.as_ref() else {
                return;
            };
            browser_inbox::remove_after_failed_wake_up(
                delivery.anchor_number,
                delivery.browser_id,
                delivery.target,
                notification,
                &delivery.endpoint,
            );
        }
    }
}

/// What the service worker is handed: the identity and inbox the entry sits in, and the
/// name that inbox gave it. Enough to ask for that notification and no other.
#[derive(serde::Serialize)]
struct WakeUpPayload {
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    inbox_notification_id: InboxNotificationId,
}

/// Seals what names `deposited`. `None` where it could not be sealed, which leaves a
/// wake-up carrying nothing rather than one carrying something the browser will not open.
fn sealed_for(
    payload_ikm: &PayloadIkm,
    payload_keyid: &PayloadKeyId,
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    deposited: &InboxNotification,
) -> Option<Vec<u8>> {
    let named = serde_json::to_vec(&WakeUpPayload {
        anchor_number,
        browser_id,
        inbox_notification_id: deposited.inbox_notification_id,
    })
    .inspect_err(|err| ic_cdk::println!("ERROR: could not write a wake-up's payload: {err}"))
    .ok()?;
    payload::seal(
        payload_ikm,
        payload_keyid,
        payload::salt_for(anchor_number, browser_id, deposited.inbox_notification_id),
        &named,
    )
    .inspect_err(|err| ic_cdk::println!("ERROR: could not seal a wake-up's payload: {err}"))
    .ok()
}

/// Build retention, urgency, and VAPID authorization headers.
pub(crate) fn push_headers(delivery: &Delivery) -> Vec<(String, String)> {
    let mut headers = vec![
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
    ];
    if delivery.body.is_some() {
        headers.push((
            "Content-Encoding".to_string(),
            payload::CONTENT_ENCODING.to_string(),
        ));
    }
    headers
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
        // A pass plans within the budget and nothing runs between planning and here,
        // so a refusal means the last recipient's browsers took the budget over. Its
        // wake-up never leaves, which is what `Rejected` already stands for.
        let Some(permit) = crate::concurrency::acquire(&WAKEUP_OUTCALL_LIMIT) else {
            settle(&delivery, PostOutcome::Rejected);
            return;
        };
        ic_cdk::spawn(async move {
            let _permit = permit;
            let outcome = post_wake_up(&delivery).await;
            settle(&delivery, outcome);
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
            body: delivery.body.clone(),
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
    use crate::notifications::backlog::app::APP_BACKLOG;
    use crate::notifications::backlog::identity::IDENTITY_BACKLOG;
    use crate::notifications::webpush::fixtures::subscription_at;
    use crate::notifications::webpush::fixtures::{
        anchor, anchor_with_browsers, app_for_key, app_key, setup, stored_subscription, subscribe,
        APP_RELAY, OTHER_APP_RELAY,
    };
    use crate::state::storage_borrow;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, ApplicationNumber, BrowserId, IdentityForNotification,
    };
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const MINUTE_NS: u64 = 60 * SECOND_NS;
    const RELAY: &str = "https://relay.example/wpush/abc";
    const ROTATED: &str = "https://relay.example/wpush/def";

    fn sender() -> Principal {
        Principal::from_slice(&[7; 10])
    }

    fn link_principal() -> Principal {
        Principal::from_slice(&[11; 10])
    }

    fn submit(anchor_number: AnchorNumber, notification_id: u64, expires_at_ns: Timestamp) {
        submit_at(anchor_number, notification_id, expires_at_ns, 0);
    }

    /// For a test whose clock has moved: the backlog drops what it has held too long,
    /// so a notification admitted at zero is gone by the time such a test looks.
    fn submit_at(
        anchor_number: AnchorNumber,
        notification_id: u64,
        expires_at_ns: Timestamp,
        now_ns: Timestamp,
    ) {
        submit_for(anchor_number, 1, notification_id, expires_at_ns, now_ns);
    }

    /// For a test that looks at the app the notification comes from, which has to be one
    /// II holds.
    fn submit_for(
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        notification_id: u64,
        expires_at_ns: Timestamp,
        now_ns: Timestamp,
    ) {
        state::notification_backlog_mut(now_ns, |backlog| {
            backlog.admit(
                StorableOriginSha256::from_origin(&APP.to_string()),
                vec![PendingNotification {
                    recipient: Principal::from_slice(&anchor_number.to_be_bytes()),
                    anchor_number,
                    application_number,
                    account_number: Some(2),
                    sender: sender(),
                    notification_id,
                    urgency: Urgency::Normal,
                    expires_at_ns: Some(expires_at_ns),
                }],
                now_ns,
            );
        });
    }

    fn still_in_backlog(now_ns: Timestamp) -> usize {
        state::notification_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total)
    }

    fn queued(anchor_number: AnchorNumber, browser_id: BrowserId) -> Vec<u64> {
        anchor(anchor_number)
            .notifications_for_browser(browser_id)
            .iter()
            .map(|queued| queued.notification_id)
            .collect()
    }

    fn change_queue(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        change: impl FnOnce(&mut Vec<InboxNotification>),
    ) {
        let mut stored = anchor(anchor_number);
        change(
            stored
                .notifications_for_browser_mut(browser_id)
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

    /// Points `browser_id` at the app record the key `seed` names, creating and registering
    /// it where needed.
    fn route_through_an_app(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
        seed: u8,
        endpoint: Option<&str>,
    ) -> NotificationAppId {
        use std::collections::BTreeMap;

        // Outside the borrow below: it takes one of its own.
        let app = app_for_key(app_key(seed), 0);
        storage_borrow_mut(|storage| {
            if let Some(endpoint) = endpoint {
                storage
                    .set_notification_app_subscription(
                        app.id,
                        Some(subscription_at(endpoint, 0)),
                        0,
                    )
                    .expect("the record is there");
            }
            let mut stored = storage.read(anchor_number).expect("reading the anchor");
            stored.point_at_notification_app(browser_id, app.id);
            stored.move_browser_session_counts(&BTreeMap::from([(browser_id, 1)]));
            storage.write(stored).expect("pointing at the app");
            app.id
        })
    }

    fn stored_app(app_id: NotificationAppId) -> Option<NotificationAppId> {
        storage_borrow(|storage| storage.notification_app(app_id)).map(|app| app.id)
    }

    /// An identity with `count` browsers, every one registered with the relay.
    fn subscribed_recipient(count: u8) -> (AnchorNumber, Vec<BrowserId>) {
        let (recipient, browsers) = anchor_with_browsers(count);
        for browser in &browsers {
            subscribe(recipient, *browser, RELAY, 0);
        }
        (recipient, browsers)
    }

    fn identity_backlog_len(now_ns: Timestamp) -> usize {
        state::identity_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total)
    }

    /// The one notification Internet Identity sends, which is also what proves a Home
    /// Screen app is still installed.
    fn notify(anchor_number: AnchorNumber, browser_id: BrowserId) -> Option<NotificationId> {
        notify_identity(
            anchor_number,
            browser_id,
            NotificationEvent::NotificationsOn(IdentityForNotification {
                anchor_number,
                name: None,
            }),
            SECOND_NS,
        )
    }

    /// A probe asks the record to prove it is installed, so it goes to the record's
    /// address and there is nowhere else to send it. A browser pointing at a record has
    /// no address of its own, and news has nowhere to go either.
    #[test]
    fn a_probe_at_a_record_with_no_address_sends_nothing() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        let browser = browsers.first().copied().expect("one browser");
        route_through_an_app(recipient, browser, 9, None);

        assert_eq!(notify(recipient, browser), None);
        assert!(queued(recipient, browser).is_empty());
        assert!(notify(recipient, browser).is_none());
    }

    #[test]
    fn a_probe_through_a_switched_off_app_sends_nothing() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        let mut stored = anchor(recipient);
        stored.disable_notification_app(browsers[0]);
        storage_borrow_mut(|storage| storage.write(stored)).expect("switching off");

        assert_eq!(notify(recipient, browsers[0]), None);
        assert!(queued(recipient, browsers[0]).is_empty());
    }

    #[test]
    fn an_app_switched_off_is_not_woken_through_the_browser_registration() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        let mut stored = anchor(recipient);
        stored.disable_notification_app(browsers[0]);
        storage_borrow_mut(|storage| storage.write(stored)).expect("switching off");

        submit(recipient, 7, 10 * SECOND_NS);

        assert!(
            plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty(),
            "a wake-up was planned for an identity that switched delivery off"
        );
        assert!(queued(recipient, browsers[0]).is_empty());
    }

    #[test]
    fn an_install_that_has_not_completed_is_not_woken() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        let mut stored = anchor(recipient);
        stored.start_notification_app_link(browsers[0], link_principal(), 10 * SECOND_NS);
        storage_borrow_mut(|storage| storage.write(stored)).expect("starting the install");

        submit(recipient, 7, 10 * SECOND_NS);

        assert!(
            plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty(),
            "a wake-up was planned for an entry whose install has not completed"
        );
        assert!(queued(recipient, browsers[0]).is_empty());
    }

    #[test]
    fn an_install_that_has_not_completed_routes_through_no_app() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        let mut stored = anchor(recipient);
        stored.start_notification_app_link(browsers[0], link_principal(), 10 * SECOND_NS);
        storage_borrow_mut(|storage| storage.write(stored)).expect("starting a second install");

        let stored = anchor(recipient);
        let browser = stored
            .browsers()
            .iter()
            .find(|browser| browser.id == browsers[0])
            .expect("a registered browser");

        assert_eq!(storage_borrow(|storage| routed_app(storage, browser)), None);
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

    /// The queue is the ration: an identity holds only so many wake-ups of one kind, so
    /// a worker asking in a loop fills its own quota and takes no turns from anyone else.
    #[test]
    fn a_browser_asking_in_a_loop_fills_its_own_quota_and_no_more() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        let (other, other_browsers) = subscribed_recipient(1);
        // Far enough out that the deadline does not ration the lane on its own: what is
        // under test is the identity's quota.
        submit(recipient, 7, 20 * MINUTE_NS);
        submit(other, 7, 20 * MINUTE_NS);
        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        let quota = IDENTITY_BACKLOG.max_pending_per_group;
        for _ in 0..quota + 5 {
            notify(recipient, browsers[0]);
        }

        assert_eq!(
            identity_backlog_len(SECOND_NS),
            quota,
            "the loop is held at the identity's own quota"
        );
        notify(other, other_browsers[0]);
        assert_eq!(
            identity_backlog_len(SECOND_NS),
            quota + 1,
            "another identity still gets in"
        );
    }

    /// Both queues are drained by one pass, and the identity half can take no more than
    /// half of it however much is waiting.
    #[test]
    fn a_flood_of_identity_wake_ups_leaves_the_app_backlog_its_share() {
        setup();
        let mut identities = Vec::new();
        for _ in 0..8 {
            let (recipient, browsers) = subscribed_recipient(1);
            submit(recipient, 7, 20 * MINUTE_NS);
            identities.push((recipient, browsers[0]));
        }
        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        for (recipient, browser_id) in &identities {
            for _ in 0..IDENTITY_BACKLOG.max_pending_per_group {
                notify(*recipient, *browser_id);
            }
        }

        let posted = plan_identity_pass(MAX_IDENTITY_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(posted.len(), MAX_IDENTITY_POSTS_PER_PASS);
        assert!(MAX_POSTS_PER_PASS - posted.len() >= MAX_POSTS_PER_PASS / 2);
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

    /// The slots a real pass's posts would be holding, so a test can put the outcall
    /// budget under pressure without an outcall.
    fn hold_outcall_slots(count: usize) -> Vec<crate::concurrency::Permit> {
        (0..count)
            .map(|_| crate::concurrency::acquire(&WAKEUP_OUTCALL_LIMIT).expect("a free slot"))
            .collect()
    }

    #[test]
    fn a_pass_plans_nothing_while_the_outcall_budget_is_full() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);

        let held = hold_outcall_slots(MAX_WAKEUP_OUTCALLS);
        assert!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());
        assert_eq!(
            still_in_backlog(SECOND_NS),
            1,
            "it keeps its place rather than being queued for a wake-up with no slot to go out on"
        );

        drop(held);
        assert_eq!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).len(), 1);
    }

    #[test]
    fn a_pass_plans_no_more_than_the_outcall_budget_leaves() {
        setup();
        // Spread over recipients to stay within the per-recipient cap.
        for _ in 0..4 {
            let (recipient, _) = subscribed_recipient(1);
            for notification_id in 0..20 {
                submit(recipient, notification_id, 10 * SECOND_NS);
            }
        }

        let _held = hold_outcall_slots(MAX_WAKEUP_OUTCALLS - 10);

        assert_eq!(plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).len(), 10);
        assert_eq!(still_in_backlog(SECOND_NS), 80 - 10);
    }

    /// A full budget decides when a wake-up is posted, never whether it was asked for:
    /// it waits in the backlog instead of being lost.
    #[test]
    fn a_wake_up_waits_in_the_backlog_while_the_outcall_budget_is_full() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        let held = hold_outcall_slots(MAX_WAKEUP_OUTCALLS);
        notify(recipient, browsers[0]);
        assert!(plan_identity_pass(MAX_IDENTITY_POSTS_PER_PASS, SECOND_NS).is_empty());

        drop(held);
        assert_eq!(
            plan_identity_pass(MAX_IDENTITY_POSTS_PER_PASS, SECOND_NS).len(),
            1,
            "the wake-up is still there once there is room"
        );
    }

    #[test]
    fn a_pass_stops_taking_at_its_bound_when_nothing_is_woken() {
        setup();
        let per_recipient = APP_BACKLOG.max_pending_per_group as u64;
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
    fn a_full_browser_inbox_drops_its_oldest_and_still_earns_a_wake_up() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        change_queue(recipient, browsers[0], |queue| {
            queue.extend((0..browser_inbox::CAPACITY as u64).map(|notification_id| {
                InboxNotification {
                    inbox_notification_id: notification_id,
                    application_number: Some(1),
                    sender: sender(),
                    notification_id,
                    expires_at_ns: 10 * SECOND_NS,
                    account_number: Some(2),
                    event: None,
                }
            }))
        });
        submit(recipient, 1_000, 10 * SECOND_NS);

        // A wake-up names the entry it was posted for, so the one this deposit displaced
        // cannot stand in for it and the new entry needs a wake-up of its own.
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        assert_eq!(deliveries.len(), 1);
        assert_eq!(
            deliveries[0]
                .notification
                .as_ref()
                .map(|one| one.notification_id),
            Some(1_000)
        );

        let left = queued(recipient, browsers[0]);
        assert_eq!(left.len(), browser_inbox::CAPACITY);
        assert_eq!(left.last(), Some(&1_000));
    }

    /// `APP` allowed to notify the identity, which gives the app a config to record
    /// against. Returns the number II holds the app under.
    fn allow_app(anchor_number: AnchorNumber) -> ApplicationNumber {
        crate::notifications::write_notifications_decision(
            anchor_number,
            &crate::origin::ApplicationOrigin::try_from(APP).expect("a bare origin"),
            true,
            0,
        )
        .expect("allowing the app");
        storage_borrow(|storage| storage.lookup_application_number_with_origin(&APP.to_string()))
            .expect("allowing the app stores it")
    }

    fn last_notified(anchor_number: AnchorNumber) -> Option<Timestamp> {
        storage_borrow(|storage| {
            storage.read_anchor_application_config(anchor_number, &APP.to_string())
        })
        .and_then(|config| config.last_notified_at_ns)
    }

    #[test]
    fn queuing_for_a_browser_records_when_the_app_last_notified() {
        setup();
        let (recipient, _) = subscribed_recipient(2);
        let app = allow_app(recipient);
        submit_for(recipient, app, 7, 10 * SECOND_NS, 0);

        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(last_notified(recipient), Some(SECOND_NS));
    }

    #[test]
    fn a_notification_that_reaches_no_browser_records_nothing() {
        setup();
        let (recipient, _) = anchor_with_browsers(1);
        let app = allow_app(recipient);
        submit_for(recipient, app, 7, 10 * SECOND_NS, 0);

        plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(last_notified(recipient), None);
    }

    /// A stamp starts no config of its own: only the account-state write reclaims one.
    #[test]
    fn an_app_with_no_config_is_left_without_one() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        let app =
            storage_borrow_mut(|storage| storage.sign_in_for_testing(recipient, &APP.to_string()));
        submit_for(recipient, app, 7, 10 * SECOND_NS, 0);

        assert!(!plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).is_empty());

        assert!(storage_borrow(|storage| {
            storage.read_anchor_application_config(recipient, &APP.to_string())
        })
        .is_none());
    }

    #[test]
    fn a_pass_does_nothing_while_notifications_are_off() {
        setup();
        let (recipient, _) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        crate::state::persistent_state_mut(|s| s.notifications_enabled = None);

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
            Some(InboxNotification {
                inbox_notification_id: 0,
                application_number: Some(1),
                sender: sender(),
                notification_id: 7,
                expires_at_ns: 10 * SECOND_NS,
                account_number: Some(2),
                event: None,
            })
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

        settle(&delivery, PostOutcome::Sent);

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

            settle(&deliveries[1], outcome);

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

        settle(&deliveries[0], PostOutcome::Gone);

        assert!(stored_subscription(recipient, browsers[0]).is_none());
        assert!(queued(recipient, browsers[0]).is_empty());
        assert!(stored_subscription(recipient, browsers[1]).is_some());
        assert_eq!(queued(recipient, browsers[1]), vec![7]);
    }

    #[test]
    fn a_gone_app_endpoint_is_forgotten_though_the_browser_registered_its_own() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        let app = route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
        assert_eq!(delivery.target, DeliveryTarget::App(app));
        assert_eq!(delivery.endpoint, APP_RELAY);
        subscribe(recipient, browsers[0], RELAY, SECOND_NS);

        settle(&delivery, PostOutcome::Gone);

        assert_eq!(stored_app(app), None);
        assert!(stored_subscription(recipient, browsers[0]).is_some());
    }

    #[test]
    fn a_gone_app_endpoint_is_forgotten_though_the_browser_switched_apps() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(1);
        let (other, other_browsers) = anchor_with_browsers(1);
        let app = route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        route_through_an_app(other, other_browsers[0], 9, None);
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
        route_through_an_app(recipient, browsers[0], 8, Some(OTHER_APP_RELAY));

        settle(&delivery, PostOutcome::Gone);

        assert_eq!(stored_app(app), None);
        assert_eq!(anchor(other).notification_app(other_browsers[0]), None);
    }

    /// A browser has one route: pointing at a record takes the address it registered for
    /// itself, so the pass wakes it at the record's and there is no own address left for
    /// a `Gone` to come back about.
    #[test]
    fn pointing_at_an_app_takes_over_from_the_browsers_own_address() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        let browser = browsers.first().copied().expect("one browser");
        let app = route_through_an_app(recipient, browser, 9, Some(APP_RELAY));
        submit(recipient, 7, 10 * SECOND_NS);

        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);

        assert_eq!(delivery.target, DeliveryTarget::App(app));
        assert_eq!(delivery.endpoint, APP_RELAY);
        assert!(stored_subscription(recipient, browser).is_none());
    }

    #[test]
    fn one_pass_wakes_an_app_routed_entry_and_a_self_registered_one() {
        setup();
        let (recipient, browsers) = anchor_with_browsers(2);
        let app = route_through_an_app(recipient, browsers[0], 9, Some(APP_RELAY));
        subscribe(recipient, browsers[1], RELAY, 0);
        submit(recipient, 7, 10 * SECOND_NS);

        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);

        assert_eq!(
            deliveries
                .iter()
                .map(|one| (one.browser_id, one.target, one.endpoint.as_str()))
                .collect::<Vec<_>>(),
            vec![
                (browsers[0], DeliveryTarget::App(app), APP_RELAY),
                (browsers[1], DeliveryTarget::Browser, RELAY),
            ]
        );
        assert_eq!(queued(recipient, browsers[0]), vec![7]);
        assert_eq!(queued(recipient, browsers[1]), vec![7]);
    }

    #[test]
    fn a_late_gone_for_an_old_endpoint_leaves_the_new_registration() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        let delivery = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS).remove(0);
        subscribe(recipient, browsers[0], ROTATED, SECOND_NS);

        settle(&delivery, PostOutcome::Gone);

        assert_eq!(
            stored_subscription(recipient, browsers[0])
                .map(|registered| registered.endpoint.into_string()),
            Some(ROTATED.to_string())
        );
    }

    #[test]
    fn a_failed_wake_up_whose_entry_was_taken_removes_nothing() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        for notification_id in [7, 8, 9] {
            submit(recipient, notification_id, 10 * SECOND_NS);
        }
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        take_oldest(recipient, browsers[0]);

        settle(&deliveries[0], PostOutcome::RelayError);

        // 8 and 9 were each named by a wake-up of their own, so the failure of the one
        // that named 7 says nothing about them.
        assert_eq!(queued(recipient, browsers[0]), vec![8, 9]);
    }

    #[test]
    fn a_failed_wake_up_past_its_deadline_removes_nothing_more() {
        setup();
        let (recipient, browsers) = subscribed_recipient(1);
        submit(recipient, 7, 10 * SECOND_NS);
        submit(recipient, 8, 20 * SECOND_NS);
        let deliveries = plan_pass(MAX_POSTS_PER_PASS, SECOND_NS);
        take_oldest(recipient, browsers[0]);

        settle(&deliveries[0], PostOutcome::RelayError);

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

        settle(&delivery, PostOutcome::RelayError);

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
