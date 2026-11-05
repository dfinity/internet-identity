//! What becomes of each notification and of each push, counted for `/metrics`.
//!
//! A notification is counted once at each stage it reaches: received, accepted, pushed
//! (a push service took one of its pushes) and delivered (one of its devices collected
//! it). Where it stops, it is counted once as a loss at the drop after the last stage it
//! reached, with the reason. A push is one copy of a notification on its way to one
//! browser or Home Screen app, and runs the same funnel from accepted on, by delivery
//! path. Resends folded into a waiting copy, and notifications the app was told to send
//! again later, sit beside the funnel: they are counted in received when they come back.
//!
//! Everything here lives on the heap, as the backlogs do, so an upgrade starts the
//! counters from zero, which Prometheus reads as a counter reset. A push still on its
//! way at the upgrade is forgotten rather than counted.

use crate::notifications::dispatch::DeliveryTarget;
use crate::notifications::webpush::vapid_key;
use candid::Principal;
use ic_metrics_encoder::MetricsEncoder;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ApplicationNumber, BrowserId, InboxNotificationId, NotificationId, Timestamp,
    Urgency,
};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

/// Apps named on their own series; any other app is counted as `other`. The number of
/// apps has no bound, and every series is one more line on every scrape.
const MAX_NAMED_ORIGINS: usize = 100;

/// Calling canisters named on their own series, for the same reason.
const MAX_NAMED_CANISTERS: usize = 100;

const OTHER: &str = "other";

/// How long past its expiry a pushed copy is waited for before it counts as never
/// collected, so a device collecting it late is told apart from one that never did.
const COLLECTION_GRACE_NS: u64 = 10 * 60 * 1_000_000_000;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Source {
    Apps,
    InternetIdentity,
}

impl Source {
    fn as_str(self) -> &'static str {
        match self {
            Source::Apps => "apps",
            Source::InternetIdentity => "ii",
        }
    }
}

/// Who sent a notification and how urgent it was, which every series carries.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Labels {
    source: Source,
    urgency: &'static str,
    /// The app's origin, or [`OTHER`] past [`MAX_NAMED_ORIGINS`]. Internet Identity's own
    /// notifications have none.
    origin: Option<String>,
}

impl Labels {
    /// An app's notification, by the origin it was sent for.
    pub(crate) fn app(origin: &str, urgency: Option<&Urgency>) -> Self {
        Labels {
            source: Source::Apps,
            urgency: urgency_label(urgency),
            origin: Some(with(|counters| counters.origin_label(origin))),
        }
    }

    /// An app's notification, by II's number for its origin. An app is numbered when its
    /// notifications are accepted, so one numbered nowhere is past the cap.
    pub(crate) fn app_number(application_number: ApplicationNumber, urgency: &Urgency) -> Self {
        Labels {
            source: Source::Apps,
            urgency: urgency_label(Some(urgency)),
            origin: Some(with(|counters| {
                counters
                    .application_origins
                    .get(&application_number)
                    .cloned()
                    .unwrap_or_else(|| OTHER.to_string())
            })),
        }
    }

    pub(crate) fn identity(urgency: &Urgency) -> Self {
        Labels {
            source: Source::InternetIdentity,
            urgency: urgency_label(Some(urgency)),
            origin: None,
        }
    }

    fn pairs<'a>(&'a self, rest: &[(&'a str, &'a str)]) -> Vec<(&'a str, &'a str)> {
        let mut pairs = rest.to_vec();
        pairs.push(("source", self.source.as_str()));
        pairs.push(("urgency", self.urgency));
        if let Some(origin) = &self.origin {
            pairs.push(("origin", origin));
        }
        pairs
    }
}

/// An absent urgency is the one the backlog gives it.
fn urgency_label(urgency: Option<&Urgency>) -> &'static str {
    match urgency {
        Some(Urgency::High) => "high",
        Some(Urgency::Normal) | None => "normal",
        Some(Urgency::Low) => "low",
        Some(Urgency::VeryLow) => "very_low",
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Stage {
    Received,
    Accepted,
    Pushed,
    Delivered,
}

impl Stage {
    fn as_str(self) -> &'static str {
        match self {
            Stage::Received => "received",
            Stage::Accepted => "accepted",
            Stage::Pushed => "pushed",
            Stage::Delivered => "delivered",
        }
    }
}

/// Where a notification or a push stopped: between received and accepted, accepted and
/// pushed, or pushed and delivered.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum DropPoint {
    Accept,
    Push,
    Deliver,
}

impl DropPoint {
    fn as_str(self) -> &'static str {
        match self {
            DropPoint::Accept => "accept",
            DropPoint::Push => "push",
            DropPoint::Deliver => "deliver",
        }
    }
}

/// Why a notification or a push was lost.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Reason {
    /// The principal has no account at the app's origin.
    UnknownPrincipal,
    /// None of the identity's browsers was used within the window.
    DormantIdentity,
    /// Not allowed by the identity, or no browser can be woken.
    NoChannel,
    /// The calling canister is not one of the app's senders.
    SenderNotAllowed,
    /// The backlog was full, and the time the app was told to send again is past the
    /// notification's expiry.
    CapacityPastExpiry,
    /// The app was over its share, and the time it was told to send again is past the
    /// notification's expiry.
    AppLimitPastExpiry,
    /// Internet Identity's own share of its backlog was full.
    BacklogFull,
    ExpiredOnArrival,
    /// The app's own expiry, shorter than the backlog's, passed while it waited.
    ExpiredAppDeadline,
    /// It waited the backlog's whole window.
    ExpiredInBacklog,
    /// No browser could be woken when it was taken from the backlog.
    NoDevice,
    StorageError,
    /// The relay has never answered Internet Identity.
    RelayUnreachable,
    /// 404 or 410.
    RelayGone,
    /// 429.
    RelayRateLimited,
    /// Any other answer outside 2xx and 5xx.
    RelayRejected,
    /// 5xx.
    RelayServerError,
    /// The outcall ended without an answer.
    RelayNoAnswer,
    NoOutcallCapacity,
    SealFailed,
    /// Pushed, and no device collected it before it expired.
    NeverFetched,
    /// A device asked for it after it expired.
    FetchedTooLate,
    /// The address it was pushed to was replaced or dropped, which clears the inbox.
    SubscriptionReplaced,
    /// Pushed out of a full inbox by a newer entry.
    InboxFull,
    ConsentRevoked,
    /// The identity went over its browser cap and this browser was given up.
    BrowserEvicted,
}

impl Reason {
    fn as_str(self) -> &'static str {
        match self {
            Reason::UnknownPrincipal => "unknown_principal",
            Reason::DormantIdentity => "dormant_identity",
            Reason::NoChannel => "no_channel",
            Reason::SenderNotAllowed => "sender_not_allowed",
            Reason::CapacityPastExpiry => "capacity_past_expiry",
            Reason::AppLimitPastExpiry => "app_limit_past_expiry",
            Reason::BacklogFull => "backlog_full",
            Reason::ExpiredOnArrival => "expired_on_arrival",
            Reason::ExpiredAppDeadline => "expired_app_deadline",
            Reason::ExpiredInBacklog => "expired_in_backlog",
            Reason::NoDevice => "no_device",
            Reason::StorageError => "storage_error",
            Reason::RelayUnreachable => "relay_unreachable",
            Reason::RelayGone => "relay_gone",
            Reason::RelayRateLimited => "relay_rate_limited",
            Reason::RelayRejected => "relay_rejected",
            Reason::RelayServerError => "relay_server_error",
            Reason::RelayNoAnswer => "relay_no_answer",
            Reason::NoOutcallCapacity => "no_outcall_capacity",
            Reason::SealFailed => "seal_failed",
            Reason::NeverFetched => "never_fetched",
            Reason::FetchedTooLate => "fetched_too_late",
            Reason::SubscriptionReplaced => "subscription_replaced",
            Reason::InboxFull => "inbox_full",
            Reason::ConsentRevoked => "consent_revoked",
            Reason::BrowserEvicted => "browser_evicted",
        }
    }

    fn drop(self) -> DropPoint {
        match self {
            Reason::UnknownPrincipal
            | Reason::DormantIdentity
            | Reason::NoChannel
            | Reason::SenderNotAllowed
            | Reason::CapacityPastExpiry
            | Reason::AppLimitPastExpiry
            | Reason::BacklogFull => DropPoint::Accept,
            Reason::ExpiredOnArrival
            | Reason::ExpiredAppDeadline
            | Reason::ExpiredInBacklog
            | Reason::NoDevice
            | Reason::StorageError
            | Reason::RelayUnreachable
            | Reason::RelayGone
            | Reason::RelayRateLimited
            | Reason::RelayRejected
            | Reason::RelayServerError
            | Reason::RelayNoAnswer
            | Reason::NoOutcallCapacity
            | Reason::SealFailed => DropPoint::Push,
            Reason::NeverFetched
            | Reason::FetchedTooLate
            | Reason::SubscriptionReplaced
            | Reason::InboxFull
            | Reason::ConsentRevoked
            | Reason::BrowserEvicted => DropPoint::Deliver,
        }
    }
}

/// Why an app was told to send a notification again later.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum DeferCause {
    /// The backlog as a whole was full.
    Capacity,
    /// The app, a recipient of it, or one of its urgency lanes was over its share.
    AppLimit,
    /// Internet Identity has not read the app's sender list yet.
    SenderList,
}

impl DeferCause {
    fn as_str(self) -> &'static str {
        match self {
            DeferCause::Capacity => "capacity",
            DeferCause::AppLimit => "app_limit",
            DeferCause::SenderList => "sender_list",
        }
    }
}

/// The way a push reaches a device: the push service the address belongs to, or the Home
/// Screen app the browser routes through.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Path {
    /// Firebase Cloud Messaging: Chrome, and the browsers built on it.
    Fcm,
    Apple,
    Mozilla,
    Windows,
    /// The Home Screen app.
    App,
    Other,
}

impl Path {
    pub(crate) fn of(target: DeliveryTarget, endpoint: &str) -> Self {
        if let DeliveryTarget::App(_) = target {
            return Path::App;
        }
        let Some(origin) = vapid_key::relay_origin_of(endpoint) else {
            return Path::Other;
        };
        let host = origin.strip_prefix("https://").unwrap_or(&origin);
        let host = host.split_once(':').map_or(host, |(host, _)| host);
        match host {
            "fcm.googleapis.com" => Path::Fcm,
            "updates.push.services.mozilla.com" => Path::Mozilla,
            "push.apple.com" => Path::Apple,
            _ if host.ends_with(".push.apple.com") => Path::Apple,
            _ if host.ends_with(".notify.windows.com") => Path::Windows,
            _ => Path::Other,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Path::Fcm => "fcm",
            Path::Apple => "apple",
            Path::Mozilla => "mozilla",
            Path::Windows => "windows",
            Path::App => "app",
            Path::Other => "other",
        }
    }
}

/// One notification taken from a backlog and fanned out. The time it was taken tells a
/// resend under the same id apart from the notification before it.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Fanned {
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) application_number: Option<ApplicationNumber>,
    pub(crate) sender: Principal,
    pub(crate) notification_id: NotificationId,
    pub(crate) taken_at_ns: Timestamp,
}

/// One inbox entry, which is one push.
type CopyKey = (AnchorNumber, BrowserId, InboxNotificationId);

struct Tracked {
    labels: Labels,
    /// Pushes neither collected nor lost yet.
    outstanding: usize,
    pushed: bool,
    delivered: bool,
    /// The reason the last pushed copy was lost, which is the notification's if none of
    /// its copies is collected.
    deliver_loss: Option<Reason>,
}

struct PushCopy {
    notification: Fanned,
    path: Path,
    expires_at_ns: Timestamp,
    /// A push service took it.
    posted: bool,
    /// Removed before its post was answered: counted as lost for this reason once the
    /// post is answered, so the push still passes through the stage it reached.
    removed: Option<Reason>,
}

/// One push a fan-out planned for a browser.
pub(crate) struct PlannedPush {
    pub(crate) browser_id: BrowserId,
    pub(crate) path: Path,
    /// The entry it was deposited as, or `None` where its relay cannot be reached and
    /// nothing was deposited.
    pub(crate) inbox_notification_id: Option<InboxNotificationId>,
}

#[derive(Default)]
struct Counters {
    notifications: BTreeMap<(Stage, Labels), u64>,
    losses: BTreeMap<(Reason, Labels), u64>,
    pushes: BTreeMap<(Stage, Path, Labels), u64>,
    push_losses: BTreeMap<(Reason, Path, Labels), u64>,
    deferred: BTreeMap<(DeferCause, Labels), u64>,
    folded: BTreeMap<Labels, u64>,
    unlisted_senders: BTreeMap<(String, Labels), u64>,
    named_origins: BTreeSet<String>,
    application_origins: BTreeMap<ApplicationNumber, String>,
    named_canisters: BTreeSet<String>,
    tracked: BTreeMap<Fanned, Tracked>,
    copies: BTreeMap<CopyKey, PushCopy>,
}

thread_local! {
    static COUNTERS: RefCell<Counters> = RefCell::new(Counters::default());
}

fn with<R>(f: impl FnOnce(&mut Counters) -> R) -> R {
    COUNTERS.with(|counters| f(&mut counters.borrow_mut()))
}

fn bump<K: Ord>(map: &mut BTreeMap<K, u64>, key: K) {
    let count = map.entry(key).or_default();
    *count = count.saturating_add(1);
}

impl Counters {
    fn origin_label(&mut self, origin: &str) -> String {
        if self.named_origins.contains(origin) {
            return origin.to_string();
        }
        if self.named_origins.len() < MAX_NAMED_ORIGINS {
            self.named_origins.insert(origin.to_string());
            return origin.to_string();
        }
        OTHER.to_string()
    }

    fn canister_label(&mut self, canister: Principal) -> String {
        let canister = canister.to_text();
        if self.named_canisters.contains(&canister) {
            return canister;
        }
        if self.named_canisters.len() < MAX_NAMED_CANISTERS {
            self.named_canisters.insert(canister.clone());
            return canister;
        }
        OTHER.to_string()
    }

    fn reached(&mut self, stage: Stage, labels: &Labels) {
        bump(&mut self.notifications, (stage, labels.clone()));
    }

    fn lost(&mut self, reason: Reason, labels: &Labels) {
        bump(&mut self.losses, (reason, labels.clone()));
    }

    /// One fewer push of the notification is on its way. The last one settles the
    /// notification: delivered if any copy was collected, otherwise lost where its
    /// copies stopped.
    fn resolve(&mut self, notification: Fanned, push_loss: Option<Reason>) {
        let Some(tracked) = self.tracked.get_mut(&notification) else {
            return;
        };
        if let Some(reason) = push_loss {
            if reason.drop() == DropPoint::Deliver {
                tracked.deliver_loss = Some(reason);
            }
        }
        tracked.outstanding = tracked.outstanding.saturating_sub(1);
        if tracked.outstanding > 0 {
            return;
        }
        let Some(tracked) = self.tracked.remove(&notification) else {
            return;
        };
        if tracked.delivered {
            return;
        }
        let reason = if tracked.pushed {
            tracked.deliver_loss.unwrap_or(Reason::NeverFetched)
        } else {
            push_loss.unwrap_or(Reason::RelayNoAnswer)
        };
        self.lost(reason, &tracked.labels);
    }

    fn copy_lost(&mut self, key: CopyKey, reason: Reason) {
        let Some(copy) = self.copies.get_mut(&key) else {
            return;
        };
        if !copy.posted {
            copy.removed.get_or_insert(reason);
            return;
        }
        let Some(copy) = self.copies.remove(&key) else {
            return;
        };
        if let Some(labels) = self
            .tracked
            .get(&copy.notification)
            .map(|t| t.labels.clone())
        {
            bump(&mut self.push_losses, (reason, copy.path, labels));
        }
        self.resolve(copy.notification, Some(reason));
    }
}

/// A notification in a call the app made, before anything is decided about it.
pub(crate) fn received(labels: &Labels) {
    with(|counters| counters.reached(Stage::Received, labels));
}

/// A received notification that went into a backlog.
pub(crate) fn accepted(labels: &Labels) {
    with(|counters| counters.reached(Stage::Accepted, labels));
}

/// A notification lost before it was fanned out to any browser.
pub(crate) fn lost(reason: Reason, labels: &Labels) {
    with(|counters| counters.lost(reason, labels));
}

pub(crate) fn deferred(cause: DeferCause, labels: &Labels) {
    with(|counters| bump(&mut counters.deferred, (cause, labels.clone())));
}

pub(crate) fn folded(labels: &Labels) {
    with(|counters| bump(&mut counters.folded, labels.clone()));
}

/// A notification refused because `canister` is not one of the app's senders.
pub(crate) fn unlisted_sender(canister: Principal, labels: &Labels) {
    with(|counters| {
        let canister = counters.canister_label(canister);
        bump(&mut counters.unlisted_senders, (canister, labels.clone()));
        counters.lost(Reason::SenderNotAllowed, labels);
    });
}

/// Names the app behind `application_number`, so what it has in the backlog is counted
/// under its origin once it is taken.
pub(crate) fn number_application(application_number: ApplicationNumber, origin: &str) {
    with(|counters| {
        let label = counters.origin_label(origin);
        if label != OTHER {
            counters
                .application_origins
                .insert(application_number, label);
        }
    });
}

/// A notification taken from a backlog and deposited for the browsers in `pushes`, whose
/// copies expire at `expires_at_ns`. One deposited nowhere is lost here.
pub(crate) fn fanned_out(
    notification: Fanned,
    labels: Labels,
    pushes: Vec<PlannedPush>,
    expires_at_ns: Timestamp,
) {
    with(|counters| {
        if pushes.is_empty() {
            counters.lost(Reason::NoDevice, &labels);
            return;
        }
        let mut outstanding = 0;
        for push in pushes {
            bump(
                &mut counters.pushes,
                (Stage::Accepted, push.path, labels.clone()),
            );
            let Some(inbox_notification_id) = push.inbox_notification_id else {
                bump(
                    &mut counters.push_losses,
                    (Reason::RelayUnreachable, push.path, labels.clone()),
                );
                continue;
            };
            outstanding += 1;
            counters.copies.insert(
                (
                    notification.anchor_number,
                    push.browser_id,
                    inbox_notification_id,
                ),
                PushCopy {
                    notification,
                    path: push.path,
                    expires_at_ns,
                    posted: false,
                    removed: None,
                },
            );
        }
        if outstanding == 0 {
            counters.lost(Reason::RelayUnreachable, &labels);
            return;
        }
        counters.tracked.insert(
            notification,
            Tracked {
                labels,
                outstanding,
                pushed: false,
                delivered: false,
                deliver_loss: None,
            },
        );
    });
}

/// The answer to the post announcing one entry: `Ok` where a push service took it.
pub(crate) fn settled(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    inbox_notification_id: InboxNotificationId,
    outcome: Result<(), Reason>,
) {
    with(|counters| {
        let key = (anchor_number, browser_id, inbox_notification_id);
        let Some(copy) = counters.copies.get_mut(&key) else {
            return;
        };
        let notification = copy.notification;
        let path = copy.path;
        let Some(labels) = counters
            .tracked
            .get(&notification)
            .map(|t| t.labels.clone())
        else {
            counters.copies.remove(&key);
            return;
        };
        match outcome {
            Ok(()) => {
                copy.posted = true;
                let removed = copy.removed;
                bump(&mut counters.pushes, (Stage::Pushed, path, labels.clone()));
                if let Some(tracked) = counters.tracked.get_mut(&notification) {
                    if !tracked.pushed {
                        tracked.pushed = true;
                        counters.reached(Stage::Pushed, &labels);
                    }
                }
                if let Some(reason) = removed {
                    counters.copy_lost(key, reason);
                }
            }
            Err(reason) => {
                counters.copies.remove(&key);
                bump(&mut counters.push_losses, (reason, path, labels));
                counters.resolve(notification, Some(reason));
            }
        }
    });
}

/// A device collected the entry, which delivers the notification if no other did first.
pub(crate) fn collected(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    inbox_notification_id: InboxNotificationId,
) {
    with(|counters| {
        let Some(copy) =
            counters
                .copies
                .remove(&(anchor_number, browser_id, inbox_notification_id))
        else {
            return;
        };
        let Some(tracked) = counters.tracked.get_mut(&copy.notification) else {
            return;
        };
        let labels = tracked.labels.clone();
        let first = !tracked.delivered;
        tracked.delivered = true;
        // A wake-up can be collected before its post is answered.
        if !copy.posted {
            bump(
                &mut counters.pushes,
                (Stage::Pushed, copy.path, labels.clone()),
            );
            if !tracked.pushed {
                tracked.pushed = true;
                counters.reached(Stage::Pushed, &labels);
            }
        }
        bump(
            &mut counters.pushes,
            (Stage::Delivered, copy.path, labels.clone()),
        );
        if first {
            counters.reached(Stage::Delivered, &labels);
        }
        counters.resolve(copy.notification, None);
    });
}

/// Entries removed from one browser's inbox before a device collected them.
pub(crate) fn entries_removed(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    inbox_notification_ids: impl IntoIterator<Item = InboxNotificationId>,
    reason: Reason,
) {
    with(|counters| {
        for inbox_notification_id in inbox_notification_ids {
            counters.copy_lost((anchor_number, browser_id, inbox_notification_id), reason);
        }
    });
}

/// Everything waiting in one browser's inbox went at once.
pub(crate) fn inbox_cleared(anchor_number: AnchorNumber, browser_id: BrowserId, reason: Reason) {
    with(|counters| {
        let keys: Vec<CopyKey> = counters
            .copies
            .range((anchor_number, browser_id, 0)..=(anchor_number, browser_id, u64::MAX))
            .map(|(key, _)| *key)
            .collect();
        for key in keys {
            counters.copy_lost(key, reason);
        }
    });
}

/// Copies past their expiry and the grace after it: a pushed one was never collected,
/// and one whose post was never answered never will be.
pub(crate) fn sweep(now_ns: Timestamp) {
    with(|counters| {
        let stale: Vec<(CopyKey, bool)> = counters
            .copies
            .iter()
            .filter(|(_, copy)| copy.expires_at_ns.saturating_add(COLLECTION_GRACE_NS) <= now_ns)
            .map(|(key, copy)| (*key, copy.posted))
            .collect();
        for (key, posted) in stale {
            if posted {
                counters.copy_lost(key, Reason::NeverFetched);
                continue;
            }
            let Some(copy) = counters.copies.remove(&key) else {
                continue;
            };
            if let Some(labels) = counters
                .tracked
                .get(&copy.notification)
                .map(|t| t.labels.clone())
            {
                bump(
                    &mut counters.push_losses,
                    (Reason::RelayNoAnswer, copy.path, labels),
                );
            }
            counters.resolve(copy.notification, Some(Reason::RelayNoAnswer));
        }
    });
}

/// Notifications fanned out and not yet delivered or lost.
fn in_flight() -> usize {
    with(|counters| counters.tracked.len())
}

pub(crate) fn encode(w: &mut MetricsEncoder<Vec<u8>>, waiting: usize) -> std::io::Result<()> {
    with(|counters| {
        let mut builder = w.counter_vec(
            "internet_identity_notifications",
            "Notifications that reached each stage since the last upgrade: received, accepted, pushed (a push service took one of its pushes) and delivered (a device collected it).",
        )?;
        for ((stage, labels), count) in &counters.notifications {
            builder = builder.value(&labels.pairs(&[("stage", stage.as_str())]), *count as f64)?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notification_losses",
            "Notifications lost since the last upgrade, by the drop after the last stage they reached and the reason.",
        )?;
        for ((reason, labels), count) in &counters.losses {
            builder = builder.value(
                &labels.pairs(&[
                    ("drop", reason.drop().as_str()),
                    ("reason", reason.as_str()),
                ]),
                *count as f64,
            )?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notification_pushes",
            "Pushes, one per browser or Home Screen app a notification goes to, that reached each stage since the last upgrade, by delivery path.",
        )?;
        for ((stage, path, labels), count) in &counters.pushes {
            builder = builder.value(
                &labels.pairs(&[("stage", stage.as_str()), ("path", path.as_str())]),
                *count as f64,
            )?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notification_push_losses",
            "Pushes lost since the last upgrade, by drop, reason and delivery path.",
        )?;
        for ((reason, path, labels), count) in &counters.push_losses {
            builder = builder.value(
                &labels.pairs(&[
                    ("drop", reason.drop().as_str()),
                    ("reason", reason.as_str()),
                    ("path", path.as_str()),
                ]),
                *count as f64,
            )?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notifications_deferred",
            "Notifications the app was told to send again later since the last upgrade, by cause. They are counted in received when they come back.",
        )?;
        for ((cause, labels), count) in &counters.deferred {
            builder = builder.value(&labels.pairs(&[("cause", cause.as_str())]), *count as f64)?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notifications_folded",
            "Resends of a notification still waiting in the backlog since the last upgrade, folded into the waiting copy.",
        )?;
        for (labels, count) in &counters.folded {
            builder = builder.value(&labels.pairs(&[]), *count as f64)?;
        }
        let mut builder = w.counter_vec(
            "internet_identity_notification_unlisted_senders",
            "Notifications refused since the last upgrade because the calling canister is not one of the app's senders, by canister.",
        )?;
        for ((canister, labels), count) in &counters.unlisted_senders {
            builder = builder.value(&labels.pairs(&[("canister", canister)]), *count as f64)?;
        }
        Ok::<(), std::io::Error>(())
    })?;
    w.encode_gauge(
        "internet_identity_notifications_in_flight",
        (waiting + in_flight()) as f64,
        "Notifications accepted and not yet delivered or lost: waiting in a backlog, or on their way to a device.",
    )
}

#[cfg(test)]
pub(crate) fn reset_for_testing() {
    with(|counters| *counters = Counters::default());
}

#[cfg(test)]
pub(crate) fn notification_count_for_testing(stage: &str) -> u64 {
    with(|counters| {
        counters
            .notifications
            .iter()
            .filter(|((s, _), _)| s.as_str() == stage)
            .map(|(_, count)| *count)
            .sum()
    })
}

#[cfg(test)]
pub(crate) fn loss_count_for_testing(reason: &str) -> u64 {
    with(|counters| {
        counters
            .losses
            .iter()
            .filter(|((r, _), _)| r.as_str() == reason)
            .map(|(_, count)| *count)
            .sum()
    })
}

#[cfg(test)]
pub(crate) fn push_loss_count_for_testing(reason: &str) -> u64 {
    with(|counters| {
        counters
            .push_losses
            .iter()
            .filter(|((r, _, _), _)| r.as_str() == reason)
            .map(|(_, count)| *count)
            .sum()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const NOW: Timestamp = 1_000;

    fn fanned(notification_id: NotificationId) -> Fanned {
        Fanned {
            anchor_number: 1,
            application_number: Some(1),
            sender: Principal::anonymous(),
            notification_id,
            taken_at_ns: NOW,
        }
    }

    fn push(browser_id: BrowserId, inbox_notification_id: InboxNotificationId) -> PlannedPush {
        PlannedPush {
            browser_id,
            path: Path::Fcm,
            inbox_notification_id: Some(inbox_notification_id),
        }
    }

    fn labels() -> Labels {
        Labels::app("https://app.example", None)
    }

    fn fan_out_to_two_browsers() {
        reset_for_testing();
        fanned_out(fanned(7), labels(), vec![push(1, 10), push(2, 20)], NOW);
    }

    #[test]
    fn one_collected_copy_delivers_the_notification_once() {
        fan_out_to_two_browsers();
        settled(1, 1, 10, Ok(()));
        settled(1, 2, 20, Ok(()));
        collected(1, 1, 10);
        collected(1, 2, 20);

        assert_eq!(notification_count_for_testing("pushed"), 1);
        assert_eq!(notification_count_for_testing("delivered"), 1);
        assert_eq!(in_flight(), 0);
    }

    #[test]
    fn a_notification_whose_every_post_failed_is_lost_at_push() {
        fan_out_to_two_browsers();
        settled(1, 1, 10, Err(Reason::RelayGone));
        settled(1, 2, 20, Err(Reason::RelayRateLimited));

        assert_eq!(push_loss_count_for_testing("relay_gone"), 1);
        assert_eq!(push_loss_count_for_testing("relay_rate_limited"), 1);
        assert_eq!(loss_count_for_testing("relay_rate_limited"), 1);
        assert_eq!(notification_count_for_testing("pushed"), 0);
    }

    #[test]
    fn a_pushed_notification_no_device_collected_is_lost_at_deliver() {
        fan_out_to_two_browsers();
        settled(1, 1, 10, Ok(()));
        settled(1, 2, 20, Err(Reason::RelayGone));
        sweep(NOW + COLLECTION_GRACE_NS);

        assert_eq!(push_loss_count_for_testing("never_fetched"), 1);
        assert_eq!(loss_count_for_testing("never_fetched"), 1);
        assert_eq!(in_flight(), 0);
    }

    #[test]
    fn a_copy_removed_before_its_post_is_answered_still_counts_as_pushed() {
        fan_out_to_two_browsers();
        entries_removed(1, 1, [10], Reason::InboxFull);
        settled(1, 1, 10, Ok(()));
        settled(1, 2, 20, Ok(()));
        collected(1, 2, 20);

        assert_eq!(push_loss_count_for_testing("inbox_full"), 1);
        assert_eq!(notification_count_for_testing("delivered"), 1);
        assert_eq!(loss_count_for_testing("inbox_full"), 0);
    }

    #[test]
    fn a_cleared_inbox_loses_the_notification_for_its_reason() {
        reset_for_testing();
        fanned_out(fanned(7), labels(), vec![push(1, 10)], NOW);
        settled(1, 1, 10, Ok(()));
        inbox_cleared(1, 1, Reason::SubscriptionReplaced);

        assert_eq!(loss_count_for_testing("subscription_replaced"), 1);
    }

    #[test]
    fn a_fan_out_that_reached_no_browser_is_lost_for_no_device() {
        reset_for_testing();
        fanned_out(fanned(7), labels(), vec![], NOW);

        assert_eq!(loss_count_for_testing("no_device"), 1);
    }

    #[test]
    fn origins_past_the_cap_are_counted_as_other() {
        reset_for_testing();
        for index in 0..MAX_NAMED_ORIGINS {
            Labels::app(&format!("https://{index}.example"), None);
        }

        assert_eq!(
            Labels::app("https://one-too-many.example", None).origin,
            Some(OTHER.to_string())
        );
        assert_eq!(
            Labels::app("https://0.example", None).origin,
            Some("https://0.example".to_string())
        );
    }

    #[test]
    fn delivery_paths_are_read_off_the_endpoint() {
        let path = |endpoint| Path::of(DeliveryTarget::Browser, endpoint);
        assert_eq!(path("https://fcm.googleapis.com/fcm/send/abc"), Path::Fcm);
        assert_eq!(path("https://web.push.apple.com/abc"), Path::Apple);
        assert_eq!(
            path("https://updates.push.services.mozilla.com/wpush/v2/abc"),
            Path::Mozilla
        );
        assert_eq!(
            path("https://wns2-par02p.notify.windows.com/w/?token=abc"),
            Path::Windows
        );
        assert_eq!(path("https://relay.example/abc"), Path::Other);
    }
}
