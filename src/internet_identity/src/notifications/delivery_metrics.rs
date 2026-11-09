//! What becomes of each notification and of each push, counted for `/metrics`.
//!
//! A notification is counted once at each stage it reaches: received, accepted, pushed
//! (a push service took one of its pushes) and delivered (one of its devices collected
//! it). One that stops is counted as a loss with the reason: up to the fan-out where it
//! stopped, past it once no copy can arrive any more, for the reason its last copy was
//! lost, or as never fetched where a copy was still waiting. A push is one copy of a
//! notification on its way to one browser or Home Screen app, and runs the funnel from
//! accepted on, by delivery path, with a reason for every push lost. Resends folded into
//! a waiting copy, and notifications the app was told to send again later, sit beside the
//! funnel: they are counted in received when they come back.
//!
//! Counting a notification once however many copies it has takes one key per live
//! notification, kept until it expires. Everything lives on the heap, as the backlogs do,
//! so an upgrade starts the counters from zero, which Prometheus reads as a counter reset.
//!
//! Every series is a line of the `/metrics` reply, which has to fit in one response. The
//! apps named on their own series are the ones most used lately by a sender they list,
//! and the series together stay within [`MAX_SERIES_BYTES`]: past it, the least used apps
//! give way to busier ones, and what still does not fit is counted under `other`. An app
//! that gives way loses its series, which Prometheus reads as series that ended, and
//! what follows is counted under `other`.

use crate::notifications::dispatch::DeliveryTarget;
use crate::notifications::webpush::vapid_key;
use crate::storage::anchor::InboxNotification;
use candid::Principal;
use ic_metrics_encoder::MetricsEncoder;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ApplicationNumber, Timestamp, Urgency,
};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::hash::{DefaultHasher, Hash, Hasher};

/// Apps named on their own series; any other app is counted as `other`. The number of
/// apps has no bound, and every series is one more line on every scrape.
const MAX_NAMED_ORIGINS: usize = 100;

/// Pairs of a calling canister and the origin it named, for the senders an origin does
/// not list, named on their own series.
const MAX_NAMED_UNLISTED_SENDERS: usize = 100;

/// How fast a ranking forgets: an entry's use counts half after this long.
const RANK_HALF_LIFE_NS: Timestamp = 24 * 60 * 60 * 1_000_000_000;

/// The bytes the series' lines take in `/metrics`. Series counted under `other` or for
/// Internet Identity itself are bounded by the labels' variants, at about 150 KB each, and
/// are created past it; with them and the rest of `/metrics` the reply stays well within
/// its 3 MiB.
const MAX_SERIES_BYTES: usize = 1_500_000;

/// A line's bytes past its labels, at the longest value and timestamp: the space before
/// each, the newline, and some to spare.
const LINE_VALUE_BYTES: usize = 40;

const OTHER: &str = "other";

/// Live notifications kept a key for. Past it, the one closest to expiry is forgotten,
/// and what becomes of its copies afterwards is not counted.
const MAX_LIVE_NOTIFICATIONS: usize = 100_000;

/// Urgency labels in lane order, which is how a live key stores one.
const URGENCIES: [&str; 4] = ["high", "normal", "low", "very_low"];

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
    /// The app's origin while it is among the apps ranked by use, otherwise [`OTHER`].
    /// Internet Identity's own notifications have none.
    origin: Option<String>,
}

impl Labels {
    /// An app's notification, by the origin it was sent for, from a caller not known to
    /// be one of its senders. The origin is named only where a listed sender already
    /// ranked it, as anyone can name any origin.
    pub(crate) fn app(origin: &str, urgency: Option<&Urgency>) -> Self {
        Labels {
            source: Source::Apps,
            urgency: urgency_label(urgency),
            origin: Some(with(|counters| counters.origin_label(origin))),
        }
    }

    /// An app's notification from a sender the origin lists, which counts as one use of
    /// the app in its ranking.
    pub(crate) fn listed_app(origin: &str, urgency: Option<&Urgency>, now_ns: Timestamp) -> Self {
        Labels {
            source: Source::Apps,
            urgency: urgency_label(urgency),
            origin: Some(with(|counters| counters.rank_app(origin, now_ns))),
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

    /// The same labels under `origin`.
    fn with_origin(&self, origin: String) -> Self {
        Labels {
            origin: Some(origin),
            ..self.clone()
        }
    }

    /// The same labels with an app's origin counted as [`OTHER`].
    fn fold_into_other(&self) -> Self {
        match self.origin {
            Some(_) => self.with_origin(OTHER.to_string()),
            None => self.clone(),
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
    /// The service worker's call to the app's canister for the content got no answer.
    PayloadUnreachable,
    /// The app's canister rejected the service worker's call for the content, or trapped.
    PayloadRejected,
    /// The address it was pushed to was replaced or dropped, which clears the inbox.
    SubscriptionReplaced,
    /// Pushed out of a full inbox by a newer entry.
    InboxFull,
    ConsentRevoked,
    /// The identity went over its browser cap and this browser was given up.
    BrowserEvicted,
    /// The user signed this browser out.
    BrowserSignedOut,
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
            Reason::PayloadUnreachable => "payload_unreachable",
            Reason::PayloadRejected => "payload_rejected",
            Reason::SubscriptionReplaced => "subscription_replaced",
            Reason::InboxFull => "inbox_full",
            Reason::ConsentRevoked => "consent_revoked",
            Reason::BrowserEvicted => "browser_evicted",
            Reason::BrowserSignedOut => "browser_signed_out",
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
            | Reason::PayloadUnreachable
            | Reason::PayloadRejected
            | Reason::SubscriptionReplaced
            | Reason::InboxFull
            | Reason::ConsentRevoked
            | Reason::BrowserEvicted
            | Reason::BrowserSignedOut => DropPoint::Deliver,
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

/// The way a push reaches a device, named after the main browser of the push service its
/// address belongs to, or the Home Screen app the browser routes through. The canister
/// sees the push service and never the browser, so each is a family: `chrome` is every
/// browser on Firebase Cloud Messaging, Brave and Opera among them, and `safari` is every
/// browser on Apple's push service, which on iOS is all of them.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Path {
    Chrome,
    Safari,
    Firefox,
    Edge,
    /// The Home Screen app.
    App,
    Other,
}

impl Path {
    const COUNT: usize = 6;
    const ALL: [Path; Path::COUNT] = [
        Path::Chrome,
        Path::Safari,
        Path::Firefox,
        Path::Edge,
        Path::App,
        Path::Other,
    ];

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
            "fcm.googleapis.com" => Path::Chrome,
            "updates.push.services.mozilla.com" => Path::Firefox,
            "push.apple.com" => Path::Safari,
            _ if host.ends_with(".push.apple.com") => Path::Safari,
            _ if host.ends_with(".notify.windows.com") => Path::Edge,
            _ => Path::Other,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Path::Chrome => "chrome",
            Path::Safari => "safari",
            Path::Firefox => "firefox",
            Path::Edge => "edge",
            Path::App => "app",
            Path::Other => "other",
        }
    }
}

/// How long past its expiry a notification's key is kept, so a device asking for a copy
/// just too late is counted as that rather than as never fetched.
const LATE_FETCH_GRACE_NS: Timestamp = 5 * 60 * 1_000_000_000;

/// A notification whose copies went out, by when it expires and a hash of who it is for
/// and its id. Every copy carries both, so each finds the same key. Ordered by expiry, so
/// the ones no copy of can arrive any more are at the front. The expiry also tells a
/// notification apart from an earlier one the app sent under the same id.
type LiveKey = (Timestamp, u64);

#[derive(Clone, Copy)]
struct Live {
    application_number: Option<ApplicationNumber>,
    /// Into [`URGENCIES`].
    urgency: u8,
    is_pushed: bool,
    is_delivered: bool,
    /// Copies on each [`Path`] that neither reached a device nor were lost yet. What is
    /// left when the key goes was never fetched.
    open_copies: [u8; Path::COUNT],
    /// Why the last copy lost ended, which is the notification's reason when none of its
    /// copies was delivered and none is left open.
    last_loss: Option<Reason>,
}

fn live_key(anchor_number: AnchorNumber, entry: &InboxNotification) -> LiveKey {
    let mut hasher = DefaultHasher::new();
    anchor_number.hash(&mut hasher);
    entry.application_number.hash(&mut hasher);
    entry.account_number.hash(&mut hasher);
    entry.sender.hash(&mut hasher);
    entry.notification_id.hash(&mut hasher);
    (entry.expires_at_ns, hasher.finish())
}

/// One push a fan-out planned for a browser.
pub(crate) struct PlannedPush {
    pub(crate) path: Path,
    /// Deposited in the browser's inbox. Not where its relay cannot be reached.
    pub(crate) is_deposited: bool,
}

/// How much an entry was used lately: `value` uses as of `at_ns`, halving every
/// [`RANK_HALF_LIFE_NS`] since.
#[derive(Clone, Copy, Debug)]
struct Score {
    value: f64,
    at_ns: Timestamp,
}

impl Score {
    fn value_at(self, now_ns: Timestamp) -> f64 {
        let half_lives = now_ns.saturating_sub(self.at_ns) as f64 / RANK_HALF_LIFE_NS as f64;
        self.value * (-half_lives).exp2()
    }

    /// Orders scores as their values at any one moment do, whichever moment that is, as
    /// every score halves at the same pace.
    fn level(self) -> f64 {
        self.value.log2() + self.at_ns as f64 / RANK_HALF_LIFE_NS as f64
    }
}

/// The entries used most lately, at most `CAPACITY` of them. A newcomer to a full ranking
/// replaces the lowest entry and starts just above where it was, so an entry with more
/// than a `CAPACITY`th of the recent use is never the one replaced.
struct Ranking<K, const CAPACITY: usize> {
    scores: BTreeMap<K, Score>,
}

impl<K, const CAPACITY: usize> Default for Ranking<K, CAPACITY> {
    fn default() -> Self {
        Ranking {
            scores: BTreeMap::new(),
        }
    }
}

impl<K: Ord + Clone, const CAPACITY: usize> Ranking<K, CAPACITY> {
    fn contains(&self, key: &K) -> bool {
        self.scores.contains_key(key)
    }

    /// Counts one use of `key`, answering the entry it replaced, if any.
    fn rank(&mut self, key: &K, now_ns: Timestamp) -> Option<K> {
        if let Some(score) = self.scores.get_mut(key) {
            *score = Score {
                value: score.value_at(now_ns) + 1.0,
                at_ns: now_ns,
            };
            return None;
        }
        let mut replaced = None;
        let mut value = 1.0;
        if self.scores.len() >= CAPACITY {
            if let Some((lowest, score)) = self.lowest() {
                value += score.value_at(now_ns);
                self.scores.remove(&lowest);
                replaced = Some(lowest);
            }
        }
        self.scores.insert(
            key.clone(),
            Score {
                value,
                at_ns: now_ns,
            },
        );
        replaced
    }

    fn lowest(&self) -> Option<(K, Score)> {
        self.scores
            .iter()
            .min_by(|(_, a), (_, b)| a.level().total_cmp(&b.level()))
            .map(|(key, score)| (key.clone(), *score))
    }

    /// The lowest entry, where it ranks below `key`.
    fn lowest_below(&self, key: &K) -> Option<K> {
        let level = self.scores.get(key)?.level();
        self.lowest()
            .filter(|(_, score)| score.level() < level)
            .map(|(lowest, _)| lowest)
    }

    fn remove(&mut self, key: &K) {
        self.scores.remove(key);
    }
}

/// The key of one series: its own labels, and the [`Labels`] every series carries.
trait Series: Ord + Clone {
    /// The metric this series is a line of.
    const METRIC: &'static str;

    fn labels(&self) -> &Labels;

    /// The labels of the series' line, in the order `/metrics` writes them.
    fn pairs(&self) -> Vec<(&str, &str)>;

    /// The app in the apps ranking this series is counted for, by origin.
    fn app_origin(&self) -> Option<&str> {
        self.labels().origin.as_deref()
    }

    /// The series this one is counted under where it has no room, or where its app is not
    /// ranked.
    fn fold_into_other(&self) -> Self;

    /// The bytes of the series' line in `/metrics`, at most.
    fn line_bytes(&self) -> usize {
        let labels: usize = self
            .pairs()
            .iter()
            .map(|(name, value)| name.len() + value.len() + r#"="","#.len())
            .sum();
        Self::METRIC.len() + "{}".len() + labels + LINE_VALUE_BYTES
    }
}

impl Series for (Stage, Labels) {
    const METRIC: &'static str = "internet_identity_notifications";
    fn labels(&self) -> &Labels {
        &self.1
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.1.pairs(&[("stage", self.0.as_str())])
    }
    fn fold_into_other(&self) -> Self {
        (self.0, self.1.fold_into_other())
    }
}

impl Series for (Reason, Labels) {
    const METRIC: &'static str = "internet_identity_notification_losses";
    fn labels(&self) -> &Labels {
        &self.1
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.1.pairs(&[
            ("drop", self.0.drop().as_str()),
            ("reason", self.0.as_str()),
        ])
    }
    fn fold_into_other(&self) -> Self {
        (self.0, self.1.fold_into_other())
    }
}

impl Series for (Stage, Path, Labels) {
    const METRIC: &'static str = "internet_identity_notification_pushes";
    fn labels(&self) -> &Labels {
        &self.2
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.2
            .pairs(&[("stage", self.0.as_str()), ("path", self.1.as_str())])
    }
    fn fold_into_other(&self) -> Self {
        (self.0, self.1, self.2.fold_into_other())
    }
}

impl Series for (Reason, Path, Labels) {
    const METRIC: &'static str = "internet_identity_notification_push_losses";
    fn labels(&self) -> &Labels {
        &self.2
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.2.pairs(&[
            ("drop", self.0.drop().as_str()),
            ("reason", self.0.as_str()),
            ("path", self.1.as_str()),
        ])
    }
    fn fold_into_other(&self) -> Self {
        (self.0, self.1, self.2.fold_into_other())
    }
}

impl Series for (DeferCause, Labels) {
    const METRIC: &'static str = "internet_identity_notifications_deferred";
    fn labels(&self) -> &Labels {
        &self.1
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.1.pairs(&[("cause", self.0.as_str())])
    }
    fn fold_into_other(&self) -> Self {
        (self.0, self.1.fold_into_other())
    }
}

/// A resend folded into a waiting copy.
impl Series for Labels {
    const METRIC: &'static str = "internet_identity_notifications_folded";
    fn labels(&self) -> &Labels {
        self
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        Labels::pairs(self, &[])
    }
    fn fold_into_other(&self) -> Self {
        Labels::fold_into_other(self)
    }
}

/// A sender an origin does not list, by the calling canister. Named by the ranking of
/// unlisted pairs, so in no app's.
impl Series for (String, Labels) {
    const METRIC: &'static str = "internet_identity_notification_unlisted_senders";
    fn labels(&self) -> &Labels {
        &self.1
    }
    fn pairs(&self) -> Vec<(&str, &str)> {
        self.1.pairs(&[("canister", &self.0)])
    }
    fn app_origin(&self) -> Option<&str> {
        None
    }
    fn fold_into_other(&self) -> Self {
        (OTHER.to_string(), self.1.fold_into_other())
    }
}

/// Deletes the series `is_dropped` picks, answering the bytes their lines took.
fn drop_series<K: Series>(map: &mut BTreeMap<K, u64>, is_dropped: impl Fn(&K) -> bool) -> usize {
    let mut bytes = 0;
    map.retain(|key, _| {
        let is_kept = !is_dropped(key);
        if !is_kept {
            bytes += key.line_bytes();
        }
        is_kept
    });
    bytes
}

/// Whether `key` is a series of the app at `origin`.
fn is_app_series<K: Series>(key: &K, origin: &str) -> bool {
    key.labels().origin.as_deref() == Some(origin)
}

/// Encodes every series of one counter.
fn encode_series<K: Series>(
    w: &mut MetricsEncoder<Vec<u8>>,
    help: &str,
    counter: &BTreeMap<K, u64>,
) -> std::io::Result<()> {
    let mut builder = w.counter_vec(K::METRIC, help)?;
    for (key, count) in counter {
        builder = builder.value(&key.pairs(), *count as f64)?;
    }
    Ok(())
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
    /// Origins, ranked by the notifications their listed senders send.
    apps: Ranking<String, MAX_NAMED_ORIGINS>,
    /// Canister and origin, ranked by the notifications refused for the pair.
    unlisted_pairs: Ranking<(String, String), MAX_NAMED_UNLISTED_SENDERS>,
    application_origins: BTreeMap<ApplicationNumber, String>,
    /// The bytes the series' lines take, see [`Series::line_bytes`].
    series_bytes: usize,
    /// New series counted under `other` for want of room.
    series_folded: u64,
    live: BTreeMap<LiveKey, Live>,
    evicted: u64,
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
    fn origin_label(&self, origin: &str) -> String {
        match self.apps.contains(&origin.to_string()) {
            true => origin.to_string(),
            false => OTHER.to_string(),
        }
    }

    fn rank_app(&mut self, origin: &str, now_ns: Timestamp) -> String {
        let origin = origin.to_string();
        if let Some(replaced) = self.apps.rank(&origin, now_ns) {
            self.drop_app(&replaced);
        }
        origin
    }

    /// Deletes the series of an app no longer ranked. Prometheus keeps what they counted
    /// up to its last scrape, and what follows is counted under `other`.
    fn drop_app(&mut self, origin: &str) {
        self.application_origins.retain(|_, named| named != origin);
        let bytes = drop_series(&mut self.notifications, |key| is_app_series(key, origin))
            + drop_series(&mut self.losses, |key| is_app_series(key, origin))
            + drop_series(&mut self.pushes, |key| is_app_series(key, origin))
            + drop_series(&mut self.push_losses, |key| is_app_series(key, origin))
            + drop_series(&mut self.deferred, |key| is_app_series(key, origin))
            + drop_series(&mut self.folded, |key| is_app_series(key, origin));
        self.series_bytes = self.series_bytes.saturating_sub(bytes);
    }

    fn rank_unlisted_pair(&mut self, pair: &(String, String), now_ns: Timestamp) {
        let Some((canister, origin)) = self.unlisted_pairs.rank(pair, now_ns) else {
            return;
        };
        let bytes = drop_series(&mut self.unlisted_senders, |key| {
            key.0 == canister && key.1.origin.as_deref() == Some(origin.as_str())
        });
        self.series_bytes = self.series_bytes.saturating_sub(bytes);
    }

    /// Makes room for a new series of `bytes` of the app at `origin`, if any, by dropping
    /// the apps ranked below it. Answers whether there is room.
    fn make_room(&mut self, origin: Option<&str>, bytes: usize) -> bool {
        while self.series_bytes.saturating_add(bytes) > MAX_SERIES_BYTES {
            let Some(origin) = origin.map(str::to_string) else {
                return false;
            };
            let Some(lowest) = self.apps.lowest_below(&origin) else {
                return false;
            };
            self.apps.remove(&lowest);
            self.drop_app(&lowest);
        }
        true
    }

    /// Counts one more under `key`, in the counter `counter` picks. A new series that
    /// does not fit is counted under `other`.
    fn count<K: Series>(&mut self, counter: fn(&mut Counters) -> &mut BTreeMap<K, u64>, key: K) {
        let mut key = key;
        let origin = key.app_origin().map(str::to_string);
        // Labels taken before their app gave way still name it.
        if origin
            .as_ref()
            .is_some_and(|origin| origin != OTHER && !self.apps.contains(origin))
        {
            key = key.fold_into_other();
        }
        if !counter(self).contains_key(&key) {
            let origin = origin.filter(|origin| self.apps.contains(origin));
            if !self.make_room(origin.as_deref(), key.line_bytes()) {
                let folded = key.fold_into_other();
                if folded != key {
                    self.series_folded = self.series_folded.saturating_add(1);
                    key = folded;
                }
            }
            if !counter(self).contains_key(&key) {
                self.series_bytes = self.series_bytes.saturating_add(key.line_bytes());
            }
        }
        bump(counter(self), key);
    }

    fn count_reached(&mut self, stage: Stage, labels: &Labels) {
        self.count(|c| &mut c.notifications, (stage, labels.clone()));
    }

    fn count_lost(&mut self, reason: Reason, labels: &Labels) {
        self.count(|c| &mut c.losses, (reason, labels.clone()));
    }

    fn count_push(&mut self, stage: Stage, path: Path, labels: &Labels) {
        self.count(|c| &mut c.pushes, (stage, path, labels.clone()));
    }

    fn count_push_lost(&mut self, reason: Reason, path: Path, labels: &Labels) {
        self.count(|c| &mut c.push_losses, (reason, path, labels.clone()));
    }

    /// The labels of a live notification.
    fn live_labels(&self, live: &Live) -> Labels {
        let urgency = URGENCIES[usize::from(live.urgency)];
        match live.application_number {
            Some(application_number) => Labels {
                source: Source::Apps,
                urgency,
                origin: Some(
                    self.application_origins
                        .get(&application_number)
                        .cloned()
                        .unwrap_or_else(|| OTHER.to_string()),
                ),
            },
            None => Labels {
                source: Source::InternetIdentity,
                urgency,
                origin: None,
            },
        }
    }

    /// Ends one open copy of `entry`'s notification on `path`, answering the labels to
    /// count it under. `None` where no copy there is open: one already counted, or one
    /// of a notification with no key, whose copies were counted when it went.
    fn close(
        &mut self,
        anchor_number: AnchorNumber,
        entry: &InboxNotification,
        path: Path,
        loss: Option<Reason>,
    ) -> Option<Labels> {
        let live = self.live.get_mut(&live_key(anchor_number, entry))?;
        let open_copies = &mut live.open_copies[path as usize];
        *open_copies = open_copies.checked_sub(1)?;
        if loss.is_some() {
            live.last_loss = loss;
        }
        let live = *live;
        Some(self.live_labels(&live))
    }

    /// Forgets the notifications no copy of can arrive any more, counting the copies still
    /// open as never fetched and an undelivered notification as lost, and past the cap the
    /// ones closest to that.
    fn prune(&mut self, now_ns: Timestamp) {
        while let Some((&(expires_at_ns, _), _)) = self.live.first_key_value() {
            let is_gone = expires_at_ns.saturating_add(LATE_FETCH_GRACE_NS) <= now_ns;
            if !is_gone && self.live.len() <= MAX_LIVE_NOTIFICATIONS {
                break;
            }
            if !is_gone {
                self.evicted = self.evicted.saturating_add(1);
            }
            let Some((_, live)) = self.live.pop_first() else {
                break;
            };
            if !is_gone {
                continue;
            }
            let labels = self.live_labels(&live);
            for path in Path::ALL {
                for _ in 0..live.open_copies[path as usize] {
                    self.count_push_lost(Reason::NeverFetched, path, &labels);
                }
            }
            if live.is_delivered {
                continue;
            }
            // A copy still open was never fetched; otherwise every copy was lost, the last
            // one for the reason the notification takes.
            let reason = match live.open_copies.iter().any(|copies| *copies > 0) {
                true => Reason::NeverFetched,
                false => live.last_loss.unwrap_or(Reason::NeverFetched),
            };
            self.count_lost(reason, &labels);
        }
    }
}

/// A notification in a call the app made, before anything is decided about it.
pub(crate) fn count_received(labels: &Labels) {
    with(|counters| counters.count_reached(Stage::Received, labels));
}

/// A received notification that went into a backlog.
pub(crate) fn count_accepted(labels: &Labels) {
    with(|counters| counters.count_reached(Stage::Accepted, labels));
}

/// A notification lost before it was fanned out to any browser.
pub(crate) fn count_lost(reason: Reason, labels: &Labels) {
    with(|counters| counters.count_lost(reason, labels));
}

pub(crate) fn count_deferred(cause: DeferCause, labels: &Labels) {
    with(|counters| counters.count(|c| &mut c.deferred, (cause, labels.clone())));
}

pub(crate) fn count_folded(labels: &Labels) {
    with(|counters| counters.count(|c| &mut c.folded, labels.clone()));
}

/// A notification for `origin` refused because `canister` is not one of its senders,
/// `labels` being its labels from [`Labels::app`].
pub(crate) fn count_unlisted_sender(
    canister: Principal,
    origin: &str,
    labels: &Labels,
    now_ns: Timestamp,
) {
    with(|counters| {
        let pair = (canister.to_text(), origin.to_string());
        counters.rank_unlisted_pair(&pair, now_ns);
        let (canister, origin) = pair;
        counters.count(
            |c| &mut c.unlisted_senders,
            (canister, labels.with_origin(origin)),
        );
        counters.count_lost(Reason::SenderNotAllowed, labels);
    });
}

/// Names the app behind `application_number`, so what it has in the backlog is counted
/// under its origin once it is taken. Only an app in the ranking is named.
pub(crate) fn number_application(application_number: ApplicationNumber, origin: &str) {
    with(|counters| {
        if counters.apps.contains(&origin.to_string()) {
            counters
                .application_origins
                .insert(application_number, origin.to_string());
        }
    });
}

/// A notification taken from a backlog and deposited, as `entry`, for the browsers in
/// `pushes`. One deposited nowhere is lost here.
pub(crate) fn track_fan_out(
    anchor_number: AnchorNumber,
    entry: &InboxNotification,
    labels: Labels,
    pushes: Vec<PlannedPush>,
    now_ns: Timestamp,
) {
    with(|counters| {
        counters.prune(now_ns);
        if pushes.is_empty() {
            counters.count_lost(Reason::NoDevice, &labels);
            return;
        }
        let mut open_copies = [0u8; Path::COUNT];
        for push in pushes {
            counters.count_push(Stage::Accepted, push.path, &labels);
            if push.is_deposited {
                let copies = &mut open_copies[push.path as usize];
                *copies = copies.saturating_add(1);
            } else {
                counters.count_push_lost(Reason::RelayUnreachable, push.path, &labels);
            }
        }
        if open_copies.iter().all(|copies| *copies == 0) {
            counters.count_lost(Reason::RelayUnreachable, &labels);
            return;
        }
        let urgency = URGENCIES
            .iter()
            .position(|known| *known == labels.urgency)
            .and_then(|index| u8::try_from(index).ok())
            .unwrap_or(1);
        counters.live.insert(
            live_key(anchor_number, entry),
            Live {
                application_number: entry.application_number,
                urgency,
                is_pushed: false,
                is_delivered: false,
                open_copies,
                last_loss: None,
            },
        );
        counters.prune(now_ns);
    });
}

/// The answer to the post announcing `entry`: `Ok` where a push service took it.
pub(crate) fn count_push_outcome(
    anchor_number: AnchorNumber,
    entry: &InboxNotification,
    path: Path,
    outcome: Result<(), Reason>,
) {
    with(|counters| {
        let reason = match outcome {
            Ok(()) => {
                let Some(live) = counters.live.get_mut(&live_key(anchor_number, entry)) else {
                    return;
                };
                let is_first_push = !std::mem::replace(&mut live.is_pushed, true);
                let live = *live;
                let labels = counters.live_labels(&live);
                counters.count_push(Stage::Pushed, path, &labels);
                if is_first_push {
                    counters.count_reached(Stage::Pushed, &labels);
                }
                return;
            }
            Err(reason) => reason,
        };
        if let Some(labels) = counters.close(anchor_number, entry, path, Some(reason)) {
            counters.count_push_lost(reason, path, &labels);
        }
    });
}

/// A device collected `entry`, which delivers its notification unless another did first.
pub(crate) fn count_collected(
    anchor_number: AnchorNumber,
    entry: &InboxNotification,
    path: Path,
    now_ns: Timestamp,
) {
    with(|counters| {
        counters.prune(now_ns);
        let Some(labels) = counters.close(anchor_number, entry, path, None) else {
            return;
        };
        counters.count_push(Stage::Delivered, path, &labels);
        let is_first_delivery = counters
            .live
            .get_mut(&live_key(anchor_number, entry))
            .is_some_and(|live| !std::mem::replace(&mut live.is_delivered, true));
        if is_first_delivery {
            counters.count_reached(Stage::Delivered, &labels);
        }
    });
}

/// Entries taken out of an inbox at `path`, for `reason`, before a device collected them.
pub(crate) fn count_removed_entries<'a>(
    anchor_number: AnchorNumber,
    entries: impl IntoIterator<Item = &'a InboxNotification>,
    path: Path,
    reason: Reason,
) {
    with(|counters| {
        for entry in entries {
            if let Some(labels) = counters.close(anchor_number, entry, path, Some(reason)) {
                counters.count_push_lost(reason, path, &labels);
            }
        }
    });
}

/// Notifications out to their browsers, with a copy still open, neither delivered nor
/// expired, by their labels.
fn in_flight(now_ns: Timestamp) -> BTreeMap<Labels, u64> {
    with(|counters| {
        let mut by_labels = BTreeMap::new();
        for live in counters
            .live
            .range((now_ns.saturating_add(1), 0)..)
            .map(|(_, live)| live)
        {
            if !live.is_delivered && live.open_copies.iter().any(|copies| *copies > 0) {
                bump(&mut by_labels, counters.live_labels(live));
            }
        }
        by_labels
    })
}

pub(crate) fn encode(
    w: &mut MetricsEncoder<Vec<u8>>,
    waiting: usize,
    now_ns: Timestamp,
) -> std::io::Result<()> {
    with(|counters| {
        encode_series(
            w,
            "Notifications that reached each stage since the last upgrade: received, accepted, pushed (a push service took one of its pushes) and delivered (a device collected it).",
            &counters.notifications,
        )?;
        encode_series(
            w,
            "Notifications lost before they were fanned out, since the last upgrade, by drop and reason. Losses past the fan-out are the gaps between stages, with their reasons on the push losses.",
            &counters.losses,
        )?;
        encode_series(
            w,
            "Pushes, one per browser or Home Screen app a notification goes to, that reached each stage since the last upgrade, by delivery path.",
            &counters.pushes,
        )?;
        encode_series(
            w,
            "Pushes lost since the last upgrade, by drop, reason and delivery path.",
            &counters.push_losses,
        )?;
        encode_series(
            w,
            "Notifications the app was told to send again later since the last upgrade, by cause. They are counted in received when they come back.",
            &counters.deferred,
        )?;
        encode_series(
            w,
            "Resends of a notification still waiting in the backlog since the last upgrade, folded into the waiting copy.",
            &counters.folded,
        )?;
        w.encode_counter(
            "internet_identity_notification_series_folded",
            counters.series_folded as f64,
            "New series counted under other since the last upgrade because the series had reached their cap.",
        )?;
        w.encode_counter(
            "internet_identity_notification_keys_evicted",
            counters.evicted as f64,
            "Live notifications forgotten since the last upgrade because more were live than the cap allows. What becomes of their copies afterwards is not counted.",
        )?;
        encode_series(
            w,
            "Notifications refused since the last upgrade because the calling canister is not one of the app's senders, by canister and the origin it named.",
            &counters.unlisted_senders,
        )?;
        Ok::<(), std::io::Error>(())
    })?;
    let mut builder = w.gauge_vec(
        "internet_identity_notifications_in_flight",
        "Notifications out to their browsers, neither delivered nor expired.",
    )?;
    for (labels, count) in in_flight(now_ns) {
        builder = builder.value(&labels.pairs(&[]), count as f64)?;
    }
    w.encode_gauge(
        "internet_identity_notifications_waiting",
        waiting as f64,
        "Notifications waiting in a backlog to be sent out.",
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
pub(crate) fn push_count_for_testing(stage: &str) -> u64 {
    with(|counters| {
        counters
            .pushes
            .iter()
            .filter(|((s, _, _), _)| s.as_str() == stage)
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
    use internet_identity_interface::internet_identity::types::NotificationId;

    const NOW: Timestamp = 1_000;
    const EXPIRES: Timestamp = 2_000;

    fn entry(notification_id: NotificationId, expires_at_ns: Timestamp) -> InboxNotification {
        InboxNotification {
            inbox_notification_id: 0,
            application_number: Some(1),
            event: None,
            sender: Principal::anonymous(),
            notification_id,
            expires_at_ns,
            account_number: None,
        }
    }

    fn labels() -> Labels {
        Labels::app("https://app.example", None)
    }

    fn deposited() -> PlannedPush {
        PlannedPush {
            path: Path::Chrome,
            is_deposited: true,
        }
    }

    fn fan_out_to_two_browsers() -> InboxNotification {
        reset_for_testing();
        let entry = entry(7, EXPIRES);
        track_fan_out(1, &entry, labels(), vec![deposited(), deposited()], NOW);
        entry
    }

    /// Long enough after `EXPIRES` for every key to be gone.
    const LATER: Timestamp = EXPIRES + LATE_FETCH_GRACE_NS;

    fn in_flight_count(now_ns: Timestamp) -> u64 {
        in_flight(now_ns).values().sum()
    }

    /// Prunes the keys gone by `LATER`, by fanning out another notification then.
    fn after_every_key_went() {
        track_fan_out(1, &entry(99, LATER * 2), labels(), vec![deposited()], LATER);
    }

    #[test]
    fn two_collected_copies_deliver_the_notification_once() {
        let entry = fan_out_to_two_browsers();
        count_push_outcome(1, &entry, Path::Chrome, Ok(()));
        count_push_outcome(1, &entry, Path::Chrome, Ok(()));
        count_collected(1, &entry, Path::Chrome, NOW);
        count_collected(1, &entry, Path::Chrome, NOW);

        assert_eq!(notification_count_for_testing("pushed"), 1);
        assert_eq!(notification_count_for_testing("delivered"), 1);
        assert_eq!(push_count_for_testing("delivered"), 2);
        assert_eq!(in_flight_count(NOW), 0);
    }

    #[test]
    fn a_resend_under_the_same_id_is_another_notification() {
        let first_entry = fan_out_to_two_browsers();
        count_collected(1, &first_entry, Path::Chrome, NOW);
        let resent_entry = entry(7, EXPIRES + 1);
        track_fan_out(1, &resent_entry, labels(), vec![deposited()], NOW);
        count_collected(1, &resent_entry, Path::Chrome, NOW);

        assert_eq!(notification_count_for_testing("delivered"), 2);
    }

    #[test]
    fn a_failed_post_is_a_push_loss_at_once() {
        let entry = fan_out_to_two_browsers();
        count_push_outcome(1, &entry, Path::Chrome, Err(Reason::RelayGone));

        assert_eq!(push_loss_count_for_testing("relay_gone"), 1);
        assert_eq!(loss_count_for_testing("relay_gone"), 0);
    }

    #[test]
    fn a_notification_whose_every_copy_was_lost_takes_the_last_reason() {
        let entry = fan_out_to_two_browsers();
        count_push_outcome(1, &entry, Path::Chrome, Err(Reason::RelayGone));
        count_removed_entries(1, [&entry], Path::Chrome, Reason::InboxFull);
        after_every_key_went();

        assert_eq!(loss_count_for_testing("inbox_full"), 1);
        assert_eq!(loss_count_for_testing("relay_gone"), 0);
    }

    #[test]
    fn an_undelivered_notification_with_a_copy_still_open_was_never_fetched() {
        let entry = fan_out_to_two_browsers();
        count_push_outcome(1, &entry, Path::Chrome, Err(Reason::RelayGone));
        after_every_key_went();

        assert_eq!(loss_count_for_testing("never_fetched"), 1);
    }

    #[test]
    fn a_delivered_notification_is_not_lost() {
        let entry = fan_out_to_two_browsers();
        count_collected(1, &entry, Path::Chrome, NOW);
        count_push_outcome(1, &entry, Path::Chrome, Err(Reason::RelayGone));
        after_every_key_went();

        assert_eq!(loss_count_for_testing("relay_gone"), 0);
        assert_eq!(loss_count_for_testing("never_fetched"), 0);
    }

    #[test]
    fn a_removed_entry_keeps_its_notifications_urgency() {
        reset_for_testing();
        let entry = entry(7, EXPIRES);
        track_fan_out(
            1,
            &entry,
            Labels::app("https://app.example", Some(&Urgency::High)),
            vec![deposited()],
            NOW,
        );
        count_removed_entries(1, [&entry], Path::Chrome, Reason::InboxFull);

        assert!(with(|counters| counters
            .push_losses
            .keys()
            .any(|(_, _, labels)| labels.urgency == "high")));
    }

    #[test]
    fn a_fan_out_that_reached_no_browser_is_lost_for_no_device() {
        reset_for_testing();
        track_fan_out(1, &entry(7, EXPIRES), labels(), vec![], NOW);

        assert_eq!(loss_count_for_testing("no_device"), 1);
    }

    #[test]
    fn copies_still_open_when_the_key_goes_were_never_fetched() {
        let entry = fan_out_to_two_browsers();
        count_collected(1, &entry, Path::Chrome, NOW);
        assert_eq!(in_flight_count(NOW), 0);

        track_fan_out(
            1,
            &self::entry(8, LATER * 2),
            labels(),
            vec![deposited()],
            LATER,
        );

        assert_eq!(push_loss_count_for_testing("never_fetched"), 1);
        assert_eq!(in_flight_count(LATER), 1);
    }

    #[test]
    fn an_undelivered_notification_leaves_the_gauge_when_it_expires() {
        fan_out_to_two_browsers();

        assert_eq!(in_flight_count(NOW), 1);
        assert_eq!(in_flight_count(EXPIRES), 0);
    }

    #[test]
    fn a_copy_is_counted_once_however_often_it_is_removed() {
        let entry = fan_out_to_two_browsers();
        count_push_outcome(1, &entry, Path::Chrome, Err(Reason::RelayGone));
        count_removed_entries(1, [&entry], Path::Chrome, Reason::SubscriptionReplaced);
        count_removed_entries(1, [&entry], Path::Chrome, Reason::SubscriptionReplaced);
        count_collected(1, &entry, Path::Chrome, LATER);

        assert_eq!(push_loss_count_for_testing("relay_gone"), 1);
        assert_eq!(push_loss_count_for_testing("subscription_replaced"), 1);
        assert_eq!(push_loss_count_for_testing("never_fetched"), 0);
        assert_eq!(push_count_for_testing("delivered"), 0);
    }

    #[test]
    fn a_copy_removed_after_its_key_is_gone_is_not_counted_again() {
        let entry = fan_out_to_two_browsers();
        track_fan_out(
            1,
            &self::entry(8, LATER * 2),
            labels(),
            vec![deposited()],
            LATER,
        );
        count_removed_entries(1, [&entry], Path::Chrome, Reason::FetchedTooLate);

        assert_eq!(push_loss_count_for_testing("never_fetched"), 2);
        assert_eq!(push_loss_count_for_testing("fetched_too_late"), 0);
    }

    #[test]
    fn the_cap_evicts_the_soonest_to_expire() {
        reset_for_testing();
        for id in 0..=MAX_LIVE_NOTIFICATIONS as u64 {
            track_fan_out(
                2,
                &entry(id, EXPIRES + id),
                labels(),
                vec![deposited()],
                NOW,
            );
        }

        assert_eq!(with(|counters| counters.live.len()), MAX_LIVE_NOTIFICATIONS);
        assert_eq!(with(|counters| counters.evicted), 1);
        assert_eq!(push_loss_count_for_testing("never_fetched"), 0);
    }

    const DAY: Timestamp = RANK_HALF_LIFE_NS;

    /// The longest origin II takes, `FRONTEND_HOSTNAME_LIMIT` bytes, so every line is as
    /// long as an app's line can be.
    fn app(index: usize) -> String {
        let origin = format!("https://{index:03}.example");
        let padding = "a".repeat(crate::delegation::FRONTEND_HOSTNAME_LIMIT - origin.len());
        format!("https://{padding}{index:03}.example")
    }

    fn origin_of(labels: &Labels) -> &str {
        labels.origin.as_deref().unwrap_or_default()
    }

    fn is_ranked(origin: &str) -> bool {
        with(|counters| counters.apps.contains(&origin.to_string()))
    }

    fn total_losses() -> u64 {
        with(|counters| counters.losses.values().sum())
    }

    /// Ranks `MAX_NAMED_ORIGINS` apps at `now_ns`, each with a loss, `app(0)` the busiest.
    fn fill_the_apps_ranking(now_ns: Timestamp) {
        for index in 0..MAX_NAMED_ORIGINS {
            let uses = if index == 0 { 5 } else { 1 };
            for _ in 0..uses {
                let labels = Labels::listed_app(&app(index), None, now_ns);
                count_lost(Reason::UnknownPrincipal, &labels);
            }
        }
    }

    #[test]
    fn an_origin_is_named_only_once_a_listed_sender_used_it() {
        reset_for_testing();
        assert_eq!(origin_of(&Labels::app(&app(1), None)), OTHER);

        Labels::listed_app(&app(1), None, NOW);

        assert_eq!(origin_of(&Labels::app(&app(1), None)), app(1));
    }

    #[test]
    fn an_unlisted_caller_takes_no_place_in_the_apps_ranking() {
        reset_for_testing();
        for index in 0..=MAX_NAMED_ORIGINS {
            let labels = Labels::app(&app(index), None);
            count_unlisted_sender(Principal::anonymous(), &app(index), &labels, NOW);
        }

        assert!(with(|counters| counters.apps.scores.is_empty()));
        assert_eq!(
            origin_of(&Labels::listed_app("https://listed.example", None, NOW)),
            "https://listed.example"
        );
    }

    #[test]
    fn a_newcomer_replaces_the_least_used_app_and_keeps_the_busiest() {
        reset_for_testing();
        fill_the_apps_ranking(NOW);
        let losses = total_losses();

        Labels::listed_app("https://newcomer.example", None, NOW);

        assert!(is_ranked(&app(0)));
        assert_eq!(
            (1..MAX_NAMED_ORIGINS)
                .filter(|index| !is_ranked(&app(*index)))
                .count(),
            1
        );
        // The replaced app's series went, and nothing it counted moved to `other`.
        assert_eq!(total_losses(), losses - 1);
        assert!(with(|counters| counters
            .losses
            .keys()
            .all(|(_, labels)| origin_of(labels) != OTHER)));
    }

    #[test]
    fn an_app_busy_long_ago_gives_way_to_apps_busy_lately() {
        reset_for_testing();
        for _ in 0..10 {
            Labels::listed_app(&app(0), None, NOW);
        }
        for index in 1..MAX_NAMED_ORIGINS {
            for _ in 0..2 {
                Labels::listed_app(&app(index), None, NOW + 7 * DAY);
            }
        }

        Labels::listed_app("https://newcomer.example", None, NOW + 7 * DAY);

        assert!(!is_ranked(&app(0)));
    }

    #[test]
    fn a_replaced_app_in_flight_is_counted_under_other_from_then_on() {
        reset_for_testing();
        let labels = Labels::listed_app(&app(0), None, NOW);
        number_application(1, &app(0));
        let entry = entry(7, EXPIRES);
        track_fan_out(1, &entry, labels, vec![deposited()], NOW);
        with(|counters| {
            counters.apps.remove(&app(0));
            counters.drop_app(&app(0));
        });

        count_collected(1, &entry, Path::Chrome, NOW);

        assert!(with(|counters| counters
            .pushes
            .keys()
            .all(|(_, _, labels)| origin_of(labels) == OTHER)));
        assert_eq!(push_count_for_testing("delivered"), 1);
    }

    #[test]
    fn labels_taken_before_their_app_gave_way_count_under_other() {
        reset_for_testing();
        let labels = Labels::listed_app(&app(0), None, NOW);
        with(|counters| {
            counters.apps.remove(&app(0));
            counters.drop_app(&app(0));
        });

        count_lost(Reason::UnknownPrincipal, &labels);

        assert!(with(|counters| counters
            .losses
            .keys()
            .all(|(_, labels)| origin_of(labels) == OTHER)));
    }

    #[test]
    fn an_unlisted_pair_gives_way_by_use_and_its_series_goes() {
        const ORIGIN: &str = "https://unlisted.example";
        reset_for_testing();
        let canister = |index: u64| Principal::from_slice(&index.to_be_bytes());
        let labels = Labels::app(ORIGIN, None);
        for index in 0..MAX_NAMED_UNLISTED_SENDERS as u64 {
            let uses = if index == 0 { 3 } else { 1 };
            for _ in 0..uses {
                count_unlisted_sender(canister(index), ORIGIN, &labels, NOW);
            }
        }

        count_unlisted_sender(canister(999), ORIGIN, &labels, NOW);

        let refused = with(|counters| counters.unlisted_senders.clone());
        let series = |index: u64| {
            (
                canister(index).to_text(),
                labels.with_origin(ORIGIN.to_string()),
            )
        };
        assert_eq!(refused.len(), MAX_NAMED_UNLISTED_SENDERS);
        assert_eq!(refused.values().sum::<u64>(), 102);
        assert_eq!(refused.get(&series(0)), Some(&3));
        assert_eq!(refused.get(&series(999)), Some(&1));
    }

    /// Counts push losses for every ranked app, busiest first, past the series cap.
    fn fill_the_series() {
        let reasons = [
            Reason::RelayGone,
            Reason::RelayRejected,
            Reason::RelayServerError,
            Reason::NeverFetched,
            Reason::FetchedTooLate,
        ];
        let urgencies = [
            Urgency::High,
            Urgency::Normal,
            Urgency::Low,
            Urgency::VeryLow,
        ];
        for index in 0..MAX_NAMED_ORIGINS {
            for urgency in &urgencies {
                let labels = Labels::app(&app(index), Some(urgency));
                for reason in reasons {
                    for path in Path::ALL {
                        with(|counters| counters.count_push_lost(reason, path, &labels));
                    }
                }
            }
        }
    }

    fn encoded_reply() -> Vec<u8> {
        let mut writer = MetricsEncoder::new(vec![], i64::MAX);
        encode(&mut writer, usize::MAX, NOW).expect("the metrics encode");
        writer.into_inner()
    }

    #[test]
    fn the_reply_stays_within_its_budget_at_the_longest_origins() {
        reset_for_testing();
        fill_the_apps_ranking(NOW);
        fill_the_series();

        assert!(with(|counters| counters.series_folded) > 0);
        let series_bytes = with(|counters| counters.series_bytes);
        let reply = encoded_reply();
        // What the cap counts covers every series line the reply holds.
        let series_lines: usize = String::from_utf8_lossy(&reply)
            .lines()
            .filter(|line| !line.starts_with('#') && line.contains('{'))
            .filter(|line| !line.starts_with("internet_identity_notifications_in_flight"))
            .map(|line| line.len() + 1)
            .sum();
        assert!(
            series_lines <= series_bytes,
            "{series_lines} > {series_bytes}"
        );
        // Past the cap only series under `other` are created: this fill's push losses and
        // the loss each app was ranked with.
        let other_bytes = (4 * 5 * Path::COUNT + 1) * 300;
        assert!(
            series_bytes <= MAX_SERIES_BYTES + other_bytes,
            "{series_bytes} bytes"
        );
        assert!(reply.len() < 2 * 1024 * 1024, "{} bytes", reply.len());
    }

    #[test]
    fn past_the_cap_a_busier_app_makes_room_by_dropping_a_quieter_one() {
        reset_for_testing();
        fill_the_apps_ranking(NOW);
        fill_the_series();
        let ranked_apps = with(|counters| counters.apps.scores.len());

        let labels = Labels::app(&app(0), Some(&Urgency::High));
        with(|counters| counters.count_lost(Reason::BacklogFull, &labels));

        assert!(with(|counters| counters
            .losses
            .contains_key(&(Reason::BacklogFull, labels.clone()))));
        assert!(with(|counters| counters.apps.scores.len()) < ranked_apps);
        assert!(is_ranked(&app(0)));
    }

    #[test]
    fn past_the_cap_the_quietest_app_counts_a_new_series_under_other() {
        reset_for_testing();
        fill_the_apps_ranking(NOW);
        fill_the_series();
        let (lowest, _) = with(|counters| counters.apps.lowest()).expect("a ranked app");
        let folded = with(|counters| counters.series_folded);

        let labels = Labels::app(&lowest, Some(&Urgency::High));
        with(|counters| counters.count_lost(Reason::BacklogFull, &labels));

        assert_eq!(with(|counters| counters.series_folded), folded + 1);
        assert!(with(|counters| counters
            .losses
            .contains_key(&(Reason::BacklogFull, labels.fold_into_other()))));
    }

    /// Fills the budget to the byte, past what the longest lines leave of it.
    fn fill_the_budget() {
        fill_the_apps_ranking(NOW);
        fill_the_series();
        with(|counters| counters.series_bytes = counters.series_bytes.max(MAX_SERIES_BYTES));
    }

    #[test]
    fn past_the_cap_unlisted_callers_share_the_other_series() {
        reset_for_testing();
        fill_the_budget();
        let urgencies = [
            Urgency::High,
            Urgency::Normal,
            Urgency::Low,
            Urgency::VeryLow,
        ];

        for index in 0..200u64 {
            let canister = Principal::from_slice(&index.to_be_bytes());
            let origin = format!("https://unlisted-{index}.example");
            for urgency in &urgencies {
                let labels = Labels::app(&origin, Some(urgency));
                count_unlisted_sender(canister, &origin, &labels, NOW);
            }
        }

        let refused = with(|counters| counters.unlisted_senders.clone());
        assert!(refused.len() <= urgencies.len(), "{} series", refused.len());
        assert!(refused
            .keys()
            .all(|(canister, labels)| canister == OTHER && origin_of(labels) == OTHER));
        assert_eq!(refused.values().sum::<u64>(), 800);
    }

    #[test]
    fn past_the_cap_only_a_series_that_loses_its_name_counts_as_folded() {
        reset_for_testing();
        fill_the_budget();
        let folded = with(|counters| counters.series_folded);

        let identity = Labels::identity(&Urgency::High);
        let unranked = Labels::app("https://unranked.example", Some(&Urgency::High));
        with(|counters| {
            counters.count_lost(Reason::BacklogFull, &identity);
            counters.count_lost(Reason::BacklogFull, &unranked);
        });
        assert_eq!(with(|counters| counters.series_folded), folded);

        let (lowest, _) = with(|counters| counters.apps.lowest()).expect("a ranked app");
        let quiet = Labels::app(&lowest, Some(&Urgency::High));
        with(|counters| counters.count_lost(Reason::BacklogFull, &quiet));
        assert_eq!(with(|counters| counters.series_folded), folded + 1);
    }

    #[test]
    fn delivery_paths_are_read_off_the_endpoint() {
        let path = |endpoint| Path::of(DeliveryTarget::Browser, endpoint);
        assert_eq!(
            path("https://fcm.googleapis.com/fcm/send/abc"),
            Path::Chrome
        );
        assert_eq!(path("https://web.push.apple.com/abc"), Path::Safari);
        assert_eq!(
            path("https://updates.push.services.mozilla.com/wpush/v2/abc"),
            Path::Firefox
        );
        assert_eq!(
            path("https://wns2-par02p.notify.windows.com/w/?token=abc"),
            Path::Edge
        );
        assert_eq!(path("https://relay.example/abc"), Path::Other);
    }
}
