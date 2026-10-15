//! What an app passes in and reads back. Everything else is internal.

use candid::{CandidType, Principal};
use serde::{Deserialize, Serialize};

/// How Internet Identity should schedule a notification against the origin's
/// other traffic. Not a rendering hint: channels without a tray honour it too.
#[derive(CandidType, Serialize, Deserialize, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Urgency {
    VeryLow,
    Low,
    #[default]
    Normal,
    High,
}

/// What the app describes when it sends.
///
/// ```
/// use identity_notifications::{Notification, Urgency};
///
/// let notification = Notification {
///     title: "New message".into(),
///     body: "See you at six".into(),
///     url: Some("https://chat.example.com/chats/alice".into()),
///     key: Some("chat-alice".into()),
///     urgency: Some(Urgency::High),
///     ..Default::default()
/// };
/// ```
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Notification {
    pub title: String,
    pub body: String,
    /// Where acting on it takes them. Must be on the sending origin or one of
    /// its alternative origins; Internet Identity refuses anything else.
    pub url: Option<String>,
    /// What this notification is about. Sending again with the same key
    /// updates that notification rather than adding another, and it is what
    /// [`crate::dismiss`] names. Derive it from data the sender cannot choose.
    pub key: Option<String>,
    /// `None` is [`Urgency::Normal`].
    pub urgency: Option<Urgency>,
    /// `None` is Internet Identity's default retention, which is also its
    /// ceiling. Nanoseconds since the epoch.
    pub expires_at: Option<u64>,
    /// Which funnel to count this notification in, on top of the hour it was
    /// sent in. One campaign, one kind of notification — whatever the app wants
    /// to read a conversion rate for. A funnel costs a row until it is
    /// forgotten, so a label per notification is an unbounded set of
    /// one-notification funnels.
    pub funnel: Option<String>,
}

/// What a channel pulls back. The rest of a [`Notification`] never leaves this
/// canister.
#[derive(CandidType, Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Content {
    pub title: String,
    pub body: String,
    pub url: Option<String>,
}

/// What became of a set of notifications, counted once each and attributed to
/// where they came in rather than to when each step happened, so the three
/// stages read as one funnel: of `queued`, how many Internet Identity took, of
/// those how many a channel showed, and of those how many were acted on.
///
/// `deferred` is not a stage — Internet Identity asking to be tried later
/// delays a notification rather than losing it — and a notification still on
/// its way is in none of the later counts yet, so gaps in a window that has not
/// settled are not yet losses.
#[derive(CandidType, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct Counts {
    /// Every call to [`crate::send`], including one the capacity ceiling
    /// refused.
    pub queued: u64,
    pub accepted: u64,
    pub received: u64,
    /// Somebody acted on it: the last stage, and the only one that says the
    /// notification did its job. Counted while the notification's own window is
    /// open, which is the period the app declared it relevant for.
    pub opened: u64,
    /// Given up on: never accepted before its window closed, or accepted and
    /// expired without any channel showing it.
    pub dropped: u64,
    /// How often Internet Identity asked for one to come back later.
    pub deferred: u64,
}

/// One hour of the delivery pipeline.
#[derive(CandidType, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct Bucket {
    /// The instant the hour begins, nanoseconds since the epoch.
    pub start: u64,
    pub counts: Counts,
}

/// What became of the notifications an app labelled with one funnel name.
#[derive(CandidType, Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct Funnel {
    pub name: String,
    pub counts: Counts,
}

#[derive(CandidType, Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Misconfigured {
    /// `notification_origin` is unset or empty.
    Origin,
    /// `notification_sender` is unset or not a principal.
    Sender,
    /// The origin does not publish this canister as one of its senders.
    NotAuthorizedSender,
}

#[derive(CandidType, Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub struct MisconfiguredSince {
    pub why: Misconfigured,
    pub since: u64,
}

#[derive(CandidType, Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Metrics {
    /// Up to 720 buckets, oldest first. Hours with no traffic are absent.
    pub hours: Vec<Bucket>,
    /// One per funnel an app has labelled, until it forgets it.
    pub funnels: Vec<Funnel>,
    /// Notifications waiting for Internet Identity to accept them.
    pub backlog: u64,
    pub misconfigured: Option<MisconfiguredSince>,
}

/// Internet Identity scopes notification ids to `(origin, recipient)`.
pub type NotificationId = u64;

/// Who a pull is for, as Internet Identity signed it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SenderInfo {
    pub origin: String,
    pub account: Principal,
}
