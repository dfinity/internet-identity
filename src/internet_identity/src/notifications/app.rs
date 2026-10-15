//! The record a Home Screen app has of its own.
//!
//! On iOS web push exists only inside an installed web app, which runs in a storage
//! partition Safari cannot reach and holds a key it generated there. That key is not a
//! browser key and never becomes one: the app never signs in, holds no session, and is
//! not something the user signs out of.
//!
//! So it is not an entry in an anchor's browser list. It is one record, here, that
//! browsers point at. One installed app is one record however many identities it serves,
//! which is what lets a second identity be delivered to the moment it points at the same
//! app rather than waiting for the app to be opened again.

use crate::storage::anchor::WebPushSubscription;
use internet_identity_interface::internet_identity::types::{PublicKey, Timestamp};

/// Numbers a notification app record. Handed out by a monotonic counter and never
/// reissued, so a browser still pointing at a deleted record can never be handed
/// somebody else's app by that reference.
pub type NotificationAppId = u64;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NotificationApp {
    pub id: NotificationAppId,
    /// The key the app generated in its own partition, DER-encoded. What the record is
    /// looked up by, and what the app signs its own calls with.
    pub key: PublicKey,
    /// When the app first opened and claimed this record, which is also the first moment
    /// anything knew it had been installed.
    pub created_at: Timestamp,
    /// When the app last called. Stamped by the app's own update calls, which is
    /// `link_notification_app` alone: a query cannot write, so an app asking a question
    /// leaves this where it was.
    pub last_used: Timestamp,
    /// What the app registered for Web Push, if anything.
    ///
    /// One subscription for the whole record, because the app has one: it is a single
    /// installed web app with a single service worker registration, whatever number of
    /// identities point at it. Its absence is what says the app has been opened but has
    /// no permission yet.
    pub webpush_subscription: Option<WebPushSubscription>,
}

/// What a browser holds about the app it points at.
///
/// The reference outlives signing out and turning notifications off. Both stop routing
/// without forgetting which app this browser has, so coming back is signing in or
/// flipping the switch rather than installing anything again.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NotificationAppRef {
    pub app_id: NotificationAppId,
    /// Whether this identity wants its notifications delivered through the app. The
    /// subscription is the app's and is shared, so turning off here can never be a
    /// subscription removal: that would take every other identity down with it.
    pub enabled: bool,
}

/// What a browser holds about the app it points at.
///
/// The reference outlives signing out and turning delivery off. Both stop routing
/// without forgetting which app this browser has, so coming back is signing in or
/// flipping the switch rather than installing anything again.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NotificationAppRef {
    pub app_id: NotificationAppId,
    /// Whether this identity wants its notifications delivered through the app. The
    /// subscription is the app's and is shared, so turning off here can never be a
    /// subscription removal: that would take every other identity down with it.
    pub enabled: bool,
}
