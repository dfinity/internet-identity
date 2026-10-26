//! The record a Home Screen app has of its own, which browsers point at. One installed
//! app is one record however many identities it serves.

use crate::storage::anchor::WebPushSubscription;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};
use std::collections::BTreeSet;

pub use internet_identity_interface::internet_identity::types::NotificationAppId;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NotificationApp {
    pub id: NotificationAppId,
    /// The principal the app signs as, self-authenticating over the key it generated in
    /// its own partition. What an app-facing caller is matched against.
    pub principal: Principal,
    pub created_at_ns: Timestamp,
    /// Stamped by the app's own update calls only, so a query leaves it where it was.
    pub last_used_ns: Timestamp,
    /// The identities whose browsers point at this app. Also the reference count: a
    /// record with none left is deleted.
    pub anchors: BTreeSet<AnchorNumber>,
    /// One subscription for the whole record, absent until the app has permission.
    pub webpush_subscription: Option<WebPushSubscription>,
}

/// What a browser holds about a Home Screen app: an install it has handed a key to, or
/// the record it ended up pointing at. Never both.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NotificationAppRef {
    /// An install this browser started. The principal is the one-shot key it put in the
    /// URL fragment, and the only caller that may complete the link.
    Pending {
        link_principal: Principal,
        expires_at_ns: Timestamp,
    },
    /// The record this browser points at. Outlives signing out and turning delivery off,
    /// so coming back is signing in or flipping the switch rather than installing again.
    Linked {
        app_id: NotificationAppId,
        /// The subscription is the app's and is shared, so turning off never removes it.
        enabled: bool,
    },
}

/// The record `app_id` names, or `None` where the deployment does not notify, so a
/// deployment with notifications off never reports an app however its anchors read.
pub fn record(app_id: NotificationAppId) -> Option<NotificationApp> {
    if !super::notifications_enabled() {
        return None;
    }
    crate::state::storage_borrow(|storage| storage.notification_app(app_id))
}

impl NotificationAppRef {
    /// The record this browser points at, where the install has completed.
    pub fn linked_app(&self) -> Option<NotificationAppId> {
        match self {
            NotificationAppRef::Pending { .. } => None,
            NotificationAppRef::Linked { app_id, .. } => Some(*app_id),
        }
    }

    /// Whether this identity wants the app to deliver for it on this device. An install
    /// that has not completed delivers nothing either.
    pub fn is_delivering(&self) -> bool {
        match self {
            NotificationAppRef::Pending { .. } => false,
            NotificationAppRef::Linked { enabled, .. } => *enabled,
        }
    }
}
