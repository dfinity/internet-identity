//! The record a Home Screen app has of its own, which browsers point at. One installed
//! app is one record however many identities it serves.

use crate::storage::anchor::WebPushSubscription;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{PublicKey, Timestamp};

/// Numbers a notification app record. Never reissued, so a browser pointing at a deleted
/// record cannot reach another app.
///
/// The record carries both this and the key below because they do different jobs: the
/// key is the credential the app authenticates with, this is the handle everything else
/// holds. A reference is `Copy` and eight bytes wide, where the DER-encoded key it names
/// is about ninety, and references are held per browser and per queued notification.
/// Keying by the public key instead would also lose what never reissuing buys: a
/// reinstall mints a new key in a storage partition of its own, so an old reference
/// would dangle with no way to tell a deleted record from one that never existed.
pub type NotificationAppId = u64;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NotificationApp {
    pub id: NotificationAppId,
    /// The key the app generated in its own partition, DER-encoded. The record is looked
    /// up by it.
    pub key: PublicKey,
    pub created_at: Timestamp,
    /// Stamped by the app's own update calls only, so a query leaves it where it was.
    pub last_used: Timestamp,
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
    pub fn delivers(&self) -> bool {
        match self {
            NotificationAppRef::Pending { .. } => false,
            NotificationAppRef::Linked { enabled, .. } => *enabled,
        }
    }
}
