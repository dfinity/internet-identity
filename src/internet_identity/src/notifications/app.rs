//! The record a Home Screen app has of its own, which browsers point at. One installed
//! app is one record however many identities it serves.

use candid::Principal;
use internet_identity_interface::internet_identity::types::{PublicKey, Timestamp};

/// Numbers a notification app record. Never reissued, so a browser pointing at a deleted
/// record cannot reach another app.
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
}
