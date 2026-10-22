use crate::notifications::app::{
    NotificationApp, NotificationAppId, NotificationAppRef, PendingProbe,
};
use crate::storage::anchor::WebPushSubscription;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use candid::Principal;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::BTreeSet;

pub type StorableNotificationAppId = NotificationAppId;

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableNotificationApp {
    #[n(0)]
    pub id: StorableNotificationAppId,
    #[cbor(n(1), with = "minicbor::bytes")]
    pub principal: Vec<u8>,
    #[n(2)]
    pub created_at: Timestamp,
    #[n(3)]
    pub last_used: Timestamp,
    #[n(4)]
    pub anchors: BTreeSet<AnchorNumber>,
    #[n(5)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
    /// The last time an entry asked this app to prove it is still installed.
    #[n(6)]
    pub pending_probe: Option<StorablePendingProbe>,
}

/// A probe an entry sent to this record, and when it sent it.
#[derive(Encode, Decode, Clone, Copy, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorablePendingProbe {
    #[n(0)]
    pub notification_id: u64,
    #[n(1)]
    pub asked_at_ns: Timestamp,
}

impl From<NotificationApp> for StorableNotificationApp {
    fn from(value: NotificationApp) -> Self {
        StorableNotificationApp {
            id: value.id,
            principal: value.principal.as_slice().to_vec(),
            created_at: value.created_at,
            last_used: value.last_used,
            anchors: value.anchors,
            webpush_subscription: value
                .webpush_subscription
                .map(StorableWebPushSubscription::from),
            pending_probe: value.pending_probe.map(|probe| StorablePendingProbe {
                notification_id: probe.notification_id,
                asked_at_ns: probe.asked_at_ns,
            }),
        }
    }
}

impl TryFrom<StorableNotificationApp> for NotificationApp {
    type Error = String;

    /// Fallible for the registration it may carry, which asserts the shape of its own
    /// fields.
    fn try_from(value: StorableNotificationApp) -> Result<Self, Self::Error> {
        Ok(NotificationApp {
            id: value.id,
            principal: Principal::from_slice(&value.principal),
            created_at: value.created_at,
            last_used: value.last_used,
            anchors: value.anchors,
            webpush_subscription: value
                .webpush_subscription
                .map(WebPushSubscription::try_from)
                .transpose()?,
            pending_probe: value.pending_probe.map(|probe| PendingProbe {
                notification_id: probe.notification_id,
                asked_at_ns: probe.asked_at_ns,
            }),
        })
    }
}

impl Storable for StorableNotificationApp {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableNotificationApp");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableNotificationApp")
    }

    const BOUND: Bound = Bound::Unbounded;
}

/// What a browser entry holds about a Home Screen app: an install it has handed a
/// one-shot key to, or the record it ended up pointing at.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableNotificationAppRef {
    #[n(0)]
    Pending(
        #[cbor(n(0), with = "minicbor::bytes")] Vec<u8>,
        #[n(1)] Timestamp,
    ),
    #[n(1)]
    Linked(#[n(0)] StorableNotificationAppId, #[n(1)] bool),
}

impl From<NotificationAppRef> for StorableNotificationAppRef {
    fn from(value: NotificationAppRef) -> Self {
        match value {
            NotificationAppRef::Pending {
                link_principal,
                expires_at_ns,
            } => StorableNotificationAppRef::Pending(
                link_principal.as_slice().to_vec(),
                expires_at_ns,
            ),
            NotificationAppRef::Linked { app_id, enabled } => {
                StorableNotificationAppRef::Linked(app_id, enabled)
            }
        }
    }
}

impl From<StorableNotificationAppRef> for NotificationAppRef {
    fn from(value: StorableNotificationAppRef) -> Self {
        match value {
            StorableNotificationAppRef::Pending(link_principal, expires_at_ns) => {
                NotificationAppRef::Pending {
                    link_principal: Principal::from_slice(&link_principal),
                    expires_at_ns,
                }
            }
            StorableNotificationAppRef::Linked(app_id, enabled) => {
                NotificationAppRef::Linked { app_id, enabled }
            }
        }
    }
}
