use crate::notifications::app::{NotificationApp, NotificationAppId};
use crate::storage::anchor::WebPushSubscription;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use serde_bytes::ByteBuf;
use std::borrow::Cow;

pub type StorableNotificationAppId = NotificationAppId;

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableNotificationApp {
    #[n(0)]
    pub id: StorableNotificationAppId,
    #[cbor(n(1), with = "minicbor::bytes")]
    pub key: Vec<u8>,
    #[n(2)]
    pub created_at: Timestamp,
    #[n(3)]
    pub last_used: Timestamp,
    #[n(4)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
}

impl From<NotificationApp> for StorableNotificationApp {
    fn from(value: NotificationApp) -> Self {
        StorableNotificationApp {
            id: value.id,
            key: value.key.into_vec(),
            created_at: value.created_at,
            last_used: value.last_used,
            webpush_subscription: value
                .webpush_subscription
                .map(StorableWebPushSubscription::from),
        }
    }
}

impl From<StorableNotificationApp> for NotificationApp {
    fn from(value: StorableNotificationApp) -> Self {
        NotificationApp {
            id: value.id,
            key: ByteBuf::from(value.key),
            created_at: value.created_at,
            last_used: value.last_used,
            webpush_subscription: value.webpush_subscription.map(WebPushSubscription::from),
        }
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

/// What a browser entry holds about the app it points at: which record, and whether this
/// identity wants that app to deliver for it.
///
/// The switch rides on the reference rather than on the record, because the record is
/// shared by every identity pointing at it while the answer is each identity's own.
#[derive(Encode, Decode, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableNotificationAppRef {
    #[n(0)]
    pub app_id: StorableNotificationAppId,
    #[n(1)]
    pub enabled: bool,
}

impl From<NotificationAppRef> for StorableNotificationAppRef {
    fn from(value: NotificationAppRef) -> Self {
        StorableNotificationAppRef {
            app_id: value.app_id,
            enabled: value.enabled,
        }
    }
}

impl From<StorableNotificationAppRef> for NotificationAppRef {
    fn from(value: StorableNotificationAppRef) -> Self {
        NotificationAppRef {
            app_id: value.app_id,
            enabled: value.enabled,
        }
    }
}
