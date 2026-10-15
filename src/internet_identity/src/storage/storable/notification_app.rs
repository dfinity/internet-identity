use crate::notifications::app::{NotificationApp, NotificationAppId};
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
}

impl From<NotificationApp> for StorableNotificationApp {
    fn from(value: NotificationApp) -> Self {
        StorableNotificationApp {
            id: value.id,
            key: value.key.into_vec(),
            created_at: value.created_at,
            last_used: value.last_used,
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
