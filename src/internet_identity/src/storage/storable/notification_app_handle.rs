use crate::notifications::app::NotificationAppId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// What a browser hands to another identity on the same device so it can point at the
/// same app. Opaque, unguessable, and good for one claim.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableNotificationAppHandle(pub [u8; 32]);

impl Storable for StorableNotificationAppHandle {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Borrowed(&self.0)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        let mut handle = [0u8; 32];
        handle.copy_from_slice(&bytes);
        StorableNotificationAppHandle(handle)
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 32,
        is_fixed_size: true,
    };
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableNotificationAppOffer {
    #[n(0)]
    pub app_id: NotificationAppId,
    #[n(1)]
    pub expires_at_ns: Timestamp,
}

impl Storable for StorableNotificationAppOffer {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableNotificationAppOffer");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableNotificationAppOffer")
    }

    const BOUND: Bound = Bound::Unbounded;
}
