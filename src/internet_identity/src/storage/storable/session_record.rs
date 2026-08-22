use crate::storage::account::SessionRecord;
use crate::storage::storable::session_device_id::StorableSessionDeviceId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Debug, Ord, Eq, PartialEq, PartialOrd)]
#[cbor(map)]
pub struct StorableSessionRecord {
    #[n(0)]
    pub created_at: Timestamp,
    #[n(1)]
    pub valid_till: Timestamp,
    #[n(2)]
    pub last_refreshed: Option<Timestamp>,
    #[n(3)]
    pub device_id: StorableSessionDeviceId,
    #[n(4)]
    pub read_only: bool,
}

impl Storable for StorableSessionRecord {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSessionRecord");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSessionRecord")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableSessionRecord> for SessionRecord {
    fn from(value: StorableSessionRecord) -> Self {
        SessionRecord {
            created_at: value.created_at,
            valid_till: value.valid_till,
            last_refreshed: value.last_refreshed,
            device_id: value.device_id,
            read_only: value.read_only,
        }
    }
}

impl From<SessionRecord> for StorableSessionRecord {
    fn from(value: SessionRecord) -> Self {
        StorableSessionRecord {
            created_at: value.created_at,
            valid_till: value.valid_till,
            last_refreshed: value.last_refreshed,
            device_id: value.device_id,
            read_only: value.read_only,
        }
    }
}
