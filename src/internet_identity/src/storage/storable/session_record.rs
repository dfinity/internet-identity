use crate::storage::account::SessionRecord;
use crate::storage::storable::duration::StorableDuration;
use crate::storage::storable::session_device_id::StorableSessionDeviceId;
use crate::storage::storable::timestamp::StorableTimestamp;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Debug, Ord, Eq, PartialEq, PartialOrd)]
#[cbor(map)]
pub struct StorableSessionRecord {
    #[n(0)]
    pub created_at_ns: StorableTimestamp,
    #[n(1)]
    pub valid_till_ns: StorableTimestamp,
    #[n(2)]
    pub max_idle_ns: Option<StorableDuration>,
    #[n(3)]
    pub last_refreshed_ns: Option<StorableTimestamp>,
    #[n(4)]
    pub device_id: StorableSessionDeviceId,
    #[n(5)]
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
            created_at_ns: value.created_at_ns,
            valid_till_ns: value.valid_till_ns,
            last_refreshed_ns: value.last_refreshed_ns,
            max_idle_ns: value.max_idle_ns,
            device_id: value.device_id,
            read_only: value.read_only,
        }
    }
}

impl From<SessionRecord> for StorableSessionRecord {
    fn from(value: SessionRecord) -> Self {
        StorableSessionRecord {
            created_at_ns: value.created_at_ns,
            valid_till_ns: value.valid_till_ns,
            last_refreshed_ns: value.last_refreshed_ns,
            max_idle_ns: value.max_idle_ns,
            device_id: value.device_id,
            read_only: value.read_only,
        }
    }
}
