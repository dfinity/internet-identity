use crate::storage::account::Session;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::duration::StorableDuration;
use crate::storage::storable::timestamp::StorableTimestamp;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Debug, Ord, Eq, PartialEq, PartialOrd)]
#[cbor(map)]
pub struct StorableSession {
    #[n(0)]
    pub created_at_ns: StorableTimestamp,
    #[n(1)]
    pub valid_till_ns: StorableTimestamp,
    #[n(2)]
    pub max_idle_ns: StorableDuration,
    #[n(3)]
    pub last_refreshed_ns: Option<StorableTimestamp>,
    #[n(4)]
    pub browser_id: StorableBrowserId,
    #[n(5)]
    pub read_only: bool,
}

impl Storable for StorableSession {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSession");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSession")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableSession> for Session {
    fn from(value: StorableSession) -> Self {
        Session {
            created_at_ns: value.created_at_ns,
            valid_till_ns: value.valid_till_ns,
            last_refreshed_ns: value.last_refreshed_ns,
            max_idle_ns: value.max_idle_ns,
            browser_id: value.browser_id,
            read_only: value.read_only,
        }
    }
}

impl From<Session> for StorableSession {
    fn from(value: Session) -> Self {
        StorableSession {
            created_at_ns: value.created_at_ns,
            valid_till_ns: value.valid_till_ns,
            last_refreshed_ns: value.last_refreshed_ns,
            max_idle_ns: value.max_idle_ns,
            browser_id: value.browser_id,
            read_only: value.read_only,
        }
    }
}
