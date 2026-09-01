use crate::storage::storable::session_device_id::StorableSessionDeviceId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableSessionDevice {
    #[n(0)]
    pub id: StorableSessionDeviceId,
    #[n(1)]
    pub name: String,
    #[n(2)]
    pub created_at: Timestamp,
    #[n(3)]
    pub last_used: Timestamp,
    #[cbor(n(4), with = "minicbor::bytes")]
    pub current_device_key: Vec<u8>,
    #[cbor(n(5), with = "minicbor::bytes")]
    pub next_device_key: Vec<u8>,
}

impl Storable for StorableSessionDevice {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSessionDevice");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSessionDevice")
    }

    const BOUND: Bound = Bound::Unbounded;
}
