use crate::storage::anchor::Device;
use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::MetadataEntry;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Clone, Debug, Default, CandidType, Deserialize, Eq, PartialEq)]
pub struct StorableFixedAnchor {
    pub devices: Vec<Device>,
    pub metadata: Option<HashMap<String, MetadataEntry>>,
}

impl Storable for StorableFixedAnchor {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut candid =
            candid::encode_one(self).expect("Failed to serialize StorableAnchor to candid");
        let mut buf = (candid.len() as u16).to_le_bytes().to_vec(); // 2 bytes for length
        buf.append(&mut candid);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let length = u16::from_le_bytes(bytes[..2].try_into().unwrap()) as usize;

        candid::decode_one(&bytes[2..length + 2])
            .expect("Failed to deserialize StorableAnchor from candid")
    }

    const BOUND: Bound = Bound::Unbounded;
}
