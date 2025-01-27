use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::AnchorNumber;
use serde::Deserialize;
use std::borrow::Cow;

/// Vectors are not supported yet in ic-stable-structures, this file
/// implements a struct to wrap this vector so it can be stored.
#[derive(Deserialize, CandidType, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
pub struct StorableAnchorNumberList(Vec<AnchorNumber>);

impl From<StorableAnchorNumberList> for Vec<AnchorNumber> {
    fn from(value: StorableAnchorNumberList) -> Self {
        value.0
    }
}

impl From<Vec<AnchorNumber>> for StorableAnchorNumberList {
    fn from(value: Vec<AnchorNumber>) -> Self {
        StorableAnchorNumberList(value)
    }
}

impl Storable for StorableAnchorNumberList {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut candid = candid::encode_one(self)
            .expect("Failed to serialize StorableAnchorNumberList to candid");
        let mut buf = (candid.len() as u16).to_le_bytes().to_vec(); // 2 bytes for length
        buf.append(&mut candid);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let length = u16::from_le_bytes(bytes[..2].try_into().unwrap()) as usize;

        candid::decode_one(&bytes[2..length + 2])
            .expect("Failed to deserialize StorableAnchorNumberList from candid")
    }

    const BOUND: Bound = Bound::Unbounded;
}
