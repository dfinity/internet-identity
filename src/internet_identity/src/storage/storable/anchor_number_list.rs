use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::AnchorNumber;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Vectors are not supported yet in ic-stable-structures, this file
/// implements a struct to wrap this vector so it can be stored.
#[derive(Encode, Decode, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
#[cbor(transparent)]
pub struct StorableAnchorNumberList(#[n(0)] Vec<StorableAnchorNumber>);

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
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAnchorNumberList");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAnchorNumberList")
    }

    const BOUND: Bound = Bound::Unbounded;
}
