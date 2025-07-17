use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableAccount {
    #[n(0)]
    pub name: String,
    #[n(1)]
    pub seed_from_anchor: Option<StorableAnchorNumber>, // Set if this is a default account
}

impl Storable for StorableAccount {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccount");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccount")
    }

    const BOUND: Bound = Bound::Unbounded;
}
