use crate::storage::account::AccountReference;
use crate::storage::storable::account_reference::StorableAccountReference;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Vectors are not supported yet in ic-stable-structures, this file
/// implements a struct to wrap this vector so it can be stored.
#[derive(Encode, Decode, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
#[cbor(transparent)]
pub struct StorableAccountReferenceList(#[n(0)] Vec<StorableAccountReference>);

impl Storable for StorableAccountReferenceList {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountReferenceList");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountReferenceList")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl StorableAccountReferenceList {
    pub fn into_vec(self) -> Vec<StorableAccountReference> {
        self.0
    }
}

impl From<StorableAccountReferenceList> for Vec<AccountReference> {
    fn from(value: StorableAccountReferenceList) -> Self {
        value
            .0
            .iter()
            .cloned()
            .map(AccountReference::from)
            .collect()
    }
}

impl From<Vec<AccountReference>> for StorableAccountReferenceList {
    fn from(value: Vec<AccountReference>) -> Self {
        StorableAccountReferenceList(
            value
                .iter()
                .cloned()
                .map(StorableAccountReference::from)
                .collect(),
        )
    }
}
