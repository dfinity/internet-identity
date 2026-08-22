use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::application_number::StorableApplicationNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The triple that identifies one account. Absent account number means the default.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableAccountLocator {
    #[n(0)]
    pub anchor_number: StorableAnchorNumber,
    #[n(1)]
    pub application_number: StorableApplicationNumber,
    #[n(2)]
    pub account_number: Option<StorableAccountNumber>,
}

impl Storable for StorableAccountLocator {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountLocator");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountLocator")
    }

    const BOUND: Bound = Bound::Unbounded;
}
