use crate::storage::account::AccountReference;
use crate::storage::storable::account_number::StorableAccountNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
#[cbor(map)]
pub struct StorableAccountReference {
    #[n(0)]
    pub account_number: Option<StorableAccountNumber>, // None is the unreserved default account
    #[n(1)]
    pub last_used: Option<Timestamp>,
}

impl Storable for StorableAccountReference {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountReference");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountReference")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableAccountReference> for AccountReference {
    fn from(value: StorableAccountReference) -> Self {
        AccountReference {
            account_number: value.account_number,
            last_used: value.last_used,
        }
    }
}

impl From<AccountReference> for StorableAccountReference {
    fn from(value: AccountReference) -> Self {
        StorableAccountReference {
            account_number: value.account_number,
            last_used: value.last_used,
        }
    }
}
