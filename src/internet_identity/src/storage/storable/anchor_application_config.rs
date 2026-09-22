use crate::storage::storable::account_number::StorableAccountNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Default, Clone, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct AnchorApplicationConfig {
    #[n(0)]
    pub default_account_number: Option<StorableAccountNumber>, // None is the unreserved synthetic account
    #[n(1)]
    pub notifications_consented_at_ns: Option<Timestamp>, // None if the app may not notify
}

impl Storable for AnchorApplicationConfig {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode AnchorApplicationConfig");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode AnchorApplicationConfig")
    }

    const BOUND: Bound = Bound::Unbounded;
}
