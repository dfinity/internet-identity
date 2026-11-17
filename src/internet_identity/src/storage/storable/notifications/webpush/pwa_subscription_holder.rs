use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::browser_id::StorableBrowserId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The installed app entry holding a push endpoint: the identity, the application, and the
/// browser entry and account the entry is keyed by on that identity's application config.
#[derive(Encode, Decode, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
// An array rather than a map: a derived index is rebuilt from the configs it mirrors, so it
// has no rows written before a field existed to decode.
#[cbor(array)]
pub struct StorablePwaSubscriptionHolder {
    #[n(0)]
    pub anchor_number: StorableAnchorNumber,
    #[n(1)]
    pub application_number: StorableApplicationNumber,
    #[n(2)]
    pub browser_id: StorableBrowserId,
    #[n(3)]
    pub account_number: Option<StorableAccountNumber>,
}

impl Storable for StorablePwaSubscriptionHolder {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer)
            .expect("failed to encode StorablePwaSubscriptionHolder");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePwaSubscriptionHolder")
    }

    /// The array header, two maximal `u64`s at nine bytes, a maximal `u32` at five and a
    /// maximal `u64` at nine.
    const BOUND: Bound = Bound::Bounded {
        max_size: 33,
        is_fixed_size: false,
    };
}
