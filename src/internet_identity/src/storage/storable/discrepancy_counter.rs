use std::borrow::Cow;

use ic_stable_structures::{storable::Bound, Storable};
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Ord, Eq, PartialEq, PartialOrd, Default)]
#[cbor(map)]
pub struct StorableDiscrepancyCounter {
    #[n(0)]
    pub account_counter_rebuilds: u64,
}

impl Storable for StorableDiscrepancyCounter {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableDiscrepancyCounter");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableDiscrepancyCounter")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl StorableDiscrepancyCounter {}
