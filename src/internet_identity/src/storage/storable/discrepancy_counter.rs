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
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableDiscrepancyCounter");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableDiscrepancyCounter")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl StorableDiscrepancyCounter {
    pub fn increment(&self, discrepancy_type: &DiscrepancyType) -> Self {
        match discrepancy_type {
            DiscrepancyType::AccountRebuild => Self {
                account_counter_rebuilds: self
                    .account_counter_rebuilds
                    .checked_add(1)
                    .expect("overflow in account_counter_rebuilds"),
            },
        }
    }
}

pub enum DiscrepancyType {
    AccountRebuild,
}
