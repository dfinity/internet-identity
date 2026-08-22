use crate::storage::storable::session_device_id::StorableSessionDeviceId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Where the session a caller authenticates as is stored.
///
/// The account is named by its principal rather than by its locator because materialising a
/// default account changes the locator and leaves the principal alone, so a rename touches
/// one entry in the principal index instead of every session of that account.
///
/// A browser keeps its id across sign-ins, so the browser alone does not name a session:
/// the creation time is what distinguishes the record this entry was written for from
/// whatever that browser creates later. Both are inputs to the session seed, so an entry
/// can only ever resolve to the one session whose principal is its own key.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableSessionHandle {
    #[cbor(n(0), with = "minicbor::bytes")]
    pub account_principal: Vec<u8>,
    #[n(1)]
    pub device_id: StorableSessionDeviceId,
    #[n(2)]
    pub created_at: u64,
}

impl Storable for StorableSessionHandle {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSessionHandle");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSessionHandle")
    }

    const BOUND: Bound = Bound::Unbounded;
}
