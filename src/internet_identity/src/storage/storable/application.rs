use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use minicbor::{Decode, Encode};
use std::{
    borrow::Cow,
    fmt::{self, Display},
};

use crate::utils::{sha256sum, slice_to_bounded_32};

#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableApplication {
    #[n(0)]
    pub origin: FrontendHostname,
    #[n(1)]
    pub stored_accounts: u64,
    #[n(2)]
    pub stored_account_references: u64,
    /// Rows that exist here while holding no reference at all.
    ///
    /// A row holding nothing is a tombstone: it says every account an identity had at
    /// this origin was moved away and its default must never be derived again. It
    /// contributes nothing to `stored_account_references`, so without counting it
    /// separately this application would look unreferenced and be retired — and the next
    /// visit would mint a fresh application number the tombstone no longer applies to,
    /// handing the identity back the default it had moved away from.
    ///
    /// A field added here decodes as absent for every application already stored, and
    /// `0` is the truth for those: nothing can write an empty list yet.
    #[n(3)]
    pub tombstones: u64,
}

impl Storable for StorableApplication {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableApplication");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableApplication")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableOriginSha256 {
    hash: [u8; 32],
}

impl Display for StorableOriginSha256 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex::encode(self.hash))
    }
}

impl StorableOriginSha256 {
    pub fn from_origin(origin: &FrontendHostname) -> Self {
        let hash = sha256sum(origin.as_bytes());
        Self { hash }
    }
}

impl Storable for StorableOriginSha256 {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(self.hash.to_vec())
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        let hash = slice_to_bounded_32(bytes.as_ref());
        Self { hash }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 32,
        is_fixed_size: true,
    };
}
