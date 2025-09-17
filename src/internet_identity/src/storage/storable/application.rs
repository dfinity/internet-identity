use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use minicbor::{Decode, Encode};
use sha2::{Digest, Sha256};
use std::{
    borrow::Cow,
    fmt::{self, Display},
};

#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableApplication {
    #[n(0)]
    pub origin: FrontendHostname,
    #[n(1)]
    pub stored_accounts: u64,
    #[n(2)]
    pub stored_account_references: u64,
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

/// Safely converts unbounded slice to a fixed-size slice.
fn slice_to_bounded_32(slice: &[u8]) -> [u8; 32] {
    let mut bounded = [0u8; 32];
    // Don't copy more than 32 bytes
    let copy_len = slice.len().min(32);
    bounded[..copy_len].copy_from_slice(&slice[..copy_len]);
    bounded
}

impl StorableOriginSha256 {
    pub fn from_origin(origin: &FrontendHostname) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(origin.as_bytes());
        let sha256sum = hasher.finalize();
        let hash = slice_to_bounded_32(&sha256sum);
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
