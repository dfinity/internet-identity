use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::borrow::Cow;

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq, Ord, PartialOrd)]
pub struct Application {
    pub origin: FrontendHostname,
    pub stored_accounts: u64,
}

impl Storable for Application {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct OriginHash {
    hash: [u8; 8],
}

impl Storable for OriginHash {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(self.hash.to_vec())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        Self {
            hash: bytes.as_ref().try_into().unwrap(),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 8,
        is_fixed_size: true,
    };
}

impl OriginHash {
    pub fn from_origin(origin: &FrontendHostname) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(origin.as_bytes());
        let full_hash_result = hasher.finalize();
        // Truncate the 32-byte SHA-256 hash to the first 8 bytes.
        let truncated_hash_slice: &[u8] = &full_hash_result[0..8];
        let hash_8_bytes: [u8; 8] = truncated_hash_slice
            .try_into()
            .expect("Failed to truncate SHA256 hash to 8 bytes; slice length should be 8.");

        Self { hash: hash_8_bytes }
    }
}
