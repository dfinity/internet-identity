use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use minicbor::{Decode, Encode};
use sha2::{Digest, Sha256};
use std::borrow::Cow;

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
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableApplication");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableApplication")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableOriginHash {
    hash: [u8; 8],
}

impl StorableOriginHash {
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

impl Storable for StorableOriginHash {
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
