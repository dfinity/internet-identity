use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::CredentialId;
use serde::Deserialize;
use std::borrow::Cow;

/// `ByteBuf` is not directly supported in ic-stable-structures,
/// this file implements a struct to wrap its inner bytes so it can be stored.
#[derive(Deserialize, CandidType, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub struct StorableCredentialId(Vec<u8>);

impl Storable for StorableCredentialId {
    fn to_bytes(&self) -> Cow<[u8]> {
        self.0.to_bytes()
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        StorableCredentialId(bytes.to_vec())
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableCredentialId> for CredentialId {
    fn from(value: StorableCredentialId) -> Self {
        CredentialId::from(value.0)
    }
}

impl From<CredentialId> for StorableCredentialId {
    fn from(value: CredentialId) -> Self {
        StorableCredentialId(value.to_vec())
    }
}
