use std::borrow::Cow;

use candid::CandidType;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::CredentialId;
use serde::Deserialize;

#[derive(CandidType, Deserialize, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableCredentialId(CredentialId);

impl From<CredentialId> for StorableCredentialId {
    fn from(value: CredentialId) -> Self {
        StorableCredentialId(value)
    }
}

impl From<StorableCredentialId> for CredentialId {
    fn from(value: StorableCredentialId) -> Self {
        value.0
    }
}

impl Storable for StorableCredentialId {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut candid =
            candid::encode_one(self).expect("Failed to serialize StorableCredentialId to candid");
        let mut buf = (candid.len() as u16).to_le_bytes().to_vec(); // 2 bytes for length
        buf.append(&mut candid);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let length = u16::from_le_bytes(bytes[..2].try_into().unwrap()) as usize;

        candid::decode_one(&bytes[2..length + 2])
            .expect("Failed to deserialize StorableCredentialId from candid")
    }

    const BOUND: Bound = Bound::Unbounded; //TODO: this may be bounded, more research is required.
}
