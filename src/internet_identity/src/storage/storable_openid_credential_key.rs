use crate::openid::OpenIdCredentialKey;
use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use serde::Deserialize;
use std::borrow::Cow;

/// Unbounded tuples are not supported yet in ic-stable-structures,
/// this file implements a struct to wrap it so it can be stored.
#[derive(Deserialize, CandidType, Clone, Ord, Eq, PartialEq, PartialOrd)]
pub struct StorableOpenIdCredentialKey(OpenIdCredentialKey);

impl From<StorableOpenIdCredentialKey> for OpenIdCredentialKey {
    fn from(value: StorableOpenIdCredentialKey) -> Self {
        value.0
    }
}

impl From<OpenIdCredentialKey> for StorableOpenIdCredentialKey {
    fn from(value: OpenIdCredentialKey) -> Self {
        StorableOpenIdCredentialKey(value)
    }
}

impl Storable for StorableOpenIdCredentialKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut candid = candid::encode_one(self)
            .expect("Failed to serialize StorableOpenIdCredentialKey to candid");
        let mut buf = (candid.len() as u16).to_le_bytes().to_vec(); // 2 bytes for length
        buf.append(&mut candid);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let length = u16::from_le_bytes(bytes[..2].try_into().unwrap()) as usize;

        candid::decode_one(&bytes[2..length + 2])
            .expect("Failed to deserialize StorableOpenIdCredentialKey from candid")
    }

    const BOUND: Bound = Bound::Unbounded;
}
