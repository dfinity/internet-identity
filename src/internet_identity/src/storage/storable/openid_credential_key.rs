use crate::openid::{Iss, OpenIdCredentialKey, Sub};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Unbounded tuples are not supported yet in ic-stable-structures,
/// this file implements a struct to wrap it so it can be stored.
#[derive(Encode, Decode, Clone, Ord, PartialOrd, Eq, PartialEq)]
#[cbor(array)]
pub struct StorableOpenIdCredentialKey(#[n(0)] pub Iss, #[n(1)] pub Sub);

impl Storable for StorableOpenIdCredentialKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredentialKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableOpenIdCredentialKey")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableOpenIdCredentialKey> for OpenIdCredentialKey {
    fn from(value: StorableOpenIdCredentialKey) -> Self {
        (value.0, value.1)
    }
}

impl From<OpenIdCredentialKey> for StorableOpenIdCredentialKey {
    fn from(value: OpenIdCredentialKey) -> Self {
        StorableOpenIdCredentialKey(value.0, value.1)
    }
}
