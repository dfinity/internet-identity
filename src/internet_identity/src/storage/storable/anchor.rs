use crate::storage::storable::openid_credential::StorableOpenIdCredential;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableAnchor {
    #[n(0)]
    pub name: Option<String>,
    #[n(1)]
    pub openid_credentials: Vec<StorableOpenIdCredential>,
}

impl Storable for StorableAnchor {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAnchor");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAnchor")
    }

    const BOUND: Bound = Bound::Unbounded;
}
