use crate::storage::storable::openid_credential::StorableOpenIdCredential;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorablePasskeyCredential {
    #[n(0)]
    pub pubkey: Vec<u8>,
    #[n(1)]
    pub credential_id: Vec<u8>,
    #[n(2)]
    pub last_usage_timestamp_ns: Option<Timestamp>,
    #[n(3)]
    pub alias: String,
    #[n(4)]
    pub origin: Option<String>,
    #[n(5)]
    pub aaguid: Option<Vec<u8>>,
}

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableRecoveryKey {
    #[n(0)]
    pub pubkey: Vec<u8>,
    #[n(1)]
    pub last_usage_timestamp_ns: Option<Timestamp>,
    #[n(2)]
    pub is_protected: Option<bool>,
}

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableAnchor {
    #[n(0)]
    pub name: Option<String>,
    #[n(1)]
    pub openid_credentials: Vec<StorableOpenIdCredential>,
    #[n(2)]
    pub created_at_ns: Option<Timestamp>,
    #[n(3)]
    pub passkey_credentials: Option<Vec<StorablePasskeyCredential>>,
    #[n(4)]
    pub recovery_phrases: Option<Vec<StorableRecoveryKey>>,
}

impl Storable for StorableAnchor {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAnchor");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAnchor")
    }

    const BOUND: Bound = Bound::Unbounded;
}
