use crate::openid::{Aud, Iss, OpenIdCredential, Sub};
use crate::storage::storable::metadata_v2::StorableMetadataEntryV2;
use crate::storage::storable::openid_credential_key::StorableOpenIdCredentialKey;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableOpenIdCredential {
    #[n(0)]
    pub iss: Iss,
    #[n(1)]
    pub sub: Sub,
    #[n(2)]
    pub aud: Aud,
    #[n(3)]
    pub last_usage_timestamp: Option<Timestamp>,
    #[n(4)]
    pub metadata: HashMap<String, StorableMetadataEntryV2>,
}

impl StorableOpenIdCredential {
    pub fn key(&self) -> StorableOpenIdCredentialKey {
        StorableOpenIdCredentialKey(self.iss.clone(), self.sub.clone())
    }
}

impl Storable for StorableOpenIdCredential {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredential");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableOpenIdCredential")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableOpenIdCredential> for OpenIdCredential {
    fn from(value: StorableOpenIdCredential) -> Self {
        OpenIdCredential {
            iss: value.iss,
            sub: value.sub,
            aud: value.aud,
            last_usage_timestamp: value.last_usage_timestamp,
            metadata: value
                .metadata
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
        }
    }
}

impl From<OpenIdCredential> for StorableOpenIdCredential {
    fn from(value: OpenIdCredential) -> Self {
        StorableOpenIdCredential {
            iss: value.iss,
            sub: value.sub,
            aud: value.aud,
            last_usage_timestamp: value.last_usage_timestamp,
            metadata: value
                .metadata
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
        }
    }
}
