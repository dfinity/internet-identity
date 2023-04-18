use crate::internet_identity::types::{DeviceData, DeviceProtection, KeyType, Purpose};
use candid::Principal;
use serde_bytes::ByteBuf;

impl DeviceData {
    pub fn principal(&self) -> Principal {
        Principal::self_authenticating(&self.pubkey)
    }

    /// Creates a new device that can be used for test purposes.
    pub fn auth_test_device() -> Self {
        Self {
            pubkey: ByteBuf::from([0; 32]),
            alias: "test device".to_string(),
            credential_id: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
            meta_data: None,
        }
    }
}
