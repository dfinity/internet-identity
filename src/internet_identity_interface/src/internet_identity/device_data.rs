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
            aaguid: None,
            purpose: Purpose::Authentication,
            // This enables the fallback storage case for legacy pin-flow.
            // Otherwise, a key without `credential_id` and with `Purpose::Authentication` would
            // not be treated as inconsistent (falling into the recovery key case).
            key_type: KeyType::BrowserStorageKey,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
        }
    }
}
