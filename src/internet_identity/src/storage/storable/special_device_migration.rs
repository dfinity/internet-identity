use candid::CandidType;
use internet_identity_interface::internet_identity::types::{KeyType, Purpose};
use minicbor::{Decode, Encode};
use serde_bytes::ByteBuf;

use crate::storage::storable::credential_id;

#[derive(Encode, Decode, Debug, Clone, PartialEq, CandidType)]
pub enum StorablePurpose {
    #[n(0)]
    Recovery,
    #[n(1)]
    Authentication,
}

impl From<Purpose> for StorablePurpose {
    fn from(value: Purpose) -> Self {
        match value {
            Purpose::Recovery => Self::Recovery,
            Purpose::Authentication => Self::Authentication,
        }
    }
}

#[derive(Encode, Decode, Debug, Clone, PartialEq, CandidType)]
pub enum StorableKeyType {
    #[n(0)]
    Unknown,
    #[n(1)]
    Platform,
    #[n(2)]
    CrossPlatform,
    #[n(3)]
    SeedPhrase,
    #[n(4)]
    BrowserStorageKey,
}

impl From<KeyType> for StorableKeyType {
    fn from(value: KeyType) -> Self {
        match value {
            KeyType::Unknown => Self::Unknown,
            KeyType::Platform => Self::Platform,
            KeyType::CrossPlatform => Self::CrossPlatform,
            KeyType::SeedPhrase => Self::SeedPhrase,
            KeyType::BrowserStorageKey => Self::BrowserStorageKey,
        }
    }
}

/// Auxiliary data originating from the fields of `Device` during the anchor migration from bounded
/// to unbounded storage. Useful for analyzing special cases, since clients were allowed to create
/// inconsistent device data, e.g., `(Some(credential_id), Authentication, SeedPhrase)`.
#[derive(Encode, Decode, Debug, Clone, PartialEq, CandidType)]
#[cbor(map)]
pub struct SpecialDeviceMigration {
    #[n(0)]
    pub credential_id: Option<Vec<u8>>,
    #[n(1)]
    pub purpose: StorablePurpose,
    #[n(2)]
    pub key_type: StorableKeyType,
}

impl From<(&Option<Vec<u8>>, &Purpose, &KeyType)> for SpecialDeviceMigration {
    fn from(value: (&Option<Vec<u8>>, &Purpose, &KeyType)) -> Self {
        let (credential_id, purpose, key_type) = value;

        let credential_id = credential_id.clone();
        let purpose = purpose.clone().into();
        let key_type = key_type.clone().into();

        Self {
            credential_id,
            purpose,
            key_type,
        }
    }
}

impl From<SpecialDeviceMigration> for (Option<ByteBuf>, Purpose, KeyType) {
    fn from(value: SpecialDeviceMigration) -> Self {
        let SpecialDeviceMigration {
            credential_id,
            purpose,
            key_type,
        } = value;

        let credential_id = credential_id.map(ByteBuf::from);

        let purpose = match purpose {
            StorablePurpose::Recovery => Purpose::Recovery,
            StorablePurpose::Authentication => Purpose::Authentication,
        };

        let key_type = match key_type {
            StorableKeyType::Unknown => KeyType::Unknown,
            StorableKeyType::Platform => KeyType::Platform,
            StorableKeyType::CrossPlatform => KeyType::CrossPlatform,
            StorableKeyType::SeedPhrase => KeyType::SeedPhrase,
            StorableKeyType::BrowserStorageKey => KeyType::BrowserStorageKey,
        };

        (credential_id, purpose, key_type)
    }
}
