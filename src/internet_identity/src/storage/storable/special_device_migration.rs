use candid::CandidType;
use internet_identity_interface::internet_identity::types::{KeyType, Purpose};
use minicbor::{Decode, Encode};

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
