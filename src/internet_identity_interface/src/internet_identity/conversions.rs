use crate::internet_identity::types::*;
use std::fmt::{Display, Formatter};

#[cfg(test)]
mod test;

impl From<DeviceWithUsage> for DeviceData {
    fn from(device: DeviceWithUsage) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            metadata: device.metadata,
        }
    }
}

impl From<DeviceData> for DeviceWithUsage {
    fn from(device: DeviceData) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            last_usage: None,
            metadata: device.metadata,
        }
    }
}

impl IdentityAnchorInfo {
    pub fn into_device_data(self) -> Vec<DeviceData> {
        self.devices.into_iter().map(DeviceData::from).collect()
    }
}

impl TryFrom<DeviceData> for WebAuthnCredential {
    type Error = ();

    fn try_from(device: DeviceData) -> Result<Self, Self::Error> {
        let credential_id = device.credential_id.ok_or(())?;
        Ok(Self {
            pubkey: device.pubkey,
            credential_id,
        })
    }
}

impl From<DeviceProtection> for AuthnMethodProtection {
    fn from(value: DeviceProtection) -> Self {
        match value {
            DeviceProtection::Protected => AuthnMethodProtection::Protected,
            DeviceProtection::Unprotected => AuthnMethodProtection::Unprotected,
        }
    }
}

impl From<AuthnMethodProtection> for DeviceProtection {
    fn from(value: AuthnMethodProtection) -> Self {
        match value {
            AuthnMethodProtection::Protected => DeviceProtection::Protected,
            AuthnMethodProtection::Unprotected => DeviceProtection::Unprotected,
        }
    }
}

impl From<DeviceWithUsage> for AuthnMethodData {
    fn from(device_data: DeviceWithUsage) -> Self {
        let authn_method = if let Some(credential_id) = device_data.credential_id.clone() {
            AuthnMethod::WebAuthn(WebAuthn {
                credential_id,
                pubkey: device_data.pubkey.clone(),
            })
        } else {
            AuthnMethod::PubKey(PublicKeyAuthn {
                pubkey: device_data.pubkey.clone(),
            })
        };
        let mut metadata = device_data.metadata.unwrap_or_default();
        if !device_data.alias.is_empty() {
            metadata.insert(
                "alias".to_string(),
                MetadataEntry::String(device_data.alias),
            );
        }
        device_data
            .origin
            .map(|origin| metadata.insert("origin".to_string(), MetadataEntry::String(origin)));

        match device_data.key_type {
            KeyType::Platform => {
                metadata.insert(
                    "authenticator_attachment".to_string(),
                    MetadataEntry::String("platform".to_string()),
                );
            }
            KeyType::CrossPlatform => {
                metadata.insert(
                    "authenticator_attachment".to_string(),
                    MetadataEntry::String("cross_platform".to_string()),
                );
            }
            KeyType::SeedPhrase => {
                metadata.insert(
                    "usage".to_string(),
                    MetadataEntry::String("recovery_phrase".to_string()),
                );
            }
            KeyType::BrowserStorageKey => {
                metadata.insert(
                    "usage".to_string(),
                    MetadataEntry::String("browser_storage_key".to_string()),
                );
            }
            KeyType::Unknown => {
                // nothing to do
            }
        };

        Self {
            authn_method,
            metadata,
            protection: AuthnMethodProtection::from(device_data.protection),
            purpose: device_data.purpose,
            last_authentication: device_data.last_usage,
        }
    }
}

impl From<DeviceRegistrationInfo> for AuthnMethodRegistration {
    fn from(value: DeviceRegistrationInfo) -> Self {
        AuthnMethodRegistration {
            expiration: value.expiration,
            authn_method: value.tentative_device.map(AuthnMethodData::from),
        }
    }
}

impl From<DeviceData> for AuthnMethodData {
    fn from(device_data: DeviceData) -> Self {
        let device_with_usage = DeviceWithUsage::from(device_data);
        AuthnMethodData::from(device_with_usage)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthnMethodConversionError {
    InvalidMetadataType {
        key: String,
        expected_type: String,
        actual_value: String,
    },
}

impl Display for AuthnMethodConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AuthnMethodConversionError::InvalidMetadataType {
                key,
                expected_type,
                actual_value: actual_type,
            } => write!(
                f,
                "Invalid metadata type for key '{}': expected {}, got value {}",
                key, expected_type, actual_type
            ),
        }
    }
}

impl TryFrom<AuthnMethodData> for DeviceWithUsage {
    type Error = AuthnMethodConversionError;

    /// Conversion from `AuthnMethodData` to `DeviceWithUsage`
    /// This conversion can fail because `AuthnMethodData` stores some explicit fields of `DeviceWithUsage`
    /// in metadata which is more permissive with regards to the type of the values.
    /// In particular the following metadata entries must be of the `string` variant for the conversion to succeed:
    ///  - alias
    ///  - origin
    ///  - authenticator_attachment
    ///  - usage
    /// This restriction may be lifted in the future.
    fn try_from(mut data: AuthnMethodData) -> Result<Self, Self::Error> {
        fn remove_metadata_string(
            data: &mut AuthnMethodData,
            key: &str,
        ) -> Result<Option<String>, AuthnMethodConversionError> {
            data.metadata
                .remove(key)
                .map(|entry| match entry {
                    MetadataEntry::String(value) => Ok(value),
                    value @ MetadataEntry::Bytes(_) | value @ MetadataEntry::Map(_) => {
                        Err(AuthnMethodConversionError::InvalidMetadataType {
                            key: key.to_string(),
                            expected_type: "string".to_string(),
                            actual_value: format!("{:?}", value),
                        })
                    }
                })
                .transpose()
        }
        // Remove the metadata entries that have a dedicated field in the `DeviceData` struct in
        // order to avoid duplication.
        let alias = remove_metadata_string(&mut data, "alias")?.unwrap_or_default();
        let origin = remove_metadata_string(&mut data, "origin")?;

        let mut key_type = remove_metadata_string(&mut data, "authenticator_attachment")?
            .map(|key_type| match key_type.as_str() {
                "platform" => KeyType::Platform,
                "cross_platform" => KeyType::CrossPlatform,
                _ => KeyType::Unknown,
            })
            .unwrap_or(KeyType::Unknown);

        if key_type == KeyType::Unknown {
            key_type = remove_metadata_string(&mut data, "usage")?
                .map(|key_type| match key_type.as_str() {
                    "recovery_phrase" => KeyType::SeedPhrase,
                    "browser_storage_key" => KeyType::BrowserStorageKey,
                    _ => KeyType::Unknown,
                })
                .unwrap_or(KeyType::Unknown);
        }

        let (pubkey, credential_id) = match data.authn_method {
            AuthnMethod::WebAuthn(WebAuthn {
                pubkey,
                credential_id,
            }) => (pubkey, Some(credential_id)),
            AuthnMethod::PubKey(PublicKeyAuthn { pubkey }) => (pubkey, None),
        };

        Ok(DeviceWithUsage {
            pubkey,
            alias,
            credential_id,
            purpose: data.purpose,
            key_type,
            protection: DeviceProtection::from(data.protection),
            origin,
            last_usage: data.last_authentication,
            metadata: Some(data.metadata),
        })
    }
}

impl From<RegisterResponse> for IdentityRegisterResponse {
    fn from(register_response: RegisterResponse) -> Self {
        match register_response {
            RegisterResponse::Registered { user_number } => {
                IdentityRegisterResponse::Ok(user_number)
            }
            RegisterResponse::CanisterFull => IdentityRegisterResponse::CanisterFull,
            RegisterResponse::BadChallenge => IdentityRegisterResponse::BadChallenge,
        }
    }
}
