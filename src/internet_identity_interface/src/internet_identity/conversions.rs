use crate::internet_identity::types::*;

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
        metadata.insert(
            "alias".to_string(),
            MetadataEntry::String(device_data.alias),
        );
        device_data
            .origin
            .map(|origin| metadata.insert("origin".to_string(), MetadataEntry::String(origin)));

        match device_data.key_type {
            KeyType::Platform => {
                metadata.insert(
                    "key_type".to_string(),
                    MetadataEntry::String("platform".to_string()),
                );
            }
            KeyType::CrossPlatform => {
                metadata.insert(
                    "key_type".to_string(),
                    MetadataEntry::String("cross_platform".to_string()),
                );
            }
            KeyType::SeedPhrase | KeyType::Unknown => {
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
