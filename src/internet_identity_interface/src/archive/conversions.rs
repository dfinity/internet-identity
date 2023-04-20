use crate::archive::types::DeviceDataWithoutAlias;
use crate::internet_identity::types::DeviceData;

impl From<DeviceData> for DeviceDataWithoutAlias {
    fn from(device_data: DeviceData) -> Self {
        Self {
            pubkey: device_data.pubkey,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose,
            key_type: device_data.key_type,
            protection: device_data.protection,
            origin: device_data.origin,
            metadata_keys: device_data
                .metadata
                .as_ref()
                .map(|m| m.keys().cloned().collect())
                .unwrap_or_default(),
        }
    }
}
