use crate::internet_identity::types::{
    DeviceData, DeviceWithUsage, IdentityAnchorInfo, WebAuthnCredential,
};

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
            meta_data: device.meta_data,
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
            meta_data: device.meta_data,
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
