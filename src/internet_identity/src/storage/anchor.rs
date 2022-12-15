use candid::{CandidType, Deserialize, Principal};
use internet_identity_interface::archive::DeviceDataWithoutAlias;
use internet_identity_interface::*;
use std::{fmt, iter};

#[cfg(test)]
mod tests;

/// Internal representation of the anchor.
/// The anchor has limited visibility for the constructor to make sure it is loaded from storage.
/// The devices can only be modified by the exposed functions which keeps invariant checking local
/// to this module.
///
/// After the upcoming stable memory migration, new fields can be added to this struct.
#[derive(Clone, Debug, Default, CandidType, Deserialize, Eq, PartialEq)]
pub struct Anchor {
    devices: Vec<Device>,
}

impl From<DeviceData> for Device {
    fn from(device_data: DeviceData) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose,
            key_type: device_data.key_type,
            protection: device_data.protection,
        }
    }
}

impl From<Device> for DeviceData {
    fn from(device: Device) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
        }
    }
}

impl From<Device> for DeviceDataWithoutAlias {
    fn from(device_data: Device) -> Self {
        Self {
            pubkey: device_data.pubkey,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose,
            key_type: device_data.key_type,
            protection: device_data.protection,
        }
    }
}

impl Anchor {
    /// Creation of new anchors is restricted in order to make sure that the device checks are
    /// not accidentally bypassed.
    pub(super) fn new() -> Anchor {
        Self { devices: vec![] }
    }

    /// Creation of new anchors is restricted in order to make sure that the device checks are
    /// not accidentally bypassed.
    pub(super) fn from_devices(devices: Vec<Device>) -> Anchor {
        // We do _not_ check invariants here, because there might be anchors that do not fulfill
        // the invariants still stored in stable memory (e.g. anchors with multiple recovery phrases).
        Self { devices }
    }

    pub fn add_device(&mut self, device: Device) -> Result<(), AnchorError> {
        if self
            .devices
            .iter()
            .find(|e| e.pubkey == device.pubkey)
            .is_some()
        {
            return Err(AnchorError::DuplicateDevice {
                device_key: device.pubkey,
            });
        }
        check_device(&device)?;
        check_invariants(&self.devices.iter().chain(iter::once(&device)).collect())?;
        self.devices.push(device);
        Ok(())
    }

    /// Removes a device from this anchor.
    /// **Note:** Does not check invariants, based on the assumption that no invariant can be
    /// violated by removing a device. See also the documentation on
    /// [check_invariants](Anchor::check_invariants).
    pub fn remove_device(&mut self, device_key: &DeviceKey) -> Result<(), AnchorError> {
        let index = self.device_index(device_key)?;
        check_mutation_allowed(&self.devices[index])?;

        self.devices.remove(index);

        // We do _not_ check invariants here, because there might be anchors that do not fulfill
        // the invariants still stored in stable memory (e.g. anchors with multiple recovery phrases).
        // By allowing the removal of devices on such anchors, they can be made conforming to the invariants
        // by removing devices.
        Ok(())
    }

    pub fn modify_device(
        &mut self,
        device_key: &DeviceKey,
        modified_device: Device,
    ) -> Result<(), AnchorError> {
        if device_key != &modified_device.pubkey {
            return Err(AnchorError::CannotModifyDeviceKey);
        }
        check_device(&modified_device)?;
        let index = self.device_index(device_key)?;
        check_mutation_allowed(&self.devices[index])?;
        check_invariants(
            &self
                .devices
                .iter()
                // filter out the device before modification
                .filter(|e| e.pubkey != device_key)
                // append the device with modification
                .chain(iter::once(&modified_device))
                .collect(),
        )?;

        self.devices[index] = modified_device;
        Ok(())
    }

    fn device_index(&self, device_key: &DeviceKey) -> Result<usize, AnchorError> {
        let Some(index) = self.devices.iter().position(|e| e.pubkey == device_key) else {
            return Err(AnchorError::NotFound {device_key: device_key.clone()});
        };
        Ok(index)
    }

    /// Returns a reference to the device given the key.
    pub fn device(&self, device_key: &DeviceKey) -> Option<&Device> {
        self.devices.iter().find(|e| e.pubkey == device_key)
    }

    /// Returns a reference to the list of devices.
    pub fn devices(&self) -> &Vec<Device> {
        &self.devices
    }

    /// Consumes self and exposes the devices.
    pub fn into_devices(self) -> Vec<Device> {
        self.devices
    }
}

/// This is an internal version of `DeviceData` useful to provide a
/// backwards compatible level between device data stored in stable memory.
/// It is similar to `DeviceDataInternal` but with redundant options removed
/// (which is possible due to the stable memory candid schema migration).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct Device {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub purpose: Purpose,
    pub key_type: KeyType,
    pub protection: DeviceProtection,
}

impl Device {
    pub fn variable_fields_len(&self) -> usize {
        self.alias.len()
            + self.pubkey.len()
            + self.credential_id.as_ref().map(|id| id.len()).unwrap_or(0)
    }
}

fn check_mutation_allowed(device: &Device) -> Result<(), AnchorError> {
    match device.protection {
        DeviceProtection::Unprotected => (),
        DeviceProtection::Protected => {
            if caller() != Principal::self_authenticating(&device.pubkey) {
                return Err(AnchorError::MutationNotAllowed {
                    actual_principal: caller(),
                    authorized_principal: Principal::self_authenticating(&device.pubkey),
                });
            }
        }
    };
    Ok(())
}

#[cfg(not(test))]
fn caller() -> Principal {
    ic_cdk::caller()
}

/// This is required because [ic_cdk::caller()] traps when executed in a non-canister environment.
#[cfg(test)]
fn caller() -> Principal {
    tests::test_caller()
}

/// This checks anchor invariants, in particular:
///   * Max number of devices
///   * Sum of sizes of all variable length fields does not exceed limit
///   * There can only be one recovery phrase
///
/// **Important:**
/// Do **not** introduce new invariants that can be violated by _removing_ devices. The reason
/// is that there might still be devices in stable memory that do not fulfill the invariants.
/// In order to not break those anchors, they need to have a path back to satisfying the invariants.
/// To allow that transition, [remove_device](Anchor::remove_device) does _not_ check the invariants based on the assumption
/// that the state of an anchor cannot get worse by removing a device.
fn check_invariants(devices: &Vec<&Device>) -> Result<(), AnchorError> {
    /// The number of devices is limited. The front-end limits the devices further
    /// by only allowing 8 devices with purpose `authentication` to make sure there is always
    /// a slot for the recovery devices.
    /// Note however, that a free device slot does not guarantee that it will fit the the anchor
    /// due to the `VARIABLE_FIELDS_LIMIT`.
    const MAX_DEVICES_PER_ANCHOR: usize = 10;

    /// Single devices can use up to 564 bytes for the variable length fields alone.
    /// In order to not give away all the anchor space to the device vector, we limit the sum of the
    /// size of all variable fields of all devices. This ensures that we have the flexibility to expand
    /// or change anchors in the future.
    /// The value 2048 was chosen because it is the max anchor size before the stable memory migration.
    /// This means that all pre-existing anchors are below this limit. And after the migration, the
    /// candid encoded `vec devices` will stay far below 4KB in size (testing showed anchors of up to
    /// 2259 bytes).
    const VARIABLE_FIELDS_LIMIT: usize = 2048;

    if devices.len() > MAX_DEVICES_PER_ANCHOR {
        return Err(AnchorError::TooManyDevices {
            num_devices: devices.len(),
            limit: MAX_DEVICES_PER_ANCHOR,
        });
    }

    let existing_variable_size: usize = devices
        .iter()
        // filter out the device being checked to not count it twice in case of update operations
        .map(|device| device.variable_fields_len())
        .sum();

    if existing_variable_size > VARIABLE_FIELDS_LIMIT {
        return Err(AnchorError::CumulativeDataLimitExceeded {
            length: existing_variable_size,
            limit: VARIABLE_FIELDS_LIMIT,
        });
    }

    // check that there is only a single recovery phrase
    if devices
        .iter()
        .filter(|device| device.key_type == KeyType::SeedPhrase)
        .count()
        > 1
    {
        return Err(AnchorError::MultipleRecoveryPhrases);
    }

    Ok(())
}

/// This checks device invariants, in particular:
///   * Sizes of various fields do not exceed limits
///   * Only recovery phrases can be protected
///
///  NOTE: while in the future we may lift this restriction, for now we do ensure that
///  protected devices are limited to recovery phrases, which the webapp expects.
fn check_device(device: &Device) -> Result<(), AnchorError> {
    check_device_limits(device)?;

    if device.protection == DeviceProtection::Protected && device.key_type != KeyType::SeedPhrase {
        return Err(AnchorError::InvalidDeviceProtection {
            key_type: device.key_type.clone(),
        });
    }
    Ok(())
}

fn check_device_limits(device: &Device) -> Result<(), AnchorError> {
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 300;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 200;

    let n = device.alias.len();
    if n > ALIAS_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "alias".to_string(),
            length: n,
            limit: ALIAS_LEN_LIMIT,
        });
    }

    let n = device.pubkey.len();
    if n > PK_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "pubkey".to_string(),
            length: n,
            limit: PK_LEN_LIMIT,
        });
    }

    let n = device
        .credential_id
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > CREDENTIAL_ID_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "credential_id".to_string(),
            length: n,
            limit: CREDENTIAL_ID_LEN_LIMIT,
        });
    }
    Ok(())
}

#[derive(Debug, Eq, PartialEq)]
pub enum AnchorError {
    TooManyDevices {
        limit: usize,
        num_devices: usize,
    },
    DeviceLimitExceeded {
        field: String,
        length: usize,
        limit: usize,
    },
    CumulativeDataLimitExceeded {
        length: usize,
        limit: usize,
    },
    InvalidDeviceProtection {
        key_type: KeyType,
    },
    MutationNotAllowed {
        authorized_principal: Principal,
        actual_principal: Principal,
    },
    MultipleRecoveryPhrases,
    CannotModifyDeviceKey,
    NotFound {
        device_key: DeviceKey,
    },
    DuplicateDevice {
        device_key: DeviceKey,
    },
}

impl fmt::Display for AnchorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnchorError::TooManyDevices { num_devices, limit } => write!(
                f,
                "Anchor device limit exceeded: num devices {}, limit {}",
                num_devices, limit
            ),
            AnchorError::DeviceLimitExceeded {
                field,
                length,
                limit,
            } => write!(
                f,
                "{} limit exceeded: length {}, limit {}",
                field, length, limit
            ),
            AnchorError::CumulativeDataLimitExceeded { length, limit } => write!(
                f,
                "Cumulative size of variable sized fields exceeds limit: length {}, limit {}. Either use shorter aliases or remove an existing device.",
                length, limit
            ),
            AnchorError::InvalidDeviceProtection { key_type } => write!(
                f,
                "Only recovery phrases can be protected but key type is {:?}",
                key_type
            ),
            AnchorError::MutationNotAllowed { actual_principal, authorized_principal } => write!(
                f,
                "Device is protected. Must be authenticated with this device to mutate: authorized principal {}, actual principal {}",
                authorized_principal, actual_principal
            ),
            AnchorError::MultipleRecoveryPhrases => write!(f, "There is already a recovery phrase and only one is allowed."),
            AnchorError::CannotModifyDeviceKey => write!(f, "Device key cannot be updated."),
            AnchorError::NotFound { device_key } => write!(f, "Device with key {} not found.", hex::encode(device_key)),
            AnchorError::DuplicateDevice { device_key } => write!(f, "Device with key {} already exists on this anchor.", hex::encode(device_key)),
        }
    }
}
