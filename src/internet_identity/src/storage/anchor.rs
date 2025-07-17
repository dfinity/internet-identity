use crate::ii_domain::IIDomain;
use crate::openid::{OpenIdCredential, OpenIdCredentialKey};
use crate::storage::storable::anchor::StorableAnchor;
use crate::storage::storable::fixed_anchor::StorableFixedAnchor;
use crate::{IC0_APP_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
use candid::{CandidType, Deserialize, Principal};
use internet_identity_interface::archive::types::DeviceDataWithoutAlias;
use internet_identity_interface::internet_identity::types::openid::OpenIdCredentialData;
use internet_identity_interface::internet_identity::types::*;
use std::collections::HashMap;
use std::fmt;

#[cfg(test)]
mod tests;

/// Internal representation of the anchor.
/// The anchor has limited visibility for the constructor to make sure it is loaded from storage.
/// The devices can only be modified by the exposed functions which keeps invariant checking local
/// to this module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Anchor {
    anchor_number: AnchorNumber,
    devices: Vec<Device>,
    openid_credentials: Vec<OpenIdCredential>,
    metadata: Option<HashMap<String, MetadataEntry>>,
    name: Option<String>,
}

impl Device {
    /// Applies the values of `device_data` to self while leaving the other fields intact.
    pub fn apply_device_data(&mut self, device_data: DeviceData) {
        self.pubkey = device_data.pubkey;
        self.alias = device_data.alias;
        self.credential_id = device_data.credential_id;
        self.purpose = device_data.purpose;
        self.key_type = device_data.key_type;
        self.protection = device_data.protection;
        self.origin = device_data.origin;
        self.metadata = device_data.metadata;
    }
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
            origin: device_data.origin,
            last_usage_timestamp: None,
            metadata: device_data.metadata,
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
            origin: device.origin,
            metadata: device.metadata,
        }
    }
}

impl From<Device> for DeviceWithUsage {
    fn from(device: Device) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            last_usage: device.last_usage_timestamp,
            metadata: device.metadata,
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
            origin: device_data.origin,
            metadata_keys: device_data
                .metadata
                .as_ref()
                .map(|m| m.keys().cloned().collect()),
        }
    }
}

impl From<OpenIdCredential> for OpenIdCredentialData {
    fn from(openid_credential: OpenIdCredential) -> Self {
        Self {
            iss: openid_credential.iss,
            sub: openid_credential.sub,
            aud: openid_credential.aud,
            last_usage_timestamp: openid_credential.last_usage_timestamp,
            metadata: openid_credential.metadata,
        }
    }
}

impl From<OpenIdCredentialData> for OpenIdCredential {
    fn from(openid_credential: OpenIdCredentialData) -> Self {
        Self {
            iss: openid_credential.iss,
            sub: openid_credential.sub,
            aud: openid_credential.aud,
            last_usage_timestamp: openid_credential.last_usage_timestamp,
            metadata: openid_credential.metadata,
        }
    }
}

impl From<Anchor> for (StorableFixedAnchor, StorableAnchor) {
    fn from(anchor: Anchor) -> Self {
        (
            StorableFixedAnchor {
                devices: anchor.devices,
                metadata: anchor.metadata,
            },
            StorableAnchor {
                openid_credentials: anchor
                    .openid_credentials
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                name: anchor.name,
            },
        )
    }
}

impl From<(AnchorNumber, StorableFixedAnchor, Option<StorableAnchor>)> for Anchor {
    fn from(
        (anchor_number, storable_anchor, stable_anchor): (
            AnchorNumber,
            StorableFixedAnchor,
            Option<StorableAnchor>,
        ),
    ) -> Self {
        Anchor {
            anchor_number,
            devices: storable_anchor.devices,
            openid_credentials: stable_anchor
                .clone()
                .map(|anchor| {
                    anchor
                        .openid_credentials
                        .into_iter()
                        .map(Into::into)
                        .collect()
                })
                .unwrap_or_default(),
            metadata: storable_anchor.metadata,
            name: stable_anchor.and_then(|anchor| anchor.name),
        }
    }
}

impl Anchor {
    /// Creation of new anchors is restricted in order to make sure that the device checks are
    /// not accidentally bypassed.
    pub(super) fn new(anchor_number: AnchorNumber) -> Anchor {
        Self {
            anchor_number,
            devices: vec![],
            openid_credentials: vec![],
            metadata: None,
            name: None,
        }
    }

    pub fn anchor_number(&self) -> AnchorNumber {
        self.anchor_number
    }

    pub fn add_device(&mut self, device: Device) -> Result<(), AnchorError> {
        if self.devices.iter().any(|e| e.pubkey == device.pubkey) {
            return Err(AnchorError::DuplicateDevice {
                device_key: device.pubkey,
            });
        }
        check_device_invariants(&device)?;
        // Check the new set of devices is consistent
        let mut devices = self.devices.clone();
        devices.push(device);
        check_anchor_invariants(&devices[..], &self.metadata)?;
        self.devices = devices;
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
        check_device_invariants(&modified_device)?;
        // Ensure the old device can be mutated
        let index = self.device_index(device_key)?;
        check_mutation_allowed(&self.devices[index])?;

        // Check the new set of devices is consistent
        let mut devices = self.devices.clone();
        devices[index] = modified_device;
        check_anchor_invariants(&devices[..], &self.metadata)?;

        // Replace devices
        self.devices = devices;
        Ok(())
    }

    fn device_index(&self, device_key: &DeviceKey) -> Result<usize, AnchorError> {
        let Some(index) = self.devices.iter().position(|e| e.pubkey == device_key) else {
            return Err(AnchorError::NotFound {
                device_key: device_key.clone(),
            });
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

    /// Sets the timestamp on the given device.
    /// **Note:** Does not check invariants, based on the assumption that no invariant can be
    /// violated by changing the last usage timestamp on a device. See also the documentation on
    /// [check_invariants](Anchor::check_invariants).
    pub fn set_device_usage_timestamp(
        &mut self,
        device_key: &DeviceKey,
        time: Timestamp,
    ) -> Result<(), AnchorError> {
        let Some(device) = self.devices.iter_mut().find(|d| d.pubkey == device_key) else {
            return Err(AnchorError::NotFound {
                device_key: device_key.clone(),
            });
        };
        device.last_usage_timestamp = Some(time);
        Ok(())
    }

    /// Returns the timestamp of the last known activity, if any.
    pub fn last_activity(&self) -> Option<Timestamp> {
        self.devices
            .iter()
            .filter_map(|d| d.last_usage_timestamp)
            .chain(
                self.openid_credentials
                    .iter()
                    .filter_map(|c| c.last_usage_timestamp),
            )
            .max()
    }

    /// Returns information about the domains this anchor was active on since the given timestamp.
    /// Activity on unknown / other domain will be dropped if there is also activity on an II domain
    /// for the following reasons:
    /// * no information is most likely caused by the device having been added before we started
    ///   collecting domain information
    /// * combinations of an unknown domain and an II domain shows that the anchor is at least partially
    ///   active on the II domain (but also does non-standard / unsupported things to their anchor).
    ///   If we are interested in this user group, we might consider extending this function to give
    ///   them their own [DomainActivity] value.
    pub fn domain_activity_since(&self, timestamp: Timestamp) -> DomainActivity {
        #[derive(Default)]
        struct Accumulator {
            ic0_app: bool,
            internet_computer_org: bool,
            non_ii: bool,
        }

        let result = self
            .devices
            .iter()
            // filter devices with no activity
            .filter(|d| {
                d.last_usage_timestamp
                    .map(|t| t >= timestamp)
                    .unwrap_or(false)
            })
            // assign domain activity
            .fold(Accumulator::default(), |mut acc, device| {
                let Some(ref origin) = device.origin else {
                    acc.non_ii = true;
                    return acc;
                };
                match origin.as_str() {
                    IC0_APP_ORIGIN => acc.ic0_app = true,
                    INTERNETCOMPUTER_ORG_ORIGIN => acc.internet_computer_org = true,
                    _ => acc.non_ii = true,
                };
                acc
            });

        // Activity on other domains is discarded if there is also activity on an II domain.
        // The reason is that II might not have complete information since domain information was
        // only introduced recently.
        match (result.ic0_app, result.internet_computer_org, result.non_ii) {
            (true, true, _) => DomainActivity::BothIIDomains,
            (true, false, _) => DomainActivity::Ic0App,
            (false, true, _) => DomainActivity::InternetComputerOrg,
            (false, false, true) => DomainActivity::NonIIDomain,
            (false, false, false) => DomainActivity::None,
        }
    }

    /// Returns a reference to the OpenID credential given the key.
    pub fn openid_credential(
        &self,
        openid_credential_key: &OpenIdCredentialKey,
    ) -> Option<&OpenIdCredential> {
        self.openid_credentials()
            .iter()
            .find(|e| &e.key() == openid_credential_key)
    }

    /// Returns a reference to the list of OpenID credentials.
    pub fn openid_credentials(&self) -> &Vec<OpenIdCredential> {
        &self.openid_credentials
    }

    fn openid_credential_index(&self, key: &OpenIdCredentialKey) -> Result<usize, AnchorError> {
        self.openid_credentials
            .iter()
            .position(|entry| &entry.key() == key)
            .ok_or(AnchorError::OpenIdCredentialNotFound)
    }

    pub fn add_openid_credential(
        &mut self,
        openid_credential: OpenIdCredential,
    ) -> Result<(), AnchorError> {
        const MAX_LINKED_CREDENTIALS: usize = 100;

        if self
            .openid_credential_index(&openid_credential.key())
            .is_ok()
        {
            return Err(AnchorError::OpenIdCredentialAlreadyRegistered);
        }

        let num_credentials = self.openid_credentials().len();
        if num_credentials >= MAX_LINKED_CREDENTIALS {
            return Err(AnchorError::TooManyOpenIdCredentials {
                limit: MAX_LINKED_CREDENTIALS,
                num_credentials,
            });
        }

        self.openid_credentials.push(openid_credential);
        Ok(())
    }

    pub fn remove_openid_credential(
        &mut self,
        key: &OpenIdCredentialKey,
    ) -> Result<(), AnchorError> {
        let index = self.openid_credential_index(key)?;
        self.openid_credentials.remove(index);
        Ok(())
    }

    pub fn update_openid_credential(
        &mut self,
        openid_credential: OpenIdCredential,
    ) -> Result<(), AnchorError> {
        let index = self.openid_credential_index(&openid_credential.key())?;
        self.openid_credentials[index] = OpenIdCredential {
            // Don't update last usage timestamp, below `set_openid_credential_usage_timestamp`
            // method should be used instead to update the timestamp explicitly.
            //
            // This is to make sure that only bookkeeping updates the last usage timestamp at the
            // correct moment. If the timestamp is updated at the wrong moment, the activity stats
            // implementation will likely break and report incorrect OpenID credential usage.
            //
            // This is in line with the behavior of last usage timestamp in devices.
            last_usage_timestamp: self.openid_credentials[index].last_usage_timestamp,
            ..openid_credential
        };
        Ok(())
    }

    pub fn set_openid_credential_usage_timestamp(
        &mut self,
        key: &OpenIdCredentialKey,
        timestamp: Timestamp,
    ) -> Result<(), AnchorError> {
        let Some(openid_credential) = self.openid_credentials.iter_mut().find(|c| &c.key() == key)
        else {
            return Err(AnchorError::OpenIdCredentialNotFound);
        };
        openid_credential.last_usage_timestamp = Some(timestamp);
        Ok(())
    }

    /// Returns a reference to the optional identity metadata map
    /// (which is independent of devices / authentication methods).
    pub fn identity_metadata(&self) -> &Option<HashMap<String, MetadataEntry>> {
        &self.metadata
    }

    /// Replaces the existing identity metadata map (which is independent of devices / authentication
    /// methods) with the provided one.
    pub fn replace_identity_metadata(
        &mut self,
        metadata: HashMap<String, MetadataEntry>,
    ) -> Result<(), AnchorError> {
        let metadata = Some(metadata);
        check_anchor_invariants(&self.devices[..], &metadata)?;
        self.metadata = metadata;
        Ok(())
    }

    pub fn set_name(&mut self, name: Option<String>) -> Result<(), AnchorError> {
        const MAX_NAME_LENGTH: usize = 128;
        if name.as_ref().is_some_and(|n| n.len() > MAX_NAME_LENGTH) {
            return Err(AnchorError::NameTooLong {
                limit: MAX_NAME_LENGTH,
            });
        }
        self.name = name;
        Ok(())
    }

    pub fn name(&self) -> Option<String> {
        self.name.clone()
    }
}

/// Possible outcomes of domain bound activity for an anchor since a specific timestamp.
pub enum DomainActivity {
    // no activity at all
    None,
    // only active on non-ii domains
    NonIIDomain,
    // only active on the identity.ic0.app domain
    Ic0App,
    // only active on the identity.internetcomputer.org domain
    InternetComputerOrg,
    // activity on both identity.ic0.app and identity.internetcomputer.org
    BothIIDomains,
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
    pub origin: Option<String>,
    pub last_usage_timestamp: Option<Timestamp>,
    pub metadata: Option<HashMap<String, MetadataEntry>>,
}

impl Device {
    pub fn variable_fields_len(&self) -> usize {
        self.alias.len()
            + self.pubkey.len()
            + self.credential_id.as_ref().map(|id| id.len()).unwrap_or(0)
            + self.origin.as_ref().map(|origin| origin.len()).unwrap_or(0)
            + self.metadata.as_ref().map(metadata_len).unwrap_or(0)
    }

    pub fn ii_domain(&self) -> Option<IIDomain> {
        self.origin
            .as_ref()
            .and_then(|origin| IIDomain::try_from(origin.as_str()).ok())
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

fn metadata_len(metadata: &HashMap<String, MetadataEntry>) -> usize {
    metadata
        .iter()
        .map(|(key, value)| key.len() + metadata_entry_len(value))
        .sum()
}

fn metadata_entry_len(entry: &MetadataEntry) -> usize {
    match entry {
        MetadataEntry::String(value) => value.len(),
        MetadataEntry::Bytes(value) => value.len(),
        MetadataEntry::Map(data) => metadata_len(data),
    }
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
fn check_anchor_invariants(
    devices: &[Device],
    identity_metadata: &Option<HashMap<String, MetadataEntry>>,
) -> Result<(), AnchorError> {
    /// The number of devices is limited. The front-end limits the devices further
    /// by only allowing 8 devices with purpose `authentication` to make sure there is always
    /// a slot for the recovery devices.
    /// Note however, that a free device slot does not guarantee that it will fit the anchor
    /// due to the `VARIABLE_FIELDS_LIMIT`.
    const MAX_DEVICES_PER_ANCHOR: usize = 10;

    /// One device can fill more than one tenth of the available space for a single anchor (4 KB)
    /// with the variable length fields alone.
    /// In order to not give away all the anchor space to the device vector and identity metadata,
    /// we limit the sum of the size of all variable fields of all devices plus the identity metadata.
    /// This ensures that we have the flexibility to expand or change anchors in the future.
    /// The value 2500 was chosen so to accommodate pre-memory-migration anchors (limited to 2048 bytes)
    /// plus an additional 452 bytes to fit new fields introduced since.
    const VARIABLE_FIELDS_LIMIT: usize = 2500;

    if devices.len() > MAX_DEVICES_PER_ANCHOR {
        return Err(AnchorError::TooManyDevices {
            num_devices: devices.len(),
            limit: MAX_DEVICES_PER_ANCHOR,
        });
    }

    let variable_fields_size = devices
        .iter()
        .map(|device| device.variable_fields_len())
        .sum::<usize>()
        + identity_metadata.as_ref().map_or(0, metadata_len);

    if variable_fields_size > VARIABLE_FIELDS_LIMIT {
        return Err(AnchorError::CumulativeDataLimitExceeded {
            length: variable_fields_size,
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
///   * Recovery phrases cannot have a credential id
///   * Metadata does not contain reserved keys
///
///  NOTE: while in the future we may lift this restriction, for now we do ensure that
///  protected devices are limited to recovery phrases, which the webapp expects.
fn check_device_invariants(device: &Device) -> Result<(), AnchorError> {
    const RESERVED_KEYS: &[&str] = &[
        "pubkey",
        "alias",
        "credential_id",
        "purpose",
        "key_type",
        "protection",
        "origin",
        "last_usage_timestamp",
        "metadata",
        "usage",
        "authenticator_attachment",
    ];

    if let Some(metadata) = &device.metadata {
        for key in RESERVED_KEYS {
            if metadata.contains_key(*key) {
                return Err(AnchorError::ReservedMetadataKey {
                    key: key.to_string(),
                });
            }
        }
    }

    check_device_limits(device)?;

    if device.key_type == KeyType::SeedPhrase && device.credential_id.is_some() {
        return Err(AnchorError::RecoveryPhraseCredentialIdMismatch);
    }

    if device.protection == DeviceProtection::Protected && device.key_type != KeyType::SeedPhrase {
        return Err(AnchorError::InvalidDeviceProtection {
            key_type: device.key_type.clone(),
        });
    }
    Ok(())
}

fn check_device_limits(device: &Device) -> Result<(), AnchorError> {
    const ORIGIN_LEN_LIMIT: usize = 50;
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 300;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 350;

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

    let n = device
        .origin
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > ORIGIN_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "origin".to_string(),
            length: n,
            limit: ORIGIN_LEN_LIMIT,
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
    RecoveryPhraseCredentialIdMismatch,
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
    OpenIdCredentialAlreadyRegistered,
    OpenIdCredentialNotFound,
    ReservedMetadataKey {
        key: String,
    },
    NameTooLong {
        limit: usize,
    },
    TooManyOpenIdCredentials {
        limit: usize,
        num_credentials: usize,
    },
}

impl fmt::Display for AnchorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnchorError::TooManyDevices { num_devices, limit } => write!(
                f,
                "Anchor device limit exceeded: num devices {num_devices}, limit {limit}"
            ),
            AnchorError::DeviceLimitExceeded {
                field,
                length,
                limit,
            } => write!(
                f,
                "{field} limit exceeded: length {length}, limit {limit}"
            ),
            AnchorError::CumulativeDataLimitExceeded { length, limit } => write!(
                f,
                "Cumulative size of variable sized fields exceeds limit: length {length}, limit {limit}."
            ),
            AnchorError::InvalidDeviceProtection { key_type } => write!(
                f,
                "Only recovery phrases can be locked but key type is {key_type:?}"
            ),
            AnchorError::MutationNotAllowed { actual_principal, authorized_principal } => write!(
                f,
                "Device is locked. Must be authenticated with this device to mutate: authorized principal {authorized_principal}, actual principal {actual_principal}"
            ),
            AnchorError::MultipleRecoveryPhrases => write!(f, "There is already a recovery phrase and only one is allowed."),
            AnchorError::CannotModifyDeviceKey => write!(f, "Device key cannot be updated."),
            AnchorError::NotFound { device_key } => write!(f, "Device with key {} not found.", hex::encode(device_key)),
            AnchorError::DuplicateDevice { device_key } => write!(f, "Device with key {} already exists on this anchor.", hex::encode(device_key)),
            AnchorError::ReservedMetadataKey { key } => write!(f, "Metadata key '{key}' is reserved and cannot be used."),
            AnchorError::RecoveryPhraseCredentialIdMismatch => write!(f, "Devices with key type seed_phrase must not have a credential id."),
            AnchorError::OpenIdCredentialAlreadyRegistered => write!(f, "OpenID credential has already been registered on this or another anchor."),
            AnchorError::OpenIdCredentialNotFound => write!(f, "OpenID credential not found."),
            AnchorError::NameTooLong {limit} => write!(f, "Name is too long. Maximum length of name is {limit}."),
            AnchorError::TooManyOpenIdCredentials { limit, num_credentials } => write!(f, "Too many OpenID credentials. Maximum number of OpenID credentials is {limit}. Current number of OpenID credentials is {num_credentials}."),
        }
    }
}
