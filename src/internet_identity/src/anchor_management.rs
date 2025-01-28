use crate::archive::{archive_operation, device_diff};
use crate::openid::{OpenIdCredential, OpenIdCredentialKey};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::TentativeDeviceRegistration;
use crate::storage::anchor::{Anchor, AnchorError, Device};
use crate::{state, stats::activity_stats};
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::openid::OpenIdCredentialData;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DeviceData, DeviceKey, DeviceRegistrationInfo, DeviceWithUsage,
    IdentityAnchorInfo, MetadataEntry,
};
use state::storage_borrow;
use std::collections::HashMap;

pub mod registration;
pub mod tentative_device_registration;

pub fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    let anchor = state::anchor(anchor_number);
    let devices = anchor
        .devices()
        .clone()
        .into_iter()
        .map(DeviceWithUsage::from)
        .collect();
    let openid_credentials = Some(
        anchor
            .openid_credentials()
            .clone()
            .into_iter()
            .map(OpenIdCredentialData::from)
            .collect(),
    );
    let now = time();

    state::tentative_device_registrations(|tentative_device_registrations| {
        match tentative_device_registrations.get(&anchor_number) {
            Some(TentativeDeviceRegistration {
                expiration,
                state:
                    DeviceTentativelyAdded {
                        tentative_device, ..
                    },
            }) if *expiration > now => IdentityAnchorInfo {
                devices,
                device_registration: Some(DeviceRegistrationInfo {
                    expiration: *expiration,
                    tentative_device: Some(tentative_device.clone()),
                }),
                openid_credentials,
            },
            Some(TentativeDeviceRegistration { expiration, .. }) if *expiration > now => {
                IdentityAnchorInfo {
                    devices,
                    device_registration: Some(DeviceRegistrationInfo {
                        expiration: *expiration,
                        tentative_device: None,
                    }),
                    openid_credentials,
                }
            }
            None | Some(_) => IdentityAnchorInfo {
                devices,
                device_registration: None,
                openid_credentials,
            },
        }
    })
}

/// Handles all the bookkeeping required on anchor activity:
/// * Records anchor & device activity within aggregated activity stats
/// * Updates the last usage timestamp
///
/// Note: modifies the anchor but not does not write to storage. It is the responsibility of the
/// caller to persist the changes. This allows anchor operations to write to storage only once,
/// combining the modifications for bookkeeping reasons (made here) with other changes to the anchor.
pub fn activity_bookkeeping(anchor: &mut Anchor, current_device_key: &DeviceKey) {
    let device = anchor
        .device(current_device_key)
        .unwrap_or_else(|| trap(&format!("bug: device {:?} not found", current_device_key)));
    activity_stats::update_activity_stats(anchor, device);
    anchor
        .set_device_usage_timestamp(current_device_key, time())
        .expect("unable to update last usage timestamp");
}

/// Handles all the bookkeeping required after a successful anchor operation:
/// * Adds the operation to the archive buffer
/// * Increments the anchor operation counter
pub fn post_operation_bookkeeping(anchor_number: AnchorNumber, operation: Operation) {
    archive_operation(anchor_number, caller(), operation);
    state::usage_metrics_mut(|metrics| {
        metrics.anchor_operation_counter += 1;
    });
}

/// Adds a device to the given anchor and returns the operation to be archived.
/// Panics if this operation violates anchor constraints (see [Anchor]).
pub fn add_device(anchor: &mut Anchor, device_data: DeviceData) -> Operation {
    let new_device = Device::from(device_data);
    anchor
        .add_device(new_device.clone())
        .unwrap_or_else(|err| trap(&format!("failed to add device: {err}")));

    Operation::AddDevice {
        device: DeviceDataWithoutAlias::from(new_device),
    }
}

/// Updates a device of the given anchor and returns the operation to be archived.
/// Panics if
/// * the device to be updated does not exist
/// * the operation violates anchor constraints (see [Anchor])
pub fn update_device(
    anchor: &mut Anchor,
    device_key: DeviceKey,
    device_data: DeviceData,
) -> Operation {
    let Some(existing_device) = anchor.device(&device_key) else {
        trap("Could not find device to update, check device key")
    };

    let mut new_device = existing_device.clone();
    new_device.apply_device_data(device_data);
    let diff = device_diff(existing_device, &new_device);

    anchor
        .modify_device(&device_key, new_device)
        .unwrap_or_else(|err| trap(&format!("failed to modify device: {err}")));

    Operation::UpdateDevice {
        device: device_key,
        new_values: diff,
    }
}

/// Replaces a device of the given anchor with another and returns the operation to be archived.
/// Panics if
/// * the device to be replaced does not exist
/// * the operation violates anchor constraints (see [Anchor])
pub fn replace_device(
    anchor_number: AnchorNumber,
    anchor: &mut Anchor,
    old_device: DeviceKey,
    new_device: DeviceData,
) -> Operation {
    anchor
        .remove_device(&old_device)
        .unwrap_or_else(|err| trap(&format!("failed to replace device: {err}")));
    let new_device = Device::from(new_device);
    anchor
        .add_device(new_device.clone())
        .unwrap_or_else(|err| trap(&format!("failed to replace device: {err}")));

    state::with_temp_keys_mut(|temp_keys| temp_keys.remove_temp_key(anchor_number, &old_device));
    Operation::ReplaceDevice {
        old_device,
        new_device: DeviceDataWithoutAlias::from(new_device),
    }
}

/// Removes a device of the given anchor and returns the operation to be archived.
/// Panics if the device to be removed does not exist
pub fn remove_device(
    anchor_number: AnchorNumber,
    anchor: &mut Anchor,
    device_key: DeviceKey,
) -> Operation {
    anchor
        .remove_device(&device_key)
        .unwrap_or_else(|err| trap(&format!("failed to remove device: {err}")));

    state::with_temp_keys_mut(|temp_keys| temp_keys.remove_temp_key(anchor_number, &device_key));
    Operation::RemoveDevice { device: device_key }
}

/// Replaces the identity metadata and returns the operation to be archived.
/// Panics if the data cannot be written (due to size constraints).
pub fn identity_metadata_replace(
    anchor: &mut Anchor,
    metadata: HashMap<String, MetadataEntry>,
) -> Result<Operation, AnchorError> {
    let metadata_keys = metadata.keys().cloned().collect();
    anchor.replace_identity_metadata(metadata)?;
    Ok(Operation::IdentityMetadataReplace { metadata_keys })
}

/// Adds an `OpenIdCredential` to the given anchor and returns the operation to be archived.
/// Returns an error if the `OpenIdCredential` already exists in this or another anchor.
pub fn add_openid_credential(
    anchor: &mut Anchor,
    openid_credential: OpenIdCredential,
) -> Result<Operation, AnchorError> {
    if lookup_anchor_with_openid_credential(&openid_credential.key()).is_some() {
        return Err(AnchorError::OpenIdCredentialAlreadyRegistered);
    }
    anchor.add_openid_credential(openid_credential.clone())?;
    Ok(Operation::AddOpenIdCredential {
        iss: openid_credential.iss,
    })
}

/// Removes an `OpenIdCredential` of the given anchor and returns the operation to be archived.
/// Return an error if the `OpenIdCredential` to be removed does not exist.
pub fn remove_openid_credential(
    anchor: &mut Anchor,
    key: &OpenIdCredentialKey,
) -> Result<Operation, AnchorError> {
    anchor.remove_openid_credential(key)?;
    let (iss, _) = key;
    Ok(Operation::RemoveOpenIdCredential { iss: iss.clone() })
}

/// Updates an `OpenIdCredential` of the given anchor, used to update details like the metadata.
/// Return an error if the `OpenIdCredential` to be updated does not exist.
#[allow(unused)]
pub fn update_openid_credential(
    anchor: &mut Anchor,
    openid_credential: OpenIdCredential,
) -> Result<(), AnchorError> {
    anchor.update_openid_credential(openid_credential)
}

/// Lookup `AnchorNumber` for the given `OpenIdCredentialKey`.
pub fn lookup_anchor_with_openid_credential(key: &OpenIdCredentialKey) -> Option<AnchorNumber> {
    storage_borrow(|storage| storage.lookup_anchor_with_openid_credential(key))
}

#[test]
fn should_register_openid_credential_only_for_a_single_anchor() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let mut anchor_0 = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let mut anchor_1 = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let openid_credential = OpenIdCredential {
        iss: "https://example.com".into(),
        sub: "example-sub".into(),
        aud: "example-aud".into(),
        last_usage_timestamp: 0,
        metadata: HashMap::default(),
    };

    // Check if OpenID credential can be added
    assert_eq!(
        add_openid_credential(&mut anchor_0, openid_credential.clone()),
        Ok(Operation::AddOpenIdCredential {
            iss: openid_credential.iss.clone()
        })
    );
    storage_borrow_mut(|storage| storage.write(anchor_0.clone()).unwrap());

    // Check if adding OpenID credential twice returns an error
    assert_eq!(
        add_openid_credential(&mut anchor_0, openid_credential.clone()),
        Err(AnchorError::OpenIdCredentialAlreadyRegistered)
    );
    storage_borrow_mut(|storage| storage.write(anchor_0.clone()).unwrap());

    // Check if adding OpenID credential to another anchor returns an error
    assert_eq!(
        add_openid_credential(&mut anchor_1, openid_credential.clone()),
        Err(AnchorError::OpenIdCredentialAlreadyRegistered)
    );

    // Check if OpenID credential can be moved to another anchor
    assert_eq!(
        remove_openid_credential(&mut anchor_0, &openid_credential.key()),
        Ok(Operation::RemoveOpenIdCredential {
            iss: openid_credential.iss.clone()
        })
    );
    storage_borrow_mut(|storage| storage.write(anchor_0.clone()).unwrap());
    assert_eq!(
        add_openid_credential(&mut anchor_1, openid_credential.clone()),
        Ok(Operation::AddOpenIdCredential {
            iss: openid_credential.iss.clone()
        })
    );
}
