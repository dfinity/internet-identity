use crate::active_anchor_stats::IIDomain;
use crate::archive::{archive_operation, device_diff};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::TentativeDeviceRegistration;
use crate::storage::anchor::{Anchor, Device};
use crate::{active_anchor_stats, state};
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::*;

pub mod registration;
pub mod tentative_device_registration;

pub fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    let devices = state::anchor(anchor_number)
        .into_devices()
        .into_iter()
        .map(DeviceWithUsage::from)
        .collect();
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
            },
            Some(TentativeDeviceRegistration { expiration, .. }) if *expiration > now => {
                IdentityAnchorInfo {
                    devices,
                    device_registration: Some(DeviceRegistrationInfo {
                        expiration: *expiration,
                        tentative_device: None,
                    }),
                }
            }
            None | Some(_) => IdentityAnchorInfo {
                devices,
                device_registration: None,
            },
        }
    })
}

/// Handles all the bookkeeping required on anchor activity:
/// * Records anchor activity within aggregated activity stats
/// * Updates the last usage timestamp
///
/// Note: modifies the anchor but not does not write to storage. It is the responsibility of the
/// caller to persist the changes. This allows anchor operations to write to storage only once,
/// combining the modifications for bookkeeping reasons (made here) with other changes to the anchor.
pub fn activity_bookkeeping(anchor: &mut Anchor, current_device_key: &DeviceKey) {
    let domain = anchor
        .device(current_device_key)
        .and_then(|device| device.origin.as_ref())
        .and_then(|origin| IIDomain::try_from(origin.as_str()).ok());
    active_anchor_stats::update_active_anchors_stats(anchor, &domain);
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
pub fn add(anchor: &mut Anchor, device_data: DeviceData) -> Operation {
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
pub fn update(anchor: &mut Anchor, device_key: DeviceKey, device_data: DeviceData) -> Operation {
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
pub fn replace(anchor: &mut Anchor, old_device: DeviceKey, new_device: DeviceData) -> Operation {
    anchor
        .remove_device(&old_device)
        .unwrap_or_else(|err| trap(&format!("failed to replace device: {err}")));
    let new_device = Device::from(new_device);
    anchor
        .add_device(new_device.clone())
        .unwrap_or_else(|err| trap(&format!("failed to replace device: {err}")));

    Operation::ReplaceDevice {
        old_device,
        new_device: DeviceDataWithoutAlias::from(new_device),
    }
}

/// Removes a device of the given anchor and returns the operation to be archived.
/// Panics if the device to be removed does not exist
pub fn remove(anchor: &mut Anchor, device_key: DeviceKey) -> Operation {
    anchor
        .remove_device(&device_key)
        .unwrap_or_else(|err| trap(&format!("failed to remove device: {err}")));

    state::with_temp_keys_mut(|temp_keys| temp_keys.remove(&device_key));

    Operation::RemoveDevice { device: device_key }
}
