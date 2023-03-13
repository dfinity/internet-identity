use crate::archive::{archive_operation, device_diff};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::TentativeDeviceRegistration;
use crate::storage::anchor::{Anchor, Device};
use crate::{active_anchor_stats, state};
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::*;

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

/// Handles all the bookkeeping required after a successful anchor operation:
/// * Adds the operation to the archive buffer
/// * Increments the anchor operation counter
/// * Records anchor activity
pub fn anchor_operation_bookkeeping(
    anchor_number: AnchorNumber,
    operation: Operation,
    previous_activity: Option<Timestamp>,
) {
    archive_operation(anchor_number, caller(), operation);
    state::usage_metrics_mut(|metrics| {
        metrics.anchor_operation_counter += 1;
    });
    active_anchor_stats::update_active_anchors_stats(previous_activity);
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

    Operation::RemoveDevice { device: device_key }
}

/// Updates the device on the anchor to reflect the current usage.
/// Note: This is considered internal bookkeeping and is not recorded in the archive and does not increase anchor operation counter.
pub fn update_last_device_usage(
    anchor_number: AnchorNumber,
    mut anchor: Anchor,
    device_key: &DeviceKey,
) {
    anchor
        .set_device_usage_timestamp(device_key, time())
        .expect("last_usage_timestamp update: unable to update last usage timestamp");
    state::storage_mut(|storage| storage.write(anchor_number, anchor)).unwrap_or_else(|err| {
        panic!("last_usage_timestamp update: unable to update anchor {anchor_number}: {err}")
    })
}
