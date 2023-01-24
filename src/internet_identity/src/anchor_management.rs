use crate::archive::{archive_operation, device_diff};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::TentativeDeviceRegistration;
use crate::storage::anchor::{Anchor, Device};
use crate::{delegation, state, trap_if_not_authenticated};
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::*;

pub mod registration;
pub mod tentative_device_registration;

pub fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    let anchor = state::anchor(anchor_number);
    trap_if_not_authenticated(&anchor, &anchor_number);

    let devices = anchor
        .into_devices()
        .into_iter()
        .map(DeviceData::from)
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

pub fn add(anchor_number: AnchorNumber, device_data: DeviceData) {
    let mut anchor = state::anchor(anchor_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(&anchor, &anchor_number);

    let new_device = Device::from(device_data);
    anchor.add_device(new_device.clone()).unwrap_or_else(|err| {
        trap(&format!(
            "failed to add device to anchor {}: {}",
            anchor_number, err
        ))
    });
    write_anchor(anchor_number, anchor);
    archive_operation(
        anchor_number,
        caller(),
        Operation::AddDevice {
            device: DeviceDataWithoutAlias::from(new_device),
        },
    );
    delegation::prune_expired_signatures();
}

pub fn update(user_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    let mut anchor = state::anchor(user_number);
    trap_if_not_authenticated(&anchor, &user_number);

    let Some(existing_device) = anchor.device(&device_key) else {
        trap("Could not find device to update, check device key")
    };

    let new_device = Device::from(device_data);
    let diff = device_diff(existing_device, &new_device);

    anchor
        .modify_device(&device_key, new_device)
        .unwrap_or_else(|err| {
            trap(&format!(
                "failed to modify device of anchor {}: {}",
                user_number, err
            ))
        });
    write_anchor(user_number, anchor);

    delegation::prune_expired_signatures();
    archive_operation(
        user_number,
        caller(),
        Operation::UpdateDevice {
            device: device_key,
            new_values: diff,
        },
    );
}

pub fn replace(anchor_number: AnchorNumber, old_device: DeviceKey, new_device: DeviceData) {
    let mut anchor = state::anchor(anchor_number);
    trap_if_not_authenticated(&anchor, &anchor_number);

    anchor.remove_device(&old_device).unwrap_or_else(|err| {
        trap(&format!(
            "failed to replace device of anchor {}: {}",
            anchor_number, err
        ))
    });
    let new_device = Device::from(new_device);
    anchor.add_device(new_device.clone()).unwrap_or_else(|err| {
        trap(&format!(
            "failed to replace device of anchor {}: {}",
            anchor_number, err
        ))
    });
    write_anchor(anchor_number, anchor);
    archive_operation(
        anchor_number,
        caller(),
        Operation::ReplaceDevice {
            old_device,
            new_device: DeviceDataWithoutAlias::from(new_device),
        },
    );
    delegation::prune_expired_signatures();
}

pub fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    let mut anchor = state::anchor(anchor_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(&anchor, &anchor_number);

    anchor.remove_device(&device_key).unwrap_or_else(|err| {
        trap(&format!(
            "failed to remove device of anchor {}: {}",
            anchor_number, err
        ))
    });
    write_anchor(anchor_number, anchor);

    archive_operation(
        anchor_number,
        caller(),
        Operation::RemoveDevice { device: device_key },
    );
    delegation::prune_expired_signatures();
}

/// Writes the supplied entries to stable memory and updates the anchor operation metric.
fn write_anchor(anchor_number: AnchorNumber, anchor: Anchor) {
    state::storage_mut(|storage| {
        storage.write(anchor_number, anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to write data of anchor {}: {}",
                anchor_number, err
            ))
        });
    });

    state::usage_metrics_mut(|metrics| {
        metrics.anchor_operation_counter += 1;
    });
}
