use crate::archive::{archive_operation, device_diff};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::{Anchor, DeviceDataInternal, TentativeDeviceRegistration};
use crate::{delegation, state, trap_if_not_authenticated};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::*;

pub mod registration;
pub mod tentative_device_registration;

pub fn get_anchor_info(user_number: UserNumber) -> IdentityAnchorInfo {
    let anchor = state::anchor(user_number);
    trap_if_not_authenticated(&anchor);

    let devices = anchor.devices.into_iter().map(DeviceData::from).collect();
    let now = time();

    state::tentative_device_registrations(|tentative_device_registrations| {
        match tentative_device_registrations.get(&user_number) {
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

pub async fn add(user_number: UserNumber, device_data: DeviceData) {
    const MAX_ENTRIES_PER_USER: usize = 10;

    let mut anchor = state::anchor(user_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(&anchor);
    let caller = caller(); // caller is only available before await
    state::ensure_salt_set().await;

    let new_device = DeviceDataInternal::from(device_data);
    check_device(&new_device, &anchor.devices);

    if anchor
        .devices
        .iter()
        .find(|e| e.pubkey == new_device.pubkey)
        .is_some()
    {
        trap("Device already added.");
    }

    if anchor.devices.len() >= MAX_ENTRIES_PER_USER {
        trap(&format!(
            "at most {} authentication information entries are allowed per user",
            MAX_ENTRIES_PER_USER,
        ));
    }

    anchor.devices.push(new_device.clone());
    write_anchor_data(user_number, anchor);

    delegation::prune_expired_signatures();

    archive_operation(
        user_number,
        caller,
        Operation::AddDevice {
            device: DeviceDataWithoutAlias::from(new_device),
        },
    );
}

/// Replace or remove an existing device.
///
/// NOTE: all mutable operations should call this function because it handles device protection
fn mutate_device_or_trap(
    entries: &mut Vec<DeviceDataInternal>,
    device_key: DeviceKey,
    new_value: Option<DeviceDataInternal>,
) -> Operation {
    let index = match entries.iter().position(|e| e.pubkey == device_key) {
        None => trap("Could not find device to mutate, check device key"),
        Some(index) => index,
    };

    let existing_device = entries.get_mut(index).unwrap();

    // Run appropriate checks for protected devices
    match existing_device.protection {
        None => (),
        Some(DeviceProtection::Unprotected) => (),
        Some(DeviceProtection::Protected) => {
            // If the call is not authenticated with the device to mutate, abort
            if caller() != Principal::self_authenticating(&existing_device.pubkey) {
                trap("Device is protected. Must be authenticated with this device to mutate");
            }
        }
    };

    match new_value {
        Some(new_device) => {
            let diff = device_diff(existing_device, &new_device);
            *existing_device = new_device;
            Operation::UpdateDevice {
                device: device_key,
                new_values: diff,
            }
        }
        None => {
            // NOTE: we void the more efficient remove_swap to ensure device ordering
            // is not changed
            entries.remove(index);
            Operation::RemoveDevice { device: device_key }
        }
    }
}

pub async fn update(user_number: UserNumber, device_key: DeviceKey, device_data: DeviceData) {
    if device_key != device_data.pubkey {
        trap("device key may not be updated");
    }
    let mut anchor = state::anchor(user_number);

    trap_if_not_authenticated(&anchor);
    let new_device = DeviceDataInternal::from(device_data);
    check_device(&new_device, &anchor.devices);

    let operation = mutate_device_or_trap(&mut anchor.devices, device_key, Some(new_device));

    write_anchor_data(user_number, anchor);

    delegation::prune_expired_signatures();

    archive_operation(user_number, caller(), operation);
}

pub async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    let mut anchor = state::anchor(user_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(&anchor);

    let caller = caller(); // caller is only available before await
    state::ensure_salt_set().await;
    delegation::prune_expired_signatures();

    let operation = mutate_device_or_trap(&mut anchor.devices, device_key, None);
    write_anchor_data(user_number, anchor);

    archive_operation(user_number, caller, operation);
}

/// Writes the supplied entries to stable memory and updates the anchor operation metric.
fn write_anchor_data(user_number: UserNumber, anchor: Anchor) {
    state::storage_mut(|storage| {
        storage.write(user_number, anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to write data of anchor {}: {}",
                user_number, err
            ))
        });
    });

    state::usage_metrics_mut(|metrics| {
        metrics.anchor_operation_counter += 1;
    });
}

/// This checks some device invariants, in particular:
///   * Sizes of various fields do not exceed limits
///   * Only recovery phrases can be protected
///   * There can only be one recovery phrase
///
///  Otherwise, trap.
///
///  NOTE: while in the future we may lift this restriction, for now we do ensure that
///  protected devices are limited to recovery phrases, which the webapp expects.
fn check_device(device_data: &DeviceDataInternal, existing_devices: &[DeviceDataInternal]) {
    check_entry_limits(device_data);

    if device_data.protection == Some(DeviceProtection::Protected)
        && device_data.key_type != Some(KeyType::SeedPhrase)
    {
        trap(&format!(
            "Only recovery phrases can be protected but key type is {:?}",
            device_data.key_type.unwrap_or(KeyType::Unknown)
        ));
    }

    // if the device is a recovery phrase, check if a different recovery phrase already exists
    if device_data.key_type == Some(KeyType::SeedPhrase)
        && existing_devices.iter().any(|existing_device| {
            existing_device.pubkey != device_data.pubkey
                && existing_device.key_type == Some(KeyType::SeedPhrase)
        })
    {
        trap("There is already a recovery phrase and only one is allowed.");
    }
}

fn check_entry_limits(device_data: &DeviceDataInternal) {
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 300;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 200;

    let n = device_data.alias.len();
    if n > ALIAS_LEN_LIMIT {
        trap(&format!(
            "alias length {} exceeds the limit of {} bytes",
            n, ALIAS_LEN_LIMIT,
        ));
    }

    let n = device_data.pubkey.len();
    if n > PK_LEN_LIMIT {
        trap(&format!(
            "public key length {} exceeds the limit of {} bytes",
            n, PK_LEN_LIMIT,
        ));
    }

    let n = device_data
        .credential_id
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > CREDENTIAL_ID_LEN_LIMIT {
        trap(&format!(
            "credential id length {} exceeds the limit of {} bytes",
            n, CREDENTIAL_ID_LEN_LIMIT,
        ));
    }
}
