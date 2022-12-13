use crate::archive::{archive_operation, device_diff};
use crate::state::RegistrationState::DeviceTentativelyAdded;
use crate::state::{Anchor, Device, TentativeDeviceRegistration};
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

    let new_device = Device::from(device_data);
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
    write_anchor(user_number, anchor);

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
    anchor: &mut Anchor,
    device_key: DeviceKey,
    new_value: Option<Device>,
) -> Operation {
    let index = match anchor.devices.iter().position(|e| e.pubkey == device_key) {
        None => trap("Could not find device to mutate, check device key"),
        Some(index) => index,
    };

    let existing_device = anchor.devices.get_mut(index).unwrap();

    // Run appropriate checks for protected devices
    match existing_device.protection {
        DeviceProtection::Unprotected => (),
        DeviceProtection::Protected => {
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
            anchor.devices.remove(index);
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
    let new_device = Device::from(device_data);
    check_device(&new_device, &anchor.devices);

    let operation = mutate_device_or_trap(&mut anchor, device_key, Some(new_device));

    write_anchor(user_number, anchor);

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

    let operation = mutate_device_or_trap(&mut anchor, device_key, None);
    write_anchor(user_number, anchor);

    archive_operation(user_number, caller, operation);
}

/// Writes the supplied entries to stable memory and updates the anchor operation metric.
fn write_anchor(user_number: UserNumber, anchor: Anchor) {
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
///   * Sum of sizes of all variable length fields does not exceed limit
///   * Only recovery phrases can be protected
///   * There can only be one recovery phrase
///
///  Otherwise, trap.
///
///  NOTE: while in the future we may lift this restriction, for now we do ensure that
///  protected devices are limited to recovery phrases, which the webapp expects.
fn check_device(device: &Device, existing_devices: &[Device]) {
    /// Single devices can use up to 564 bytes for the variable length fields alone.
    /// In order to not give away all the anchor space to the device vector, we limit the sum of the
    /// size of all variable fields of all devices. This ensures that we have the flexibility to expand
    /// or change anchors in the future.
    /// The value 2048 was chosen because it is the max anchor size before the stable memory migration.
    /// This means that all pre-existing anchors are below this limit. And after the migration, the
    /// candid encoded `vec devices` will stay far below 4KB in size (testing showed anchors of up to
    /// 2259 bytes).
    const VARIABLE_FIELDS_LIMIT: usize = 2048;

    check_entry_limits(device);

    if device.protection == DeviceProtection::Protected && device.key_type != KeyType::SeedPhrase {
        trap(&format!(
            "Only recovery phrases can be protected but key type is {:?}",
            device.key_type
        ));
    }

    // if the device is a recovery phrase, check if a different recovery phrase already exists
    if device.key_type == KeyType::SeedPhrase
        && existing_devices.iter().any(|existing_device| {
            existing_device.pubkey != device.pubkey
                && existing_device.key_type == KeyType::SeedPhrase
        })
    {
        trap("There is already a recovery phrase and only one is allowed.");
    }

    let existing_variable_size: usize = existing_devices
        .iter()
        // filter out the device being checked to not count it twice in case of update operations
        .filter(|elem| elem.pubkey != device.pubkey)
        .map(|device| device.variable_fields_len())
        .sum();

    if existing_variable_size + device.variable_fields_len() > VARIABLE_FIELDS_LIMIT {
        trap("Devices exceed allowed storage limit. Either use shorter aliases or remove an existing device.")
    }
}

fn check_entry_limits(device: &Device) {
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 300;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 200;

    let n = device.alias.len();
    if n > ALIAS_LEN_LIMIT {
        trap(&format!(
            "alias length {} exceeds the limit of {} bytes",
            n, ALIAS_LEN_LIMIT,
        ));
    }

    let n = device.pubkey.len();
    if n > PK_LEN_LIMIT {
        trap(&format!(
            "public key length {} exceeds the limit of {} bytes",
            n, PK_LEN_LIMIT,
        ));
    }

    let n = device
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
