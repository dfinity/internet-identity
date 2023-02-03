use crate::state::RegistrationState::{DeviceRegistrationModeActive, DeviceTentativelyAdded};
use crate::state::TentativeDeviceRegistration;
use crate::{add, secs_to_nanos, state};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::{call, trap};
use internet_identity_interface::AddTentativeDeviceResponse::{
    AddedTentatively, AnotherDeviceTentativelyAdded,
};
use internet_identity_interface::VerifyTentativeDeviceResponse::{NoDeviceToVerify, WrongCode};
use internet_identity_interface::*;
use std::collections::HashMap;

// 15 mins
const REGISTRATION_MODE_DURATION: u64 = secs_to_nanos(900);
// How many anchors can be in registration mode simultaneously
const MAX_ANCHORS_IN_REGISTRATION_MODE: usize = 10_000;
// How many verification attempts are given for a tentative device
const MAX_DEVICE_REGISTRATION_ATTEMPTS: u8 = 3;

/// Enables device registration mode for the given anchor and returns the expiration timestamp (when it will be disabled again).
/// If the device registration mode is already active it will just return the expiration timestamp again.
pub fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);
        if registrations.len() >= MAX_ANCHORS_IN_REGISTRATION_MODE {
            trap("too many anchors in device registration mode");
        }

        match registrations.get(&anchor_number) {
            Some(TentativeDeviceRegistration { expiration, .. }) => *expiration, // already enabled, just return the existing expiration
            None => {
                let expiration = time() + REGISTRATION_MODE_DURATION;
                registrations.insert(
                    anchor_number,
                    TentativeDeviceRegistration {
                        expiration,
                        state: DeviceRegistrationModeActive,
                    },
                );
                expiration
            }
        }
    })
}

pub fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);
        registrations.remove(&anchor_number)
    });
}

pub async fn add_tentative_device(
    anchor_number: AnchorNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    let verification_code = new_verification_code().await;
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);

        match registrations.get_mut(&anchor_number) {
            None => AddTentativeDeviceResponse::DeviceRegistrationModeOff,
            Some(TentativeDeviceRegistration { expiration, .. }) if *expiration <= now => {
                AddTentativeDeviceResponse::DeviceRegistrationModeOff
            }
            Some(TentativeDeviceRegistration {
                state: DeviceTentativelyAdded { .. },
                ..
            }) => AnotherDeviceTentativelyAdded,
            Some(mut registration) => {
                registration.state = DeviceTentativelyAdded {
                    tentative_device: device_data,
                    failed_attempts: 0,
                    verification_code: verification_code.clone(),
                };
                AddedTentatively {
                    device_registration_timeout: registration.expiration,
                    verification_code,
                }
            }
        }
    })
}

pub fn verify_tentative_device(
    anchor_number: AnchorNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    match get_verified_device(anchor_number, user_verification_code) {
        Ok(device) => {
            add(anchor_number, device);
            VerifyTentativeDeviceResponse::Verified
        }
        Err(err) => err,
    }
}

/// Checks the device verification code for a tentative device.
/// If valid, returns the device to be added and exits device registration mode
/// If invalid, returns the appropriate error to send to the client and increases failed attempts. Exits device registration mode if there are no retries left.
fn get_verified_device(
    anchor_number: AnchorNumber,
    user_verification_code: DeviceVerificationCode,
) -> Result<DeviceData, VerifyTentativeDeviceResponse> {
    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);

        let mut tentative_registration = registrations
            .remove(&anchor_number)
            .ok_or(VerifyTentativeDeviceResponse::DeviceRegistrationModeOff)?;

        match tentative_registration.state {
            DeviceRegistrationModeActive => Err(NoDeviceToVerify),
            DeviceTentativelyAdded {
                failed_attempts,
                verification_code,
                tentative_device,
            } => {
                if user_verification_code == verification_code {
                    return Ok(tentative_device);
                }

                let failed_attempts = failed_attempts + 1;
                if failed_attempts < MAX_DEVICE_REGISTRATION_ATTEMPTS {
                    tentative_registration.state = DeviceTentativelyAdded {
                        failed_attempts,
                        tentative_device,
                        verification_code,
                    };
                    // reinsert because retries are allowed
                    registrations.insert(anchor_number, tentative_registration);
                }
                Err(WrongCode {
                    retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - failed_attempts),
                })
            }
        }
    })
}

/// Return a decimal representation of a random `u32` to be used as verification code
async fn new_verification_code() -> DeviceVerificationCode {
    let res: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get randomness: {err}")),
    };
    let rand = u32::from_be_bytes(res[..4].try_into().unwrap_or_else(|_| {
        trap(&format!(
            "when creating device verification code from raw_rand output, expected raw randomness to be of length 32, got {}",
            res.len()
        ));
    }));

    // the format! makes sure that the resulting string has exactly 6 characters.
    format!("{:06}", (rand % 1_000_000))
}

/// Removes __all__ expired device registrations -> there is no need to check expiration immediately after pruning.
fn prune_expired_tentative_device_registrations(
    registrations: &mut HashMap<AnchorNumber, TentativeDeviceRegistration>,
) {
    let now = time();

    registrations.retain(|_, TentativeDeviceRegistration { expiration, .. }| *expiration > now)
}
