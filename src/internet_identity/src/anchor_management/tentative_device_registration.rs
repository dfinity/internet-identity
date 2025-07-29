use crate::anchor_management::add_device;
use crate::authz_utils::IdentityUpdateError;
use crate::state::RegistrationState::{
    DeviceRegistrationModeActive, DeviceTentativelyAdded, SessionTentativelyAdded,
    SessionTentativelyConfirmed,
};
use crate::state::TentativeDeviceRegistration;
use crate::storage::anchor::Anchor;
use crate::{secs_to_nanos, state};
use candid::{CandidType, Principal};
use ic_cdk::api::time;
use ic_cdk::{call, trap};
use internet_identity_interface::archive::types::Operation;
use internet_identity_interface::internet_identity::types::*;
use std::collections::hash_map::Entry;
use std::collections::{hash_map, HashMap};
use TentativeDeviceRegistrationError::{AnotherDeviceTentativelyAdded, DeviceRegistrationModeOff};

// 15 mins
const REGISTRATION_MODE_DURATION: u64 = secs_to_nanos(900);
// How many anchors can be in registration mode simultaneously
const MAX_ANCHORS_IN_REGISTRATION_MODE: usize = 10_000;
// How many verification attempts are given for a tentative device
const MAX_DEVICE_REGISTRATION_ATTEMPTS: u8 = 3;

/// Enables device registration mode for the given anchor and returns the expiration timestamp (when it will be disabled again).
/// If the device registration mode is already active for the given registration id it will just return the expiration timestamp again.
pub fn enter_device_registration_mode(
    identity_number: IdentityNumber,
    reg_id: Option<ValidatedRegistrationId>,
) -> Result<Timestamp, AuthnMethodRegistrationModeEnterError> {
    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);
            if registrations.len() >= MAX_ANCHORS_IN_REGISTRATION_MODE {
                return Err(
                    AuthnMethodRegistrationModeEnterError::InternalCanisterError(
                        "too many anchors in device registration mode".to_string(),
                    ),
                );
            }

            match registrations.entry(identity_number) {
                hash_map::Entry::Occupied(in_progress) => match &in_progress.get() {
                    // Return expiration if registration id matches (also true if both None)
                    &TentativeDeviceRegistration { id, expiration, .. } if *id == reg_id => {
                        Ok(*expiration)
                    }
                    // Return error otherwise
                    _ => Err(AuthnMethodRegistrationModeEnterError::AlreadyInProgress),
                },
                hash_map::Entry::Vacant(entry) => {
                    let expiration = time() + REGISTRATION_MODE_DURATION;
                    if let Some(id) = &reg_id {
                        lookup.insert(id.clone(), identity_number);
                    }
                    entry.insert(TentativeDeviceRegistration {
                        expiration,
                        state: DeviceRegistrationModeActive,
                        id: reg_id.clone(),
                    });
                    Ok(expiration)
                }
            }
        })
    })
}

pub fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);
            if let Some(TentativeDeviceRegistration {
                id: Some(reg_id), ..
            }) = registrations.remove(&anchor_number)
            {
                lookup.remove(&reg_id);
            }
        });
    });
}

pub struct TentativeRegistrationInfo {
    pub verification_code: DeviceConfirmationCode,
    pub device_registration_timeout: Timestamp,
}

#[derive(Debug)]
pub enum TentativeDeviceRegistrationError {
    DeviceRegistrationModeOff,
    AnotherDeviceTentativelyAdded,
    IdentityUpdateError(IdentityUpdateError),
}

impl From<IdentityUpdateError> for TentativeDeviceRegistrationError {
    fn from(err: IdentityUpdateError) -> Self {
        TentativeDeviceRegistrationError::IdentityUpdateError(err)
    }
}

pub async fn add_tentative_device(
    anchor_number: AnchorNumber,
    tentative_device: DeviceData,
) -> Result<TentativeRegistrationInfo, TentativeDeviceRegistrationError> {
    let confirmation_code = new_confirmation_code().await;
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            // Get registration else throw error
            let registration = registrations
                .get_mut(&anchor_number)
                .ok_or(DeviceRegistrationModeOff)?;

            match registration {
                // Make sure registration isn't expired
                TentativeDeviceRegistration { expiration, .. } if *expiration <= now => {
                    Err(DeviceRegistrationModeOff)
                }
                // Add tentative session if registration mode is active
                TentativeDeviceRegistration {
                    state: DeviceRegistrationModeActive,
                    ..
                } => {
                    registration.state = DeviceTentativelyAdded {
                        tentative_device,
                        failed_attempts: 0,
                        confirmation_code: confirmation_code.clone(),
                    };
                    Ok(TentativeRegistrationInfo {
                        device_registration_timeout: registration.expiration,
                        verification_code: confirmation_code,
                    })
                }
                // Else return error
                _ => Err(AnotherDeviceTentativelyAdded),
            }
        })
    })
}

#[derive(Debug)]
pub enum VerifyTentativeDeviceError {
    WrongCode { retries_left: u8 },
    DeviceRegistrationModeOff,
    NoDeviceToVerify,
}

/// Confirm the tentative device or session using the submitted `user_confirmation_code`.
///
/// # Returns
/// [`Some(DeviceData)`] if a device (or [`None`] if a session) has been confirmed
///
/// # Errors
/// Returns an error if either there's no tentative device or session or the code is incorrect.
pub fn confirm_tentative_device_or_session(
    anchor_number: AnchorNumber,
    user_confirmation_code: DeviceConfirmationCode,
) -> Result<Option<DeviceData>, VerifyTentativeDeviceError> {
    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            // Get registration else throw error
            let registration = registrations
                .get_mut(&anchor_number)
                .ok_or(VerifyTentativeDeviceError::DeviceRegistrationModeOff)?;

            let (should_remove, response) = match &mut registration.state {
                DeviceRegistrationModeActive | SessionTentativelyConfirmed { .. } => {
                    (false, Err(VerifyTentativeDeviceError::NoDeviceToVerify))
                }
                DeviceTentativelyAdded {
                    failed_attempts,
                    confirmation_code: verification_code,
                    tentative_device,
                } => {
                    if user_confirmation_code == *verification_code {
                        // Verification successful - return device to be added and remove the registration
                        (true, Ok(Some(tentative_device.clone())))
                    } else {
                        // Increment failed attempts counter
                        *failed_attempts += 1;

                        // Remove registration if max attempts reached
                        (
                            *failed_attempts >= MAX_DEVICE_REGISTRATION_ATTEMPTS,
                            Err(VerifyTentativeDeviceError::WrongCode {
                                retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - *failed_attempts),
                            }),
                        )
                    }
                }
                SessionTentativelyAdded {
                    failed_attempts,
                    confirmation_code: verification_code,
                    tentative_session,
                } => {
                    if user_confirmation_code == *verification_code {
                        let principal = *tentative_session;

                        // Verification successful - we'll confirm the session and keep the registration
                        registration.state = SessionTentativelyConfirmed {
                            tentative_session: principal,
                        };
                        (false, Ok(None))
                    } else {
                        // Increment failed attempts counter
                        *failed_attempts += 1;

                        // Remove registration if max attempts reached
                        (
                            *failed_attempts >= MAX_DEVICE_REGISTRATION_ATTEMPTS,
                            Err(VerifyTentativeDeviceError::WrongCode {
                                retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - *failed_attempts),
                            }),
                        )
                    }
                }
            };
            // Now handle removal if needed, after we're done with the mutable borrow
            if should_remove {
                if let Some(TentativeDeviceRegistration {
                    id: Some(reg_id), ..
                }) = registrations.remove(&anchor_number)
                {
                    // Clean up the lookup table
                    lookup.remove(&reg_id);
                }
            }
            response
        })
    })
}

/// Return a decimal representation of a random `u32` to be used as confirmation code
async fn new_confirmation_code() -> DeviceConfirmationCode {
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
    lookup: &mut HashMap<ValidatedRegistrationId, AnchorNumber>,
) {
    let now = time();

    registrations.retain(|_, TentativeDeviceRegistration { expiration, id, .. }| {
        let expired = now >= *expiration;
        if expired {
            if let Some(id) = id {
                lookup.remove(id);
            }
        }
        !expired
    });
}

#[derive(CandidType, Clone, Eq, PartialEq, Hash)]
pub struct ValidatedRegistrationId(String);

impl ValidatedRegistrationId {
    pub fn try_new(s: String) -> Result<Self, String> {
        if s.chars().count() != 5 {
            return Err("RegistrationId must be exactly 5 characters".to_string());
        }

        if !s.chars().all(|c| c.is_ascii_alphanumeric()) {
            return Err(
                "RegistrationId must only contain characters from base62 encoding (0-9, A-Z, a-z)"
                    .to_string(),
            );
        }

        Ok(ValidatedRegistrationId(s))
    }
}

impl From<IdentityUpdateError> for AuthnMethodRegistrationModeEnterError {
    fn from(err: IdentityUpdateError) -> Self {
        match err {
            IdentityUpdateError::Unauthorized(principal) => {
                AuthnMethodRegistrationModeEnterError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(identity_nr, storage_err) => {
                AuthnMethodRegistrationModeEnterError::InternalCanisterError(format!(
                    "Storage error for identity {identity_nr}: {storage_err}"
                ))
            }
        }
    }
}
