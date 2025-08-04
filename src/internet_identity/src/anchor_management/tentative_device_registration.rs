use crate::authz_utils::IdentityUpdateError;
use crate::state::RegistrationState::{
    DeviceRegistrationModeActive, DeviceTentativelyAdded, SessionTentativelyAdded,
    SessionTentativelyConfirmed,
};
use crate::state::TentativeDeviceRegistration;
use crate::{secs_to_nanos, state};
use candid::{CandidType, Principal};
use ic_cdk::api::time;
use ic_cdk::{call, trap};
use internet_identity_interface::internet_identity::types::*;
use std::collections::{hash_map, HashMap};

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

pub async fn add_tentative_device(
    anchor_number: AnchorNumber,
    tentative_device: DeviceData,
) -> Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError> {
    let confirmation_code = new_confirmation_code().await;
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            // Get registration else throw error
            let registration = registrations
                .get_mut(&anchor_number)
                .ok_or(AuthnMethodRegisterError::RegistrationModeOff)?;

            match registration {
                // Make sure registration isn't expired
                TentativeDeviceRegistration { expiration, .. } if *expiration <= now => {
                    Err(AuthnMethodRegisterError::RegistrationModeOff)
                }
                // Add tentative device if registration mode is active
                TentativeDeviceRegistration {
                    state: DeviceRegistrationModeActive,
                    ..
                } => {
                    registration.state = DeviceTentativelyAdded {
                        tentative_device,
                        failed_attempts: 0,
                        confirmation_code: confirmation_code.clone(),
                    };
                    Ok(AuthnMethodConfirmationCode {
                        expiration: registration.expiration,
                        confirmation_code,
                    })
                }
                // Else return error
                _ => Err(AuthnMethodRegisterError::RegistrationAlreadyInProgress),
            }
        })
    })
}

pub async fn add_tentative_session(
    anchor_number: AnchorNumber,
    tentative_session: Principal,
) -> Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError> {
    let confirmation_code = new_confirmation_code().await;
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            // Get registration else throw error
            let registration = registrations
                .get_mut(&anchor_number)
                .ok_or(AuthnMethodRegisterError::RegistrationModeOff)?;

            match registration {
                // Make sure registration isn't expired
                TentativeDeviceRegistration { expiration, .. } if *expiration <= now => {
                    Err(AuthnMethodRegisterError::RegistrationModeOff)
                }
                // Add tentative session if registration mode is active
                TentativeDeviceRegistration {
                    state: DeviceRegistrationModeActive,
                    ..
                } => {
                    registration.state = SessionTentativelyAdded {
                        tentative_session,
                        failed_attempts: 0,
                        confirmation_code: confirmation_code.clone(),
                    };
                    Ok(AuthnMethodConfirmationCode {
                        expiration: registration.expiration,
                        confirmation_code,
                    })
                }
                // Else return error
                _ => Err(AuthnMethodRegisterError::RegistrationAlreadyInProgress),
            }
        })
    })
}

/// Confirm the tentative device or session using the submitted `user_confirmation_code`.
///
/// # Returns
/// [`Some(DeviceData)`] if a device (or [`None`] if a session) has been confirmed
///
/// # Errors
/// Returns an error if either there's no tentative device (or session) or the code is incorrect.
pub fn confirm_tentative_device_or_session(
    anchor_number: AnchorNumber,
    user_confirmation_code: DeviceConfirmationCode,
) -> Result<Option<DeviceData>, AuthnMethodConfirmationError> {
    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            // Get registration else throw error
            let registration = registrations
                .get_mut(&anchor_number)
                .ok_or(AuthnMethodConfirmationError::RegistrationModeOff)?;

            let (should_remove, response) = match &mut registration.state {
                DeviceRegistrationModeActive | SessionTentativelyConfirmed { .. } => (
                    false,
                    Err(AuthnMethodConfirmationError::NoAuthnMethodToConfirm),
                ),
                DeviceTentativelyAdded {
                    failed_attempts,
                    confirmation_code,
                    tentative_device,
                } => {
                    if user_confirmation_code == *confirmation_code {
                        // Verification successful - return device to be added and remove the registration
                        (true, Ok(Some(tentative_device.clone())))
                    } else {
                        // Increment failed attempts counter
                        *failed_attempts += 1;

                        // Remove registration if max attempts reached
                        (
                            *failed_attempts >= MAX_DEVICE_REGISTRATION_ATTEMPTS,
                            Err(AuthnMethodConfirmationError::WrongCode {
                                retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - *failed_attempts),
                            }),
                        )
                    }
                }
                SessionTentativelyAdded {
                    failed_attempts,
                    confirmation_code,
                    tentative_session,
                } => {
                    if user_confirmation_code == *confirmation_code {
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
                            Err(AuthnMethodConfirmationError::WrongCode {
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

pub fn get_confirmed_session(anchor_number: AnchorNumber) -> Option<Principal> {
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            registrations
                .get(&anchor_number)
                .and_then(|registration| match registration {
                    // Make sure registration isn't expired
                    TentativeDeviceRegistration { expiration, .. } if *expiration <= now => None,
                    // Return confirmed session
                    TentativeDeviceRegistration {
                        state:
                            SessionTentativelyConfirmed {
                                tentative_session, ..
                            },
                        ..
                    } => Some(*tentative_session),
                    // Else return None
                    _ => None,
                })
        })
    })
}

pub fn get_tentative_registration(anchor_number: AnchorNumber) -> Option<DeviceRegistrationInfo> {
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        state::lookup_tentative_device_registration_mut(|lookup| {
            prune_expired_tentative_device_registrations(registrations, lookup);

            registrations
                .get(&anchor_number)
                .and_then(|registration| match registration {
                    // Make sure registration isn't expired
                    TentativeDeviceRegistration { expiration, .. } if *expiration <= now => None,
                    // Return tentative device
                    TentativeDeviceRegistration {
                        expiration,
                        state:
                            DeviceTentativelyAdded {
                                tentative_device, ..
                            },
                        ..
                    } => Some(DeviceRegistrationInfo {
                        expiration: *expiration,
                        tentative_device: Some(tentative_device.clone()),
                        tentative_session: None,
                    }),
                    // Return tentative session
                    TentativeDeviceRegistration {
                        expiration,
                        state:
                            SessionTentativelyAdded {
                                tentative_session, ..
                            },
                        ..
                    } => Some(DeviceRegistrationInfo {
                        expiration: *expiration,
                        tentative_device: None,
                        tentative_session: Some(*tentative_session),
                    }),
                    // Else return None
                    _ => None,
                })
        })
    })
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

impl From<IdentityUpdateError> for AuthnMethodRegistrationModeExitError {
    fn from(err: IdentityUpdateError) -> Self {
        match err {
            IdentityUpdateError::Unauthorized(principal) => {
                AuthnMethodRegistrationModeExitError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(identity_nr, storage_err) => {
                AuthnMethodRegistrationModeExitError::InternalCanisterError(format!(
                    "Storage error for identity {identity_nr}: {storage_err}"
                ))
            }
        }
    }
}
