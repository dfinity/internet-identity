use crate::anchor_management::{activity_bookkeeping, post_operation_bookkeeping};
use crate::state;
use crate::state::ChallengeInfo;
use crate::storage::anchor::Device;
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::*;

mod captcha;
mod rate_limit;
pub mod registration_flow_v2;

pub use captcha::Base64;

pub async fn create_challenge() -> Challenge {
    let mut rng = captcha::make_rng().await;

    state::inflight_challenges_mut(|inflight_challenges| {
        captcha::prune_expired_challenges(inflight_challenges);

        // Error out if there are too many inflight challenges
        if inflight_challenges.len()
            >= state::persistent_state(|s| s.captcha_config.max_unsolved_captchas) as usize
        {
            trap("too many inflight captchas");
        }

        // actually create the challenge

        // First, we try to find a new (unique) challenge key. It's unlikely we'll have collisions
        // when generating the key, but to err on the safe side we try up to 10 times.
        const MAX_TRIES: u8 = 10;

        for _ in 0..MAX_TRIES {
            let challenge_key = captcha::random_string(&mut rng, 10);
            if !inflight_challenges.contains_key(&challenge_key) {
                // Then we create the CAPTCHA
                let (Base64(png_base64), chars) = captcha::create_captcha(rng);

                // Finally insert
                inflight_challenges.insert(
                    challenge_key.clone(),
                    ChallengeInfo {
                        created: time(),
                        chars,
                    },
                );

                return Challenge {
                    png_base64,
                    challenge_key,
                };
            }
        }

        trap(&format!("Could not find a new key after {MAX_TRIES} tries"));
    })
}

pub fn register(
    device_data: DeviceData,
    challenge_result: ChallengeAttempt,
    // A temporary key than can be used in lieu of 'device_data' for a brief period of time
    // The key is optional for backwards compatibility
    temp_key: Option<Principal>,
) -> RegisterResponse {
    rate_limit::process_rate_limit()
        .unwrap_or_else(|_| trap("rate limit reached, try again later"));
    if let Err(()) = captcha::check_challenge(challenge_result) {
        return RegisterResponse::BadChallenge;
    }

    let device = Device::from(device_data);
    let device_principal = Principal::self_authenticating(&device.pubkey);

    if let Some(ref temp_key) = temp_key {
        if temp_key == &device_principal {
            panic!("temp_key and device key must not be equal")
        }
    }

    // sanity check
    verify_caller_is_device_or_temp_key(&temp_key, &device_principal);

    let allocation = state::storage_borrow_mut(|storage| storage.allocate_anchor());
    let Some(mut anchor) = allocation else {
        return RegisterResponse::CanisterFull;
    };
    let anchor_number = anchor.anchor_number();

    anchor
        .add_device(device.clone())
        .unwrap_or_else(|err| trap(&format!("failed to register anchor {anchor_number}: {err}")));
    activity_bookkeeping(
        &mut anchor,
        &AuthorizationKey::DeviceKey(device.pubkey.clone()),
    );

    // write anchor to stable memory
    state::storage_borrow_mut(|storage| {
        storage.create(anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to write data of anchor {anchor_number}: {err}"
            ))
        });
        storage.registration_rates.new_registration()
    });

    // Save the 'temp_key' as a mean of authenticating for a short period of time, see
    // `TempKeys`
    if let Some(temp_key) = temp_key {
        state::with_temp_keys_mut(|temp_keys| {
            temp_keys.add_temp_key(&device.pubkey, anchor_number, temp_key)
        });
    }

    let operation = Operation::RegisterAnchor {
        device: DeviceDataWithoutAlias::from(device),
    };
    post_operation_bookkeeping(anchor_number, operation);
    RegisterResponse::Registered {
        user_number: anchor_number,
    }
}

/// Perform a sanity check on the caller. If the caller is neither the device nor the temporary key, then we
/// could technically allow this to go through but it's most likely a mistake.
fn verify_caller_is_device_or_temp_key(temp_key: &Option<Principal>, device_principal: &Principal) {
    let caller = caller();
    if &caller != device_principal {
        if let Some(temp_key) = temp_key {
            if &caller != temp_key {
                trap(&format!("caller {caller} could not be authenticated against device pubkey {device_principal} or temporary key {temp_key}"));
            }
        } else {
            trap(&format!("caller {caller} could not be authenticated against device pubkey {device_principal} and no temporary key was sent"));
        }
    }
}
