use crate::anchor_management::{activity_bookkeeping, post_operation_bookkeeping};
use crate::state::ChallengeInfo;
use crate::storage::anchor::Device;
use crate::storage::Salt;
use crate::{secs_to_nanos, state};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::{call, caller, trap};
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::*;
use rand_core::{RngCore, SeedableRng};
use std::collections::HashMap;

#[cfg(not(feature = "dummy_captcha"))]
use captcha::filters::Wave;
use lazy_static::lazy_static;

mod rate_limit;

// 5 mins
const CAPTCHA_CHALLENGE_LIFETIME: u64 = secs_to_nanos(300);
// How many captcha challenges we keep in memory (at most)
const MAX_INFLIGHT_CHALLENGES: usize = 500;

pub async fn create_challenge() -> Challenge {
    let mut rng = make_rng().await;

    state::inflight_challenges_mut(|inflight_challenges| {
        let now = time();

        // Prune old challenges. This drops all challenges that are older than
        // CAPTCHA_CHALLENGE_LIFETIME
        inflight_challenges.retain(|_, v| v.created > now - CAPTCHA_CHALLENGE_LIFETIME);

        // Error out if there are too many inflight challenges
        if inflight_challenges.len() >= MAX_INFLIGHT_CHALLENGES {
            trap("too many inflight captchas");
        }

        // actually create the challenge

        // First, we try to find a new (unique) challenge key. It's unlikely we'll have collisions
        // when generating the key, but to err on the safe side we try up to 10 times.
        const MAX_TRIES: u8 = 10;

        for _ in 0..MAX_TRIES {
            let challenge_key = random_string(&mut rng, 10);
            if !inflight_challenges.contains_key(&challenge_key) {
                // Then we create the CAPTCHA
                let (Base64(png_base64), chars) = create_captcha(rng);

                // Finally insert
                inflight_challenges.insert(
                    challenge_key.clone(),
                    ChallengeInfo {
                        created: now,
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

// Get a random number generator based on 'raw_rand'
async fn make_rng() -> rand_chacha::ChaCha20Rng {
    let raw_rand: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get seed: {err}")),
    };
    let seed: Salt = raw_rand[..].try_into().unwrap_or_else(|_| {
        trap(&format!(
            "when creating seed from raw_rand output, expected raw randomness to be of length 32, got {}",
            raw_rand.len()
        ));
    });

    rand_chacha::ChaCha20Rng::from_seed(seed)
}

// Generate an n-char long string of random characters. The characters are sampled from the rang
// a-z.
//
// NOTE: The 'rand' crate (currently) does not build on wasm32-unknown-unknown so we have to
// make-do with the RngCore trait (as opposed to Rng), therefore we have to implement this
// ourselves as opposed to using one of rand's distributions.
fn random_string<T: RngCore>(rng: &mut T, n: usize) -> String {
    let mut chars: Vec<u8> = vec![];

    // The range
    let a: u8 = b'a';
    let z: u8 = b'z';

    // n times, get a random number as u32, then shrink to u8, and finally shrink to the size of
    // our range. Finally, offset by the start of our range.
    for _ in 0..n {
        let next: u8 = rng.next_u32() as u8 % (z - a) + a;
        chars.push(next);
    }

    return String::from_utf8_lossy(&chars).to_string();
}

#[cfg(feature = "dummy_captcha")]
fn create_captcha<T: RngCore>(rng: T) -> (Base64, String) {
    let mut captcha = captcha::RngCaptcha::from_rng(rng);
    let captcha = captcha.set_chars(&vec!['a']).add_chars(1).view(96, 48);

    let resp = match captcha.as_base64() {
        Some(png_base64) => Base64(png_base64),
        None => trap("Could not get base64 of captcha"),
    };

    return (resp, captcha.chars_as_string());
}

lazy_static! {
    /// Map of problematic characters that are easily mixed up by humans to the "normalized" replacement.
    /// I.e. the captcha will never contain any of the characters in the key set and any input provided
    /// will be mapped to the matching value.
    /// Note: the captcha library already excludes the characters o, O and 0.
    static ref CHAR_REPLACEMENTS: HashMap<char, char> = vec![
        ('C', 'c'),
        ('l', '1'),
        ('S', 's'),
        ('X', 'x'),
        ('Y', 'y'),
        ('Z', 'z'),
        ('P', 'p'),
        ('W', 'w'),
    ].into_iter().collect();
}

const CAPTCHA_LENGTH: usize = 5;
#[cfg(not(feature = "dummy_captcha"))]
fn create_captcha<T: RngCore>(rng: T) -> (Base64, String) {
    let mut captcha = captcha::RngCaptcha::from_rng(rng);
    let mut chars = captcha.supported_chars();
    chars.retain(|c| !CHAR_REPLACEMENTS.contains_key(c));

    let captcha = captcha
        .set_chars(&chars)
        .add_chars(CAPTCHA_LENGTH as u32)
        .apply_filter(Wave::new(2.0, 20.0).horizontal())
        .apply_filter(Wave::new(2.0, 20.0).vertical())
        .view(220, 120);
    // if you ever change the size of the captcha, make sure to also change the
    // CSS in the frontend to match the new size (.c-captcha-placeholder)

    let resp = match captcha.as_base64() {
        Some(png_base64) => Base64(png_base64),
        None => trap("Could not get base64 of captcha"),
    };

    (resp, captcha.chars_as_string())
}

// Check whether the CAPTCHA challenge was solved
fn check_challenge(res: ChallengeAttempt) -> Result<(), ()> {
    // avoid processing too many characters
    if res.chars.len() > CAPTCHA_LENGTH {
        return Err(());
    }
    // Normalize challenge attempts by replacing characters that are not in the captcha character set
    // with the respective replacement from CHAR_REPLACEMENTS.
    let normalized_challenge_res: String = res
        .chars
        .chars()
        .map(|c| *CHAR_REPLACEMENTS.get(&c).unwrap_or(&c))
        .collect();

    state::inflight_challenges_mut(|inflight_challenges| {
        match inflight_challenges.remove(&res.key) {
            Some(challenge) => {
                if normalized_challenge_res != challenge.chars {
                    return Err(());
                }
                Ok(())
            }
            None => Err(()),
        }
    })
}

pub fn solve_challenge(solution_attempt: ChallengeAttempt) -> ChallengeCheckResult {
    if let Err(()) = check_challenge(solution_attempt) {
        return ChallengeCheckResult::BadChallenge;
    }
    if caller() == Principal::anonymous() {
        return ChallengeCheckResult::InvalidCaller;
    }
    if state::with_temp_keys_mut(|temp_keys| temp_keys.add_onboarding_temp_key(caller())).is_err() {
        return ChallengeCheckResult::RateLimitExcceeded;
    }
    ChallengeCheckResult::Success
}

pub fn register(
    device_data: DeviceData,
    challenge_attempt: Option<ChallengeAttempt>,
) -> RegisterResponse {
    rate_limit::process_rate_limit();
    let temp_key = determine_temp_key();
    if let Err(()) = check_challenge_if_necessary(challenge_attempt, &temp_key) {
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
    let Some((anchor_number, mut anchor)) = allocation else {
        return RegisterResponse::CanisterFull;
    };

    anchor
        .add_device(device.clone())
        .unwrap_or_else(|err| trap(&format!("failed to register anchor {anchor_number}: {err}")));
    activity_bookkeeping(&mut anchor, &device.pubkey);

    // write anchor to stable memory
    state::storage_borrow_mut(|storage| {
        storage.write(anchor_number, anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to write data of anchor {anchor_number}: {err}"
            ))
        });
    });

    // Save / promote the 'temp_key' as a mean of authenticating for the current session, see 'TempKeys'
    if let Some(temp_key) = temp_key {
        state::with_temp_keys_mut(|temp_keys| {
            temp_keys.add_device_bound_temp_key(&device.pubkey, anchor_number, temp_key)
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

/// Determines caller corresponds to a temp key. If so, returns the principal.
fn determine_temp_key() -> Option<Principal> {
    let caller = caller();
    if state::with_temp_keys_mut(|temp_keys| temp_keys.check_onboarding_temp_key(&caller)).is_ok() {
        Some(caller)
    } else {
        None
    }
}

fn check_challenge_if_necessary(
    challenge_attempt: Option<ChallengeAttempt>,
    temp_key: &Option<Principal>,
) -> Result<(), ()> {
    // temp keys have the captcha already verified
    if temp_key.is_some() {
        return Ok(());
    }
    challenge_attempt.map_or(Err(()), check_challenge)
}

/// Perform a sanity check on the caller. If the caller is neither the device nor the temporary key, then we
/// could technically allow this to go through but it's most likely a mistake.
fn verify_caller_is_device_or_temp_key(temp_key: &Option<Principal>, device_principal: &Principal) {
    let caller = caller();
    if &caller != device_principal {
        if let Some(temp_key) = temp_key {
            if &caller != temp_key {
                trap(&format!(
                    "caller {} could not be authenticated against device pubkey {} or temporary key {}",
                    caller,
                    device_principal,
                    temp_key
                ));
            }
        } else {
            trap(&format!(
                "caller {} could not be authenticated against device pubkey {} and no temporary key was sent",
                caller,
                device_principal,
            ));
        }
    }
}
