use crate::anchor_management::write_anchor;
use crate::archive::archive_operation;
use crate::state::ChallengeInfo;
use crate::storage::anchor::Device;
use crate::storage::Salt;
use crate::{delegation, secs_to_nanos, state};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::{call, caller, trap};
use internet_identity_interface::archive::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::*;
use rand_core::{RngCore, SeedableRng};

#[cfg(not(feature = "dummy_captcha"))]
use captcha::filters::Wave;

// 5 mins
const CAPTCHA_CHALLENGE_LIFETIME: u64 = secs_to_nanos(300);
// How many captcha challenges we keep in memory (at most)
const MAX_INFLIGHT_CHALLENGES: usize = 500;

pub async fn create_challenge() -> Challenge {
    let mut rng = make_rng().await;

    delegation::prune_expired_signatures();

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

#[cfg(not(feature = "dummy_captcha"))]
fn create_captcha<T: RngCore>(rng: T) -> (Base64, String) {
    let mut captcha = captcha::RngCaptcha::from_rng(rng);
    let captcha = captcha
        .add_chars(5)
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
    state::inflight_challenges_mut(|inflight_challenges| {
        match inflight_challenges.remove(&res.key) {
            Some(challenge) => {
                if res.chars != challenge.chars {
                    return Err(());
                }
                Ok(())
            }
            None => Err(()),
        }
    })
}

pub fn register(device_data: DeviceData, challenge_result: ChallengeAttempt) -> RegisterResponse {
    delegation::prune_expired_signatures();
    if let Err(()) = check_challenge(challenge_result) {
        return RegisterResponse::BadChallenge;
    }

    let device = Device::from(device_data);

    if caller() != Principal::self_authenticating(&device.pubkey) {
        trap(&format!(
            "{} could not be authenticated against {:?}",
            caller(),
            device.pubkey
        ));
    }

    let allocation = state::storage_mut(|storage| storage.allocate_anchor());
    match allocation {
        Some((anchor_number, mut anchor)) => {
            anchor.add_device(device.clone()).unwrap_or_else(|err| {
                trap(&format!("failed to register anchor {anchor_number}: {err}"))
            });
            write_anchor(anchor_number, anchor);
            archive_operation(
                anchor_number,
                caller(),
                Operation::RegisterAnchor {
                    device: DeviceDataWithoutAlias::from(device),
                },
            );
            RegisterResponse::Registered {
                user_number: anchor_number,
            }
        }
        None => RegisterResponse::CanisterFull,
    }
}
