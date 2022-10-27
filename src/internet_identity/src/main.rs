use crate::assets::init_assets;
use crate::state::RegistrationState::{DeviceRegistrationModeActive, DeviceTentativelyAdded};
use crate::state::{AssetHashes, ChallengeInfo, DeviceDataInternal, TentativeDeviceRegistration};
use crate::AddTentativeDeviceResponse::{AddedTentatively, AnotherDeviceTentativelyAdded};
use crate::VerifyTentativeDeviceResponse::{NoDeviceToVerify, WrongCode};
use assets::ContentType;
use candid::Principal;
use ic_cdk::api::call::call;
use ic_cdk::api::{caller, data_certificate, id, set_certified_data, time, trap};
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_certified_map::{AsHashTree, Hash, HashTree};
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity::signature_map::SignatureMap;
use rand_chacha::rand_core::{RngCore, SeedableRng};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::convert::TryInto;
use storage::{Salt, Storage};

use internet_identity_interface::*;

mod assets;
mod hash;
mod http;
mod state;
mod storage;

const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}

#[cfg(not(feature = "dummy_captcha"))]
use captcha::filters::Wave;

// 30 mins
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 60);
// 30 days
const MAX_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 24 * 60 * 60);
// 1 min
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(60);

// 5 mins
const CAPTCHA_CHALLENGE_LIFETIME: u64 = secs_to_nanos(300);
// How many captcha challenges we keep in memory (at most)
const MAX_INFLIGHT_CHALLENGES: usize = 500;

// 15 mins
const REGISTRATION_MODE_DURATION: u64 = secs_to_nanos(900);
// How many users can be in registration mode simultaneously
const MAX_USERS_IN_REGISTRATION_MODE: usize = 10_000;
// How many verification attempts are given for a tentative device
const MAX_DEVICE_REGISTRATION_ATTEMPTS: u8 = 3;

const LABEL_ASSETS: &[u8] = b"http_assets";
const LABEL_SIG: &[u8] = b"sig";

#[update]
async fn init_salt() {
    state::init_salt().await;
}

/// Enables device registration mode for the given user and returns the expiration timestamp (when it will be disabled again).
/// If the device registration mode is already active it will just return the expiration timestamp again.
#[update]
fn enter_device_registration_mode(user_number: UserNumber) -> Timestamp {
    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);
        if registrations.len() >= MAX_USERS_IN_REGISTRATION_MODE {
            trap("too many users in device registration mode");
        }

        match registrations.get(&user_number) {
            Some(TentativeDeviceRegistration { expiration, .. }) => *expiration, // already enabled, just return the existing expiration
            None => {
                let expiration = time() + REGISTRATION_MODE_DURATION;
                registrations.insert(
                    user_number,
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

#[update]
fn exit_device_registration_mode(user_number: UserNumber) {
    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);
        registrations.remove(&user_number)
    });
}

#[update]
async fn add_tentative_device(
    user_number: UserNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    let verification_code = new_verification_code().await;
    let now = time();

    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);

        match registrations.get_mut(&user_number) {
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

#[update]
async fn verify_tentative_device(
    user_number: UserNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    match get_verified_device(user_number, user_verification_code) {
        Ok(device) => {
            add(user_number, device).await;
            VerifyTentativeDeviceResponse::Verified
        }
        Err(err) => err,
    }
}

/// Checks the device verification code for a tentative device.
/// If valid, returns the device to be added and exits device registration mode
/// If invalid, returns the appropriate error to send to the client and increases failed attempts. Exits device registration mode if there are no retries left.
fn get_verified_device(
    user_number: UserNumber,
    user_verification_code: DeviceVerificationCode,
) -> Result<DeviceData, VerifyTentativeDeviceResponse> {
    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::tentative_device_registrations_mut(|registrations| {
        prune_expired_tentative_device_registrations(registrations);

        let mut tentative_registration = registrations
            .remove(&user_number)
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
                    registrations.insert(user_number, tentative_registration);
                }
                return Err(WrongCode {
                    retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - failed_attempts),
                });
            }
        }
    })
}

/// Return a decimal representation of a random `u32` to be used as verification code
async fn new_verification_code() -> DeviceVerificationCode {
    let res: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get randomness: {}", err)),
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
    registrations: &mut HashMap<UserNumber, TentativeDeviceRegistration>,
) {
    let now = time();

    registrations.retain(|_, value| match &value {
        TentativeDeviceRegistration { expiration, .. } => expiration > &now,
    })
}

#[update]
async fn register(device_data: DeviceData, challenge_result: ChallengeAttempt) -> RegisterResponse {
    if let Err(()) = check_challenge(challenge_result) {
        return RegisterResponse::BadChallenge;
    }

    check_device(&device_data, &vec![]);

    if caller() != Principal::self_authenticating(device_data.pubkey.clone()) {
        trap(&format!(
            "{} could not be authenticated against {:?}",
            caller(),
            device_data.pubkey
        ));
    }

    state::ensure_salt_set().await;
    prune_expired_signatures();

    let allocation = state::storage_mut(|storage| storage.allocate_user_number());
    match allocation {
        Some(user_number) => {
            write_anchor_data(user_number, vec![DeviceDataInternal::from(device_data)]);
            RegisterResponse::Registered { user_number }
        }
        None => RegisterResponse::CanisterFull,
    }
}

#[update]
async fn add(user_number: UserNumber, device_data: DeviceData) {
    const MAX_ENTRIES_PER_USER: usize = 10;

    let mut entries = state::anchor_devices(user_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));
    state::ensure_salt_set().await;

    check_device(&device_data, &entries);

    if entries
        .iter()
        .find(|e| e.pubkey == device_data.pubkey)
        .is_some()
    {
        trap("Device already added.");
    }

    if entries.len() >= MAX_ENTRIES_PER_USER {
        trap(&format!(
            "at most {} authentication information entries are allowed per user",
            MAX_ENTRIES_PER_USER,
        ));
    }

    entries.push(DeviceDataInternal::from(device_data));
    write_anchor_data(user_number, entries);

    prune_expired_signatures();
}

/// Replace or remove an existing device.
///
/// NOTE: all mutable operations should call this function because it handles device protection
fn mutate_device_or_trap(
    entries: &mut Vec<DeviceDataInternal>,
    device_key: DeviceKey,
    new_value: Option<DeviceData>,
) {
    let index = match entries.iter().position(|e| e.pubkey == device_key) {
        None => trap("Could not find device to mutate, check device key"),
        Some(index) => index,
    };

    let device = entries.get_mut(index).unwrap();

    // Run appropriate checks for protected devices
    match device.protection {
        None => (),
        Some(DeviceProtection::Unprotected) => (),
        Some(DeviceProtection::Protected) => {
            // If the call is not authenticated with the device to mutate, abort
            if caller() != Principal::self_authenticating(&device.pubkey) {
                trap("Device is protected. Must be authenticated with this device to mutate");
            }
        }
    };

    match new_value {
        Some(device_data) => {
            *device = device_data.into();
        }
        None => {
            // NOTE: we void the more efficient remove_swap to ensure device ordering
            // is not changed
            entries.remove(index);
        }
    }
}

#[update]
async fn update(user_number: UserNumber, device_key: DeviceKey, device_data: DeviceData) {
    if device_key != device_data.pubkey {
        trap("device key may not be updated");
    }
    let mut entries = state::anchor_devices(user_number);

    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));
    check_device(&device_data, &entries);

    mutate_device_or_trap(&mut entries, device_key, Some(device_data));

    write_anchor_data(user_number, entries);

    prune_expired_signatures();
}

#[update]
async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    let mut entries = state::anchor_devices(user_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::ensure_salt_set().await;
    prune_expired_signatures();

    mutate_device_or_trap(&mut entries, device_key, None);
    write_anchor_data(user_number, entries);
}

/// Writes the supplied entries to stable memory and updates the anchor operation metric.
fn write_anchor_data(user_number: UserNumber, entries: Vec<DeviceDataInternal>) {
    state::storage_mut(|storage| {
        storage.write(user_number, entries).unwrap_or_else(|err| {
            trap(&format!(
                "failed to write device data of user {}: {}",
                user_number, err
            ))
        });
    });

    state::usage_metrics_mut(|metrics| {
        metrics.anchor_operation_counter += 1;
    });
}

#[update]
async fn create_challenge() -> Challenge {
    let mut rng = make_rng().await;

    prune_expired_signatures();

    state::inflight_challenges_mut(|inflight_challenges| {
        let now = time() as u64;

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

        trap(&format!(
            "Could not find a new key after {} tries",
            MAX_TRIES
        ));
    })
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
    let a: u8 = 'a' as u8;
    let z: u8 = 'z' as u8;

    // n times, get a random number as u32, then shrink to u8, and finally shrink to the size of
    // our range. Finally, offset by the start of our range.
    for _ in 0..n {
        let next: u8 = rng.next_u32() as u8 % (z - a) + a;
        chars.push(next);
    }

    return String::from_utf8_lossy(&chars).to_string();
}

// Get a random number generator based on 'raw_rand'
async fn make_rng() -> rand_chacha::ChaCha20Rng {
    let raw_rand: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get seed: {}", err)),
    };
    let seed: Salt = raw_rand[..].try_into().unwrap_or_else(|_| {
        trap(&format!(
                "when creating seed from raw_rand output, expected raw randomness to be of length 32, got {}",
                raw_rand.len()
                ));
    });

    rand_chacha::ChaCha20Rng::from_seed(seed)
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

    let resp = match captcha.as_base64() {
        Some(png_base64) => Base64(png_base64),
        None => trap("Could not get base64 of captcha"),
    };

    return (resp, captcha.chars_as_string());
}

// Check whether the CAPTCHA challenge was solved
fn check_challenge(res: ChallengeAttempt) -> Result<(), ()> {
    state::inflight_challenges_mut(|inflight_challenges| {
        match inflight_challenges.remove(&res.key) {
            Some(challenge) => {
                if res.chars != challenge.chars {
                    return Err(());
                }
                return Ok(());
            }
            None => Err(()),
        }
    })
}

/// Returns all devices of the user (authentication and recovery) but no information about device registrations.
/// Note: Will be changed in the future to be more consistent with get_anchor_info.
#[query]
fn lookup(user_number: UserNumber) -> Vec<DeviceData> {
    state::storage(|storage| {
        storage
            .read(user_number)
            .unwrap_or_default()
            .into_iter()
            .map(DeviceData::from)
            .collect()
    })
}

#[update] // this is an update call because queries are not (yet) certified
fn get_anchor_info(user_number: UserNumber) -> IdentityAnchorInfo {
    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    let devices = entries.into_iter().map(DeviceData::from).collect();
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

#[query]
fn get_principal(user_number: UserNumber, frontend: FrontendHostname) -> Principal {
    check_frontend_length(&frontend);

    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    let seed = calculate_seed(user_number, &frontend);
    let public_key = der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(&public_key)
}

/// This makes this Candid service self-describing, so that for example Candid UI, but also other
/// tools, can seamlessly integrate with it. The concrete interface (method name etc.) is
/// provisional, but works.
#[query]
fn __get_candid_interface_tmp_hack() -> String {
    include_str!("../internet_identity.did").to_string()
}

#[update]
async fn prepare_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    let entries = state::anchor_devices(user_number);
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::ensure_salt_set().await;
    prune_expired_signatures();
    check_frontend_length(&frontend);

    let delta = u64::min(
        max_time_to_live.unwrap_or(DEFAULT_EXPIRATION_PERIOD_NS),
        MAX_EXPIRATION_PERIOD_NS,
    );
    let expiration = (time() as u64).saturating_add(delta);
    let seed = calculate_seed(user_number, &frontend);

    state::signature_map_mut(|sigs| {
        add_signature(sigs, session_key, seed, expiration);
    });
    update_root_hash();

    state::usage_metrics_mut(|metrics| {
        metrics.delegation_counter += 1;
    });
    (
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    )
}

#[query]
fn get_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);

    let entries = state::anchor_devices(user_number);
    trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

    state::asset_hashes_and_sigs(|asset_hashes, sigs| {
        match get_signature(
            asset_hashes,
            sigs,
            session_key.clone(),
            calculate_seed(user_number, &frontend),
            expiration,
        ) {
            Some(signature) => GetDelegationResponse::SignedDelegation(SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                },
                signature: ByteBuf::from(signature),
            }),
            None => GetDelegationResponse::NoSuchDelegation,
        }
    })
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[query]
fn stats() -> InternetIdentityStats {
    state::storage(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_user_number_range(),
        users_registered: storage.user_count() as u64,
    })
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    state::storage_mut(|storage| {
        if let Some(arg) = maybe_arg {
            *storage = Storage::new(arg.assigned_user_number_range, DefaultMemoryImpl::default());
        }
        storage.flush();
    });

    update_root_hash();
}

#[post_upgrade]
fn retrieve_data() {
    init_assets();
    state::initialize_from_stable_memory();

    // We drop all the signatures on upgrade, users will
    // re-request them if needed.
    update_root_hash();
}

fn calculate_seed(user_number: UserNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let user_number_str = user_number.to_string();
    let user_number_blob = user_number_str.bytes();
    blob.push(user_number_blob.len() as u8);
    blob.extend(user_number_blob);

    blob.push(frontend.bytes().len() as u8);
    blob.extend(frontend.bytes());

    hash::hash_bytes(blob)
}

fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
    let my_canister_id: Vec<u8> = id().as_ref().to_vec();

    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(my_canister_id.len() as u8);
    bitstring.extend(my_canister_id);
    bitstring.extend(seed);

    let mut der: Vec<u8> = vec![];
    // sequence of length 17 + the bit string length
    der.push(0x30);
    der.push(17 + bitstring.len() as u8);
    der.extend(vec![
        // sequence of length 12 for the OID
        0x30, 0x0C, // OID 1.3.6.1.4.1.56387.1.2
        0x06, 0x0A, 0x2B, 0x06, 0x01, 0x04, 0x01, 0x83, 0xB8, 0x43, 0x01, 0x02,
    ]);
    // BIT string of given length
    der.push(0x03);
    der.push(1 + bitstring.len() as u8);
    der.push(0x00);
    der.extend(bitstring);
    der
}

fn delegation_signature_msg_hash(d: &Delegation) -> Hash {
    use hash::Value;

    let mut m = HashMap::new();
    m.insert("pubkey", Value::Bytes(d.pubkey.as_slice()));
    m.insert("expiration", Value::U64(d.expiration));
    if let Some(targets) = d.targets.as_ref() {
        let mut arr = Vec::with_capacity(targets.len());
        for t in targets.iter() {
            arr.push(Value::Bytes(t.as_ref()));
        }
        m.insert("targets", Value::Array(arr));
    }
    let map_hash = hash::hash_of_map(m);
    hash::hash_with_domain(b"ic-request-auth-delegation", &map_hash)
}

fn update_root_hash() {
    use ic_certified_map::{fork_hash, labeled_hash};
    state::asset_hashes_and_sigs(|asset_hashes, sigs| {
        let prefixed_root_hash = fork_hash(
            // NB: Labels added in lexicographic order
            &labeled_hash(LABEL_ASSETS, &asset_hashes.root_hash()),
            &labeled_hash(LABEL_SIG, &sigs.root_hash()),
        );
        set_certified_data(&prefixed_root_hash[..]);
    })
}

fn get_signature(
    asset_hashes: &AssetHashes,
    sigs: &SignatureMap,
    pk: PublicKey,
    seed: Hash,
    expiration: Timestamp,
) -> Option<Vec<u8>> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let witness = sigs.witness(hash::hash_bytes(seed), msg_hash)?;

    let witness_hash = witness.reconstruct();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(&witness_hash),
            hex::encode(&root_hash)
        ));
    }

    let tree = ic_certified_map::fork(
        HashTree::Pruned(ic_certified_map::labeled_hash(
            LABEL_ASSETS,
            &asset_hashes.root_hash(),
        )),
        ic_certified_map::labeled(&LABEL_SIG[..], witness),
    );

    #[derive(Serialize)]
    struct Sig<'a> {
        certificate: ByteBuf,
        tree: HashTree<'a>,
    }

    let sig = Sig {
        certificate: ByteBuf::from(certificate),
        tree,
    };

    let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
    cbor.self_describe().unwrap();
    sig.serialize(&mut cbor).unwrap();
    Some(cbor.into_inner())
}

fn add_signature(sigs: &mut SignatureMap, pk: PublicKey, seed: Hash, expiration: Timestamp) {
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let expires_at = (time() as u64).saturating_add(DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
}

/// Removes a batch of expired signatures from the signature map.
///
/// This function is supposed to piggy back on update calls to
/// amortize the cost of tree pruning.  Each operation on the signature map
/// will prune at most MAX_SIGS_TO_PRUNE other signatures.
fn prune_expired_signatures() {
    const MAX_SIGS_TO_PRUNE: usize = 10;
    let num_pruned =
        state::signature_map_mut(|sigs| sigs.prune_expired(time() as u64, MAX_SIGS_TO_PRUNE));
    if num_pruned > 0 {
        update_root_hash();
    }
}

// Checks if the caller is authenticated against any of the public keys provided
// and traps if not.
fn trap_if_not_authenticated<'a>(public_keys: impl Iterator<Item = &'a PublicKey>) {
    for pk in public_keys {
        if caller() == Principal::self_authenticating(pk) {
            return;
        }
    }
    trap(&format!("{} could not be authenticated.", caller()))
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
fn check_device(device_data: &DeviceData, existing_devices: &[DeviceDataInternal]) {
    check_entry_limits(device_data);

    if device_data.protection == DeviceProtection::Protected
        && device_data.key_type != KeyType::SeedPhrase
    {
        trap(&format!(
            "Only recovery phrases can be protected but key type is {:?}",
            device_data.key_type
        ));
    }

    // if the device is a recovery phrase, check if a different recovery phrase already exists
    if device_data.key_type == KeyType::SeedPhrase
        && existing_devices.iter().any(|existing_device| {
            existing_device.pubkey != device_data.pubkey
                && existing_device.key_type == Some(KeyType::SeedPhrase)
        })
    {
        trap("There is already a recovery phrase and only one is allowed.");
    }
}

fn check_entry_limits(device_data: &DeviceData) {
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

fn check_frontend_length(frontend: &FrontendHostname) {
    const FRONTEND_HOSTNAME_LIMIT: usize = 255;

    let n = frontend.len();
    if frontend.len() > FRONTEND_HOSTNAME_LIMIT {
        trap(&format!(
            "frontend hostname {} exceeds the limit of {} bytes",
            n, FRONTEND_HOSTNAME_LIMIT,
        ));
    }
}

fn main() {}
