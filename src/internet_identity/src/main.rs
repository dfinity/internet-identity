use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::convert::TryInto;

#[cfg(not(feature = "dummy_captcha"))]
use captcha::filters::Wave;
use ic_cdk::api::call::call;
use ic_cdk::api::{caller, data_certificate, id, set_certified_data, time, trap};
use ic_cdk::export::candid::{CandidType, Deserialize, Principal};
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_certified_map::{AsHashTree, Hash, HashTree, RbTree};
use rand_chacha::rand_core::{RngCore, SeedableRng};
use serde::Serialize;
use serde_bytes::ByteBuf;
use AddTentativeDeviceResponse::DeviceRegistrationModeOff;

use crate::AddTentativeDeviceResponse::AnotherDeviceTentativelyAdded;
use assets::ContentType;
use internet_identity::nonce_cache::NonceCache;
use internet_identity::signature_map::SignatureMap;
use storage::{Salt, Storage};
use TentativeDeviceRegistrationState::DeviceTentativelyAdded;
use VerifyTentativeDeviceResponse::WrongCode;

use crate::assets::init_assets;
use crate::http::{HeaderField, HttpRequest, HttpResponse};
use crate::TentativeDeviceRegistrationState::DeviceRegistrationModeActive;

mod assets;
mod http;

const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}

// 30 mins
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 60);
// 8 days
const MAX_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(8 * 24 * 60 * 60);
// 1 min
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(60);
// 5 mins
const CAPTCHA_CHALLENGE_LIFETIME: u64 = secs_to_nanos(300);
// 15 mins
const REGISTRATION_MODE_DURATION: u64 = secs_to_nanos(900);

// How many captcha challenges we keep in memory (at most)
const MAX_INFLIGHT_CHALLENGES: usize = 500;
// How many users can be in registration mode simultaneously
const MAX_USERS_IN_REGISTRATION_MODE: usize = 10_000;
// How many verification attempts are given for a tentative device
const MAX_DEVICE_REGISTRATION_ATTEMPTS: u8 = 3;

const LABEL_ASSETS: &[u8] = b"http_assets";
const LABEL_SIG: &[u8] = b"sig";

type UserNumber = u64;
type CredentialId = ByteBuf;
type PublicKey = ByteBuf;
type DeviceKey = PublicKey;
type UserKey = PublicKey;
type SessionKey = PublicKey;
type FrontendHostname = String;
type Timestamp = u64; // in nanos since epoch
type Signature = ByteBuf;
type DeviceVerificationCode = String;
type FailedAttemptsCounter = u8;

struct Base64(String);

#[derive(Clone, Debug, CandidType, Deserialize)]
enum Purpose {
    #[serde(rename = "recovery")]
    Recovery,
    #[serde(rename = "authentication")]
    Authentication,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum KeyType {
    #[serde(rename = "unknown")]
    Unknown,
    #[serde(rename = "platform")]
    Platform,
    #[serde(rename = "cross_platform")]
    CrossPlatform,
    #[serde(rename = "seed_phrase")]
    SeedPhrase,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct DeviceData {
    pubkey: DeviceKey,
    alias: String,
    credential_id: Option<CredentialId>,
    purpose: Purpose,
    key_type: KeyType,
}

/// This is an internal version of `DeviceData` primarily useful to provide a
/// backwards compatible level between older device data stored in stable memory
/// (that might not contain purpose or key_type) and new ones added.
#[derive(Clone, Debug, CandidType, Deserialize)]
struct DeviceDataInternal {
    pubkey: DeviceKey,
    alias: String,
    credential_id: Option<CredentialId>,
    purpose: Option<Purpose>,
    key_type: Option<KeyType>,
}

impl From<DeviceData> for DeviceDataInternal {
    fn from(device_data: DeviceData) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            purpose: Some(device_data.purpose),
            key_type: Some(device_data.key_type),
        }
    }
}

impl From<DeviceDataInternal> for DeviceData {
    fn from(device_data_internal: DeviceDataInternal) -> Self {
        Self {
            pubkey: device_data_internal.pubkey,
            alias: device_data_internal.alias,
            credential_id: device_data_internal.credential_id,
            purpose: device_data_internal
                .purpose
                .unwrap_or(Purpose::Authentication),
            key_type: device_data_internal.key_type.unwrap_or(KeyType::Unknown),
        }
    }
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct Delegation {
    pubkey: PublicKey,
    expiration: Timestamp,
    targets: Option<Vec<Principal>>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct SignedDelegation {
    delegation: Delegation,
    signature: Signature,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum GetDelegationResponse {
    #[serde(rename = "signed_delegation")]
    SignedDelegation(SignedDelegation),
    #[serde(rename = "no_such_delegation")]
    NoSuchDelegation,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum RegisterResponse {
    #[serde(rename = "registered")]
    Registered { user_number: UserNumber },
    #[serde(rename = "canister_full")]
    CanisterFull,
    #[serde(rename = "bad_challenge")]
    BadChallenge,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum AddTentativeDeviceResponse {
    #[serde(rename = "added_tentatively")]
    AddedTentatively {
        verification_code: DeviceVerificationCode,
        device_registration_timeout: Timestamp,
    },
    #[serde(rename = "device_registration_mode_off")]
    DeviceRegistrationModeOff,
    #[serde(rename = "another_device_tentatively_added")]
    AnotherDeviceTentativelyAdded,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum VerifyTentativeDeviceResponse {
    #[serde(rename = "verified")]
    Verified,
    #[serde(rename = "wrong_code")]
    WrongCode { retries_left: u8 },
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct IdentityAnchorInfo {
    devices: Vec<DeviceData>,
    tentative_device: Option<DeviceData>,
    device_registration_mode_expiration: Option<Timestamp>,
}

mod hash;
mod storage;

#[derive(Clone, Debug, CandidType, Deserialize)]
struct InternetIdentityStats {
    assigned_user_number_range: (UserNumber, UserNumber),
    users_registered: u64,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct InternetIdentityInit {
    assigned_user_number_range: (UserNumber, UserNumber),
}

type AssetHashes = RbTree<&'static str, Hash>;

enum TentativeDeviceRegistrationState {
    DeviceRegistrationModeActive {
        expiration: Timestamp,
    },
    DeviceTentativelyAdded {
        expiration: Timestamp,
        tentative_device: DeviceData,
        verification_code: DeviceVerificationCode,
        failed_attempts: FailedAttemptsCounter,
    },
}

struct State {
    storage: RefCell<Storage<Vec<DeviceDataInternal>>>,
    sigs: RefCell<SignatureMap>,
    asset_hashes: RefCell<AssetHashes>,
    last_upgrade_timestamp: Cell<Timestamp>,
    // note: we COULD persist this through upgrades, although this is currently NOT persisted
    // through upgrades
    inflight_challenges: RefCell<HashMap<ChallengeKey, ChallengeInfo>>,
    // tentative device registrations, not persisted across updates
    // if a user number is present in this map then registration mode is active until expiration
    tentative_device_registrations: RefCell<HashMap<UserNumber, TentativeDeviceRegistrationState>>,
}

impl Default for State {
    fn default() -> Self {
        const FIRST_USER_ID: UserNumber = 10_000;
        Self {
            storage: RefCell::new(Storage::new((
                FIRST_USER_ID,
                FIRST_USER_ID.saturating_add(storage::DEFAULT_RANGE_SIZE),
            ))),
            sigs: RefCell::new(SignatureMap::default()),
            asset_hashes: RefCell::new(AssetHashes::default()),
            last_upgrade_timestamp: Cell::new(0),
            inflight_challenges: RefCell::new(HashMap::new()),
            tentative_device_registrations: RefCell::new(HashMap::new()),
        }
    }
}

// The challenges we store and check against
struct ChallengeInfo {
    created: Timestamp,
    chars: String,
}

type ChallengeKey = String;

// The user's attempt
#[derive(Clone, Debug, CandidType, Deserialize)]
struct ChallengeAttempt {
    chars: String,
    key: ChallengeKey,
}

// What we send the user
#[derive(Clone, Debug, CandidType, Deserialize)]
struct Challenge {
    png_base64: String,
    challenge_key: ChallengeKey,
}

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<HashMap<&'static str, (Vec<HeaderField>, &'static [u8])>> = RefCell::new(HashMap::default());
}

#[update]
async fn init_salt() {
    STATE.with(|s| {
        if s.storage.borrow().salt().is_some() {
            trap("Salt already set");
        }
    });

    let res: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get salt: {}", err)),
    };
    let salt: Salt = res[..].try_into().unwrap_or_else(|_| {
        trap(&format!(
            "expected raw randomness to be of length 32, got {}",
            res.len()
        ));
    });

    STATE.with(|s| {
        let mut store = s.storage.borrow_mut();
        store.update_salt(salt); // update_salt() traps if salt has already been set
    });
}

#[update]
fn enter_device_registration_mode(user_number: UserNumber) -> Timestamp {
    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });
        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        prune_expired_tentative_device_registrations(state);

        if state.tentative_device_registrations.borrow().len() >= MAX_USERS_IN_REGISTRATION_MODE {
            trap("too many users in device registration mode");
        }

        let mut device_registration_state = state.tentative_device_registrations.borrow_mut();
        match device_registration_state.get(&user_number) {
            Some(
                DeviceTentativelyAdded { expiration, .. }
                | DeviceRegistrationModeActive { expiration },
            ) => *expiration, // already enabled, just return the existing expiration
            None => {
                let expiration = time() + REGISTRATION_MODE_DURATION;
                device_registration_state
                    .insert(user_number, DeviceRegistrationModeActive { expiration });
                expiration
            }
        }
    })
}

#[update]
fn exit_device_registration_mode(user_number: UserNumber) {
    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });
        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        prune_expired_tentative_device_registrations(state);

        state
            .tentative_device_registrations
            .borrow_mut()
            .remove(&user_number);
    })
}

#[update]
async fn add_tentative_device(
    user_number: UserNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    let verification_code = generate_device_verification_code().await;

    match check_tentative_device_registration_prerequisites(user_number) {
        Ok(device_registration_timeout) => {
            STATE.with(|state| {
                state.tentative_device_registrations.borrow_mut().insert(
                    user_number,
                    DeviceTentativelyAdded {
                        expiration: device_registration_timeout,
                        tentative_device: device_data,
                        verification_code: verification_code.clone(),
                        failed_attempts: 0,
                    },
                )
            });
            AddTentativeDeviceResponse::AddedTentatively {
                verification_code,
                device_registration_timeout,
            }
        }
        Err(err) => err,
    }
}

#[update]
async fn verify_tentative_device(
    user_number: UserNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    match check_tentative_device_code_counting_failed_attempts(user_number, user_verification_code) {
        Ok(device) => {
            add(user_number, device).await;
            VerifyTentativeDeviceResponse::Verified
        }
        Err(err) => err,
    }
}

/// Checks the device verification pin for a tentative device.
/// If valid, returns the device to be added and exits device registration mode
/// If invalid, returns the appropriate error to send to the client and increases failed attempts. Exits device registration mode if there are no retries left.
fn check_tentative_device_code_counting_failed_attempts(
    user_number: UserNumber,
    user_verification_code: DeviceVerificationCode,
) -> Result<DeviceData, VerifyTentativeDeviceResponse> {
    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });
        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        prune_expired_tentative_device_registrations(state);

        let mut device_registration_state = state.tentative_device_registrations.borrow_mut();
        match device_registration_state.remove(&user_number) {
            None | Some(DeviceRegistrationModeActive { .. }) => Err(WrongCode { retries_left: 0 }),
            Some(DeviceTentativelyAdded {
                expiration,
                tentative_device,
                verification_code,
                mut failed_attempts,
            }) => {
                if user_verification_code == verification_code {
                    return Ok(tentative_device);
                }

                failed_attempts = failed_attempts + 1;
                if failed_attempts < MAX_DEVICE_REGISTRATION_ATTEMPTS {
                    // reinsert because retries are allowed
                    device_registration_state.insert(
                        user_number,
                        DeviceTentativelyAdded {
                            expiration,
                            tentative_device,
                            verification_code,
                            failed_attempts,
                        },
                    );
                }
                return Err(WrongCode {
                    retries_left: (MAX_DEVICE_REGISTRATION_ATTEMPTS - failed_attempts) as u8,
                });
            }
        }
    })
}

/// Return a decimal representation of a random `u32` to be used as verification code
async fn generate_device_verification_code() -> DeviceVerificationCode {
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

fn check_tentative_device_registration_prerequisites(
    user_number: UserNumber,
) -> Result<Timestamp, AddTentativeDeviceResponse> {
    STATE.with(|state| {
        prune_expired_tentative_device_registrations(state);

        match state
            .tentative_device_registrations
            .borrow()
            .get(&user_number)
        {
            None => return Err(DeviceRegistrationModeOff),
            Some(DeviceTentativelyAdded { .. }) => Err(AnotherDeviceTentativelyAdded),
            Some(DeviceRegistrationModeActive { expiration }) => Ok(*expiration),
        }
    })
}

fn prune_expired_tentative_device_registrations(state: &State) {
    state
        .tentative_device_registrations
        .borrow_mut()
        .retain(|_, value| match value {
            DeviceRegistrationModeActive { expiration }
            | DeviceTentativelyAdded { expiration, .. } => *expiration > time(),
        });
}

#[update]
async fn register(device_data: DeviceData, challenge_result: ChallengeAttempt) -> RegisterResponse {
    if let Err(()) = check_challenge(challenge_result) {
        return RegisterResponse::BadChallenge;
    }

    check_entry_limits(&device_data);

    if caller() != Principal::self_authenticating(device_data.pubkey.clone()) {
        ic_cdk::trap(&format!(
            "{} could not be authenticated against {:?}",
            caller(),
            device_data.pubkey
        ));
    }

    ensure_salt_set().await;

    STATE.with(|s| {
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());

        let mut store = s.storage.borrow_mut();
        match store.allocate_user_number() {
            Some(user_number) => {
                store
                    .write(user_number, vec![DeviceDataInternal::from(device_data)])
                    .unwrap_or_else(|err| {
                        trap(&format!("failed to store user device data: {}", err))
                    });
                RegisterResponse::Registered { user_number }
            }
            None => RegisterResponse::CanisterFull,
        }
    })
}

#[update]
async fn add(user_number: UserNumber, device_data: DeviceData) {
    const MAX_ENTRIES_PER_USER: usize = 10;

    check_entry_limits(&device_data);

    ensure_salt_set().await;

    STATE.with(|s| {
        let mut entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        for e in entries.iter_mut() {
            if e.pubkey == device_data.pubkey {
                trap("Device already added.");
            }
        }

        if entries.len() >= MAX_ENTRIES_PER_USER {
            trap(&format!(
                "at most {} authentication information entries are allowed per user",
                MAX_ENTRIES_PER_USER,
            ));
        }

        entries.push(DeviceDataInternal::from(device_data));
        s.storage
            .borrow()
            .write(user_number, entries)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to write device data of user {}: {}",
                    user_number, err
                ))
            });

        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());
    })
}

#[update]
async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    ensure_salt_set().await;
    STATE.with(|s| {
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());

        let mut entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        if let Some(i) = entries.iter().position(|e| e.pubkey == device_key) {
            entries.swap_remove(i as usize);
        }

        s.storage
            .borrow()
            .write(user_number, entries)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to persist device data of user {}: {}",
                    user_number, err
                ))
            });
    })
}

#[update]
async fn create_challenge() -> Challenge {
    let mut rng = make_rng().await;

    let resp = STATE.with(|s| {
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());

        let mut inflight_challenges = s.inflight_challenges.borrow_mut();

        let now = time() as u64;

        // Prune old challenges. This drops all challenges that are older than
        // CAPTCHA_CHALLENGE_LIFETIME
        // TODO: test this
        inflight_challenges.retain(|_, v| v.created > now - CAPTCHA_CHALLENGE_LIFETIME);

        // Error out if there are too many inflight challenges
        // TODO: test this
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
    });

    resp
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
    STATE.with(|s| {
        let mut inflight_challenges = s.inflight_challenges.borrow_mut();
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

#[query]
fn lookup(user_number: UserNumber) -> Vec<DeviceData> {
    STATE.with(|s| {
        s.storage
            .borrow()
            .read(user_number)
            .unwrap_or_default()
            .into_iter()
            .map(DeviceData::from)
            .collect()
    })
}

#[update] // this is an update call because queries are not (yet) certified
fn get_anchor_info(user_number: UserNumber) -> IdentityAnchorInfo {
    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });
        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        let devices = entries
            .into_iter()
            .map(DeviceData::from)
            .collect();

        let now = time();
        match state
            .tentative_device_registrations
            .borrow()
            .get(&user_number)
        {
            Some(DeviceTentativelyAdded {
                expiration,
                tentative_device,
                ..
            }) if *expiration > now => IdentityAnchorInfo {
                devices,
                tentative_device: Some(tentative_device.clone()),
                device_registration_mode_expiration: Some(*expiration),
            },
            Some(DeviceRegistrationModeActive { expiration }) if *expiration > now => {
                IdentityAnchorInfo {
                    devices,
                    tentative_device: None,
                    device_registration_mode_expiration: Some(*expiration),
                }
            }
            None | Some(_) => IdentityAnchorInfo {
                devices,
                tentative_device: None,
                device_registration_mode_expiration: None,
            },
        }
    })
}

#[query]
fn get_principal(user_number: UserNumber, frontend: FrontendHostname) -> Principal {
    check_frontend_length(&frontend);

    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        let seed = calculate_seed(user_number, &frontend);
        let public_key = der_encode_canister_sig_key(seed.to_vec());
        Principal::self_authenticating(&public_key)
    })
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
    ensure_salt_set().await;

    STATE.with(|s| {
        let entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        check_frontend_length(&frontend);

        let delta = u64::min(
            max_time_to_live.unwrap_or(DEFAULT_EXPIRATION_PERIOD_NS),
            MAX_EXPIRATION_PERIOD_NS,
        );
        let expiration = (time() as u64).saturating_add(delta);

        let seed = calculate_seed(user_number, &frontend);
        let mut sigs = s.sigs.borrow_mut();
        add_signature(&mut sigs, session_key, seed, expiration);
        update_root_hash(&s.asset_hashes.borrow(), &sigs);
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut sigs);

        (
            ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
            expiration,
        )
    })
}

#[query]
fn get_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);

    STATE.with(|state| {
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        match get_signature(
            &state.asset_hashes.borrow(),
            &state.sigs.borrow(),
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
    STATE.with(|state| {
        let storage = state.storage.borrow();
        InternetIdentityStats {
            assigned_user_number_range: storage.assigned_user_number_range(),
            users_registered: storage.user_count() as u64,
        }
    })
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    STATE.with(|state| {
        if let Some(arg) = maybe_arg {
            state
                .storage
                .replace(Storage::new(arg.assigned_user_number_range));
        }
        state.storage.borrow().flush();
        update_root_hash(&state.asset_hashes.borrow(), &state.sigs.borrow());
    });
}

#[post_upgrade]
fn retrieve_data() {
    init_assets();
    STATE.with(|s| {
        s.last_upgrade_timestamp.set(time() as u64);
        match Storage::from_stable_memory() {
            Some(mut storage) => {
                let (lo, hi) = storage.assigned_user_number_range();
                let max_entries = storage.max_entries() as u64;
                if (hi - lo) != max_entries {
                    // This code might be executed for 2 reasons:
                    //
                    // 1. We used to specify a nonsensical limit of 8B entries
                    //    by default.  We couldn't store more than 2M entries
                    //    in a single canister at that point, so we needed to
                    //    lower the upper limit on upgrade.
                    //
                    // 2. After stable memory limits were increased, we could
                    //    affort storing more entries by using the 64 bit
                    //    stable memory API.  So we needed to increase the
                    //    upper limit on upgrade.
                    storage.set_user_number_range((lo, lo.saturating_add(max_entries)));
                }
                s.storage.replace(storage);
            }
            None => {
                s.storage.borrow().flush();
            }
        }

        // We drop all the signatures on upgrade, users will
        // re-request them if needed.
        update_root_hash(&s.asset_hashes.borrow(), &s.sigs.borrow());
    });
}

fn calculate_seed(user_number: UserNumber, frontend: &FrontendHostname) -> Hash {
    let salt = STATE
        .with(|s| s.storage.borrow().salt().cloned())
        .unwrap_or_else(|| trap("Salt is not set. Try calling init_salt() to set it"));

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

fn update_root_hash(a: &AssetHashes, m: &SignatureMap) {
    use ic_certified_map::{fork_hash, labeled_hash};

    let prefixed_root_hash = fork_hash(
        // NB: Labels added in lexicographic order
        &labeled_hash(LABEL_ASSETS, &a.root_hash()),
        &labeled_hash(LABEL_SIG, &m.root_hash()),
    );
    set_certified_data(&prefixed_root_hash[..]);
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
fn prune_expired_signatures(asset_hashes: &AssetHashes, sigs: &mut SignatureMap) {
    const MAX_SIGS_TO_PRUNE: usize = 10;
    let num_pruned = sigs.prune_expired(time() as u64, MAX_SIGS_TO_PRUNE);

    if num_pruned > 0 {
        update_root_hash(asset_hashes, sigs);
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
    ic_cdk::trap(&format!("{} could not be authenticated.", caller()))
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

// Checks if salt is empty and calls `init_salt` to set it.
async fn ensure_salt_set() {
    let salt = STATE.with(|s| s.storage.borrow().salt().cloned());
    if salt.is_none() {
        init_salt().await;
    }

    STATE.with(|s| {
        if s.storage.borrow().salt().is_none() {
            trap("Salt is not set. Try calling init_salt() to set it");
        }
    });
}

fn main() {}
