use crate::archive::{ArchiveData, ArchiveInfo, ArchiveState, ArchiveStatusCache};
use crate::storage::DEFAULT_RANGE_SIZE_V1;
use crate::{PersistentStateError, Salt, Storage};
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api::management_canister::main::CanisterStatusResponse;
use ic_cdk::api::time;
use ic_cdk::{call, trap};
use ic_certified_map::{Hash, RbTree};
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity::signature_map::SignatureMap;
use internet_identity_interface::*;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::time::Duration;

pub type Assets = HashMap<&'static str, (Vec<HeaderField>, &'static [u8])>;
pub type AssetHashes = RbTree<&'static str, Hash>;

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<Assets> = RefCell::new(HashMap::default());
}

/// Internal representation of the anchor.
/// After the upcoming stable memory migration, new fields can be added to this struct.
#[derive(Clone, Debug, Default, CandidType, Deserialize, Eq, PartialEq)]
pub struct Anchor {
    pub devices: Vec<DeviceDataInternal>,
}

/// This is an internal version of `DeviceData` primarily useful to provide a
/// backwards compatible level between older device data stored in stable memory
/// (that might not contain purpose or key_type) and new ones added.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DeviceDataInternal {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub purpose: Option<Purpose>,
    pub key_type: Option<KeyType>,
    pub protection: Option<DeviceProtection>,
}

impl From<DeviceData> for DeviceDataInternal {
    fn from(device_data: DeviceData) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            purpose: Some(device_data.purpose),
            key_type: Some(device_data.key_type),
            protection: Some(device_data.protection),
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
            protection: device_data_internal
                .protection
                .unwrap_or(DeviceProtection::Unprotected),
        }
    }
}

impl From<DeviceDataInternal> for DeviceDataWithoutAlias {
    fn from(device_data: DeviceDataInternal) -> Self {
        Self {
            pubkey: device_data.pubkey,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose.unwrap_or(Purpose::Authentication),
            key_type: device_data.key_type.unwrap_or(KeyType::Unknown),
            protection: device_data
                .protection
                .unwrap_or(DeviceProtection::Unprotected),
        }
    }
}

pub struct TentativeDeviceRegistration {
    pub expiration: Timestamp,
    pub state: RegistrationState,
}

/// Registration state of new devices added using the two step device add flow
pub enum RegistrationState {
    DeviceRegistrationModeActive,
    DeviceTentativelyAdded {
        tentative_device: DeviceData,
        verification_code: DeviceVerificationCode,
        failed_attempts: FailedAttemptsCounter,
    },
}

#[derive(Default)]
pub struct UsageMetrics {
    // number of prepare_delegation calls since last upgrade
    pub delegation_counter: u64,
    // number of anchor operations (register, add, remove, update) since last upgrade
    pub anchor_operation_counter: u64,
}

// The challenges we store and check against
pub struct ChallengeInfo {
    pub created: Timestamp,
    pub chars: String,
}

pub type ChallengeKey = String;

// The user's attempt
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ChallengeAttempt {
    pub chars: String,
    pub key: ChallengeKey,
}

// What we send the user
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Challenge {
    pub png_base64: String,
    pub challenge_key: ChallengeKey,
}

#[derive(Clone, Default, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct PersistentState {
    // Information related to the archive
    pub archive_info: ArchiveInfo,
    // Amount of cycles that need to be attached when II creates a canister
    pub canister_creation_cycles_cost: u64,
}

struct State {
    storage: RefCell<Storage<DefaultMemoryImpl>>,
    sigs: RefCell<SignatureMap>,
    asset_hashes: RefCell<AssetHashes>,
    last_upgrade_timestamp: Cell<Timestamp>,
    // note: we COULD persist this through upgrades, although this is currently NOT persisted
    // through upgrades
    inflight_challenges: RefCell<HashMap<ChallengeKey, ChallengeInfo>>,
    // tentative device registrations, not persisted across updates
    // if a user number is present in this map then registration mode is active until expiration
    tentative_device_registrations: RefCell<HashMap<UserNumber, TentativeDeviceRegistration>>,
    // additional usage metrics, NOT persisted across updates (but probably should be in the future)
    usage_metrics: RefCell<UsageMetrics>,
    // State that is temporarily persisted in stable memory during upgrades using
    // pre- and post-upgrade hooks.
    // This must remain small as it is serialized and deserialized on pre- and post-upgrade.
    // Be careful when making changes here, as II needs to be able to update and roll back.
    persistent_state: RefCell<PersistentState>,
    // Cache of the archive status (to make unwanted calls to deploy_archive cheap to dismiss).
    archive_status_cache: RefCell<Option<ArchiveStatusCache>>,
}

impl Default for State {
    fn default() -> Self {
        const FIRST_USER_ID: UserNumber = 10_000;
        Self {
            storage: RefCell::new(Storage::new(
                (
                    FIRST_USER_ID,
                    FIRST_USER_ID.saturating_add(DEFAULT_RANGE_SIZE_V1),
                ),
                DefaultMemoryImpl::default(),
            )),
            sigs: RefCell::new(SignatureMap::default()),
            asset_hashes: RefCell::new(AssetHashes::default()),
            last_upgrade_timestamp: Cell::new(0),
            inflight_challenges: RefCell::new(HashMap::new()),
            tentative_device_registrations: RefCell::new(HashMap::new()),
            usage_metrics: RefCell::new(UsageMetrics::default()),
            persistent_state: RefCell::new(PersistentState::default()),
            archive_status_cache: RefCell::new(None),
        }
    }
}

// Checks if salt is empty and calls `init_salt` to set it.
pub async fn ensure_salt_set() {
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

pub async fn init_salt() {
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

pub fn salt() -> [u8; 32] {
    STATE
        .with(|s| s.storage.borrow().salt().cloned())
        .unwrap_or_else(|| trap("Salt is not set. Try calling init_salt() to set it"))
}

pub fn initialize_from_stable_memory() {
    STATE.with(|s| {
        s.last_upgrade_timestamp.set(time() as u64);
        match Storage::from_memory(DefaultMemoryImpl::default()) {
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
                    //    afford storing more entries by using the 64 bit
                    //    stable memory API.  So we needed to increase the
                    //    upper limit on upgrade.
                    storage.set_user_number_range((lo, lo.saturating_add(max_entries)));
                }
                s.storage.replace(storage);
            }
            None => {
                s.storage.borrow_mut().flush();
            }
        }
    });
}

pub fn save_persistent_state() {
    STATE.with(|s| {
        s.storage
            .borrow_mut()
            .write_persistent_state(&s.persistent_state.borrow());
    })
}

pub fn load_persistent_state() {
    STATE.with(|s| {
        let storage = s.storage.borrow();
        match storage.read_persistent_state() {
            Ok(loaded_state) => *s.persistent_state.borrow_mut() = loaded_state,
            Err(PersistentStateError::NotFound) => {
                if storage.version() >= 2 {
                    trap("unable to load persistent state: not found")
                }
                *s.persistent_state.borrow_mut() = PersistentState::default()
            }
            Err(err) => trap(&format!(
                "failed to recover persistent state! Err: {:?}",
                err
            )),
        }
    })
}

// helper methods to access / modify the state in a convenient way

pub fn anchor(anchor: UserNumber) -> Anchor {
    STATE.with(|s| {
        s.storage.borrow().read(anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                anchor, err
            ))
        })
    })
}

pub fn archive_state() -> ArchiveState {
    STATE.with(|s| s.persistent_state.borrow().archive_info.state.clone())
}

pub fn expected_archive_hash() -> Option<[u8; 32]> {
    STATE.with(|s| {
        s.persistent_state
            .borrow()
            .archive_info
            .expected_module_hash
            .clone()
    })
}

pub fn archive_data() -> Option<ArchiveData> {
    STATE.with(|s| {
        if let ArchiveState::Created(ref data) = s.persistent_state.borrow().archive_info.state {
            Some(data.clone())
        } else {
            None
        }
    })
}

pub fn increment_archive_seq_nr() {
    STATE.with(|s| {
        if let ArchiveState::Created(ref mut data) =
            s.persistent_state.borrow_mut().archive_info.state
        {
            data.sequence_number += 1;
        } else {
            trap("no archive deployed")
        }
    })
}

pub fn tentative_device_registrations<R>(
    f: impl FnOnce(&HashMap<UserNumber, TentativeDeviceRegistration>) -> R,
) -> R {
    STATE.with(|s| f(&*s.tentative_device_registrations.borrow()))
}

pub fn tentative_device_registrations_mut<R>(
    f: impl FnOnce(&mut HashMap<UserNumber, TentativeDeviceRegistration>) -> R,
) -> R {
    STATE.with(|s| f(&mut *s.tentative_device_registrations.borrow_mut()))
}

pub fn assets<R>(f: impl FnOnce(&Assets) -> R) -> R {
    ASSETS.with(|assets| f(&*assets.borrow()))
}

pub fn assets_and_hashes_mut<R>(f: impl FnOnce(&mut Assets, &mut AssetHashes) -> R) -> R {
    ASSETS.with(|assets| {
        STATE.with(|s| f(&mut *assets.borrow_mut(), &mut *s.asset_hashes.borrow_mut()))
    })
}

pub fn asset_hashes_and_sigs<R>(f: impl FnOnce(&AssetHashes, &SignatureMap) -> R) -> R {
    STATE.with(|s| f(&*s.asset_hashes.borrow(), &*s.sigs.borrow()))
}

pub fn signature_map<R>(f: impl FnOnce(&SignatureMap) -> R) -> R {
    STATE.with(|s| f(&*s.sigs.borrow()))
}

pub fn signature_map_mut<R>(f: impl FnOnce(&mut SignatureMap) -> R) -> R {
    STATE.with(|s| f(&mut *s.sigs.borrow_mut()))
}

pub fn storage<R>(f: impl FnOnce(&Storage<DefaultMemoryImpl>) -> R) -> R {
    STATE.with(|s| f(&*s.storage.borrow()))
}

pub fn storage_mut<R>(f: impl FnOnce(&mut Storage<DefaultMemoryImpl>) -> R) -> R {
    STATE.with(|s| f(&mut *s.storage.borrow_mut()))
}

pub fn usage_metrics<R>(f: impl FnOnce(&UsageMetrics) -> R) -> R {
    STATE.with(|s| f(&*s.usage_metrics.borrow()))
}

pub fn usage_metrics_mut<R>(f: impl FnOnce(&mut UsageMetrics) -> R) -> R {
    STATE.with(|s| f(&mut *s.usage_metrics.borrow_mut()))
}

pub fn inflight_challenges<R>(f: impl FnOnce(&HashMap<ChallengeKey, ChallengeInfo>) -> R) -> R {
    STATE.with(|s| f(&*s.inflight_challenges.borrow()))
}

pub fn inflight_challenges_mut<R>(
    f: impl FnOnce(&mut HashMap<ChallengeKey, ChallengeInfo>) -> R,
) -> R {
    STATE.with(|s| f(&mut *s.inflight_challenges.borrow_mut()))
}

pub fn last_upgrade_timestamp() -> Timestamp {
    STATE.with(|s| s.last_upgrade_timestamp.get())
}

pub fn persistent_state<R>(f: impl FnOnce(&PersistentState) -> R) -> R {
    STATE.with(|s| f(&*s.persistent_state.borrow()))
}

pub fn persistent_state_mut<R>(f: impl FnOnce(&mut PersistentState) -> R) -> R {
    STATE.with(|s| f(&mut *s.persistent_state.borrow_mut()))
}

pub fn cached_archive_status() -> Option<CanisterStatusResponse> {
    STATE.with(|s| match *s.archive_status_cache.borrow() {
        None => None,
        Some(ref cached_status) => {
            // cache is outdated
            if time() - cached_status.timestamp > Duration::from_secs(60 * 60).as_nanos() as u64 {
                return None;
            }
            Some(cached_status.status.clone())
        }
    })
}

pub fn cache_archive_status(archive_status: ArchiveStatusCache) {
    STATE.with(|state| {
        *state.archive_status_cache.borrow_mut() = Some(archive_status);
    })
}
