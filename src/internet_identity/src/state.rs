use crate::archive::{ArchiveData, ArchiveState, ArchiveStatusCache};
use crate::state::temp_keys::TempKeys;
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::activity_counter::authn_method_counter::AuthnMethodCounter;
use crate::stats::activity_stats::activity_counter::domain_active_anchor_counter::DomainActiveAnchorCounter;
use crate::stats::activity_stats::ActivityStats;
use crate::stats::event_stats::EventKey;
use crate::storage::anchor::Anchor;
use crate::storage::MAX_ENTRIES;
use crate::{random_salt, Storage};
use asset_util::CertifiedAssets;
use candid::{CandidType, Deserialize};
use ic_canister_sig_creation::signature_map::SignatureMap;
use ic_cdk::trap;
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity_interface::internet_identity::types::*;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::time::Duration;

mod temp_keys;

/// Default value for max number of inflight captchas.
pub const DEFAULT_MAX_INFLIGHT_CAPTCHAS: u64 = 500;

/// Default registration rate limit config.
pub const DEFAULT_RATE_LIMIT_CONFIG: RateLimitConfig = RateLimitConfig {
    time_per_token_ns: Duration::from_secs(10).as_nanos() as u64,
    max_tokens: 20_000,
};

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<CertifiedAssets> = RefCell::new(CertifiedAssets::default());
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
    // number of prepare_id_alias calls since last upgrade
    pub prepare_id_alias_counter: u64,
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

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct PersistentState {
    // Information related to the archive
    pub archive_state: ArchiveState,
    // Amount of cycles that need to be attached when II creates a canister
    pub canister_creation_cycles_cost: u64,
    // Configuration for the rate limit on `register`, if any.
    pub registration_rate_limit: RateLimitConfig,
    // Daily and monthly active anchor statistics
    pub active_anchor_stats: ActivityStats<ActiveAnchorCounter>,
    // Daily and monthly active anchor statistics (filtered by domain)
    pub domain_active_anchor_stats: ActivityStats<DomainActiveAnchorCounter>,
    // Daily and monthly active authentication methods on the II domains.
    pub active_authn_method_stats: ActivityStats<AuthnMethodCounter>,
    // Maximum number of inflight captchas
    pub max_inflight_captchas: u64,
    // Count of entries in the event_data BTreeMap
    // event_data is expected to have a lot of entries, thus counting by iterating over it is not
    // an option.
    pub event_data_count: u64,
    // Count of entries in the event_aggregations BTreeMap
    // event_aggregations is expected to have a lot of entries, thus counting by iterating over it is not
    // an option.
    pub event_aggregations_count: u64,
    // Key into the event_data BTreeMap where the 24h tracking window starts.
    // This key is used to remove old entries from the 24h event aggregations.
    // If it is `none`, then the 24h window starts from the newest entry in the event_data
    // BTreeMap minus 24h.
    pub event_stats_24h_start: Option<EventKey>,
}

impl Default for PersistentState {
    fn default() -> Self {
        let time = time();
        Self {
            archive_state: ArchiveState::default(),
            canister_creation_cycles_cost: 0,
            registration_rate_limit: DEFAULT_RATE_LIMIT_CONFIG,
            active_anchor_stats: ActivityStats::new(time),
            domain_active_anchor_stats: ActivityStats::new(time),
            active_authn_method_stats: ActivityStats::new(time),
            max_inflight_captchas: DEFAULT_MAX_INFLIGHT_CAPTCHAS,
            event_data_count: 0,
            event_aggregations_count: 0,
            event_stats_24h_start: None,
        }
    }
}

#[cfg(not(test))]
fn time() -> Timestamp {
    ic_cdk::api::time()
}

/// This is required because [ic_cdk::api::time()] traps when executed in a non-canister environment.
#[cfg(test)]
fn time() -> Timestamp {
    // Return a fixed time for testing
    1709647706487990000 // Tue Mar 05 2024 14:08:26 GMT+0000
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct RateLimitState {
    // Number of tokens available for calls, where each call will deduct one token. If tokens reaches
    // 0 the rate limit will cancel the call.
    pub tokens: u64,
    // Timestamp from which `time_per_token_ns` (see RateLimitConfig) must have passed to
    // increment `tokens`.
    pub token_timestamp: Timestamp,
}

enum StorageState {
    Uninitialised,
    Initialised(Storage<DefaultMemoryImpl>),
}

struct State {
    storage_state: RefCell<StorageState>,
    sigs: RefCell<SignatureMap>,
    // Temporary keys that can be used in lieu of a particular device
    temp_keys: RefCell<TempKeys>,
    last_upgrade_timestamp: Cell<Timestamp>,
    // note: we COULD persist this through upgrades, although this is currently NOT persisted
    // through upgrades
    inflight_challenges: RefCell<HashMap<ChallengeKey, ChallengeInfo>>,
    // tentative device registrations, not persisted across updates
    // if an anchor number is present in this map then registration mode is active until expiration
    tentative_device_registrations: RefCell<HashMap<AnchorNumber, TentativeDeviceRegistration>>,
    // additional usage metrics, NOT persisted across updates (but probably should be in the future)
    usage_metrics: RefCell<UsageMetrics>,
    // State that is temporarily persisted in stable memory during upgrades using
    // pre- and post-upgrade hooks.
    // This must remain small as it is serialized and deserialized on pre- and post-upgrade.
    // Be careful when making changes here, as II needs to be able to update and roll back.
    persistent_state: RefCell<PersistentState>,
    // Cache of the archive status (to make unwanted calls to deploy_archive cheap to dismiss).
    archive_status_cache: RefCell<Option<ArchiveStatusCache>>,
    // Tracking data for the registration rate limit, if any. Not persisted across upgrades.
    registration_rate_limit: RefCell<Option<RateLimitState>>,
    // Counter to ensure uniqueness of event data in case multiple events have the same timestamp
    event_data_uniqueness_counter: Cell<u16>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            storage_state: RefCell::new(StorageState::Uninitialised),
            sigs: RefCell::new(SignatureMap::default()),
            temp_keys: RefCell::new(TempKeys::default()),
            last_upgrade_timestamp: Cell::new(0),
            inflight_challenges: RefCell::new(HashMap::new()),
            tentative_device_registrations: RefCell::new(HashMap::new()),
            usage_metrics: RefCell::new(UsageMetrics::default()),
            persistent_state: RefCell::new(PersistentState::default()),
            archive_status_cache: RefCell::new(None),
            registration_rate_limit: RefCell::new(None),
            event_data_uniqueness_counter: Cell::new(0),
        }
    }
}

// Checks if salt is empty and calls `init_salt` to set it.
pub async fn ensure_salt_set() {
    let salt = storage_borrow(|storage| storage.salt().cloned());
    if salt.is_none() {
        init_salt().await;
    }

    storage_borrow(|storage| {
        if storage.salt().is_none() {
            trap("Salt is not set. Try calling init_salt() to set it");
        }
    });
}

pub async fn init_salt() {
    storage_borrow(|storage| {
        if storage.salt().is_some() {
            trap("Salt already set");
        }
    });

    let salt = random_salt().await;
    storage_borrow_mut(|storage| storage.update_salt(salt)); // update_salt() traps if salt has already been set
}

pub fn salt() -> [u8; 32] {
    storage_borrow(|storage| {
        storage
            .salt()
            .cloned()
            .unwrap_or_else(|| trap("Salt is not set. Try calling init_salt() to set it"))
    })
}

pub fn init_new() {
    let memory = DefaultMemoryImpl::default();
    const FIRST_ANCHOR_NUMBER: AnchorNumber = 10_000;
    let storage = Storage::new(
        (
            FIRST_ANCHOR_NUMBER,
            FIRST_ANCHOR_NUMBER.saturating_add(MAX_ENTRIES),
        ),
        memory,
    );
    storage_replace(storage);
}

pub fn init_from_stable_memory() {
    STATE.with(|s| {
        s.last_upgrade_timestamp.set(time());
    });
    let storage = Storage::from_memory(DefaultMemoryImpl::default());
    storage_replace(storage);
}

pub fn save_persistent_state() {
    STATE.with(|s| {
        storage_borrow_mut(|storage| storage.write_persistent_state(&s.persistent_state.borrow()))
    })
}

pub fn load_persistent_state() {
    STATE.with(|s| {
        let loaded_state = storage_borrow(|storage| storage.read_persistent_state());

        // Reset the event_data and event_aggregations if they are reported as empty from the persistent state
        // Necessary to handle rollback across the versions where the event_data and event_aggregations were introduced
        if loaded_state.event_aggregations_count == 0 {
            storage_borrow_mut(|storage| storage.event_aggregations.clear_new());
        }
        if loaded_state.event_data_count == 0 {
            storage_borrow_mut(|storage| storage.event_data.clear_new());
        }
        *s.persistent_state.borrow_mut() = loaded_state;
    });
}

// helper methods to access / modify the state in a convenient way

pub fn anchor(anchor: AnchorNumber) -> Anchor {
    storage_borrow(|storage| {
        storage.read(anchor).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {anchor}: {err}"
            ))
        })
    })
}

pub fn archive_state() -> ArchiveState {
    STATE.with(|s| s.persistent_state.borrow().archive_state.clone())
}

pub fn archive_data_mut<R>(f: impl FnOnce(&mut ArchiveData) -> R) -> R {
    STATE.with(|s| {
        if let ArchiveState::Created { ref mut data, .. } =
            s.persistent_state.borrow_mut().archive_state
        {
            f(data)
        } else {
            trap("no archive deployed")
        }
    })
}

pub fn tentative_device_registrations<R>(
    f: impl FnOnce(&HashMap<AnchorNumber, TentativeDeviceRegistration>) -> R,
) -> R {
    STATE.with(|s| f(&s.tentative_device_registrations.borrow()))
}

pub fn tentative_device_registrations_mut<R>(
    f: impl FnOnce(&mut HashMap<AnchorNumber, TentativeDeviceRegistration>) -> R,
) -> R {
    STATE.with(|s| f(&mut s.tentative_device_registrations.borrow_mut()))
}

pub fn assets_mut<R>(f: impl FnOnce(&mut CertifiedAssets) -> R) -> R {
    ASSETS.with(|assets| f(&mut assets.borrow_mut()))
}

pub fn assets_and_signatures<R>(f: impl FnOnce(&CertifiedAssets, &SignatureMap) -> R) -> R {
    ASSETS.with(|assets| STATE.with(|s| f(&assets.borrow(), &s.sigs.borrow())))
}

pub fn signature_map<R>(f: impl FnOnce(&SignatureMap) -> R) -> R {
    STATE.with(|s| f(&s.sigs.borrow()))
}

pub fn signature_map_mut<R>(f: impl FnOnce(&mut SignatureMap) -> R) -> R {
    STATE.with(|s| f(&mut s.sigs.borrow_mut()))
}

pub fn storage_borrow<R>(f: impl FnOnce(&Storage<DefaultMemoryImpl>) -> R) -> R {
    STATE.with(|s| match s.storage_state.borrow().deref() {
        StorageState::Uninitialised => trap("Storage not initialized."),
        StorageState::Initialised(storage) => f(storage),
    })
}

pub fn storage_borrow_mut<R>(f: impl FnOnce(&mut Storage<DefaultMemoryImpl>) -> R) -> R {
    STATE.with(|s| match s.storage_state.borrow_mut().deref_mut() {
        StorageState::Uninitialised => trap("Storage not initialized."),
        StorageState::Initialised(ref mut storage) => f(storage),
    })
}

pub fn storage_replace(storage: Storage<DefaultMemoryImpl>) {
    STATE.with(|s| s.storage_state.replace(StorageState::Initialised(storage)));
}

pub fn with_temp_keys_mut<R>(f: impl FnOnce(&mut TempKeys) -> R) -> R {
    STATE.with(|s| f(&mut s.temp_keys.borrow_mut()))
}

pub fn with_temp_keys<R>(f: impl FnOnce(&TempKeys) -> R) -> R {
    STATE.with(|s| f(&mut s.temp_keys.borrow()))
}

pub fn usage_metrics<R>(f: impl FnOnce(&UsageMetrics) -> R) -> R {
    STATE.with(|s| f(&s.usage_metrics.borrow()))
}

pub fn usage_metrics_mut<R>(f: impl FnOnce(&mut UsageMetrics) -> R) -> R {
    STATE.with(|s| f(&mut s.usage_metrics.borrow_mut()))
}

pub fn inflight_challenges<R>(f: impl FnOnce(&HashMap<ChallengeKey, ChallengeInfo>) -> R) -> R {
    STATE.with(|s| f(&s.inflight_challenges.borrow()))
}

pub fn inflight_challenges_mut<R>(
    f: impl FnOnce(&mut HashMap<ChallengeKey, ChallengeInfo>) -> R,
) -> R {
    STATE.with(|s| f(&mut s.inflight_challenges.borrow_mut()))
}

pub fn last_upgrade_timestamp() -> Timestamp {
    STATE.with(|s| s.last_upgrade_timestamp.get())
}

pub fn persistent_state<R>(f: impl FnOnce(&PersistentState) -> R) -> R {
    STATE.with(|s| f(&s.persistent_state.borrow()))
}

pub fn persistent_state_mut<R>(f: impl FnOnce(&mut PersistentState) -> R) -> R {
    STATE.with(|s| f(&mut s.persistent_state.borrow_mut()))
}

pub fn registration_rate_limit<R>(f: impl FnOnce(&Option<RateLimitState>) -> R) -> R {
    STATE.with(|s| f(&s.registration_rate_limit.borrow()))
}

pub fn registration_rate_limit_mut<R>(f: impl FnOnce(&mut Option<RateLimitState>) -> R) -> R {
    STATE.with(|s| f(&mut s.registration_rate_limit.borrow_mut()))
}

pub fn cached_archive_status() -> Option<ArchiveStatusCache> {
    STATE.with(|s| match *s.archive_status_cache.borrow() {
        None => None,
        Some(ref cached_status) => {
            // cache is outdated
            if time() - cached_status.timestamp > Duration::from_secs(60 * 60).as_nanos() as u64 {
                return None;
            }
            Some(cached_status.clone())
        }
    })
}

pub fn cache_archive_status(archive_status: ArchiveStatusCache) {
    STATE.with(|state| {
        *state.archive_status_cache.borrow_mut() = Some(archive_status);
    })
}

pub fn invalidate_archive_status_cache() {
    STATE.with(|state| {
        *state.archive_status_cache.borrow_mut() = None;
    })
}

pub fn get_and_inc_event_data_counter() -> u16 {
    STATE.with(|s| {
        let counter = s.event_data_uniqueness_counter.get();
        s.event_data_uniqueness_counter.set(counter.wrapping_add(1));
        counter
    })
}
