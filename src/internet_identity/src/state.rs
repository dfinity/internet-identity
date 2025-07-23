use crate::anchor_management::tentative_device_registration::ValidatedRegistrationId;
use crate::archive::{ArchiveData, ArchiveState, ArchiveStatusCache};
use crate::state::flow_states::FlowStates;
use crate::state::temp_keys::TempKeys;
use crate::state::RegistrationState::DeviceTentativelyAdded;
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

pub mod flow_states;
mod temp_keys;

/// Default captcha config
pub const DEFAULT_CAPTCHA_CONFIG: CaptchaConfig = CaptchaConfig {
    max_unsolved_captchas: 500,
    captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaEnabled),
};

/// Default registration rate limit config.
pub const DEFAULT_RATE_LIMIT_CONFIG: RateLimitConfig = RateLimitConfig {
    time_per_token_ns: Duration::from_secs(10).as_nanos() as u64,
    max_tokens: 20_000,
};

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<CertifiedAssets> = RefCell::new(CertifiedAssets::default());
}

#[derive(Clone)]
pub struct TentativeDeviceRegistration {
    pub expiration: Timestamp,
    pub state: RegistrationState,
    pub id: Option<ValidatedRegistrationId>,
}

impl TentativeDeviceRegistration {
    pub fn to_info_if_still_valid(&self, now: Timestamp) -> Option<DeviceRegistrationInfo> {
        match self {
            TentativeDeviceRegistration {
                expiration,
                state:
                    DeviceTentativelyAdded {
                        tentative_device, ..
                    },
                ..
            } if *expiration > now => Some(DeviceRegistrationInfo {
                expiration: *expiration,
                tentative_device: Some(tentative_device.clone()),
            }),
            TentativeDeviceRegistration { expiration, .. } if *expiration > now => {
                Some(DeviceRegistrationInfo {
                    expiration: *expiration,
                    tentative_device: None,
                })
            }
            _ => None,
        }
    }
}

/// Registration state of new devices added using the two step device add flow
#[derive(Clone)]
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
    // Configuration of the captcha challenge during registration flow
    pub captcha_config: CaptchaConfig,
    // Configuration for Related Origins Requests
    pub related_origins: Option<Vec<String>>,
    // Configuration for New Flow Origins
    pub new_flow_origins: Option<Vec<String>>,
    // Configuration for OpenID Google client
    pub openid_google: Option<OpenIdConfig>,
    // Configuration for Web Analytics tool
    pub analytics_config: Option<AnalyticsConfig>,
    // Key into the event_data BTreeMap where the 24h tracking window starts.
    // This key is used to remove old entries from the 24h event aggregations.
    // If it is `none`, then the 24h window starts from the newest entry in the event_data
    // BTreeMap minus 24h.
    pub event_stats_24h_start: Option<EventKey>,
    pub fetch_root_key: Option<bool>,
    pub enable_dapps_explorer: Option<bool>,
    pub is_production: Option<bool>,
    pub dummy_auth: Option<DummyAuthConfig>,
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
            captcha_config: DEFAULT_CAPTCHA_CONFIG,
            related_origins: None,
            new_flow_origins: None,
            openid_google: None,
            analytics_config: None,
            event_stats_24h_start: None,
            fetch_root_key: None,
            enable_dapps_explorer: None,
            is_production: None,
            dummy_auth: None,
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

#[allow(clippy::large_enum_variant)]
#[derive(Default)]
enum StorageState {
    #[default]
    Uninitialised,
    Initialised(Storage<DefaultMemoryImpl>),
}

#[derive(Default)]
struct State {
    storage_state: RefCell<StorageState>,
    sigs: RefCell<SignatureMap>,
    // Temporary keys that can be used in lieu of a particular device
    temp_keys: RefCell<TempKeys>,
    flow_states: RefCell<FlowStates>,
    last_upgrade_timestamp: Cell<Timestamp>,
    // note: we COULD persist this through upgrades, although this is currently NOT persisted
    // through upgrades
    inflight_challenges: RefCell<HashMap<ChallengeKey, ChallengeInfo>>,
    // tentative device registrations, not persisted across updates
    // if an anchor number is present in this map then registration mode is active until expiration
    tentative_device_registrations: RefCell<HashMap<AnchorNumber, TentativeDeviceRegistration>>,
    // lookup table so we can easily return a user's active registrations in identity_info
    lookup_tentative_device_registration: RefCell<HashMap<ValidatedRegistrationId, AnchorNumber>>,
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
        *s.persistent_state.borrow_mut() =
            storage_borrow(|storage| storage.read_persistent_state());
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

pub fn lookup_tentative_device_registration<R>(
    f: impl FnOnce(&HashMap<ValidatedRegistrationId, AnchorNumber>) -> R,
) -> R {
    STATE.with(|s| f(&s.lookup_tentative_device_registration.borrow()))
}

pub fn lookup_tentative_device_registration_mut<R>(
    f: impl FnOnce(&mut HashMap<ValidatedRegistrationId, AnchorNumber>) -> R,
) -> R {
    STATE.with(|s| f(&mut s.lookup_tentative_device_registration.borrow_mut()))
}

pub fn get_tentative_device_registration_by_identity(
    identity_number: IdentityNumber,
) -> Option<TentativeDeviceRegistration> {
    tentative_device_registrations(|registrations| registrations.get(&identity_number).cloned())
}

pub fn get_identity_number_by_registration_id(
    id: &ValidatedRegistrationId,
) -> Option<IdentityNumber> {
    lookup_tentative_device_registration(|lookup| lookup.get(id).copied()).and_then(
        |identity_number| {
            let TentativeDeviceRegistration { expiration, .. } =
                get_tentative_device_registration_by_identity(identity_number)?;
            if expiration > time() {
                Some(identity_number)
            } else {
                None
            }
        },
    )
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

pub fn with_flow_states_mut<R>(f: impl FnOnce(&mut FlowStates) -> R) -> R {
    STATE.with(|s| f(&mut s.flow_states.borrow_mut()))
}

pub fn with_flow_states<R>(f: impl FnOnce(&FlowStates) -> R) -> R {
    STATE.with(|s| f(&mut s.flow_states.borrow()))
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
