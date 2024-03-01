use crate::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::activity_stats::activity_counter::authn_method_counter::AuthnMethodCounter;
use crate::activity_stats::activity_counter::domain_active_anchor_counter::DomainActiveAnchorCounter;
use crate::activity_stats::ActivityStats;
use crate::archive::ArchiveState;
use crate::state::{
    PersistentState, DEFAULT_MAX_INFLIGHT_CAPTCHAS, DEFAULT_MAX_NUM_DELEGATION_ORIGINS,
    DEFAULT_RATE_LIMIT_CONFIG,
};
use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{
    FrontendHostname, RateLimitConfig, Timestamp,
};
use std::borrow::Cow;
use std::collections::HashMap;
use std::time::Duration;

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct StorablePersistentState {
    archive_state: ArchiveState,
    canister_creation_cycles_cost: u64,
    registration_rate_limit: RateLimitConfig,
    active_anchor_stats: ActivityStats<ActiveAnchorCounter>,
    domain_active_anchor_stats: ActivityStats<DomainActiveAnchorCounter>,
    active_authn_method_stats: ActivityStats<AuthnMethodCounter>,
    latest_delegation_origins: HashMap<FrontendHostname, Timestamp>,
    max_num_latest_delegation_origins: u64,
    max_inflight_captchas: u64,
}

impl Storable for StorablePersistentState {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to serialize persistent state"))
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to deserialize persistent state")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl Default for StorablePersistentState {
    fn default() -> Self {
        let time = time();
        Self {
            archive_state: ArchiveState::default(),
            canister_creation_cycles_cost: 0,
            registration_rate_limit: RateLimitConfig {
                time_per_token_ns: Duration::from_secs(10).as_nanos() as u64,
                max_tokens: 20_000,
            },
            active_anchor_stats: ActivityStats::new(time),
            domain_active_anchor_stats: ActivityStats::new(time),
            active_authn_method_stats: ActivityStats::new(time),
            latest_delegation_origins: HashMap::new(),
            max_num_latest_delegation_origins: DEFAULT_MAX_NUM_DELEGATION_ORIGINS,
            max_inflight_captchas: DEFAULT_MAX_INFLIGHT_CAPTCHAS,
        }
    }
}

impl From<PersistentState> for StorablePersistentState {
    fn from(persistent_state: PersistentState) -> Self {
        let time = time();
        Self {
            archive_state: persistent_state.archive_state,
            canister_creation_cycles_cost: persistent_state.canister_creation_cycles_cost,
            registration_rate_limit: persistent_state
                .registration_rate_limit
                .unwrap_or(DEFAULT_RATE_LIMIT_CONFIG),
            active_anchor_stats: persistent_state
                .active_anchor_stats
                .unwrap_or(ActivityStats::new(time)),
            domain_active_anchor_stats: persistent_state
                .domain_active_anchor_stats
                .unwrap_or(ActivityStats::new(time)),
            active_authn_method_stats: persistent_state
                .active_authn_method_stats
                .unwrap_or(ActivityStats::new(time)),
            latest_delegation_origins: persistent_state
                .latest_delegation_origins
                .unwrap_or_default(),
            max_num_latest_delegation_origins: persistent_state
                .max_num_latest_delegation_origins
                .unwrap_or(DEFAULT_MAX_NUM_DELEGATION_ORIGINS),
            max_inflight_captchas: persistent_state
                .max_inflight_captchas
                .unwrap_or(DEFAULT_MAX_INFLIGHT_CAPTCHAS),
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
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64
}

impl From<StorablePersistentState> for PersistentState {
    fn from(storable_persistent_state: StorablePersistentState) -> Self {
        Self {
            archive_state: storable_persistent_state.archive_state,
            canister_creation_cycles_cost: storable_persistent_state.canister_creation_cycles_cost,
            registration_rate_limit: Some(storable_persistent_state.registration_rate_limit),
            active_anchor_stats: Some(storable_persistent_state.active_anchor_stats),
            domain_active_anchor_stats: Some(storable_persistent_state.domain_active_anchor_stats),
            active_authn_method_stats: Some(storable_persistent_state.active_authn_method_stats),
            latest_delegation_origins: Some(storable_persistent_state.latest_delegation_origins),
            max_num_latest_delegation_origins: Some(
                storable_persistent_state.max_num_latest_delegation_origins,
            ),
            max_inflight_captchas: Some(storable_persistent_state.max_inflight_captchas),
        }
    }
}
