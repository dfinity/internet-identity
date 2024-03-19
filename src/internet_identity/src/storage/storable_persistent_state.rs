use crate::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::activity_stats::activity_counter::authn_method_counter::AuthnMethodCounter;
use crate::activity_stats::activity_counter::domain_active_anchor_counter::DomainActiveAnchorCounter;
use crate::activity_stats::ActivityStats;
use crate::archive::ArchiveState;
use crate::state::PersistentState;
use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{
    FrontendHostname, RateLimitConfig, Timestamp,
};
use std::borrow::Cow;
use std::collections::HashMap;

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
        Self::from(PersistentState::default())
    }
}

impl From<PersistentState> for StorablePersistentState {
    fn from(s: PersistentState) -> Self {
        Self {
            archive_state: s.archive_state,
            canister_creation_cycles_cost: s.canister_creation_cycles_cost,
            registration_rate_limit: s.registration_rate_limit,
            active_anchor_stats: s.active_anchor_stats,
            domain_active_anchor_stats: s.domain_active_anchor_stats,
            active_authn_method_stats: s.active_authn_method_stats,
            latest_delegation_origins: s.latest_delegation_origins,
            max_num_latest_delegation_origins: s.max_num_latest_delegation_origins,
            max_inflight_captchas: s.max_inflight_captchas,
        }
    }
}

impl From<StorablePersistentState> for PersistentState {
    fn from(s: StorablePersistentState) -> Self {
        Self {
            archive_state: s.archive_state,
            canister_creation_cycles_cost: s.canister_creation_cycles_cost,
            registration_rate_limit: s.registration_rate_limit,
            active_anchor_stats: s.active_anchor_stats,
            domain_active_anchor_stats: s.domain_active_anchor_stats,
            active_authn_method_stats: s.active_authn_method_stats,
            latest_delegation_origins: s.latest_delegation_origins,
            max_num_latest_delegation_origins: s.max_num_latest_delegation_origins,
            max_inflight_captchas: s.max_inflight_captchas,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::{DEFAULT_MAX_DELEGATION_ORIGINS, DEFAULT_MAX_INFLIGHT_CAPTCHAS};
    use std::time::Duration;

    #[test]
    fn should_convert_storable_persistent_state_to_persistent_state() {
        let storable_persistent_state = StorablePersistentState::default();
        let persistent_state = PersistentState::from(storable_persistent_state.clone());
        assert_eq!(
            storable_persistent_state,
            StorablePersistentState::from(persistent_state)
        );
    }

    #[test]
    fn should_have_expected_default_values() {
        let test_time = 1709647706487990000u64;
        let expected_defaults = StorablePersistentState {
            archive_state: ArchiveState::default(),
            canister_creation_cycles_cost: 0,
            registration_rate_limit: RateLimitConfig {
                time_per_token_ns: Duration::from_secs(10).as_nanos() as u64,
                max_tokens: 20_000,
            },
            active_anchor_stats: ActivityStats::new(test_time),
            domain_active_anchor_stats: ActivityStats::new(test_time),
            active_authn_method_stats: ActivityStats::new(test_time),
            latest_delegation_origins: HashMap::new(),
            max_num_latest_delegation_origins: DEFAULT_MAX_DELEGATION_ORIGINS,
            max_inflight_captchas: DEFAULT_MAX_INFLIGHT_CAPTCHAS,
        };

        assert_eq!(StorablePersistentState::default(), expected_defaults);

        let expected_defaults = PersistentState {
            archive_state: ArchiveState::default(),
            canister_creation_cycles_cost: 0,
            registration_rate_limit: RateLimitConfig {
                time_per_token_ns: Duration::from_secs(10).as_nanos() as u64,
                max_tokens: 20_000,
            },
            active_anchor_stats: ActivityStats::new(test_time),
            domain_active_anchor_stats: ActivityStats::new(test_time),
            active_authn_method_stats: ActivityStats::new(test_time),
            latest_delegation_origins: HashMap::new(),
            max_num_latest_delegation_origins: DEFAULT_MAX_DELEGATION_ORIGINS,
            max_inflight_captchas: DEFAULT_MAX_INFLIGHT_CAPTCHAS,
        };
        assert_eq!(PersistentState::default(), expected_defaults);
    }
}
