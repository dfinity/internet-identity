use crate::archive::ArchiveState;
use crate::state::{PersistentState, DEFAULT_CAPTCHA_CONFIG};
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::activity_counter::authn_method_counter::AuthnMethodCounter;
use crate::stats::activity_stats::activity_counter::domain_active_anchor_counter::DomainActiveAnchorCounter;
use crate::stats::activity_stats::ActivityStats;
use crate::stats::event_stats::EventKey;
use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{
    AnalyticsConfig, CaptchaConfig, DummyAuthConfig, FrontendHostname, OpenIdConfig,
    RateLimitConfig, Timestamp,
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
    // unused, kept for stable memory compatibility
    latest_delegation_origins: HashMap<FrontendHostname, Timestamp>,
    // unused, kept for stable memory compatibility
    max_num_latest_delegation_origins: u64,
    // unused, kept for stable memory compatibility
    max_inflight_captchas: u64,

    // opt fields because of backwards compatibility
    event_stats_24h_start: Option<EventKey>,
    captcha_config: Option<CaptchaConfig>,
    related_origins: Option<Vec<String>>,
    new_flow_origins: Option<Vec<String>>,
    openid_google: Option<OpenIdConfig>,
    analytics_config: Option<AnalyticsConfig>,
    fetch_root_key: Option<bool>,
    enable_dapps_explorer: Option<bool>,
    is_production: Option<bool>,
    dummy_auth: Option<DummyAuthConfig>,
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
            // unused, kept for stable memory compatibility
            latest_delegation_origins: Default::default(),
            // unused, kept for stable memory compatibility
            max_num_latest_delegation_origins: 0,
            // unused, kept for stable memory compatibility
            max_inflight_captchas: 0,
            event_stats_24h_start: s.event_stats_24h_start,
            captcha_config: Some(s.captcha_config),
            related_origins: s.related_origins,
            new_flow_origins: s.new_flow_origins,
            openid_google: s.openid_google,
            analytics_config: s.analytics_config,
            fetch_root_key: s.fetch_root_key,
            enable_dapps_explorer: s.enable_dapps_explorer,
            is_production: s.is_production,
            dummy_auth: s.dummy_auth,
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
            captcha_config: s.captcha_config.unwrap_or(DEFAULT_CAPTCHA_CONFIG),
            related_origins: s.related_origins,
            new_flow_origins: s.new_flow_origins,
            openid_google: s.openid_google,
            analytics_config: s.analytics_config,
            event_stats_24h_start: s.event_stats_24h_start,
            fetch_root_key: s.fetch_root_key,
            enable_dapps_explorer: s.enable_dapps_explorer,
            is_production: s.is_production,
            dummy_auth: s.dummy_auth,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::{
        CaptchaTrigger, StaticCaptchaTrigger,
    };
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
            max_num_latest_delegation_origins: 0,
            max_inflight_captchas: 0,
            event_stats_24h_start: None,
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaEnabled),
            }),
            related_origins: None,
            new_flow_origins: None,
            openid_google: None,
            analytics_config: None,
            fetch_root_key: None,
            enable_dapps_explorer: None,
            is_production: None,
            dummy_auth: None,
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
            captcha_config: CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaEnabled),
            },
            related_origins: None,
            new_flow_origins: None,
            openid_google: None,
            event_stats_24h_start: None,
            analytics_config: None,
            fetch_root_key: None,
            enable_dapps_explorer: None,
            is_production: None,
            dummy_auth: None,
        };
        assert_eq!(PersistentState::default(), expected_defaults);
    }
}
