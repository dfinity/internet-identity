use crate::ii_domain::IIDomain;
use crate::stats::activity_stats::activity_counter::ActivityCounter;
use crate::storage::anchor::{Anchor, DomainActivity};
use candid::{CandidType, Deserialize};
use internet_identity_interface::internet_identity::types::Timestamp;

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct DomainActiveAnchorCounter {
    pub start_timestamp: Timestamp,
    pub ic0_app_counter: u64,
    pub internetcomputer_org_counter: u64,
    pub both_ii_domains_counter: u64,
}

pub struct DomainActivityContext<'a> {
    pub anchor: &'a Anchor,
    pub current_domain: IIDomain,
}

impl DomainActiveAnchorCounter {
    fn increment_counter_for_domain(&mut self, domain: &IIDomain) {
        match domain {
            IIDomain::Ic0App => self.ic0_app_counter += 1,
            IIDomain::InternetComputerOrg => self.internetcomputer_org_counter += 1,
        }
    }

    fn decrement_counter_for_domain(&mut self, domain: &IIDomain) {
        match domain {
            IIDomain::Ic0App => self.ic0_app_counter -= 1,
            IIDomain::InternetComputerOrg => self.internetcomputer_org_counter -= 1,
        }
    }
}

impl ActivityCounter for DomainActiveAnchorCounter {
    type CountingContext<'a> = DomainActivityContext<'a>;

    fn new(start_timestamp: Timestamp) -> Self {
        Self {
            start_timestamp,
            ic0_app_counter: 0,
            internetcomputer_org_counter: 0,
            both_ii_domains_counter: 0,
        }
    }

    fn start_timestamp(&self) -> Timestamp {
        self.start_timestamp
    }

    /// Increases the counter on a counter for a single II domain if the there was no activity before.
    ///
    /// If there has been activity on an II domain and this is activity on the _other II_ domain, then
    /// we decrement the counter for the previous single domain and instead increment the counter for
    /// both II domains.
    ///
    /// If the anchor was already active on both domains, the activity has been counted in this counter
    /// already and no action needs to be taken.
    ///
    /// Only called if `current_domain` corresponds to an II domain.
    fn count_event(&mut self, context: &Self::CountingContext<'_>) {
        let previous_domain_activity = context.anchor.domain_activity_since(self.start_timestamp);

        match previous_domain_activity {
            DomainActivity::None | DomainActivity::NonIIDomain => {
                self.increment_counter_for_domain(&context.current_domain);
            }
            DomainActivity::Ic0App | DomainActivity::InternetComputerOrg => {
                if !context
                    .current_domain
                    .is_same_domain(&previous_domain_activity)
                {
                    // the anchor switched from being active on only one II domain to being active on both
                    // --> total active remains the same, but the anchor switches to the both domains bucket
                    self.decrement_counter_for_domain(&context.current_domain.other_ii_domain());
                    self.both_ii_domains_counter += 1;
                }
            }
            DomainActivity::BothIIDomains => {
                // already counted => do nothing
            }
        }
    }
}
