use crate::state;
use crate::storage::anchor::{Anchor, DomainActivity};
use crate::{IC0_APP_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
use ic_cdk::api::time;
use internet_identity_interface::{
    ActiveAnchorCounter, ActiveAnchorStatistics, ActivityCounter, CompletedActiveAnchorStats,
    DomainActiveAnchorCounter, OngoingActiveAnchorStats, Timestamp,
};

mod stats_maintenance;

pub fn update_active_anchors_stats(anchor: &Anchor, current_domain: &Option<IIDomain>) {
    let previous_activity_timestamp = anchor.last_activity();

    state::persistent_state_mut(|persistent_state| {
        update_activity_stats(&mut persistent_state.active_anchor_stats, |counter| {
            update_active_anchor_counter(counter, previous_activity_timestamp)
        });

        if let Some(domain) = current_domain {
            update_activity_stats(
                &mut persistent_state.domain_active_anchor_stats,
                |counter| update_ii_domain_counter(counter, anchor, domain),
            )
        }
    })
}

fn update_activity_stats<T: ActivityCounter>(
    stats: &mut Option<ActiveAnchorStatistics<T>>,
    update: impl Fn(&mut T),
) {
    match stats {
        None => {
            let mut new_stats = new_active_anchor_statistics(time());
            update_counters(&mut new_stats, update);
            *stats = Some(new_stats)
        }
        Some(ref mut stats) => {
            stats_maintenance::process_stats(stats);
            update_counters(stats, update);
        }
    }
}

fn update_counters<T: ActivityCounter>(
    stats: &mut ActiveAnchorStatistics<T>,
    update: impl Fn(&mut T),
) {
    update(&mut stats.ongoing.daily_active_anchors);
    stats
        .ongoing
        .monthly_active_anchors
        .iter_mut()
        .for_each(update);
}

/// Increases the counter on a window if the `previous_activity_timestamp` lies before
/// the `start_timestamp` (i.e. a window where the anchor has not yet had any activity in).
///
/// For any given anchor and window the `previous_activity_timestamp` is only absent or before the
/// `window.start_timestamp` once because:
/// * `previous_activity_timestamp` of the given anchor is set to `ic_cdk::time()` in this canister call
/// * `window.start_timestamp` is <= `ic_cdk::time()` for any active window
fn update_active_anchor_counter(
    counter: &mut ActiveAnchorCounter,
    previous_activity_timestamp: Option<Timestamp>,
) {
    if let Some(timestamp) = previous_activity_timestamp {
        if counter.start_timestamp > timestamp {
            counter.counter += 1;
        }
    } else {
        // increase counter if there is no previous activity
        counter.counter += 1;
    }
}

#[derive(Eq, PartialEq)]
pub enum IIDomain {
    Ic0AppDomain,
    InternetComputerOrgDomain,
}

impl IIDomain {
    pub fn is_same_domain(&self, activity: &DomainActivity) -> bool {
        match self {
            IIDomain::Ic0AppDomain => matches!(activity, DomainActivity::Ic0App),
            IIDomain::InternetComputerOrgDomain => {
                matches!(activity, DomainActivity::InternetComputerOrg)
            }
        }
    }

    pub fn other_domain(&self) -> IIDomain {
        match self {
            IIDomain::Ic0AppDomain => IIDomain::InternetComputerOrgDomain,
            IIDomain::InternetComputerOrgDomain => IIDomain::Ic0AppDomain,
        }
    }
}

impl TryFrom<&str> for IIDomain {
    type Error = ();

    fn try_from(origin: &str) -> Result<Self, Self::Error> {
        match origin {
            IC0_APP_ORIGIN => Ok(IIDomain::Ic0AppDomain),
            INTERNETCOMPUTER_ORG_ORIGIN => Ok(IIDomain::InternetComputerOrgDomain),
            _ => Err(()),
        }
    }
}

/// Increases the counter on a window for a single II domain if the there was no activity before.
///
/// If there has been activity on an II domain and this is activity on the _other_ domain, then
/// we decrement the counter for the previous single domain and instead increment the counter for
/// both II domains.
///
/// If the anchor was already active on both domains, the activity has been counted in this window
/// already and no action needs to be taken.
///
/// Only called if `current_domain` corresponds to an II domain.
fn update_ii_domain_counter(
    counter: &mut DomainActiveAnchorCounter,
    anchor: &Anchor,
    current_domain: &IIDomain,
) {
    let previous_domain_activity = anchor.domain_activity_since(counter.start_timestamp);

    match previous_domain_activity {
        DomainActivity::None | DomainActivity::Other => {
            increment_counter_for_domain(counter, current_domain)
        }
        DomainActivity::Ic0App | DomainActivity::InternetComputerOrg => {
            if !current_domain.is_same_domain(&previous_domain_activity) {
                // the anchor switched from being active on only one II domain to being active on both
                // --> total active remains the same, but the anchor switches to the both domains bucket
                decrement_counter_for_domain(counter, &current_domain.other_domain());
                counter.both_ii_domains_counter += 1;
            }
        }
        DomainActivity::BothIIDomains => {
            // already counted => do nothing
        }
    }
}

fn increment_counter_for_domain(counter: &mut DomainActiveAnchorCounter, domain: &IIDomain) {
    match domain {
        IIDomain::Ic0AppDomain => counter.ic0_app_counter += 1,
        IIDomain::InternetComputerOrgDomain => counter.internetcomputer_org_counter += 1,
    }
}

fn decrement_counter_for_domain(counter: &mut DomainActiveAnchorCounter, domain: &IIDomain) {
    match domain {
        IIDomain::Ic0AppDomain => counter.ic0_app_counter -= 1,
        IIDomain::InternetComputerOrgDomain => counter.internetcomputer_org_counter -= 1,
    }
}

fn new_active_anchor_statistics<T: ActivityCounter>(time: Timestamp) -> ActiveAnchorStatistics<T> {
    ActiveAnchorStatistics {
        completed: CompletedActiveAnchorStats {
            daily_active_anchors: None,
            monthly_active_anchors: None,
        },
        ongoing: OngoingActiveAnchorStats {
            daily_active_anchors: ActivityCounter::new(time),
            monthly_active_anchors: vec![ActivityCounter::new(time)],
        },
    }
}
