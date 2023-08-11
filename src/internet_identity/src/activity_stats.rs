use crate::activity_stats::activity_counter::active_anchor_counter::update_active_anchor_counter;
use crate::activity_stats::activity_counter::domain_active_anchor_counter::update_ii_domain_counter;
use crate::activity_stats::activity_counter::ActivityCounter;
use crate::ii_domain::IIDomain;
use crate::state;
use crate::storage::anchor::Anchor;
use candid::{CandidType, Deserialize};
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::Timestamp;

pub mod activity_counter;
mod stats_maintenance;

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActiveAnchorStatistics<T> {
    // Stats for the last completed collection period for daily and monthly active anchors
    pub completed: CompletedActiveAnchorStats<T>,
    // ongoing periods for daily and monthly active anchors
    pub ongoing: OngoingActiveAnchorStats<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct CompletedActiveAnchorStats<T> {
    pub daily_active_anchors: Option<T>,
    pub monthly_active_anchors: Option<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct OngoingActiveAnchorStats<T> {
    // Ongoing active anchor counter for the current 24 h time bucket.
    pub daily_active_anchors: T,
    // Monthly active users are collected using 30-day sliding windows.
    // This vec contains up to 30 30-day active windows each offset by one day.
    // The vec is sorted, new collection windows are added at the end.
    pub monthly_active_anchors: Vec<T>,
}

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
