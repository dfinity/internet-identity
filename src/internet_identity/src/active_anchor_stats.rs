use crate::state;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::{
    ActiveAnchorCounter, ActiveAnchorStatistics, CompletedActiveAnchorStats,
    OngoingActiveAnchorStats, Timestamp,
};

mod stats_maintenance;

pub fn update_active_anchors_stats(previous_activity_timestamp: Option<Timestamp>) {
    state::persistent_state_mut(|persistent_state| {
        update_activity_stats(&mut persistent_state.active_anchor_stats, |counter| {
            update_active_anchor_counter(counter, previous_activity_timestamp)
        })
    })
}

fn update_activity_stats(
    stats: &mut Option<ActiveAnchorStatistics>,
    update: impl Fn(&mut ActiveAnchorCounter),
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

fn update_counters(stats: &mut ActiveAnchorStatistics, update: impl Fn(&mut ActiveAnchorCounter)) {
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

fn new_active_anchor_statistics(time: Timestamp) -> ActiveAnchorStatistics {
    ActiveAnchorStatistics {
        completed: CompletedActiveAnchorStats {
            daily_active_anchors: None,
            monthly_active_anchors: None,
        },
        ongoing: OngoingActiveAnchorStats {
            daily_active_anchors: new_active_anchor_counter(time),
            monthly_active_anchors: vec![new_active_anchor_counter(time)],
        },
    }
}

fn new_active_anchor_counter(time: Timestamp) -> ActiveAnchorCounter {
    ActiveAnchorCounter {
        start_timestamp: time,
        counter: 0,
    }
}
