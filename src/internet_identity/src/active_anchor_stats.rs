use crate::state;
use ic_cdk::api::time;
use internet_identity_interface::{
    ActiveAnchorCounter, ActiveAnchorStatistics, CompletedActiveAnchorStats,
    OngoingActiveAnchorStats, Timestamp,
};

mod stats_maintenance;

pub fn update_active_anchors_stats(previous_activity_timestamp: Option<Timestamp>) {
    state::persistent_state_mut(
        |persistent_state| match persistent_state.active_anchor_stats {
            None => {
                let mut stats = new_active_anchor_statistics(time());
                increment_stats_on_activity(&mut stats, previous_activity_timestamp);
                persistent_state.active_anchor_stats = Some(stats)
            }
            Some(ref mut stats) => {
                stats_maintenance::process_active_anchor_stats(stats);
                increment_stats_on_activity(stats, previous_activity_timestamp);
            }
        },
    )
}

/// Increases the counter on all windows, where the `previous_activity_timestamp` (if any) lies before
/// their respective `start_timestamp` (i.e. all windows where the anchor has not yet had any activity in).
///
/// For any given anchor and window the `previous_activity_timestamp` is only absent or before the
/// `window.start_timestamp` once because:
/// * `previous_activity_timestamp` of the given anchor is set to `ic_cdk::time()` in this call
/// * `window.start_timestamp` is <= `ic_cdk::time()` for any active window
fn increment_stats_on_activity(
    stats: &mut ActiveAnchorStatistics,
    previous_activity_timestamp: Option<Timestamp>,
) {
    // increase all counters and exit early if there is no previous activity
    let Some(timestamp) = previous_activity_timestamp else {
        stats.ongoing.daily_active_anchors.counter += 1;
        stats.ongoing.monthly_active_anchors.iter_mut().for_each(|monthly_stats| monthly_stats.counter +=1 );
        return;
    };

    if stats.ongoing.daily_active_anchors.start_timestamp > timestamp {
        stats.ongoing.daily_active_anchors.counter += 1;
    }

    stats
        .ongoing
        .monthly_active_anchors
        .iter_mut()
        .for_each(|monthly_stats| {
            if monthly_stats.start_timestamp > timestamp {
                monthly_stats.counter += 1;
            }
        });
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
