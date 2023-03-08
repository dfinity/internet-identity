use crate::{state, DAY_NS};
use ic_cdk::api::time;
use internet_identity_interface::{
    ActiveAnchorCounter, ActiveAnchorStatistics, CompletedActiveAnchorStats,
    OngoingActiveAnchorStats, Timestamp,
};

pub fn update_active_anchors_stats(previous_activity_timestamp: Option<Timestamp>) {
    state::persistent_state_mut(
        |persistent_state| match persistent_state.active_anchor_stats {
            None => {
                let mut stats = new_active_anchor_statistics(time());
                increment_stats_on_activity(&mut stats, previous_activity_timestamp);
                persistent_state.active_anchor_stats = Some(stats)
            }
            Some(ref mut stats) => {
                process_active_anchor_stats(stats);
                increment_stats_on_activity(stats, previous_activity_timestamp);
            }
        },
    )
}

/// Updates the active anchor counters if an ongoing collection bucket has completed.
fn process_active_anchor_stats(stats: &mut ActiveAnchorStatistics) {
    process_daily_stats(stats);
    process_monthly_stats(stats);
}

/// If the ongoing daily active users bucket was started 24h ago (or earlier), it replaces the current
/// completed daily active user counter and a new ongoing 24h counter is created
/// (starting from the end of the now completed counter).
#[allow(clippy::identity_op)]
fn process_daily_stats(stats: &mut ActiveAnchorStatistics) {
    let now = time();
    if stats.ongoing.daily_active_anchors.start_timestamp + 1 * DAY_NS <= now {
        // there might have been no activity for more than 24h, so the new window will skip the
        // period with no activity and start a new counter for the currently active 24h window
        let offset = (now - stats.ongoing.daily_active_anchors.start_timestamp) % DAY_NS;
        let new_start_timestamp = now - offset;

        if stats.ongoing.daily_active_anchors.start_timestamp + 2 * DAY_NS <= now {
            // there was no activity for more than 24h since the end of the completed window so the
            // last ongoing window is already outdated
            // -> create an empty counter for the last completed window
            stats.completed.daily_active_anchors =
                Some(new_active_anchor_counter(new_start_timestamp - 1 * DAY_NS));
        } else {
            stats.completed.daily_active_anchors = Some(stats.ongoing.daily_active_anchors.clone());
        }

        stats.ongoing.daily_active_anchors = new_active_anchor_counter(new_start_timestamp)
    }
}

/// Monthly active anchor counters are processed as follows:
/// * buckets are removed from the ongoing collection vector if they are completed
/// * the completed monthly counter is replaced by the most recently completed 30-day collection bucket
///   or an empty one if the last completed bucket is already outdated
/// * a new monthly ongoing collection period is added if the most recent one was started more than
///   24h ago
#[allow(clippy::identity_op)]
fn process_monthly_stats(stats: &mut ActiveAnchorStatistics) {
    let now = time();
    // Remove all completed 30-day time windows from the ongoing collection vector
    while let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.first() {
        if monthly_stats.start_timestamp + 30 * DAY_NS <= now {
            let counter = stats.ongoing.monthly_active_anchors.remove(0);
            stats.completed.monthly_active_anchors = Some(counter);
        } else {
            break;
        }
    }

    if let Some(ref monthly_stats) = stats.completed.monthly_active_anchors {
        // there was no activity for more than 24h since the end of the completed window so the
        // last ongoing window is already outdated
        // -> create an empty counter for the last completed window
        if monthly_stats.start_timestamp + 31 * DAY_NS <= now {
            // align empty completed 30-day window to the 24h collection interval
            let offset = (now - monthly_stats.start_timestamp) % DAY_NS;
            stats.completed.monthly_active_anchors =
                Some(new_active_anchor_counter(now - 30 * DAY_NS - offset));
        }
    }

    // Align new window to the currently running 24h interval
    let start_timestamp = stats.ongoing.daily_active_anchors.start_timestamp;
    // Note: requires daily stats to be processed before the monthly stats
    assert!(now - start_timestamp < DAY_NS);

    if let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.last() {
        if monthly_stats.start_timestamp + 1 * DAY_NS <= now {
            // Start a new 30-day time window if the last one starts more than 24h in the past.
            // This will result in at most 30 ongoing collection windows:
            // * at most one window is added every 24h
            // * windows that started more than 30 days ago are removed (see above)
            // => within a 30-day period there can be at most 30 active windows offset by 24h each.
            //
            // Example using 3 day collection periods:
            // Each character in <-> indicates a 24h window
            // <->
            //  <->
            //   <->
            //    <-> <- this window has no overlap with the first window anymore
            //           -> for any time t there are at most 3 ongoing collection windows
            stats
                .ongoing
                .monthly_active_anchors
                .push(new_active_anchor_counter(start_timestamp));
        }
    } else {
        // there was no activity for so long that there is no ongoing 30-day collection window anymore
        // -> start collecting again (in sync with the daily active anchor stats)
        // Note: requires daily stats to be processed before the monthly stats
        stats
            .ongoing
            .monthly_active_anchors
            .push(new_active_anchor_counter(start_timestamp));
    }
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
