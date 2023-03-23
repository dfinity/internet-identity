use crate::active_anchor_stats::new_active_anchor_counter;
use crate::DAY_NS;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::ActiveAnchorStatistics;

/// Updates the active anchor counters if an ongoing collection bucket has completed.
pub fn process_stats(stats: &mut ActiveAnchorStatistics) {
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
