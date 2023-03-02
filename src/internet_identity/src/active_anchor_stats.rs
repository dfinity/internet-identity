use crate::{state, DAY};
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
                rotate_active_anchor_stats_if_necessary(stats);
                increment_stats_on_activity(stats, previous_activity_timestamp);
            }
        },
    )
}

/// This method updates the active anchor counters if an ongoing collection bucket has completed.
///
/// For the daily (24h) active user counter:
/// If the ongoing daily active users bucket was started 24h ago (or earlier), it replaces the current
/// completed daily active user counter and a new ongoing 24h counter is created
/// (starting from the end of the now completed counter).
///
/// For the monthly (30 day) active user counters:
/// if the oldest ongoing 30 day bucket was started 30 days ago (or earlier), it replaces the current
/// completed monthly active user counter and a new ongoing 30 day counter is created
/// (starting from the end of the now completed counter).
#[allow(clippy::identity_op)]
fn rotate_active_anchor_stats_if_necessary(stats: &mut ActiveAnchorStatistics) {
    let now = time();
    if stats.ongoing.daily_active_anchors.start_timestamp + 1 * DAY <= now {
        let new_start_timestamp = stats.ongoing.daily_active_anchors.start_timestamp + 1 * DAY;
        stats.completed.daily_active_anchors = Some(stats.ongoing.daily_active_anchors.clone());
        stats.ongoing.daily_active_anchors = new_active_anchor_counter(new_start_timestamp)
    }

    if let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.first() {
        if monthly_stats.start_timestamp + 30 * DAY <= now {
            let counter = stats.ongoing.monthly_active_anchors.remove(0);
            let new_start_timestamp = counter.start_timestamp + 30 * DAY;
            stats.completed.monthly_active_anchors = Some(counter);
            stats
                .ongoing
                .monthly_active_anchors
                .push(new_active_anchor_counter(new_start_timestamp));
        }
    }
    if let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.last() {
        // start a new 30 day time window if the last one starts more than 24h in the past.
        if monthly_stats.start_timestamp + 1 * DAY <= now {
            stats
                .ongoing
                .monthly_active_anchors
                .push(new_active_anchor_counter(
                    monthly_stats.start_timestamp + 1 * DAY,
                ));
        }
    }
}

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
