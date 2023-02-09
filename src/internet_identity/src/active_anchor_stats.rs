use crate::{state, DAY};
use ic_cdk::api::time;
use internet_identity_interface::{ActiveAnchorCounter, ActiveAnchorStatistics, Timestamp};

pub fn update_active_anchors_stats(previous_activity_timestamp: Option<Timestamp>) {
    state::persistent_state_mut(
        |persistent_state| match persistent_state.active_anchor_stats {
            None => {
                let mut stats = ActiveAnchorStatistics::new(time());
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

#[allow(clippy::identity_op)]
fn rotate_active_anchor_stats_if_necessary(stats: &mut ActiveAnchorStatistics) {
    let now = time();
    if stats.ongoing.daily_active_anchors.start_timestamp + 1 * DAY <= now {
        let new_start_timestamp = stats.ongoing.daily_active_anchors.start_timestamp + 1 * DAY;
        stats.completed.daily_active_anchors = Some(stats.ongoing.daily_active_anchors.clone());
        stats.ongoing.daily_active_anchors = ActiveAnchorCounter::new(new_start_timestamp)
    }

    if let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.first() {
        if monthly_stats.start_timestamp + 30 * DAY <= now {
            let counter = stats.ongoing.monthly_active_anchors.remove(0);
            let new_start_timestamp = counter.start_timestamp + 30 * DAY;
            stats.completed.monthly_active_anchors = Some(counter);
            stats
                .ongoing
                .monthly_active_anchors
                .push(ActiveAnchorCounter::new(new_start_timestamp));
        }
    }
    if let Some(monthly_stats) = stats.ongoing.monthly_active_anchors.first() {
        // start a new 30 day time window if the last one starts more than 24h in the past.
        if monthly_stats.start_timestamp + 1 * DAY <= now {
            stats
                .ongoing
                .monthly_active_anchors
                .push(ActiveAnchorCounter::new(
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
