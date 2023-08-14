use crate::activity_stats::activity_counter::domain_active_anchor_counter::DomainActivityContext;
use crate::activity_stats::activity_counter::ActivityCounter;
use crate::state;
use crate::storage::anchor::{Anchor, Device};
use candid::{CandidType, Deserialize};
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::Timestamp;

pub mod activity_counter;
mod stats_maintenance;

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActivityStats<T: ActivityCounter> {
    // Stats for the last completed collection period for daily and monthly activity
    pub completed: CompletedActivityStats<T>,
    // ongoing collection periods for daily and monthly activity
    pub ongoing: OngoingActivityStats<T>,
}

impl<T: ActivityCounter> ActivityStats<T> {
    fn new(time: Timestamp) -> Self {
        Self {
            completed: CompletedActivityStats {
                daily_events: None,
                monthly_events: None,
            },
            ongoing: OngoingActivityStats {
                daily_events: ActivityCounter::new(time),
                monthly_events: vec![ActivityCounter::new(time)],
            },
        }
    }

    /// Updates all ongoing counters with the given context.
    /// Also performs maintenance on the stats, e.g. removing old monthly counters.
    fn update_counters(&mut self, context: &T::CountingContext<'_>) {
        stats_maintenance::process_stats(self);
        self.ongoing.daily_events.count_event(context);
        self.ongoing
            .monthly_events
            .iter_mut()
            .for_each(|counter| counter.count_event(context));
    }
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct CompletedActivityStats<T: ActivityCounter> {
    // Completed daily activity counter.
    //
    // For legacy reasons / stable memory compatibility the old name is kept when serializing.
    // This will be cleaned up when the stats are moved out of the persistent state directly
    // into the stable memory.
    #[serde(rename = "daily_active_anchors")]
    pub daily_events: Option<T>,

    // Completed monthly activity counter.
    //
    // For legacy reasons / stable memory compatibility the old name is kept when serializing.
    // This will be cleaned up when the stats are moved out of the persistent state directly
    // into the stable memory.
    #[serde(rename = "monthly_active_anchors")]
    pub monthly_events: Option<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct OngoingActivityStats<T: ActivityCounter> {
    // Ongoing activity counter for the current 24 h time bucket.
    //
    // For legacy reasons / stable memory compatibility the old name is kept when serializing.
    // This will be cleaned up when the stats are moved out of the persistent state directly
    // into the stable memory.
    #[serde(rename = "daily_active_anchors")]
    pub daily_events: T,

    // Monthly activity is collected using 30-day sliding windows.
    // This vec contains up to 30 30-day active windows each offset by one day.
    // The vec is sorted, new collection windows are added at the end.
    //
    // For legacy reasons / stable memory compatibility the old name is kept when serializing.
    // This will be cleaned up when the stats are moved out of the persistent state directly
    // into the stable memory.
    #[serde(rename = "monthly_active_anchors")]
    pub monthly_events: Vec<T>,
}

pub fn update_activity_stats(anchor: &Anchor, current_device: &Device) {
    state::persistent_state_mut(|persistent_state| {
        // Active anchor stats across all domains
        let stats = persistent_state
            .active_anchor_stats
            .get_or_insert_with(|| ActivityStats::new(time()));
        stats.update_counters(&anchor.last_activity());

        // Active anchor stats, II domains only
        if let Some(domain) = current_device.ii_domain() {
            let context = DomainActivityContext {
                anchor,
                current_domain: domain,
            };
            let stats = persistent_state
                .domain_active_anchor_stats
                .get_or_insert_with(|| ActivityStats::new(time()));
            stats.update_counters(&context);
        }

        // Active authn methods stats, II domains only
        let stats = persistent_state
            .active_authn_method_stats
            .get_or_insert_with(|| ActivityStats::new(time()));
        stats.update_counters(&current_device);
    })
}
