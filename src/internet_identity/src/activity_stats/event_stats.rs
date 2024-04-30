//! This module contains the event-based statistics for the Internet Identity.
//! Two data structures are required to maintain the event-based statistics:
//! - `event_data`: A map from timestamps to events.
//! - `event_aggregations`: A map from aggregation keys to aggregated values (i.e. running totals).
//!
//! Example of event_data:
//! ```
//! event_data: {
//!   "1640995200000": {
//!     "event": {
//!       "PrepareDelegation": {
//!         "ii_domain": "Ic0AppDomain",
//!         "frontend": "https://dapp.example.com",
//!         "session_duration_ns": 3000000000
//!       }
//!     }
//!   },
//!   "1640998800000": {
//!     "event": {
//!       "PrepareDelegation": {
//!         "ii_domain": "InternetComputerOrgDomain",
//!         "frontend": "https://anotherapp.example.com",
//!         "session_duration_ns": 4500000000
//!       }
//!     }
//!   },
//!   //...
//! }
//! ```
//!
//! Example of event_aggregations:
//! ```
//! event_aggregations: {
//!   "PD_count_24h_ic0.app_https://dapp.example.com": 5,
//!   "PD_sess_sec_24h_ic0.app_https://dapp.example.com": 15,
//!   "PD_count_30d_internetcomputer.org_https://anotherapp.example.com": 20,
//!   "PD_sess_sec_30d_internetcomputer.org_https://anotherapp.example.com": 60
//! }
//! ```

use crate::activity_stats::event_stats::event_aggregations::AGGREGATIONS;
use crate::ii_domain::IIDomain;
use crate::state::storage_borrow_mut;
use crate::storage::Storage;
use crate::DAY_NS;
use ic_cdk::api::time;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::{Memory, StableBTreeMap, Storable};
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashMap;

/// This module defines the aggregations over the events.
mod event_aggregations;

#[cfg(test)]
mod tests;

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug)]
pub struct EventData {
    pub event: Event,
}

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug)]
pub enum Event {
    PrepareDelegation(PrepareDelegationEvent),
}

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug)]
pub struct PrepareDelegationEvent {
    pub ii_domain: Option<IIDomain>,
    pub frontend: FrontendHostname,
    pub session_duration_ns: u64,
}

impl Storable for EventData {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

/// Result of applying an aggregation function to an event.
/// The key is the aggregation key in the event_aggregations map and the weight is the weight of
/// the event (i.e. by how much the aggregation value is impacted by this event).
/// For example, if the aggregation is a count the weight is 1.
struct AggregationEvent {
    key: String,
    weight: u64,
}

/// Updates the event-based stats with the given event.
///
/// This adds the event to the event_data map and updates the daily and monthly aggregations over
/// the event data.
pub fn update_event_based_stats(event: EventData) {
    let now = time();
    storage_borrow_mut(|s| {
        update_events_internal(event, now, s);
    })
}

/// Internal helper useful for testing.
fn update_events_internal<M: Memory>(event: EventData, now: u64, s: &mut Storage<M>) {
    s.event_data.insert(now, event.clone());

    let pruned_24h = if let Some((timestamp, _)) = s.event_data.iter_upper_bound(&now).next() {
        // `timestamp` denotes the time of the last event recorded just before `now`
        // The difference (now - timestamp) is the time that has passed since the last event
        // and hence the last time the daily stats were updated.
        // Therefore, we need to prune all events from the daily aggregations that happened in
        // that same difference, but 24 hours ago.
        //
        //    |<----------------------- 24h ----------------------->|
        // |--|--------------------------------------------------|--|   --> time
        // ^  ^                                                  ^  ^
        // |  |                                                  |  └ now
        // |  └ now - 24h (prune_window_end)                     └ timestamp
        // └ timestamp - 24h (prune_window_start)

        let prune_window_start = timestamp - DAY_NS;
        let prune_window_end = now - DAY_NS;
        s.event_data
            .range(prune_window_start..=prune_window_end)
            .collect::<Vec<_>>()
    } else {
        // there is no event before `now` in the db, so the list of pruned events is empty
        vec![]
    };

    // Update 24h aggregations
    AGGREGATIONS.iter().for_each(|aggregation_filter_map| {
        update_aggregation(
            |input| aggregation_filter_map("24h", input),
            now,
            event.clone(),
            &pruned_24h,
            &mut s.event_aggregations,
        );
    });

    // This pruning _deletes_ the data older than 30 days. Do this after the 24h aggregation
    // otherwise the daily stats become inaccurate on the unlikely event that there is no activity
    // for 30 days.
    let pruned_30d = prune_events(&mut s.event_data, now);

    // Update 30d aggregations
    AGGREGATIONS.iter().for_each(|aggregation_filter_map| {
        update_aggregation(
            |input| aggregation_filter_map("30d", input),
            now,
            event.clone(),
            &pruned_30d,
            &mut s.event_aggregations,
        );
    });
}

/// Adds an event to the event_data map and simultaneously removes events older than the retention period (30d).
/// Returns a vec of tuples of the pruned events and their timestamps.
fn prune_events<M: Memory>(
    db: &mut StableBTreeMap<Timestamp, EventData, M>,
    now: Timestamp,
) -> Vec<(Timestamp, EventData)> {
    const RETENTION_PERIOD: u64 = 30 * DAY_NS;

    let pruned_events = db.range(..=now - RETENTION_PERIOD).collect();
    for entry in &pruned_events {
        let entry: &(Timestamp, EventData) = entry;
        db.remove(&entry.0);
    }

    pruned_events
}

fn update_aggregation<M: Memory, F>(
    aggregation_filter_map: F,
    now: Timestamp,
    new_event: EventData,
    pruned_events: &[(Timestamp, EventData)],
    db: &mut StableBTreeMap<String, u64, M>,
) where
    F: Fn(&(u64, EventData)) -> Option<AggregationEvent>,
{
    // Process the pruned events to calculate the pruned weight by aggregation key.
    // See PREPARE_DELEGATION_COUNT and PREPARE_DELEGATION_SESSION_SECONDS for examples of such keys.
    let mut pruned_weight_by_key = pruned_events
        .iter()
        .filter_map(&aggregation_filter_map)
        .fold(HashMap::<_, u64>::new(), |mut map, weighted_aggregation| {
            let value =
                map.get(&weighted_aggregation.key).unwrap_or(&0) + weighted_aggregation.weight;
            map.insert(weighted_aggregation.key, value);
            map
        });

    // Process the new event and merge it with the pruned events
    if let Some(AggregationEvent { key, mut weight }) = aggregation_filter_map(&(now, new_event)) {
        if let Some(pruned_weight) = pruned_weight_by_key.get_mut(&key) {
            if *pruned_weight <= weight {
                weight -= *pruned_weight;
                // more weight is added than pruned, so the key can be removed from the pruned map
                pruned_weight_by_key.remove(&key);
            } else {
                // we can just reduce the amount of pruned weight
                *pruned_weight -= weight;
                weight = 0; // no weight left to add
            }
        }
        // if there is more weight of the current event, write it to the db
        if weight > 0 {
            let current_weight = db.get(&key).unwrap_or(0);
            db.insert(key, current_weight + weight);
        }
    }

    // Update the db to reflect the pruned weights
    pruned_weight_by_key
        .into_iter()
        .for_each(|(key, pruned_weight)| {
            let current_weight = db.get(&key).unwrap_or_else(|| {
                panic!("aggregation key \"{}\" not found in DB when pruning", key)
            });
            assert!(
                current_weight >= pruned_weight,
                "pruned weight {} exceeds current weight {} for key \"{}\"",
                pruned_weight,
                current_weight,
                key
            );
            let new_weight = current_weight - pruned_weight;
            if new_weight == 0 {
                db.remove(&key);
            } else {
                db.insert(key, new_weight);
            }
        });
}
