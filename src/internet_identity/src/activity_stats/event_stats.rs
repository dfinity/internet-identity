//! This module contains the event-based statistics for the Internet Identity.
//! Two data structures are required to maintain the event-based statistics:
//! - `event_data`: A map from timestamps to events.
//! - `event_aggregations`: A map from aggregation keys to aggregated values (i.e. running totals).
//!
//! Example of event_data:
//! ```
//! event_data: [
//!   {
//!     time: 1640995200000,
//!     counter: 0,
//!   } ->  {
//!     "event": {
//!       "PrepareDelegation": {
//!         "ii_domain": "Ic0App",
//!         "frontend": "https://dapp.example.com",
//!         "session_duration_ns": 3000000000
//!       }
//!     }
//!   },
//!   {
//!     time: 1641095230000,
//!     counter: 1,
//!   } ->  {
//!     "event": {
//!       "PrepareDelegation": {
//!         "ii_domain": "InternetComputerOrg",
//!         "frontend": "https://anotherapp.example.com",
//!         "session_duration_ns": 4500000000
//!       }
//!     }
//!   },
//!   ...
//! ]
//! ```
//!
//! Example of event_aggregations:
//! ```
//! event_aggregations: [
//!   {
//!     kind: AggregationKind::PrepareDelegationCount,
//!     window: AggregationWindow::Day,
//!     ii_domain: Some(IIDomain::Ic0App),
//!     data: "https://dapp.example.com".as_bytes(),
//!   } ->  5,
//!   {
//!     kind: AggregationKind::PrepareDelegationSessionSeconds,
//!     window: AggregationWindow::Day,
//!     ii_domain: Some(IIDomain::Ic0App),
//!     data: "https://dapp.example.com".as_bytes(),
//!   } ->  2700,
//!   ...
//! ]
//! ```

use crate::activity_stats::event_stats::event_aggregations::{
    AggregationKey, AggregationWindow, AGGREGATIONS,
};
use crate::ii_domain::IIDomain;
use crate::state::storage_borrow_mut;
use crate::storage::Storage;
use crate::{state, DAY_NS};
use ic_cdk::api::time;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::{Memory, StableBTreeMap, Storable};
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashMap;

/// This module defines the aggregations over the events.
pub mod event_aggregations;

#[cfg(test)]
mod tests;

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct EventKey {
    /// Timestamp of the event.
    pub time: Timestamp,
    /// A counter providing uniqueness for events with the same timestamp.
    pub counter: u16,
}

impl EventKey {
    pub fn new(time: Timestamp) -> Self {
        Self {
            time,
            counter: state::get_and_inc_event_data_counter(),
        }
    }

    pub fn min_key(time: Timestamp) -> Self {
        Self { time, counter: 0 }
    }

    pub fn max_key(time: Timestamp) -> Self {
        Self {
            time,
            counter: u16::MAX,
        }
    }
}

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

const EVENT_KEY_SIZE: usize = 10;
impl Storable for EventKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buf = Vec::with_capacity(EVENT_KEY_SIZE);
        buf.extend(self.time.to_be_bytes());
        buf.extend(self.counter.to_be_bytes());
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        Self {
            time: u64::from_be_bytes(
                TryFrom::try_from(&bytes[0..8]).expect("failed to read event key timestamp"),
            ),
            counter: u16::from_be_bytes(
                TryFrom::try_from(&bytes[8..10]).expect("failed to read event key counter"),
            ),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        is_fixed_size: true,
        max_size: EVENT_KEY_SIZE as u32,
    };
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
/// The key is the [AggregationKey] in the event_aggregations map and the weight is the weight of
/// the event (i.e. by how much the aggregation value is impacted by this event).
/// For example, if the aggregation is a count the weight is 1.
pub struct AggregationEvent {
    key: AggregationKey,
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
fn update_events_internal<M: Memory>(event: EventData, now: Timestamp, s: &mut Storage<M>) {
    let current_key = EventKey::new(now);
    let option = s.event_data.insert(current_key.clone(), event.clone());
    assert!(
        option.is_none(),
        "event already exists for key {:?}",
        current_key
    );

    let pruned_24h = if let Some((prev_key, _)) = s.event_data.iter_upper_bound(&current_key).next()
    {
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

        let prune_window_start = prev_key.time - DAY_NS;
        let prune_window_end = now - DAY_NS;
        s.event_data
            .range(EventKey::min_key(prune_window_start)..=EventKey::max_key(prune_window_end))
            .collect::<Vec<_>>()
    } else {
        // there is no event before `now` in the db, so the list of pruned events is empty
        vec![]
    };

    // Update 24h aggregations
    AGGREGATIONS.iter().for_each(|aggregation| {
        update_aggregation(
            |(_, data)| aggregation.process_event(AggregationWindow::Day, data),
            current_key.clone(),
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
    AGGREGATIONS.iter().for_each(|aggregation| {
        update_aggregation(
            |(_, data)| aggregation.process_event(AggregationWindow::Month, data),
            current_key.clone(),
            event.clone(),
            &pruned_30d,
            &mut s.event_aggregations,
        );
    });
}

/// Adds an event to the event_data map and simultaneously removes events older than the retention period (30d).
/// Returns a vec of tuples of the pruned events and their timestamps.
fn prune_events<M: Memory>(
    db: &mut StableBTreeMap<EventKey, EventData, M>,
    now: Timestamp,
) -> Vec<(EventKey, EventData)> {
    const RETENTION_PERIOD: u64 = 30 * DAY_NS;

    let pruned_events = db
        .range(..=EventKey::max_key(now - RETENTION_PERIOD))
        .collect();
    for entry in &pruned_events {
        let entry: &(EventKey, EventData) = entry;
        db.remove(&entry.0);
    }

    pruned_events
}

fn update_aggregation<M: Memory, F>(
    aggregation_filter_map: F,
    now: EventKey,
    new_event: EventData,
    pruned_events: &[(EventKey, EventData)],
    db: &mut StableBTreeMap<AggregationKey, u64, M>,
) where
    F: Fn(&(EventKey, EventData)) -> Option<AggregationEvent>,
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
                panic!("aggregation key \"{:?}\" not found in DB when pruning", key)
            });
            assert!(
                current_weight >= pruned_weight,
                "pruned weight {} exceeds current weight {} for key \"{:?}\"",
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
