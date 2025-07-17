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

use crate::ii_domain::IIDomain;
use crate::state::storage_borrow_mut;
use crate::stats::event_stats::event_aggregations::AGGREGATIONS;
use crate::storage::Storage;
use crate::{state, DAY_NS};
use candid::CandidType;
use ic_cdk::api::time;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::{Memory, StableBTreeMap, Storable};
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashMap;

/// This module defines the aggregations over the events.
mod event_aggregations;

pub use event_aggregations::*;

/// Number of past events to process per aggregation window in a single update call
/// This limit will be applied twice, to the 24h and 30d aggregations.
const MAX_EVENTS_TO_PROCESS: usize = 100;

#[cfg(test)]
mod tests;

#[derive(Deserialize, Serialize, CandidType, Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
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

    pub fn next_key(&self) -> Self {
        match self.counter {
            u16::MAX => Self {
                time: self.time + 1,
                counter: 0,
            },
            _ => Self {
                time: self.time,
                counter: self.counter + 1,
            },
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
    /// No longer used, but kept for backwards compatibility.
    /// Used to be injected by a timer to artificially trigger regular pruning. No longer needed
    /// due to the explicit [MAX_EVENTS_TO_PROCESS] limit.
    PruneEvent,
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
    if option.is_some() {
        ic_cdk::println!("WARN: Event already exists for key {:?}", current_key);
    }

    // Collect events that should no longer be reflected in the 24h aggregations. Does _not_ delete
    // the underlying events.
    let removed_24h = events_to_remove_from_24h_aggregations(now, &current_key, &s.event_data);
    // Update 24h aggregations
    update_aggregations(
        &current_key,
        &event,
        &removed_24h,
        AggregationWindow::Day,
        &mut s.event_aggregations,
    );

    // This pruning _deletes_ the data older than 30 days. Do this after the 24h aggregation
    // otherwise the daily stats become inaccurate on the unlikely event that there is no activity
    // for 30 days.
    let pruned_30d = prune_events(&mut s.event_data, now);
    // Update 30d aggregations
    update_aggregations(
        &current_key,
        &event,
        &pruned_30d,
        AggregationWindow::Month,
        &mut s.event_aggregations,
    );
}

/// Iterates over all aggregations and updates them based on the new event and the pruned events.
fn update_aggregations<M: Memory>(
    event_key: &EventKey,
    event_data: &EventData,
    pruned_events: &[(EventKey, EventData)],
    window: AggregationWindow,
    aggregations_db: &mut StableBTreeMap<AggregationKey, u64, M>,
) {
    AGGREGATIONS.iter().for_each(|aggregation| {
        update_aggregation(
            |(_, data)| aggregation.process_event(window.clone(), data),
            event_key.clone(),
            event_data.clone(),
            pruned_events,
            aggregations_db,
        );
    });
}

/// Collects events older than 24h that need to be removed from the 24h aggregations.
/// Given events are kept for 30 days, the events are not deleted from the supplied `db`.
/// Instead, this function simply updates the `event_stats_24h_start` state variable
/// that denotes the first event that should be removed from the 24h window in the next call.
///
/// Returns a vec of tuples of the pruned events and their timestamps, at most [MAX_EVENTS_TO_PROCESS].
fn events_to_remove_from_24h_aggregations<M: Memory>(
    now: Timestamp,
    current_key: &EventKey,
    db: &StableBTreeMap<EventKey, EventData, M>,
) -> Vec<(EventKey, EventData)> {
    /// Calculates the 24h window start based on the current key:
    /// - The current key is used to find the last event right before it.
    /// - The timestamp of that key is then shifted back 24h.
    ///
    /// This assumes that the 24h window has been correctly pruned in the past, including up to the
    /// previous event.
    ///
    /// ```
    ///    |<----------------------- 24h ----------------------->|
    /// |--|--------------------------------------------------|--|   --> time
    /// ^  ^                                                  ^  ^
    /// |  |                                                  |  └ current_key
    /// |  └ now - 24h                                        └ previous event
    /// └ previous event timestamp - 24h
    /// ```
    fn window_start_from_current_key<M: Memory>(
        current_key: &EventKey,
        db: &StableBTreeMap<EventKey, EventData, M>,
    ) -> Option<EventKey> {
        db.iter_upper_bound(current_key)
            .next()
            .map(|(k, _)| EventKey::min_key(k.time - DAY_NS))
    }

    let window_start =
        // Load the window start from persistent state. This value will be set if events have been
        // removed from the 24h window before.
        state::persistent_state(|s| s.event_stats_24h_start.clone()).or_else(|| {
            // Alternatively, calculate it from the current key. This is necessary in two cases:
            // - the events have never been removed before because they are not yet 24h old.
            // - this is the first event after an II upgrade from a version that did not have this
            //   state variable to track the beginning of the 24h window.
            window_start_from_current_key(current_key, db)
        });

    let Some(start_key) = window_start else {
        // there is no key to start from, so the list of removed events is empty.
        return vec![];
    };

    // Always aim to collect events up to 24h ago, even if past update calls did not manage due to
    // the MAX_EVENTS_TO_PROCESS limit.
    let window_end_timestamp = now - DAY_NS;
    let events = db
        .range(start_key..=EventKey::max_key(window_end_timestamp))
        .take(MAX_EVENTS_TO_PROCESS)
        .collect::<Vec<_>>();

    // update the persistent state with they key _after_ the one being pointed to by the last event
    // so that the next amortized clean-up can continue from there.
    if let Some((k, _)) = events.last() {
        state::persistent_state_mut(|s| {
            s.event_stats_24h_start = Some(k.next_key());
        });
    }
    events
}

/// Removes events older than the retention period (30d).
/// Returns a vec of tuples of the pruned events and their timestamps.
/// Prunes at most [MAX_EVENTS_TO_PROCESS].
fn prune_events<M: Memory>(
    db: &mut StableBTreeMap<EventKey, EventData, M>,
    now: Timestamp,
) -> Vec<(EventKey, EventData)> {
    const RETENTION_PERIOD: u64 = 30 * DAY_NS;

    let pruned_events: Vec<_> = db
        .range(..=EventKey::max_key(now - RETENTION_PERIOD))
        .take(MAX_EVENTS_TO_PROCESS)
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
        // zero weighted events are ignored when adjusting aggregations during pruning because they
        // don't contribute any change to the aggregations hence pruning them has no effect on
        // aggregations
        .filter(|weighted_aggregation| weighted_aggregation.weight > 0)
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
            // Adjust aggregation weight by subtracting the pruned weight
            if let Some(current_weight) = db.get(&key) {
                let new_weight = current_weight.saturating_sub(pruned_weight);
                if new_weight == 0 {
                    db.remove(&key);
                } else {
                    db.insert(key, new_weight);
                }
            } else {
                // This should not happen, but if it does, it's a bug.
                // Print a message so that we can investigate once canister logs are available.
                ic_cdk::println!("WARN: Aggregation key {:?} not found in db", key);
            }
        });
}
