use crate::ii_domain::IIDomain::{Ic0App, InternetComputerOrg};
use crate::ii_domain::{maybe_domain_to_label, IIDomain};
use crate::state::storage_borrow;
use crate::stats::event_stats::event_aggregations::AggregationWindow::{Day, Month};
use crate::stats::event_stats::{AggregationEvent, Event, EventData, PrepareDelegationEvent};
use crate::storage::Storage;
use candid::Deserialize;
use ic_stable_structures::{storable, Memory, Storable};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Bound;
use std::time::Duration;

#[cfg(test)]
mod tests;

/// Trait for an event aggregation.
///
/// Each aggregation provides a `process_event` function that takes an [AggregationWindow] and [EventData]
/// and maps it to an optional [AggregationEvent] (i.e. a key for the aggregation and a weight for the event).
/// If the event is not part of the aggregation, the function should return [None].
pub trait Aggregation {
    fn kind(&self) -> AggregationKind;

    fn label(&self) -> &'static str;

    fn key_label(&self, key: AggregationKey) -> String;

    fn process_event(
        &self,
        window: AggregationWindow,
        event: &EventData,
    ) -> Option<AggregationEvent>;
}

/// Returns a map of all aggregations, each with up to `limit` many data points.
/// For each aggregation, the data points are sorted by weight in descending order.
pub fn all_aggregations_top_n(limit: usize) -> HashMap<String, Vec<(String, u64)>> {
    let mut aggregations = HashMap::new();
    for aggregation in &AGGREGATIONS {
        for window in &[Day, Month] {
            for domain in &[None, Some(Ic0App), Some(InternetComputerOrg)] {
                let tuples: Vec<_> =
                    retrieve_aggregation(*aggregation, window.clone(), domain.clone())
                        .into_iter()
                        .take(limit)
                        .collect();
                if tuples.is_empty() {
                    // don't add empty aggregations
                    continue;
                }
                aggregations.insert(
                    format!(
                        "{} {} {}",
                        aggregation.label(),
                        window.label(),
                        maybe_domain_to_label(domain)
                    ),
                    tuples,
                );
            }
        }
    }
    aggregations
}

/// Retrieve the aggregation data for the given window and II domain.
/// The data is returned as a map from a human-readable label to the aggregation weight.
pub fn retrieve_aggregation(
    aggregation: &dyn Aggregation,
    window: AggregationWindow,
    ii_domain: Option<IIDomain>,
) -> Vec<(String, u64)> {
    storage_borrow(|s| retrieve_aggregation_internal(aggregation, window, ii_domain, s))
}

fn retrieve_aggregation_internal<M: Memory>(
    aggregation: &dyn Aggregation,
    window: AggregationWindow,
    ii_domain: Option<IIDomain>,
    s: &Storage<M>,
) -> Vec<(String, u64)> {
    // Limit the number of aggregations retrieved in one go to avoid hitting the cycles limit when
    // retrieving stats or metrics. This limit is currently over-provisioned by ~10x.
    // If the limit is reached, we should switch to a naturally ordered aggregation
    // data structure such that we can retrieve the top-n elements directly.
    const AGGREGATIONS_LIMIT: usize = 5_000;
    let range_start = AggregationKey {
        kind: aggregation.kind(),
        window,
        ii_domain,
        data: ByteBuf::from(vec![]),
    };
    let range_end = range_start.upper_bound();
    let mut data: Vec<_> = s
        .event_aggregations
        .range((Bound::Included(range_start), range_end))
        .take(AGGREGATIONS_LIMIT)
        .map(|(key, weight)| (aggregation.key_label(key), weight))
        .collect();
    data.sort_by(|(_, weight_a), (_, weight_b)| weight_b.cmp(weight_a));
    data
}

/// Aggregates the count of prepare delegation events
/// - per dapp frontend
/// - per II domain
/// - over 24h and 30d buckets
struct PrepareDelegationCount;

impl Aggregation for PrepareDelegationCount {
    fn kind(&self) -> AggregationKind {
        AggregationKind::PrepareDelegationCount
    }

    fn label(&self) -> &'static str {
        "prepare_delegation_count"
    }

    fn key_label(&self, key: AggregationKey) -> String {
        String::from_utf8(key.data.into_vec()).expect("Invalid UTF-8")
    }

    fn process_event(
        &self,
        window: AggregationWindow,
        event: &EventData,
    ) -> Option<AggregationEvent> {
        prepare_delegation_aggregation(self.kind(), window, |_| 1, event)
    }
}

/// Aggregates the cumulative session duration in seconds of prepare delegation events
/// - per dapp frontend
/// - per II domain
/// - over 24h and 30d buckets
struct PrepareDelegationSessionSeconds;

impl Aggregation for PrepareDelegationSessionSeconds {
    fn kind(&self) -> AggregationKind {
        AggregationKind::PrepareDelegationSessionSeconds
    }

    fn label(&self) -> &'static str {
        "prepare_delegation_session_seconds"
    }

    fn key_label(&self, key: AggregationKey) -> String {
        String::from_utf8(key.data.into_vec()).expect("Invalid UTF-8")
    }

    fn process_event(
        &self,
        window: AggregationWindow,
        event: &EventData,
    ) -> Option<AggregationEvent> {
        prepare_delegation_aggregation(
            self.kind(),
            window,
            |event| Duration::from_nanos(event.session_duration_ns).as_secs(),
            event,
        )
    }
}

/// Aggregates the number of injected [Event::PruneEvent] events
/// - over 24h and 30d buckets
struct PruneEventCount;

impl Aggregation for PruneEventCount {
    fn kind(&self) -> AggregationKind {
        AggregationKind::PruneEventCount
    }

    fn label(&self) -> &'static str {
        "prune_event_count"
    }

    fn key_label(&self, _key: AggregationKey) -> String {
        // this aggregation only maintains a single count, so the key label is empty.
        "".to_string()
    }

    fn process_event(
        &self,
        window: AggregationWindow,
        event: &EventData,
    ) -> Option<AggregationEvent> {
        match &event.event {
            Event::PruneEvent => {
                let key = AggregationKey {
                    kind: self.kind(),
                    window,
                    ii_domain: None,
                    data: ByteBuf::from(vec![]),
                };
                Some(AggregationEvent { key, weight: 1 })
            }
            _ => None,
        }
    }
}

pub const PD_COUNT: &dyn Aggregation = &PrepareDelegationCount;
pub const PD_SESS_SEC: &dyn Aggregation = &PrepareDelegationSessionSeconds;
pub const PRUNE_COUNT: &dyn Aggregation = &PruneEventCount;

/// List of aggregations currently maintained over events.
pub const AGGREGATIONS: [&'static dyn Aggregation; 3] = [PD_COUNT, PD_SESS_SEC, PRUNE_COUNT];

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Hash)]
pub enum AggregationWindow {
    Day,   // 24 hours
    Month, // 30 days
}

impl AggregationWindow {
    pub fn next(&self) -> Option<AggregationWindow> {
        use AggregationWindow::*;
        match &self {
            Day => Some(Month),
            Month => None,
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            Day => "24h",
            Month => "30d",
        }
    }
}

/// All kinds of aggregation types currently supported.
#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Hash)]
pub enum AggregationKind {
    PrepareDelegationCount,
    PrepareDelegationSessionSeconds,
    PruneEventCount,
}

impl AggregationKind {
    pub fn next(&self) -> Option<AggregationKind> {
        use AggregationKind::*;
        match &self {
            PrepareDelegationCount => Some(PrepareDelegationSessionSeconds),
            PrepareDelegationSessionSeconds => Some(PruneEventCount),
            PruneEventCount => None,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Hash)]
pub struct AggregationKey {
    /// The aggregation kind (i.e. category).
    kind: AggregationKind,
    /// The aggregation window (i.e. bucket size).
    window: AggregationWindow,
    /// The II domain, if any, that is associated with the aggregation.
    ii_domain: Option<IIDomain>,
    /// Additional data that is used to differentiate multiple entries of the same aggregation.
    /// For example, the frontend origin of a dapp in the case of `prepare_delegation` aggregations.
    data: ByteBuf,
}

impl AggregationKey {
    #[cfg(test)] // currently only used in tests
    pub fn new(
        kind: AggregationKind,
        window: AggregationWindow,
        ii_domain: Option<IIDomain>,
        frontend: String,
    ) -> Self {
        Self {
            kind,
            window,
            ii_domain,
            data: ByteBuf::from(frontend.as_bytes()),
        }
    }

    /// Returns the end bound of and aggregation key range that belong to the same `kind`, `window`
    /// and `ii_domain` as the current key.
    /// The upper bound can then be used to retrieve a range of aggregations with all possible `data`
    /// values, starting from the current key.
    fn upper_bound(&self) -> Bound<Self> {
        let empty_data = ByteBuf::from(vec![]);
        let next_ii_domain = match self.ii_domain {
            None => Some(Ic0App),
            Some(Ic0App) => Some(InternetComputerOrg),
            Some(InternetComputerOrg) => None,
        };
        if next_ii_domain.is_some() {
            let end_bound = Bound::Excluded(AggregationKey {
                kind: self.kind.clone(),
                window: self.window.clone(),
                ii_domain: next_ii_domain,
                data: empty_data,
            });
            return end_bound;
        }

        let next_window = self.window.next();
        if let Some(window) = next_window {
            let end_bound = Bound::Excluded(AggregationKey {
                kind: self.kind.clone(),
                window,
                ii_domain: None,
                data: empty_data,
            });
            return end_bound;
        }

        let next_kind = self.kind.next();
        if let Some(kind) = next_kind {
            let end_bound = Bound::Excluded(AggregationKey {
                kind,
                window: Day,
                ii_domain: None,
                data: empty_data,
            });
            return end_bound;
        }
        // This is the last category of aggregation, so all values starting from the given key belong to the same category.
        Bound::Unbounded
    }
}

impl Storable for AggregationKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: storable::Bound = storable::Bound::Unbounded;
}

fn prepare_delegation_aggregation(
    kind: AggregationKind,
    window: AggregationWindow,
    weight_func: fn(&PrepareDelegationEvent) -> u64,
    event: &EventData,
) -> Option<AggregationEvent> {
    match &event.event {
        Event::PrepareDelegation(prepare_delegation_event) => {
            let key = AggregationKey {
                kind,
                window,
                ii_domain: prepare_delegation_event.ii_domain.clone(),
                data: ByteBuf::from(prepare_delegation_event.frontend.as_bytes()),
            };
            Some(AggregationEvent {
                key,
                weight: weight_func(prepare_delegation_event),
            })
        }
        _ => None,
    }
}
