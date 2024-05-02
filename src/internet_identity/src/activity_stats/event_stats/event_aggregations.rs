use crate::activity_stats::event_stats::{
    AggregationEvent, Event, EventData, PrepareDelegationEvent,
};
use crate::ii_domain::IIDomain;
use candid::Deserialize;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::time::Duration;

/// Trait for an event aggregation.
///
/// Each aggregation provides a `process_event` function that takes an [AggregationWindow] and [EventData]
/// and maps it to an optional [AggregationEvent] (i.e. a key for the aggregation and a weight for the event).
/// If the event is not part of the aggregation, the function should return [None].
pub trait Aggregation {
    fn kind(&self) -> AggregationKind;
    fn process_event(
        &self,
        window: AggregationWindow,
        event: &EventData,
    ) -> Option<AggregationEvent>;
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

/// List of aggregations currently maintained over events.
pub const AGGREGATIONS: [&'static dyn Aggregation; 2] =
    [&PrepareDelegationCount, &PrepareDelegationSessionSeconds];

#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Hash)]
pub enum AggregationWindow {
    Day,   // 24 hours
    Month, // 30 days
}

/// All kinds of aggregation types currently supported.
#[derive(Deserialize, Serialize, Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Hash)]
pub enum AggregationKind {
    PrepareDelegationCount,
    PrepareDelegationSessionSeconds,
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
}

impl Storable for AggregationKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
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
    }
}
