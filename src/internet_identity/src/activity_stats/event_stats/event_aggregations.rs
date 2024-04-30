use crate::activity_stats::event_stats::{
    AggregationEvent, Event, EventData, PrepareDelegationEvent,
};
use crate::ii_domain::IIDomain;
use std::time::Duration;

/// Type definition for an aggregation function.
///
/// Each aggregation is defined as a function that takes a bucket size label (i.e. "24h" or "30d")
/// and a tuple of `(timestamp, event_data)` and maps it to an optional [AggregationEvent]
/// (i.e. a key for the aggregation and a weight for the event).
/// If the event is not part of the aggregation, the function should return [None].
type Aggregation = fn(&str, &(u64, EventData)) -> Option<AggregationEvent>;

/// All aggregations currently maintained over event data.
pub const AGGREGATIONS: [Aggregation; 2] =
    [PREPARE_DELEGATION_COUNT, PREPARE_DELEGATION_SESSION_SECONDS];

/// Aggregates the count of prepare delegation events
/// - per dapp frontend
/// - per II domain
/// - over 24h and 30d buckets
///
/// Results in labels like "PD_count_<bucket_length>_<ii_domain>_<frontend_hostname>"
/// e.g "PD_count_24h_ic0.app_https://dapp.example.com"
const PREPARE_DELEGATION_COUNT: fn(&str, &(u64, EventData)) -> Option<AggregationEvent> =
    |key_duration: &str, (_timestamp, event): &(u64, EventData)| -> Option<AggregationEvent> {
        prepare_delegation_aggregation("PD_count", key_duration, |_| 1, event)
    };

/// Aggregates the cumulative session duration in seconds of prepare delegation events
/// - per dapp frontend
/// - per II domain
/// - over 24h and 30d buckets
///
/// Results in labels like "PD_sess_sec_<bucket_length>_<ii_domain>_<frontend_hostname>"
/// e.g "PD_sess_sec_24h_ic0.app_https://dapp.example.com"
const PREPARE_DELEGATION_SESSION_SECONDS: fn(&str, &(u64, EventData)) -> Option<AggregationEvent> =
    |key_duration: &str, (_timestamp, event): &(u64, EventData)| -> Option<AggregationEvent> {
        prepare_delegation_aggregation(
            "PD_sess_sec",
            key_duration,
            |event| Duration::from_nanos(event.session_duration_ns).as_secs(),
            event,
        )
    };

fn prepare_delegation_aggregation(
    key_prefix: &str,
    key_duration: &str,
    weight_func: fn(&PrepareDelegationEvent) -> u64,
    event: &EventData,
) -> Option<AggregationEvent> {
    match &event.event {
        Event::PrepareDelegation(prepare_delegation_event) => {
            let key = format!(
                "{}_{}_{}_{}",
                key_prefix,
                key_duration,
                ii_domain_to_label(&prepare_delegation_event.ii_domain),
                prepare_delegation_event.frontend
            );
            Some(AggregationEvent {
                key,
                weight: weight_func(prepare_delegation_event),
            })
        }
    }
}

fn ii_domain_to_label(domain: &Option<IIDomain>) -> &str {
    match domain {
        Some(IIDomain::Ic0AppDomain) => "ic0.app",
        Some(IIDomain::InternetComputerOrgDomain) => "internetcomputer.org",
        None => "other",
    }
}
