use crate::ii_domain::IIDomain;
use crate::stats::event_stats::event_aggregations::AggregationWindow::{Day, Month};
use crate::stats::event_stats::event_aggregations::{
    retrieve_aggregation_internal, PD_COUNT, PD_SESS_SEC,
};
use crate::stats::event_stats::{update_events_internal, Event, EventData, PrepareDelegationEvent};
use crate::storage::Storage;
use crate::DAY_NS;
use ic_stable_structures::VectorMemory;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

const TIMESTAMP: u64 = 1_714_387_451 * 10u64.pow(9);
const EXAMPLE_URL: &str = "https://example.com";

const DUMMY_SESSION_LENGTH_SEC: u64 = 900;

#[test]
fn should_retrieve_aggregations() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(DUMMY_SESSION_LENGTH_SEC).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    // expected existing
    let results = retrieve_aggregation_internal(PD_COUNT, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(EXAMPLE_URL.to_string(), 1)]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(
        results,
        vec![(EXAMPLE_URL.to_string(), DUMMY_SESSION_LENGTH_SEC)]
    );

    let results = retrieve_aggregation_internal(PD_COUNT, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(EXAMPLE_URL.to_string(), 1)]);

    let results =
        retrieve_aggregation_internal(PD_SESS_SEC, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(
        results,
        vec![(EXAMPLE_URL.to_string(), DUMMY_SESSION_LENGTH_SEC)]
    );

    // expected non-existing
    let results = retrieve_aggregation_internal(
        PD_SESS_SEC,
        Day,
        Some(IIDomain::InternetComputerOrg),
        &storage,
    );
    assert_eq!(results, vec![]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Day, None, &storage);
    assert_eq!(results, vec![]);

    let results = retrieve_aggregation_internal(
        PD_SESS_SEC,
        Month,
        Some(IIDomain::InternetComputerOrg),
        &storage,
    );
    assert_eq!(results, vec![]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Month, None, &storage);
    assert_eq!(results, vec![]);
}

#[test]
fn should_retrieve_aggregations_for_multiple_frontend_origins_sorted_by_weight() {
    let mut storage = test_storage();
    for i in 0..10 {
        let event = EventData {
            event: Event::PrepareDelegation(PrepareDelegationEvent {
                ii_domain: Some(IIDomain::Ic0App),
                frontend: format!("https://example{i}.com"),
                session_duration_ns: Duration::from_secs(DUMMY_SESSION_LENGTH_SEC + i).as_nanos()
                    as u64,
            }),
        };
        update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    }

    let per_origin_weight_one = (0..10)
        .map(|i| (format!("https://example{i}.com"), 1))
        .collect::<Vec<_>>();
    let per_origin_sess_weight = (0..10)
        .rev() // aggregations are sorted by weight descending
        .map(|i| {
            (
                format!("https://example{i}.com"),
                DUMMY_SESSION_LENGTH_SEC + i,
            )
        })
        .collect::<Vec<_>>();
    let results = retrieve_aggregation_internal(PD_COUNT, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, per_origin_weight_one);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, per_origin_sess_weight);

    let results = retrieve_aggregation_internal(PD_COUNT, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, per_origin_weight_one);

    let results =
        retrieve_aggregation_internal(PD_SESS_SEC, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, per_origin_sess_weight);
}

#[test]
fn should_retrieve_aggregations_by_domain() {
    const SESSION_LENGTH_1: u64 = 10;
    const SESSION_LENGTH_2: u64 = 20;
    const SESSION_LENGTH_3: u64 = 30;
    let fe1 = "https://example1.com".to_string();
    let fe2 = "https://example2.com".to_string();
    let fe3 = "https://example3.com".to_string();
    let mut storage = test_storage();

    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: fe1.clone(),
            session_duration_ns: Duration::from_secs(SESSION_LENGTH_1).as_nanos() as u64,
        }),
    };
    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::InternetComputerOrg),
            frontend: fe2.clone(),
            session_duration_ns: Duration::from_secs(SESSION_LENGTH_2).as_nanos() as u64,
        }),
    };
    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: None,
            frontend: fe3.clone(),
            session_duration_ns: Duration::from_secs(SESSION_LENGTH_3).as_nanos() as u64,
        }),
    };
    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    let results = retrieve_aggregation_internal(PD_COUNT, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(fe1.clone(), 1)]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(fe1.clone(), SESSION_LENGTH_1)]);

    let results =
        retrieve_aggregation_internal(PD_COUNT, Day, Some(IIDomain::InternetComputerOrg), &storage);
    assert_eq!(results, vec![(fe2.clone(), 1)]);

    let results = retrieve_aggregation_internal(
        PD_SESS_SEC,
        Day,
        Some(IIDomain::InternetComputerOrg),
        &storage,
    );
    assert_eq!(results, vec![(fe2.clone(), SESSION_LENGTH_2)]);

    let results = retrieve_aggregation_internal(PD_COUNT, Month, None, &storage);
    assert_eq!(results, vec![(fe3.clone(), 1)]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Month, None, &storage);
    assert_eq!(results, vec![(fe3.clone(), SESSION_LENGTH_3)]);
}

#[test]
fn should_retrieve_aggregation_by_time_window() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(DUMMY_SESSION_LENGTH_SEC).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP + DAY_NS, &mut storage);

    let results = retrieve_aggregation_internal(PD_COUNT, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(EXAMPLE_URL.to_string(), 1)]);

    let results = retrieve_aggregation_internal(PD_SESS_SEC, Day, Some(IIDomain::Ic0App), &storage);
    assert_eq!(
        results,
        vec![(EXAMPLE_URL.to_string(), DUMMY_SESSION_LENGTH_SEC)]
    );

    let results = retrieve_aggregation_internal(PD_COUNT, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(EXAMPLE_URL.to_string(), 2)]);

    let results =
        retrieve_aggregation_internal(PD_SESS_SEC, Month, Some(IIDomain::Ic0App), &storage);
    assert_eq!(results, vec![(EXAMPLE_URL.to_string(), 1800)]);
}

fn test_storage() -> Storage<Rc<RefCell<Vec<u8>>>> {
    let memory = VectorMemory::default();
    Storage::new((1, 100), memory.clone())
}
