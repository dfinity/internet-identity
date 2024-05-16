use crate::ii_domain::IIDomain;
use crate::state::persistent_state;
use crate::stats::event_stats::event_aggregations::AggregationWindow::Month;
use crate::stats::event_stats::event_aggregations::{
    AggregationKey, AggregationKind, AggregationWindow,
};
use crate::stats::event_stats::Event::PruneEvent;
use crate::stats::event_stats::{
    update_events_internal, Event, EventData, EventKey, PrepareDelegationEvent,
};
use crate::storage::Storage;
use crate::DAY_NS;
use ic_stable_structures::VectorMemory;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;
use AggregationKind::{PrepareDelegationCount, PrepareDelegationSessionSeconds, PruneEventCount};
use AggregationWindow::Day;

const TIMESTAMP: u64 = 1_714_387_451 * 10u64.pow(9);
const SESS_DURATION_SEC: u64 = 900;
const EXAMPLE_URL: &str = "https://example.com";
const EXAMPLE_URL_2: &str = "https://other-example.com";

#[test]
fn should_store_event_and_add_to_aggregations() {
    let mut storage = test_storage();
    let domain: Option<IIDomain> = Some(IIDomain::Ic0App);
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: domain.clone(),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 4);
    let expected_aggregations: [AggregationKey; 4] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Day,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Month,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Month,
            domain,
            EXAMPLE_URL.to_string(),
        ),
    ];
    for key in expected_aggregations.iter() {
        assert!(storage.event_aggregations.contains_key(key));
    }

    assert_eq!(storage.event_data.len(), 1);
    assert_eq!(
        storage
            .event_data
            .get(&EventKey::min_key(TIMESTAMP))
            .unwrap(),
        event
    );

    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_store_multiple_events_with_same_timestamp() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    assert_eq!(storage.event_data.len(), 2);
    storage.event_data.iter().for_each(|(key, value)| {
        assert_eq!(key.time, TIMESTAMP);
        assert_eq!(value, event.clone());
    });
    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_track_ii_domains() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };
    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::InternetComputerOrg),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };
    let event3 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: None,
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event1, TIMESTAMP, &mut storage);
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);
    update_events_internal(event3, TIMESTAMP + 2, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 12); // 4 per domain
    let expected_aggregations: [AggregationKey; 3] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::InternetComputerOrg),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(PrepareDelegationCount, Day, None, EXAMPLE_URL.to_string()),
    ];
    for key in expected_aggregations.iter() {
        assert!(storage.event_aggregations.contains_key(key));
    }
    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_track_multiple_frontends() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };
    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL_2.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event1, TIMESTAMP, &mut storage);
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 8); // 4 per domain
    let expected_aggregations: [AggregationKey; 2] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL_2.to_string(),
        ),
    ];
    for key in expected_aggregations.iter() {
        assert!(storage.event_aggregations.contains_key(key));
    }
    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_store_multiple_events_and_aggregate_expected_weight_count() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP + 1, &mut storage);

    assert_eq!(storage.event_data.len(), 2);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC * 2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC * 2
    );
    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_prune_daily_events_after_24h() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    assert_event_count_consistent(&mut storage);
    update_events_internal(event.clone(), TIMESTAMP + DAY_NS, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(storage.event_data.len(), 2);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC * 2
    );
}

#[test]
fn should_prune_monthly_events_after_30d() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    assert_event_count_consistent(&mut storage);
    update_events_internal(event.clone(), TIMESTAMP + DAY_NS * 30, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(storage.event_data.len(), 1);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationCount,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Month,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        SESS_DURATION_SEC
    );
}

#[test]
fn should_account_for_dapps_changing_session_lifetime() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(2 * SESS_DURATION_SEC),
        }),
    };
    update_events_internal(event1, TIMESTAMP, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        1800
    );

    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        2700
    );

    let event3 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(1).as_nanos() as u64,
        }),
    };
    update_events_internal(event3, TIMESTAMP + 1 + DAY_NS, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&AggregationKey::new(
                PrepareDelegationSessionSeconds,
                Day,
                Some(IIDomain::Ic0App),
                EXAMPLE_URL.to_string()
            ))
            .unwrap(),
        1
    );
}

#[test]
fn should_remove_aggregations_without_events_when_pruning() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(event, TIMESTAMP, &mut storage);
    assert_event_count_consistent(&mut storage);

    assert_eq!(storage.event_aggregations.len(), 4);
    let expected_aggregations: [AggregationKey; 4] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Month,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Month,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL.to_string(),
        ),
    ];
    for key in expected_aggregations.iter() {
        assert!(storage.event_aggregations.contains_key(key));
    }

    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0App),
            frontend: EXAMPLE_URL_2.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    // after this update, the previous aggregations should be removed as their weight is 0
    // (this means that there is no event being counted for them)
    update_events_internal(event2, TIMESTAMP + 30 * DAY_NS, &mut storage);
    assert_event_count_consistent(&mut storage);

    let expected_aggregations_2: [AggregationKey; 4] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL_2.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Day,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL_2.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Month,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL_2.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Month,
            Some(IIDomain::Ic0App),
            EXAMPLE_URL_2.to_string(),
        ),
    ];
    assert_eq!(storage.event_aggregations.len(), 4);
    for key in expected_aggregations_2.iter() {
        assert!(storage.event_aggregations.contains_key(key));
    }

    assert_eq!(storage.event_data.len(), 1);
}

#[test]
fn should_ignore_prune_events_for_prepare_delegation_aggregations() {
    let mut storage = test_storage();
    let domain: Option<IIDomain> = Some(IIDomain::Ic0App);
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: domain.clone(),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: to_ns(SESS_DURATION_SEC),
        }),
    };

    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);
    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);

    let sess_sec_aggregations: [AggregationKey; 2] = [
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Day,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationSessionSeconds,
            Month,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
    ];
    for key in sess_sec_aggregations.iter() {
        assert_eq!(
            storage.event_aggregations.get(key).unwrap(),
            SESS_DURATION_SEC
        );
    }
    let pd_count_aggregations: [AggregationKey; 2] = [
        AggregationKey::new(
            PrepareDelegationCount,
            Day,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
        AggregationKey::new(
            PrepareDelegationCount,
            Month,
            domain.clone(),
            EXAMPLE_URL.to_string(),
        ),
    ];
    for key in pd_count_aggregations.iter() {
        assert_eq!(storage.event_aggregations.get(key).unwrap(), 1);
    }
    assert_event_count_consistent(&mut storage);
}

#[test]
fn should_add_prune_events_to_prune_aggregations() {
    let mut storage = test_storage();

    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);
    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);
    update_events_internal(EventData { event: PruneEvent }, TIMESTAMP, &mut storage);

    let sess_sec_aggregations: [AggregationKey; 2] = [
        AggregationKey::new(PruneEventCount, Day, None, "".to_string()),
        AggregationKey::new(PruneEventCount, Month, None, "".to_string()),
    ];
    for key in sess_sec_aggregations.iter() {
        assert_eq!(storage.event_aggregations.get(key).unwrap(), 3);
    }
    assert_event_count_consistent(&mut storage);
}

/// Make sure the cached count values are consistent with the actual data
fn assert_event_count_consistent(storage: &mut Storage<Rc<RefCell<Vec<u8>>>>) {
    assert_eq!(
        storage.event_data.iter().count(),
        persistent_state(|s| s.event_data_count) as usize
    );
    assert_eq!(
        storage.event_aggregations.iter().count(),
        persistent_state(|s| s.event_aggregations_count) as usize
    );
}

fn to_ns(secs: u64) -> u64 {
    Duration::from_secs(secs).as_nanos() as u64
}

fn test_storage() -> Storage<Rc<RefCell<Vec<u8>>>> {
    const DEFAULT_ID_RANGE: (u64, u64) = (1, 100);
    let memory = VectorMemory::default();
    Storage::new(DEFAULT_ID_RANGE, memory.clone())
}
