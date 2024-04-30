use crate::activity_stats::event_stats::{
    update_events_internal, Event, EventData, PrepareDelegationEvent,
};
use crate::ii_domain::IIDomain;
use crate::storage::Storage;
use crate::DAY_NS;
use ic_stable_structures::VectorMemory;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

const TIMESTAMP: u64 = 1_714_387_451 * 10u64.pow(9);
const EXAMPLE_URL: &str = "https://example.com";
const EXAMPLE_URL_2: &str = "https://other-example.com";

#[test]
fn should_store_event_and_add_to_aggregations() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 4);
    const EXPECTED_AGGREGATIONS: [&str; 4] = [
        "PD_count>24h>ic0.app>https://example.com",
        "PD_sess_sec>24h>ic0.app>https://example.com",
        "PD_count>30d>ic0.app>https://example.com",
        "PD_sess_sec>30d>ic0.app>https://example.com",
    ];
    for key in EXPECTED_AGGREGATIONS.iter() {
        assert!(storage.event_aggregations.contains_key(&key.to_string()));
    }

    assert_eq!(storage.event_data.len(), 1);
    assert_eq!(storage.event_data.get(&TIMESTAMP).unwrap(), event);
}

#[test]
fn should_track_ii_domains() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };
    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::InternetComputerOrgDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };
    let event3 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: None,
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };

    update_events_internal(event1, TIMESTAMP, &mut storage);
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);
    update_events_internal(event3, TIMESTAMP + 2, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 12); // 4 per domain
    const EXPECTED_AGGREGATIONS: [&str; 3] = [
        "PD_count>24h>ic0.app>https://example.com",
        "PD_count>24h>internetcomputer.org>https://example.com",
        "PD_count>24h>other>https://example.com",
    ];
    for key in EXPECTED_AGGREGATIONS.iter() {
        assert!(storage.event_aggregations.contains_key(&key.to_string()));
    }
}

#[test]
fn should_track_multiple_frontends() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };
    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL_2.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };

    update_events_internal(event1, TIMESTAMP, &mut storage);
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 8); // 4 per domain
    const EXPECTED_AGGREGATIONS: [&str; 2] = [
        "PD_count>24h>ic0.app>https://example.com",
        "PD_count>24h>ic0.app>https://other-example.com",
    ];
    for key in EXPECTED_AGGREGATIONS.iter() {
        assert!(storage.event_aggregations.contains_key(&key.to_string()));
    }
}

#[test]
fn should_store_multiple_events_and_aggregate_expected_weight_count() {
    let mut storage = test_storage();
    let sess_duration_secs = 900;
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(sess_duration_secs).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP + 1, &mut storage);

    assert_eq!(storage.event_data.len(), 2);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs * 2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs * 2
    );
}

#[test]
fn should_store_prune_daily_events_after_24h() {
    let mut storage = test_storage();
    let sess_duration_secs = 900;
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(sess_duration_secs).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP + DAY_NS, &mut storage);

    assert_eq!(storage.event_data.len(), 2);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        2
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs * 2
    );
}

#[test]
fn should_store_prune_monthly_events_after_30d() {
    let mut storage = test_storage();
    let sess_duration_secs = 900;
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(sess_duration_secs).as_nanos() as u64,
        }),
    };

    update_events_internal(event.clone(), TIMESTAMP, &mut storage);
    update_events_internal(event.clone(), TIMESTAMP + DAY_NS * 30, &mut storage);

    assert_eq!(storage.event_data.len(), 1);
    assert_eq!(storage.event_aggregations.len(), 4);
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_count>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        1
    );
    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>30d>ic0.app>https://example.com".to_string())
            .unwrap(),
        sess_duration_secs
    );
}

#[test]
fn should_account_for_dapps_changing_session_lifetime() {
    let mut storage = test_storage();
    let event1 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(1800).as_nanos() as u64,
        }),
    };
    update_events_internal(event1, TIMESTAMP, &mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        1800
    );

    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };
    update_events_internal(event2, TIMESTAMP + 1, &mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        2700
    );

    let event3 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(1).as_nanos() as u64,
        }),
    };
    update_events_internal(event3, TIMESTAMP + 1 + DAY_NS, &mut storage);

    assert_eq!(
        storage
            .event_aggregations
            .get(&"PD_sess_sec>24h>ic0.app>https://example.com".to_string())
            .unwrap(),
        1
    );
}

#[test]
fn should_remove_aggregations_without_events_when_pruning() {
    let mut storage = test_storage();
    let event = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };

    update_events_internal(event, TIMESTAMP, &mut storage);

    assert_eq!(storage.event_aggregations.len(), 4);
    const EXPECTED_AGGREGATIONS: [&str; 4] = [
        "PD_count>24h>ic0.app>https://example.com",
        "PD_sess_sec>24h>ic0.app>https://example.com",
        "PD_count>30d>ic0.app>https://example.com",
        "PD_sess_sec>24h>ic0.app>https://example.com",
    ];
    for key in EXPECTED_AGGREGATIONS.iter() {
        assert!(storage.event_aggregations.contains_key(&key.to_string()));
    }

    let event2 = EventData {
        event: Event::PrepareDelegation(PrepareDelegationEvent {
            ii_domain: Some(IIDomain::Ic0AppDomain),
            frontend: EXAMPLE_URL_2.to_string(),
            session_duration_ns: Duration::from_secs(900).as_nanos() as u64,
        }),
    };

    // after this update, the previous aggregations should be removed as their weight is 0
    // (this means that there is no event being counted for them)
    update_events_internal(event2, TIMESTAMP + 30 * DAY_NS, &mut storage);

    const EXPECTED_AGGREGATIONS_2: [&str; 4] = [
        "PD_count>24h>ic0.app>https://other-example.com",
        "PD_sess_sec>24h>ic0.app>https://other-example.com",
        "PD_count>30d>ic0.app>https://other-example.com",
        "PD_sess_sec>24h>ic0.app>https://other-example.com",
    ];
    assert_eq!(storage.event_aggregations.len(), 4);
    for key in EXPECTED_AGGREGATIONS_2.iter() {
        assert!(storage.event_aggregations.contains_key(&key.to_string()));
    }

    assert_eq!(storage.event_data.len(), 1);
}

fn test_storage() -> Storage<Rc<RefCell<Vec<u8>>>> {
    const DEFAULT_ID_RANGE: (u64, u64) = (1, 100);
    let memory = VectorMemory::default();
    Storage::new(DEFAULT_ID_RANGE, memory.clone())
}
