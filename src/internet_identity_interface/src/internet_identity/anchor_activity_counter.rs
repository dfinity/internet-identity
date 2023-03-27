use crate::internet_identity::types::{ActiveAnchorCounter, DomainActiveAnchorCounter, Timestamp};

pub trait ActivityCounter: Clone {
    fn new(start_timestamp: Timestamp) -> Self;
    fn start_timestamp(&self) -> Timestamp;
}

impl ActivityCounter for ActiveAnchorCounter {
    fn new(start_timestamp: Timestamp) -> Self {
        Self {
            start_timestamp,
            counter: 0,
        }
    }

    fn start_timestamp(&self) -> Timestamp {
        self.start_timestamp
    }
}

impl ActivityCounter for DomainActiveAnchorCounter {
    fn new(start_timestamp: Timestamp) -> Self {
        Self {
            start_timestamp,
            ic0_app_counter: 0,
            internetcomputer_org_counter: 0,
            both_ii_domains_counter: 0,
        }
    }

    fn start_timestamp(&self) -> Timestamp {
        self.start_timestamp
    }
}
