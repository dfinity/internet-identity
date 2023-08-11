use crate::activity_stats::activity_counter::ActivityCounter;
use candid::{CandidType, Deserialize};
use internet_identity_interface::internet_identity::types::Timestamp;

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActiveAnchorCounter {
    pub start_timestamp: Timestamp,
    pub counter: u64,
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

/// Increases the counter on a window if the `previous_activity_timestamp` lies before
/// the `start_timestamp` (i.e. a window where the anchor has not yet had any activity in).
///
/// For any given anchor and window the `previous_activity_timestamp` is only absent or before the
/// `window.start_timestamp` once because:
/// * `previous_activity_timestamp` of the given anchor is set to `ic_cdk::time()` in this canister call
/// * `window.start_timestamp` is <= `ic_cdk::time()` for any active window
pub fn update_active_anchor_counter(
    counter: &mut ActiveAnchorCounter,
    previous_activity_timestamp: Option<Timestamp>,
) {
    if let Some(timestamp) = previous_activity_timestamp {
        if counter.start_timestamp > timestamp {
            counter.counter += 1;
        }
    } else {
        // increase counter if there is no previous activity
        counter.counter += 1;
    }
}
