//! What a flush decides, with no `await` in sight: the canister layer builds a
//! batch, makes the call, and hands the answer back here.

use super::ii;
use super::store::{Entry, Event, Store};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Outcome {
    /// Sent, with whatever Internet Identity would not take.
    Answered(Vec<ii::NotAccepted>),
    /// The batch was too large, and nothing in it was enqueued.
    TooMany(usize),
    /// The origin does not publish this canister as one of its senders.
    NotAuthorized,
    /// The call itself failed.
    Unreachable,
}

pub fn arg(origin: &str, batch: &[Entry]) -> ii::SendNotificationArg {
    ii::SendNotificationArg {
        origin: origin.to_string(),
        notifications: batch
            .iter()
            .map(|entry| ii::Notification {
                id: entry.id,
                recipient: entry.recipient,
                expires_at: Some(entry.expires_at),
                urgency: Some(urgency_of(entry.lane)),
            })
            .collect(),
    }
}

/// Applies an outcome to the batch that produced it.
pub fn apply(store: &mut Store, batch: Vec<Entry>, outcome: Outcome, now: u64) {
    match outcome {
        Outcome::Answered(not_accepted) => {
            let refused: HashMap<u64, ii::NotAcceptedReason> = not_accepted
                .iter()
                .map(|entry| (entry.id, entry.reason))
                .collect();

            for entry in batch {
                match refused.get(&entry.id) {
                    None => {
                        store.record_for(&entry, Event::Accepted, 1);
                        store.settle(&entry);
                    }
                    Some(ii::NotAcceptedReason::Deferred { retry_after }) => {
                        store.record_for(&entry, Event::Deferred, 1);
                        store.defer(entry, *retry_after);
                    }
                    // No such recipient, or no channel: the user fixes both,
                    // and has until the notification expires to do it.
                    Some(_) => {
                        if !store.park(entry, now) {
                            store.record_for(&entry, Event::Dropped, 1);
                        }
                    }
                }
            }
        }

        Outcome::Unreachable => {
            for entry in batch {
                if !store.park(entry, now) {
                    store.record(now, Event::Dropped, 1);
                }
            }
        }

        Outcome::TooMany(_) | Outcome::NotAuthorized => {
            for entry in batch {
                store.requeue(entry, now);
            }
        }
    }
}

fn urgency_of(lane: u8) -> ii::Urgency {
    match lane {
        0 => ii::Urgency::High,
        1 => ii::Urgency::Normal,
        2 => ii::Urgency::Low,
        _ => ii::Urgency::VeryLow,
    }
}
