//! Move backlog entries into processing on a timer, keeping this work off app calls.
//! Each tick transfers a bounded batch synchronously so failed stores can be rolled back.

use crate::notifications::{notifications_enabled, processing};
use crate::state;
use ic_cdk_timers::set_timer_interval;
use internet_identity_interface::internet_identity::types::Timestamp;
use std::time::Duration;

/// Tick before the backlog's one-minute stall threshold.
pub(crate) const INTERVAL: Duration = Duration::from_secs(10);

pub(crate) const MAX_PER_TICK: usize = 200;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) struct Tick {
    pub(crate) discarded: usize,
    pub(crate) moved: usize,
}

/// Arm on install and upgrade; upgrades clear existing timers.
pub(crate) fn start() {
    set_timer_interval(INTERVAL, || {
        tick(ic_cdk::api::time());
    });
}

/// Sweep expired processing entries, then transfer a batch that fits.
/// A full processing queue leaves the backlog stall timer running.
pub(crate) fn tick(now_ns: Timestamp) -> Tick {
    if !notifications_enabled() {
        return Tick::default();
    }

    let discarded = processing::discard_expired(now_ns);
    let limit = processing::free_space().min(MAX_PER_TICK);
    let stored = state::notification_backlog_mut(now_ns, |backlog| {
        backlog.take_batch(limit, now_ns, |batch| {
            processing::store_batch(batch).map(|()| batch.len())
        })
    });

    match stored {
        Ok(moved) => Tick { discarded, moved },
        Err(does_not_fit) => {
            ic_cdk::println!(
                "Notification tick took {} with room for {}, leaving them queued",
                does_not_fit.batch,
                does_not_fit.free
            );
            Tick {
                discarded,
                moved: 0,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::backlog::PendingNotification;
    use crate::notifications::test_setup as setup;
    use crate::storage::storable::application::StorableOriginSha256;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use internet_identity_interface::internet_identity::types::Urgency;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";

    fn enable() {
        crate::state::persistent_state_mut(|s| {
            s.notifications_enabled_origins = Some([APP.to_string()].into_iter().collect());
        });
    }

    fn submit(
        anchor_number: AnchorNumber,
        ids: impl IntoIterator<Item = u64>,
        expires_at_ns: Option<Timestamp>,
        now_ns: Timestamp,
    ) {
        let items: Vec<_> = ids
            .into_iter()
            .map(|notification_id| PendingNotification {
                recipient: candid::Principal::from_slice(&anchor_number.to_be_bytes()),
                anchor_number,
                notification_id,
                urgency: Urgency::Normal,
                expires_at_ns,
            })
            .collect();
        state::notification_backlog_mut(now_ns, |backlog| {
            backlog.admit(
                StorableOriginSha256::from_origin(&APP.to_string()),
                items,
                now_ns,
            );
        });
    }

    fn still_queued(now_ns: Timestamp) -> usize {
        state::notification_backlog_mut(now_ns, |backlog| backlog.stats(now_ns).stored_total)
    }

    #[test]
    fn a_tick_moves_what_apps_queued_into_the_processing_queue() {
        setup();
        enable();
        submit(1, 1..=3, None, 1);

        assert_eq!(
            tick(2),
            Tick {
                discarded: 0,
                moved: 3
            }
        );

        assert_eq!(still_queued(2), 0);
        assert_eq!(processing::next_due(10, 2).len(), 3);
    }

    #[test]
    fn an_empty_backlog_ticks_to_nothing() {
        setup();
        enable();

        assert_eq!(tick(1), Tick::default());
    }

    #[test]
    fn a_tick_does_nothing_while_notifications_are_off() {
        setup();
        enable();
        submit(1, 1..=3, None, 1);
        crate::state::persistent_state_mut(|s| s.notifications_enabled_origins = None);

        assert_eq!(tick(2), Tick::default());

        assert_eq!(still_queued(2), 3);
    }

    #[test]
    fn a_tick_moves_no_more_than_one_messages_worth() {
        setup();
        enable();
        let queued = MAX_PER_TICK + 20;
        // Spread entries across recipients to stay within the per-recipient cap.
        for recipient in 0..queued as u64 / 20 {
            submit(recipient, 1..=20, None, 1);
        }

        assert_eq!(tick(2).moved, MAX_PER_TICK);
        assert_eq!(still_queued(2), queued - MAX_PER_TICK);

        assert_eq!(tick(3).moved, queued - MAX_PER_TICK);
    }

    #[test]
    fn a_full_processing_queue_leaves_the_backlog_where_it_is() {
        setup();
        enable();
        for recipient in 0..processing::MAX_ENTRIES / 20 {
            submit(recipient, 1..=20, None, 1);
        }
        while tick(2).moved > 0 {}
        assert_eq!(processing::free_space(), 0);

        submit(9_999, 1..=5, None, 3);
        assert_eq!(tick(4), Tick::default());

        assert_eq!(still_queued(4), 5);
    }

    #[test]
    fn a_tick_drops_what_expired_while_it_waited_to_be_sent() {
        setup();
        enable();
        submit(1, [1], Some(100), 1);
        assert_eq!(tick(2).moved, 1);

        assert_eq!(
            tick(150),
            Tick {
                discarded: 1,
                moved: 0
            }
        );

        assert!(processing::next_due(10, 150).is_empty());
    }
}
