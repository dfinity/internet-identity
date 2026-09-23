//! Notifications waiting for the dispatcher.
//!
//! Each app is a sender in [`crate::admission_queue`]. Notifications are identified
//! by recipient and app-chosen ID, capped per recipient, and ordered by urgency.
// The submission endpoint and dispatcher will use this in later PRs.
#![allow(dead_code)]

use crate::admission_queue::{
    AdmissionQueue, Entry, QueueConfig, QueueItem, QueueSnapshot, RetryPolicy,
};
use crate::state::storage_borrow_mut;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::backlog::{StorableBacklogEntry, StorableBacklogKey};
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

/// RFC 8030 urgency levels. Variant order sets the queue priority, highest first,
/// which also decides how many turns each level gets when notifications are taken.
/// The dispatcher must convert these to the corresponding HTTP header strings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Urgency {
    High,
    Normal,
    Low,
    VeryLow,
}

/// A request to wake a recipient's devices, without notification content.
///
/// One wake-up carries one notification, so nothing stands in for a notification
/// above a recipient's cap. The app has to send it again once the cap frees up.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PendingNotification {
    pub(crate) recipient: AnchorNumber,
    /// Chosen by the app and unique within that app and recipient.
    pub(crate) notification_id: u64,
    pub(crate) urgency: Urgency,
    /// When the app stops wanting this delivered. `None` leaves
    /// `discard_entries_after_ns` in charge, and a later time is clamped to it.
    pub(crate) expires_at_ns: Option<Timestamp>,
}

impl Urgency {
    /// The level a priority index names, saturating at the least urgent. Reads back
    /// what [`QueueItem::priority`] wrote once an entry has left the queue.
    pub(crate) fn at_priority(priority: usize) -> Self {
        match priority {
            0 => Urgency::High,
            1 => Urgency::Normal,
            2 => Urgency::Low,
            _ => Urgency::VeryLow,
        }
    }
}

impl QueueItem for PendingNotification {
    /// Different recipients may have the same notification ID.
    type Key = (AnchorNumber, u64);

    /// Limit pending wake-ups per recipient within each app.
    type Group = AnchorNumber;

    const PRIORITY_LEVELS: usize = 4;

    fn key(&self) -> Self::Key {
        (self.recipient, self.notification_id)
    }

    fn group(&self) -> Self::Group {
        self.recipient
    }

    fn priority(&self) -> usize {
        self.urgency as usize // Converts the enum variant order to a priority number, highest first.
    }

    fn expires_at_ns(&self) -> Option<Timestamp> {
        self.expires_at_ns
    }
}

/// Pending notifications grouped by the app that sent them.
///
/// The caller must authorize the app and hash its canonical origin after the
/// legacy-domain remap. Otherwise, aliases such as `https://app.icp0.io` and
/// `https://app.ic0.app` would receive separate queue allowances.
pub(crate) type NotificationBacklog = AdmissionQueue<StorableOriginSha256, PendingNotification>;

const SECOND_NS: u64 = 1_000_000_000;
const MINUTE_NS: u64 = 60 * SECOND_NS;

pub(crate) const NOTIFICATION_BACKLOG: QueueConfig = QueueConfig {
    max_entries: 10_000,
    // One app may not use every slot.
    max_entries_per_sender: 7_000,
    max_pending_per_group: 20,
    pressure_cleared_below: 8_000,
    // Stop trying to wake devices for entries older than five minutes.
    discard_entries_after_ns: 5 * MINUTE_NS,
    // An app resending keeps a notification alive, but never beyond this.
    max_lifetime_ns: 15 * MINUTE_NS,
    retry: RetryPolicy {
        base_ms: 5_000,
        ceiling_ms: 300_000,
        when_stalled_ms: 600_000,
        doubles_every_ns: 30 * SECOND_NS,
        max_doublings: 6,
        // Allow one minute without a successful take before reporting a stall.
        stalled_after_silence_ns: MINUTE_NS,
    },
};

// Reject invalid limits at compile time.
const _: () = assert!(NOTIFICATION_BACKLOG.is_coherent());

// One app alone must stay below the pressure reset threshold.
const _: () = assert!(
    NOTIFICATION_BACKLOG.max_entries_per_sender < NOTIFICATION_BACKLOG.pressure_cleared_below
);

// Opening one slot must not reset the pressure timer.
const _: () =
    assert!(NOTIFICATION_BACKLOG.pressure_cleared_below < NOTIFICATION_BACKLOG.max_entries);

/// Writes every queued notification into stable memory for the upgrade to carry.
///
/// The rotation is left behind: a sender's place in the turn order is worth less than
/// the code to carry it, and restarting costs a sender at most one turn. The work
/// itself is kept, because an app told its notification was accepted will not send it
/// again.
pub(crate) fn persist(backlog: &NotificationBacklog) {
    storage_borrow_mut(|storage| {
        for (origin, entry) in backlog.entries() {
            storage.add_backlog_notification(
                StorableBacklogKey {
                    origin: origin.clone(),
                    expires_at_ns: entry.expires_at_ns,
                    recipient: entry.item.recipient,
                    notification_id: entry.item.notification_id,
                },
                StorableBacklogEntry {
                    received_at_ns: entry.received_at_ns,
                    urgency: entry.item.urgency as u8,
                    app_expires_at_ns: entry.item.expires_at_ns,
                },
            );
        }
    });
}

/// Reads back what [`persist`] wrote and empties the map. `None` when nothing was
/// parked, which is every install and every upgrade that found the queue empty.
///
/// Arrival times and deadlines come back as they were, so an entry keeps the life it
/// had rather than starting over. Retry clocks restart at `now_ns`, since the silence
/// a sender should back off from is silence this canister is responsible for.
pub(crate) fn restore(now_ns: Timestamp) -> Option<NotificationBacklog> {
    let parked = storage_borrow_mut(|storage| storage.drain_backlog_notifications());
    if parked.is_empty() {
        return None;
    }

    // Rows arrive grouped by sender, since the origin hash leads the key.
    let mut by_sender: Vec<(StorableOriginSha256, Vec<Entry<PendingNotification>>)> = Vec::new();
    for (key, stored) in parked {
        let entry = Entry {
            received_at_ns: stored.received_at_ns,
            expires_at_ns: key.expires_at_ns,
            item: PendingNotification {
                recipient: key.recipient,
                notification_id: key.notification_id,
                urgency: Urgency::at_priority(stored.urgency as usize),
                expires_at_ns: stored.app_expires_at_ns,
            },
        };
        match by_sender.last_mut() {
            Some((origin, entries)) if *origin == key.origin => entries.push(entry),
            _ => by_sender.push((key.origin, vec![entry])),
        }
    }

    Some(NotificationBacklog::restore(
        NOTIFICATION_BACKLOG,
        QueueSnapshot::from_entries(by_sender),
        now_ns,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admission_queue::{Admission, Admitted, Taken};
    use crate::notifications::test_setup;

    /// The outcomes without their keys, for assertions that only check what happened.
    fn results<Key>(admitted: Vec<Admitted<Key>>) -> Vec<Admission> {
        admitted
            .into_iter()
            .map(|answer| answer.admission)
            .collect()
    }

    fn origin(host: &str) -> StorableOriginSha256 {
        StorableOriginSha256::from_origin(&host.to_string())
    }

    fn notification(recipient: AnchorNumber, notification_id: u64) -> PendingNotification {
        PendingNotification {
            recipient,
            notification_id,
            urgency: Urgency::Normal,
            expires_at_ns: None,
        }
    }

    fn at_urgency(notification_id: u64, urgency: Urgency) -> PendingNotification {
        PendingNotification {
            urgency,
            ..notification(1, notification_id)
        }
    }

    #[test]
    fn urgency_priorities_run_highest_first_and_stay_in_range() {
        let priorities: Vec<usize> = [
            Urgency::High,
            Urgency::Normal,
            Urgency::Low,
            Urgency::VeryLow,
        ]
        .iter()
        .map(|urgency| {
            PendingNotification {
                urgency: *urgency,
                ..notification(1, 1)
            }
            .priority()
        })
        .collect();

        assert_eq!(priorities, vec![0, 1, 2, 3]);
        assert!(priorities
            .iter()
            .all(|priority| *priority < PendingNotification::PRIORITY_LEVELS));
    }

    #[test]
    fn one_id_reused_across_recipients_stays_two_notifications() {
        assert_ne!(notification(1, 7).key(), notification(2, 7).key());
    }

    #[test]
    fn a_recipients_notifications_share_a_group() {
        assert_eq!(notification(1, 7).group(), notification(1, 8).group());
        assert_ne!(notification(1, 7).group(), notification(2, 7).group());
    }

    #[test]
    fn two_apps_are_two_senders() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);

        backlog.admit(origin("https://a.example"), vec![notification(1, 1)], 1);
        let other =
            results(backlog.admit(origin("https://b.example"), vec![notification(1, 1)], 1));

        assert_eq!(other, vec![Admission::Accepted]);
        assert_eq!(backlog.stats(1).active_senders, 2);
    }

    #[test]
    fn a_recipient_past_the_cap_sends_the_app_away_to_retry() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        let cap = NOTIFICATION_BACKLOG.max_pending_per_group as u64;

        let within: Vec<_> = (0..cap).map(|id| notification(1, id)).collect();
        assert!(results(backlog.admit(app.clone(), within, 1))
            .iter()
            .all(|admission| *admission == Admission::Accepted));

        assert_eq!(
            results(backlog.admit(app.clone(), vec![notification(1, cap)], 1)),
            vec![Admission::Full {
                retry_after_ms: NOTIFICATION_BACKLOG.retry.base_ms
            }]
        );

        // Taking one frees the recipient's slot, so the retry lands.
        take(&mut backlog, 1, 2);
        assert_eq!(
            results(backlog.admit(app, vec![notification(1, cap)], 2)),
            vec![Admission::Accepted]
        );
    }

    #[test]
    fn a_notification_comes_back_out_with_its_app_and_arrival() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        backlog.admit(app.clone(), vec![notification(42, 7)], 1_000);

        let taken = take(&mut backlog, 10, 2_000);

        assert_eq!(taken.len(), 1);
        assert_eq!(taken[0].sender, app);
        assert_eq!(taken[0].entry.received_at_ns, 1_000);
        assert_eq!(taken[0].entry.expires_at_ns, 1_000 + 5 * MINUTE_NS);
        assert_eq!(taken[0].entry.item, notification(42, 7));
    }

    #[test]
    fn the_more_urgent_notification_leaves_first() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        backlog.admit(app.clone(), vec![at_urgency(1, Urgency::VeryLow)], 1);
        backlog.admit(app, vec![at_urgency(2, Urgency::High)], 2);

        assert_eq!(ids_taken(&mut backlog, 10, 3), vec![2, 1]);
    }

    #[test]
    fn urgency_levels_take_turns_rather_than_draining_in_order() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let queued: Vec<_> = (1..=6)
            .map(|id| at_urgency(id, Urgency::High))
            .chain((7..=8).map(|id| at_urgency(id, Urgency::Normal)))
            .collect();
        backlog.admit(origin("https://a.example"), queued, 1);

        // Four levels, so High runs four turns and hands over to Normal with work left.
        assert_eq!(ids_taken(&mut backlog, 10, 2), vec![1, 2, 3, 4, 7, 8, 5, 6]);
    }

    #[test]
    fn an_app_may_retire_a_notification_early() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        backlog.admit(
            origin("https://a.example"),
            vec![PendingNotification {
                expires_at_ns: Some(30 * SECOND_NS),
                ..notification(1, 1)
            }],
            0,
        );

        // Well inside the five minutes the queue would otherwise allow.
        assert!(take(&mut backlog, 10, 31 * SECOND_NS).is_empty());
    }

    /// Takes a batch this caller always keeps.
    fn take(
        backlog: &mut NotificationBacklog,
        limit: usize,
        now_ns: u64,
    ) -> Vec<Taken<StorableOriginSha256, PendingNotification>> {
        backlog
            .take_batch(limit, now_ns, |batch| Ok::<_, ()>(batch.to_vec()))
            .unwrap()
    }

    /// Everything a persisted entry has to come back with, in the order it would be
    /// sent, so a difference shows up as a difference in what leaves the queue.
    fn round_trip(
        backlog: &NotificationBacklog,
        now_ns: Timestamp,
    ) -> Vec<Taken<StorableOriginSha256, PendingNotification>> {
        persist(backlog);
        let mut restored = restore(now_ns).expect("a queue that held something");
        take(&mut restored, 1_000, now_ns)
    }

    #[test]
    fn a_queued_notification_comes_back_with_its_app_arrival_and_deadline() {
        test_setup();
        let app = origin("https://a.example");
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        backlog.admit(
            app.clone(),
            vec![PendingNotification {
                expires_at_ns: Some(90 * SECOND_NS),
                ..notification(42, 7)
            }],
            1_000,
        );

        let taken = round_trip(&backlog, 2_000);

        assert_eq!(taken.len(), 1);
        assert_eq!(taken[0].sender, app);
        assert_eq!(taken[0].entry.received_at_ns, 1_000);
        assert_eq!(taken[0].entry.expires_at_ns, 90 * SECOND_NS);
        assert_eq!(
            taken[0].entry.item,
            PendingNotification {
                expires_at_ns: Some(90 * SECOND_NS),
                ..notification(42, 7)
            }
        );
    }

    /// Senders share the queue by taking turns, so the restored queue has to hand out
    /// one per sender rather than draining whoever happened to be read back first.
    #[test]
    fn senders_still_take_turns_after_a_restore() {
        test_setup();
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        for (host, ids) in [("https://a.example", [1, 2]), ("https://b.example", [3, 4])] {
            let items = ids.iter().map(|id| notification(1, *id)).collect();
            backlog.admit(origin(host), items, 1);
        }

        let senders: Vec<StorableOriginSha256> = round_trip(&backlog, 2)
            .iter()
            .map(|taken| taken.sender.clone())
            .collect();

        // Which app leads is its hash's place among the senders, so the property is
        // that neither goes twice running, not which one is first.
        assert_eq!(senders.len(), 4);
        assert_ne!(senders[0], senders[1]);
        assert_ne!(senders[1], senders[2]);
        assert_ne!(senders[2], senders[3]);
    }

    /// Urgency decides the order entries leave in, so a level that came back wrong
    /// would reorder the queue.
    #[test]
    fn urgency_survives_the_round_trip() {
        test_setup();
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let queued = [
            Urgency::VeryLow,
            Urgency::Low,
            Urgency::Normal,
            Urgency::High,
        ]
        .iter()
        .enumerate()
        .map(|(index, urgency)| at_urgency(index as u64, *urgency))
        .collect();
        backlog.admit(origin("https://a.example"), queued, 1);

        let urgencies: Vec<Urgency> = round_trip(&backlog, 2)
            .iter()
            .map(|taken| taken.entry.item.urgency)
            .collect();

        assert_eq!(
            urgencies,
            vec![
                Urgency::High,
                Urgency::Normal,
                Urgency::Low,
                Urgency::VeryLow
            ]
        );
    }

    /// The rows exist for one upgrade. A second restore must not resurrect them, or
    /// every upgrade would replay whatever the last one carried.
    #[test]
    fn a_restore_empties_what_it_read() {
        test_setup();
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        backlog.admit(origin("https://a.example"), vec![notification(1, 1)], 1);

        persist(&backlog);
        assert!(restore(2).is_some());

        assert!(restore(2).is_none());
    }

    /// An install has nothing parked, and neither has an upgrade that caught the
    /// queue empty, so both have to read as "no queue" rather than an empty one.
    #[test]
    fn nothing_parked_restores_to_no_queue() {
        test_setup();

        assert!(restore(1).is_none());

        persist(&NotificationBacklog::new(NOTIFICATION_BACKLOG, 0));
        assert!(restore(1).is_none());
    }

    fn ids_taken(backlog: &mut NotificationBacklog, limit: usize, now_ns: u64) -> Vec<u64> {
        take(backlog, limit, now_ns)
            .iter()
            .map(|taken| taken.entry.item.notification_id)
            .collect()
    }
}
