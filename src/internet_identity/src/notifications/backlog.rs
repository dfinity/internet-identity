//! Notifications waiting for the dispatcher.
//!
//! Each app is a sender in [`crate::admission_queue`]. Notifications are identified
//! by recipient and app-chosen ID, capped per recipient, and ordered by urgency.
// The submission endpoint and dispatcher will use this in later PRs.
#![allow(dead_code)]

use crate::admission_queue::{AdmissionQueue, QueueConfig, QueueItem, RetryPolicy};
use crate::storage::storable::application::StorableOriginSha256;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admission_queue::{Admission, Admitted};

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
        backlog.take_batch(1, 2);
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

        let taken = backlog.take_batch(10, 2_000);

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
        assert!(backlog.take_batch(10, 31 * SECOND_NS).is_empty());
    }

    fn ids_taken(backlog: &mut NotificationBacklog, limit: usize, now_ns: u64) -> Vec<u64> {
        backlog
            .take_batch(limit, now_ns)
            .iter()
            .map(|taken| taken.entry.item.notification_id)
            .collect()
    }
}
