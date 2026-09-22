//! Notifications waiting for the dispatcher.
//!
//! Each app has a tenant in [`crate::admission_queue`]. Notifications are identified
//! by recipient and app-chosen ID, capped per recipient, and ordered by urgency.
// The submission endpoint and dispatcher will use this in later PRs.
#![allow(dead_code)]

use crate::admission_queue::{AdmissionQueue, QueueConfig, QueueItem, RetryPolicy};
use crate::storage::storable::application::StorableOriginSha256;
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// RFC 8030 urgency levels. Variant order sets the queue priority, highest first.
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
/// Folding assumes a wake-up makes the browser fetch all pending notifications
/// from the app. That fetch path must be implemented before enabling delivery.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PendingNotification {
    pub(crate) recipient: AnchorNumber,
    /// Chosen by the app and unique within that app and recipient.
    pub(crate) notification_id: u64,
    pub(crate) urgency: Urgency,
}

impl QueueItem for PendingNotification {
    /// Different recipients may have the same notification ID.
    type Key = (AnchorNumber, u64);

    /// Limit pending wake-ups per recipient within each app.
    type Group = AnchorNumber;

    const LANES: usize = 4;

    fn key(&self) -> Self::Key {
        (self.recipient, self.notification_id)
    }

    fn group(&self) -> Self::Group {
        self.recipient
    }

    fn lane(&self) -> usize {
        self.urgency as usize
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
    max_entries_per_tenant: 7_000,
    max_pending_per_group: 20,
    pressure_cleared_below: 8_000,
    // Stop trying to wake devices for entries older than five minutes.
    discard_after_ns: 5 * MINUTE_NS,
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
    NOTIFICATION_BACKLOG.max_entries_per_tenant < NOTIFICATION_BACKLOG.pressure_cleared_below
);

// Opening one slot must not reset the pressure timer.
const _: () =
    assert!(NOTIFICATION_BACKLOG.pressure_cleared_below < NOTIFICATION_BACKLOG.max_entries);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::admission_queue::Admission;

    fn origin(host: &str) -> StorableOriginSha256 {
        StorableOriginSha256::from_origin(&host.to_string())
    }

    fn notification(recipient: AnchorNumber, notification_id: u64) -> PendingNotification {
        PendingNotification {
            recipient,
            notification_id,
            urgency: Urgency::Normal,
        }
    }

    #[test]
    fn urgency_lanes_run_highest_first_and_stay_in_range() {
        let lanes: Vec<usize> = [
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
            .lane()
        })
        .collect();

        assert_eq!(lanes, vec![0, 1, 2, 3]);
        assert!(lanes.iter().all(|lane| *lane < PendingNotification::LANES));
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
    fn two_apps_are_two_tenants() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);

        backlog.admit(origin("https://a.example"), vec![notification(1, 1)], 1);
        let other = backlog.admit(origin("https://b.example"), vec![notification(1, 1)], 1);

        assert_eq!(other, vec![Admission::Stored]);
        assert_eq!(backlog.stats(1).active_tenants, 2);
    }

    #[test]
    fn a_recipient_folds_once_it_has_too_many_pending() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        let cap = NOTIFICATION_BACKLOG.max_pending_per_group as u64;

        let within: Vec<_> = (0..cap).map(|id| notification(1, id)).collect();
        assert!(backlog
            .admit(app.clone(), within, 1)
            .iter()
            .all(|admission| *admission == Admission::Stored));

        // The caller relies on existing wake-ups to cover this notification too.
        assert_eq!(
            backlog.admit(app, vec![notification(1, cap)], 1),
            vec![Admission::Folded]
        );
    }

    #[test]
    fn a_notification_comes_back_out_with_its_app_and_arrival() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        backlog.admit(app.clone(), vec![notification(42, 7)], 1_000);

        let taken = backlog.take_batch(10, 2_000);

        assert_eq!(taken.len(), 1);
        assert_eq!(taken[0].tenant, app);
        assert_eq!(taken[0].entry.received_at_ns, 1_000);
        assert_eq!(taken[0].entry.item, notification(42, 7));
    }

    #[test]
    fn the_more_urgent_notification_leaves_first() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let app = origin("https://a.example");
        backlog.admit(
            app.clone(),
            vec![PendingNotification {
                urgency: Urgency::VeryLow,
                ..notification(1, 1)
            }],
            1,
        );
        backlog.admit(
            app,
            vec![PendingNotification {
                urgency: Urgency::High,
                ..notification(1, 2)
            }],
            2,
        );

        let taken = backlog.take_batch(10, 3);

        assert_eq!(
            taken
                .iter()
                .map(|t| t.entry.item.notification_id)
                .collect::<Vec<_>>(),
            vec![2, 1]
        );
    }
}
