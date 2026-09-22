//! Everything II has accepted and not yet handed to the processing queue.
//!
//! The mechanism is [`crate::admission_queue`], which is generic
//! and knows nothing about notifications. This is the notification half of that
//! contract: what an item is, how the queue should identify and order it, and the
//! sizes II runs at.
// Nothing calls this yet. The entrypoint and the dispatcher that do arrive in
// later PRs of this stack.
#![allow(dead_code)]

use crate::admission_queue::{AdmissionQueue, QueueConfig, QueueItem, RetryPolicy};
use crate::storage::storable::application::StorableOriginSha256;
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// The four levels of RFC 8030 `Urgency`, highest first so the variant index is the
/// lane to drain first. Kept identical to the header value the relay is given, so
/// nothing downstream has to translate it.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Urgency {
    High,
    Normal,
    Low,
    VeryLow,
}

/// A notification II has accepted and not yet handed on.
///
/// Content-free: this says who to wake and for which app, never what the
/// notification is about. The worker fetches that from the app itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PendingNotification {
    pub recipient: AnchorNumber,
    /// Chosen by the app. Unique only within that app and that recipient.
    pub notification_id: u64,
    pub urgency: Urgency,
}

impl QueueItem for PendingNotification {
    /// The id alone would collide: an app numbering from one per user reuses the
    /// same id across recipients.
    type Key = (AnchorNumber, u64);

    /// Capped per recipient, since each pending notification becomes one outcall per
    /// device, and the first wake-up to land already makes the worker pull the rest.
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

/// Everything II has accepted and not yet handed to the processing queue, keyed by
/// the app that sent it.
///
/// The tenant is the SHA-256 of the *canonical* origin, hashed by the caller after
/// the legacy-domain remap. This type cannot tell `https://app.icp0.io` from
/// `https://app.ic0.app`, so hashing before the remap would give one app two tenants
/// and two shares of the queue.
pub type NotificationBacklog = AdmissionQueue<StorableOriginSha256, PendingNotification>;

const SECOND_NS: u64 = 1_000_000_000;
const MINUTE_NS: u64 = 60 * SECOND_NS;

pub const NOTIFICATION_BACKLOG: QueueConfig = QueueConfig {
    max_entries: 10_000,
    // Below the ceiling, so an app that signs up later finds room rather than a
    // queue one broadcast has already filled.
    max_entries_per_tenant: 7_000,
    max_pending_per_group: 20,
    pressure_cleared_below: 8_000,
    // How long a notification is still worth waking a device for.
    discard_after_ns: 5 * MINUTE_NS,
    retry: RetryPolicy {
        base_ms: 5_000,
        ceiling_ms: 300_000,
        when_stalled_ms: 600_000,
        doubles_every_ns: 30 * SECOND_NS,
        max_doublings: 6,
        // The dispatcher fires every 2s, so this is thirty missed turns. It moves if
        // that interval does.
        stalled_after_silence_ns: MINUTE_NS,
    },
};

// Asserted at compile time rather than in a test: these are constants, so a test
// would be optimized away and the build should refuse an incoherent config.
const _: () = assert!(NOTIFICATION_BACKLOG.is_coherent());

// One app must not be able to fill the queue, or an app signing up later finds
// nothing free.
const _: () = assert!(
    NOTIFICATION_BACKLOG.max_entries_per_tenant < NOTIFICATION_BACKLOG.pressure_cleared_below
);

// Pressure has to clear below the ceiling, or the retry hint flaps at the boundary.
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

        // The wake-ups already queued will make the worker pull this one too, so it
        // is covered rather than refused.
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
