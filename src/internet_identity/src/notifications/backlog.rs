//! Pending device wake-ups, grouped by app and limited per identity.
//! Deduplication uses (recipient, notification ID); urgency sets the lane.
// Used by the submission endpoint and dispatcher in follow-up PRs.
#![allow(dead_code)]

use super::admission_queue::{AdmissionQueue, QueueConfig, QueueItem, RetryPolicy};
use super::dispatch;
use crate::storage::storable::application::StorableOriginSha256;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationNumber, NotificationId, Timestamp, Urgency,
};

/// One device wake-up request per notification, without notification content.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PendingNotification {
    /// The principal the app addressed, one of possibly several for an identity.
    pub(crate) recipient: Principal,
    /// The identity behind `recipient`, whose browsers are woken.
    pub(crate) anchor_number: AnchorNumber,
    /// II's number for the app's origin, which the service worker is told.
    pub(crate) application_number: ApplicationNumber,
    /// The account at the app `recipient` is. `None` is the unreserved default account.
    pub(crate) account_number: Option<AccountNumber>,
    /// The app canister that sent it, which the service worker fetches it from.
    pub(crate) sender: Principal,
    /// App-chosen ID, unique per recipient within the app.
    pub(crate) notification_id: NotificationId,
    pub(crate) urgency: Urgency,
    /// Optional app deadline, capped by the queue expiry limit.
    pub(crate) expires_at_ns: Option<Timestamp>,
}

impl QueueItem for PendingNotification {
    type Key = (Principal, NotificationId);

    type Group = AnchorNumber;

    const TURNS: &'static [usize] = &[4, 3, 2, 1];

    fn key(&self) -> Self::Key {
        (self.recipient, self.notification_id)
    }

    fn group(&self) -> Self::Group {
        self.anchor_number
    }

    /// Lanes run most urgent first, which is the reverse of how the levels are
    /// declared. Written out rather than derived from the discriminant, so adding a
    /// level is a compile error here instead of a silent renumbering.
    fn lane(&self) -> usize {
        match self.urgency {
            Urgency::High => 0,
            Urgency::Normal => 1,
            Urgency::Low => 2,
            Urgency::VeryLow => 3,
        }
    }

    fn expires_at_ns(&self) -> Option<Timestamp> {
        self.expires_at_ns
    }
}

/// Callers must authorize the app and hash its canonical origin after legacy-domain
/// remapping so aliases share the same queue limits.
pub(crate) type NotificationBacklog = AdmissionQueue<StorableOriginSha256, PendingNotification>;

const SECOND_NS: u64 = 1_000_000_000;
const MINUTE_NS: u64 = 60 * SECOND_NS;
const EXPIRY_NS: u64 = 5 * MINUTE_NS;

/// Browsers the sizing assumes each recipient has registered.
const BROWSERS_PER_RECIPIENT: usize = 2;

/// Notifications the dispatcher drains within one expiry window, so nothing admitted
/// outlasts it while recipients keep to [`BROWSERS_PER_RECIPIENT`].
const DRAINED_PER_EXPIRY: usize = dispatch::MAX_POSTS_PER_PASS
    * (EXPIRY_NS / dispatch::INTERVAL.as_nanos() as u64) as usize
    / BROWSERS_PER_RECIPIENT;

pub(crate) const NOTIFICATION_BACKLOG: QueueConfig = QueueConfig {
    max_entries: DRAINED_PER_EXPIRY,
    max_entries_per_sender: DRAINED_PER_EXPIRY * 7 / 10,
    max_pending_per_group: 20,
    pressure_cleared_below: DRAINED_PER_EXPIRY * 8 / 10,
    discard_entries_after_ns: EXPIRY_NS,
    max_lifetime_ns: 15 * MINUTE_NS,
    retry: RetryPolicy {
        base_ns: 5 * SECOND_NS,
        ceiling_ns: 5 * MINUTE_NS,
        when_stalled_ns: 10 * MINUTE_NS,
        doubles_every_ns: 30 * SECOND_NS,
        stalled_after_silence_ns: MINUTE_NS,
    },
};

const _: () = assert!(NOTIFICATION_BACKLOG.is_coherent());

// A single app must stay below the pressure reset threshold.
const _: () = assert!(
    NOTIFICATION_BACKLOG.max_entries_per_sender < NOTIFICATION_BACKLOG.pressure_cleared_below
);

// Opening one slot must not reset the pressure timer.
const _: () =
    assert!(NOTIFICATION_BACKLOG.pressure_cleared_below < NOTIFICATION_BACKLOG.max_entries);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::admission_queue::{Admission, Admitted, Taken};

    fn results<Key>(admitted: Vec<Admitted<Key>>) -> Vec<Admission> {
        admitted
            .into_iter()
            .map(|answer| answer.admission)
            .collect()
    }

    fn origin(host: &str) -> StorableOriginSha256 {
        StorableOriginSha256::from_origin(&host.to_string())
    }

    fn principal(seed: u64) -> Principal {
        Principal::from_slice(&seed.to_be_bytes())
    }

    fn notification(anchor_number: AnchorNumber, notification_id: u64) -> PendingNotification {
        PendingNotification {
            recipient: principal(anchor_number),
            anchor_number,
            application_number: 1,
            account_number: None,
            sender: principal(u64::MAX),
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
        .into_iter()
        .map(|urgency| {
            PendingNotification {
                urgency,
                ..notification(1, 1)
            }
            .lane()
        })
        .collect();

        assert_eq!(priorities, vec![0, 1, 2, 3]);
        assert!(priorities
            .iter()
            .all(|priority| *priority < PendingNotification::TURNS.len()));
    }

    #[test]
    fn one_id_reused_across_recipients_stays_two_notifications() {
        assert_ne!(notification(1, 7).key(), notification(2, 7).key());
    }

    #[test]
    fn one_id_sent_to_two_accounts_of_one_identity_stays_two_notifications() {
        let mut backlog = NotificationBacklog::new(NOTIFICATION_BACKLOG, 0);
        let first = notification(1, 7);
        let second = PendingNotification {
            recipient: principal(2),
            ..notification(1, 7)
        };
        assert_eq!(first.group(), second.group());

        assert_eq!(
            results(backlog.admit(origin("https://a.example"), vec![first, second], 1)),
            vec![Admission::Accepted; 2]
        );
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
                retry_after_ns: NOTIFICATION_BACKLOG.retry.base_ns
            }]
        );

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

        // High gets four turns before Normal, even with High entries remaining.
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

        // The app deadline expires before the queue limit of five minutes.
        assert!(take(&mut backlog, 10, 31 * SECOND_NS).is_empty());
    }

    fn take(
        backlog: &mut NotificationBacklog,
        limit: usize,
        now_ns: u64,
    ) -> Vec<Taken<StorableOriginSha256, PendingNotification>> {
        backlog
            .take_batch(limit, now_ns, |batch| Ok::<_, ()>(batch.to_vec()))
            .unwrap()
    }

    fn ids_taken(backlog: &mut NotificationBacklog, limit: usize, now_ns: u64) -> Vec<u64> {
        take(backlog, limit, now_ns)
            .iter()
            .map(|taken| taken.entry.item.notification_id)
            .collect()
    }
}
