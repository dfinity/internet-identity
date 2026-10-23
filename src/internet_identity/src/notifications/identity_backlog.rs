//! Internet Identity's own wake-ups, taking turns across identities.
//! Deduplication uses the notification id; urgency sets the lane.
//!
//! Separate from the app backlog because the two are filled by different parties: an
//! app's notifications arrive from its canister, and these arrive from the identity's own
//! browsers. One queue for both would let a browser that can provoke wake-ups take turns
//! away from every app.

use super::admission_queue::{AdmissionQueue, QueueConfig, QueueItem, RetryPolicy};
use super::dispatch::{self, IdentityNotification};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, NotificationEvent, NotificationId, Timestamp, Urgency,
};

/// One of Internet Identity's own wake-ups, waiting for a pass to post it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct PendingIdentityNotification {
    /// The identity whose entries are woken. Also what the queue takes turns across.
    pub(crate) anchor_number: AnchorNumber,
    /// Absent is every entry of the identity; present names the subset, which is how a
    /// probe reaches one device.
    pub(crate) destination: Option<Vec<BrowserId>>,
    /// Allocated when the wake-up is submitted, so a resubmission folds into the one
    /// already waiting. A wake-up carrying nothing new takes one of its own, which is
    /// what keeps the key uniform.
    pub(crate) notification_id: NotificationId,
    /// What the destinations are given to show, written onto their entries when the pass
    /// expands this. Absent where the wake-up announces nothing new, which is a worker
    /// being asked to come back for what its entry already holds.
    pub(crate) event: Option<NotificationEvent>,
    /// Decides the address each entry is reached at when a pass expands this.
    pub(crate) kind: IdentityNotification,
    pub(crate) urgency: Urgency,
    pub(crate) expires_at_ns: Option<Timestamp>,
}

impl QueueItem for PendingIdentityNotification {
    /// The id alone: ids are allocated by the canister and never repeat, so dedup catches
    /// a genuine resubmission and nothing else. The destination is deliberately absent —
    /// a worker looping on delegations submits fresh ids and is stopped by the quota.
    type Key = NotificationId;

    /// Per kind, per identity, so a burst of one kind cannot take the whole of an
    /// identity's allowance from its other wake-ups.
    type Group = IdentityNotification;

    const TURNS: &'static [usize] = &[4, 3, 2, 1];

    fn key(&self) -> Self::Key {
        self.notification_id
    }

    fn group(&self) -> Self::Group {
        self.kind
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

/// Senders are identities, so the turns the structure takes are turns between identities.
pub(crate) type IdentityBacklog = AdmissionQueue<AnchorNumber, PendingIdentityNotification>;

const SECOND_NS: u64 = 1_000_000_000;
const MINUTE_NS: u64 = 60 * SECOND_NS;

/// Shorter than the hour one of these is worth showing for once it reaches a device: a
/// wake-up not posted within five minutes is stale as a wake-up whatever it carries, and
/// the item's own deadline still caps it from above.
const EXPIRY_NS: u64 = 5 * MINUTE_NS;

/// Entries the sizing assumes each identity has registered.
const BROWSERS_PER_IDENTITY: usize = 2;

/// Wake-ups the identity half of a pass drains within one expiry window, so nothing
/// admitted outlasts it while identities keep to [`BROWSERS_PER_IDENTITY`].
const DRAINED_PER_EXPIRY: usize = dispatch::MAX_IDENTITY_POSTS_PER_PASS
    * (EXPIRY_NS / dispatch::INTERVAL.as_nanos() as u64) as usize
    / BROWSERS_PER_IDENTITY;

pub(crate) const IDENTITY_NOTIFICATION_BACKLOG: QueueConfig = QueueConfig {
    max_entries: DRAINED_PER_EXPIRY,
    // A flat ceiling rather than a fraction of the whole: the senders here are
    // identities, and the point of the queue is that none of them dominates. Twenty
    // entries each carrying a wake-up plus a few events is about twenty-five.
    max_entries_per_sender: 64,
    // Per kind, per identity. MAX_BROWSERS is 20 and a wake-up is per entry, so this is
    // one kind's worth for every entry the identity has.
    max_pending_per_group: 20,
    pressure_cleared_below: DRAINED_PER_EXPIRY * 8 / 10,
    discard_entries_after_ns: EXPIRY_NS,
    max_lifetime_ns: 15 * MINUTE_NS,
    // Unchanged from the app backlog: the relays and their failure modes are the same.
    retry: RetryPolicy {
        base_ns: 5 * SECOND_NS,
        ceiling_ns: 5 * MINUTE_NS,
        when_stalled_ns: 10 * MINUTE_NS,
        doubles_every_ns: 30 * SECOND_NS,
        stalled_after_silence_ns: MINUTE_NS,
    },
};

const _: () = assert!(IDENTITY_NOTIFICATION_BACKLOG.is_coherent());

// A single identity must stay below the pressure reset threshold.
const _: () = assert!(
    IDENTITY_NOTIFICATION_BACKLOG.max_entries_per_sender
        < IDENTITY_NOTIFICATION_BACKLOG.pressure_cleared_below
);

// Opening one slot must not reset the pressure timer.
const _: () = assert!(
    IDENTITY_NOTIFICATION_BACKLOG.pressure_cleared_below
        < IDENTITY_NOTIFICATION_BACKLOG.max_entries
);
