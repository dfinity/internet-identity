use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_queue::StorableQueuedNotification;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use crate::storage::storable::timestamp::StorableTimestamp;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableBrowser {
    #[n(0)]
    pub id: StorableBrowserId,
    /// What this browser reported about itself when it registered. Immutable: a
    /// sign-in that reports something else is a browser this anchor has not seen, and
    /// registers under its own entry.
    #[n(1)]
    pub description: StorableBrowserDescription,
    #[n(2)]
    pub created_at: Timestamp,
    #[n(3)]
    pub last_used: Timestamp,
    #[cbor(n(4), with = "minicbor::bytes")]
    pub current_browser_key: Vec<u8>,
    #[cbor(n(5), with = "minicbor::bytes")]
    pub next_browser_key: Vec<u8>,
    /// Sessions this browser holds, counted where the reference lists holding them are
    /// written.
    #[n(6)]
    pub session_count: u32,
    /// What this browser registered for Web Push, if anything.
    #[n(7)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
    /// The notification app record this browser points at, where it has one.
    #[n(8)]
    pub notification_app: Option<StorableNotificationAppRef>,
    /// What this identity's service worker has yet to show on this device, oldest first.
    ///
    /// Held by the entry rather than by the subscription, because the address can be the
    /// app's and shared with every other identity pointing at it, while what is waiting
    /// is this identity's alone.
    #[n(9)]
    pub notifications: Option<Vec<StorableQueuedNotification>>,
    /// When this entry was last woken a second time for a notification it could not show
    /// yet. The wake-up is posted at the service worker's own asking, so it is allowed
    /// again only once the delegation it was for has expired.
    #[n(10)]
    pub woken_again_at_ns: Option<StorableTimestamp>,
    /// The probe this entry last sent to the Home Screen app it points at. Whether it
    /// is still pending is read off the queue, so this id outliving the entry says
    /// nothing on its own.
    ///
    /// Kept so the browser watching for its answer is told about its own probe and not
    /// about some other notification of ours that happens to be queued: both are
    /// Internet Identity's own, and the queue cannot tell them apart.
    #[n(11)]
    pub pending_probe: Option<u64>,
}
