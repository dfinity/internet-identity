use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_inbox::StorableInboxNotification;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

/// Indices 7 and 9 are retired and must never be reused. 7 held the Web Push
/// registration before a push carried a payload, 9 the inbox before its entries were
/// named. Neither can be read into 10 and 11, which need sealing keys and an id per
/// entry, so a record written at either is dropped and the browser registers again.
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

    // #[n(7)] RETIRED, never reuse.
    /// The notification app record this browser points at, where it has one.
    #[n(8)]
    pub notification_app: Option<StorableNotificationAppRef>,

    // #[n(9)] RETIRED, never reuse.
    /// What this browser registered for Web Push, if anything.
    #[n(10)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
    /// What this identity's service worker has yet to show on this device, oldest first.
    #[n(11)]
    pub notifications: Option<Vec<StorableInboxNotification>>,
    /// What the next entry deposited in that inbox is named.
    ///
    /// Absent on an entry written before entries were named. A map decodes without the
    /// keys it does not carry only where the field admits their absence, and the inbox at
    /// 11 is empty on such an entry, so its run of ids starts where a fresh one does.
    #[n(12)]
    pub next_inbox_notification_id: Option<u64>,
}
