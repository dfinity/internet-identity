use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_inbox::StorableInboxNotification;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
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
    /// The notification app record this browser points at, where it has one.
    #[n(8)]
    pub notification_app: Option<StorableNotificationAppRef>,
    /// What this browser registered for Web Push, if anything.
    ///
    /// 7 held the registration before a push carried a payload, and 9 the inbox whose
    /// entries were unnamed. Neither can be read into the shapes below, which require the
    /// browser's sealing keys and an id per entry, so both were retired rather than
    /// migrated: a browser re-registers and is whole again. Do not reuse either index.
    #[n(10)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
    /// What this identity's service worker has yet to show on this device, oldest first.
    #[n(11)]
    pub notifications: Option<Vec<StorableInboxNotification>>,
    /// What the next entry deposited in that inbox is named.
    #[n(12)]
    pub next_inbox_notification_id: u64,
}
