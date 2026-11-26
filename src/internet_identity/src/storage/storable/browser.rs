use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_inbox::StorableInbox;
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
    #[n(10)]
    pub webpush_subscription: Option<StorableWebPushSubscription>,
    /// What the next entry deposited in that inbox is named. `None` on an entry written
    /// before entries were named, whose inbox is empty anyway.
    #[n(12)]
    pub next_inbox_notification_id: Option<u64>,
    /// What this identity's service worker has yet to show on this device, oldest first.
    #[n(13)]
    pub inbox: Option<StorableInbox>,
    /// The entry of this identity that this one shares a device with, set where an
    /// installed web app on this entry claimed the link from that entry.
    #[n(14)]
    pub same_device_browser_id: Option<StorableBrowserId>,
    /// The one-shot key an installed web app on this entry registered for that claim.
    #[n(15)]
    pub pending_same_device_link: Option<StorablePendingSameDeviceLink>,
    // #[n(7)] RETIRED, never reuse: the Web Push registration before a push carried a payload.
    // #[n(9)] RETIRED, never reuse: the inbox before its entries were named.
    // #[n(11)] RETIRED, never reuse: the inbox before its entries named an app's account by
    // principal.
}

/// A one-shot key another entry of the identity proves possession of to link itself to
/// this entry as sharing its device, and when it stops being accepted.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorablePendingSameDeviceLink {
    /// DER-encoded P-256 SPKI.
    #[cbor(n(0), with = "minicbor::bytes")]
    pub link_public_key: Vec<u8>,
    #[n(1)]
    pub expires_at_ns: Timestamp,
}
