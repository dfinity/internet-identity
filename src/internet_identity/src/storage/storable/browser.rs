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
    ///
    /// Absent on an entry written before entries were named. A map decodes without the
    /// keys it does not carry only where the field admits their absence, and the inbox at
    /// 11 is empty on such an entry, so its run of ids starts where a fresh one does.
    #[n(12)]
    pub next_inbox_notification_id: Option<u64>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::storable::browser_description::{
        StorableBrowserBrand, StorableFormFactor, StorableOperatingSystem,
    };
    use pretty_assertions::assert_eq;

    /// An entry as it was written before a browser carried any notification state: the
    /// keys below and nothing else.
    #[derive(Encode)]
    #[cbor(map)]
    struct BrowserWithoutNotifications {
        #[n(0)]
        id: StorableBrowserId,
        #[n(1)]
        description: StorableBrowserDescription,
        #[n(2)]
        created_at: Timestamp,
        #[n(3)]
        last_used: Timestamp,
        #[cbor(n(4), with = "minicbor::bytes")]
        current_browser_key: Vec<u8>,
        #[cbor(n(5), with = "minicbor::bytes")]
        next_browser_key: Vec<u8>,
        #[n(6)]
        session_count: u32,
    }

    /// Every anchor read decodes its browsers, so a key this struct requires and a stored
    /// entry does not carry takes down far more than notifications.
    #[test]
    fn an_entry_written_before_notifications_still_decodes() {
        let stored = BrowserWithoutNotifications {
            id: 3,
            description: StorableBrowserDescription {
                brand: StorableBrowserBrand::Chrome,
                os: StorableOperatingSystem::Macos,
                form_factor: StorableFormFactor::Desktop,
                model: None,
            },
            created_at: 1,
            last_used: 2,
            current_browser_key: vec![1; 91],
            next_browser_key: vec![2; 91],
            session_count: 4,
        };
        let mut bytes = Vec::new();
        minicbor::encode(&stored, &mut bytes).expect("encoding the older shape");

        let read: StorableBrowser =
            minicbor::decode(&bytes).expect("an entry without the notification keys");

        assert_eq!(read.id, 3);
        assert_eq!(read.session_count, 4);
        assert_eq!(read.webpush_subscription, None);
        assert_eq!(read.notifications, None);
        assert_eq!(read.next_inbox_notification_id, None);
    }
}
