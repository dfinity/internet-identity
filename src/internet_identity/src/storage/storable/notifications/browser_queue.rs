//! A notification a browser's service worker has yet to take, stored on the Web Push
//! subscription it was woken through, so the registration going takes it too.

use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::notifications::notification_id::StorableNotificationId;
use crate::storage::storable::timestamp::StorableTimestamp;
use internet_identity_interface::internet_identity::types::NotificationEvent;
use minicbor::{Decode, Encode};

/// What the service worker needs to fetch it: the app and account it is for, the
/// canister to ask, and the app's own ID.
///
/// Internet Identity's own notification has no app and no account, and carries the event
/// instead of pointing at a canister to fetch text from.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableQueuedNotification {
    #[n(0)]
    pub application_number: Option<StorableApplicationNumber>,
    /// The app canister that sent it.
    #[cbor(n(1), with = "minicbor::bytes")]
    pub sender: Vec<u8>,
    #[n(2)]
    pub notification_id: StorableNotificationId,
    #[n(3)]
    pub expires_at_ns: StorableTimestamp,
    /// `None` is the unreserved default account.
    #[n(4)]
    pub account_number: Option<StorableAccountNumber>,
    /// Set where Internet Identity sent this itself.
    #[n(5)]
    pub event: Option<StorableNotificationEvent>,
}

/// Something Internet Identity has to tell the user about their own identity.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableNotificationEvent {
    #[n(0)]
    NotificationsOn(#[n(0)] StorableAnchorNumber),
}

impl From<NotificationEvent> for StorableNotificationEvent {
    fn from(value: NotificationEvent) -> Self {
        match value {
            NotificationEvent::NotificationsOn(anchor_number) => {
                StorableNotificationEvent::NotificationsOn(anchor_number)
            }
        }
    }
}

impl From<StorableNotificationEvent> for NotificationEvent {
    fn from(value: StorableNotificationEvent) -> Self {
        match value {
            StorableNotificationEvent::NotificationsOn(anchor_number) => {
                NotificationEvent::NotificationsOn(anchor_number)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use candid::Principal;
    use pretty_assertions::assert_eq;

    #[test]
    fn a_queued_notification_round_trips_its_sender_as_bytes() {
        let queued = StorableQueuedNotification {
            application_number: Some(u64::MAX),
            sender: vec![0xff; Principal::MAX_LENGTH_IN_BYTES],
            notification_id: u64::MAX,
            expires_at_ns: u64::MAX,
            account_number: Some(u64::MAX),
            event: None,
        };
        let mut bytes = Vec::new();
        minicbor::encode(&queued, &mut bytes).expect("encoding a queued notification");

        assert_eq!(bytes.len(), 1 + (1 + 9) + (1 + 2 + 29) + 3 * (1 + 9));
        assert_eq!(
            minicbor::decode::<StorableQueuedNotification>(&bytes).expect("decoding it"),
            queued
        );
    }
}
