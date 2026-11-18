//! A notification a browser's service worker has yet to take, stored on the browser's entry.

use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::notifications::notification_id::StorableNotificationId;
use crate::storage::storable::timestamp::StorableTimestamp;
use internet_identity_interface::internet_identity::types::{
    IdentityForNotification, NotificationEvent,
};
use minicbor::{Decode, Encode};

/// One entry of a browser's inbox: the canister that sent it, its id there, when it stops
/// being worth showing, and who it is for.
///
/// A layout of its own, stored under the browser entry's key 13. The retired inbox under
/// key 11 had keys 5 and 6 as well; nothing here reads them.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableInboxNotification {
    /// Ours, unique within the inbox holding it.
    #[n(0)]
    pub inbox_notification_id: u64,
    /// The canister that sent it: the app's, or Internet Identity's own.
    #[cbor(n(1), with = "minicbor::bytes")]
    pub sender: Vec<u8>,
    #[n(2)]
    pub notification_id: StorableNotificationId,
    #[n(3)]
    pub expires_at_ns: StorableTimestamp,
    #[n(4)]
    pub kind: StorableInboxNotificationKind,
}

/// Who an inbox entry is for: an account at an app, or the identity itself.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableInboxNotificationKind {
    /// An app's notification to the account with this principal, which names the app too.
    #[n(0)]
    App {
        #[cbor(n(0), with = "minicbor::bytes")]
        account_principal: Vec<u8>,
    },
    /// Internet Identity's own, and what it has to say.
    #[n(1)]
    Identity {
        #[n(0)]
        event: StorableNotificationEvent,
    },
}

/// Something Internet Identity has to tell the user about their own identity.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableNotificationEvent {
    #[n(0)]
    NotificationsOn(#[n(0)] StorableAnchorNumber, #[n(1)] Option<String>),
}

impl From<NotificationEvent> for StorableNotificationEvent {
    fn from(value: NotificationEvent) -> Self {
        match value {
            NotificationEvent::NotificationsOn(identity) => {
                StorableNotificationEvent::NotificationsOn(identity.anchor_number, identity.name)
            }
        }
    }
}

impl From<StorableNotificationEvent> for NotificationEvent {
    fn from(value: StorableNotificationEvent) -> Self {
        match value {
            StorableNotificationEvent::NotificationsOn(anchor_number, name) => {
                NotificationEvent::NotificationsOn(IdentityForNotification {
                    anchor_number,
                    name,
                })
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
    fn an_apps_entry_round_trips_its_sender_and_account_as_bytes() {
        let queued = StorableInboxNotification {
            inbox_notification_id: u64::MAX,
            sender: vec![0xff; Principal::MAX_LENGTH_IN_BYTES],
            notification_id: u64::MAX,
            expires_at_ns: u64::MAX,
            kind: StorableInboxNotificationKind::App {
                account_principal: vec![0xff; Principal::MAX_LENGTH_IN_BYTES],
            },
        };
        let mut bytes = Vec::new();
        minicbor::encode(&queued, &mut bytes).expect("encoding a queued notification");

        // The map header, three keyed maximal `u64`s, the keyed sender, and the keyed kind:
        // a two-item array of its index and a one-item array holding the account principal.
        assert_eq!(
            bytes.len(),
            1 + 3 * (1 + 9) + (1 + 2 + 29) + (1 + 1 + 1 + 1 + 2 + 29)
        );
        assert_eq!(
            minicbor::decode::<StorableInboxNotification>(&bytes).expect("decoding it"),
            queued
        );
    }
}
