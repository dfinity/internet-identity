//! A notification a browser's service worker has yet to take, stored on the browser's entry.

use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::notifications::notification_id::StorableNotificationId;
use crate::storage::storable::timestamp::StorableTimestamp;
use internet_identity_interface::internet_identity::types::{
    IdentityForNotification, NotificationEvent,
};
use minicbor::data::Type;
use minicbor::decode;
use minicbor::encode;
use minicbor::{Decode, Decoder, Encode, Encoder};

/// A browser's inbox as stored: its entries, oldest first, as one CBOR array.
///
/// An entry that does not decode is skipped, and the rest of the inbox read: a release
/// that adds a kind of entry must not leave an earlier one, rolled back to, unable to read
/// the browser entry and with it the whole anchor. An inbox entry is a notification worth
/// at most a day, so dropping one costs that notification and nothing else.
#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableInbox(pub Vec<StorableInboxNotification>);

impl<C> Encode<C> for StorableInbox {
    fn encode<W: encode::Write>(
        &self,
        encoder: &mut Encoder<W>,
        ctx: &mut C,
    ) -> Result<(), encode::Error<W::Error>> {
        self.0.encode(encoder, ctx)
    }
}

impl<'b, C> Decode<'b, C> for StorableInbox {
    fn decode(decoder: &mut Decoder<'b>, ctx: &mut C) -> Result<Self, decode::Error> {
        let mut num_remaining_entries = decoder.array()?;
        let mut entries = Vec::new();
        loop {
            match num_remaining_entries {
                Some(0) => break,
                Some(count) => num_remaining_entries = Some(count.saturating_sub(1)),
                None => {
                    if decoder.datatype()? == Type::Break {
                        decoder.skip()?;
                        break;
                    }
                }
            }
            let start = decoder.position();
            match StorableInboxNotification::decode(decoder, ctx) {
                Ok(entry) => entries.push(entry),
                Err(err) => {
                    ic_cdk::println!("WARN: skipped an inbox entry that does not decode: {err}");
                    decoder.set_position(start);
                    decoder.skip()?;
                }
            }
        }
        Ok(StorableInbox(entries))
    }
}

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
    /// Internet Identity's own, checking that an app's installed web app receives
    /// notifications for the account with this principal.
    #[n(2)]
    InstalledAppProbe {
        #[cbor(n(0), with = "minicbor::bytes")]
        account_principal: Vec<u8>,
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

    /// An entry as a release with a kind this one does not know writes it.
    #[derive(Encode)]
    #[cbor(map)]
    struct EntryOfAnUnknownKind {
        #[n(0)]
        inbox_notification_id: u64,
        #[cbor(n(1), with = "minicbor::bytes")]
        sender: Vec<u8>,
        #[n(2)]
        notification_id: u64,
        #[n(3)]
        expires_at_ns: u64,
        #[n(4)]
        kind: UnknownKind,
    }

    #[derive(Encode)]
    enum UnknownKind {
        #[n(7)]
        Later {
            #[n(0)]
            field: u64,
        },
    }

    #[test]
    fn an_inbox_entry_of_an_unknown_kind_is_skipped_and_the_rest_read() {
        let known_entry = StorableInboxNotification {
            inbox_notification_id: 2,
            sender: vec![1; 10],
            notification_id: 3,
            expires_at_ns: 4,
            kind: StorableInboxNotificationKind::App {
                account_principal: vec![9; 10],
            },
        };
        let mut bytes = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut bytes);
        encoder.array(2).expect("starting the inbox");
        encoder
            .encode(EntryOfAnUnknownKind {
                inbox_notification_id: 1,
                sender: vec![1; 10],
                notification_id: 2,
                expires_at_ns: 3,
                kind: UnknownKind::Later { field: 5 },
            })
            .expect("encoding the unknown entry");
        encoder
            .encode(&known_entry)
            .expect("encoding the known entry");

        let inbox: StorableInbox = minicbor::decode(&bytes).expect("decoding the inbox");

        assert_eq!(inbox, StorableInbox(vec![known_entry]));
    }

    #[test]
    fn an_inbox_reads_back_as_written() {
        let inbox = StorableInbox(vec![StorableInboxNotification {
            inbox_notification_id: 2,
            sender: vec![1; 10],
            notification_id: 3,
            expires_at_ns: 4,
            kind: StorableInboxNotificationKind::App {
                account_principal: vec![9; 10],
            },
        }]);
        let mut bytes = Vec::new();
        minicbor::encode(&inbox, &mut bytes).expect("encoding the inbox");

        assert_eq!(
            minicbor::decode::<StorableInbox>(&bytes).expect("decoding it"),
            inbox
        );
    }
}
