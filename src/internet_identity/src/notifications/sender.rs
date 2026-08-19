//! Sender authorization: which origin a canister may notify for, cached from
//! the dApp's `/.well-known/ii-notification-senders` (the HTTP fetch lives in
//! `well_known`). A canister principal doesn't encode its web origin, so this
//! reverse map is how a `notification_send` caller is resolved to an origin.

use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::notifications::sender::StorableSenderOrigin;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};

/// The origin `sender` is authorized to notify for, or `None` if never cached.
pub fn origin_for(sender: Principal) -> Option<FrontendHostname> {
    storage_borrow(|storage| {
        storage
            .notification_sender_canisters_memory
            .get(&sender)
            .map(|entry| entry.origin)
    })
}

/// Cache `sender -> origin`, learned from a dApp's well-known senders list.
#[cfg_attr(test, allow(dead_code))]
pub fn cache_sender(sender: Principal, origin: FrontendHostname, now_ns: Timestamp) {
    storage_borrow_mut(|storage| {
        storage.notification_sender_canisters_memory.insert(
            sender,
            StorableSenderOrigin {
                origin,
                cached_at_ns: now_ns,
            },
        );
    });
}
