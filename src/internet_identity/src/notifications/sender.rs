//! Sender authorization: the verified `(canister, origin)` bindings a
//! `notification_send` caller must satisfy. A binding is recorded when an
//! origin's `/.well-known/ii-notification-senders` lists the canister (fetched
//! at consent, see `well_known`). At send time the caller must also declare that
//! origin, so the trust is two-way: an origin can vouch for a canister, and the
//! canister vouches for the origin, and only the intersection is authorized — an
//! origin cannot authorize a canister it does not own.

use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::sender::StorableSenderBinding;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};

/// Whether `sender` is a verified sender for `origin` — i.e. that origin's
/// well-known list named it. The caller declares the origin in the send request;
/// this is the check that the origin vouches back.
pub fn is_authorized_sender(sender: Principal, origin: &FrontendHostname) -> bool {
    let origin_hash = StorableOriginSha256::from_origin(origin);
    storage_borrow(|storage| {
        storage
            .notification_sender_bindings_memory
            .contains_key(&(sender, origin_hash))
    })
}

/// Record a `(sender, origin)` binding, learned from that origin's well-known
/// senders list.
#[cfg_attr(test, allow(dead_code))]
pub fn bind_sender(sender: Principal, origin: FrontendHostname, now_ns: Timestamp) {
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow_mut(|storage| {
        storage.notification_sender_bindings_memory.insert(
            (sender, origin_hash),
            StorableSenderBinding {
                cached_at_ns: now_ns,
            },
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::test_setup as setup;

    #[test]
    fn a_bound_sender_is_authorized_for_its_origin() {
        setup();
        let sender = Principal::from_slice(&[1u8; 10]);
        let origin = "https://app.example".to_string();
        bind_sender(sender, origin.clone(), 0);
        assert!(is_authorized_sender(sender, &origin));
    }

    #[test]
    fn an_unbound_sender_is_not_authorized() {
        setup();
        let sender = Principal::from_slice(&[1u8; 10]);
        assert!(!is_authorized_sender(
            sender,
            &"https://app.example".to_string()
        ));
    }

    #[test]
    fn a_binding_does_not_authorize_a_different_origin() {
        // Poisoning defense: origin Z listing canister X binds (X, Z), but X
        // declaring Y (its real origin) is not authorized by that — the
        // declared origin must match the binding.
        setup();
        let sender = Principal::from_slice(&[1u8; 10]);
        bind_sender(sender, "https://evil.example".to_string(), 0);
        assert!(!is_authorized_sender(
            sender,
            &"https://app.example".to_string()
        ));
    }
}
