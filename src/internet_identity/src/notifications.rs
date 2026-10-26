//! Per-app consent, and the queue an app's accepted notifications wait in.
//!
//! Callers reach the consent functions through `main.rs`, which validates and
//! authorizes first, so everything here acts on an origin already folded to the
//! spelling consent is keyed by.

pub(crate) mod backlog;
pub(crate) mod browser_inbox;
pub mod delegation;
pub(crate) mod dispatch;
pub mod installed_app;
pub mod senders;
pub mod submission;
pub(crate) mod validation;
pub mod webpush;

/// A browser unused for this long counts as gone and is not woken. The send endpoint
/// answers `NoSuchRecipient` for a recipient whose every browser is past it.
pub(crate) const BROWSER_GONE_AFTER_NS: u64 = 60 * 24 * 60 * 60 * 1_000_000_000;

use crate::origin::ApplicationOrigin;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::anchor_application_config::PermissionDecision;
use crate::storage::StorageError;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, NotAccepted, NotAcceptedReason, Notification,
    NotificationGrantConsentError, NotificationRevokeConsentError, SendNotificationResponse,
    Timestamp,
};
pub(crate) use validation::NOT_ENABLED_MESSAGE;
pub use validation::{
    admit_batch, enabled_or, fetchable_origin, notifications_enabled, notifying_origin,
    TooManyNotifications,
};

/// Nothing was enqueued: the whole batch is the sender's to send again at
/// `retry_after`.
pub fn defer_whole_batch(
    notifications: Vec<Notification>,
    retry_after: Timestamp,
) -> SendNotificationResponse {
    SendNotificationResponse {
        not_accepted: notifications
            .into_iter()
            .map(|Notification { id, recipient, .. }| NotAccepted {
                id,
                recipient,
                reason: NotAcceptedReason::Deferred { retry_after },
            })
            .collect(),
    }
}

/// Grants `origin` permission to notify `anchor_number`.
///
/// Takes an `ApplicationOrigin` rather than a string, so a spelling nobody folded
/// cannot reach the write.
pub fn grant_consent(
    anchor_number: AnchorNumber,
    origin: &ApplicationOrigin,
    now_ns: Timestamp,
) -> Result<(), NotificationGrantConsentError> {
    write_notifications_decision(anchor_number, origin, true, now_ns)
        .map_err(|err| NotificationGrantConsentError::InternalCanisterError(format!("{err}")))
}

/// Withdraws the origin's consent, and what it still has queued for the identity's
/// browsers. Device subscriptions stay: they are shared across every consented app.
pub fn revoke_consent(
    anchor_number: AnchorNumber,
    origin: &ApplicationOrigin,
    now_ns: Timestamp,
) -> Result<(), NotificationRevokeConsentError> {
    // Consent that cannot exist is already withdrawn, so an app the identity never
    // reached is nothing to report.
    let _ = write_notifications_decision(anchor_number, origin, false, now_ns);
    browser_inbox::remove_app_entries(anchor_number, origin);
    Ok(())
}

/// Whether `origin` may notify `anchor_number`, for callers holding the folded spelling
/// as a plain string rather than an `ApplicationOrigin`.
pub fn consent_granted_for(anchor_number: AnchorNumber, origin: &FrontendHostname) -> bool {
    storage_borrow(|storage| {
        storage
            .read_anchor_application_config(anchor_number, origin)
            .is_some_and(|config| config.notifications_allowed())
    })
}

pub fn consent_granted(anchor_number: AnchorNumber, origin: &ApplicationOrigin) -> bool {
    consent_granted_for(anchor_number, &origin.as_str().to_string())
}

/// Records the identity's answer for notifications, leaving the rest of the app's config
/// as it stands.
///
/// Only an answer that changes something is written, so `changed_at_ns` is when it last
/// changed. Denying an app the identity never allowed decides nothing: it stays unset.
fn write_notifications_decision(
    anchor_number: AnchorNumber,
    origin: &ApplicationOrigin,
    allowed: bool,
    now_ns: Timestamp,
) -> Result<(), StorageError> {
    // The storage index keys by the owned spelling; this is the one place the folded
    // value crosses out of its type.
    let origin = &origin.as_str().to_string();
    storage_borrow_mut(|storage| {
        let mut config = storage
            .read_anchor_application_config(anchor_number, origin)
            .unwrap_or_default();
        let unchanged = match config.notifications {
            Some(decision) => decision.allowed == allowed,
            None => !allowed,
        };
        if unchanged {
            return Ok(());
        }
        config.notifications = Some(PermissionDecision {
            allowed,
            changed_at_ns: now_ns,
        });
        storage.write_anchor_application_config(anchor_number, origin, config, now_ns)
    })
}

#[cfg(test)]
pub(crate) fn test_setup() {
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;
    crate::state::storage_replace(Storage::new((0, 100), VectorMemory::default()));
    storage_borrow_mut(|s| s.update_salt([7u8; 32]));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::test_setup as setup;

    const APP: &str = "https://app.example";
    const NEVER: &str = "https://never.example";
    const VISITED: &str = "https://visited.example";

    /// Turns notifications on, so a request for any origin validates.
    fn enable_notifications() {
        crate::state::persistent_state_mut(|s| s.notifications_enabled = Some(true));
    }

    /// Requests are built through `TryFrom`, which is the only way to make one, so the
    /// tests go through the same validation a caller does.
    fn grant(
        anchor_number: AnchorNumber,
        origin: &str,
        now_ns: Timestamp,
    ) -> Result<(), NotificationGrantConsentError> {
        grant_consent(
            anchor_number,
            &notifying_origin(origin).expect("a notifiable origin"),
            now_ns,
        )
    }

    fn revoke(
        anchor_number: AnchorNumber,
        origin: &str,
    ) -> Result<(), NotificationRevokeConsentError> {
        revoke_consent(
            anchor_number,
            &notifying_origin(origin).expect("a notifiable origin"),
            1_000,
        )
    }

    fn granted(anchor_number: AnchorNumber, origin: &str) -> bool {
        consent_granted(
            anchor_number,
            &notifying_origin(origin).expect("a notifiable origin"),
        )
    }

    /// An identity that has signed in at `origin`, which is what a grant needs.
    fn anchor_at(origin: &str) -> AnchorNumber {
        storage_borrow_mut(|storage| {
            let anchor = storage.allocate_anchor(0).expect("allocating an anchor");
            let anchor_number = anchor.anchor_number();
            storage.write(anchor).expect("writing the anchor");
            storage.sign_in_for_testing(anchor_number, &origin.to_string());
            anchor_number
        })
    }

    #[test]
    fn grant_then_revoke_consent_round_trips() {
        setup();
        enable_notifications();
        let anchor = anchor_at(APP);

        grant(anchor, APP, 1_000).unwrap();
        assert!(granted(anchor, APP));

        revoke(anchor, APP).unwrap();
        assert!(!granted(anchor, APP));
    }

    /// An app can ask before the identity has ever signed in at it, so the grant mints
    /// the application it hangs off rather than refusing.
    #[test]
    fn grants_at_an_app_the_identity_has_never_signed_in_at() {
        setup();
        enable_notifications();
        let anchor = anchor_at(VISITED);

        grant(anchor, NEVER, 1_000).unwrap();
        assert!(granted(anchor, NEVER));
        // The origin it had already reached is untouched by minting another.
        assert!(!granted(anchor, VISITED));
    }

    /// Nothing to withdraw is already withdrawn, so the caller has nothing to handle.
    #[test]
    fn revoking_what_was_never_granted_succeeds() {
        setup();
        enable_notifications();
        let anchor = anchor_at(APP);

        assert!(revoke(anchor, APP).is_ok());
        assert!(revoke(anchor, NEVER).is_ok());
    }

    /// Admitted the way the endpoint admits one, so the deferral tests hand the reply
    /// builder what a caller's call would leave it.
    fn batch(notifications: Vec<Notification>) -> Vec<Notification> {
        enable_notifications();
        admit_batch(notifications).unwrap_or_else(|_| panic!("within the cap"))
    }

    fn notification(id: u64, recipient: &str) -> Notification {
        Notification::new(
            id,
            candid::Principal::from_text(recipient).expect("a principal"),
        )
    }

    /// The interface promises a batch applies in order and the reply names each
    /// (recipient, id) at most once, so a repeated pair collapses to its last entry.
    #[test]
    fn a_repeated_recipient_and_id_is_deferred_once() {
        let repeated = notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai");
        let response = defer_whole_batch(batch(vec![repeated.clone(), repeated.clone()]), 1_000);

        assert_eq!(response.not_accepted.len(), 1);
        assert_eq!(response.not_accepted[0].id, 1);
    }

    /// One id sent to two recipients is two notifications, not a repeat.
    #[test]
    fn one_id_for_two_recipients_is_deferred_twice() {
        let response = defer_whole_batch(
            batch(vec![
                notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai"),
                notification(1, "rrkah-fqaaa-aaaaa-aaaaq-cai"),
            ]),
            1_000,
        );

        assert_eq!(response.not_accepted.len(), 2);
    }

    #[test]
    fn a_deferred_entry_carries_when_to_send_it_again() {
        let response = defer_whole_batch(
            batch(vec![notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai")]),
            1_000,
        );

        assert_eq!(
            response.not_accepted[0].reason,
            NotAcceptedReason::Deferred { retry_after: 1_000 }
        );
    }

    /// Consent and the default account share one config row.
    #[test]
    fn consent_leaves_the_default_account_alone() {
        setup();
        enable_notifications();
        let origin = APP.to_string();
        let anchor = anchor_at(APP);

        let before = storage_borrow(|s| {
            s.read_anchor_application_config(anchor, &origin)
                .and_then(|config| config.default_account_number)
        });

        grant(anchor, APP, 1_000).unwrap();

        let after = storage_borrow(|s| {
            s.read_anchor_application_config(anchor, &origin)
                .and_then(|config| config.default_account_number)
        });
        assert_eq!(before, after);
        assert!(granted(anchor, APP));
    }
}
