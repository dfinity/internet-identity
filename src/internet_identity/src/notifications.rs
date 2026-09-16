//! Per-app consent: whether an app may notify an identity.
//!
//! Callers reach this through `main.rs`, which validates and authorizes first, so
//! everything here acts on an origin already folded to the spelling consent is keyed by.

mod validation;
pub mod webpush;

use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::StorageError;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, NotificationGrantConsentError, NotificationRevokeConsentError,
    Timestamp,
};
pub use validation::{
    notifications_enabled, ValidatedNotificationConsentGrantedRequest,
    ValidatedNotificationGrantConsentRequest, ValidatedNotificationRevokeConsentRequest,
};

/// Grants `origin` permission to notify `anchor_number`.
pub fn grant_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    now_ns: Timestamp,
) -> Result<(), NotificationGrantConsentError> {
    write_consent(anchor_number, &origin, Some(now_ns))
        .map_err(|_| NotificationGrantConsentError::NoSuchSession)
}

/// Withdraws `origin`'s consent. Device subscriptions stay: they are shared across every
/// consented app.
pub fn revoke_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationRevokeConsentError> {
    // Consent that cannot exist is already withdrawn, so an app the identity never
    // reached is nothing to report.
    let _ = write_consent(anchor_number, &origin, None);
    Ok(())
}

/// Whether `origin` may notify `anchor_number`.
pub fn consent_granted(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    storage_borrow(|storage| {
        storage
            .read_anchor_application_config(anchor_number, &origin)
            .and_then(|config| config.notifications_consented_at_ns)
            .is_some()
    })
}

/// Moves the one field this owns, leaving the rest of the app's config as it stands.
fn write_consent(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    consented_at_ns: Option<Timestamp>,
) -> Result<(), StorageError> {
    storage_borrow_mut(|storage| {
        let mut config = storage
            .read_anchor_application_config(anchor_number, origin)
            .unwrap_or_default();
        config.notifications_consented_at_ns = consented_at_ns;
        storage.write_anchor_application_config(anchor_number, origin, config)
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
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        grant_consent(anchor, origin.clone(), 1_000).unwrap();
        assert!(consent_granted(anchor, origin.clone()));

        revoke_consent(anchor, origin.clone()).unwrap();
        assert!(!consent_granted(anchor, origin));
    }

    /// Consent hangs off the application, which only a sign-in mints.
    #[test]
    fn refuses_an_app_the_identity_has_never_signed_in_at() {
        setup();
        let anchor = anchor_at("https://visited.example");

        assert_eq!(
            grant_consent(anchor, "https://never.example".to_string(), 1_000),
            Err(NotificationGrantConsentError::NoSuchSession)
        );
    }

    /// Nothing to withdraw is already withdrawn, so the caller has nothing to handle.
    #[test]
    fn revoking_what_was_never_granted_succeeds() {
        setup();
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        assert!(revoke_consent(anchor, origin).is_ok());
        assert!(revoke_consent(anchor, "https://never.example".to_string()).is_ok());
    }

    /// Consent and the default account share one config row.
    #[test]
    fn consent_leaves_the_default_account_alone() {
        setup();
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        let before = storage_borrow(|s| {
            s.read_anchor_application_config(anchor, &origin)
                .and_then(|config| config.default_account_number)
        });

        grant_consent(anchor, origin.clone(), 1_000).unwrap();

        let after = storage_borrow(|s| {
            s.read_anchor_application_config(anchor, &origin)
                .and_then(|config| config.default_account_number)
        });
        assert_eq!(before, after);
        assert!(consent_granted(anchor, origin));
    }
}
