//! Per-app consent: whether an app may notify an identity.

use super::{
    authorize_query, authorize_update, check_enabled, consent_origin, feature_enabled,
    NotificationError,
};
use crate::state::{storage_borrow, storage_borrow_mut};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, Timestamp,
};

fn set_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    now_ns: Timestamp,
) -> Result<(), NotificationError> {
    let origin = consent_origin(&origin)?;

    storage_borrow_mut(|storage| {
        let application_number = storage
            .notification_application(anchor_number, &origin)
            .ok_or(NotificationError::SessionMissing)?;
        storage.set_notification_consent(anchor_number, application_number, Some(now_ns));
        Ok(())
    })
}

fn clear_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    let origin = consent_origin(&origin)?;

    storage_borrow_mut(|storage| {
        // No application, so no consent row to withdraw.
        if let Some(application_number) = storage.notification_application(anchor_number, &origin) {
            storage.set_notification_consent(anchor_number, application_number, None);
        }
    });
    Ok(())
}

pub(crate) fn has_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    let Ok(origin) = consent_origin(&origin) else {
        return false;
    };
    storage_borrow(|storage| {
        storage
            .notification_application(anchor_number, &origin)
            .and_then(|application_number| {
                storage.notification_consent(anchor_number, application_number)
            })
            .is_some()
    })
}

// ---- caller-facing entry points (called from main.rs's thin wrappers) ----

/// Grants `origin` permission to notify the caller's anchor.
///
/// `SessionMissing` for an origin the identity has never signed in at: consent hangs off
/// the application, which only a sign-in mints.
pub fn grant_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    set_consent(anchor_number, origin, ic_cdk::api::time())
}

/// Revokes `origin`'s consent. Device subscriptions stay: they are shared across every
/// consented app.
pub fn revoke_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    clear_consent(anchor_number, origin)
}

/// Whether `origin` may notify this identity. `false` for an unauthorized or disabled
/// call, so neither can be probed.
pub fn consent_status(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    feature_enabled() && authorize_query(anchor_number) && has_consent(anchor_number, origin)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::FRONTEND_HOSTNAME_LIMIT;
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

        set_consent(anchor, origin.clone(), 1_000).unwrap();
        assert!(has_consent(anchor, origin.clone()));

        clear_consent(anchor, origin.clone()).unwrap();
        assert!(!has_consent(anchor, origin));
    }

    /// Consent hangs off the application, which only a sign-in mints.
    #[test]
    fn refuses_an_app_the_identity_has_never_signed_in_at() {
        setup();
        let anchor = anchor_at("https://visited.example");

        assert_eq!(
            set_consent(anchor, "https://never.example".to_string(), 1_000),
            Err(NotificationError::SessionMissing)
        );
    }

    #[test]
    fn revoking_unconsented_origin_is_a_harmless_no_op() {
        setup();
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        assert!(clear_consent(anchor, origin).is_ok());
        assert!(clear_consent(anchor, "https://never.example".to_string()).is_ok());
    }

    #[test]
    fn consent_rejects_oversized_origin() {
        setup();
        let too_long = "a".repeat(FRONTEND_HOSTNAME_LIMIT + 1);
        assert!(set_consent(1, too_long, 0).is_err());
    }

    #[test]
    fn consent_rejects_non_https_origin() {
        setup();
        assert!(set_consent(1, "http://app.example".to_string(), 0).is_err());
    }

    /// A grant naming a modern gateway finds the row the sign-in created under the
    /// legacy one.
    #[test]
    fn a_gateway_twin_is_the_same_consent() {
        setup();
        let anchor = anchor_at("https://abc-cai.ic0.app");

        set_consent(anchor, "https://abc-cai.icp0.io".to_string(), 1_000).unwrap();
        assert!(has_consent(anchor, "https://abc-cai.ic0.app".to_string()));
        assert!(has_consent(anchor, "https://abc-cai.icp.net".to_string()));

        clear_consent(anchor, "https://abc-cai.icp.net".to_string()).unwrap();
        assert!(!has_consent(anchor, "https://abc-cai.icp0.io".to_string()));
    }

    /// Consent and the default account share one config row.
    #[test]
    fn consent_leaves_the_default_account_alone() {
        setup();
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        let application = storage_borrow(|s| {
            s.notification_application(anchor, &origin)
                .expect("the sign-in stored one")
        });
        let before = storage_borrow(|s| {
            s.lookup_anchor_application_config(anchor, application)
                .default_account_number
        });

        set_consent(anchor, origin.clone(), 1_000).unwrap();

        let after = storage_borrow(|s| {
            s.lookup_anchor_application_config(anchor, application)
                .default_account_number
        });
        assert_eq!(before, after);
        assert!(has_consent(anchor, origin));
    }
}
