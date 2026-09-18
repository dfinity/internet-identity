//! Per-app consent: whether an app may notify an identity.

use super::{
    authorize_query, authorize_update, check_enabled, consent_origin, feature_enabled,
    NotificationError,
};
use crate::state::{storage_borrow, storage_borrow_mut};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ConsentStatusRequest, FrontendHostname, NotificationGrantConsentRequest,
    NotificationRevokeConsentRequest, Timestamp,
};

/// Records `consented_at_ns` as `origin`'s consent for `anchor_number`, or withdraws it
/// with `None`.
///
/// A grant needs an application to hang off and is refused without one. A withdrawal is
/// not: consent that cannot exist is already withdrawn.
fn set_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    consented_at_ns: Option<Timestamp>,
) -> Result<(), NotificationError> {
    let origin = consent_origin(&origin)?;

    storage_borrow_mut(|storage| {
        let mut config = storage
            .read_anchor_application_config(anchor_number, &origin)
            .unwrap_or_default();
        config.notifications_consented_at_ns = consented_at_ns;
        match storage.write_anchor_application_config(anchor_number, &origin, config) {
            Ok(()) => Ok(()),
            Err(_) if consented_at_ns.is_some() => Err(NotificationError::SessionMissing),
            Err(_) => Ok(()),
        }
    })
}

pub(crate) fn has_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    let Ok(origin) = consent_origin(&origin) else {
        return false;
    };
    storage_borrow(|storage| {
        storage
            .read_anchor_application_config(anchor_number, &origin)
            .and_then(|config| config.notifications_consented_at_ns)
            .is_some()
    })
}

// ---- caller-facing entry points (called from main.rs's thin wrappers) ----

/// Grants `origin` permission to notify the caller's anchor.
///
/// `SessionMissing` for an origin the identity has never signed in at: consent hangs off
/// the application, which only a sign-in mints.
pub fn grant_consent(
    NotificationGrantConsentRequest {
        anchor_number,
        origin,
    }: NotificationGrantConsentRequest,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    set_consent(anchor_number, origin, Some(ic_cdk::api::time()))
}

/// Revokes `origin`'s consent. Device subscriptions stay: they are shared across every
/// consented app.
pub fn revoke_consent(
    NotificationRevokeConsentRequest {
        anchor_number,
        origin,
    }: NotificationRevokeConsentRequest,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    set_consent(anchor_number, origin, None)
}

/// Whether `origin` may notify this identity. `false` for an unauthorized or disabled
/// call, so neither can be probed.
pub fn consent_status(
    ConsentStatusRequest {
        anchor_number,
        origin,
    }: ConsentStatusRequest,
) -> bool {
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

        set_consent(anchor, origin.clone(), Some(1_000)).unwrap();
        assert!(has_consent(anchor, origin.clone()));

        set_consent(anchor, origin.clone(), None).unwrap();
        assert!(!has_consent(anchor, origin));
    }

    /// Consent hangs off the application, which only a sign-in mints.
    #[test]
    fn refuses_an_app_the_identity_has_never_signed_in_at() {
        setup();
        let anchor = anchor_at("https://visited.example");

        assert_eq!(
            set_consent(anchor, "https://never.example".to_string(), Some(1_000)),
            Err(NotificationError::SessionMissing)
        );
    }

    #[test]
    fn revoking_unconsented_origin_is_a_harmless_no_op() {
        setup();
        let origin = "https://app.example".to_string();
        let anchor = anchor_at(&origin);

        assert!(set_consent(anchor, origin, None).is_ok());
        assert!(set_consent(anchor, "https://never.example".to_string(), None).is_ok());
    }

    #[test]
    fn consent_rejects_oversized_origin() {
        setup();
        let too_long = "a".repeat(FRONTEND_HOSTNAME_LIMIT + 1);
        assert!(set_consent(1, too_long, Some(0)).is_err());
    }

    #[test]
    fn consent_rejects_non_https_origin() {
        setup();
        assert!(set_consent(1, "http://app.example".to_string(), Some(0)).is_err());
    }

    /// A grant naming a modern gateway finds the row the sign-in created under the
    /// legacy one.
    #[test]
    fn a_gateway_twin_is_the_same_consent() {
        setup();
        let anchor = anchor_at("https://abc-cai.ic0.app");

        set_consent(anchor, "https://abc-cai.icp0.io".to_string(), Some(1_000)).unwrap();
        assert!(has_consent(anchor, "https://abc-cai.ic0.app".to_string()));
        assert!(has_consent(anchor, "https://abc-cai.icp.net".to_string()));

        set_consent(anchor, "https://abc-cai.icp.net".to_string(), None).unwrap();
        assert!(!has_consent(anchor, "https://abc-cai.icp0.io".to_string()));
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

        set_consent(anchor, origin.clone(), Some(1_000)).unwrap();

        let after = storage_borrow(|s| {
            s.read_anchor_application_config(anchor, &origin)
                .and_then(|config| config.default_account_number)
        });
        assert_eq!(before, after);
        assert!(has_consent(anchor, origin));
    }
}
