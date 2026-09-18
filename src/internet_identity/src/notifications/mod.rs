//! Notifications: who may notify a user, and how they're reached.
//!
//! Each `pub fn` checks the feature flag and authorizes the anchor, then delegates to a
//! storage-only helper taking the resolved `anchor_number`, which keeps the storage logic
//! testable off-canister.

pub mod consent;

use crate::authz_utils::{
    check_authorization, check_authz_and_record_activity, IdentityUpdateError,
};
use crate::delegation::frontend_length_within_limit;
use ic_cdk::caller;
use internet_identity_interface::internet_identity::types::attributes::remap_to_legacy_domain;
pub use internet_identity_interface::internet_identity::types::NotificationError;
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};
use url::Url;

/// Accepts a length-bounded, bare `https://host[:port]`. Anything else is rejected
/// rather than trimmed, so one app cannot hold two consent rows.
fn validate_origin(origin: &str) -> Result<(), NotificationError> {
    let invalid = |reason: &str| Err(NotificationError::InvalidOrigin(reason.to_string()));

    if origin.is_empty() {
        return invalid("origin is empty");
    }
    // Before parsing, so an enormous string is refused without being parsed.
    frontend_length_within_limit(&origin.to_string()).map_err(NotificationError::InvalidOrigin)?;

    let Ok(url) = Url::parse(origin) else {
        return invalid("origin is not a URL");
    };
    if url.scheme() != "https" {
        return invalid("origin must be an https:// URL");
    }
    // One comparison against the browser's own serialization, so credentials, path,
    // query, fragment, trailing slash, explicit :443 and uppercase host all fail here.
    if url.origin().ascii_serialization() != origin {
        return invalid("origin must be a bare https://host[:port]");
    }
    Ok(())
}

/// Validates an origin and folds it to its canonical spelling, through the attribute
/// path's `remap_to_legacy_domain` so the fold matches the one the frontend derives
/// principals with. Every path that turns an origin into a consent key goes through this.
fn consent_origin(origin: &str) -> Result<FrontendHostname, NotificationError> {
    validate_origin(origin)?;
    Ok(remap_to_legacy_domain(origin))
}

fn feature_enabled() -> bool {
    crate::state::persistent_state(|s| s.notifications_enabled.unwrap_or(false))
}

/// Server-side kill switch; every entry point checks it first.
fn check_enabled() -> Result<(), NotificationError> {
    if feature_enabled() {
        Ok(())
    } else {
        Err(NotificationError::Disabled)
    }
}

/// Whether the anchor is one this canister holds.
///
/// Read directly, because the authorization helpers below reach it through
/// `state::anchor`, which traps on an unregistered number, and a trap tells a caller
/// which numbers are free.
fn anchor_exists(anchor_number: AnchorNumber) -> bool {
    crate::state::storage_borrow(|storage| storage.read(anchor_number)).is_ok()
}

/// Authorize an update via the standard activity-recording gate. Takes the anchor as an
/// argument, not a caller reverse-lookup, so it works for OpenID-only identities too.
fn authorize_update(anchor_number: AnchorNumber) -> Result<(), NotificationError> {
    if !anchor_exists(anchor_number) {
        // Word for word what a wrong caller gets, so the two cannot be told apart.
        return Err(NotificationError::Unauthorized(
            IdentityUpdateError::Unauthorized(caller()).to_string(),
        ));
    }
    check_authz_and_record_activity(anchor_number)
        .map_err(|err| NotificationError::Unauthorized(err.to_string()))?;
    Ok(())
}

/// Read-only authorization; a query must not record activity.
fn authorize_query(anchor_number: AnchorNumber) -> bool {
    anchor_exists(anchor_number) && check_authorization(anchor_number).is_ok()
}

#[cfg(test)]
pub(crate) fn test_setup() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;
    storage_replace(Storage::new((0, 100), VectorMemory::default()));
    storage_borrow_mut(|s| s.update_salt([7u8; 32]));
}

#[cfg(test)]
mod origin_tests {
    use super::{consent_origin, validate_origin};

    #[test]
    fn canonicalizes_every_gateway_to_the_legacy_one() {
        for origin in [
            "https://abc-cai.icp0.io",
            "https://abc-cai.icp.net",
            "https://abc-cai.ic0.app",
        ] {
            assert_eq!(consent_origin(origin).unwrap(), "https://abc-cai.ic0.app");
        }
    }

    #[test]
    fn leaves_a_custom_domain_alone() {
        assert_eq!(
            consent_origin("https://oisy.com").unwrap(),
            "https://oisy.com"
        );
    }

    /// The frontend only remaps a single `[\w-]+(.raw)?` label, so a deeper name is a
    /// different origin to it and must stay a different consent row here.
    #[test]
    fn leaves_a_deeper_subdomain_alone() {
        assert_eq!(
            consent_origin("https://foo.bar.icp0.io").unwrap(),
            "https://foo.bar.icp0.io"
        );
    }

    #[test]
    fn keeps_the_raw_label_the_frontend_keeps() {
        assert_eq!(
            consent_origin("https://abc-cai.raw.icp0.io").unwrap(),
            "https://abc-cai.raw.ic0.app"
        );
    }

    /// A browser never serializes userinfo into an origin.
    #[test]
    fn rejects_credentials_in_the_authority() {
        for origin in [
            "https://user@app.example",
            "https://user:pass@app.example",
            "https://@app.example",
        ] {
            assert!(
                validate_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }

    /// Each parses far enough to look like an authority, and none is a spelling a
    /// browser can hand an app.
    #[test]
    fn rejects_an_authority_no_browser_can_serialize() {
        for origin in [
            // Out of range for a port, which is a u16.
            "https://app.example:99999",
            // Not a host: a space cannot appear in one.
            "https://not a host",
            // The default port, which a browser drops, so this keys a second row for one app.
            "https://app.example:443",
            // A browser lowercases the host, for the same reason.
            "https://APP.example",
            // An origin carries no path at all, not even the empty one.
            "https://app.example/",
        ] {
            assert!(
                validate_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }

    #[test]
    fn rejects_an_over_long_origin() {
        let too_long = format!(
            "https://{}",
            "a".repeat(crate::delegation::FRONTEND_HOSTNAME_LIMIT)
        );
        assert!(validate_origin(&too_long).is_err());
    }

    #[test]
    fn accepts_a_bare_origin_with_an_optional_port() {
        validate_origin("https://app.example").unwrap();
        validate_origin("https://app.example:8443").unwrap();
    }

    /// Anything past the authority would let one app hold several consent rows.
    #[test]
    fn rejects_anything_past_the_authority() {
        for origin in [
            "https://app.example/",
            "https://app.example/path",
            "https://app.example?q=1",
            "https://app.example#frag",
            "https://",
            "https://:443",
            "https://app.example:",
            "https://app.example:https",
            "http://app.example",
        ] {
            assert!(
                validate_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }
}
