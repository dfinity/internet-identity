//! Turning a caller's consent request into one the consent logic can act on: an origin
//! this deployment notifies for, spelled the one way it is keyed by.

use crate::delegation::frontend_length_within_limit;
use internet_identity_interface::internet_identity::types::attributes::remap_to_legacy_domain;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, NotificationConsentGrantedRequest,
    NotificationGrantConsentError, NotificationGrantConsentRequest, NotificationRevokeConsentError,
    NotificationRevokeConsentRequest,
};
use url::Url;

pub struct ValidatedNotificationGrantConsentRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
}

pub struct ValidatedNotificationRevokeConsentRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
}

pub struct ValidatedNotificationConsentGrantedRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
}

impl TryFrom<NotificationGrantConsentRequest> for ValidatedNotificationGrantConsentRequest {
    type Error = NotificationGrantConsentError;

    fn try_from(
        NotificationGrantConsentRequest {
            anchor_number,
            origin,
        }: NotificationGrantConsentRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)
                .map_err(NotificationGrantConsentError::InternalCanisterError)?,
        })
    }
}

impl TryFrom<NotificationRevokeConsentRequest> for ValidatedNotificationRevokeConsentRequest {
    type Error = NotificationRevokeConsentError;

    fn try_from(
        NotificationRevokeConsentRequest {
            anchor_number,
            origin,
        }: NotificationRevokeConsentRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)
                .map_err(NotificationRevokeConsentError::InternalCanisterError)?,
        })
    }
}

impl TryFrom<NotificationConsentGrantedRequest> for ValidatedNotificationConsentGrantedRequest {
    type Error = String;

    fn try_from(
        NotificationConsentGrantedRequest {
            anchor_number,
            origin,
        }: NotificationConsentGrantedRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)?,
        })
    }
}

/// Folds `origin` to the spelling consent is keyed by, and refuses one this deployment
/// does not notify for.
fn notifying_origin(origin: &str) -> Result<FrontendHostname, String> {
    let origin = remap_to_legacy_domain(&canonical_origin(origin)?);
    enabled_for(&origin)?;
    Ok(origin)
}

/// Accepts a length-bounded, bare `https://host[:port]`. Anything else is rejected
/// rather than trimmed, so one app cannot hold two consent rows.
fn canonical_origin(origin: &str) -> Result<FrontendHostname, String> {
    if origin.is_empty() {
        return Err("origin is empty".to_string());
    }
    // Before parsing, so an enormous string is refused without being parsed.
    frontend_length_within_limit(&origin.to_string())?;

    let Ok(url) = Url::parse(origin) else {
        return Err("origin is not a URL".to_string());
    };
    if url.scheme() != "https" {
        return Err("origin must be an https:// URL".to_string());
    }
    // One comparison against the browser's own serialization, so credentials, path,
    // query, fragment, trailing slash, explicit :443 and uppercase host all fail here.
    if url.origin().ascii_serialization() != origin {
        return Err("origin must be a bare https://host[:port]".to_string());
    }
    Ok(origin.to_string())
}

/// Server-side kill switch; every entry point goes through it.
fn enabled_for(origin: &FrontendHostname) -> Result<(), String> {
    if crate::state::persistent_state(|s| s.notifications_enabled.unwrap_or(false)) {
        Ok(())
    } else {
        Err(format!("notifications are not enabled for {origin}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::FRONTEND_HOSTNAME_LIMIT;

    fn enable(_origins: &[&str]) {
        crate::state::persistent_state_mut(|s| s.notifications_enabled = Some(true));
    }

    #[test]
    fn canonicalizes_every_gateway_to_the_legacy_one() {
        enable(&["https://abc-cai.ic0.app"]);
        for origin in [
            "https://abc-cai.icp0.io",
            "https://abc-cai.icp.net",
            "https://abc-cai.ic0.app",
        ] {
            assert_eq!(notifying_origin(origin).unwrap(), "https://abc-cai.ic0.app");
        }
    }

    #[test]
    fn leaves_a_custom_domain_alone() {
        enable(&["https://oisy.com"]);
        assert_eq!(
            notifying_origin("https://oisy.com").unwrap(),
            "https://oisy.com"
        );
    }

    /// The frontend only remaps a single `[\w-]+(.raw)?` label, so a deeper name is a
    /// different origin to it and must stay a different consent row here.
    #[test]
    fn leaves_a_deeper_subdomain_alone() {
        enable(&["https://foo.bar.icp0.io"]);
        assert_eq!(
            notifying_origin("https://foo.bar.icp0.io").unwrap(),
            "https://foo.bar.icp0.io"
        );
    }

    #[test]
    fn keeps_the_raw_label_the_frontend_keeps() {
        enable(&["https://abc-cai.raw.ic0.app"]);
        assert_eq!(
            notifying_origin("https://abc-cai.raw.icp0.io").unwrap(),
            "https://abc-cai.raw.ic0.app"
        );
    }

    #[test]
    fn refuses_every_origin_while_the_feature_is_off() {
        crate::state::persistent_state_mut(|s| s.notifications_enabled = None);
        assert!(notifying_origin("https://allowed.example").is_err());
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
                canonical_origin(origin).is_err(),
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
                canonical_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }

    #[test]
    fn rejects_an_over_long_origin() {
        let too_long = format!("https://{}", "a".repeat(FRONTEND_HOSTNAME_LIMIT));
        assert!(canonical_origin(&too_long).is_err());
    }

    #[test]
    fn accepts_a_bare_origin_with_an_optional_port() {
        canonical_origin("https://app.example").unwrap();
        canonical_origin("https://app.example:8443").unwrap();
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
                canonical_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }
}
