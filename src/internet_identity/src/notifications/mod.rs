//! Notifications: who may notify a user, and how they're reached.
//!
//! Consent is channel-agnostic and lives here; channel-specific state lives
//! under [`webpush`]. Each `pub fn` checks the feature flag and authorizes the
//! anchor, then delegates to a storage-only helper taking the resolved
//! `anchor_number` — keeping the storage logic testable off-canister.

pub mod consent;
pub mod webpush;

use crate::authz_utils::{check_authorization, check_authz_and_record_activity};
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};

/// Bounds the origin so `delegation::get_principal` can't trap on it — the same
/// 255-byte limit `delegation::check_frontend_length` traps on.
pub const MAX_ORIGIN_LEN: usize = 255;

/// Why a notification call was refused. A variant rather than a string: a
/// caller cannot branch on prose, and the shape is free to choose before the
/// first release and breaking after it.
#[derive(candid::CandidType, serde::Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum NotificationError {
    /// This deployment has notifications turned off.
    Disabled,
    /// The caller may not act for this anchor.
    Unauthorized(String),
    /// The origin is not a bare `https://host[:port]`.
    InvalidOrigin(String),
    /// Subscription fields were rejected; every failure is reported at once.
    InvalidSubscription(Vec<String>),
    /// No such subscribed endpoint, or no consent for that origin.
    NotFound,
}

/// A consent origin must be a length-bounded, bare `https://host[:port]`. The
/// scheme check mirrors the delegation path, which treats non-`https` frontends
/// as insecure/dev. Anything past the authority is rejected rather than
/// trimmed, so one app cannot hold two consent rows.
fn validate_origin(origin: &str) -> Result<(), NotificationError> {
    let invalid = |reason: &str| Err(NotificationError::InvalidOrigin(reason.to_string()));

    if origin.is_empty() || origin.len() > MAX_ORIGIN_LEN {
        return Err(NotificationError::InvalidOrigin(format!(
            "origin length {} out of range (1..={MAX_ORIGIN_LEN})",
            origin.len()
        )));
    }
    let Some(authority) = origin.strip_prefix("https://") else {
        return invalid("origin must be an https:// URL");
    };
    if authority.contains(['/', '?', '#']) {
        return invalid("origin must not carry a path, query or fragment");
    }
    let (host, port) = match authority.split_once(':') {
        Some((host, port)) => (host, Some(port)),
        None => (authority, None),
    };
    if host.is_empty() {
        return invalid("origin has no host");
    }
    if port.is_some_and(|port| port.is_empty() || !port.bytes().all(|b| b.is_ascii_digit())) {
        return invalid("origin port must be numeric");
    }
    Ok(())
}

/// The gateway domains a canister subdomain can be served through. The
/// frontend canonicalizes `*.icp0.io` / `*.icp.net` to the legacy `*.ic0.app`
/// so an identity's principal is the same whichever gateway it signed in
/// through, which makes the canonical origin a stable key but not necessarily a
/// reachable URL.
const LEGACY_GATEWAY: &str = ".ic0.app";
const GATEWAYS: [&str; 3] = [".ic0.app", ".icp0.io", ".icp.net"];

/// Rewrites a canister-subdomain origin to the legacy gateway, mirroring the
/// frontend's `remapToLegacyDomain`. Applied to any origin an untrusted caller
/// declares, so it keys against the same consent the sign-in recorded no matter
/// which gateway the caller names. Anything else (a custom domain) is returned
/// unchanged.
fn canonical_origin(origin: &str) -> String {
    match subdomain_of(origin) {
        Some(subdomain) => format!("https://{subdomain}{LEGACY_GATEWAY}"),
        None => origin.to_string(),
    }
}

/// The subdomain of a `https://<subdomain>.<gateway>` origin, or `None` when the
/// origin is not a canister subdomain on a known gateway.
fn subdomain_of(origin: &str) -> Option<&str> {
    let host = origin.strip_prefix("https://")?;
    if host.contains('/') {
        return None;
    }
    GATEWAYS
        .iter()
        .find_map(|gateway| host.strip_suffix(gateway))
        .filter(|subdomain| !subdomain.is_empty())
}

/// Validates an origin and folds it to its canonical spelling in one step.
/// Every path that turns an origin into a consent key goes through this: a
/// grant naming `https://app.icp0.io` and one naming `https://app.ic0.app` are
/// the same app, and keying them apart makes the first grant unrevocable and
/// undeliverable.
fn consent_origin(origin: &str) -> Result<FrontendHostname, NotificationError> {
    validate_origin(origin)?;
    Ok(canonical_origin(origin))
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

/// Authorize an update via the standard activity-recording gate. Takes the
/// anchor as an argument (not a caller reverse-lookup) so it works for
/// OpenID-only identities too.
fn authorize_update(anchor_number: AnchorNumber) -> Result<(), NotificationError> {
    check_authz_and_record_activity(anchor_number)
        .map_err(|err| NotificationError::Unauthorized(err.to_string()))?;
    Ok(())
}

/// Read-only authorization; a query must not record activity.
fn authorize_query(anchor_number: AnchorNumber) -> bool {
    check_authorization(anchor_number).is_ok()
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
    use super::{canonical_origin, validate_origin};

    #[test]
    fn canonicalizes_every_gateway_to_the_legacy_one() {
        for origin in [
            "https://abc-cai.icp0.io",
            "https://abc-cai.icp.net",
            "https://abc-cai.ic0.app",
        ] {
            assert_eq!(canonical_origin(origin), "https://abc-cai.ic0.app");
        }
    }

    #[test]
    fn leaves_a_custom_domain_alone() {
        assert_eq!(canonical_origin("https://oisy.com"), "https://oisy.com");
    }

    #[test]
    fn accepts_a_bare_origin_with_an_optional_port() {
        validate_origin("https://app.example").unwrap();
        validate_origin("https://app.example:8443").unwrap();
    }

    /// Anything past the authority would let one app hold several consent rows,
    /// only one of which the send path would ever find.
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
