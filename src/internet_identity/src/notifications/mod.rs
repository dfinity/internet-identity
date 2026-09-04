//! Notifications: who may notify a user, and how they're reached.
//!
//! Consent is channel-agnostic and lives here; channel-specific state lives
//! under [`push`]. Each `pub fn` checks the feature flag and authorizes the
//! anchor, then delegates to a storage-only helper taking the resolved
//! `anchor_number` — keeping the storage logic testable off-canister.

pub mod consent;
pub mod send;
pub mod sender;
pub mod webpush;
pub mod well_known;

use crate::authz_utils::{check_authorization, check_authz_and_record_activity};
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// Bounds the origin so `delegation::get_principal` can't trap on it — the same
/// 255-byte limit `delegation::check_frontend_length` traps on.
pub const MAX_ORIGIN_LEN: usize = 255;

/// A consent origin must be a length-bounded `https://` URL. The scheme check
/// mirrors the delegation path, which treats non-`https` frontends as
/// insecure/dev.
fn validate_origin(origin: &str) -> Result<(), String> {
    if origin.is_empty() || origin.len() > MAX_ORIGIN_LEN {
        return Err(format!(
            "origin length {} out of range (1..={MAX_ORIGIN_LEN})",
            origin.len()
        ));
    }
    if !origin.starts_with("https://") {
        return Err("origin must be an https:// URL".to_string());
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

/// Where to look for a canister subdomain's well-known, most likely first. Each
/// attempt is a replicated outcall costing seconds, so the order matters: the
/// canonical origin names the legacy gateway, which is the one least likely to
/// serve a recent canister, and starting there would spend an outcall to learn
/// nothing. A custom domain is only ever served at itself.
const FETCH_GATEWAYS: [&str; 3] = [".icp0.io", ".ic0.app", ".icp.net"];

fn fetch_origins(origin: &str) -> Vec<String> {
    match subdomain_of(origin) {
        Some(subdomain) => FETCH_GATEWAYS
            .iter()
            .map(|gateway| format!("https://{subdomain}{gateway}"))
            .collect(),
        None => vec![origin.to_string()],
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

fn feature_enabled() -> bool {
    crate::state::persistent_state(|s| s.notifications_enabled.unwrap_or(false))
}

/// Server-side kill switch; every entry point checks it first.
fn check_enabled() -> Result<(), String> {
    if feature_enabled() {
        Ok(())
    } else {
        Err("notifications are disabled".to_string())
    }
}

/// Authorize an update via the standard activity-recording gate. Takes the
/// anchor as an argument (not a caller reverse-lookup) so it works for
/// OpenID-only identities too.
fn authorize_update(anchor_number: AnchorNumber) -> Result<(), String> {
    check_authz_and_record_activity(anchor_number).map_err(|err| format!("Unauthorized: {err}"))?;
    Ok(())
}

/// Read-only authorization; a query must not record activity.
fn authorize_query(anchor_number: AnchorNumber) -> bool {
    check_authorization(anchor_number).is_ok()
}

// ---- storage-only helpers ----

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
    use super::{canonical_origin, fetch_origins};

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
        assert_eq!(
            fetch_origins("https://oisy.com"),
            vec!["https://oisy.com"],
            "a custom domain is only served at itself"
        );
    }

    #[test]
    fn fetches_a_canister_subdomain_from_the_modern_gateway_first() {
        assert_eq!(
            fetch_origins("https://abc-cai.ic0.app"),
            vec![
                "https://abc-cai.icp0.io",
                "https://abc-cai.ic0.app",
                "https://abc-cai.icp.net"
            ],
            "the legacy gateway must not cost the first outcall"
        );
    }
}
