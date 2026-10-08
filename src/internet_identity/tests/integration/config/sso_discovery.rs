//! Tests for the `discover_sso` (update, drives) / `get_sso_discovery_status`
//! (query, reads status) discovery flow, and its app-facing counterpart
//! `app_sso_domain_check` / `app_sso_domain_status`. There is no domain
//! allowlist: any bare-authority domain is accepted and reads `Pending` until
//! its discovery fetch lands, while anything else fails at once. (Domain
//! validation, the `https`/loopback scheme rules, and failed fetches are
//! unit-tested in `openid::sso`.)

use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::{
    AppSsoDomainStatus, SsoDiscoveryStatus,
};

/// With no domain allowlist, any (uncached) discovery domain reads `Pending`,
/// and driving it is accepted (a no-op until the fetch lands).
#[test]
fn sso_discovery_accepts_any_domain() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg_and_cycles(&env, II_WASM.clone(), None, 10_000_000_000_000);

    for domain in ["example.com", "sub.example.org", "some-idp.test"] {
        assert_eq!(
            api::get_sso_discovery(&env, canister_id, domain).unwrap(),
            SsoDiscoveryStatus::Pending
        );
        api::discover_sso(&env, canister_id, domain).unwrap();
    }
}

/// A domain that is not a bare authority fails at once, with no retry time,
/// on both the frontend and the app-facing status.
#[test]
fn sso_discovery_fails_a_malformed_domain_at_once() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg_and_cycles(&env, II_WASM.clone(), None, 10_000_000_000_000);

    for domain in ["evil.com@127.0.0.1", "example.com/path"] {
        assert_eq!(
            api::get_sso_discovery(&env, canister_id, domain).unwrap(),
            SsoDiscoveryStatus::Failed { retry_after: None }
        );
        api::app_sso_domain_check(&env, canister_id, domain).unwrap();
        assert_eq!(
            api::app_sso_domain_status(&env, canister_id, domain).unwrap(),
            AppSsoDomainStatus::Unavailable { retry_after: None }
        );
    }
}

/// An uncached bare-authority domain reads `Pending` on the app-facing status,
/// and checking it is accepted.
#[test]
fn app_sso_domain_status_is_pending_for_an_uncached_domain() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg_and_cycles(&env, II_WASM.clone(), None, 10_000_000_000_000);

    assert_eq!(
        api::app_sso_domain_status(&env, canister_id, "example.com").unwrap(),
        AppSsoDomainStatus::Pending
    );
    api::app_sso_domain_check(&env, canister_id, "example.com").unwrap();
}
