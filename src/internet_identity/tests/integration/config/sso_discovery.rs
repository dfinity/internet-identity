//! Tests for the `discover_sso` (update, drives) / `get_sso_discovery` (query,
//! reads state) allowlist gate.

use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::{
    InternetIdentityInit, SsoDiscoveryState,
};

/// On a non-production install only `beta.dfinity.org` is allowed by default;
/// the query reports `NotAllowed` for other domains (including the production
/// domain) and `Pending` for the allowed one.
#[test]
fn sso_discovery_gates_on_allowlist() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg_and_cycles(&env, II_WASM.clone(), None, 10_000_000_000_000);

    // Arbitrary and production domains are not allowed on a non-production canister.
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "evil.example.com").unwrap(),
        SsoDiscoveryState::NotAllowed
    );
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "dfinity.org").unwrap(),
        SsoDiscoveryState::NotAllowed
    );

    // The default non-production domain is allowed and reads `Pending` until
    // warm; driving it is accepted (a no-op until the fetch lands).
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "beta.dfinity.org").unwrap(),
        SsoDiscoveryState::Pending
    );
    api::discover_sso(&env, canister_id, "beta.dfinity.org").unwrap();
}

/// An explicit `sso_discoverable_domains` allowlist replaces the built-in
/// defaults.
#[test]
fn sso_discovery_honours_explicit_allowlist() {
    let env = env();
    let arg = InternetIdentityInit {
        sso_discoverable_domains: Some(vec!["example.com".to_string()]),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(arg),
        10_000_000_000_000,
    );

    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "example.com").unwrap(),
        SsoDiscoveryState::Pending
    );
    // The default domain is no longer allowed once an explicit list is set.
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "beta.dfinity.org").unwrap(),
        SsoDiscoveryState::NotAllowed
    );
}

/// The `sso_allow_any_domain` deploy flag opens the gate to every domain: a
/// domain off the allowlist reads `Pending` instead of `NotAllowed`.
#[test]
fn sso_allow_any_domain_opens_the_gate() {
    let env = env();
    let arg = InternetIdentityInit {
        sso_discoverable_domains: Some(vec!["example.com".to_string()]),
        sso_allow_any_domain: Some(true),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(arg),
        10_000_000_000_000,
    );

    // The explicit entry is still allowed.
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "example.com").unwrap(),
        SsoDiscoveryState::Pending
    );
    // A domain off the allowlist is now accepted rather than `NotAllowed`.
    assert_eq!(
        api::get_sso_discovery(&env, canister_id, "evil.example.com").unwrap(),
        SsoDiscoveryState::Pending
    );
    api::discover_sso(&env, canister_id, "evil.example.com").unwrap();
}
