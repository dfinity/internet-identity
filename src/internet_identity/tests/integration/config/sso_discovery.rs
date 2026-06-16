//! Tests for the `discover_sso` (update, drives) / `get_sso_discovery` (query,
//! reads) allowlist gate.

use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::InternetIdentityInit;

/// On a non-production install only `beta.dfinity.org` is allowed by default;
/// other domains (including the production domain) are rejected by both the
/// drive (update) and the read (query) before any discovery fetch is started.
#[test]
fn sso_discovery_rejects_disallowed_domain() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg_and_cycles(&env, II_WASM.clone(), None, 10_000_000_000_000);

    // An arbitrary domain is rejected.
    assert!(api::discover_sso(&env, canister_id, "evil.example.com")
        .unwrap()
        .is_err());
    assert!(
        api::get_sso_discovery(&env, canister_id, "evil.example.com")
            .unwrap()
            .is_err()
    );

    // The production domain is rejected on a non-production canister.
    assert!(api::discover_sso(&env, canister_id, "dfinity.org")
        .unwrap()
        .is_err());

    // The default non-production domain is allowed: the drive kicks off the
    // fetch (Ok) and the query reads no value yet (Ok(None)) — neither errors.
    assert_eq!(
        api::discover_sso(&env, canister_id, "beta.dfinity.org").unwrap(),
        Ok(())
    );
    assert!(matches!(
        api::get_sso_discovery(&env, canister_id, "beta.dfinity.org").unwrap(),
        Ok(None)
    ));
}

/// An explicit `sso_discoverable_domains` allowlist replaces the built-in
/// defaults, for both the drive and the read.
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

    // The allowlisted domain is accepted; the query reads `None` until warm.
    assert_eq!(
        api::discover_sso(&env, canister_id, "example.com").unwrap(),
        Ok(())
    );
    assert!(matches!(
        api::get_sso_discovery(&env, canister_id, "example.com").unwrap(),
        Ok(None)
    ));

    // The default domain is no longer allowed once an explicit list is set.
    assert!(api::discover_sso(&env, canister_id, "beta.dfinity.org")
        .unwrap()
        .is_err());
}
