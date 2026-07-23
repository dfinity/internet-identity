//! Tests for the `discover_sso` (update, drives) / `get_sso_discovery_status`
//! (query, reads status) discovery flow. There is no domain allowlist: any
//! bare-authority domain is accepted and reads `Pending` until its discovery
//! fetch lands. (Domain validation and the `https`/loopback scheme rules are
//! unit-tested in `openid::sso`.)

use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::SsoDiscoveryStatus;

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
