use canister_tests::api::internet_identity as api;
use canister_tests::framework::{env, install_ii_canister_with_arg, II_WASM};
use internet_identity_interface::internet_identity::types::{
    DiscoverableOidcConfig, InternetIdentityInit,
};

/// Built-in fallback when neither `is_production` nor
/// `sso_discoverable_domains` is set in the init args. Keep in sync with
/// `openid::generic::allowed_discovery_domains()`.
const BETA_ALLOWED_DOMAIN: &str = "beta.dfinity.org";

/// Built-in fallback for production installs when no explicit
/// `sso_discoverable_domains` is set. Keep in sync with
/// `openid::generic::allowed_discovery_domains()`.
const PROD_ALLOWED_DOMAIN: &str = "dfinity.org";

fn example_oidc_config() -> DiscoverableOidcConfig {
    DiscoverableOidcConfig {
        discovery_domain: BETA_ALLOWED_DOMAIN.into(),
    }
}

#[test]
fn should_init_without_oidc_configs() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert!(discovered.is_empty());
}

#[test]
fn should_add_oidc_config_via_update_call() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);

    api::add_discoverable_oidc_config(&env, canister_id, example_oidc_config()).unwrap();

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert_eq!(discovered.len(), 1);
    assert_eq!(discovered[0].discovery_domain, BETA_ALLOWED_DOMAIN);
    // Discovery requires HTTP outcalls, which don't run in PocketIC tests,
    // so every discovered field is `None` on init.
    assert_eq!(discovered[0].client_id, None);
    assert_eq!(discovered[0].openid_configuration, None);
    assert_eq!(discovered[0].issuer, None);
}

#[test]
fn should_deduplicate_oidc_configs() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);

    api::add_discoverable_oidc_config(&env, canister_id, example_oidc_config()).unwrap();
    api::add_discoverable_oidc_config(&env, canister_id, example_oidc_config()).unwrap();

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert_eq!(discovered.len(), 1);
}

#[test]
fn should_coexist_with_openid_configs() {
    let env = env();
    let config = InternetIdentityInit {
        openid_configs: Some(vec![
            internet_identity_interface::internet_identity::types::OpenIdConfig {
                name: "Example".into(),
                logo: String::new(),
                issuer: "https://example.com".into(),
                client_id: "app.example.com".into(),
                jwks_uri: "https://example.com/oauth2/v3/certs".into(),
                auth_uri: "https://example.com/o/oauth2/v2/auth".into(),
                auth_scope: vec!["openid".into()],
                fedcm_uri: None,
                email_verification: None,
            },
        ]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Add OIDC config alongside existing openid_configs
    api::add_discoverable_oidc_config(&env, canister_id, example_oidc_config()).unwrap();

    // `config()` reflects the init-arg-driven `openid_configs`.
    let result = api::config(&env, canister_id).unwrap();
    assert!(result.openid_configs.is_some());

    // SSO providers are registered via `add_discoverable_oidc_config` and
    // surfaced through `discovered_oidc_configs`, not through init-args
    // round-trip.
    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert_eq!(discovered.len(), 1);
    assert_eq!(discovered[0].discovery_domain, BETA_ALLOWED_DOMAIN);
}

/// On a non-production install (`is_production` unset / false), the prod
/// allowlist entry (`dfinity.org`) is rejected and only `beta.dfinity.org`
/// is accepted. Keeping the two disjoint ensures a DNS takeover of the
/// beta test domain can't backdoor the production canister.
#[test]
fn should_reject_disallowed_discovery_domain() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);

    let result = api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: "evil.example.com".into(),
        },
    );
    assert!(result.is_err());

    // Production domain is rejected on a non-production canister.
    let result = api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: PROD_ALLOWED_DOMAIN.into(),
        },
    );
    assert!(result.is_err());
}

/// On a production install (`is_production == Some(true)`), only
/// `dfinity.org` is allowed; `beta.dfinity.org` is rejected.
#[test]
fn should_allow_only_production_domain_on_production_canister() {
    let env = env();
    let config = InternetIdentityInit {
        is_production: Some(true),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Beta domain is rejected on a production canister.
    let rejected = api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: BETA_ALLOWED_DOMAIN.into(),
        },
    );
    assert!(rejected.is_err());

    // Production domain is accepted.
    api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: PROD_ALLOWED_DOMAIN.into(),
        },
    )
    .unwrap();

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert_eq!(discovered.len(), 1);
    assert_eq!(discovered[0].discovery_domain, PROD_ALLOWED_DOMAIN);
}

/// `sso_discoverable_domains` (when set) replaces the built-in defaults.
/// This is what e2e tests rely on to register `localhost:11107`, and what
/// production installs will use to widen the canary list once SSO exits
/// the proof-of-concept phase.
#[test]
fn should_honour_explicit_sso_discoverable_domains() {
    let env = env();
    let config = InternetIdentityInit {
        sso_discoverable_domains: Some(vec!["localhost:11107".into(), "acme.example".into()]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Built-in default (`beta.dfinity.org`) is no longer accepted — the
    // explicit list replaces it rather than extending it.
    let rejected = api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: BETA_ALLOWED_DOMAIN.into(),
        },
    );
    assert!(rejected.is_err());

    // Both explicit entries are accepted.
    api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: "localhost:11107".into(),
        },
    )
    .unwrap();
    api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: "acme.example".into(),
        },
    )
    .unwrap();

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    let domains: Vec<String> = discovered
        .into_iter()
        .map(|d| d.discovery_domain)
        .collect();
    assert_eq!(domains.len(), 2);
    assert!(domains.contains(&"localhost:11107".to_string()));
    assert!(domains.contains(&"acme.example".to_string()));
}

/// Setting `sso_discoverable_domains` overrides `is_production`: even on
/// a production install, the explicit list is what gates registration.
#[test]
fn should_honour_explicit_list_over_is_production() {
    let env = env();
    let config = InternetIdentityInit {
        is_production: Some(true),
        sso_discoverable_domains: Some(vec!["test.id.ai".into()]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Production default (`dfinity.org`) is no longer accepted.
    let rejected = api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: PROD_ALLOWED_DOMAIN.into(),
        },
    );
    assert!(rejected.is_err());

    // The explicit entry is accepted.
    api::add_discoverable_oidc_config(
        &env,
        canister_id,
        DiscoverableOidcConfig {
            discovery_domain: "test.id.ai".into(),
        },
    )
    .unwrap();
}
