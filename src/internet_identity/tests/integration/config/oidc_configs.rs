use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    DiscoverableOidcConfig, InternetIdentityInit, OpenIdConfig,
};

fn example_oidc_config() -> DiscoverableOidcConfig {
    DiscoverableOidcConfig {
        // Must be on the canary allowlist in `openid::generic::ALLOWED_DISCOVERY_DOMAINS`.
        discovery_domain: "dfinity.org".into(),
    }
}

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(api::config(&env, canister_id).unwrap().oidc_configs, None);
}

#[test]
fn should_init_config() {
    let env = env();
    let config = InternetIdentityInit {
        oidc_configs: Some(vec![example_oidc_config()]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    assert_eq!(
        api::config(&env, canister_id).unwrap().oidc_configs,
        config.oidc_configs
    );
}

#[test]
fn should_enable_config_via_upgrade() {
    let env = env();
    let config = InternetIdentityInit {
        oidc_configs: None,
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    let enabled = Some(vec![example_oidc_config()]);
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            oidc_configs: enabled.clone(),
            ..Default::default()
        }),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().oidc_configs,
        enabled
    );
}

#[test]
fn should_retain_config_across_unrelated_upgrade() {
    let env = env();
    let config = InternetIdentityInit {
        oidc_configs: Some(vec![example_oidc_config()]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

    // Upgrade with unrelated config change
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            related_origins: Some(vec!["https://example.com".into()]),
            ..Default::default()
        }),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().oidc_configs,
        config.oidc_configs
    );
}

#[test]
fn should_clear_openid_configs_when_oidc_configs_set() {
    let env = env();
    let config = InternetIdentityInit {
        openid_configs: Some(vec![OpenIdConfig {
            name: "Example".into(),
            logo: String::new(),
            issuer: "https://example.com".into(),
            client_id: "app.example.com".into(),
            jwks_uri: "https://example.com/oauth2/v3/certs".into(),
            auth_uri: "https://example.com/o/oauth2/v2/auth".into(),
            auth_scope: vec!["openid".into()],
            fedcm_uri: None,
            email_verification: None,
        }]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Upgrade with oidc_configs should clear openid_configs
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            oidc_configs: Some(vec![example_oidc_config()]),
            ..Default::default()
        }),
    )
    .unwrap();
    let result = api::config(&env, canister_id).unwrap();
    assert_eq!(result.openid_configs, None);
    assert!(result.oidc_configs.is_some());
}

#[test]
fn should_return_discovered_oidc_configs() {
    let env = env();
    let config = InternetIdentityInit {
        oidc_configs: Some(vec![example_oidc_config()]),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert_eq!(discovered.len(), 1);
    assert_eq!(discovered[0].discovery_domain, "dfinity.org");
    // Discovery requires HTTP outcalls, which don't run in PocketIC tests,
    // so every discovered field is `None` on init.
    assert_eq!(discovered[0].client_id, None);
    assert_eq!(discovered[0].openid_configuration, None);
    assert_eq!(discovered[0].issuer, None);
}

#[test]
fn should_return_empty_discovered_oidc_configs_when_none_configured() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert!(discovered.is_empty());
}

/// Canary allowlist should reject any domain that isn't `dfinity.org` —
/// the canister init traps, so installation fails.
#[test]
#[should_panic(expected = "canary allowlist")]
fn should_reject_disallowed_discovery_domain() {
    let env = env();
    let config = InternetIdentityInit {
        oidc_configs: Some(vec![DiscoverableOidcConfig {
            discovery_domain: "evil.example.com".into(),
        }]),
        ..Default::default()
    };

    install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));
}
