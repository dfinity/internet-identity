use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    InternetIdentityInit, DiscoverableOidcConfig, OpenIdConfig,
};

fn example_oidc_config() -> DiscoverableOidcConfig {
    DiscoverableOidcConfig {
        name: "Google".into(),
        logo: String::new(),
        discovery_url: "https://accounts.google.com/.well-known/openid-configuration".into(),
        client_id: Some("test-client-id".into()),
        email_verification: None,
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
    assert_eq!(discovered[0].name, "Google");
    assert_eq!(discovered[0].client_id, Some("test-client-id".into()));
    assert_eq!(
        discovered[0].discovery_url,
        "https://accounts.google.com/.well-known/openid-configuration"
    );
    // Issuer is None because discovery hasn't run (no HTTP outcalls in tests)
    assert_eq!(discovered[0].issuer, None);
}

#[test]
fn should_return_empty_discovered_oidc_configs_when_none_configured() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);

    let discovered = api::discovered_oidc_configs(&env, canister_id).unwrap();
    assert!(discovered.is_empty());
}
