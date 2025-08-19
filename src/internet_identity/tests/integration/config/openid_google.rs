use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{InternetIdentityInit, OpenIdConfig};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(api::config(&env, canister_id).unwrap().openid_configs, None,);
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            openid_configs: Some(vec![]),
            ..Default::default()
        },
        InternetIdentityInit {
            openid_configs: Some(vec![OpenIdConfig {
                name: "Example".to_string(),
                logo_uri: "https://example.com/logo.png".to_string(),
                config_uri: "https://example.com/.well-known/openid-configuration".to_string(),
                client_id: "https://example.com".into(),
            }]),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().openid_configs,
            config.openid_configs
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        openid_configs: None,
        ..Default::default()
    };
    let enabled_value = Some(vec![OpenIdConfig {
        name: "Example".to_string(),
        logo_uri: "https://example.com/logo.png".to_string(),
        config_uri: "https://example.com/.well-known/openid-configuration".to_string(),
        client_id: "https://example.com".into(),
    }]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.openid_configs = enabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().openid_configs,
        enabled_value
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        openid_configs: Some(vec![OpenIdConfig {
            name: "Example".to_string(),
            logo_uri: "https://example.com/logo.png".to_string(),
            config_uri: "https://example.com/.well-known/openid-configuration".to_string(),
            client_id: "https://example.com".into(),
        }]),
        ..Default::default()
    };
    let disabled_value = Some(vec![]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.openid_configs = disabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().openid_configs,
        disabled_value
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        openid_configs: Some(vec![OpenIdConfig {
            name: "Example".to_string(),
            logo_uri: "https://example.com/logo.png".to_string(),
            config_uri: "https://example.com/.well-known/openid-configuration".to_string(),
            client_id: "https://example.com".into(),
        }]),
        ..Default::default()
    };
    let updated_value = Some(vec![OpenIdConfig {
        name: "Example2".to_string(),
        logo_uri: "https://example2.com/logo.png".to_string(),
        config_uri: "https://example2.com/.well-known/openid-configuration".to_string(),
        client_id: "https://example2.com".into(),
    }]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.openid_configs = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().openid_configs,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            openid_configs: None,
            ..Default::default()
        },
        InternetIdentityInit {
            openid_configs: Some(vec![]),
            ..Default::default()
        },
        InternetIdentityInit {
            openid_configs: Some(vec![OpenIdConfig {
                name: "Example".to_string(),
                logo_uri: "https://example.com/logo.png".to_string(),
                config_uri: "https://example.com/.well-known/openid-configuration".to_string(),
                client_id: "https://example.com".into(),
            }]),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        // No config argument
        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None).unwrap();
        // Unrelated config change
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
            api::config(&env, canister_id).unwrap().openid_configs,
            config.openid_configs
        );
    }
}
