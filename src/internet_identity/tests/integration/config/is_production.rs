use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::InternetIdentityInit;

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(api::config(&env, canister_id).unwrap().is_production, None);
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            is_production: None,
            ..Default::default()
        },
        InternetIdentityInit {
            is_production: Some(false),
            ..Default::default()
        },
        InternetIdentityInit {
            is_production: Some(true),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().is_production,
            config.is_production
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        is_production: None,
        ..Default::default()
    };
    let enabled_value = Some(true);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.is_production = enabled_value;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().is_production,
        enabled_value
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        is_production: Some(true),
        ..Default::default()
    };
    let disabled_value = Some(false);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.is_production = disabled_value;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().is_production,
        disabled_value
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        is_production: Some(false),
        ..Default::default()
    };
    let updated_value = Some(true);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.is_production = updated_value;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().is_production,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            is_production: None,
            ..Default::default()
        },
        InternetIdentityInit {
            is_production: Some(false),
            ..Default::default()
        },
        InternetIdentityInit {
            is_production: Some(true),
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
                openid_google: Some(Some(OpenIdConfig {
                    client_id: "https://example.com".into(),
                })),
                ..Default::default()
            }),
        )
        .unwrap();
        assert_eq!(
            api::config(&env, canister_id).unwrap().is_production,
            config.is_production
        );
    }
}
