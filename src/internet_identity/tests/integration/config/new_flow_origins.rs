use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{InternetIdentityInit, OpenIdConfig};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().new_flow_origins,
        None
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            new_flow_origins: None,
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec![]),
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec!["https://example.com".into()]),
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec![
                "https://example1.com".into(),
                "https://example2.com".into(),
            ]),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().new_flow_origins,
            config.new_flow_origins
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        new_flow_origins: None,
        ..Default::default()
    };
    let enabled_value = Some(vec!["https://example.com".into()]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.new_flow_origins = enabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().new_flow_origins,
        enabled_value
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        new_flow_origins: Some(vec!["https://example.com".into()]),
        ..Default::default()
    };
    let disabled_value = Some(vec![]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.new_flow_origins = disabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().new_flow_origins,
        disabled_value
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        new_flow_origins: Some(vec!["https://example.com".into()]),
        ..Default::default()
    };
    let updated_value = Some(vec![
        "https://example1.com".into(),
        "https://example2.com".into(),
    ]);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.new_flow_origins = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().new_flow_origins,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            new_flow_origins: None,
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec![]),
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec!["https://example.com".into()]),
            ..Default::default()
        },
        InternetIdentityInit {
            new_flow_origins: Some(vec![
                "https://example1.com".into(),
                "https://example2.com".into(),
            ]),
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
            api::config(&env, canister_id).unwrap().new_flow_origins,
            config.new_flow_origins
        );
    }
}
