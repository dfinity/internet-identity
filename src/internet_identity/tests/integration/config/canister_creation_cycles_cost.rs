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
        api::config(&env, canister_id)
            .unwrap()
            .canister_creation_cycles_cost,
        Some(0)
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            canister_creation_cycles_cost: Some(0),
            ..Default::default()
        },
        InternetIdentityInit {
            canister_creation_cycles_cost: Some(123),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id)
                .unwrap()
                .canister_creation_cycles_cost,
            config.canister_creation_cycles_cost
        );
    }
}

#[test]
fn cannot_enable_config() {
    // Canister creation cycle costs config is always enabled,
    // this test is here to explicitly mention this.
}

#[test]
fn cannot_disable_config() {
    // Canister creation cycle costs config is always enabled,
    // this test is here to explicitly mention this.
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        canister_creation_cycles_cost: Some(123),
        ..Default::default()
    };
    let updated_value = Some(456);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.canister_creation_cycles_cost = updated_value;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id)
            .unwrap()
            .canister_creation_cycles_cost,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            canister_creation_cycles_cost: Some(0),
            ..Default::default()
        },
        InternetIdentityInit {
            canister_creation_cycles_cost: Some(123),
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
            api::config(&env, canister_id)
                .unwrap()
                .canister_creation_cycles_cost,
            config.canister_creation_cycles_cost
        );
    }
}
