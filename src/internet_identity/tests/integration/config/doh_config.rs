use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{DohConfig, InternetIdentityInit};

fn reserving(reserved_domains: Option<Vec<String>>, max_cache_age_secs: Option<u64>) -> DohConfig {
    DohConfig {
        reserved_domains,
        max_cache_age_secs,
    }
}

fn with_doh_config(doh_config: Option<Option<DohConfig>>) -> InternetIdentityInit {
    InternetIdentityInit {
        doh_config,
        ..Default::default()
    }
}

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(None)
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let doh_configs = vec![
        reserving(None, None),
        reserving(Some(vec![]), Some(60)),
        reserving(Some(vec!["gmail.com".into()]), None),
        reserving(
            Some(vec!["gmail.com".into(), "outlook.com".into()]),
            Some(3600),
        ),
    ];

    for doh_config in doh_configs {
        let canister_id = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            Some(with_doh_config(Some(Some(doh_config.clone())))),
        );
        assert_eq!(
            api::config(&env, canister_id).unwrap().doh_config,
            Some(Some(doh_config))
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let enabled_value = reserving(Some(vec!["gmail.com".into()]), Some(60));

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(None)
    );
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(with_doh_config(Some(Some(enabled_value.clone())))),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(Some(enabled_value))
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let initial_value = reserving(Some(vec!["gmail.com".into()]), Some(60));
    let updated_value = reserving(Some(vec!["outlook.com".into()]), Some(120));

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(with_doh_config(Some(Some(initial_value.clone())))),
    );
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(Some(initial_value))
    );
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(with_doh_config(Some(Some(updated_value.clone())))),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(Some(updated_value))
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let initial_value = reserving(Some(vec!["gmail.com".into()]), Some(60));

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(with_doh_config(Some(Some(initial_value.clone())))),
    );
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(Some(initial_value))
    );
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(with_doh_config(Some(None))),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(None)
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let doh_configs = vec![
        None,
        Some(reserving(None, None)),
        Some(reserving(Some(vec![]), Some(60))),
        Some(reserving(Some(vec!["gmail.com".into()]), Some(3600))),
    ];

    for doh_config in doh_configs {
        let canister_id = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            Some(with_doh_config(Some(doh_config.clone()))),
        );
        // No config argument
        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None).unwrap();
        // Unrelated config change
        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                enable_dapps_explorer: Some(true),
                ..Default::default()
            }),
        )
        .unwrap();
        assert_eq!(
            api::config(&env, canister_id).unwrap().doh_config,
            Some(doh_config)
        );
    }
}
