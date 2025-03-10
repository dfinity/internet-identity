use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, InternetIdentityInit, OpenIdConfig,
};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(api::config(&env, canister_id).unwrap().archive_config, None);
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            archive_config: None,
            ..Default::default()
        },
        InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: [17; 32],
                entries_buffer_limit: 123_789,
                polling_interval_ns: 659_871_258,
                entries_fetch_limit: 33,
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: [24; 32],
                entries_buffer_limit: 789_123,
                polling_interval_ns: 258_659_871,
                entries_fetch_limit: 16,
            }),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().archive_config,
            config.archive_config
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        archive_config: None,
        ..Default::default()
    };
    let enabled_value = Some(ArchiveConfig {
        module_hash: [17; 32],
        entries_buffer_limit: 123_789,
        polling_interval_ns: 659_871_258,
        entries_fetch_limit: 33,
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.archive_config = enabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().archive_config,
        enabled_value
    );
}

#[test]
fn cannot_disable_config() {
    // Archive config cannot be disabled once enabled,
    // this test is here to explicitly mention this.
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        archive_config: Some(ArchiveConfig {
            module_hash: [17; 32],
            entries_buffer_limit: 123_789,
            polling_interval_ns: 659_871_258,
            entries_fetch_limit: 33,
        }),
        ..Default::default()
    };
    let updated_value = Some(ArchiveConfig {
        module_hash: [24; 32],
        entries_buffer_limit: 789_123,
        polling_interval_ns: 258_659_871,
        entries_fetch_limit: 16,
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.archive_config = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().archive_config,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: [17; 32],
                entries_buffer_limit: 123_789,
                polling_interval_ns: 659_871_258,
                entries_fetch_limit: 33,
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: [24; 32],
                entries_buffer_limit: 789_123,
                polling_interval_ns: 258_659_871,
                entries_fetch_limit: 16,
            }),
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
            api::config(&env, canister_id).unwrap().related_origins,
            config.related_origins
        );
    }
}
