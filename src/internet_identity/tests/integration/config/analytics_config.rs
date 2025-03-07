use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::AnalyticsConfig::Plausible;
use internet_identity_interface::internet_identity::types::{InternetIdentityInit, OpenIdConfig};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().analytics_config,
        Some(None)
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            analytics_config: Some(None),
            ..Default::default()
        },
        InternetIdentityInit {
            analytics_config: Some(Some(Plausible {
                domain: None,
                hash_mode: None,
                track_localhost: None,
                api_host: None,
            })),
            ..Default::default()
        },
        InternetIdentityInit {
            analytics_config: Some(Some(Plausible {
                domain: Some("https://example1.com".into()),
                hash_mode: Some(true),
                track_localhost: Some(true),
                api_host: Some("https://example2.com".into()),
            })),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().analytics_config,
            config.analytics_config
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        analytics_config: Some(None),
        ..Default::default()
    };
    let enabled_value = Some(Some(Plausible {
        domain: Some("https://example1.com".into()),
        hash_mode: Some(true),
        track_localhost: Some(true),
        api_host: Some("https://example2.com".into()),
    }));

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.analytics_config = enabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().analytics_config,
        enabled_value
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        analytics_config: Some(Some(Plausible {
            domain: Some("https://example1.com".into()),
            hash_mode: Some(true),
            track_localhost: Some(true),
            api_host: Some("https://example2.com".into()),
        })),
        ..Default::default()
    };
    let disabled_value = Some(None);

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.analytics_config = disabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().analytics_config,
        disabled_value
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        analytics_config: Some(Some(Plausible {
            domain: None,
            hash_mode: None,
            track_localhost: None,
            api_host: None,
        })),
        ..Default::default()
    };
    let updated_value = Some(Some(Plausible {
        domain: Some("https://example1.com".into()),
        hash_mode: Some(true),
        track_localhost: Some(true),
        api_host: Some("https://example2.com".into()),
    }));

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.analytics_config = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().analytics_config,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            analytics_config: Some(None),
            ..Default::default()
        },
        InternetIdentityInit {
            analytics_config: Some(Some(Plausible {
                domain: None,
                hash_mode: None,
                track_localhost: None,
                api_host: None,
            })),
            ..Default::default()
        },
        InternetIdentityInit {
            analytics_config: Some(Some(Plausible {
                domain: Some("https://example1.com".into()),
                hash_mode: Some(true),
                track_localhost: Some(true),
                api_host: Some("https://example2.com".into()),
            })),
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
            api::config(&env, canister_id).unwrap().analytics_config,
            config.analytics_config
        );
    }
}
