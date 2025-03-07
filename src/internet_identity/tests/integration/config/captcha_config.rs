use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::CaptchaTrigger::{Dynamic, Static};
use internet_identity_interface::internet_identity::types::StaticCaptchaTrigger::{
    CaptchaDisabled, CaptchaEnabled,
};
use internet_identity_interface::internet_identity::types::{
    CaptchaConfig, InternetIdentityInit, OpenIdConfig,
};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().captcha_config,
        Some(CaptchaConfig {
            max_unsolved_captchas: 500,
            captcha_trigger: Static(CaptchaEnabled)
        })
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 200,
                captcha_trigger: Static(CaptchaEnabled),
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 300,
                captcha_trigger: Static(CaptchaDisabled),
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 400,
                captcha_trigger: Dynamic {
                    threshold_pct: 12,
                    current_rate_sampling_interval_s: 456,
                    reference_rate_sampling_interval_s: 9999,
                },
            }),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id).unwrap().captcha_config,
            config.captcha_config
        );
    }
}

#[test]
fn should_enable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 300,
            captcha_trigger: Static(CaptchaDisabled),
        }),
        ..Default::default()
    };
    let enabled_value = Some(CaptchaConfig {
        max_unsolved_captchas: 300,
        captcha_trigger: Static(CaptchaEnabled),
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.captcha_config = enabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().captcha_config,
        enabled_value
    );
}

#[test]
fn should_disable_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 300,
            captcha_trigger: Static(CaptchaEnabled),
        }),
        ..Default::default()
    };
    let disabled_value = Some(CaptchaConfig {
        max_unsolved_captchas: 300,
        captcha_trigger: Static(CaptchaDisabled),
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.captcha_config = disabled_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().captcha_config,
        disabled_value
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 300,
            captcha_trigger: Static(CaptchaEnabled),
        }),
        ..Default::default()
    };
    let updated_value = Some(CaptchaConfig {
        max_unsolved_captchas: 788,
        captcha_trigger: Dynamic {
            threshold_pct: 12,
            current_rate_sampling_interval_s: 456,
            reference_rate_sampling_interval_s: 9999,
        },
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.captcha_config = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().captcha_config,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 200,
                captcha_trigger: Static(CaptchaEnabled),
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 300,
                captcha_trigger: Static(CaptchaDisabled),
            }),
            ..Default::default()
        },
        InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 400,
                captcha_trigger: Dynamic {
                    threshold_pct: 12,
                    current_rate_sampling_interval_s: 456,
                    reference_rate_sampling_interval_s: 9999,
                },
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
            api::config(&env, canister_id).unwrap().captcha_config,
            config.captcha_config
        );
    }
}
