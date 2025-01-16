use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, CaptchaConfig, CaptchaTrigger, InternetIdentityInit, OpenIdConfig,
    RateLimitConfig,
};
use pocket_ic::CallError;

#[test]
fn should_retain_anchor_on_user_range_change() -> Result<(), CallError> {
    let env = env();
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((3456, 798977)),
        archive_config: Some(ArchiveConfig {
            module_hash: [17; 32],
            entries_buffer_limit: 123789,
            polling_interval_ns: 659871258,
            entries_fetch_limit: 33,
        }),
        canister_creation_cycles_cost: Some(123),
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 99,
            max_tokens: 874,
        }),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 788,
            captcha_trigger: CaptchaTrigger::Dynamic {
                threshold_pct: 12,
                current_rate_sampling_interval_s: 456,
                reference_rate_sampling_interval_s: 9999,
            },
        }),
        related_origins: None,
        openid_google: Some(None),
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

    assert_eq!(api::config(&env, canister_id)?, config);
    Ok(())
}

#[test]
fn should_retain_config_after_none() -> Result<(), CallError> {
    let env = env();
    let related_origins = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
        "https://identity.icp0.io".to_string(),
    ]
    .to_vec();
    let openid_google = OpenIdConfig {
        client_id: "https://example.com".into(),
    };
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((3456, 798977)),
        archive_config: Some(ArchiveConfig {
            module_hash: [17; 32],
            entries_buffer_limit: 123789,
            polling_interval_ns: 659871258,
            entries_fetch_limit: 33,
        }),
        canister_creation_cycles_cost: Some(123),
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 99,
            max_tokens: 874,
        }),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 788,
            captcha_trigger: CaptchaTrigger::Dynamic {
                threshold_pct: 12,
                current_rate_sampling_interval_s: 456,
                reference_rate_sampling_interval_s: 9999,
            },
        }),
        related_origins: Some(related_origins),
        openid_google: Some(Some(openid_google)),
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

    assert_eq!(api::config(&env, canister_id)?, config);

    let _ = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None);

    assert_eq!(api::config(&env, canister_id)?, config);

    Ok(())
}

#[test]
fn should_override_partially() -> Result<(), CallError> {
    let env = env();
    let related_origins = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
        "https://identity.icp0.io".to_string(),
    ]
    .to_vec();
    let openid_google = Some(OpenIdConfig {
        client_id: "https://example.com".into(),
    });
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((3456, 798977)),
        archive_config: Some(ArchiveConfig {
            module_hash: [17; 32],
            entries_buffer_limit: 123789,
            polling_interval_ns: 659871258,
            entries_fetch_limit: 33,
        }),
        canister_creation_cycles_cost: Some(123),
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 99,
            max_tokens: 874,
        }),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 788,
            captcha_trigger: CaptchaTrigger::Dynamic {
                threshold_pct: 12,
                current_rate_sampling_interval_s: 456,
                reference_rate_sampling_interval_s: 9999,
            },
        }),
        related_origins: Some(related_origins),
        openid_google: Some(openid_google),
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

    assert_eq!(api::config(&env, canister_id)?, config);

    let new_captcha = CaptchaConfig {
        max_unsolved_captchas: 5000,
        captcha_trigger: CaptchaTrigger::Dynamic {
            threshold_pct: 100,
            current_rate_sampling_interval_s: 600,
            reference_rate_sampling_interval_s: 3600,
        },
    };
    let config_2 = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: Some(new_captcha.clone()),
        related_origins: None,
        openid_google: None,
    };

    let _ =
        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config_2.clone()));

    let expected_config_2 = InternetIdentityInit {
        captcha_config: Some(new_captcha.clone()),
        ..config
    };

    assert_eq!(api::config(&env, canister_id)?, expected_config_2);

    let related_origins_2: Vec<String> = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ]
    .to_vec();
    let openid_google2 = None;
    let config_3 = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: Some(related_origins_2.clone()),
        openid_google: Some(openid_google2.clone()),
    };

    let _ =
        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config_3.clone()));

    let expected_config_3 = InternetIdentityInit {
        related_origins: Some(related_origins_2.clone()),
        openid_google: Some(openid_google2.clone()),
        ..expected_config_2
    };

    assert_eq!(api::config(&env, canister_id)?, expected_config_3);

    Ok(())
}
