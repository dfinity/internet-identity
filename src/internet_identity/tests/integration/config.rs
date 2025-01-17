use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use ic_cdk::api::management_canister::provisional::CanisterId;
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, CaptchaConfig, CaptchaTrigger, InternetIdentityInit, OpenIdConfig,
    RateLimitConfig,
};
use pocket_ic::{CallError, PocketIc};

const DEFAULT_CONFIG: InternetIdentityInit = InternetIdentityInit {
    assigned_user_number_range: Some((10000, 1_000_000)),
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
    openid_google: None,
};

/// Utility method to check if install/upgrade config matches config returned from query,
/// this maps `None` to `Some(None)` for `Option<Option>` since it does not return `None`.
fn assert_config(
    env: &PocketIc,
    canister_id: CanisterId,
    expected_config: &InternetIdentityInit,
) -> Result<(), CallError> {
    assert_eq!(
        api::config(env, canister_id)?,
        InternetIdentityInit {
            openid_google: expected_config.openid_google.clone().or(Some(None)),
            ..expected_config.clone()
        }
    );
    Ok(())
}

/// Utility method to install and check if config matches install config
#[cfg(test)]
fn install_and_assert(
    env: &PocketIc,
    config: &InternetIdentityInit,
) -> Result<CanisterId, CallError> {
    let canister_id = install_ii_canister_with_arg(env, II_WASM.clone(), Some(config.clone()));
    assert_config(env, canister_id, config)?;
    Ok(canister_id)
}

/// Utility method to upgrade and check if config matches upgrade config
#[cfg(test)]
fn upgrade_and_assert(
    env: &PocketIc,
    canister_id: CanisterId,
    config: &InternetIdentityInit,
    expected_config: &InternetIdentityInit,
) -> Result<(), CallError> {
    upgrade_ii_canister_with_arg(env, canister_id, II_WASM.clone(), Some(config.clone()))?;
    assert_config(env, canister_id, expected_config)?;
    Ok(())
}

#[test]
fn should_retain_anchor_on_user_range_change() -> Result<(), CallError> {
    let env = env();
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((3456, 798977)),
        ..DEFAULT_CONFIG
    };

    let canister_id = install_and_assert(&env, &config)?;
    upgrade_and_assert(&env, canister_id, &config, &config)?;
    
    Ok(())
}

#[test]
fn should_retain_config_after_none() -> Result<(), CallError> {
    let env = env();
    let config = InternetIdentityInit {
        related_origins: Some(vec![
            "https://identity.internetcomputer.org".into(),
            "https://identity.ic0.app".into(),
            "https://identity.icp0.io".into(),
        ]),
        openid_google: Some(Some(OpenIdConfig {
            client_id: "https://example.com".into(),
        })),
        ..DEFAULT_CONFIG
    };

    let canister_id = install_and_assert(&env, &config)?;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone()))?;
    assert_config(&env, canister_id, &config)?;

    Ok(())
}

#[test]
fn should_update_related_origins() -> Result<(), CallError> {
    let env = env();
    let config_1 = InternetIdentityInit {
        related_origins: Some(vec![
            "https://identity.internetcomputer.org".into(),
            "https://identity.ic0.app".into(),
            "https://identity.icp0.io".into(),
        ]),
        ..DEFAULT_CONFIG
    };
    let config_2 = InternetIdentityInit {
        related_origins: Some(vec!["https://identity.ic0.app".into()]),
        ..DEFAULT_CONFIG
    };
    let disabled_config = InternetIdentityInit {
        related_origins: Some(vec![]),
        ..DEFAULT_CONFIG
    };
    let other_config = InternetIdentityInit {
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 5000,
            captcha_trigger: CaptchaTrigger::Dynamic {
                threshold_pct: 100,
                current_rate_sampling_interval_s: 600,
                reference_rate_sampling_interval_s: 3600,
            },
        }),
        ..DEFAULT_CONFIG
    };

    // Check if related origins are installed correctly
    let canister_id_1 = install_and_assert(&env, &config_1)?;
    install_and_assert(&env, &config_2)?;
    install_and_assert(&env, &disabled_config)?;

    // Check if related origins are updated and then untouched
    upgrade_and_assert(&env, canister_id_1, &config_2, &config_2)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &config_2)?;

    // Check if related origins are disabled and then untouched
    upgrade_and_assert(&env, canister_id_1, &disabled_config, &disabled_config)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &disabled_config)?;

    // 3 Check if related origins are enabled and then untouched
    upgrade_and_assert(&env, canister_id_1, &config_1, &config_1)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &config_1)?;

    Ok(())
}

#[test]
fn should_update_openid_google() -> Result<(), CallError> {
    let env = env();
    let config_1 = InternetIdentityInit {
        openid_google: Some(Some(OpenIdConfig {
            client_id: "https://example1.com".into(),
        })),
        ..DEFAULT_CONFIG
    };
    let config_2 = InternetIdentityInit {
        openid_google: Some(Some(OpenIdConfig {
            client_id: "https://example2.com".into(),
        })),
        ..DEFAULT_CONFIG
    };
    let disabled_config = InternetIdentityInit {
        openid_google: Some(None),
        ..DEFAULT_CONFIG
    };
    let other_config = InternetIdentityInit {
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 5000,
            captcha_trigger: CaptchaTrigger::Dynamic {
                threshold_pct: 100,
                current_rate_sampling_interval_s: 600,
                reference_rate_sampling_interval_s: 3600,
            },
        }),
        ..DEFAULT_CONFIG
    };

    // Check if open id config is installed correctly
    let canister_id_1 = install_and_assert(&env, &config_1)?;
    install_and_assert(&env, &config_2)?;
    install_and_assert(&env, &disabled_config)?;

    // Check if open id config is updated and then untouched
    upgrade_and_assert(&env, canister_id_1, &config_2, &config_2)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &config_2)?;

    // Check if open id config is disabled and then untouched
    upgrade_and_assert(&env, canister_id_1, &disabled_config, &disabled_config)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &disabled_config)?;

    // Check if open id config is enabled and then untouched
    upgrade_and_assert(&env, canister_id_1, &config_1, &config_1)?;
    upgrade_and_assert(&env, canister_id_1, &other_config, &config_1)?;

    Ok(())
}
