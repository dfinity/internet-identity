use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    InternetIdentityInit, RateLimitConfig,
};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().register_rate_limit,
        Some(RateLimitConfig {
            time_per_token_ns: 10_000_000_000,
            max_tokens: 20_000,
        })
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let config = InternetIdentityInit {
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 1_000_000_000,
            max_tokens: 100,
        }),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    assert_eq!(
        api::config(&env, canister_id).unwrap().register_rate_limit,
        config.register_rate_limit
    );
}

#[test]
fn should_enable_config() {
    // Register rate limit config cannot be enabled,
    // this test is here to explicitly mention this.
}

#[test]
fn should_disable_config() {
    // Register rate limit config cannot be disabled,
    // this test is here to explicitly mention this.
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 1_000_000_000,
            max_tokens: 100,
        }),
        ..Default::default()
    };
    let updated_value = Some(RateLimitConfig {
        time_per_token_ns: 2_000_000_000,
        max_tokens: 200,
    });

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.register_rate_limit = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().register_rate_limit,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let config = InternetIdentityInit {
        register_rate_limit: Some(RateLimitConfig {
            time_per_token_ns: 1_000_000_000,
            max_tokens: 100,
        }),
        ..Default::default()
    };
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
        api::config(&env, canister_id).unwrap().register_rate_limit,
        config.register_rate_limit
    );
}
