use canister_tests::api::internet_identity as api;
use canister_tests::framework::{env, install_ii_canister_with_arg, II_WASM};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, CaptchaConfig, CaptchaTrigger, InternetIdentityInit, RateLimitConfig,
};
use pocket_ic::CallError;

#[test]
fn should_retain_anchor_on_user_range_change() -> Result<(), CallError> {
    let env = env();
    let related_origins: Vec<String> = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
        "https://identity.icp0.io".to_string(),
    ]
    .to_vec();
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
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

    assert_eq!(api::config(&env, canister_id)?, config);
    Ok(())
}
