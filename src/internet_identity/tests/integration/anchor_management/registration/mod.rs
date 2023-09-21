//! Tests for the user registration flow. The registration process consists of two canister calls:
//! 1. create_challenge: retrieve a captcha
//! 2. register: submit the captcha solution and device information to create a new anchor

mod temp_keys;

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use std::time::Duration;

/// Tests user registration with cross checks for get_anchor_credentials, get_anchor_info and get_principal.
#[test]
fn should_register_new_anchor() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::init_salt(&env, canister_id)?;
    let user_number = flows::register_anchor(&env, canister_id);

    let anchor_credentials = api::get_anchor_credentials(&env, canister_id, user_number)?;
    assert_eq!(
        anchor_credentials.credentials,
        vec![WebAuthnCredential::try_from(device_data_1()).unwrap()]
    );
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
    let principal = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number,
        "https://some-frontend.com",
    )?;
    assert_ne!(principal, Principal::anonymous());
    Ok(())
}

/// Tests that multiple anchors can be registered (even with the same device / keys). This is useful
/// for users to separate different contexts using multiple anchors.
#[test]
fn should_allow_multiple_registrations() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number_1 =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());

    assert_ne!(user_number_1, user_number_2);
}

/// Tests that the user numbers start at the beginning of the init range and are capped at the end (exclusive).
#[test]
fn should_assign_correct_user_numbers() -> Result<(), CallError> {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((127, 129)));

    let user_number = flows::register_anchor(&env, canister_id);
    assert_eq!(user_number, 127);

    let user_number = flows::register_anchor(&env, canister_id);
    assert_eq!(user_number, 128);

    let challenge = api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )?;
    assert!(matches!(result, RegisterResponse::CanisterFull));
    Ok(())
}

/// Tests that the call to register needs to be signed by the device that is being registered.
/// This is to make sure that the initial public key belongs to a private key that can be used to sign requests.
#[test]
fn registration_with_mismatched_sender_fails() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let challenge = api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_2(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated against").unwrap(),
    );
    Ok(())
}

/// Verifies that non-recovery devices cannot be registered as protected.
#[test]
fn should_not_register_non_recovery_device_as_protected() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let mut device1 = device_data_1();
    device1.protection = DeviceProtection::Protected;

    let challenge = api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device1,
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Only recovery phrases can be locked but key type is Unknown").unwrap(),
    );
    Ok(())
}

/// Tests that the solution to the captcha needs to be correct.
#[test]
fn should_not_allow_wrong_captcha() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let challenge = api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "wrong solution".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )?;

    assert!(matches!(result, RegisterResponse::BadChallenge));
    Ok(())
}

/// Tests that there is a time limit for captchas.
/// Currently only checked by create_challenge, see L2-766.
#[test]
fn should_not_allow_expired_captcha() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let challenge = api::create_challenge(&env, canister_id)?;
    env.advance_time(Duration::from_secs(301)); // one second longer than captcha validity

    // required because register does not check captcha expiry
    api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )?;

    assert!(matches!(result, RegisterResponse::BadChallenge));
    Ok(())
}

/// Tests that there is a maximum number of captchas that can be created in a given timeframe.
#[test]
fn should_limit_captcha_creation() -> Result<(), CallError> {
    let env = env();
    let init_arg = InternetIdentityInit {
        max_inflight_captchas: Some(3),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(init_arg));

    for _ in 0..3 {
        api::create_challenge(&env, canister_id)?;
    }
    let result = api::create_challenge(&env, canister_id);

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("too many inflight captchas").unwrap(),
    );
    Ok(())
}

/// Tests that the `register` call will hit the rate limit on too many calls and that the limit
/// will allow new calls after some time.
#[test]
fn should_rate_limit_register_calls() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_rate_limit(RateLimitConfig {
            time_per_token_ns: Duration::from_secs(1).as_nanos() as u64,
            max_tokens: 2,
        }),
    );

    for _ in 0..2 {
        flows::register_anchor(&env, canister_id);
    }
    let challenge = api::create_challenge(&env, canister_id)?;
    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key.clone(),
        },
        None,
    );
    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("rate limit reached, try again later").unwrap(),
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_current_tokens",
        0f64,
    );

    env.advance_time(Duration::from_secs(1));

    let result = api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    );
    assert!(result.is_ok());
    Ok(())
}

/// Tests that the `register` rate limit does not replenish tokens to more than max_tokens.
#[test]
fn should_not_allow_more_than_max_tokens_calls_on_rate_limit() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_rate_limit(RateLimitConfig {
            time_per_token_ns: Duration::from_secs(1).as_nanos() as u64,
            max_tokens: 10_000,
        }),
    );

    // the canister starts out with max_tokens, so use some to make the replenish actually do something
    for _ in 0..10 {
        flows::register_anchor(&env, canister_id);
    }

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_current_tokens",
        9_990f64,
    );

    env.advance_time(Duration::from_secs(100));

    // some activity required to process the rate limit (which will use one token)
    flows::register_anchor(&env, canister_id);

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_current_tokens",
        9_999f64, // 1 less than the limit because of register call required to update rate limit
    );
    Ok(())
}

/// Tests that the II correctly reports the max tokens metric.
#[test]
fn should_report_max_rate_limit_tokens() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_rate_limit(RateLimitConfig {
            time_per_token_ns: Duration::from_secs(1).as_nanos() as u64,
            max_tokens: 2,
        }),
    );

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_max_tokens",
        2f64,
    );
    Ok(())
}

/// Tests that the II correctly reports the max tokens metric.
#[test]
fn should_report_time_per_rate_limit_token() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_rate_limit(RateLimitConfig {
            time_per_token_ns: Duration::from_secs(1).as_nanos() as u64,
            max_tokens: 2,
        }),
    );

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_time_per_tokens_seconds",
        1f64,
    );
    Ok(())
}
