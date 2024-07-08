use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    arg_with_anchor_range, env, expect_user_error_with_message, install_ii_canister,
    install_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    ChallengeAttempt, IdentityRegisterError, MetadataEntryV2,
};
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::Duration;

#[test]
fn should_register_new_identity() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((42, 44)));
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    assert_eq!(identity_number, 42);
}

#[test]
fn should_register_multiple_identities() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number_1 = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let identity_number_2 = create_identity_with_authn_method(&env, canister_id, &authn_method);

    assert_ne!(identity_number_1, identity_number_2);
}

#[test]
fn should_not_exceed_configured_identity_range() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((42, 44)));

    let authn_method = test_authn_method();
    create_identity_with_authn_method(&env, canister_id, &authn_method);
    create_identity_with_authn_method(&env, canister_id, &authn_method);

    let challenge = api_v2::captcha_create(&env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    let result = api_v2::identity_register(
        &env,
        canister_id,
        authn_method.principal(),
        &authn_method,
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )
    .expect("API call failed");
    assert!(matches!(result, Err(IdentityRegisterError::CanisterFull)));
}

#[test]
fn should_verify_sender_matches_authn_method() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let challenge = api_v2::captcha_create(&env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    let result = api_v2::identity_register(
        &env,
        canister_id,
        Principal::anonymous(),
        &test_authn_method(),
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
}

#[test]
fn should_not_allow_wrong_captcha() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();

    let challenge = api_v2::captcha_create(&env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    let result = api_v2::identity_register(
        &env,
        canister_id,
        authn_method.principal(),
        &authn_method,
        &ChallengeAttempt {
            chars: "wrong solution".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )
    .expect("API call failed");
    assert!(matches!(result, Err(IdentityRegisterError::BadCaptcha)));
}

#[test]
fn should_not_allow_expired_captcha() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();

    let challenge = api_v2::captcha_create(&env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    env.advance_time(Duration::from_secs(301)); // one second longer than captcha validity

    let result = api_v2::identity_register(
        &env,
        canister_id,
        authn_method.principal(),
        &authn_method,
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )
    .expect("API call failed");
    assert!(matches!(result, Err(IdentityRegisterError::BadCaptcha)));
}

#[test]
fn should_fail_on_invalid_metadata() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let mut authn_method = test_authn_method();
    authn_method.metadata.insert(
        "usage".to_string(),
        MetadataEntryV2::Bytes(ByteBuf::from("invalid")),
    );

    let challenge = api_v2::captcha_create(&env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    let result = api_v2::identity_register(
        &env,
        canister_id,
        authn_method.principal(),
        &authn_method,
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
        None,
    )
    .expect("API call failed");
    assert!(matches!(
        result,
        Err(IdentityRegisterError::InvalidMetadata(_))
    ));
}
