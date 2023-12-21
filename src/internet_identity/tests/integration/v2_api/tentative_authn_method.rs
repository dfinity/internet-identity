use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_pubkey_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, II_WASM,
};
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodRegistration, TentativeAuthnMethodAddError, TentativeAuthnMethodAddInfo,
    TentativeAuthnMethodVerificationError,
};
use regex::Regex;
use std::ops::Add;
use std::time::{Duration, UNIX_EPOCH};

#[test]
fn should_enter_authn_method_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let result = api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    assert_eq!(
        result.expiration,
        env.time()
            .add(Duration::from_secs(900)) // 900 seconds -> 15 min
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );
    Ok(())
}

#[test]
fn should_require_authentication_to_enter_authn_method_registration_mode() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let result = api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z0-9-]+ could not be authenticated.").unwrap(),
    );
}

#[test]
fn should_register_tentative_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    let add_response = api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("tentative_authn_method_add failed");

    api_v2::tentative_authn_method_verify(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.verification_code,
    )?
    .expect("tentative_authn_method_verify failed");

    Ok(())
}

#[test]
fn should_verify_tentative_authn_method_after_failed_attempt() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    let add_response = api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("tentative_authn_method_add failed");

    let result = api_v2::tentative_authn_method_verify(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        "wrong code",
    )?;

    assert!(matches!(
        result,
        Err(TentativeAuthnMethodVerificationError::WrongCode { retries_left: 2 })
    ));

    api_v2::tentative_authn_method_verify(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.verification_code,
    )?
    .expect("tentative_authn_method_verify failed");

    Ok(())
}

#[test]
fn identity_info_should_return_tentative_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    let authn_method2 = sample_pubkey_authn_method(1);
    let TentativeAuthnMethodAddInfo { expiration, .. } =
        api_v2::tentative_authn_method_add(&env, canister_id, identity_number, &authn_method2)?
            .expect("tentative_authn_method_add failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");

    assert!(matches!(
        identity_info.authn_method_registration,
        Some(AuthnMethodRegistration {authn_method: Some(tenative_authn_method),
            expiration: tentative_authn_method_expiration
        }) if tenative_authn_method == authn_method2 && tentative_authn_method_expiration == expiration
    ));
    Ok(())
}

#[test]
fn should_reject_tentative_authn_method_if_not_in_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let result = api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(TentativeAuthnMethodAddError::RegistrationModeOff)
    ));

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");
    api_v2::tentative_authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    let result = api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(TentativeAuthnMethodAddError::RegistrationModeOff)
    ));
    Ok(())
}

#[test]
fn should_reject_tentative_authn_method_if_registration_mode_is_expired() -> Result<(), CallError> {
    const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    env.advance_time(REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1));

    let result = api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(TentativeAuthnMethodAddError::RegistrationModeOff)
    ));
    Ok(())
}

#[test]
fn should_reject_verification_without_tentative_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    let result = api_v2::tentative_authn_method_verify(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        "invalid code",
    )?;

    assert!(matches!(
        result,
        Err(TentativeAuthnMethodVerificationError::NoAuthnMethodToVerify)
    ));
    Ok(())
}

#[test]
fn should_reject_verification_with_wrong_code() -> Result<(), CallError> {
    const MAX_RETRIES: u8 = 3;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    api_v2::tentative_authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("tentative_authn_method_registration_mode_enter failed");

    api_v2::tentative_authn_method_add(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("tentative_authn_method_add failed");

    for expected_retries in (0..MAX_RETRIES).rev() {
        assert!(matches!(
            api_v2::tentative_authn_method_verify(
                &env,
                canister_id,
                authn_method.principal(),
                identity_number,
                "invalid code"
            )?,
            Err(TentativeAuthnMethodVerificationError::WrongCode {
                retries_left
            }) if retries_left == expected_retries
        ));
    }

    assert!(matches!(
        api_v2::tentative_authn_method_verify(
            &env,
            canister_id,
            authn_method.principal(),
            identity_number,
            "invalid code"
        )?,
        Err(TentativeAuthnMethodVerificationError::RegistrationModeOff)
    ));
    Ok(())
}
