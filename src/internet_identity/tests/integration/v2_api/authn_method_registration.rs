use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_pubkey_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, install_ii_with_archive, time};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodConfirmationCode, AuthnMethodConfirmationError, AuthnMethodRegisterError,
    AuthnMethodRegistration, AuthnMethodRegistrationModeEnterError,
};
use pocket_ic::CallError;
use std::time::Duration;

#[test]
fn should_enter_authn_method_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    let result = api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    assert_eq!(
        result.expiration,
        time(&env) + Duration::from_secs(900).as_nanos() as u64 // 900 seconds -> 15 min
    );
    Ok(())
}

#[test]
fn should_require_authentication_to_enter_authn_method_registration_mode() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    let result = api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        Some(registration_mode_id),
    )
    .expect("authn_method_registration_mode_enter failed");

    assert!(matches!(
        result,
        Err(AuthnMethodRegistrationModeEnterError::Unauthorized(_))
    ))
}

#[test]
fn should_register_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    let add_response = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.confirmation_code,
    )?
    .expect("authn_method_confirm failed");

    Ok(())
}

#[test]
fn should_verify_authn_method_after_failed_attempt() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    let add_response = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    let result = api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        "wrong code",
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodConfirmationError::WrongCode { retries_left: 2 })
    ));

    api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.confirmation_code,
    )?
    .expect("authn_method_confirm failed");

    Ok(())
}

#[test]
fn identity_info_should_return_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    let authn_method2 = sample_pubkey_authn_method(1);
    let AuthnMethodConfirmationCode { expiration, .. } =
        api_v2::authn_method_register(&env, canister_id, identity_number, &authn_method2)?
            .expect("authn_method_register failed");

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
fn should_reject_authn_method_if_not_in_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    let result = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("authn_method_registration_mode_exit failed");

    let result = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));
    Ok(())
}

#[test]
fn should_reject_authn_method_if_registration_mode_is_expired() -> Result<(), CallError> {
    const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    env.advance_time(REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1));

    let result = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));
    Ok(())
}

#[test]
fn should_reject_confirmation_without_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    let result = api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        "invalid code",
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodConfirmationError::NoAuthnMethodToConfirm)
    ));
    Ok(())
}

#[test]
fn should_reject_confirmation_with_wrong_code() -> Result<(), CallError> {
    const MAX_RETRIES: u8 = 3;
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    for expected_retries in (0..MAX_RETRIES).rev() {
        assert!(matches!(
            api_v2::authn_method_confirm(
                &env,
                canister_id,
                authn_method.principal(),
                identity_number,
                "invalid code"
            )?,
            Err(AuthnMethodConfirmationError::WrongCode {
                retries_left
            }) if retries_left == expected_retries
        ));
    }

    assert!(matches!(
        api_v2::authn_method_confirm(
            &env,
            canister_id,
            authn_method.principal(),
            identity_number,
            "invalid code"
        )?,
        Err(AuthnMethodConfirmationError::RegistrationModeOff)
    ));
    Ok(())
}

#[test]
fn should_return_no_registration_when_no_tentative_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");

    assert!(identity_info.authn_method_registration.is_none());
    Ok(())
}

#[test]
fn should_return_registrations_when_tentative_device_not_verified() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Register a tentative device
    api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");

    assert!(identity_info.authn_method_registration.is_some());
    Ok(())
}

#[test]
fn should_return_no_registrations_when_tentative_device_verified() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Register a tentative device
    let add_response = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    // Confirm the tentative device
    api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.confirmation_code,
    )?
    .expect("authn_method_confirm failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");

    assert!(identity_info.authn_method_registration.is_none());
    Ok(())
}

#[test]
fn should_return_no_registrations_after_registration_mode_exit() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Register a tentative device
    api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    // Exit registration mode without confirming
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("authn_method_registration_mode_exit failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");

    assert!(identity_info.authn_method_registration.is_none());
    Ok(())
}

#[test]
fn should_return_identity_number_for_existing_registration_id() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id,
    )?;

    assert_eq!(result, Some(identity_number));
    Ok(())
}

#[test]
fn should_return_none_after_registration_mode_exit() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Exit registration mode
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
    )?
    .expect("authn_method_registration_mode_exit failed");

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id,
    )?;
    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_return_none_after_tentative_device_confirmation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Register and confirm a tentative device
    let add_response = api_v2::authn_method_register(
        &env,
        canister_id,
        identity_number,
        &sample_pubkey_authn_method(1),
    )?
    .expect("authn_method_register failed");

    api_v2::authn_method_confirm(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &add_response.confirmation_code,
    )?
    .expect("authn_method_confirm failed");

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_return_none_after_registration_mode_expiration() -> Result<(), CallError> {
    const REGISTRATION_MODE_EXPIRATION: std::time::Duration = std::time::Duration::from_secs(900);
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Advance time past expiration
    env.advance_time(REGISTRATION_MODE_EXPIRATION + std::time::Duration::from_secs(1));

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_return_none_for_registration_id_too_short() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let invalid_id = "abc1".to_string(); // Too short

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        invalid_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_return_none_for_registration_id_too_long() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let invalid_id = "abcdef".to_string(); // Too long

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        invalid_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_return_none_for_registration_id_invalid_chars() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let invalid_id = "abcdç".to_string(); // Invalid chars

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        invalid_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_handle_multiple_registration_ids() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method1 = test_authn_method();
    let authn_method2 = sample_pubkey_authn_method(1);

    let identity_number1 = create_identity_with_authn_method(&env, canister_id, &authn_method1);
    let identity_number2 = create_identity_with_authn_method(&env, canister_id, &authn_method2);

    let registration_mode_id1 = "abc12".to_string();
    let registration_mode_id2 = "def34".to_string();

    // Enter registration mode for first identity
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method1.principal(),
        identity_number1,
        Some(registration_mode_id1.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Enter registration mode for second identity
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method2.principal(),
        identity_number2,
        Some(registration_mode_id2.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Verify both lookups work correctly
    let result1 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id1,
    )?;

    let result2 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id2,
    )?;

    assert_eq!(result1, Some(identity_number1));
    assert_eq!(result2, Some(identity_number2));
    Ok(())
}

#[test]
fn should_exit_registrations_separately() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method1 = test_authn_method();
    let authn_method2 = sample_pubkey_authn_method(1);

    let identity_number1 = create_identity_with_authn_method(&env, canister_id, &authn_method1);
    let identity_number2 = create_identity_with_authn_method(&env, canister_id, &authn_method2);

    let registration_mode_id1 = "abc12".to_string();
    let registration_mode_id2 = "def34".to_string();

    // Enter registration mode for first identity
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method1.principal(),
        identity_number1,
        Some(registration_mode_id1.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Enter registration mode for second identity
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method2.principal(),
        identity_number2,
        Some(registration_mode_id2.clone()),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Exit registration mode for first identity
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method1.principal(),
        identity_number1,
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Verify both lookups work correctly
    let result1 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id1.clone(),
    )?;

    let result2 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id2.clone(),
    )?;

    assert_eq!(result1, None);
    assert_eq!(result2, Some(identity_number2));

    // Exit registration mode for second identity
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method2.principal(),
        identity_number2,
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Verify both lookups work correctly
    let result1 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id1,
    )?;

    let result2 = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        registration_mode_id2,
    )?;

    assert_eq!(result1, None);
    assert_eq!(result2, None);

    Ok(())
}

#[test]
fn should_return_none_for_nonexistent_registration_id() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let nonexistent_id = "abc12".to_string();

    let result = api_v2::lookup_by_registration_mode_id(
        &env,
        canister_id,
        Principal::anonymous(),
        nonexistent_id,
    )?;

    assert!(result.is_none());
    Ok(())
}

#[test]
fn should_reject_second_registration_with_different_id() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let first_registration_id = "0fZr4".to_string();
    let second_registration_id = "abc12".to_string();

    // First call with the first registration ID should succeed
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(first_registration_id),
    )?
    .expect("First authn_method_registration_mode_enter failed");

    // Second call with a different registration ID should fail with InvalidRegistrationId
    let result = api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(second_registration_id.clone()),
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodRegistrationModeEnterError::AlreadyInProgress)
    ));
    Ok(())
}

#[test]
fn should_return_registration_with_same_id() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let first_registration_id = "0fZr4".to_string();

    // First call with the first registration ID should succeed
    let result_1 = api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(first_registration_id.clone()),
    )?
    .expect("First authn_method_registration_mode_enter failed");

    // Second call with a different registration ID should fail with InvalidRegistrationId
    let result_2 = api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(first_registration_id.clone()),
    )?
    .expect("Second authn_method_registration_mode_enter failed");

    assert_eq!(result_1, result_2);
    Ok(())
}
