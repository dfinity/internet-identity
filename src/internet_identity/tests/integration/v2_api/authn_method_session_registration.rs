use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_webauthn_authn_method,
};
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodConfirmationError, AuthnMethodRegisterError, AuthnMethodRegistration,
};
use pocket_ic::RejectResponse;
use std::time::Duration;

#[test]
fn should_register_authn_method_through_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let existing_authn_method = sample_webauthn_authn_method(0);
    let new_authn_method = sample_webauthn_authn_method(1);
    let session = sample_webauthn_authn_method(2);
    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &existing_authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        existing_authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");

    // Register session
    let add_response = api_v2::authn_method_session_register(
        &env,
        canister_id,
        session.principal(),
        identity_number,
    )?
    .expect("authn_method_session_register failed");

    // Assert that correct session has been tentatively registered
    let identity_info = api_v2::identity_info(
        &env,
        canister_id,
        existing_authn_method.principal(),
        identity_number,
    )?
    .expect("identity_info failed");
    assert_eq!(
        identity_info.authn_method_registration,
        Some(AuthnMethodRegistration {
            expiration: add_response.expiration,
            session: Some(session.principal()),
            authn_method: None,
        })
    );

    // Confirm session
    api_v2::authn_method_confirm(
        &env,
        canister_id,
        existing_authn_method.principal(),
        identity_number,
        &add_response.confirmation_code,
    )?
    .expect("authn_method_confirm failed");

    // Exit registration mode with authn method
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        session.principal(),
        identity_number,
        Some(new_authn_method.clone()),
    )?
    .expect("authn_method_registration_mode_exit failed");

    // Assert that we can authenticate with new authn method
    let updated_identity_info = api_v2::identity_info(
        &env,
        canister_id,
        new_authn_method.principal(),
        identity_number,
    )?
    .expect("Unable to authenticate with new authn method");

    // Assert new authn method is listed
    assert!(updated_identity_info
        .authn_methods
        .into_iter()
        .map(|data| data.authn_method)
        .collect::<Vec<_>>()
        .contains(&new_authn_method.authn_method));

    // Assert registration mode is closed
    assert_eq!(updated_identity_info.authn_method_registration, None);

    Ok(())
}

#[test]
fn should_confirm_session_after_failed_attempt() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_webauthn_authn_method(0);
    let session = sample_webauthn_authn_method(1);
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

    // Register session
    let add_response = api_v2::authn_method_session_register(
        &env,
        canister_id,
        session.principal(),
        identity_number,
    )?
    .expect("authn_method_session_register failed");

    // Confirm session with wrong code
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

    // Assert we can confirm session with correct code afterward
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
fn should_reject_session_if_not_in_registration_mode() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_webauthn_authn_method(0);
    let session = sample_webauthn_authn_method(1);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Assert registration mode is off initially and thus registrations are rejected
    let result = api_v2::authn_method_session_register(
        &env,
        canister_id,
        session.principal(),
        identity_number,
    )?;
    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));

    // Enter and exit registration mode
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
        None,
    )?
    .expect("authn_method_registration_mode_exit failed");

    // Assert registrations are rejected after exiting registration mode
    let result = api_v2::authn_method_session_register(
        &env,
        canister_id,
        session.principal(),
        identity_number,
    )?;
    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));

    Ok(())
}

#[test]
fn should_reject_session_if_registration_mode_is_expired() -> Result<(), RejectResponse> {
    const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_webauthn_authn_method(0);
    let session = sample_webauthn_authn_method(1);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode and let it expire
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");
    env.advance_time(REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1));

    // Assert registrations are rejected after registration mode has expired
    let result = api_v2::authn_method_session_register(
        &env,
        canister_id,
        session.principal(),
        identity_number,
    )?;
    assert!(matches!(
        result,
        Err(AuthnMethodRegisterError::RegistrationModeOff)
    ));

    Ok(())
}

#[test]
fn should_reject_confirmation_with_wrong_code() -> Result<(), RejectResponse> {
    const MAX_RETRIES: u8 = 3;
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_webauthn_authn_method(0);
    let session = sample_webauthn_authn_method(1);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let registration_mode_id = "0fZr4".to_string();

    // Enter registration mode and register session
    api_v2::authn_method_registration_mode_enter(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )?
    .expect("authn_method_registration_mode_enter failed");
    api_v2::authn_method_session_register(&env, canister_id, session.principal(), identity_number)?
        .expect("authn_method_session_register failed");

    // Assert multiple wrong code attempts are all rejected
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

    // Assert that registration mode is off after too many attempts
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
fn should_return_no_registrations_after_registration_mode_exit() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_webauthn_authn_method(0);
    let session = sample_webauthn_authn_method(1);
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

    // Register a tentative session
    api_v2::authn_method_session_register(&env, canister_id, session.principal(), identity_number)?
        .expect("authn_method_session_register failed");

    // Exit registration mode without confirming
    api_v2::authn_method_registration_mode_exit(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        None,
    )?
    .expect("authn_method_registration_mode_exit failed");

    // Assert registration mode is off
    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity_info failed");
    assert_eq!(identity_info.authn_method_registration, None);

    Ok(())
}
