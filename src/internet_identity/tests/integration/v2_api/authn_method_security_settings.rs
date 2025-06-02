use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, expect_user_error_with_message, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose, AuthnMethodSecuritySettings,
    AuthnMethodSecuritySettingsReplaceError, MetadataEntryV2,
};
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

#[test]
fn should_replace_authn_method_security_settings() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = recovery_phrase_authn_method();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_security_settings = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .security_settings;
    assert_eq!(actual_security_settings, &authn_method.security_settings);

    let new_security_settings = AuthnMethodSecuritySettings {
        protection: AuthnMethodProtection::Protected,
        purpose: AuthnMethodPurpose::Recovery,
    };
    api_v2::authn_method_security_settings_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &authn_method.public_key(),
        &new_security_settings,
    )?
    .expect("security settings replace failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_security_settings = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .security_settings;
    assert_eq!(actual_security_settings, &new_security_settings);
    Ok(())
}

#[test]
fn should_require_authentication_to_replace_security_settings() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = recovery_phrase_authn_method();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_security_settings = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .security_settings;
    assert_eq!(actual_security_settings, &authn_method.security_settings);

    let new_security_settings = AuthnMethodSecuritySettings {
        protection: AuthnMethodProtection::Protected,
        purpose: AuthnMethodPurpose::Recovery,
    };
    let result = api_v2::authn_method_security_settings_replace(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &authn_method.public_key(),
        &new_security_settings,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
    Ok(())
}

#[test]
fn should_check_authn_method_exists() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = recovery_phrase_authn_method();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_security_settings = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .security_settings;
    assert_eq!(actual_security_settings, &authn_method.security_settings);

    let new_security_settings = AuthnMethodSecuritySettings {
        protection: AuthnMethodProtection::Protected,
        purpose: AuthnMethodPurpose::Recovery,
    };
    let result = api_v2::authn_method_security_settings_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &ByteBuf::from(vec![1, 2, 3, 4]),
        &new_security_settings,
    )?;

    assert!(matches!(
        result,
        Err(AuthnMethodSecuritySettingsReplaceError::AuthnMethodNotFound)
    ));
    Ok(())
}

fn recovery_phrase_authn_method() -> AuthnMethodData {
    AuthnMethodData {
        metadata: HashMap::from([(
            "usage".to_string(),
            MetadataEntryV2::String("recovery_phrase".to_string()),
        )]),
        ..test_authn_method()
    }
}
