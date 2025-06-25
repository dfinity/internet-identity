use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::{
    IdentityPropertiesReplace, IdentityPropertiesReplaceError,
};
use pocket_ic::CallError;

#[test]
fn should_set_name() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.name.is_none());

    let properties = IdentityPropertiesReplace {
        name: Some("Test Name".to_string()),
    };

    api_v2::identity_properties_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &properties,
    )?
    .expect("identity properties replace failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert_eq!(identity_info.name, Some("Test Name".to_string()));
    Ok(())
}

#[test]
fn should_clear_name() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    // First set a name
    let properties = IdentityPropertiesReplace {
        name: Some("Test Name".to_string()),
    };

    api_v2::identity_properties_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &properties,
    )?
    .expect("identity properties replace failed");

    // Verify name is set
    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert_eq!(identity_info.name, Some("Test Name".to_string()));

    // Now clear the name
    let properties = IdentityPropertiesReplace { name: None };

    api_v2::identity_properties_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &properties,
    )?
    .expect("identity properties replace failed");

    // Verify name is cleared
    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.name.is_none());
    Ok(())
}

#[test]
fn should_require_authentication_to_replace_identity_properties() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let properties = IdentityPropertiesReplace {
        name: Some("Test Name".to_string()),
    };

    let result = api_v2::identity_properties_replace(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &properties,
    )?;
    assert!(matches!(
        result,
        Err(IdentityPropertiesReplaceError::Unauthorized(_))
    ));
    Ok(())
}

#[test]
fn should_not_set_too_long_name() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let properties = IdentityPropertiesReplace {
        name: Some("a".repeat(3000)), // Very long name
    };

    let result = api_v2::identity_properties_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &properties,
    )?;
    assert!(matches!(
        result,
        Err(IdentityPropertiesReplaceError::NameTooLong { limit: 128 })
    ));
    Ok(())
}
