use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_pubkey_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, expect_user_error_with_message, install_ii_with_archive};
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;

#[test]
fn should_remove_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method_1 = sample_pubkey_authn_method(1);
    let principal = authn_method_1.principal();
    let authn_method_2 = sample_pubkey_authn_method(2);

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method_1);
    api_v2::authn_method_add(
        &env,
        canister_id,
        principal,
        identity_number,
        &authn_method_2,
    )?
    .expect("authn method add failed");

    let identity_info = api_v2::identity_info(&env, canister_id, principal, identity_number)?
        .expect("identity info failed");

    assert_eq!(identity_info.authn_methods.len(), 2);

    api_v2::authn_method_remove(
        &env,
        canister_id,
        principal,
        identity_number,
        &authn_method_2.public_key(),
    )?
    .expect("authn method remove failed");
    Ok(())
}

#[test]
fn should_require_authentication_to_remove_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_pubkey_authn_method(1);

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let result = api_v2::authn_method_remove(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &authn_method.public_key(),
    );
    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
    Ok(())
}

/// Verifies that the even last authn_method can be removed.
/// This behaviour should be changed because it makes anchors unusable, see GIX-745.
#[test]
fn should_remove_last_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = sample_pubkey_authn_method(1);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");

    assert_eq!(identity_info.authn_methods.len(), 1);

    api_v2::authn_method_remove(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &authn_method.public_key(),
    )?
    .expect("authn method remove failed");

    let identity_authn_info = api_v2::identity_authn_info(&env, canister_id, identity_number)?
        .expect("identity_authn_info failed");

    assert_eq!(identity_authn_info.authn_methods.len(), 0);
    assert_eq!(identity_authn_info.recovery_authn_methods.len(), 0);
    Ok(())
}
