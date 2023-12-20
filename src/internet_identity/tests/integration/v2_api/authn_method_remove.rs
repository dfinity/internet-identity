use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_pubkey_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, II_WASM,
};
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use regex::Regex;

#[test]
fn should_remove_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
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
    let canister_id = install_ii_canister(&env, II_WASM.clone());
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
