use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, eq_ignoring_last_authentication, sample_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, II_WASM,
};
use canister_tests::match_value;
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodAddResponse, IdentityInfoResponse, MetadataEntry,
};
use regex::Regex;
use serde_bytes::ByteBuf;

#[test]
fn should_add_authn_method() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method_1 = sample_authn_method(1);
    let principal = authn_method_1.principal();
    let authn_method_2 = sample_authn_method(2);

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method_1);
    match_value!(
        api_v2::authn_method_add(
            &env,
            canister_id,
            principal,
            identity_number,
            &authn_method_2,
        )?,
        Some(AuthnMethodAddResponse::Ok)
    );

    match_value!(
        api_v2::identity_info(&env, canister_id, principal, identity_number)?,
        Some(IdentityInfoResponse::Ok(identity_info))
    );

    assert!(eq_ignoring_last_authentication(
        &identity_info.authn_methods[1],
        &authn_method_2
    ));
    Ok(())
}

#[test]
fn should_require_authentication_for_identity_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = sample_authn_method(1);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let result = api_v2::authn_method_add(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &authn_method,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
    Ok(())
}

#[test]
fn should_report_error_on_failed_conversion() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method_1 = sample_authn_method(1);
    let principal = authn_method_1.principal();
    let mut authn_method_2 = sample_authn_method(2);
    authn_method_2.metadata.insert(
        "usage".to_string(),
        MetadataEntry::Bytes(ByteBuf::from("invalid")),
    );

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method_1);

    match_value!(
        api_v2::authn_method_add(
            &env,
            canister_id,
            principal,
            identity_number,
            &authn_method_2,
        )?,
        Some(AuthnMethodAddResponse::InvalidMetadata(_))
    );

    Ok(())
}
