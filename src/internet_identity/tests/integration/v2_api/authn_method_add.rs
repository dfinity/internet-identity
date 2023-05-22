use crate::v2_api::authn_method_test_helpers::{
    eq_ignoring_last_authentication, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use ic_test_state_machine_client::{CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodAddResponse, AuthnMethodData, ChallengeAttempt, DeviceData,
    DeviceWithUsage, IdentityInfoResponse, IdentityNumber, MetadataEntry, PublicKeyAuthn,
    RegisterResponse,
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

    let identity_number = create_identity_with_authn_method(&env, canister_id, authn_method_1);
    let result = api_v2::authn_method_add(
        &env,
        canister_id,
        principal,
        identity_number,
        &authn_method_2,
    )?
    .unwrap();

    assert!(matches!(result, AuthnMethodAddResponse::Ok));

    let Some(IdentityInfoResponse::Ok(identity_info)) =
        api_v2::identity_info(&env, canister_id, principal, identity_number)? else {
        panic!("Expected identity info to be returned");
    };
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
    let identity_number =
        create_identity_with_authn_method(&env, canister_id, authn_method.clone());

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
        "key_type".to_string(),
        MetadataEntry::Bytes(ByteBuf::from("invalid")),
    );

    let identity_number = create_identity_with_authn_method(&env, canister_id, authn_method_1);
    let result = api_v2::authn_method_add(
        &env,
        canister_id,
        principal,
        identity_number,
        &authn_method_2,
    )?
    .unwrap();

    assert!(matches!(result, AuthnMethodAddResponse::InvalidMetadata(_)));

    Ok(())
}

fn sample_authn_method(i: u8) -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(vec![i; 32]),
        }),
        ..test_authn_method()
    }
}

fn create_identity_with_authn_method(
    env: &StateMachine,
    canister_id: CanisterId,
    authn_method: AuthnMethodData,
) -> IdentityNumber {
    let challenge = api::create_challenge(env, canister_id).unwrap();
    let device = DeviceData::from(DeviceWithUsage::try_from(authn_method).unwrap());
    let challenge_attempt = ChallengeAttempt {
        chars: "a".to_string(),
        key: challenge.challenge_key,
    };
    let RegisterResponse::Registered { user_number} = api::register(env, canister_id, device.principal(), &device, &challenge_attempt, None).unwrap() else {
        panic!("Expected device to be registered");
    };
    user_number
}
