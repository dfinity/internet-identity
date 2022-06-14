use crate::api::register;
use crate::framework::{principal_1, principal_2, CallError};
use crate::{api, framework};
use framework::device_data_1;
use ic_error_types::ErrorCode;
use ic_state_machine_tests::StateMachine;
use internet_identity_interface as types;
use regex::Regex;

#[test]
fn ii_canister_can_be_installed() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_works() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_retains_anchors() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    let challenge = api::create_challenge(&env, canister_id);
    let user_number = match register(
        &env,
        canister_id,
        principal_1(),
        device_data_1(),
        types::ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    )
    .unwrap()
    {
        types::RegisterResponse::Registered { user_number } => user_number,
        response => panic!("could not register: {:?}", response),
    };
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

    let retrieved_device_data = api::lookup(&env, canister_id, user_number);

    assert_eq!(retrieved_device_data, vec![device_data_1()]);
}

#[test]
fn ii_canister_can_be_upgraded_and_rolled_back() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
    api::health_check(&env, canister_id);
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);
}

#[test]
fn registration_with_mismatched_sender_fails() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    let challenge = api::create_challenge(&env, canister_id);
    let result = register(
        &env,
        canister_id,
        principal_2(),
        device_data_1(),
        types::ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    );

    assert!(matches!(result,
            Err(CallError::UserError(user_error)) if user_error.code() == ErrorCode::CanisterCalledTrap &&
            Regex::new("[a-z0-9-]+ could not be authenticated against").unwrap().is_match(user_error.description())));
}
