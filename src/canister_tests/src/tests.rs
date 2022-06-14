use crate::api;
use crate::framework;
use ic_state_machine_tests::StateMachine;
use internet_identity_interface as types;

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
    let user_number = match api::register(
        &env,
        canister_id,
        framework::some_device_data(),
        types::ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    ) {
        types::RegisterResponse::Registered { user_number } => user_number,
        response => panic!("could not register: {:?}", response),
    };
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

    let retrieved_device_data = api::lookup(&env, canister_id, user_number);

    assert_eq!(retrieved_device_data, vec![framework::some_device_data()]);
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
