use crate::api::internet_identity::{create_challenge, register};
use crate::framework::{device_data_1, principal_1};
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::StateMachine;
use internet_identity_interface::{AnchorNumber, ChallengeAttempt, DeviceData, RegisterResponse};

pub fn register_anchor(env: &StateMachine, canister_id: CanisterId) -> AnchorNumber {
    register_anchor_with(env, canister_id, principal_1(), &device_data_1())
}

pub fn register_anchor_with(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    device_data: &DeviceData,
) -> AnchorNumber {
    let challenge = create_challenge(env, canister_id).expect("challenge creation failed");
    let user_number = match register(
        env,
        canister_id,
        sender,
        device_data,
        ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    ) {
        Ok(RegisterResponse::Registered { user_number }) => user_number,
        response => panic!("could not register: {response:?}"),
    };
    user_number
}
