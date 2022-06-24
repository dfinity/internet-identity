use crate::api::{create_challenge, register};
use crate::framework::{device_data_1, principal_1};
use ic_state_machine_tests::{CanisterId, StateMachine};
use ic_types::PrincipalId;
use internet_identity_interface::{ChallengeAttempt, DeviceData, RegisterResponse, UserNumber};

pub fn register_anchor(env: &StateMachine, canister_id: CanisterId) -> UserNumber {
    register_anchor_with(env, canister_id, principal_1(), device_data_1())
}

pub fn register_anchor_with(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    device_data: DeviceData,
) -> UserNumber {
    let challenge = create_challenge(&env, canister_id);
    let user_number = match register(
        &env,
        canister_id,
        sender,
        device_data,
        ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    ) {
        Ok(RegisterResponse::Registered { user_number }) => user_number,
        response => panic!("could not register: {:?}", response),
    };
    user_number
}
