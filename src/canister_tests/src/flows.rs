use crate::api::internet_identity::{create_challenge, http_request, register};
use crate::framework::{device_data_1, principal_1};
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::{
    ChallengeAttempt, DeviceData, HttpRequest, RegisterResponse, UserNumber,
};
use serde_bytes::ByteBuf;
use state_machine_client::StateMachine;

pub fn register_anchor(env: &StateMachine, canister_id: CanisterId) -> UserNumber {
    register_anchor_with(env, canister_id, principal_1(), &device_data_1())
}

pub fn register_anchor_with(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    device_data: &DeviceData,
) -> UserNumber {
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
        response => panic!("could not register: {:?}", response),
    };
    user_number
}

pub fn get_metrics(env: &StateMachine, canister_id: CanisterId) -> String {
    let response = http_request(
        env,
        canister_id,
        HttpRequest {
            method: "GET".to_string(),
            url: "/metrics".to_string(),
            headers: vec![],
            body: ByteBuf::new(),
        },
    )
    .expect("HTTP request to /metrics failed");
    String::from_utf8_lossy(&*response.body).to_string()
}
