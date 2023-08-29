use canister_tests::api::internet_identity as api;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::StateMachine;
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, ChallengeAttempt, DeviceData,
    DeviceWithUsage, IdentityNumber, PublicKeyAuthn, Purpose, RegisterResponse,
};
use serde_bytes::ByteBuf;

pub fn eq_ignoring_last_authentication(a: &AuthnMethodData, b: &AuthnMethodData) -> bool {
    let a = AuthnMethodData {
        last_authentication: None,
        ..a.clone()
    };
    let b = AuthnMethodData {
        last_authentication: None,
        ..b.clone()
    };
    a == b
}

pub fn test_authn_method() -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(vec![0; 32]),
        }),
        metadata: Default::default(),
        protection: AuthnMethodProtection::Unprotected,
        purpose: Purpose::Authentication,
        last_authentication: None,
    }
}

pub fn create_identity_with_authn_method(
    env: &StateMachine,
    canister_id: CanisterId,
    authn_method: &AuthnMethodData,
) -> IdentityNumber {
    let challenge = api::create_challenge(env, canister_id).unwrap();
    let device = DeviceData::from(DeviceWithUsage::try_from(authn_method.clone()).unwrap());
    let challenge_attempt = ChallengeAttempt {
        chars: "a".to_string(),
        key: challenge.challenge_key,
    };
    let RegisterResponse::Registered { user_number } = api::register(
        env,
        canister_id,
        device.principal(),
        &device,
        &challenge_attempt,
        None,
    )
    .unwrap() else {
        panic!("Expected device to be registered");
    };
    user_number
}

pub fn sample_authn_method(i: u8) -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(vec![i; 32]),
        }),
        ..test_authn_method()
    }
}
