use canister_tests::api::internet_identity::api_v2;
use canister_tests::match_value;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::StateMachine;
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, CaptchaCreateResponse, ChallengeAttempt,
    IdentityNumber, IdentityRegisterResponse, PublicKeyAuthn, Purpose,
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
    match_value!(
        api_v2::captcha_create(env, canister_id).unwrap(),
        Some(CaptchaCreateResponse::Ok(challenge))
    );

    let challenge_attempt = ChallengeAttempt {
        chars: "a".to_string(),
        key: challenge.challenge_key,
    };
    match_value!(
        api_v2::identity_register(
            env,
            canister_id,
            authn_method.principal(),
            authn_method,
            &challenge_attempt,
            None,
        ),
        Ok(Some(IdentityRegisterResponse::Ok(user_number)))
    );
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
