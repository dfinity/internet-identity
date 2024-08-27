use canister_tests::api::internet_identity::api_v2;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, ChallengeAttempt, IdentityNumber, MetadataEntryV2, PublicKeyAuthn,
    WebAuthn,
};
use pocket_ic::PocketIc;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

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

pub fn assert_eq_ignoring_last_authentication(
    authn_methods1: &[AuthnMethodData],
    authn_methods2: &[AuthnMethodData],
) {
    authn_methods1
        .iter()
        .zip(authn_methods2.iter())
        .for_each(|(a, b)| {
            assert!(
                eq_ignoring_last_authentication(a, b),
                "authn methods are not equal: {:?} != {:?}",
                a,
                b
            )
        });
}

pub fn test_authn_method() -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(vec![0; 32]),
        }),
        metadata: Default::default(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    }
}

pub fn create_identity_with_authn_method(
    env: &PocketIc,
    canister_id: CanisterId,
    authn_method: &AuthnMethodData,
) -> IdentityNumber {
    let challenge = api_v2::captcha_create(env, canister_id)
        .expect("API call failed")
        .expect("captcha_create failed");

    let challenge_attempt = ChallengeAttempt {
        chars: "a".to_string(),
        key: challenge.challenge_key,
    };
    api_v2::identity_register(
        env,
        canister_id,
        authn_method.principal(),
        authn_method,
        &challenge_attempt,
        None,
    )
    .expect("API call failed")
    .expect("identity_register failed")
}

pub fn create_identity_with_authn_methods(
    env: &PocketIc,
    canister_id: CanisterId,
    authn_methods: &[AuthnMethodData],
) -> IdentityNumber {
    let first_authn_method = authn_methods.first().expect("authn_methods is empty");
    let identity_number = create_identity_with_authn_method(env, canister_id, first_authn_method);

    for authn_method in authn_methods.iter().skip(1) {
        api_v2::authn_method_add(
            env,
            canister_id,
            first_authn_method.principal(),
            identity_number,
            authn_method,
        )
        .expect("API call failed")
        .expect("authn_method_add failed");
    }
    identity_number
}

pub fn sample_pubkey_authn_method(i: u8) -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(vec![i; 32]),
        }),
        ..test_authn_method()
    }
}

pub fn sample_webauthn_authn_method(i: u8) -> AuthnMethodData {
    AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: ByteBuf::from(vec![i; 32]),
            credential_id: ByteBuf::from(vec![i * 2; 32]),
        }),
        ..test_authn_method()
    }
}

pub fn sample_authn_methods() -> Vec<AuthnMethodData> {
    let authn_method1 = AuthnMethodData {
        metadata: HashMap::from([
            (
                "some_key".to_string(),
                MetadataEntryV2::String("some data".to_string()),
            ),
            (
                "origin".to_string(),
                MetadataEntryV2::String("https://some.origin".to_string()),
            ),
            (
                "alias".to_string(),
                MetadataEntryV2::String("Test Authn Method 1".to_string()),
            ),
        ]),
        ..sample_pubkey_authn_method(0)
    };

    let authn_method2 = AuthnMethodData {
        metadata: HashMap::from([(
            "different_key".to_string(),
            MetadataEntryV2::String("other data".to_string()),
        )]),
        ..sample_webauthn_authn_method(1)
    };

    let authn_method3 = AuthnMethodData {
        metadata: HashMap::from([
            (
                "recovery_metadata_1".to_string(),
                MetadataEntryV2::String("recovery data 1".to_string()),
            ),
            (
                "origin".to_string(),
                MetadataEntryV2::String("https://identity.ic0.app".to_string()),
            ),
            (
                "usage".to_string(),
                MetadataEntryV2::String("recovery_phrase".to_string()),
            ),
        ]),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Protected,
            purpose: AuthnMethodPurpose::Recovery,
        },
        ..sample_pubkey_authn_method(2)
    };

    let authn_method4 = AuthnMethodData {
        metadata: HashMap::default(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Recovery,
        },
        ..sample_webauthn_authn_method(3)
    };

    let authn_method5 = AuthnMethodData {
        metadata: HashMap::from([
            (
                "origin".to_string(),
                MetadataEntryV2::String("https://identity.internetcomputer.org".to_string()),
            ),
            (
                "alias".to_string(),
                MetadataEntryV2::String("Test Authn Method 5".to_string()),
            ),
            (
                "usage".to_string(),
                MetadataEntryV2::String("browser_storage_key".to_string()),
            ),
        ]),
        ..sample_webauthn_authn_method(4)
    };

    vec![
        authn_method1,
        authn_method2,
        authn_method3,
        authn_method4,
        authn_method5,
    ]
}
