//! Tests related to openid_credential_add, openid_credential_remove, openid_prepare_delegation and openid_get_delegation

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, create_identity_with_openid_credential,
};
use candid::Principal;
use canister_tests::{api::internet_identity as api, framework::*};
use identity_jose::{jwk::Jwk, jws::Decoder};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, DeployArchiveResult, InternetIdentityInit, OpenIdConfig,
    OpenIdCredentialKey, OpenIdDelegationError, PublicKeyAuthn,
};
use pocket_ic::common::rest::{CanisterHttpReply, CanisterHttpResponse, MockCanisterHttpResponse};
use pocket_ic::{CallError, PocketIc};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use std::time::Duration;

/// Verifies that Google Accounts can be added
#[test]
fn can_link_google_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) = openid_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("{e:?}")))?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    Ok(())
}

/// Verifies that Google Accounts can be removed
#[test]
fn can_remove_google_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, claims, test_time, test_principal, test_authn_method) = openid_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("{e:?}")))?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    api::openid_credential_remove(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &claims.key(),
    )?
    .map_err(|e| CallError::Reject(format!("{e:?}")))?;

    // Try to get delegation based on credential, should fail now
    // Create session key
    let pub_session_key = ByteBuf::from("session public key");

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    // Prepare the delegation
    match api::openid_prepare_delegation(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
    )? {
        Ok(_) => panic!("We shouldn't be able to get a delegation here!"),
        Err(err) => match err {
            OpenIdDelegationError::NoSuchAnchor => Ok(()),
            _ => panic!("We should get a NoSuchAnchor error here!"),
        },
    }
}

/// Verifies that valid JWT delegations are issued based on added credential.
#[test]
fn can_get_valid_jwt_delegation() -> Result<(), CallError> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) = openid_test_data();

    // Create identity
    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    // Link Google Account to Identity
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("{e:?}")))?;

    // Create session key
    let pub_session_key = ByteBuf::from("session public key");

    // Prepare the delegation
    let prepare_response = match api::openid_prepare_delegation(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
    )? {
        Ok(response) => response,
        Err(err) => panic!("Failing at openid_prepare_delegation: {err:?}"),
    };

    assert_eq!(
        prepare_response.expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    // Get the delegation
    let signed_delegation = match api::openid_get_delegation(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
        &prepare_response.expiration,
    )? {
        Ok(signed_delegation) => signed_delegation,
        Err(err) => {
            panic!("Failing at openid_get_delegation: {err:?}")
        }
    };

    // Verify the delegation
    verify_delegation(
        &env,
        prepare_response.user_key,
        &signed_delegation,
        &env.root_key().unwrap(),
    );
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(
        signed_delegation.delegation.expiration,
        prepare_response.expiration
    );
    Ok(())
}

/// Verifies that you can register with google
#[test]
fn can_register_with_google() -> Result<(), CallError> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, _claims, test_time, test_principal, _test_authn_method) = openid_test_data();

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    // Create identity (this will panic if it doesn't work)
    // the test principal here is technically from webauthn, while in practice it would be a temporary random frontend keypair
    // however, this makes no functional difference. we just need a principal and salt together with a jwt
    // which contains a signed nonce derived from said principal and salt.

    let _identity_number =
        create_identity_with_openid_credential(&env, canister_id, &jwt, &salt, test_principal);

    Ok(())
}

/// Verifies that you cannot register with a faulty jwt
#[test]
#[should_panic]
fn cannot_register_with_faulty_jwt() {
    let env = env();

    let canister_id = setup_canister(&env);

    let (_jwt, salt, _claims, test_time, test_principal, _test_authn_method) = openid_test_data();

    let faulty_jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6Ijc2M2Y3YzRjZDI2YTFlYjJiMWIzOWE4OGY0NDM0ZDFmNGQ5YTM2OGIiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJmQkcxS3IzUWt5Z0dHelNJWG9Pd2p3RF95QjhXS0FfcVJPUlZjMFp0WHlJIiwibmJmIjoxNzQwNTgzNDEyLCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQwNTgzNzEyLCJleHAiOjE3NDA1ODczMTIsImp0aSI6IjhjNjkzMWE4YmVmZjllOWM3OTRmYjM5ZTkwNTExOTM4MTk4MDgxZDYifQ.PVAbLj1Fv7AUwH16nFiedJkmPOUg1UkPnAkVj6S9MDhpEV467tP7iOxQCx64i0_imTymcjkzH9pcfTsaKpY8fWPrWSWZzDy9S4GygjOQeg13NXg_H23X2-IY_OVHKqtrAibhZZUppvczijqZja7-HmUivoAJIGsMOk1IxbJdalOhE5yQtsYEx4ZBxFemR7CTfMzopsAaRWgPHI7T0MENuiCbkSy_NYQPBzNpmGcKoZoyUbleFUzej8gbkqpoIUVdfwuNtoe_TMjED5eqJxi1Pip85iy4wJTa2RKUTZxUfqVCaTEftVt8U-PV1UgPsxpu0mKS5z5bXylmgclUzcNnmh";

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    // Create identity - this will panic if it doesn't work. It should panic as we are using a faulty jwt.

    let _identity_number = create_identity_with_openid_credential(
        &env,
        canister_id,
        faulty_jwt,
        &salt,
        test_principal,
    );
}

/// Verifies that JWT cannot be maliciously gotten by reassociating the credential and anchors between the prepare and get calls.
#[test]
fn cannot_get_valid_jwt_delegation_after_reassociation() -> Result<(), CallError> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, claims, test_time, test_principal, test_authn_method_data) = openid_test_data();
    let (
        second_jwt,
        second_salt,
        _second_claims,
        second_test_time,
        second_test_principal,
        second_test_authn_method_data,
    ) = second_openid_test_data();

    // Create identity
    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &test_authn_method_data);

    // Link Google Account to Identity
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    let second_time_to_advance =
        Duration::from_millis(second_test_time) - Duration::from_millis(test_time);

    env.advance_time(time_to_advance);

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("Error at first add: {e:?}")))?;

    // Create session key
    let pub_session_key = ByteBuf::from("session public key");

    // Prepare the delegation
    let prepare_response = match api::openid_prepare_delegation(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
    )? {
        Ok(response) => response,
        Err(err) => panic!("Failing at openid_prepare_delegation: {err:?}"),
    };

    assert_eq!(
        prepare_response.expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    api::openid_credential_remove(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &claims.key(),
    )?
    .map_err(|e| CallError::Reject(format!("Error at remove: {e:?}")))?;

    env.advance_time(second_time_to_advance);

    let second_identity_number =
        create_identity_with_authn_method(&env, canister_id, &second_test_authn_method_data);

    api::openid_credential_add(
        &env,
        canister_id,
        second_test_principal,
        second_identity_number,
        &second_jwt,
        &second_salt,
    )?
    .map_err(|e| CallError::Reject(format!("Error at second add: {e:?}")))?;

    // Get the delegation
    match api::openid_get_delegation(
        &env,
        canister_id,
        second_test_principal,
        &second_jwt,
        &second_salt,
        &pub_session_key, // Note that this only works if we have access to the original session key
        &prepare_response.expiration,
    )? {
        Ok(_) => Err(CallError::Reject(
            "We shouldn't be able to get this delegation!".to_string(),
        )),
        Err(_) => Ok(()),
    }
}

static CLIENT_ID: &str = "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com";

#[derive(Deserialize)]
#[allow(dead_code)]
pub struct Claims {
    iss: String,
    sub: String,
    aud: String,
    nonce: String,
    iat: u64,
    // Optional Google specific claims
    email: Option<String>,
    name: Option<String>,
    picture: Option<String>,
}

impl Claims {
    fn key(&self) -> OpenIdCredentialKey {
        (self.iss.clone(), self.sub.clone())
    }
}

#[derive(Serialize, Deserialize)]
struct Certs {
    keys: Vec<Jwk>,
}

pub fn setup_canister(env: &PocketIc) -> Principal {
    let args = InternetIdentityInit {
        openid_google: Some(Some(OpenIdConfig {
            client_id: CLIENT_ID.to_string(),
        })),
        archive_config: Some(ArchiveConfig {
            module_hash: archive_wasm_hash(&ARCHIVE_WASM),
            entries_buffer_limit: 10_000,
            polling_interval_ns: Duration::from_secs(1).as_nanos() as u64,
            entries_fetch_limit: 10,
        }),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };
    // Cycles are needed before installation because of the async HTTP outcalls
    let canister_id = install_ii_canister_with_arg_and_cycles(
        env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );

    match api::deploy_archive(env, canister_id, &ARCHIVE_WASM) {
        Ok(DeployArchiveResult::Success(_archive_principal)) => {
            // Successfully deployed.
        }
        Ok(unexpected_result) => {
            panic!("archive deployment returned unexpected Ok result: {unexpected_result:?}");
        }
        Err(err) => {
            panic!("archive deployment failed: {err:?}");
        }
    }

    // Mock google certs response
    mock_google_certs_response(env);

    canister_id
}

fn mock_google_certs_response(env: &PocketIc) {
    const MAX_ATTEMPTS: u32 = 10;
    let mut attempts = 0;

    loop {
        env.tick();
        attempts += 1;

        let requests = env.get_canister_http();

        if let Some(cert_request) = requests
            .iter()
            .find(|req| req.url == "https://www.googleapis.com/oauth2/v3/certs")
        {
            // Use the same test certificate data that's used in google.rs
            let mock_certs = serde_json::from_str::<Certs>(r#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"25f8211713788b6145474b5029b0141bd5b3de9c","n":"0qTcwnqUqJqsyu57JAC4IOAgTuMrccabAKKj5T93F68NoCk4kAax0oJhDArisYpiLrQ__YJJ9HFm3TKkuiPZeb1xqSSXAnIZVo8UigTLQDQLCTq3O-aD5EyQTOhOHWxJBZcpyLO-dZVuOIbv8fNMcXpNCioHVHO04gI_mvaw8ZzbU_j8ZeHSPk4wTBNfmH4l0mYRDhoQHLkZxxvc2V71ppBPYbnX-4t6h7XcuTkLJKBxfrR43G5nNzDuFsIbBnS2fjVLEv_1LYj9G5Q5XwiCFS0BON-oqQNzRWF53nkf91bMm2TaROg21KKJbZqfEjUhCVlMDFmBW-MNv69-C19PZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"5d12ab782cb6096285f69e48aea99079bb59cb86","n":"uac7NRcojCutcceWq1nrpLGJjQ7ywvgWsUcb1DWMKJ3KNNHiRzh9jshoi9tmq1zlarJ_h7GQg8iU1qD7SgpVYJmjlKG1MNVRAtuNrNMC0UAnNfG7mBBNorHFndfp-9cLTiMjXSXRzhNqiMvTVKeolRdMB2lH9RzJnwlpXtvUbD7M1pXOlPlMaOy1zxUnHn0uszU5mPRQk79i03BNrAdhwrAUB-ZuMnqpjaUcb9VU3KIwuZNPtsVenLN12sRYpaZ6WBw8Q9q7fAoaJUovM0Go8deC9pJYyxJuHdVo9HP0osyzg3g_rOYi14wmvMBuiDf3F4pTnudAfFyl3d0Mn_i4ZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b","n":"y8TPCPz2Fp0OhBxsxu6d_7erT9f9XJ7mx7ZJPkkeZRxhdnKtg327D4IGYsC4fLAfpkC8qN58sZGkwRTNs-i7yaoD5_8nupq1tPYvnt38ddVghG9vws-2MvxfPQ9m2uxBEdRHmels8prEYGCH6oFKcuWVsNOt4l_OPoJRl4uiuiwd6trZik2GqDD_M6bn21_w6AD_jmbzN4mh8Od4vkA1Z9lKb3Qesksxdog-LWHsljN8ieiz1NhbG7M-GsIlzu-typJfud3tSJ1QHb-E_dEfoZ1iYK7pMcojb5ylMkaCj5QySRdJESq9ngqVRDjF4nX8DK5RQUS7AkrpHiwqyW0Csw","e":"AQAB"}]}"#).unwrap().keys;

            let http_response = CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                status: 200,
                headers: vec![],
                body: serde_json::to_vec(&Certs { keys: mock_certs }).unwrap(),
            });

            let response = MockCanisterHttpResponse {
                subnet_id: cert_request.subnet_id,
                request_id: cert_request.request_id,
                response: http_response,
                additional_responses: vec![],
            };

            env.mock_canister_http_response(response);
            env.tick();
            return;
        }

        if attempts >= MAX_ATTEMPTS {
            panic!("No Google cert requests found after {MAX_ATTEMPTS} attempts");
        }
    }
}

pub fn openid_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6Ijc2M2Y3YzRjZDI2YTFlYjJiMWIzOWE4OGY0NDM0ZDFmNGQ5YTM2OGIiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJmQkcxS3IzUWt5Z0dHelNJWG9Pd2p3RF95QjhXS0FfcVJPUlZjMFp0WHlJIiwibmJmIjoxNzQwNTgzNDEyLCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQwNTgzNzEyLCJleHAiOjE3NDA1ODczMTIsImp0aSI6IjhjNjkzMWE4YmVmZjllOWM3OTRmYjM5ZTkwNTExOTM4MTk4MDgxZDYifQ.PVAbLj1Fv7AUwH16nFiedJkmPOUg1UkPnAkVj6S9MDhpEV467tP7iOxQCx64i0_imTymcjkzH9pcfTsaKpY8fWPrWSWZzDy9S4GygjOQeg13NXg_H23X2-IY_OVHKqtrAibhZZUppvczijqZja7-HmUivoAJIGsMOk1IxbJdalOhE5yQtsYEx4ZBxFemR7CTfMzopsAaRWgPHI7T0MENuiCbkSy_NYQPBzNpmGcKoZoyUbleFUzej8gbkqpoIUVdfwuNtoe_TMjED5eqJxi1Pip85iy4wJTa2RKUTZxUfqVCaTEftVt8U-PV1UgPsxpu0mKS5z5bXylmgclUzcNnmg";
    let salt: [u8; 32] = [
        107, 14, 204, 55, 92, 39, 93, 230, 53, 20, 153, 234, 70, 25, 120, 74, 136, 94, 251, 187,
        238, 96, 97, 180, 255, 135, 20, 149, 143, 27, 159, 83,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1740583715239;
    let test_principal = Principal::from_slice(&[
        211, 40, 186, 145, 43, 2, 6, 17, 232, 23, 22, 44, 51, 178, 233, 163, 131, 231, 82, 174, 66,
        201, 203, 1, 102, 109, 20, 75, 2,
    ]);

    // This is the public key of the credential used to sign in
    // You get it with connection.identity.credentialData[0].pubkey
    // Notably, you don't get it with connection.identity.getPublicKey()
    // Even though you get the above principal with connection.identity.getPrincipal()

    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 252, 182, 240, 218, 160, 61, 178, 176, 17, 228, 185, 84, 148, 45, 86, 216,
        171, 120, 72, 246, 212, 55, 212, 167, 142, 59, 227, 0, 242, 182, 129, 211, 34, 88, 32, 158,
        197, 96, 131, 51, 156, 176, 65, 128, 29, 75, 98, 163, 187, 104, 38, 255, 65, 92, 234, 229,
        245, 221, 74, 40, 202, 29, 83, 162, 84, 177, 204,
    ];

    let test_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(test_pubkey),
        }),
        metadata: Default::default(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    (
        jwt.into(),
        salt,
        claims,
        test_time,
        test_principal,
        test_authn_method,
    )
}

fn second_openid_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6IjI1ZjgyMTE3MTM3ODhiNjE0NTQ3NGI1MDI5YjAxNDFiZDViM2RlOWMiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJHc1NfbzBPa05CTF9PMTkxZVNOR0lEeHhmYzdXTjNrZGdhWUpMcWFhSHRrIiwibmJmIjoxNzQxMDE2NjAyLCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQxMDE2OTAyLCJleHAiOjE3NDEwMjA1MDIsImp0aSI6ImZkZjYwYmU3ZGE4NTcxMDRiMWYzZWM0MDdmZDI0NTVhZjBiMzQ2MGQifQ.NBtQpzUb5F5qWLovJNaTbqLgw3ieeWqpmdvN94YwJ8SK3QHeMhg9nq18gzQeUIEsAItwtmJJUTI4VRuVJUtrZIdKwl1Y0Cv0hSeLKDMON3N5juysvkmm8uJC5PV14mAZTPwbN6uK3hZHEddnQGnTWZzh2QvjBPdFehQ9yMce_VregMuirPpEXHX-qRfy9QYw7FxNpy6zw7pqLW_cicMM2WP_7g1eUryD6RgsB9V_QCLftnDlIhB70pPiZ7dnCIZtTqT4NV_8WfCowXw-nfcJ001tgoQHoSd_o1uRDRheGYwpk7cdRRovratwFQKPxmeweVuqUxeYUVmCHqPa7Y5qsg";
    let salt: [u8; 32] = [
        73, 220, 36, 27, 90, 88, 236, 203, 175, 35, 73, 47, 62, 19, 239, 54, 105, 37, 123, 90, 175,
        248, 124, 179, 244, 231, 182, 142, 180, 139, 171, 253,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1741016902000;
    let test_principal = Principal::from_slice(&[
        189, 168, 196, 34, 223, 103, 250, 254, 55, 167, 15, 174, 41, 207, 68, 219, 125, 21, 215,
        167, 119, 47, 20, 195, 139, 233, 255, 210, 2,
    ]);

    // This is the public key of the credential used to sign in
    // You get it with connection.identity.credentialData[0].pubkey
    // Notably, you don't get it with connection.identity.getPublicKey()
    // Even though you get the above principal with connection.identity.getPrincipal()

    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 186, 6, 79, 74, 150, 108, 73, 69, 11, 154, 213, 120, 228, 162, 244, 219, 50,
        15, 108, 166, 154, 59, 197, 43, 180, 128, 122, 81, 145, 5, 55, 89, 34, 88, 32, 110, 143,
        94, 76, 94, 197, 172, 41, 10, 127, 224, 31, 66, 150, 206, 21, 4, 148, 86, 141, 117, 36, 16,
        119, 242, 232, 155, 6, 154, 223, 6, 123,
    ];

    let test_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(test_pubkey),
        }),
        metadata: Default::default(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    (
        jwt.into(),
        salt,
        claims,
        test_time,
        test_principal,
        test_authn_method,
    )
}

fn number_of_openid_credentials(
    env: &PocketIc,
    canister_id: Principal,
    sender: Principal,
    identity_number: u64,
) -> Result<usize, CallError> {
    let openid_credentials = api::get_anchor_info(env, canister_id, sender, identity_number)
        .map_err(|err| CallError::Reject(format!("{err:?}")))?
        .openid_credentials
        .ok_or("Could not fetch credentials!")
        .map_err(|err| CallError::Reject(format!("{err:?}")))?;

    Ok(openid_credentials.len())
}
