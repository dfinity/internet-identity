//! Tests related to openid_credential_add, openid_credential_remove, openid_prepare_delegation and openid_get_delegation

use crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method;
use candid::Principal;
use canister_tests::{api::internet_identity as api, framework::*};
use identity_jose::{jwk::Jwk, jws::Decoder};
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, CaptchaConfig, CaptchaTrigger, InternetIdentityInit, OpenIdConfig,
    OpenIdCredentialKey, OpenIdDelegationError, PublicKeyAuthn, StaticCaptchaTrigger,
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

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("{:?}", e)))
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

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?
    .map_err(|e| CallError::Reject(format!("{:?}", e)))?;

    api::openid_credential_remove(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &claims.key(),
    )?
    .map_err(|e| CallError::Reject(format!("{:?}", e)))?;

    // Try to get delegation based on credential, should fail now
    // Create session key
    let pub_session_key = ByteBuf::from("session public key");

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

/// Verifies that valid JWT delegations are issued based on added .
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
    .map_err(|e| CallError::Reject(format!("{:?}", e)))?;

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
        Err(err) => panic!("Failing at openid_prepare_delegation: {:?}", err),
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
            panic!("Failing at openid_get_delegation: {:?}", err)
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
    .map_err(|e| CallError::Reject(format!("Error at first add: {:?}", e)))?;

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
        Err(err) => panic!("Failing at openid_prepare_delegation: {:?}", err),
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
    .map_err(|e| CallError::Reject(format!("Error at remove: {:?}", e)))?;

    env.advance_time(second_time_to_advance);

    let second_identity_number =
        create_identity_with_authn_method(&env, canister_id, &second_test_authn_method_data);

    println!("{}", time(&env));

    api::openid_credential_add(
        &env,
        canister_id,
        second_test_principal,
        second_identity_number,
        &second_jwt,
        &second_salt,
    )?
    .map_err(|e| CallError::Reject(format!("Error at second add: {:?}", e)))?;

    // Get the delegation
    match api::openid_get_delegation(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
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
struct Claims {
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

fn setup_canister(env: &PocketIc) -> Principal {
    let args = InternetIdentityInit {
        openid_google: Some(Some(OpenIdConfig {
            client_id: CLIENT_ID.to_string(),
        })),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 50, // same as in our dfx.json
            captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaDisabled),
        }),
        ..Default::default()
    };
    // Cycles are needed before installation because of the async HTTP outcalls
    let canister_id = install_ii_canister_with_arg_and_cycles(
        env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );

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

fn openid_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6IjI1ZjgyMTE3MTM3ODhiNjE0NTQ3NGI1MDI5YjAxNDFiZDViM2RlOWMiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJMRWdxNmVsdk9LbExQUGlvR3AxX1NOTWktRF9mN3B4R0RwZFJVV3BCenFrIiwibmJmIjoxNzQxMDE2NDQ2LCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQxMDE2NzQ2LCJleHAiOjE3NDEwMjAzNDYsImp0aSI6IjQ4MzFmYzQ2YTFmMDUwZWViNDhjNjA1Y2YzNTFmYWEzYmVmNTQ4MDgifQ.UlGL2Cff4dp_bj1gmSjGRA9fv_iPfcAozEpM1pZXta--usoY-F8mAICASoHgJ1O75-pjD76SCra1H79EERCiKIWWiUlZK6rzEjOzSj5i2Z5sx688MRvhCBN5x6nZ1aQb_5Ussc4HevViEaewmfRHmueSEHHoWe9z4DdLvfBuWC1cp8qnOIaa0WRAEqsaovI61utPamch8v0SZLGGlkXINVEWmGbLyjMVZL6jDoa_x63AVVW70QDRfsLmDzUxjNHPlH_SwiNg4zhqKGPi-9nhn3lYqJ2jjrPLQrXkh8MbQA4w2QRTlq-7zBzAVL9xozkKJw3Ax9FihAwyh5gUnuzVFA";
    let salt: [u8; 32] = [
        249, 188, 133, 93, 44, 150, 215, 76, 177, 250, 1, 198, 222, 22, 154, 60, 25, 61, 27, 248,
        179, 222, 228, 29, 149, 137, 60, 255, 63, 164, 72, 76,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1741016746000;
    let test_principal = Principal::from_slice(&[
        9, 11, 202, 68, 108, 83, 230, 177, 241, 222, 137, 0, 183, 151, 140, 86, 164, 118, 33, 206,
        228, 17, 140, 70, 115, 102, 61, 177, 2,
    ]);

    // This is the public key of the credential used to sign in
    // You get it with connection.identity.credentialData[0].pubkey
    // Notably, you don't get it with connection.identity.getPublicKey()
    // Even though you get the above principal with connection.identity.getPrincipal()

    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 190, 143, 23, 40, 191, 192, 75, 197, 71, 10, 69, 135, 17, 136, 47, 43, 189,
        230, 180, 2, 223, 186, 52, 156, 210, 148, 246, 232, 15, 29, 102, 104, 34, 88, 32, 146, 168,
        238, 42, 248, 88, 236, 34, 126, 6, 124, 126, 58, 10, 174, 208, 198, 161, 32, 25, 190, 101,
        78, 97, 56, 58, 196, 217, 105, 116, 85, 185,
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
