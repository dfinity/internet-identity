//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use candid_parser::bindings::javascript::test;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use getrandom;
use ic_stable_structures::Storable;
use identity_jose::jwk::Jwk;
use identity_jose::jws::{CompactJwsEncoder, Decoder, JwsHeader};
use internet_identity_interface::internet_identity::authn_method;
use internet_identity_interface::internet_identity::types::AuthnMethod;
use internet_identity_interface::internet_identity::types::AuthnMethodData;
use internet_identity_interface::internet_identity::types::AuthnMethodProtection;
use internet_identity_interface::internet_identity::types::AuthnMethodPurpose;
use internet_identity_interface::internet_identity::types::AuthnMethodSecuritySettings;
use internet_identity_interface::internet_identity::types::GetDelegationResponse;
use internet_identity_interface::internet_identity::types::InternetIdentityInit;
use internet_identity_interface::internet_identity::types::OpenIdConfig;
use internet_identity_interface::internet_identity::types::OpenIdPrepareDelegationResponse;
use internet_identity_interface::internet_identity::types::PublicKeyAuthn;
use pocket_ic::common::rest::CanisterHttpReply;
use pocket_ic::common::rest::CanisterHttpResponse;
use pocket_ic::common::rest::MockCanisterHttpResponse;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::PocketIc;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::time::Duration;

use crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method;
use crate::v2_api::authn_method_test_helpers::test_authn_method;

/// Verifies that Google Accounts can be added
/// // TODO: need to create new test data to use with this
#[test]
fn can_link_google_account() -> Result<(), CallError> {
    let env = env();

    let args = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: None,
        openid_google: Some(Some(OpenIdConfig {
            client_id: CLIENT_ID.to_string(),
        })),
    };
    // Cycles are needed before installation because of the async HTTP outcalls
    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );

    // Mock google certs response
    mock_google_certs_response(&env);

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

/// Verifies that valid JWT delegations are issued.
#[test]
fn can_get_valid_jwt_delegation() -> Result<(), CallError> {
    let env = env();

    let args = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: None,
        openid_google: Some(Some(OpenIdConfig {
            client_id: CLIENT_ID.to_string(),
        })),
    };
    // Cycles are needed before installation because of the async HTTP outcalls
    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );

    // Mock google certs response
    mock_google_certs_response(&env);

    // Create identity

    // Link Google Account to Identity

    // Advance to token validity period
    let (jwt, salt, _claims, test_time, _test_principal, test_authn_method) = openid_test_data();
    env.advance_time(Duration::from_secs(test_time / 10_000));

    let pub_session_key = ByteBuf::from("session public key");

    // Another tick for the asyncness
    env.tick();

    let prepare_response = match api::openid_prepare_delegation(
        &env,
        canister_id,
        test_authn_method.principal(),
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

    let signed_delegation = match api::openid_get_delegation(
        &env,
        canister_id,
        principal_1(),
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

#[derive(Deserialize)]
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

#[derive(Serialize, Deserialize)]
struct Certs {
    keys: Vec<Jwk>,
}

fn openid_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
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

static CLIENT_ID: &str = "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com";

/*
FIFTH TIME'S THE CHARM

nonce
fBG1Kr3QkygGGzSIXoOwjwD_yB8WKA_qRORVc0ZtXyI

salt
107, 14, 204, 55, 92, 39, 93, 230, 53, 20, 153, 234, 70, 25, 120, 74, 136, 94, 251, 187, 238, 96, 97, 180, 255, 135, 20, 149, 143, 27, 159, 83,

principal
211, 40, 186, 145, 43, 2, 6, 17, 232, 23, 22, 44, 51, 178, 233, 163, 131, 231, 82, 174, 66, 201, 203, 1, 102, 109, 20, 75, 2,

jwt
eyJhbGciOiJSUzI1NiIsImtpZCI6Ijc2M2Y3YzRjZDI2YTFlYjJiMWIzOWE4OGY0NDM0ZDFmNGQ5YTM2OGIiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJmQkcxS3IzUWt5Z0dHelNJWG9Pd2p3RF95QjhXS0FfcVJPUlZjMFp0WHlJIiwibmJmIjoxNzQwNTgzNDEyLCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQwNTgzNzEyLCJleHAiOjE3NDA1ODczMTIsImp0aSI6IjhjNjkzMWE4YmVmZjllOWM3OTRmYjM5ZTkwNTExOTM4MTk4MDgxZDYifQ.PVAbLj1Fv7AUwH16nFiedJkmPOUg1UkPnAkVj6S9MDhpEV467tP7iOxQCx64i0_imTymcjkzH9pcfTsaKpY8fWPrWSWZzDy9S4GygjOQeg13NXg_H23X2-IY_OVHKqtrAibhZZUppvczijqZja7-HmUivoAJIGsMOk1IxbJdalOhE5yQtsYEx4ZBxFemR7CTfMzopsAaRWgPHI7T0MENuiCbkSy_NYQPBzNpmGcKoZoyUbleFUzej8gbkqpoIUVdfwuNtoe_TMjED5eqJxi1Pip85iy4wJTa2RKUTZxUfqVCaTEftVt8U-PV1UgPsxpu0mKS5z5bXylmgclUzcNnmg

time
1740583715239

pubkey
165, 1, 2, 3, 38, 32, 1, 33, 88, 32, 252, 182, 240, 218, 160, 61, 178, 176, 17, 228, 185, 84, 148, 45, 86, 216, 171, 120, 72, 246, 212, 55, 212, 167, 142, 59, 227, 0, 242, 182, 129, 211, 34, 88, 32, 158, 197, 96, 131, 51, 156, 176, 65, 128, 29, 75, 98, 163, 187, 104, 38, 255, 65, 92, 234, 229, 245, 221, 74, 40, 202, 29, 83, 162, 84, 177, 204,

pubkey?
48,94,48,12,6,10,43,6,1,4,1,131,184,67,1,1,3,78,0,165,1,2,3,38,32,1,33,88,32,252,182,240,218,160,61,178,176,17,228,185,84,148,45,86,216,171,120,72,246,212,55,212,167,142,59,227,0,242,182,129,211,34,88,32,158,197,96,131,51,156,176,65,128,29,75,98,163,187,104,38,255,65,92,234,229,245,221,74,40,202,29,83,162,84,177,204
*/
