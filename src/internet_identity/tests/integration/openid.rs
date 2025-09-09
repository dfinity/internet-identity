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
    OpenIdCredentialAddError, OpenIdCredentialKey, OpenIdDelegationError, PublicKeyAuthn,
};
use pocket_ic::common::rest::{CanisterHttpReply, CanisterHttpResponse, MockCanisterHttpResponse};
use pocket_ic::{PocketIc, RejectResponse};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use std::time::Duration;

fn sync_time(env: &PocketIc, test_time: u64) {
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(env));
    env.advance_time(time_to_advance);
}

/// Verifies that Google Accounts can be added
#[test]
fn can_link_google_account() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    Ok(())
}

/// Verifies that Microsoft Accounts can be added
#[test]
fn can_link_microsoft_account() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        one_openid_microsoft_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    Ok(())
}

/// Verifies that the same Microsoft account cannot be linked to two different identities
#[test]
fn cannot_link_same_microsoft_account_to_two_identities() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        one_openid_microsoft_test_data();
    // This is the same Microsoft account as the one in `one_openid_microsoft_test_data`, but with a different principal.
    // This information is part of the hardcoded JWT.
    let (jwt2, salt2, _claims2, test_time2, test_principal2, test_authn_method2) =
        openid_microsoft_same_as_one_but_different_principal_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);
    let identity_number2 =
        create_identity_with_authn_method(&env, canister_id, &test_authn_method2);

    sync_time(&env, test_time);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );
    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal2, identity_number2)?,
        0
    );

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );
    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal2, identity_number2)?,
        0
    );

    sync_time(&env, test_time2);

    let result = api::openid_credential_add(
        &env,
        canister_id,
        test_principal2,
        identity_number2,
        &jwt2,
        &salt2,
    )?;

    assert_eq!(
        result,
        Err(OpenIdCredentialAddError::OpenIdCredentialAlreadyRegistered)
    );
    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );
    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal2, identity_number2)?,
        0
    );

    Ok(())
}

// Linking Microsoft accounts from different tenants to the same identity is not allowed in the frontend, but is allowed in the backend.
// This test verifies that the backend permits this behaviour.
#[test]
fn can_link_microsoft_account_from_different_tenant() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        one_openid_microsoft_test_data();
    // The tenant is part of `jwt`
    let (jwt2, salt2, _claims2, test_time2, _test_principal2, _test_authn_method2) =
        second_openid_microsoft_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    sync_time(&env, test_time2);

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt2,
        &salt2,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        2
    );

    Ok(())
}

/// Verifies that Google Accounts can be removed
#[test]
fn can_remove_google_account() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        0
    );

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

    assert_eq!(
        number_of_openid_credentials(&env, canister_id, test_principal, identity_number)?,
        1
    );

    let _ = api::openid_credential_remove(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &claims.key(),
    )?;

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
fn can_get_valid_jwt_delegation() -> Result<(), RejectResponse> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    // Create identity
    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

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
fn can_register_with_google() -> Result<(), RejectResponse> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, _claims, test_time, test_principal, _test_authn_method) =
        openid_google_test_data();

    sync_time(&env, test_time);

    // Create identity (this will panic if it doesn't work)
    // the test principal here is technically from webauthn, while in practice it would be a temporary random frontend keypair
    // however, this makes no functional difference. we just need a principal and salt together with a jwt
    // which contains a signed nonce derived from said principal and salt.

    let _identity_number =
        create_identity_with_openid_credential(&env, canister_id, &jwt, &salt, test_principal);

    Ok(())
}

#[test]
fn can_register_with_microsoft() -> Result<(), RejectResponse> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, _claims, test_time, test_principal, _test_authn_method) =
        one_openid_microsoft_test_data();

    sync_time(&env, test_time);

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

    let (_jwt, salt, _claims, test_time, test_principal, _test_authn_method) =
        openid_google_test_data();

    let faulty_jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6Ijc2M2Y3YzRjZDI2YTFlYjJiMWIzOWE4OGY0NDM0ZDFmNGQ5YTM2OGIiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiIzNjA1ODc5OTE2NjgtNjNicGMxZ25ncDFzNWdibzFhbGRhbDRhNTBjMWowYmIuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDcxNzAzNjg4OTgyMTkwMzU3MjEiLCJoZCI6ImRmaW5pdHkub3JnIiwiZW1haWwiOiJhbmRyaS5zY2hhdHpAZGZpbml0eS5vcmciLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibm9uY2UiOiJmQkcxS3IzUWt5Z0dHelNJWG9Pd2p3RF95QjhXS0FfcVJPUlZjMFp0WHlJIiwibmJmIjoxNzQwNTgzNDEyLCJuYW1lIjoiQW5kcmkgU2NoYXR6IiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0k1YUU0Mmo0Ml9JcEdqSHFjT2lUemVQLXRZaWNhMFZSLURnYklWcjJCWGtOSWxoUT1zOTYtYyIsImdpdmVuX25hbWUiOiJBbmRyaSIsImZhbWlseV9uYW1lIjoiU2NoYXR6IiwiaWF0IjoxNzQwNTgzNzEyLCJleHAiOjE3NDA1ODczMTIsImp0aSI6IjhjNjkzMWE4YmVmZjllOWM3OTRmYjM5ZTkwNTExOTM4MTk4MDgxZDYifQ.PVAbLj1Fv7AUwH16nFiedJkmPOUg1UkPnAkVj6S9MDhpEV467tP7iOxQCx64i0_imTymcjkzH9pcfTsaKpY8fWPrWSWZzDy9S4GygjOQeg13NXg_H23X2-IY_OVHKqtrAibhZZUppvczijqZja7-HmUivoAJIGsMOk1IxbJdalOhE5yQtsYEx4ZBxFemR7CTfMzopsAaRWgPHI7T0MENuiCbkSy_NYQPBzNpmGcKoZoyUbleFUzej8gbkqpoIUVdfwuNtoe_TMjED5eqJxi1Pip85iy4wJTa2RKUTZxUfqVCaTEftVt8U-PV1UgPsxpu0mKS5z5bXylmgclUzcNnmh";

    sync_time(&env, test_time);

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
fn cannot_get_valid_jwt_delegation_after_reassociation() -> Result<(), RejectResponse> {
    let env = env();

    let canister_id = setup_canister(&env);

    let (jwt, salt, claims, test_time, test_principal, test_authn_method_data) =
        openid_google_test_data();
    let (
        second_jwt,
        second_salt,
        _second_claims,
        second_test_time,
        second_test_principal,
        second_test_authn_method_data,
    ) = second_openid_google_test_data();

    // Create identity
    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &test_authn_method_data);

    // Link Google Account to Identity
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    let second_time_to_advance =
        Duration::from_millis(second_test_time) - Duration::from_millis(test_time);

    env.advance_time(time_to_advance);

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )?;

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

    let _ = api::openid_credential_remove(
        &env,
        canister_id,
        test_principal,
        identity_number,
        &claims.key(),
    )?;

    env.advance_time(second_time_to_advance);

    let second_identity_number =
        create_identity_with_authn_method(&env, canister_id, &second_test_authn_method_data);

    let _ = api::openid_credential_add(
        &env,
        canister_id,
        second_test_principal,
        second_identity_number,
        &second_jwt,
        &second_salt,
    )?;

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
        Ok(_) => panic!("Should not have been able to get delegation"),
        Err(_) => Ok(()),
    }
}

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
        openid_configs: Some(vec![OpenIdConfig {
            name: "Google".into(),
            logo: "<svg viewBox=\"0 0 24 24\"><path d=\"M12.19 2.83A9.15 9.15 0 0 0 4 16.11c1.5 3 4.6 5.06 8.18 5.06 2.47 0 4.55-.82 6.07-2.22a8.95 8.95 0 0 0 2.73-6.74c0-.65-.06-1.28-.17-1.88h-8.63v3.55h4.93a4.23 4.23 0 0 1-1.84 2.76c-3.03 2-7.12.55-8.22-2.9h-.01a5.5 5.5 0 0 1 5.14-7.26 5 5 0 0 1 3.5 1.37l2.63-2.63a8.8 8.8 0 0 0-6.13-2.39z\" style=\"fill: currentColor;\"></path></svg>".into(),
            issuer: "https://accounts.google.com".into(),
            client_id: "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com"
                .into(),
            jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".into(),
            auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".into(),
            auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
            fedcm_uri: Some("https://accounts.google.com/gsi/fedcm.json".into()),
        }, OpenIdConfig {
            name: "Microsoft".into(),
            logo: "<svg viewBox=\"0 0 24 24\"><path d=\"M2.5 2.5h9v9h-9zm10 0h9v9h-9zm-10 10h9v9h-9zm10 0h9v9h-9z\" style=\"fill: currentColor;\"></path></svg>".into(),
            issuer: "https://login.microsoftonline.com/{tid}/v2.0".into(),
            client_id: "d948c073-eebd-4ab8-861d-055f7ab49e17"
                .into(),
            jwks_uri: "https://login.microsoftonline.com/common/discovery/v2.0/keys".into(),
            auth_uri: "https://login.microsoftonline.com/common/oauth2/v2.0/authorize".into(),
            auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
            fedcm_uri: Some("".into()),
        }]),
        archive_config: Some(ArchiveConfig {
            module_hash: wasm_module_hash(&ARCHIVE_WASM),
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
    mock_microsoft_certs_response(env);

    canister_id
}

fn mock_google_certs_response(env: &PocketIc) {
    // This is the URL that the canister will fetch the Google certificates
    let url = "https://www.googleapis.com/oauth2/v3/certs";
    // These are the certificates at the time of the related Google JWT mocked data was created.
    let mock_certs = r#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"25f8211713788b6145474b5029b0141bd5b3de9c","n":"0qTcwnqUqJqsyu57JAC4IOAgTuMrccabAKKj5T93F68NoCk4kAax0oJhDArisYpiLrQ__YJJ9HFm3TKkuiPZeb1xqSSXAnIZVo8UigTLQDQLCTq3O-aD5EyQTOhOHWxJBZcpyLO-dZVuOIbv8fNMcXpNCioHVHO04gI_mvaw8ZzbU_j8ZeHSPk4wTBNfmH4l0mYRDhoQHLkZxxvc2V71ppBPYbnX-4t6h7XcuTkLJKBxfrR43G5nNzDuFsIbBnS2fjVLEv_1LYj9G5Q5XwiCFS0BON-oqQNzRWF53nkf91bMm2TaROg21KKJbZqfEjUhCVlMDFmBW-MNv69-C19PZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"5d12ab782cb6096285f69e48aea99079bb59cb86","n":"uac7NRcojCutcceWq1nrpLGJjQ7ywvgWsUcb1DWMKJ3KNNHiRzh9jshoi9tmq1zlarJ_h7GQg8iU1qD7SgpVYJmjlKG1MNVRAtuNrNMC0UAnNfG7mBBNorHFndfp-9cLTiMjXSXRzhNqiMvTVKeolRdMB2lH9RzJnwlpXtvUbD7M1pXOlPlMaOy1zxUnHn0uszU5mPRQk79i03BNrAdhwrAUB-ZuMnqpjaUcb9VU3KIwuZNPtsVenLN12sRYpaZ6WBw8Q9q7fAoaJUovM0Go8deC9pJYyxJuHdVo9HP0osyzg3g_rOYi14wmvMBuiDf3F4pTnudAfFyl3d0Mn_i4ZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b","n":"y8TPCPz2Fp0OhBxsxu6d_7erT9f9XJ7mx7ZJPkkeZRxhdnKtg327D4IGYsC4fLAfpkC8qN58sZGkwRTNs-i7yaoD5_8nupq1tPYvnt38ddVghG9vws-2MvxfPQ9m2uxBEdRHmels8prEYGCH6oFKcuWVsNOt4l_OPoJRl4uiuiwd6trZik2GqDD_M6bn21_w6AD_jmbzN4mh8Od4vkA1Z9lKb3Qesksxdog-LWHsljN8ieiz1NhbG7M-GsIlzu-typJfud3tSJ1QHb-E_dEfoZ1iYK7pMcojb5ylMkaCj5QySRdJESq9ngqVRDjF4nX8DK5RQUS7AkrpHiwqyW0Csw","e":"AQAB"}]}"#;
    mock_certs_response(env, url, mock_certs);
}

fn mock_microsoft_certs_response(env: &PocketIc) {
    // This is the URL that the canister will fetch the Microsoft certificates
    let url = "https://login.microsoftonline.com/common/discovery/v2.0/keys";
    // These are the certificates at the time of the related Microsoft JWT mocked data was created.
    let mock_certs = r#"{"keys":[{"kty":"RSA","use":"sig","kid":"PoVKeirIOvmTyLQ9G9BenBwos7k","x5t":"PoVKeirIOvmTyLQ9G9BenBwos7k","n":"ruYyUq1ElSb8QCCt0XWWRSFpUq0JkyfEvvlCa4fPDi0GZbSGgJg3qYa0co2RsBIYHczXkc71kHVpktySAgYK1KMK264e-s7Vymeq-ypHEDpRsaWric_kKEIvKZzRsyUBUWf0CUhtuUvAbDTuaFnQ4g5lfoa7u3vtsv1za5Gmn6DUPirrL_-xqijP9IsHGUKaTmB4M_qnAu6vUHCpXZnN0YTJDoK7XrVJFaKj8RrTdJB89GFJeTFHA2OX472ToyLdCDn5UatYwmht62nXGlH7_G1kW1YMpeSSwzpnMEzUUk7A8UXrvFTHXEpfXhsv0LA59dm9Hi1mIXaOe1w-icA_rQ","e":"AQAB","x5c":["MIIC/jCCAeagAwIBAgIJAM52mWWK+FEeMA0GCSqGSIb3DQEBCwUAMC0xKzApBgNVBAMTImFjY291bnRzLmFjY2Vzc2NvbnRyb2wud2luZG93cy5uZXQwHhcNMjUwMzIwMDAwNTAyWhcNMzAwMzIwMDAwNTAyWjAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAruYyUq1ElSb8QCCt0XWWRSFpUq0JkyfEvvlCa4fPDi0GZbSGgJg3qYa0co2RsBIYHczXkc71kHVpktySAgYK1KMK264e+s7Vymeq+ypHEDpRsaWric/kKEIvKZzRsyUBUWf0CUhtuUvAbDTuaFnQ4g5lfoa7u3vtsv1za5Gmn6DUPirrL/+xqijP9IsHGUKaTmB4M/qnAu6vUHCpXZnN0YTJDoK7XrVJFaKj8RrTdJB89GFJeTFHA2OX472ToyLdCDn5UatYwmht62nXGlH7/G1kW1YMpeSSwzpnMEzUUk7A8UXrvFTHXEpfXhsv0LA59dm9Hi1mIXaOe1w+icA/rQIDAQABoyEwHzAdBgNVHQ4EFgQUcZ2MLLOas+d9WbkFSnPdxag09YIwDQYJKoZIhvcNAQELBQADggEBABPXBmwv703IlW8Zc9Kj7W215+vyM5lrJjUubnl+s8vQVXvyN7bh5xP2hzEKWb+u5g/brSIKX/A7qP3m/z6C8R9GvP5WRtF2w1CAxYZ9TWTzTS1La78edME546QejjveC1gX9qcLbEwuLAbYpau2r3vlIqgyXo+8WLXA0neGIRa2JWTNy8FJo0wnUttGJz9LQE4L37nR3HWIxflmOVgbaeyeaj2VbzUE7MIHIkK1bqye2OiKU82w1QWLV/YCny0xdLipE1g2uNL8QVob8fTU2zowd2j54c1YTBDy/hTsxpXfCFutKwtELqWzYxKTqYfrRCc1h0V4DGLKzIjtggTC+CY="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"JYhAcTPMZ_LX6DBlOWQ7Hn0NeXE","x5t":"JYhAcTPMZ_LX6DBlOWQ7Hn0NeXE","n":"otxHCrbdDCUhPHT05zemCxen_h3vbWY9BxgH5yL4HPhAfj8xFrn-B5vgySJKUj4yprzwkT-EfcCfpMNDgxvVHyzMzWQWk_XcYCA72Whkt4kZnEzk-oxeasJ2rJ7NIpWMdQbamfw4BT8GYyplE1Pd2oiBGTeW4qxJow5qVmu_xwqpdq0MiViNQMQgLmy1FElDweue2Hsr9PUF-bp1dFPSp1m1kqvpMBsLZd4-2cc-k3gl12PB0O3GbswwFdoKf9iN0mJ_0GdRRQiXm-BXpnOPNiTcYrLiz2wWCpYQrLmxpJhm3oqLPGWrQtyVeO8vUeLI0fW4wpjYCQK_KPEi1x7T2Q","e":"AQAB","x5c":["MIIC/jCCAeagAwIBAgIJAM5KoNE1LHYtMA0GCSqGSIb3DQEBCwUAMC0xKzApBgNVBAMTImFjY291bnRzLmFjY2Vzc2NvbnRyb2wud2luZG93cy5uZXQwHhcNMjUwNzA3MTQzMjA3WhcNMzAwNzA3MTQzMjA3WjAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAotxHCrbdDCUhPHT05zemCxen/h3vbWY9BxgH5yL4HPhAfj8xFrn+B5vgySJKUj4yprzwkT+EfcCfpMNDgxvVHyzMzWQWk/XcYCA72Whkt4kZnEzk+oxeasJ2rJ7NIpWMdQbamfw4BT8GYyplE1Pd2oiBGTeW4qxJow5qVmu/xwqpdq0MiViNQMQgLmy1FElDweue2Hsr9PUF+bp1dFPSp1m1kqvpMBsLZd4+2cc+k3gl12PB0O3GbswwFdoKf9iN0mJ/0GdRRQiXm+BXpnOPNiTcYrLiz2wWCpYQrLmxpJhm3oqLPGWrQtyVeO8vUeLI0fW4wpjYCQK/KPEi1x7T2QIDAQABoyEwHzAdBgNVHQ4EFgQUbmZcQgdPC+xHVw/WrRHiajm/FNIwDQYJKoZIhvcNAQELBQADggEBACVhPf9gP/hr7lLx8OCaGaAVEEQ+DGqMuSzHJTSgcGXwKsVbsHO7Ih39ERM+ZoUZoawARETLw7c0UW0iiBZsdBnjVhzaEHha/9tIXOs9THAqwNY6hhN4tYY9MOB4U5gBL7SaQnwelHsY6oQSavK/lA25e/t0Uxz2BOEWYy6/59n3Tu8AMRySz6Q1YnVO6Ww5pNBTjHV+8kFbsj3ln5C4rAoTBJwHSYdh6Yz9OZB7UGhVCzkYQ7FO5xJ1WW5FPezVl4Osnr/JhjwvAidcq4dKO7OGHTIaDa72IcJ9IFsPdCxaCIspz4A1zEX6C+Z8bONccUs5xeAeGwaZypw9XJW341M="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"wf9rj3EkQvzKpxDG-mJn1gALi6U","x5t":"wf9rj3EkQvzKpxDG-mJn1gALi6U","n":"n_GLtQeV5wmNp7Zs0KC3SDOYaSEGLdo7MuMfNKQsrQ8tFaOysX-K4JmKjzL0qildP0xVKGJdLY70l1vLhqkxzlY0o0CssWzSMs5XCmNlxGUquaeN_F5f_zxvp_FgKH69ESnOxCAk6AbwTxww5pg1hAJGleyIN17PazVSBBCo4VSeWEYUL6S60N8i2xNb9Udxv1u8apEr9SKjrjR8oTrhfblySXMhKHFpvrHVlFWpDBojOKBZFs9IRFbsurm-LpNrw2ZAx2UJgYjf5_w-vLu3YfeYhWc65AW1hZeMs7Se33a2O6yelGe2wur2PRkpoE8_zoIB59CAO7bPEBSy27i4gQ","e":"AQAB","x5c":["MIIC6jCCAdKgAwIBAgIJAJEe9FiqEcETMA0GCSqGSIb3DQEBCwUAMCMxITAfBgNVBAMTGGxvZ2luLm1pY3Jvc29mdG9ubGluZS51czAeFw0yNTA4MDMxNjAxMzlaFw0zMDA4MDMxNjAxMzlaMCMxITAfBgNVBAMTGGxvZ2luLm1pY3Jvc29mdG9ubGluZS51czCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAJ/xi7UHlecJjae2bNCgt0gzmGkhBi3aOzLjHzSkLK0PLRWjsrF/iuCZio8y9KopXT9MVShiXS2O9Jdby4apMc5WNKNArLFs0jLOVwpjZcRlKrmnjfxeX/88b6fxYCh+vREpzsQgJOgG8E8cMOaYNYQCRpXsiDdez2s1UgQQqOFUnlhGFC+kutDfItsTW/VHcb9bvGqRK/Uio640fKE64X25cklzIShxab6x1ZRVqQwaIzigWRbPSERW7Lq5vi6Ta8NmQMdlCYGI3+f8Pry7t2H3mIVnOuQFtYWXjLO0nt92tjusnpRntsLq9j0ZKaBPP86CAefQgDu2zxAUstu4uIECAwEAAaMhMB8wHQYDVR0OBBYEFG/tpMvaA2NenbVzVfX1Ufs7sYE4MA0GCSqGSIb3DQEBCwUAA4IBAQBvLzLR9G8JaVfAybdsd0br+DPwMM3Zj33dG4/koqcSXH2PQuECRNMsap9ZDVq7Z63yeJhoVELI51LP67sZdoKm7L1P+mfpa1d4tCvMxaQRT9HlfzUzqr7xLbsOJa2+Nuzm5LpGZ0xVKP2ohMqSjZEZeMG7sVbMKB8qyJgT0VuXexRQyPqCeBrlbq6MomljX4t+tBM6FlZ0bLSX6qzM25XZc9xzL8tshNp1zB3FNksQmIRyRRom8rAjE4L/e6K1Sqrzs4zfQUorJCM/fhw9jh5ZHfdlnOvQrP2CU4NUC1UN1b9OqqVyLYtNc4e9M7y108RuKWs4xrxr5Rv+Jf/3lnwm"],"cloud_instance_name":"microsoftonline.us","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"dwd9Kc_5AQRdu0kNDRn5H5_ejaM","x5t":"dwd9Kc_5AQRdu0kNDRn5H5_ejaM","n":"iZWr4oIvi03jYIIv-ydKdHVZBAVCfv5_vjJDKyFEwWXCyymPYic1CSulPxofEBpbYXG5HKpOPJX9tk9iRt-ydw-co97lg02f1HJJdWqxU2o3vJBbFDvfG9ek1a30OUeYVmV0wyMK72khJL1iE7gUPztjnlDR8GyyzufYrEfufNG1tYRnKagEEEZ0mq-pnN9RVaZ7znXoI2H0bQy9GIj5KdwSRF3dT8YFEgolPgKE4mUV6RY0iLeOf3SRrfmAVokHp9VR91XJgxoit2ixqfRL7groKMLD5OxZtxEvxbMR2zMG0xcQtxBNjGAeHuGI7gCxWNFYNJB5IrCkNjOkmmx2iw","e":"AQAB","x5c":["MIIC6TCCAdGgAwIBAgIIWm3FLoFNqXQwDQYJKoZIhvcNAQELBQAwIzEhMB8GA1UEAxMYbG9naW4ubWljcm9zb2Z0b25saW5lLnVzMB4XDTI1MDgxOTE2MDExMloXDTMwMDgxOTE2MDExMlowIzEhMB8GA1UEAxMYbG9naW4ubWljcm9zb2Z0b25saW5lLnVzMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAiZWr4oIvi03jYIIv+ydKdHVZBAVCfv5/vjJDKyFEwWXCyymPYic1CSulPxofEBpbYXG5HKpOPJX9tk9iRt+ydw+co97lg02f1HJJdWqxU2o3vJBbFDvfG9ek1a30OUeYVmV0wyMK72khJL1iE7gUPztjnlDR8GyyzufYrEfufNG1tYRnKagEEEZ0mq+pnN9RVaZ7znXoI2H0bQy9GIj5KdwSRF3dT8YFEgolPgKE4mUV6RY0iLeOf3SRrfmAVokHp9VR91XJgxoit2ixqfRL7groKMLD5OxZtxEvxbMR2zMG0xcQtxBNjGAeHuGI7gCxWNFYNJB5IrCkNjOkmmx2iwIDAQABoyEwHzAdBgNVHQ4EFgQUDGVTWHZoF6G+adWAE3khY05HkKYwDQYJKoZIhvcNAQELBQADggEBADiPcDlVjZqXT8kX7MzT3rzYNM5bLwn25T9KQQvjY3tQp3Ref7skaDaUxR0VP51SFAKVED+vAexE6p3zBnl1Aur8gv1DIayjzp1ZwovRHRNykikindIqg7JXTz6a+jDOPcvpIWjM034X1kG92aQzo2L1FkE2hG7KEJybNWz9WJiC0IBMgFpMl6S0gn5gzYrWVwv4NP6PMPA/5IJdK0/XDlXTZuf2BnTKzy9c0obwC6PWM2m2WhsUW6AkCfQ/+10PUvvKwGOCNcP1Du9ejBEI0deer4/0grMPosz0+lIWEyj9O0NF15FPhfN9BO+6RM+9zLZUiY8pNIdkwwraYAPl0pU="],"cloud_instance_name":"microsoftonline.us","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"4pxhphrFjooBXV6t0JO_kpTQNl8","x5t":"4pxhphrFjooBXV6t0JO_kpTQNl8","n":"u2CavgKbig1Xt4gjxxQ3pb0iSaDw8_UoczFMicTtlKhRVjJcFclU5vZUuJYyIhE2CRZmrrza5k_-iaiebll1-mU7tmfN4iEg3FZoM2yS_ZiVJ2XpCB6ovz-jiPSYiy51az51mXztCBEAr6UPX6oOMppimgROt0dRps9SaOw9AFWfonj3BHLyFeON5bFu_JJ9stj0u-xQyP5s52ivNEjbGyYb3aX0NmwgEp7u38FT1zX4OK4hCX95-xNQwAjdyd0pTwqHR3hCfNqMGBfk4BlMb4kkk9oJk162qNt_o4jtQe5CGu7PDNh3L7FbSq4vFA5gZrSxPeLHwT8Y9ZGI9y3J5Q","e":"AQAB","x5c":["MIIDCjCCAfKgAwIBAgIQLIBnPFTz2C/4hjdwzi+JWzANBgkqhkiG9w0BAQsFADApMScwJQYDVQQDEx5MaXZlIElEIFNUUyBTaWduaW5nIFB1YmxpYyBLZXkwHhcNMjUwMzMwMTIwNDE0WhcNMzAwMzMwMTIwNDE0WjApMScwJQYDVQQDEx5MaXZlIElEIFNUUyBTaWduaW5nIFB1YmxpYyBLZXkwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC7YJq+ApuKDVe3iCPHFDelvSJJoPDz9ShzMUyJxO2UqFFWMlwVyVTm9lS4ljIiETYJFmauvNrmT/6JqJ5uWXX6ZTu2Z83iISDcVmgzbJL9mJUnZekIHqi/P6OI9JiLLnVrPnWZfO0IEQCvpQ9fqg4ymmKaBE63R1Gmz1Jo7D0AVZ+iePcEcvIV443lsW78kn2y2PS77FDI/mznaK80SNsbJhvdpfQ2bCASnu7fwVPXNfg4riEJf3n7E1DACN3J3SlPCodHeEJ82owYF+TgGUxviSST2gmTXrao23+jiO1B7kIa7s8M2HcvsVtKri8UDmBmtLE94sfBPxj1kYj3LcnlAgMBAAGjLjAsMB0GA1UdDgQWBBQSRzxsStOrWpoFyqOTStyfqK5uEzALBgNVHQ8EBAMCAsQwDQYJKoZIhvcNAQELBQADggEBALEzA/iWpeaVXX1f1FUghknhPk3/3dMJBlYyaq/cWCHT5XMDkCwc+KWjH98pNs0obOcXSsw0IaFzhVaRBm39JsG1IFhUISs1jHRbLc40cof5FAoX9q8L19CBZS3XSUC9NT2aFOzAGBCAHur9Lu/b6TcHVzpUKPL1tmM3wK9irOWCyOO5nVzetGqqgFaN06B5OQ/tVILvSAC3X0rgJ++oixuLjx9ReMSh5V2tW1lvOxlLWeIoYDltj54g+SIE1F9x6C9tXZORL4+mcHr9tQWQBUehPui4xosNbClW1FZtViQVD4CYVLW2i1P+ddotX7JZts+qU2VHUMaZgBlblM4PYQo="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"},{"kty":"RSA","use":"sig","kid":"dd55f7QP3HZky-ekQBDWKe7ADN0","x5t":"dd55f7QP3HZky-ekQBDWKe7ADN0","n":"s950MOniBjjiB82Zx5FehPuYJKtguUAP5vpMNZ6JxYLGzn5h3FPplx0kkPEezg7dnH-sFxGwq_C7P14ja0YmHUkmozqdbg4GRS8xp9Ue0-KWs4BTTkXvMPnc3xBU6be4SmHK-c7A0E54YfokiY1KTRCaq_RRPDlVZJuc6s6rK4nRoPo0eOCj7H4b--QnKmPXiFSPNp40NC7FRfx9m7SYSltubIDzq1f4j2Qlrv90f2kT5DP81_2DGdHZ_ao7ypPX2_z_m4ycQgfAhIYhNHRai6iGtbU6NPkw38LS29bVbGycVCaBoq6_re8k_UiOM7gP9l96QDFnkMmNhpRxT7t34w","e":"AQAB","x5c":["MIIDCzCCAfOgAwIBAgIRAMTs5DVuj/oHplagHKvwLhswDQYJKoZIhvcNAQELBQAwKTEnMCUGA1UEAxMeTGl2ZSBJRCBTVFMgU2lnbmluZyBQdWJsaWMgS2V5MB4XDTI1MDMxNzE1MzEwMloXDTMwMDMxNzE1MzEwMlowKTEnMCUGA1UEAxMeTGl2ZSBJRCBTVFMgU2lnbmluZyBQdWJsaWMgS2V5MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAs950MOniBjjiB82Zx5FehPuYJKtguUAP5vpMNZ6JxYLGzn5h3FPplx0kkPEezg7dnH+sFxGwq/C7P14ja0YmHUkmozqdbg4GRS8xp9Ue0+KWs4BTTkXvMPnc3xBU6be4SmHK+c7A0E54YfokiY1KTRCaq/RRPDlVZJuc6s6rK4nRoPo0eOCj7H4b++QnKmPXiFSPNp40NC7FRfx9m7SYSltubIDzq1f4j2Qlrv90f2kT5DP81/2DGdHZ/ao7ypPX2/z/m4ycQgfAhIYhNHRai6iGtbU6NPkw38LS29bVbGycVCaBoq6/re8k/UiOM7gP9l96QDFnkMmNhpRxT7t34wIDAQABoy4wLDAdBgNVHQ4EFgQUFPr+ao1bn0+7HqNjit54vxkyDnMwCwYDVR0PBAQDAgLEMA0GCSqGSIb3DQEBCwUAA4IBAQApk5hLeFI7bGJiIv6P+Ct74+mGdDYycRJAGbW/ihurk/NlDZWQ095ik3TGprTQ/4C+7ks8qtQOyVLhC2qR6ESZsBhgwWr7dxAHH2Q1DUBZN51N6xYo4adRWi6o0tNLZr7ZVEglV5e03Smu/rC9E04hocHpYiWKcZ9bpsVjK6Q1gcWcWnw1iAAZpGVD2xkpoFvssn0xiz0fCO0b6JT9l6YWeKJgBMz9LIFGoMaY1iREkCQryw6YGjdhEsSRUDvsDg5ICW4pJT9FOOthbsRm6H3reCa768AFImWaIESL3pqOXOppGDPL7dKY0Tt2uy0iQIRl0TkGHgrzoSa3q3Sl6VfT"],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"}]}"#;
    mock_certs_response(env, url, mock_certs);
}

fn mock_certs_response(env: &PocketIc, url: &str, mock_certs: &str) {
    const MAX_ATTEMPTS: u32 = 10;
    let mut attempts = 0;

    loop {
        env.tick();
        attempts += 1;

        let requests = env.get_canister_http();

        if let Some(cert_request) = requests.iter().find(|req| req.url == url) {
            // Use the same test certificate data that's used in google.rs
            let mock_certs = serde_json::from_str::<Certs>(mock_certs).unwrap().keys;

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
            panic!("No cert requests found for URL '{url}' after {MAX_ATTEMPTS} attempts");
        }
    }
}

/**
 * Explanation of the fields used for the test data:
 * - JWT: the JWT received by the openID provider.
 * - salt: the salt used to create the JWT from before.
 * - test_time: the `iat` (issued at) field from the JWT
 * - test_principal: the principal of the identity used the link the OpenID account.
 * - test_pubkey: the public key of the credential used to sign in with the identity from `test_principal`.
 *   Not the public key of the principal of the identity. You don't get it with connection.identity.getPublicKey().
 *
 * How to get the test data:
 * 1. Setup a local environment with open id providers.
 * 2. Create an identity with Passkey in the local environment.
 * 3. Log in with that identity and console.log the public key from `DiscoverablePasskeyIdentity.useExisting` `getPublicKey` argument.
 *    console.log("in da lookup", lookupResult.pubkey);
 *    This is the `test_pubkey`.
 * 4. Link an OpenID account to that identity.
 *    Add a few logs:
 *    - the identity's principal with `identity.getPrincipal().toUint8Array()`. This goes to `test_principal`.
 *    - the jwt after requesting it. This goes to `jwt`.
 *    - the salt from the authenticatedStore. This goes to `salt`.
 *    - the rest of the fields in the JWT claims, find the `iat`. This goes to `test_time`.
 *    - For example, you can find the JWT, salt and principal in `linkOpenIdAccount` from `addAccessMethodFlow`.
 *    - The claims you can log them in `decodeJWT`.
 *
 * Additional notes:
 * - The openID configuration when installing the canister in the test environment must match your local environment.
 * - If you add a new openID providers, you need to mock the credentials with `mock_certs_response`.
 * - We need to set the time in the pocket-ic environment becuase the JWT are already expired at the time of the test.
 * - These JWT can still be used to register an identity.
 */
pub fn openid_google_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
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

fn second_openid_google_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
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

fn one_openid_microsoft_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
    let jwt = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IkpZaEFjVFBNWl9MWDZEQmxPV1E3SG4wTmVYRSJ9.eyJhdWQiOiJkOTQ4YzA3My1lZWJkLTRhYjgtODYxZC0wNTVmN2FiNDllMTciLCJpc3MiOiJodHRwczovL2xvZ2luLm1pY3Jvc29mdG9ubGluZS5jb20vNGE0MzVjNWUtNjQ1MS00YzFhLWE4MWYtYWI5NjY2YjZkZThmL3YyLjAiLCJpYXQiOjE3NTY4MDgzMjQsIm5iZiI6MTc1NjgwODMyNCwiZXhwIjoxNzU2ODEyMjI0LCJhaW8iOiJBVlFBcS84WkFBQUExQjhrYVdEVWp6V0xnSUxVT2hIQ1pWVndhbk1wNGVnVzdURzZwTytnVSsyYzdKRVRJckV5VHlySkxQQ0h1VkZINkUrbzRlMzhCQjZ6dmlWSU9kTzkxNVRHVDhEaUR3bkhCazYxTSt2bTdJaz0iLCJjX2hhc2giOiJGUzJsWllUYTIwcWozZVl1enczUXBnIiwibmFtZSI6Ikxsb3JlbsOnIE11bnRhbmVyIFBlcmVsbG8iLCJub25jZSI6ImN1UmM4VlNEN1ZkQU9ISmpsX1UxbkNWdlpvamQtMGJoUE81X0lGbTc0N2MiLCJvaWQiOiIxYjI2NDVmNy04YjdmLTQyMTAtYjQxYy01MDM1MmQ1OWYyZTgiLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJsbG9yZW5jLm11bnRhbmVyQGRmaW5pdHkub3JnIiwicmgiOiIxLkFTNEFYbHhEU2xGa0dreW9INnVXWnJiZWozUEFTTm05N3JoS2hoMEZYM3EwbmhlNUFPd3VBQS4iLCJzaWQiOiIwMDdkZWE3OS0yNjY5LWZjNTItMzQwOS01Y2NjZDkxOTAzMjEiLCJzdWIiOiJydkF0eGluNk1TblRsN1RnUlg4RlhYQ0tQbEVlTklmUHI0bHdQT1lfd293IiwidGlkIjoiNGE0MzVjNWUtNjQ1MS00YzFhLWE4MWYtYWI5NjY2YjZkZThmIiwidXRpIjoiU3pLR0k3cG44MC1ZdnRmMmxuZ0RBUSIsInZlciI6IjIuMCJ9.kS8C8IlRoMaYoFyru-D06WzdeS8mHA3LupXyrOqXwwb4AIMMUDETlJEznAQ6iZxK4iAhAPAqAnC9TS_j0sacRCTBA3Rks-tkuwV2sA3XdwDsoFOnJdBs-N5GEXJNv45TzQ0jQANnXBJwwgH9hS-ledFZiutvzaTfDGpAymxx58qj7VDG5fTMxpiPMNCr42sNidw7B8ifUJgcfcxt_8wsTN_mui4Q6wtWRQvPnbesyTvRaOg2S6LMG3m8RBNYtHvXlICwD1kaKS5wUiYcrN3gg6wqOXCI3w57S5yfnGNo1tF4sWCfR0ZkfyHfVzdXK_6BwCty7rt4udp-NFsCAVXNRQ";
    let salt: [u8; 32] = [
        196, 116, 153, 227, 8, 104, 231, 67, 202, 28, 156, 132, 101, 84, 170, 111, 86, 233, 29, 54,
        230, 234, 243, 167, 159, 27, 102, 53, 166, 149, 172, 207,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1756808324000;
    let test_principal = Principal::from_slice(&[
        33, 56, 228, 195, 129, 228, 78, 174, 18, 66, 159, 91, 0, 114, 146, 13, 69, 50, 30, 206, 73,
        70, 162, 63, 23, 149, 200, 139, 2,
    ]);
    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 114, 42, 126, 192, 250, 94, 195, 79, 142, 211, 6, 212, 9, 135, 147, 58, 253,
        65, 125, 244, 95, 13, 249, 210, 209, 90, 66, 232, 237, 16, 43, 67, 34, 88, 32, 202, 212,
        22, 86, 222, 64, 75, 9, 157, 166, 125, 253, 46, 167, 174, 115, 181, 178, 11, 188, 189, 144,
        205, 63, 23, 227, 218, 35, 14, 101, 7, 235,
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

/**
 * This is the same Microsoft account as the one in `one_openid_microsoft_test_data`, but with a different principal.
 * This information is part of the hardcoded JWT.
 */
fn openid_microsoft_same_as_one_but_different_principal_test_data(
) -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
    let jwt = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IkpZaEFjVFBNWl9MWDZEQmxPV1E3SG4wTmVYRSJ9.eyJhdWQiOiJkOTQ4YzA3My1lZWJkLTRhYjgtODYxZC0wNTVmN2FiNDllMTciLCJpc3MiOiJodHRwczovL2xvZ2luLm1pY3Jvc29mdG9ubGluZS5jb20vNGE0MzVjNWUtNjQ1MS00YzFhLWE4MWYtYWI5NjY2YjZkZThmL3YyLjAiLCJpYXQiOjE3NTY4MDk4OTcsIm5iZiI6MTc1NjgwOTg5NywiZXhwIjoxNzU2ODEzNzk3LCJhaW8iOiJBVlFBcS84WkFBQUEwSnViSXg1RGp6MkdhZ2tHVVlCaENaMnZuL0lQUEhMTzlFLzZOak1HNFZnZ1MyZ1Fjb21adGhwSTlYcTE0Z3VDVzl4NGhtOG9ZSWNlbnIrNys4SUxxeEU3SlFYYklJSFR2ekt0ZjAvaXd6ND0iLCJjX2hhc2giOiJzUzRxbFJHM0dTcGl1R2d6dnp2N3lRIiwibmFtZSI6Ikxsb3JlbsOnIE11bnRhbmVyIFBlcmVsbG8iLCJub25jZSI6IjRRc3QzVTNBeEl5OUx1ajQtck9UczhqbnlxbWVIYUxuVjc5UHdiZkQ2c0UiLCJvaWQiOiIxYjI2NDVmNy04YjdmLTQyMTAtYjQxYy01MDM1MmQ1OWYyZTgiLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJsbG9yZW5jLm11bnRhbmVyQGRmaW5pdHkub3JnIiwicmgiOiIxLkFTNEFYbHhEU2xGa0dreW9INnVXWnJiZWozUEFTTm05N3JoS2hoMEZYM3EwbmhlNUFPd3VBQS4iLCJzaWQiOiIwMDdkZWE3OS0yNjY5LWZjNTItMzQwOS01Y2NjZDkxOTAzMjEiLCJzdWIiOiJydkF0eGluNk1TblRsN1RnUlg4RlhYQ0tQbEVlTklmUHI0bHdQT1lfd293IiwidGlkIjoiNGE0MzVjNWUtNjQ1MS00YzFhLWE4MWYtYWI5NjY2YjZkZThmIiwidXRpIjoidmdNd1BCVzB3MGVrUjhnbVF2d0FBQSIsInZlciI6IjIuMCJ9.dOLgbEsEkh-2unXLBWCjr9OPV27I3hqF4zuE9PgaGAnNfPqMa2dNjlDbIp74buUO8O9BxVQdzWZ4yP36GohbLstl6hS5uxG10Z4VMwsv6L9qXxLfS_4wjKEi3fu1z_4fdbTCYClJryQ2COQnSzIShudQtR6Sw12snQylv8AWs0sreBbi2TVZwrgewQ_6HC3RaQfNoO1MBKXLR8P3V-7mLrplDNu3nWUHFBfgK8iI58usgHO3NFpdYd2FvSjX8ShMdU35PogNz520T8cQEVsjc_IUiEWFgBjXOzuTw18rEHDf5_0DfMRnJDZ6u7qn5OBtjDY7bgZ1pxtJas-u1TT43A";
    let salt: [u8; 32] = [
        248, 17, 147, 158, 173, 176, 67, 222, 21, 206, 90, 244, 23, 215, 200, 214, 219, 39, 213,
        124, 225, 127, 112, 189, 122, 46, 84, 28, 4, 177, 98, 233,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1756809897000;
    let test_principal = Principal::from_slice(&[
        207, 89, 197, 37, 100, 13, 121, 8, 153, 196, 203, 90, 42, 72, 233, 220, 119, 173, 118, 203,
        235, 245, 229, 42, 249, 96, 210, 28, 2,
    ]);
    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 8, 146, 104, 45, 59, 242, 233, 149, 153, 10, 83, 252, 72, 236, 114, 32, 116,
        99, 16, 86, 47, 224, 150, 170, 9, 191, 42, 181, 81, 125, 157, 194, 34, 88, 32, 64, 124, 12,
        58, 148, 180, 243, 137, 40, 0, 10, 151, 172, 157, 34, 32, 129, 114, 68, 156, 126, 187, 174,
        224, 55, 171, 240, 28, 242, 24, 183, 78,
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

fn second_openid_microsoft_test_data() -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData)
{
    let jwt = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjRweGhwaHJGam9vQlhWNnQwSk9fa3BUUU5sOCJ9.eyJ2ZXIiOiIyLjAiLCJpc3MiOiJodHRwczovL2xvZ2luLm1pY3Jvc29mdG9ubGluZS5jb20vOTE4ODA0MGQtNmM2Ny00YzViLWIxMTItMzZhMzA0YjY2ZGFkL3YyLjAiLCJzdWIiOiJBQUFBQUFBQUFBQUFBQUFBQUFBQUFLQmNkbVNjQmM1WlJMVldfTnZCbm5VIiwiYXVkIjoiZDk0OGMwNzMtZWViZC00YWI4LTg2MWQtMDU1ZjdhYjQ5ZTE3IiwiZXhwIjoxNzU2ODk1MzE2LCJpYXQiOjE3NTY4MDg2MTYsIm5iZiI6MTc1NjgwODYxNiwibmFtZSI6Ikxsb3JlbsOnIE11bnRhbmVyIiwicHJlZmVycmVkX3VzZXJuYW1lIjoibGxvcmVuYy5tdW50YW5lckBnbWFpbC5jb20iLCJvaWQiOiIwMDAwMDAwMC0wMDAwLTAwMDAtYzJlYS05YzU2NDhjNjQ5NDMiLCJlbWFpbCI6Imxsb3JlbmMubXVudGFuZXJAZ21haWwuY29tIiwidGlkIjoiOTE4ODA0MGQtNmM2Ny00YzViLWIxMTItMzZhMzA0YjY2ZGFkIiwiY19oYXNoIjoiYjBIODJIZFNRcFZWY2RRU0QyLUFnUSIsIm5vbmNlIjoiSjUtTXQ3WlNhTWFwQlNzRkdJVzJWY25pRTAxa3ZtYjl1cUlJTzdUNWlWbyIsImFpbyI6IkRoOUtQWURNVTNpWU56SmhDQW01R0E1V3FkSk0yck9VZlhTbDJMV25qN3ltWEZGMWZ6OFpVMnJOeW56NWhWZEVmOHpabzhqNFAzKjE3RURvZndGQXNLNnFub1ZnUGRJdklCbGw2M2NpQmhHOWpQZGohQlcycVRtbko0cUtGdjBEckdGR1QyUFkzbkg5ZEdLN0doWXBWbVMwQXY0RjhGcW1iQjNSakRwWm1BR3gifQ.bkhQEC4oTvewk2k2oasFoPuTKC0i7QRUU13fugnDiXEGwWJ4oJz2gzrrVNcenBQ2-GH4WSWul-iwAmOCq9keFtLDb3Y5_7SApFoRqO0QLzV50Kl1wryLg7dVHrZfoJ0Juj29mdlej0nwUYlkxSn2qRoHFQappmpWBZOGCiohJRx7rb9Q_FcLMWelPL8FBSArHQhznOfJxQAxouEpK5tZVZHgSZjnfX8lxg2LF0cgw6mwy3t6eJQ4cA-Rp4-G-3YndkDmaNtoac1arYMTMggXQxZseU__RSkrpxRae8EkIIGhyDKqwiw46RZLAQnpHV0CkjolIvcUqaNpXtSscwZADg";
    let salt: [u8; 32] = [
        130, 72, 159, 133, 3, 151, 246, 106, 96, 151, 157, 243, 233, 14, 234, 0, 220, 62, 210, 94,
        76, 220, 218, 255, 97, 101, 136, 232, 156, 181, 30, 210,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let test_time = 1756808616000;
    // Same as `openid_microsoft_test_data`.
    let test_principal = Principal::from_slice(&[
        33, 56, 228, 195, 129, 228, 78, 174, 18, 66, 159, 91, 0, 114, 146, 13, 69, 50, 30, 206, 73,
        70, 162, 63, 23, 149, 200, 139, 2,
    ]);
    // Same as `openid_microsoft_test_data`.
    let test_pubkey = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 114, 42, 126, 192, 250, 94, 195, 79, 142, 211, 6, 212, 9, 135, 147, 58, 253,
        65, 125, 244, 95, 13, 249, 210, 209, 90, 66, 232, 237, 16, 43, 67, 34, 88, 32, 202, 212,
        22, 86, 222, 64, 75, 9, 157, 166, 125, 253, 46, 167, 174, 115, 181, 178, 11, 188, 189, 144,
        205, 63, 23, 227, 218, 35, 14, 101, 7, 235,
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
) -> Result<usize, RejectResponse> {
    let openid_credentials = api::get_anchor_info(env, canister_id, sender, identity_number)?
        .openid_credentials
        .expect("Could not fetch credentials!");

    Ok(openid_credentials.len())
}
