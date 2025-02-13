//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use candid_parser::bindings::javascript::test;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use getrandom;
use identity_jose::jwk::Jwk;
use identity_jose::jws::{CompactJwsEncoder, Decoder, JwsHeader};
use internet_identity_interface::internet_identity::types::GetDelegationResponse;
use internet_identity_interface::internet_identity::types::InternetIdentityInit;
use internet_identity_interface::internet_identity::types::OpenIdConfig;
use internet_identity_interface::internet_identity::types::OpenIdPrepareDelegationResponse;
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
            client_id: "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com"
                .into(),
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

    let auth_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &auth_method);
    //   let identity_number = flows::register_anchor_with_device(&env, canister_id, auth_method);

    let (jwt, salt, _claims) = test_data();

    api::openid_credential_add(
        &env,
        canister_id,
        test_principal(),
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
            client_id: "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com"
                .into(),
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
    env.advance_time(Duration::from_secs(116_465_500));

    let pub_session_key = ByteBuf::from("session public key");
    let principal = test_principal();
    let (jwt, salt, _claims) = test_data();

    // Another tick for the asyncness
    env.tick();

    let prepare_response = match api::openid_prepare_delegation(
        &env,
        canister_id,
        principal,
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

fn salt() -> [u8; 32] {
    let mut salt = [0u8; 32];
    getrandom::getrandom(&mut salt).expect("Failed to generate random salt");
    salt
}

fn create_anonymous_nonce(principal: Principal) -> (String, [u8; 32]) {
    // Generate random salt
    let salt = salt();

    // Calculate SHA-256 hash
    let mut hasher = Sha256::new();
    hasher.update(salt);
    hasher.update(principal);
    let hash: [u8; 32] = hasher.finalize().into();
    let nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

    (nonce, salt)
}

fn mock_google_oidc_response(nonce: String, time: u64) -> String {
    let mut header = JwsHeader::new();
    header.set_kid("test-key-id");

    let time_in_seconds = time / 1_000_000_000;

    let claims = json!({
        "iss": "https://accounts.google.com",
        "sub": "123456789",
        "aud":  "https://example.com",
        "nonce": nonce,
        "iat": time_in_seconds,
        "email": "test@example.com",
    })
    .to_string();

    let encoder =
        CompactJwsEncoder::new(claims.as_bytes(), &header).expect("Failed to create encoder");
    let signing_input = encoder.signing_input().to_vec();
    encoder.into_jws(&signing_input)
}

fn test_data() -> (String, [u8; 32], Claims) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let salt: [u8; 32] = [
        143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81,
        139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();

    (jwt.into(), salt, claims)
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
            let mock_certs = serde_json::from_str::<Certs>(r#"{"keys":[{"n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use": "sig","kty": "RSA","alg": "RS256","kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847","e": "AQAB"}]}"#).unwrap().keys;

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

fn test_principal() -> Principal {
    Principal::from_text("x4gp4-hxabd-5jt4d-wc6uw-qk4qo-5am4u-mncv3-wz3rt-usgjp-od3c2-oae").unwrap()
}
