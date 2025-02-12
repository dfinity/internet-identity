//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use getrandom;
use identity_jose::jws::Decoder;
use identity_jose::jws::{CompactJwsEncoder, JwsHeader};
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
use serde::Deserialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::time::Duration;
use std::time::SystemTime;

/// Verifies that valid JWT delegations are issued.
#[test]
fn should_get_valid_jwt_delegation() -> Result<(), CallError> {
    let env = env();

    let args = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: None,
        openid_google: Some(Some(OpenIdConfig {
            client_id: "https://example.com".into(),
        })),
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(args));

    // Mock google certs response
    mock_google_certs_response(&env);

    let pub_session_key = ByteBuf::from("session public key");
    let principal = principal_1();
    let (nonce, salt) = create_anonymous_nonce(principal);
    let jwt = mock_google_oidc_response(nonce, time(&env));

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

fn mock_google_certs_response(env: &PocketIc) {
    const MAX_ATTEMPTS: u32 = 1000;
    let mut attempts = 0;

    loop {
        // Wait for HTTP outcall
        env.tick();
        attempts += 1;

        // Get pending requests
        let requests = env.get_canister_http();

        // Try to find Google certs request
        if let Some(cert_request) = requests
            .iter()
            .find(|req| req.url == "https://www.googleapis.com/oauth2/v3/certs")
        {
            // Create a mock certificate response
            let mock_certs = json!({
                "keys": [{
                    "kty": "RSA",
                    "kid": "test-key-id",  // Must match the kid in mock_google_oidc_response
                    "n": "xyzzy",  // Base64URL-encoded modulus
                    "e": "AQAB",   // Base64URL-encoded exponent (65537)
                    "alg": "RS256",
                    "use": "sig"
                }]
            });

            // Create the mock response
            let http_response = CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                status: 200,
                headers: vec![],
                body: mock_certs.to_string().into_bytes(),
            });

            let response = MockCanisterHttpResponse {
                subnet_id: cert_request.subnet_id,
                request_id: cert_request.request_id,
                response: http_response,
                additional_responses: vec![],
            };

            // Send the mock response
            env.mock_canister_http_response(response);
            env.tick();
            return;
        }

        if attempts >= MAX_ATTEMPTS {
            panic!("No Google cert requests found after {MAX_ATTEMPTS} attempts");
        }
    }
}
