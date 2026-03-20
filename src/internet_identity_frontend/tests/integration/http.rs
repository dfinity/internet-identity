//! Tests for the HTTP interactions of the II frontend canister.
//! These tests verify asset serving, certification, and well-known endpoints.

use candid::Principal;
use canister_tests::api::http_request;
use canister_tests::framework::*;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_response_verification::types::VerificationInfo;
use ic_response_verification::verify_request_response_pair;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::InternetIdentityFrontendArgs;
use pocket_ic::{PocketIc, RejectResponse};
use serde_bytes::ByteBuf;
use serde_json::json;
use std::time::Duration;

fn default_frontend_args() -> InternetIdentityFrontendArgs {
    InternetIdentityFrontendArgs {
        backend_canister_id: Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap(),
        backend_origin: "https://rdmx6-jaaaa-aaaaa-aaadq-cai.icp0.io".to_string(),
        related_origins: None,
        fetch_root_key: None,
        analytics_config: None,
        dummy_auth: None,
        dev_csp: None,
    }
}

/// Verifies that some expected assets are delivered, certified and have security headers.
#[test]
fn frontend_canister_serves_http_assets() -> Result<(), RejectResponse> {
    let assets: Vec<(&str, Option<&str>)> = vec![("/", None), ("/.well-known/ic-domains", None)];
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    for (asset, encoding) in assets {
        for certification_version in 1..=2 {
            let request = HttpRequest {
                method: "GET".to_string(),
                url: asset.to_string(),
                headers: vec![],
                body: ByteBuf::new(),
                certificate_version: Some(certification_version),
            };
            let http_response = http_request(&env, canister_id, &request)?;

            assert_eq!(http_response.status_code, 200);

            if let Some(enc) = encoding {
                let (_, content_encoding) = http_response
                    .headers
                    .iter()
                    .find(|(name, _)| name.to_lowercase() == "content-encoding")
                    .expect("Content-Encoding header not found");
                assert_eq!(
                    content_encoding, enc,
                    "unexpected Content-Encoding header value"
                );
            }
            verify_security_headers(&http_response.headers, &None);

            let result = verify_response_certification(
                &env,
                canister_id,
                request,
                http_response,
                certification_version,
            );
            assert_eq!(result.verification_version, certification_version);
        }
    }
    Ok(())
}

/// Verifies that `.well-known/webauthn` assets are delivered, certified and have security headers if present in the config.
#[test]
fn frontend_canister_serves_webauthn_assets() -> Result<(), RejectResponse> {
    let env = env();
    let related_origins: Vec<String> = vec![
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ];
    let args = InternetIdentityFrontendArgs {
        related_origins: Some(related_origins.clone()),
        ..default_frontend_args()
    };
    let canister_id = install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), args);

    for certification_version in 1..=2 {
        let request = HttpRequest {
            method: "GET".to_string(),
            url: "/.well-known/webauthn".to_string(),
            headers: vec![],
            body: ByteBuf::new(),
            certificate_version: Some(certification_version),
        };
        let http_response = http_request(&env, canister_id, &request)?;
        let response_body = String::from_utf8_lossy(&http_response.body).to_string();

        assert_eq!(http_response.status_code, 200);

        let expected_content = json!({
            "origins": related_origins.clone(),
        })
        .to_string();
        assert_eq!(response_body, expected_content);

        let (_, content_type) = http_response
            .headers
            .iter()
            .find(|(name, _)| name.to_lowercase() == "content-type")
            .expect("Content-Type header not found");
        assert_eq!(
            content_type, "application/json",
            "unexpected Content-Type header value"
        );
        verify_security_headers(&http_response.headers, &Some(related_origins.clone()));

        let result = verify_response_certification(
            &env,
            canister_id,
            request,
            http_response,
            certification_version,
        );
        assert_eq!(result.verification_version, certification_version);
    }
    Ok(())
}

/// Verifies that clients that do not indicate any certification version will get a v1 certificate.
#[test]
fn frontend_should_fallback_to_v1_certification() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 1;
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let request = HttpRequest {
        method: "GET".to_string(),
        url: "/".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: None,
    };
    let http_response = http_request(&env, canister_id, &request)?;

    assert_eq!(http_response.status_code, 200);

    let result = verify_response_certification(
        &env,
        canister_id,
        request,
        http_response,
        CERTIFICATION_VERSION,
    );
    assert_eq!(result.verification_version, CERTIFICATION_VERSION);

    Ok(())
}

#[test]
fn frontend_must_not_cache_well_known_ic_domains() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let well_known_request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/ic-domains".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let well_known_response = http_request(&env, canister_id, &well_known_request)?;

    assert_eq!(well_known_response.status_code, 200);
    // Make sure cache-control is set to no-cache
    let cache_control = well_known_response
        .headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "cache-control")
        .map(|(_, value)| value.clone());
    assert!(
        cache_control
            .is_none_or(|v| v.contains("no-cache")),
        "expected no-cache or no Cache-Control header, got: {:?}",
        cache_control,
    );

    let result = verify_response_certification(
        &env,
        canister_id,
        well_known_request,
        well_known_response,
        CERTIFICATION_VERSION,
    );
    assert_eq!(result.verification_version, CERTIFICATION_VERSION);

    Ok(())
}

#[test]
fn frontend_must_not_cache_well_known_webauthn() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let related_origins: Vec<String> = vec![
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ];
    let args = InternetIdentityFrontendArgs {
        related_origins: Some(related_origins.clone()),
        ..default_frontend_args()
    };
    let canister_id = install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), args);

    let well_known_request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/webauthn".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let well_known_response = http_request(&env, canister_id, &well_known_request)?;

    assert_eq!(well_known_response.status_code, 200);
    // Make sure cache-control is set to no-cache
    let cache_control = well_known_response
        .headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "cache-control")
        .map(|(_, value)| value.clone());
    assert!(
        cache_control
            .is_none_or(|v| v.contains("no-cache")),
        "expected no-cache or no Cache-Control header, got: {:?}",
        cache_control,
    );

    let result = verify_response_certification(
        &env,
        canister_id,
        well_known_request,
        well_known_response,
        CERTIFICATION_VERSION,
    );
    assert_eq!(result.verification_version, CERTIFICATION_VERSION);

    Ok(())
}

fn verify_response_certification(
    env: &PocketIc,
    canister_id: CanisterId,
    request: HttpRequest,
    http_response: HttpResponse,
    min_certification_version: u16,
) -> VerificationInfo {
    verify_request_response_pair(
        request.try_into().expect("Cannot represent HttpRequest"),
        http_response
            .try_into()
            .expect("Cannot represent HttpResponse"),
        canister_id.as_slice(),
        time(env) as u128,
        Duration::from_secs(300).as_nanos(),
        &env.root_key().unwrap(),
        min_certification_version as u8,
    )
    .unwrap_or_else(|e| panic!("validation failed: {e}"))
}
