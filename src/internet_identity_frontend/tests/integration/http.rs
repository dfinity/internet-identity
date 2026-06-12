//! Tests for the HTTP interactions of the II frontend canister.
//! These tests verify asset serving, certification, and well-known endpoints.

use candid::Principal;
use canister_tests::api::http_request;
use canister_tests::framework::*;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_response_verification::types::VerificationInfo;
use ic_response_verification::verify_request_response_pair;
use internet_identity_interface::http_gateway::HeaderField;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::InternetIdentityFrontendArgs;
use pocket_ic::{PocketIc, RejectResponse};
use serde_bytes::ByteBuf;
use serde_json::json;
use std::time::Duration;

/// Verify that expected security headers are present in the response.
/// This is specific to the frontend canister, which sets its own Permissions-Policy
/// that differs from the backend canister's.
fn verify_frontend_security_headers(headers: &[HeaderField]) {
    let header_map: std::collections::HashMap<String, &str> = headers
        .iter()
        .map(|(k, v)| (k.to_lowercase(), v.as_str()))
        .collect();

    assert_eq!(
        header_map.get("x-frame-options"),
        Some(&"DENY"),
        "X-Frame-Options header missing or incorrect"
    );
    assert_eq!(
        header_map.get("x-content-type-options"),
        Some(&"nosniff"),
        "X-Content-Type-Options header missing or incorrect"
    );
    assert!(
        header_map.contains_key("content-security-policy"),
        "Content-Security-Policy header missing"
    );
    assert!(
        header_map.contains_key("strict-transport-security"),
        "Strict-Transport-Security header missing"
    );
    assert!(
        header_map.contains_key("referrer-policy"),
        "Referrer-Policy header missing"
    );
    assert!(
        header_map.contains_key("permissions-policy"),
        "Permissions-Policy header missing"
    );
}

fn default_frontend_args() -> InternetIdentityFrontendArgs {
    InternetIdentityFrontendArgs {
        backend_canister_id: Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap(),
        backend_origin: "https://rdmx6-jaaaa-aaaaa-aaadq-cai.icp0.io".to_string(),
        related_origins: None,
        fetch_root_key: None,
        analytics_config: None,
        dummy_auth: None,
        dev_csp: None,
        featured_dashboard_apps: None,
        feature_flags: None,
    }
}

/// Verifies that some expected assets are delivered, certified and have security headers.
/// The frontend canister's AssetRouter only supports v2 certification.
#[test]
fn frontend_canister_serves_http_assets() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 2;
    let assets: Vec<(&str, Option<&str>)> = vec![
        ("/", None),
        ("/.well-known/ic-domains", None),
        ("/.well-known/ic-smtp-canister-id", None),
    ];
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    for (asset, encoding) in assets {
        let request = HttpRequest {
            method: "GET".to_string(),
            url: asset.to_string(),
            headers: vec![],
            body: ByteBuf::new(),
            certificate_version: Some(CERTIFICATION_VERSION),
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
        verify_frontend_security_headers(&http_response.headers);

        let result = verify_response_certification(
            &env,
            canister_id,
            request,
            http_response,
            CERTIFICATION_VERSION,
        );
        assert_eq!(result.verification_version, CERTIFICATION_VERSION);
    }
    Ok(())
}

/// Verifies that `.well-known/webauthn` assets are delivered, certified and have security headers if present in the config.
#[test]
fn frontend_canister_serves_webauthn_assets() -> Result<(), RejectResponse> {
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

    let request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/webauthn".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
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
    verify_frontend_security_headers(&http_response.headers);

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

/// Verifies that `.well-known/ic-smtp-canister-id` serves the configured backend canister principal.
#[test]
fn frontend_canister_serves_ic_smtp_canister_id() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let args = default_frontend_args();
    let expected_principal = args.backend_canister_id.to_text();
    let canister_id = install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), args);

    let request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/ic-smtp-canister-id".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let http_response = http_request(&env, canister_id, &request)?;

    assert_eq!(http_response.status_code, 200);
    assert_eq!(
        String::from_utf8_lossy(&http_response.body),
        expected_principal,
    );

    let (_, content_type) = http_response
        .headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "content-type")
        .expect("Content-Type header not found");
    assert_eq!(
        content_type, "text/plain",
        "unexpected Content-Type header value"
    );
    verify_frontend_security_headers(&http_response.headers);

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

/// Verifies that clients that do not indicate any certification version will still get a valid
/// (v2) certificate. The frontend canister's AssetRouter always uses v2 certification.
#[test]
fn frontend_serves_certified_response_without_version_hint() -> Result<(), RejectResponse> {
    const CERTIFICATION_VERSION: u16 = 2;
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
            .as_ref()
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
            .as_ref()
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

/// The HTTP gateway can only deliver certified responses for the IdP's
/// form_post callback if the query handler upgrades the POST to update mode.
#[test]
fn frontend_canister_upgrades_callback_post_to_update() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let request = HttpRequest {
        method: "POST".to_string(),
        url: "/callback".to_string(),
        headers: vec![(
            "Content-Type".to_string(),
            "application/x-www-form-urlencoded".to_string(),
        )],
        body: ByteBuf::from(b"id_token=abc.def.ghi&state=c3RhdGU".to_vec()),
        certificate_version: None,
    };
    let response = http_request(&env, canister_id, &request)?;

    assert_eq!(response.upgrade, Some(true));
    Ok(())
}

/// POSTs to anything but /callback don't trigger an upgrade (which would
/// cost an update call) and are rejected right away.
#[test]
fn frontend_canister_rejects_post_to_other_paths() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let request = HttpRequest {
        method: "POST".to_string(),
        url: "/authorize".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: None,
    };
    let response = http_request(&env, canister_id, &request)?;

    assert_eq!(response.status_code, 405);
    assert_ne!(response.upgrade, Some(true));
    Ok(())
}

/// A valid form_post body is translated into the landing page that delivers
/// the payload to the frontend, with the inline script hash-pinned in the
/// response CSP.
#[test]
fn frontend_canister_translates_form_post_callback() -> Result<(), RejectResponse> {
    const ID_TOKEN: &str = "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJ0ZXN0In0.c2lnbmF0dXJl";
    const STATE: &str = "Y2FsbGJhY2stc3RhdGU";
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let request = HttpRequest {
        method: "POST".to_string(),
        url: "/callback".to_string(),
        headers: vec![(
            "Content-Type".to_string(),
            "application/x-www-form-urlencoded".to_string(),
        )],
        // `code` accompanies our `response_type=code id_token` request and is
        // ignored by the handler.
        body: ByteBuf::from(format!("code=4%2Fabc&id_token={ID_TOKEN}&state={STATE}").into_bytes()),
        certificate_version: None,
    };
    let response = canister_tests::api::http_request_update(&env, canister_id, &request)?;

    assert_eq!(response.status_code, 200);
    verify_frontend_security_headers(&response.headers);
    let body = std::str::from_utf8(&response.body).expect("Body is not valid UTF-8");
    assert!(body.contains(&format!("\"id_token\":\"{ID_TOKEN}\"")));
    assert!(body.contains(&format!("\"state\":\"{STATE}\"")));
    assert!(body.contains("BroadcastChannel(\"redirect_callback\")"));
    assert!(body.contains("sessionStorage.setItem(\"ii-openid-callback-data\""));

    // The inline script must be pinned in the CSP so nothing else can run.
    let csp = response
        .headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "content-security-policy")
        .map(|(_, value)| value.clone())
        .expect("Content-Security-Policy header missing");
    assert!(csp.contains("sha384-"));

    let cache_control = response
        .headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "cache-control")
        .map(|(_, value)| value.clone());
    assert_eq!(cache_control.as_deref(), Some("no-store"));
    Ok(())
}

/// RFC 6749 error reports from the IdP travel through the same landing page
/// so the frontend can surface them as `OAuthProviderError`.
#[test]
fn frontend_canister_translates_form_post_provider_error() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    let request = HttpRequest {
        method: "POST".to_string(),
        url: "/callback".to_string(),
        headers: vec![],
        body: ByteBuf::from(
            b"error=access_denied&error_description=User+cancelled&state=c3RhdGU".to_vec(),
        ),
        certificate_version: None,
    };
    let response = canister_tests::api::http_request_update(&env, canister_id, &request)?;

    assert_eq!(response.status_code, 200);
    let body = std::str::from_utf8(&response.body).expect("Body is not valid UTF-8");
    assert!(body.contains("\"error\":\"access_denied\""));
    assert!(body.contains("\"error_description\":\"User cancelled\""));
    Ok(())
}

/// Bodies that aren't a translatable callback get a static error page, not
/// a page with an embedded payload.
#[test]
fn frontend_canister_rejects_malformed_form_post() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_frontend_canister(&env, II_FRONTEND_WASM.clone(), default_frontend_args());

    for body in [
        &b"id_token=abc.def.ghi"[..],                  // missing state
        &b"state=c3RhdGU"[..],                         // missing id_token and error
        &b"id_token=<script>&state=c3RhdGU"[..],       // invalid token charset
        &b"id_token=abc.def.ghi&state=bad state!"[..], // invalid state charset
    ] {
        let request = HttpRequest {
            method: "POST".to_string(),
            url: "/callback".to_string(),
            headers: vec![],
            body: ByteBuf::from(body.to_vec()),
            certificate_version: None,
        };
        let response = canister_tests::api::http_request_update(&env, canister_id, &request)?;

        assert_eq!(response.status_code, 400);
        let page = std::str::from_utf8(&response.body).expect("Body is not valid UTF-8");
        assert!(page.contains("Sign-in could not be completed"));
        assert!(!page.contains("<script>"));
    }
    Ok(())
}

/// `http_request_update` only exists for the callback translation; anything
/// else is rejected.
#[test]
fn frontend_canister_rejects_other_update_requests() -> Result<(), RejectResponse> {
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
    let response = canister_tests::api::http_request_update(&env, canister_id, &request)?;

    assert_eq!(response.status_code, 405);
    Ok(())
}
