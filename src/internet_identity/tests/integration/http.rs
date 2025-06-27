//! Tests for the HTTP interactions according to the HTTP gateway spec: https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway
//! Includes tests for the HTTP endpoint (including asset certification) and the metrics endpoint.

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, create_identity_with_authn_methods,
    sample_pubkey_authn_method, test_authn_method,
};
use canister_tests::api::internet_identity::api_v2;
use canister_tests::api::{http_request, internet_identity as api};
use canister_tests::flows;
use canister_tests::framework::*;
use flate2::read::GzDecoder;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_response_verification::types::VerificationInfo;
use ic_response_verification::verify_request_response_pair;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::vc_mvp::PrepareIdAliasRequest;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, CaptchaConfig, CaptchaTrigger, ChallengeAttempt, FrontendHostname,
    InternetIdentityInit, MetadataEntryV2,
};
use pocket_ic::{CallError, PocketIc};
use serde_bytes::ByteBuf;
use serde_json::json;
use std::collections::HashMap;
use std::io::Read;
use std::time::Duration;

/// Verifies that some expected assets are delivered, certified and have security headers.
#[test]
fn ii_canister_serves_http_assets() -> Result<(), CallError> {
    let assets: Vec<(&str, Option<&str>)> = vec![("/", None), ("/.well-known/ic-domains", None)];
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // for each asset and certification version, fetch the asset, check the HTTP status code, headers and certificate.
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

            // check the appropriate Content-Encoding header is set
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
fn ii_canister_serves_webauthn_assets() -> Result<(), CallError> {
    let env = env();
    let related_origins: Vec<String> = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ]
    .to_vec();
    let config = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: Some(related_origins.clone()),
        new_flow_origins: None,
        openid_google: None,
        analytics_config: None,
        fetch_root_key: None,
        enable_dapps_explorer: None,
        is_production: None,
        dummy_auth: None,
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));

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

        // check the appropriate Content-Type header is set
        let (_, content_type) = http_response
            .headers
            .iter()
            .find(|(name, _)| name.to_lowercase() == "content-type")
            .expect("Content-Encoding header not found");
        assert_eq!(
            content_type, "application/json",
            "unexpected Content-Encoding header value"
        );
        verify_security_headers(&http_response.headers, &config.related_origins);

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

#[test]
fn ii_canister_serves_webauthn_assets_after_upgrade() -> Result<(), CallError> {
    let env = env();
    let related_origins: Vec<String> = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ]
    .to_vec();
    let config = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: Some(related_origins.clone()),
        new_flow_origins: None,
        openid_google: None,
        analytics_config: None,
        fetch_root_key: None,
        enable_dapps_explorer: None,
        is_production: None,
        dummy_auth: None,
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    let request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/webauthn".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(2),
    };
    let http_response = http_request(&env, canister_id, &request)?;
    let response_body = String::from_utf8_lossy(&http_response.body).to_string();
    assert_eq!(http_response.status_code, 200);
    let expected_content = json!({
        "origins": related_origins,
    })
    .to_string();
    assert_eq!(response_body, expected_content);

    let _ = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None);

    let http_response_1 = http_request(&env, canister_id, &request)?;
    let response_body_1 = String::from_utf8_lossy(&http_response_1.body).to_string();
    assert_eq!(response_body_1, expected_content);

    let related_origins_2: Vec<String> = [
        "https://beta.identity.internetcomputer.org".to_string(),
        "https://beta.identity.ic0.app".to_string(),
    ]
    .to_vec();
    let config_2 = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: Some(related_origins_2.clone()),
        new_flow_origins: None,
        openid_google: None,
        analytics_config: None,
        fetch_root_key: None,
        enable_dapps_explorer: None,
        is_production: None,
        dummy_auth: None,
    };

    let _ = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config_2));

    let http_response_2 = http_request(&env, canister_id, &request)?;
    let response_body_2 = String::from_utf8_lossy(&http_response_2.body).to_string();
    let expected_content_2 = json!({
        "origins": related_origins_2,
    })
    .to_string();
    assert_eq!(response_body_2, expected_content_2);
    Ok(())
}

/// Verifies that clients that do not indicate any certification version will get a v1 certificate.
#[test]
fn should_fallback_to_v1_certification() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 1;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

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

/// Verifies that the cache-control header is set for fonts.
// #[test] TODO: There's no CSS file directly referenced in the HTML file anymore since the SvelteKit migration
#[allow(dead_code)]
fn should_set_cache_control_for_fonts() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // Get index page
    let index_request = HttpRequest {
        method: "GET".to_string(),
        url: "/".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let index_response = http_request(&env, canister_id, &index_request)?;

    // Convert body to string
    let html = String::from_utf8(index_response.body.into_vec()).expect("Failed to parse HTML");

    // Find the CSS URL in the HTML
    let css_url = {
        let css_suffix = "cacheable.css";
        let css_end = html
            .find(css_suffix)
            .expect("Could not find cacheable.css in HTML");
        let prefix_start = html[..css_end]
            .rfind('/')
            .expect("Could not find starting / for CSS URL");

        html[prefix_start..css_end + css_suffix.len()].to_string()
    };

    // Get CSS file
    let css_request = HttpRequest {
        method: "GET".to_string(),
        url: css_url,
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };

    let css_response = http_request(&env, canister_id, &css_request)?;

    let css_result = verify_response_certification(
        &env,
        canister_id,
        css_request,
        css_response.clone(),
        CERTIFICATION_VERSION,
    );
    assert_eq!(css_result.verification_version, CERTIFICATION_VERSION);

    let css_body = String::from_utf8(css_response.body.into_vec()).expect("Failed to parse CSS");

    // Find the Font URL in the CSS
    let font_url = css_body
        .lines()
        .find(|line| line.contains("CircularXXWeb-Regular"))
        .and_then(|line| {
            // Extract URL from the line using the pattern: url(/path) format("woff2")
            if let Some(url_start) = line.find("url(") {
                let start = url_start + 4; // "url(" is 4 chars
                if let Some(url_end) = line[start..].find(")") {
                    let url = line[start..start + url_end].trim_matches(|c| c == '"' || c == '/');
                    Some(format!("/{url}"))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .expect("Could not find css URL in HTML");

    let font_request = HttpRequest {
        method: "GET".to_string(),
        url: font_url,
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let font_response = http_request(&env, canister_id, &font_request)?;

    assert_eq!(font_response.status_code, 200);
    assert!(font_response.headers.contains(&(
        "Cache-Control".to_string(),
        "public, max-age=31536000".to_string()
    )));

    let result = verify_response_certification(
        &env,
        canister_id,
        font_request,
        font_response,
        CERTIFICATION_VERSION,
    );
    assert_eq!(result.verification_version, CERTIFICATION_VERSION);

    Ok(())
}

/// Verifies that the cache-control header is set for the SPA file.
// #[test] TODO: There's no spa file anymore since the SvelteKit migration
#[allow(dead_code)]
fn should_set_cache_control_for_spa() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // Get index page
    let index_request = HttpRequest {
        method: "GET".to_string(),
        url: "/".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let index_response = http_request(&env, canister_id, &index_request)?;

    // Convert body to string
    let html = String::from_utf8(index_response.body.into_vec()).expect("Failed to parse HTML");

    // Find the SPA URL in the HTML
    let spa_url = {
        let spa_suffix = "cacheable.js";
        let spa_end = html
            .find(spa_suffix)
            .expect("Could not find cacheable.js in HTML");
        let prefix_start = html[..spa_end]
            .rfind('/')
            .expect("Could not find starting / for spa URL");

        html[prefix_start..spa_end + spa_suffix.len()].to_string()
    };

    // Get SPA file
    let request = HttpRequest {
        method: "GET".to_string(),
        url: spa_url,
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };

    let http_response = http_request(&env, canister_id, &request)?;

    assert_eq!(http_response.status_code, 200);
    assert!(http_response.headers.contains(&(
        "Cache-Control".to_string(),
        "public, max-age=31536000".to_string()
    )));

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

/// Verifies that the cache-control header is set for the icons.
// #[test] TODO: There's no spa file referencing the icons anymore since the SvelteKit migration
#[allow(dead_code)]
fn should_set_cache_control_for_icons() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // Icon we are testing for
    const ICON_NAME: &str = "icpswap_logo";
    const ICON_SUFFIX: &str = ".webp";

    // Get index page
    let index_request = HttpRequest {
        method: "GET".to_string(),
        url: "/".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let index_response = http_request(&env, canister_id, &index_request)?;

    // Convert body to string
    let index_html =
        String::from_utf8(index_response.body.into_vec()).expect("Failed to parse HTML");

    // Find all cacheable.js URLs in the HTML
    let spa_urls: Vec<String> = {
        let mut urls = Vec::new();
        let mut search_start = 0;
        let spa_suffix = "cacheable.js";

        while let Some(spa_end) = index_html[search_start..].find(spa_suffix) {
            let absolute_end = search_start + spa_end;
            if let Some(prefix_start) = index_html[..absolute_end].rfind('/') {
                urls.push(index_html[prefix_start..absolute_end + spa_suffix.len()].to_string());
            }
            search_start = absolute_end + spa_suffix.len();
        }
        urls
    };

    // Try each SPA file until we find the icon
    let mut found_icon = false;
    for spa_url in spa_urls {
        // Get SPA file
        let spa_request = HttpRequest {
            method: "GET".to_string(),
            url: spa_url,
            headers: vec![],
            body: ByteBuf::new(),
            certificate_version: Some(CERTIFICATION_VERSION),
        };

        let spa_response = http_request(&env, canister_id, &spa_request)?;

        assert_eq!(spa_response.status_code, 200);
        assert!(spa_response.headers.contains(&(
            "Cache-Control".to_string(),
            "public, max-age=31536000".to_string()
        )));

        let spa_result = verify_response_certification(
            &env,
            canister_id,
            spa_request,
            spa_response.clone(),
            CERTIFICATION_VERSION,
        );
        assert_eq!(spa_result.verification_version, CERTIFICATION_VERSION);

        let spa_bytes = spa_response.body.into_vec();

        // Decompress the SPA bytes
        let mut decoder = GzDecoder::new(&spa_bytes[..]);
        let mut spa_body = String::new();
        decoder.read_to_string(&mut spa_body).unwrap();

        // Try to find the icon URL in this SPA file
        if let Some(icon_url) = spa_body
            .lines()
            .find(|line| line.contains(ICON_NAME))
            .and_then(|line| {
                if let Some(url_start) = line.find(&format!("\"/{ICON_NAME}")) {
                    if let Some(url_end) = line[url_start..].find(&format!("{ICON_SUFFIX}\"")) {
                        let url = line[url_start..url_start + url_end + 5]
                            .trim_matches(|c| c == '"' || c == '/');
                        Some(format!("/{url}"))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        {
            // Found the icon, test it
            let icon_request = HttpRequest {
                method: "GET".to_string(),
                url: icon_url,
                headers: vec![],
                body: ByteBuf::new(),
                certificate_version: Some(CERTIFICATION_VERSION),
            };

            let icon_response = http_request(&env, canister_id, &icon_request)?;

            assert_eq!(icon_response.status_code, 200);
            assert!(icon_response.headers.contains(&(
                "Cache-Control".to_string(),
                "public, max-age=31536000".to_string()
            )));

            let icon_result = verify_response_certification(
                &env,
                canister_id,
                icon_request,
                icon_response,
                CERTIFICATION_VERSION,
            );
            assert_eq!(icon_result.verification_version, CERTIFICATION_VERSION);

            found_icon = true;
            break;
        }
    }

    assert!(found_icon, "Icon not found in any cacheable.js file");
    Ok(())
}

#[test]
fn must_not_cache_well_known_ic_domains() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // Get index page
    let well_known_request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/ic-domains".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let well_known_response = http_request(&env, canister_id, &well_known_request)?;

    assert_eq!(well_known_response.status_code, 200);
    println!("{:?}", well_known_response.headers);
    assert!(
        !well_known_response // Make sure we have no cache-control headers whatsoever on the response
            .headers
            .clone()
            .into_iter()
            .map(|headers| headers.0) // Get only the key
            .collect::<Vec<String>>()
            .contains(&"Cache-Control".to_string())
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
fn must_not_cache_well_known_webauthn() -> Result<(), CallError> {
    const CERTIFICATION_VERSION: u16 = 2;
    let env = env();
    let related_origins: Vec<String> = [
        "https://identity.internetcomputer.org".to_string(),
        "https://identity.ic0.app".to_string(),
    ]
    .to_vec();
    let config = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: None,
        related_origins: Some(related_origins.clone()),
        new_flow_origins: None,
        openid_google: None,
        analytics_config: None,
        fetch_root_key: None,
        enable_dapps_explorer: None,
        is_production: None,
        dummy_auth: None,
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));

    // Get index page
    let well_known_request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/webauthn".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(CERTIFICATION_VERSION),
    };
    let well_known_response = http_request(&env, canister_id, &well_known_request)?;

    assert_eq!(well_known_response.status_code, 200);
    println!("{:?}", well_known_response.headers);
    assert!(
        !well_known_response // Make sure we have no cache-control headers whatsoever on the response
            .headers
            .clone()
            .into_iter()
            .map(|headers| headers.0) // Get only the key
            .collect::<Vec<String>>()
            .contains(&"Cache-Control".to_string())
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

/// Verifies that expected metrics are available via the HTTP endpoint.
#[test]
fn ii_canister_serves_http_metrics() -> Result<(), CallError> {
    let metrics = vec![
        "internet_identity_user_count",
        "internet_identity_min_user_number",
        "internet_identity_max_user_number",
        "internet_identity_signature_count",
        "internet_identity_stable_memory_pages",
        "stable_memory_bytes",
        "internet_identity_heap_pages",
        "heap_memory_bytes",
        "internet_identity_last_upgrade_timestamp",
        "internet_identity_inflight_challenges",
        "internet_identity_users_in_registration_mode",
        "internet_identity_buffered_archive_entries",
        "internet_identity_prepare_id_alias_counter",
    ];
    let env = env();
    env.advance_time(Duration::from_secs(300)); // Advance time to see it reflected on the metrics endpoint

    // Spawn an archive so that we also get the archive related metrics
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone()),
    );
    deploy_archive_via_ii(&env, canister_id);

    let metrics_body = get_metrics(&env, canister_id);
    for metric in metrics {
        let (_, metric_timestamp) = parse_metric(&metrics_body, metric);
        assert_eq!(
            metric_timestamp,
            Duration::from_nanos(time(&env)).as_millis() as u64,
            "metric timestamp did not match state machine time"
        )
    }
    Ok(())
}

/// Verifies that the metrics list the expected user range as configured.
#[test]
fn metrics_should_list_configured_user_range() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_anchor_range((10_123, 8_188_860)),
    );

    let metrics = get_metrics(&env, canister_id);

    let (min_user_number, _) = parse_metric(&metrics, "internet_identity_min_user_number");
    let (max_user_number, _) = parse_metric(&metrics, "internet_identity_max_user_number");
    assert_eq!(min_user_number, 10_123f64);
    assert_eq!(max_user_number, 8_188_859f64);
    Ok(())
}

/// Verifies that the metrics list the default user range if none is configured.
#[test]
fn metrics_should_list_default_user_range() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let metrics = get_metrics(&env, canister_id);

    let (min_user_number, _) = parse_metric(&metrics, "internet_identity_min_user_number");
    let (max_user_number, _) = parse_metric(&metrics, "internet_identity_max_user_number");
    assert_eq!(min_user_number, 10_000f64);
    assert_eq!(max_user_number, 67_116_815f64);
    Ok(())
}

/// Verifies that the user count metric is updated correctly.
#[test]
fn metrics_user_count_should_increase_after_register() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_user_count",
        0f64,
    );
    for count in 0..2 {
        flows::register_anchor(&env, canister_id);
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_user_count",
            (count + 1) as f64,
        );
    }
    Ok(())
}

/// Verifies that the signature count metric is updated correctly.
#[test]
fn metrics_signature_and_delegation_count() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let frontend_hostname = "https://some-dapp.com";
    let user_number = flows::register_anchor(&env, canister_id);

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_signature_count",
        0f64,
    );
    for count in 0..3 {
        api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname,
            &ByteBuf::from(format!("session key {count}")),
            None,
        )?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_signature_count",
            (count + 1) as f64,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_delegation_counter",
            (count + 1) as f64,
        );
    }

    // long after expiry (we don't want this test to break, if we change the default delegation expiration)
    env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
    // we need to make an update call to prune expired delegations
    api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &ByteBuf::from("last session key"),
        None,
    )?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_signature_count",
        1f64, // old ones pruned and a new one created
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_delegation_counter",
        4f64, // delegation counter is not affected by pruning
    );
    Ok(())
}

/// Verifies that the stable memory pages count metric is updated correctly.
#[test]
fn metrics_stable_memory_pages_should_increase_with_more_users() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let metrics = get_metrics(&env, canister_id);
    let (initial_memory_pages, _) = parse_metric(&metrics, "internet_identity_stable_memory_pages");

    // the anchor offset is 2 pages -> adding a single anchor increases stable memory usage by
    // one bucket (i.e. 128 pages) allocated by the memory manager.
    flows::register_anchor(&env, canister_id);

    let metrics = get_metrics(&env, canister_id);
    let (pages_with_users, _) = parse_metric(&metrics, "internet_identity_stable_memory_pages");
    assert!(initial_memory_pages < pages_with_users);
    Ok(())
}

/// Verifies that the last II wasm upgrade timestamp is updated correctly.
#[test]
fn metrics_last_upgrade_timestamp_should_update_after_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    // immediately upgrade because installing the canister does not set the metric
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_last_upgrade_timestamp",
        time(&env) as f64,
    );

    env.advance_time(Duration::from_secs(300)); // the state machine does not advance time on its own
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_last_upgrade_timestamp",
        time(&env) as f64,
    );
    Ok(())
}

/// Verifies that the inflight challenges metric is updated correctly.
#[test]
fn metrics_inflight_challenges() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
    assert_eq!(challenge_count, 0f64);

    let challenge_1 = api::create_challenge(&env, canister_id)?;
    api::create_challenge(&env, canister_id)?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
    assert_eq!(challenge_count, 2f64);

    // solving a challenge removes it from the inflight pool
    api::register(
        &env,
        canister_id,
        principal_1(),
        &device_data_1(),
        &ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge_1.challenge_key,
        },
        None,
    )?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
    assert_eq!(challenge_count, 1f64);

    // long after expiry (we don't want this test to break, if we change the captcha expiration)
    env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
    // the only call that prunes expired captchas
    api::create_challenge(&env, canister_id)?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
    assert_eq!(challenge_count, 1f64); // 1 pruned due to expiry, but also one created

    Ok(())
}

/// Verifies that the users in registration mode metric is updated correctly.
#[test]
fn metrics_device_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number_1 = flows::register_anchor(&env, canister_id);
    let user_number_2 = flows::register_anchor(&env, canister_id);

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) =
        parse_metric(&metrics, "internet_identity_users_in_registration_mode");
    assert_eq!(challenge_count, 0f64);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;
    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_2)?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) =
        parse_metric(&metrics, "internet_identity_users_in_registration_mode");
    assert_eq!(challenge_count, 2f64);

    api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) =
        parse_metric(&metrics, "internet_identity_users_in_registration_mode");
    assert_eq!(challenge_count, 1f64);

    // long after expiry (we don't want this test to break, if we change the registration mode expiration)
    env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
    // make an update call related to tentative devices so that registration mode expiry gets checked
    api::add_tentative_device(&env, canister_id, user_number_2, &device_data_2())?;

    let metrics = get_metrics(&env, canister_id);
    let (challenge_count, _) =
        parse_metric(&metrics, "internet_identity_users_in_registration_mode");
    assert_eq!(challenge_count, 0f64);

    Ok(())
}

/// Verifies that the anchor operation count metric is updated correctly.
#[test]
fn metrics_anchor_operations() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_anchor_operations_counter",
        0f64,
    );

    let user_number = flows::register_anchor(&env, canister_id);
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_anchor_operations_counter",
        1f64,
    );

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_anchor_operations_counter",
        2f64,
    );

    let mut device = device_data_2();
    device.alias = "new alias".to_string();
    api::update(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device.pubkey,
        &device,
    )?;
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_anchor_operations_counter",
        3f64,
    );

    api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2().pubkey,
    )?;
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_anchor_operations_counter",
        4f64,
    );

    Ok(())
}

#[test]
fn should_list_virtual_memory_metrics() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"header\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"identities\"}",
        0f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"archive_buffer\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"event_data\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"event_aggregations\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"reference_registration_rate\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"current_registration_rate\"}",
        1f64,
    );

    let authn_method = test_authn_method();
    create_identity_with_authn_method(&env, canister_id, &authn_method);

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"header\"}",
        1f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_virtual_memory_size_pages{memory=\"identities\"}",
        1f64,
    );

    // To test the archive buffer and event data related memory metrics growing,
    // we would have a very complex setup and require a large number of request.
    // Or load a prepared state with a large number of entries.
    // This is not done here, as it would either require brittle setup or a long-running test.

    Ok(())
}

#[test]
fn should_list_aggregated_session_seconds_and_event_data_counters() -> Result<(), CallError> {
    let pub_session_key = ByteBuf::from("session public key");
    let authn_method_ic0 = AuthnMethodData {
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntryV2::String("https://identity.ic0.app".to_string()),
        )]),
        ..sample_pubkey_authn_method(1)
    };
    let authn_method_internetcomputer = AuthnMethodData {
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntryV2::String("https://identity.internetcomputer.org".to_string()),
        )]),
        ..sample_pubkey_authn_method(2)
    };

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number_1 = create_identity_with_authn_methods(
        &env,
        canister_id,
        &[
            test_authn_method(),
            authn_method_ic0.clone(),
            authn_method_internetcomputer.clone(),
        ],
    );

    let metrics = get_metrics(&env, canister_id);
    // make sure empty data is not listed on the metrics endpoint
    assert!(!metrics.contains("internet_identity_prepare_delegation_session_seconds{"));
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_data_count",
        0f64,
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_aggregations_count",
        0f64,
    );

    api::prepare_delegation(
        &env,
        canister_id,
        test_authn_method().principal(),
        user_number_1,
        "https://some-dapp-1.com",
        &pub_session_key,
        None,
    )?;
    api::prepare_delegation(
        &env,
        canister_id,
        authn_method_ic0.principal(),
        user_number_1,
        "https://some-dapp-2.com",
        &pub_session_key,
        Some(Duration::from_secs(3600).as_nanos() as u64),
    )?;
    api::prepare_delegation(
        &env,
        canister_id,
        authn_method_ic0.principal(),
        user_number_1,
        "https://some-dapp-2.com",
        &pub_session_key,
        None,
    )?;
    api::prepare_delegation(
        &env,
        canister_id,
        authn_method_internetcomputer.principal(),
        user_number_1,
        "https://some-dapp-3.com",
        &pub_session_key,
        None,
    )?;

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-2.com\",window=\"24h\",ii_origin=\"ic0.app\"}",
        5400f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_count{dapp=\"https://some-dapp-2.com\",window=\"24h\",ii_origin=\"ic0.app\"}",
        2f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-2.com\",window=\"30d\",ii_origin=\"ic0.app\"}",
        5400f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_count{dapp=\"https://some-dapp-2.com\",window=\"30d\",ii_origin=\"ic0.app\"}",
        2f64,
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_data_count",
        4f64,
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_aggregations_count",
        12f64,
    );
    // make sure aggregations for other II domains are not listed on the metrics endpoint
    assert!(
        !metrics.contains(
            "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-1.com\",window=\"24h\""));
    assert!(
        !metrics.contains(
            "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-3.com\",window=\"24h\""));
    assert!(!metrics.contains("ii_origin=\"other\""));
    assert!(!metrics.contains("ii_origin=\"internetcomputer.org\""));

    // advance time one day to see it reflected on the daily stats
    env.advance_time(Duration::from_secs(60 * 60 * 24));
    // call prepare delegation again to trigger stats update
    api::prepare_delegation(
        &env,
        canister_id,
        authn_method_internetcomputer.principal(),
        user_number_1,
        "https://some-dapp-4.com",
        &pub_session_key,
        None,
    )?;

    let metrics = get_metrics(&env, canister_id);
    // The 24h metrics should be gone now
    assert!(
        !metrics.contains(
            "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-2.com\",window=\"24h\""));
    assert!(
        !metrics.contains(
            "internet_identity_prepare_delegation_count{dapp=\"https://some-dapp-2.com\",window=\"24h\""));

    // The 30d metrics should still be there
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_session_seconds{dapp=\"https://some-dapp-2.com\",window=\"30d\",ii_origin=\"ic0.app\"}",
        5400f64,
    );
    assert_metric(
        &metrics,
        "internet_identity_prepare_delegation_count{dapp=\"https://some-dapp-2.com\",window=\"30d\",ii_origin=\"ic0.app\"}",
        2f64,
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_data_count",
        5f64,
    );
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_event_aggregations_count",
        10f64,
    );
    Ok(())
}

#[test]
fn should_list_prepare_id_alias_counter() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);

    let prepare_id_alias_req = PrepareIdAliasRequest {
        identity_number,
        relying_party: FrontendHostname::from("https://some-dapp.com"),
        issuer: FrontendHostname::from("https://some-issuer-1.com"),
    };

    for _ in 0..3 {
        api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req.clone(),
        )?
        .expect("Got 'None' from prepare_id_alias");
    }

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_prepare_id_alias_counter",
        3f64,
    );
    Ok(())
}

#[test]
fn should_report_registration_rates() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 20,
                    current_rate_sampling_interval_s: 10,
                    reference_rate_sampling_interval_s: 100,
                },
            }),
            ..InternetIdentityInit::default()
        }),
    );

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_registrations_per_second{type=\"reference_rate\"}",
        0.0,
    );
    assert_metric(
        &metrics,
        "internet_identity_registrations_per_second{type=\"current_rate\"}",
        0.0,
    );
    assert_metric(
        &metrics,
        "internet_identity_registrations_per_second{type=\"captcha_threshold_rate\"}",
        0.0,
    );

    for _ in 0..20 {
        // make sure both registration flows are counted
        flows::register_anchor(&env, canister_id); // legacy API
        create_identity_with_authn_method(&env, canister_id, &test_authn_method()); // v2 API
        env.advance_time(Duration::from_secs(1));
    }

    // advance time a little further to make reference rate be different from the current rate
    env.advance_time(Duration::from_secs(5));
    env.tick(); // tick for the advance time to become effective
    let metrics = get_metrics(&env, canister_id);
    assert_metric_approx(
        &metrics,
        "internet_identity_registrations_per_second{type=\"reference_rate\"}",
        0.4,
        0.1,
    );
    assert_metric_approx(
        &metrics,
        "internet_identity_registrations_per_second{type=\"current_rate\"}",
        2f64,
        0.1,
    );
    assert_metric_approx(
        &metrics,
        "internet_identity_registrations_per_second{type=\"captcha_threshold_rate\"}",
        0.48,
        0.1,
    );
    Ok(())
}

#[test]
fn should_report_total_account_metrics() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();
    let name = "Callisto".to_string();

    let initial_metrics = get_metrics(&env, canister_id);
    assert_metric(
        &initial_metrics,
        "internet_identity_total_accounts_count",
        0f64,
    );
    assert_metric(
        &initial_metrics,
        "internet_identity_total_account_references_count",
        0f64,
    );
    assert_metric(
        &initial_metrics,
        "internet_identity_total_application_count",
        0f64,
    );
    assert_metric(
        &initial_metrics,
        "internet_identity_account_counter_discrepancy_count",
        0f64,
    );

    let _ = api_v2::create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )?;
    let metrics = get_metrics(&env, canister_id);
    assert_metric(&metrics, "internet_identity_total_accounts_count", 1f64);
    assert_metric(
        &metrics,
        "internet_identity_total_account_references_count",
        // One for default account, one for created account
        2f64,
    );
    assert_metric(&metrics, "internet_identity_total_application_count", 1f64);
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
        ic_http_certification::HttpRequest {
            method: request.method,
            url: request.url,
            headers: request.headers,
            body: request.body.into_vec(),
        },
        ic_http_certification::HttpResponse {
            status_code: http_response.status_code,
            headers: http_response.headers,
            body: http_response.body.into_vec(),
            upgrade: None,
        },
        canister_id.as_slice(),
        time(env) as u128,
        Duration::from_secs(300).as_nanos(),
        &env.root_key().unwrap(),
        min_certification_version as u8,
    )
    .unwrap_or_else(|e| panic!("validation failed: {e}"))
}
