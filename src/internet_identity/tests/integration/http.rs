//! Tests for the HTTP interactions according to the HTTP gateway spec: https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway
//! Includes tests for the HTTP endpoint (including asset certification) and the metrics endpoint.

use canister_tests::api::{http_request, internet_identity as api};
use canister_tests::flows;
use canister_tests::framework::*;
use ic_response_verification::types::{Request, Response};
use ic_response_verification::{verify_request_response_pair, MIN_VERIFICATION_VERSION};
use ic_test_state_machine_client::CallError;
use internet_identity_interface::http_gateway::HttpRequest;
use internet_identity_interface::internet_identity::types::ChallengeAttempt;
use serde_bytes::ByteBuf;
use std::time::{Duration, UNIX_EPOCH};

/// Verifies that some expected assets are delivered, certified and have security headers.
#[test]
fn ii_canister_serves_http_assets() -> Result<(), CallError> {
    let assets: Vec<(&str, Option<&str>)> = vec![
        ("/", None),
        ("/index.js", Some("gzip")),
        ("/.well-known/ic-domains", None),
    ];
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // for each asset, fetch the asset, check the HTTP status code, headers and certificate.
    for (asset, encoding) in assets {
        let request = HttpRequest {
            method: "GET".to_string(),
            url: asset.to_string(),
            headers: vec![],
            body: ByteBuf::new(),
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
        verify_security_headers(&http_response.headers);

        verify_request_response_pair(
            Request {
                method: request.method,
                url: request.url,
                headers: request.headers,
            },
            Response {
                status_code: http_response.status_code,
                headers: http_response.headers,
                body: http_response.body.into_vec(),
            },
            canister_id.as_slice(),
            time(&env) as u128,
            Duration::from_secs(300).as_nanos(),
            &env.root_key(),
            MIN_VERIFICATION_VERSION,
        )
        .unwrap_or_else(|e| panic!("validation for asset \"{asset}\" failed: {e}"));
    }
    Ok(())
}

/// Verifies that all expected metrics are available via the HTTP endpoint.
#[test]
fn ii_canister_serves_http_metrics() -> Result<(), CallError> {
    let metrics = vec![
        "internet_identity_user_count",
        "internet_identity_min_user_number",
        "internet_identity_max_user_number",
        "internet_identity_signature_count",
        "internet_identity_stable_memory_pages",
        "internet_identity_last_upgrade_timestamp",
        "internet_identity_inflight_challenges",
        "internet_identity_users_in_registration_mode",
        "internet_identity_buffered_archive_entries",
        "internet_identity_max_num_latest_delegation_origins",
    ];
    let env = env();
    env.advance_time(Duration::from_secs(300)); // advance time to see it reflected on the metrics endpoint

    // spawn an archive so that we also get the archive related metrics
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
            env.time(),
            "metric timestamp did not match state machine time"
        )
    }
    Ok(())
}

/// Verifies that the metrics list the expected user range.
#[test]
fn metrics_should_list_expected_user_range() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let metrics = get_metrics(&env, canister_id);

    let (min_user_number, _) = parse_metric(&metrics, "internet_identity_min_user_number");
    let (max_user_number, _) = parse_metric(&metrics, "internet_identity_max_user_number");
    assert_eq!(min_user_number, 10_000f64);
    assert_eq!(max_user_number, 8_188_859f64);
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
    let (stable_memory_pages, _) = parse_metric(&metrics, "internet_identity_stable_memory_pages");
    // empty II has some metadata in stable memory which requires at least one page
    assert_eq!(stable_memory_pages, 1f64);

    // the anchor offset is 2 pages -> adding a single anchor increases stable memory usage to
    // 3 pages
    flows::register_anchor(&env, canister_id);

    let metrics = get_metrics(&env, canister_id);
    let (stable_memory_pages, _) = parse_metric(&metrics, "internet_identity_stable_memory_pages");
    assert_eq!(stable_memory_pages, 3f64);
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
        env.time().duration_since(UNIX_EPOCH).unwrap().as_nanos() as f64,
    );

    env.advance_time(Duration::from_secs(300)); // the state machine does not advance time on its own
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_last_upgrade_timestamp",
        env.time().duration_since(UNIX_EPOCH).unwrap().as_nanos() as f64,
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
