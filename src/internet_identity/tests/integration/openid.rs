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
    OpenIdCredentialAddError, OpenIdCredentialKey, OpenIdDelegationError, OpenIdResult,
    PublicKeyAuthn, SsoCredentialMigrationEntry,
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

/// Verifies that a JWT can be verified purely from the JWK cache seeded via
/// `OpenIdConfig.seed_jwks`, without ever fetching the provider's `jwks_uri`.
/// No certs HTTP response is mocked here, so a successful add proves the seed
/// populated the cache.
#[test]
fn can_link_google_account_with_seeded_jwks() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister_with_seeded_google_jwks(&env);
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);

    sync_time(&env, test_time);

    // NOTE: deliberately NOT calling `mock_google_certs_response` here — the key
    // used to verify the JWT must come from the install-time `seed_jwks`.
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

// ---------------------------------------------------------------------------
// SSO (discoverable provider) end-to-end tests.
//
// Unlike the configured-provider tests above (which pass `discovery_domain =
// None` and rely on a hardcoded `(iss, aud)` provider), these drive the
// on-demand SSO path through the canister endpoints WITH a `discovery_domain`.
// They mock the two discovery hops + the JWKS fetch, so the single-flight
// discovery/JWKS caches warm from cold (the endpoints return `Pending`) to
// `Ready`, mirroring what the frontend's retry-while-`Pending` loop does. The
// Google test JWT/JWKS are reused, routed through SSO discovery whose hop-2
// issuer is `https://accounts.google.com`.
// ---------------------------------------------------------------------------

/// Domain on the SSO allowlist for these tests.
const SSO_DOMAIN: &str = "example.org";

/// The three outcalls the SSO path makes, as `(url, json_body)`. Hosts are
/// chosen to satisfy the canister's discovery validation: the hop-2 issuer host
/// matches the hop-1 `openid_configuration` host, and the `authorization_endpoint`
/// host matches the issuer host.
fn sso_http_responses() -> Vec<(String, String)> {
    vec![
        // Hop 1: II OpenID configuration served at the discovery domain. Its
        // `client_id` is the JWT's `aud`; `name` becomes the stamped `sso_name`.
        (
            format!("https://{SSO_DOMAIN}/.well-known/ii-openid-configuration"),
            r#"{"client_id":"360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com","openid_configuration":"https://accounts.google.com/.well-known/openid-configuration","name":"Example"}"#.to_string(),
        ),
        // Hop 2: the standard OIDC discovery document.
        (
            "https://accounts.google.com/.well-known/openid-configuration".to_string(),
            r#"{"issuer":"https://accounts.google.com","jwks_uri":"https://www.googleapis.com/oauth2/v3/certs","authorization_endpoint":"https://accounts.google.com/o/oauth2/v2/auth","scopes_supported":["openid","email","profile"]}"#.to_string(),
        ),
        // JWKS: the single key (matched by `kid`) that signed the Google test JWT.
        (
            "https://www.googleapis.com/oauth2/v3/certs".to_string(),
            r#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b","n":"y8TPCPz2Fp0OhBxsxu6d_7erT9f9XJ7mx7ZJPkkeZRxhdnKtg327D4IGYsC4fLAfpkC8qN58sZGkwRTNs-i7yaoD5_8nupq1tPYvnt38ddVghG9vws-2MvxfPQ9m2uxBEdRHmels8prEYGCH6oFKcuWVsNOt4l_OPoJRl4uiuiwd6trZik2GqDD_M6bn21_w6AD_jmbzN4mh8Od4vkA1Z9lKb3Qesksxdog-LWHsljN8ieiz1NhbG7M-GsIlzu-typJfud3tSJ1QHb-E_dEfoZ1iYK7pMcojb5ylMkaCj5QySRdJESq9ngqVRDjF4nX8DK5RQUS7AkrpHiwqyW0Csw","e":"AQAB"}]}"#.to_string(),
        ),
    ]
}

/// Install II with only the SSO allowlist set (no configured providers), plus
/// the cycles the discovery/JWKS HTTP outcalls need.
fn setup_sso_canister(env: &PocketIc) -> Principal {
    let args = InternetIdentityInit {
        sso_discoverable_domains: Some(vec![SSO_DOMAIN.to_string()]),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };
    install_ii_canister_with_arg_and_cycles(env, II_WASM.clone(), Some(args), 10_000_000_000_000)
}

/// Tick once and answer every pending SSO discovery/JWKS outcall from
/// `responses`. Returns how many it answered (0 once the caches are warm and the
/// canister stops fetching). Panics on an outcall to an unexpected URL.
fn answer_sso_http(env: &PocketIc, responses: &[(String, String)]) -> usize {
    env.tick();
    let mut answered = 0;
    for req in env.get_canister_http() {
        let body = responses
            .iter()
            .find(|(url, _)| *url == req.url)
            .unwrap_or_else(|| panic!("unexpected SSO outcall to {}", req.url))
            .1
            .clone();
        env.mock_canister_http_response(MockCanisterHttpResponse {
            subnet_id: req.subnet_id,
            request_id: req.request_id,
            response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                status: 200,
                headers: vec![],
                body: body.into_bytes(),
            }),
            additional_responses: vec![],
        });
        env.tick();
        answered += 1;
    }
    answered
}

/// Drive an SSO endpoint call that warms its caches on demand: invoke `call`,
/// and while it reports `Pending` (cold/in-flight discovery or JWKS), advance
/// the outcalls and retry — exactly the frontend's retry-while-`Pending` loop.
fn drive_sso_until_ready<T, E, F>(
    env: &PocketIc,
    responses: &[(String, String)],
    mut call: F,
) -> Result<T, E>
where
    F: FnMut() -> OpenIdResult<T, E>,
{
    for _ in 0..30 {
        match call() {
            OpenIdResult::Ok(value) => return Ok(value),
            OpenIdResult::Err(err) => return Err(err),
            OpenIdResult::Pending => {
                // Advance the in-flight discovery (hop 1 -> hop 2) and JWKS
                // fills; several ticks let each sequential outcall surface.
                for _ in 0..8 {
                    answer_sso_http(env, responses);
                }
            }
        }
    }
    panic!("SSO caches never warmed");
}

/// Links an SSO account end to end: the cold discovery + JWKS caches warm via
/// `Pending` retries, and the stored credential carries the discovered
/// `sso_domain` / `sso_name` stamp.
#[test]
fn can_link_sso_account_via_discovery() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_sso_canister(&env);
    let responses = sso_http_responses();
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);
    sync_time(&env, test_time);

    drive_sso_until_ready(&env, &responses, || {
        api::openid_credential_add_with_discovery(
            &env,
            canister_id,
            test_principal,
            identity_number,
            &jwt,
            &salt,
            Some(SSO_DOMAIN),
        )
        .unwrap()
    })
    .expect("SSO credential add failed");

    let credentials = api::get_anchor_info(&env, canister_id, test_principal, identity_number)?
        .openid_credentials
        .expect("Could not fetch credentials!");
    assert_eq!(credentials.len(), 1);
    assert_eq!(credentials[0].sso_domain, Some(SSO_DOMAIN.to_string()));
    assert_eq!(credentials[0].sso_name, Some("Example".to_string()));

    Ok(())
}

/// Regression test for the boundary canonicalization: a mixed-case
/// `discovery_domain` (the allowlist gate is case-insensitive) must be stored as
/// the canonical lowercase `sso_domain`, so the `sso:<domain>` scope is stable.
#[test]
fn sso_discovery_domain_is_canonicalized() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_sso_canister(&env);
    let responses = sso_http_responses();
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);
    sync_time(&env, test_time);

    // Caller supplies a non-canonical (mixed-case, padded) domain.
    drive_sso_until_ready(&env, &responses, || {
        api::openid_credential_add_with_discovery(
            &env,
            canister_id,
            test_principal,
            identity_number,
            &jwt,
            &salt,
            Some("  Example.ORG  "),
        )
        .unwrap()
    })
    .expect("SSO credential add failed");

    let credentials = api::get_anchor_info(&env, canister_id, test_principal, identity_number)?
        .openid_credentials
        .expect("Could not fetch credentials!");
    assert_eq!(credentials.len(), 1);
    // Canonicalized at the boundary despite the mixed-case/padded input.
    assert_eq!(credentials[0].sso_domain, Some(SSO_DOMAIN.to_string()));

    Ok(())
}

/// A JWT delegation can be prepared and fetched through the SSO path: the update
/// (`openid_prepare_delegation`) warms the caches via `Pending`, then the query
/// (`openid_get_delegation`) reads the warm caches and returns the delegation.
#[test]
fn can_get_sso_delegation_via_discovery() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_sso_canister(&env);
    let responses = sso_http_responses();
    let (jwt, salt, _claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

    let identity_number = create_identity_with_authn_method(&env, canister_id, &test_authn_method);
    sync_time(&env, test_time);

    // Link the SSO credential first (also warms the caches).
    drive_sso_until_ready(&env, &responses, || {
        api::openid_credential_add_with_discovery(
            &env,
            canister_id,
            test_principal,
            identity_number,
            &jwt,
            &salt,
            Some(SSO_DOMAIN),
        )
        .unwrap()
    })
    .expect("SSO credential add failed");

    let pub_session_key = ByteBuf::from("session public key");

    let prepare_response = drive_sso_until_ready(&env, &responses, || {
        api::openid_prepare_delegation_with_discovery(
            &env,
            canister_id,
            test_principal,
            &jwt,
            &salt,
            &pub_session_key,
            Some(SSO_DOMAIN),
        )
        .unwrap()
    })
    .expect("SSO prepare delegation failed");

    // The query path can't drive outcalls, but the caches are warm now.
    let signed_delegation = match api::openid_get_delegation_with_discovery(
        &env,
        canister_id,
        test_principal,
        &jwt,
        &salt,
        &pub_session_key,
        &prepare_response.expiration,
        Some(SSO_DOMAIN),
    )? {
        OpenIdResult::Ok(signed_delegation) => signed_delegation,
        other => panic!("expected a signed delegation, got {other:?}"),
    };

    assert_eq!(
        signed_delegation.delegation.pubkey, pub_session_key,
        "delegation should be bound to the requested session key"
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
    #[allow(unused_variables)]
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

/// Verifies that the `sso_credential_migration` upgrade arg backfills the
/// `sso_domain` / `sso_name` fields of stored credentials via the batched
/// timer migration (see `docs/ongoing/openid-sso-prod-readiness.md` §8.6).
#[test]
fn should_backfill_sso_fields_via_credential_migration() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = setup_canister(&env);
    let (jwt, salt, claims, test_time, test_principal, test_authn_method) =
        openid_google_test_data();

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

    // The credential was written by a direct provider, so it carries no SSO
    // stamp — the exact shape pre-migration SSO credentials are stored in.
    let credentials = api::get_anchor_info(&env, canister_id, test_principal, identity_number)?
        .openid_credentials
        .expect("Could not fetch credentials!");
    assert_eq!(credentials[0].sso_domain, None);
    assert_eq!(credentials[0].sso_name, None);

    // Upgrade with a migration entry matching the stored credential's
    // `(iss, aud)`. The stamped values `acme.com` / `Acme Corp` exist ONLY
    // in this arg: no discovery task is ever registered in this test, so the
    // response-shaping fallback (`sso_fields_for`, which reads
    // `DISCOVERY_TASKS`) can only ever yield `(None, None)` here. The
    // post-upgrade assertions below therefore prove the values came from the
    // stored stamp written by the migration, not from the live fallback.
    let arg = InternetIdentityInit {
        sso_credential_migration: Some(vec![SsoCredentialMigrationEntry {
            discovery_domain: "acme.com".into(),
            issuer: claims.iss.clone(),
            client_id: claims.aud.clone(),
            name: Some("Acme Corp".into()),
        }]),
        ..Default::default()
    };
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(arg))
        .expect("Failed to upgrade canister");

    // Let the interval timer drive the batch migration to completion.
    for _ in 0..5 {
        env.advance_time(Duration::from_secs(1));
        env.tick();
    }

    let credentials = api::get_anchor_info(&env, canister_id, test_principal, identity_number)?
        .openid_credentials
        .expect("Could not fetch credentials!");
    assert_eq!(credentials[0].sso_domain, Some("acme.com".to_string()));
    assert_eq!(credentials[0].sso_name, Some("Acme Corp".to_string()));

    Ok(())
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
    pub iss: String,
    pub sub: String,
    pub aud: String,
    pub nonce: String,
    pub iat: u64,
    // Optional Google specific claims
    pub email: Option<String>,
    pub name: Option<String>,
    pub picture: Option<String>,
}

impl Claims {
    fn key(&self) -> OpenIdCredentialKey {
        (self.iss.clone(), self.sub.clone(), self.aud.clone())
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
            email_verification: None,
            seed_jwks: None,
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
            email_verification: None,
            seed_jwks: None,
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

/// Installs II with a single Google provider whose JWK cache is seeded — via
/// `OpenIdConfig.seed_jwks` — with *two* keys served by
/// [`mock_google_certs_response`]: a decoy (kid
/// `25f8211713788b6145474b5029b0141bd5b3de9c`) and the key that signed the JWT
/// returned by [`openid_google_test_data`] (kid
/// `763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b`). Seeding more than one key
/// exercises the multi-JWK seed path and the by-`kid` match during
/// verification.
///
/// Each JWK is expressed exactly as the API expects it: the list of that JWK's
/// JSON `(field, value)` pairs. Unlike [`setup_canister`], this does NOT mock
/// the certs HTTP response, so a successful link proves verification used the
/// seeded cache.
pub fn setup_canister_with_seeded_google_jwks(env: &PocketIc) -> Principal {
    // Turn a JWK JSON object into the `(field, value)` pair list `seed_jwks`
    // expects for a single key.
    let jwk_pairs = |jwk_json: &str| -> Vec<(String, String)> {
        serde_json::from_str::<serde_json::Value>(jwk_json)
            .unwrap()
            .as_object()
            .unwrap()
            .iter()
            .map(|(field, value)| (field.clone(), value.as_str().unwrap().to_string()))
            .collect()
    };
    // A decoy key (not used to sign the JWT) plus the actual signing key.
    let decoy_jwk_json = r#"{"kty":"RSA","use":"sig","alg":"RS256","kid":"25f8211713788b6145474b5029b0141bd5b3de9c","n":"0qTcwnqUqJqsyu57JAC4IOAgTuMrccabAKKj5T93F68NoCk4kAax0oJhDArisYpiLrQ__YJJ9HFm3TKkuiPZeb1xqSSXAnIZVo8UigTLQDQLCTq3O-aD5EyQTOhOHWxJBZcpyLO-dZVuOIbv8fNMcXpNCioHVHO04gI_mvaw8ZzbU_j8ZeHSPk4wTBNfmH4l0mYRDhoQHLkZxxvc2V71ppBPYbnX-4t6h7XcuTkLJKBxfrR43G5nNzDuFsIbBnS2fjVLEv_1LYj9G5Q5XwiCFS0BON-oqQNzRWF53nkf91bMm2TaROg21KKJbZqfEjUhCVlMDFmBW-MNv69-C19PZQ","e":"AQAB"}"#;
    let signing_jwk_json = r#"{"kty":"RSA","use":"sig","alg":"RS256","kid":"763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b","n":"y8TPCPz2Fp0OhBxsxu6d_7erT9f9XJ7mx7ZJPkkeZRxhdnKtg327D4IGYsC4fLAfpkC8qN58sZGkwRTNs-i7yaoD5_8nupq1tPYvnt38ddVghG9vws-2MvxfPQ9m2uxBEdRHmels8prEYGCH6oFKcuWVsNOt4l_OPoJRl4uiuiwd6trZik2GqDD_M6bn21_w6AD_jmbzN4mh8Od4vkA1Z9lKb3Qesksxdog-LWHsljN8ieiz1NhbG7M-GsIlzu-typJfud3tSJ1QHb-E_dEfoZ1iYK7pMcojb5ylMkaCj5QySRdJESq9ngqVRDjF4nX8DK5RQUS7AkrpHiwqyW0Csw","e":"AQAB"}"#;
    let seed_jwks = vec![jwk_pairs(decoy_jwk_json), jwk_pairs(signing_jwk_json)];

    let args = InternetIdentityInit {
        openid_configs: Some(vec![OpenIdConfig {
            name: "Google".into(),
            logo: String::new(),
            issuer: "https://accounts.google.com".into(),
            client_id: "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com"
                .into(),
            jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".into(),
            auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".into(),
            auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
            fedcm_uri: Some("https://accounts.google.com/gsi/fedcm.json".into()),
            email_verification: None,
            seed_jwks: Some(seed_jwks),
        }]),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };

    // Cycles are needed before installation because of the async HTTP outcalls.
    install_ii_canister_with_arg_and_cycles(env, II_WASM.clone(), Some(args), 10_000_000_000_000)
}

pub fn mock_google_certs_response(env: &PocketIc) {
    // This is the URL that the canister will fetch the Google certificates
    let url = "https://www.googleapis.com/oauth2/v3/certs";
    // These are the certificates at the time of the related Google JWT mocked data was created.
    let mock_certs = r#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"25f8211713788b6145474b5029b0141bd5b3de9c","n":"0qTcwnqUqJqsyu57JAC4IOAgTuMrccabAKKj5T93F68NoCk4kAax0oJhDArisYpiLrQ__YJJ9HFm3TKkuiPZeb1xqSSXAnIZVo8UigTLQDQLCTq3O-aD5EyQTOhOHWxJBZcpyLO-dZVuOIbv8fNMcXpNCioHVHO04gI_mvaw8ZzbU_j8ZeHSPk4wTBNfmH4l0mYRDhoQHLkZxxvc2V71ppBPYbnX-4t6h7XcuTkLJKBxfrR43G5nNzDuFsIbBnS2fjVLEv_1LYj9G5Q5XwiCFS0BON-oqQNzRWF53nkf91bMm2TaROg21KKJbZqfEjUhCVlMDFmBW-MNv69-C19PZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"5d12ab782cb6096285f69e48aea99079bb59cb86","n":"uac7NRcojCutcceWq1nrpLGJjQ7ywvgWsUcb1DWMKJ3KNNHiRzh9jshoi9tmq1zlarJ_h7GQg8iU1qD7SgpVYJmjlKG1MNVRAtuNrNMC0UAnNfG7mBBNorHFndfp-9cLTiMjXSXRzhNqiMvTVKeolRdMB2lH9RzJnwlpXtvUbD7M1pXOlPlMaOy1zxUnHn0uszU5mPRQk79i03BNrAdhwrAUB-ZuMnqpjaUcb9VU3KIwuZNPtsVenLN12sRYpaZ6WBw8Q9q7fAoaJUovM0Go8deC9pJYyxJuHdVo9HP0osyzg3g_rOYi14wmvMBuiDf3F4pTnudAfFyl3d0Mn_i4ZQ","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"763f7c4cd26a1eb2b1b39a88f4434d1f4d9a368b","n":"y8TPCPz2Fp0OhBxsxu6d_7erT9f9XJ7mx7ZJPkkeZRxhdnKtg327D4IGYsC4fLAfpkC8qN58sZGkwRTNs-i7yaoD5_8nupq1tPYvnt38ddVghG9vws-2MvxfPQ9m2uxBEdRHmels8prEYGCH6oFKcuWVsNOt4l_OPoJRl4uiuiwd6trZik2GqDD_M6bn21_w6AD_jmbzN4mh8Od4vkA1Z9lKb3Qesksxdog-LWHsljN8ieiz1NhbG7M-GsIlzu-typJfud3tSJ1QHb-E_dEfoZ1iYK7pMcojb5ylMkaCj5QySRdJESq9ngqVRDjF4nX8DK5RQUS7AkrpHiwqyW0Csw","e":"AQAB"}]}"#;
    mock_certs_response(env, url, mock_certs);
}

pub fn mock_microsoft_certs_response(env: &PocketIc) {
    // This is the URL that the canister will fetch the Microsoft certificates
    let url = "https://login.microsoftonline.com/common/discovery/v2.0/keys";
    // These are the certificates at the time of the related Microsoft JWT mocked data was created.
    let mock_certs = r#"{"keys":[{"kty":"RSA","use":"sig","kid":"PoVKeirIOvmTyLQ9G9BenBwos7k","x5t":"PoVKeirIOvmTyLQ9G9BenBwos7k","n":"ruYyUq1ElSb8QCCt0XWWRSFpUq0JkyfEvvlCa4fPDi0GZbSGgJg3qYa0co2RsBIYHczXkc71kHVpktySAgYK1KMK264e-s7Vymeq-ypHEDpRsaWric_kKEIvKZzRsyUBUWf0CUhtuUvAbDTuaFnQ4g5lfoa7u3vtsv1za5Gmn6DUPirrL_-xqijP9IsHGUKaTmB4M_qnAu6vUHCpXZnN0YTJDoK7XrVJFaKj8RrTdJB89GFJeTFHA2OX472ToyLdCDn5UatYwmht62nXGlH7_G1kW1YMpeSSwzpnMEzUUk7A8UXrvFTHXEpfXhsv0LA59dm9Hi1mIXaOe1w-icA_rQ","e":"AQAB","x5c":["MIIC/jCCAeagAwIBAgIJAM52mWWK+FEeMA0GCSqGSIb3DQEBCwUAMC0xKzApBgNVBAMTImFjY291bnRzLmFjY2Vzc2NvbnRyb2wud2luZG93cy5uZXQwHhcNMjUwMzIwMDAwNTAyWhcNMzAwMzIwMDAwNTAyWjAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAruYyUq1ElSb8QCCt0XWWRSFpUq0JkyfEvvlCa4fPDi0GZbSGgJg3qYa0co2RsBIYHczXkc71kHVpktySAgYK1KMK264e+s7Vymeq+ypHEDpRsaWric/kKEIvKZzRsyUBUWf0CUhtuUvAbDTuaFnQ4g5lfoa7u3vtsv1za5Gmn6DUPirrL/+xqijP9IsHGUKaTmB4M/qnAu6vUHCpXZnN0YTJDoK7XrVJFaKj8RrTdJB89GFJeTFHA2OX472ToyLdCDn5UatYwmht62nXGlH7/G1kW1YMpeSSwzpnMEzUUk7A8UXrvFTHXEpfXhsv0LA59dm9Hi1mIXaOe1w+icA/rQIDAQABoyEwHzAdBgNVHQ4EFgQUcZ2MLLOas+d9WbkFSnPdxag09YIwDQYJKoZIhvcNAQELBQADggEBABPXBmwv703IlW8Zc9Kj7W215+vyM5lrJjUubnl+s8vQVXvyN7bh5xP2hzEKWb+u5g/brSIKX/A7qP3m/z6C8R9GvP5WRtF2w1CAxYZ9TWTzTS1La78edME546QejjveC1gX9qcLbEwuLAbYpau2r3vlIqgyXo+8WLXA0neGIRa2JWTNy8FJo0wnUttGJz9LQE4L37nR3HWIxflmOVgbaeyeaj2VbzUE7MIHIkK1bqye2OiKU82w1QWLV/YCny0xdLipE1g2uNL8QVob8fTU2zowd2j54c1YTBDy/hTsxpXfCFutKwtELqWzYxKTqYfrRCc1h0V4DGLKzIjtggTC+CY="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"JYhAcTPMZ_LX6DBlOWQ7Hn0NeXE","x5t":"JYhAcTPMZ_LX6DBlOWQ7Hn0NeXE","n":"otxHCrbdDCUhPHT05zemCxen_h3vbWY9BxgH5yL4HPhAfj8xFrn-B5vgySJKUj4yprzwkT-EfcCfpMNDgxvVHyzMzWQWk_XcYCA72Whkt4kZnEzk-oxeasJ2rJ7NIpWMdQbamfw4BT8GYyplE1Pd2oiBGTeW4qxJow5qVmu_xwqpdq0MiViNQMQgLmy1FElDweue2Hsr9PUF-bp1dFPSp1m1kqvpMBsLZd4-2cc-k3gl12PB0O3GbswwFdoKf9iN0mJ_0GdRRQiXm-BXpnOPNiTcYrLiz2wWCpYQrLmxpJhm3oqLPGWrQtyVeO8vUeLI0fW4wpjYCQK_KPEi1x7T2Q","e":"AQAB","x5c":["MIIC/jCCAeagAwIBAgIJAM5KoNE1LHYtMA0GCSqGSIb3DQEBCwUAMC0xKzApBgNVBAMTImFjY291bnRzLmFjY2Vzc2NvbnRyb2wud2luZG93cy5uZXQwHhcNMjUwNzA3MTQzMjA3WhcNMzAwNzA3MTQzMjA3WjAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAotxHCrbdDCUhPHT05zemCxen/h3vbWY9BxgH5yL4HPhAfj8xFrn+B5vgySJKUj4yprzwkT+EfcCfpMNDgxvVHyzMzWQWk/XcYCA72Whkt4kZnEzk+oxeasJ2rJ7NIpWMdQbamfw4BT8GYyplE1Pd2oiBGTeW4qxJow5qVmu/xwqpdq0MiViNQMQgLmy1FElDweue2Hsr9PUF+bp1dFPSp1m1kqvpMBsLZd4+2cc+k3gl12PB0O3GbswwFdoKf9iN0mJ/0GdRRQiXm+BXpnOPNiTcYrLiz2wWCpYQrLmxpJhm3oqLPGWrQtyVeO8vUeLI0fW4wpjYCQK/KPEi1x7T2QIDAQABoyEwHzAdBgNVHQ4EFgQUbmZcQgdPC+xHVw/WrRHiajm/FNIwDQYJKoZIhvcNAQELBQADggEBACVhPf9gP/hr7lLx8OCaGaAVEEQ+DGqMuSzHJTSgcGXwKsVbsHO7Ih39ERM+ZoUZoawARETLw7c0UW0iiBZsdBnjVhzaEHha/9tIXOs9THAqwNY6hhN4tYY9MOB4U5gBL7SaQnwelHsY6oQSavK/lA25e/t0Uxz2BOEWYy6/59n3Tu8AMRySz6Q1YnVO6Ww5pNBTjHV+8kFbsj3ln5C4rAoTBJwHSYdh6Yz9OZB7UGhVCzkYQ7FO5xJ1WW5FPezVl4Osnr/JhjwvAidcq4dKO7OGHTIaDa72IcJ9IFsPdCxaCIspz4A1zEX6C+Z8bONccUs5xeAeGwaZypw9XJW341M="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"wf9rj3EkQvzKpxDG-mJn1gALi6U","x5t":"wf9rj3EkQvzKpxDG-mJn1gALi6U","n":"n_GLtQeV5wmNp7Zs0KC3SDOYaSEGLdo7MuMfNKQsrQ8tFaOysX-K4JmKjzL0qildP0xVKGJdLY70l1vLhqkxzlY0o0CssWzSMs5XCmNlxGUquaeN_F5f_zxvp_FgKH69ESnOxCAk6AbwTxww5pg1hAJGleyIN17PazVSBBCo4VSeWEYUL6S60N8i2xNb9Udxv1u8apEr9SKjrjR8oTrhfblySXMhKHFpvrHVlFWpDBojOKBZFs9IRFbsurm-LpNrw2ZAx2UJgYjf5_w-vLu3YfeYhWc65AW1hZeMs7Se33a2O6yelGe2wur2PRkpoE8_zoIB59CAO7bPEBSy27i4gQ","e":"AQAB","x5c":["MIIC6jCCAdKgAwIBAgIJAJEe9FiqEcETMA0GCSqGSIb3DQEBCwUAMCMxITAfBgNVBAMTGGxvZ2luLm1pY3Jvc29mdG9ubGluZS51czAeFw0yNTA4MDMxNjAxMzlaFw0zMDA4MDMxNjAxMzlaMCMxITAfBgNVBAMTGGxvZ2luLm1pY3Jvc29mdG9ubGluZS51czCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAJ/xi7UHlecJjae2bNCgt0gzmGkhBi3aOzLjHzSkLK0PLRWjsrF/iuCZio8y9KopXT9MVShiXS2O9Jdby4apMc5WNKNArLFs0jLOVwpjZcRlKrmnjfxeX/88b6fxYCh+vREpzsQgJOgG8E8cMOaYNYQCRpXsiDdez2s1UgQQqOFUnlhGFC+kutDfItsTW/VHcb9bvGqRK/Uio640fKE64X25cklzIShxab6x1ZRVqQwaIzigWRbPSERW7Lq5vi6Ta8NmQMdlCYGI3+f8Pry7t2H3mIVnOuQFtYWXjLO0nt92tjusnpRntsLq9j0ZKaBPP86CAefQgDu2zxAUstu4uIECAwEAAaMhMB8wHQYDVR0OBBYEFG/tpMvaA2NenbVzVfX1Ufs7sYE4MA0GCSqGSIb3DQEBCwUAA4IBAQBvLzLR9G8JaVfAybdsd0br+DPwMM3Zj33dG4/koqcSXH2PQuECRNMsap9ZDVq7Z63yeJhoVELI51LP67sZdoKm7L1P+mfpa1d4tCvMxaQRT9HlfzUzqr7xLbsOJa2+Nuzm5LpGZ0xVKP2ohMqSjZEZeMG7sVbMKB8qyJgT0VuXexRQyPqCeBrlbq6MomljX4t+tBM6FlZ0bLSX6qzM25XZc9xzL8tshNp1zB3FNksQmIRyRRom8rAjE4L/e6K1Sqrzs4zfQUorJCM/fhw9jh5ZHfdlnOvQrP2CU4NUC1UN1b9OqqVyLYtNc4e9M7y108RuKWs4xrxr5Rv+Jf/3lnwm"],"cloud_instance_name":"microsoftonline.us","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"dwd9Kc_5AQRdu0kNDRn5H5_ejaM","x5t":"dwd9Kc_5AQRdu0kNDRn5H5_ejaM","n":"iZWr4oIvi03jYIIv-ydKdHVZBAVCfv5_vjJDKyFEwWXCyymPYic1CSulPxofEBpbYXG5HKpOPJX9tk9iRt-ydw-co97lg02f1HJJdWqxU2o3vJBbFDvfG9ek1a30OUeYVmV0wyMK72khJL1iE7gUPztjnlDR8GyyzufYrEfufNG1tYRnKagEEEZ0mq-pnN9RVaZ7znXoI2H0bQy9GIj5KdwSRF3dT8YFEgolPgKE4mUV6RY0iLeOf3SRrfmAVokHp9VR91XJgxoit2ixqfRL7groKMLD5OxZtxEvxbMR2zMG0xcQtxBNjGAeHuGI7gCxWNFYNJB5IrCkNjOkmmx2iw","e":"AQAB","x5c":["MIIC6TCCAdGgAwIBAgIIWm3FLoFNqXQwDQYJKoZIhvcNAQELBQAwIzEhMB8GA1UEAxMYbG9naW4ubWljcm9zb2Z0b25saW5lLnVzMB4XDTI1MDgxOTE2MDExMloXDTMwMDgxOTE2MDExMlowIzEhMB8GA1UEAxMYbG9naW4ubWljcm9zb2Z0b25saW5lLnVzMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAiZWr4oIvi03jYIIv+ydKdHVZBAVCfv5/vjJDKyFEwWXCyymPYic1CSulPxofEBpbYXG5HKpOPJX9tk9iRt+ydw+co97lg02f1HJJdWqxU2o3vJBbFDvfG9ek1a30OUeYVmV0wyMK72khJL1iE7gUPztjnlDR8GyyzufYrEfufNG1tYRnKagEEEZ0mq+pnN9RVaZ7znXoI2H0bQy9GIj5KdwSRF3dT8YFEgolPgKE4mUV6RY0iLeOf3SRrfmAVokHp9VR91XJgxoit2ixqfRL7groKMLD5OxZtxEvxbMR2zMG0xcQtxBNjGAeHuGI7gCxWNFYNJB5IrCkNjOkmmx2iwIDAQABoyEwHzAdBgNVHQ4EFgQUDGVTWHZoF6G+adWAE3khY05HkKYwDQYJKoZIhvcNAQELBQADggEBADiPcDlVjZqXT8kX7MzT3rzYNM5bLwn25T9KQQvjY3tQp3Ref7skaDaUxR0VP51SFAKVED+vAexE6p3zBnl1Aur8gv1DIayjzp1ZwovRHRNykikindIqg7JXTz6a+jDOPcvpIWjM034X1kG92aQzo2L1FkE2hG7KEJybNWz9WJiC0IBMgFpMl6S0gn5gzYrWVwv4NP6PMPA/5IJdK0/XDlXTZuf2BnTKzy9c0obwC6PWM2m2WhsUW6AkCfQ/+10PUvvKwGOCNcP1Du9ejBEI0deer4/0grMPosz0+lIWEyj9O0NF15FPhfN9BO+6RM+9zLZUiY8pNIdkwwraYAPl0pU="],"cloud_instance_name":"microsoftonline.us","issuer":"https://login.microsoftonline.com/{tenantid}/v2.0"},{"kty":"RSA","use":"sig","kid":"4pxhphrFjooBXV6t0JO_kpTQNl8","x5t":"4pxhphrFjooBXV6t0JO_kpTQNl8","n":"u2CavgKbig1Xt4gjxxQ3pb0iSaDw8_UoczFMicTtlKhRVjJcFclU5vZUuJYyIhE2CRZmrrza5k_-iaiebll1-mU7tmfN4iEg3FZoM2yS_ZiVJ2XpCB6ovz-jiPSYiy51az51mXztCBEAr6UPX6oOMppimgROt0dRps9SaOw9AFWfonj3BHLyFeON5bFu_JJ9stj0u-xQyP5s52ivNEjbGyYb3aX0NmwgEp7u38FT1zX4OK4hCX95-xNQwAjdyd0pTwqHR3hCfNqMGBfk4BlMb4kkk9oJk162qNt_o4jtQe5CGu7PDNh3L7FbSq4vFA5gZrSxPeLHwT8Y9ZGI9y3J5Q","e":"AQAB","x5c":["MIIDCjCCAfKgAwIBAgIQLIBnPFTz2C/4hjdwzi+JWzANBgkqhkiG9w0BAQsFADApMScwJQYDVQQDEx5MaXZlIElEIFNUUyBTaWduaW5nIFB1YmxpYyBLZXkwHhcNMjUwMzMwMTIwNDE0WhcNMzAwMzMwMTIwNDE0WjApMScwJQYDVQQDEx5MaXZlIElEIFNUUyBTaWduaW5nIFB1YmxpYyBLZXkwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC7YJq+ApuKDVe3iCPHFDelvSJJoPDz9ShzMUyJxO2UqFFWMlwVyVTm9lS4ljIiETYJFmauvNrmT/6JqJ5uWXX6ZTu2Z83iISDcVmgzbJL9mJUnZekIHqi/P6OI9JiLLnVrPnWZfO0IEQCvpQ9fqg4ymmKaBE63R1Gmz1Jo7D0AVZ+iePcEcvIV443lsW78kn2y2PS77FDI/mznaK80SNsbJhvdpfQ2bCASnu7fwVPXNfg4riEJf3n7E1DACN3J3SlPCodHeEJ82owYF+TgGUxviSST2gmTXrao23+jiO1B7kIa7s8M2HcvsVtKri8UDmBmtLE94sfBPxj1kYj3LcnlAgMBAAGjLjAsMB0GA1UdDgQWBBQSRzxsStOrWpoFyqOTStyfqK5uEzALBgNVHQ8EBAMCAsQwDQYJKoZIhvcNAQELBQADggEBALEzA/iWpeaVXX1f1FUghknhPk3/3dMJBlYyaq/cWCHT5XMDkCwc+KWjH98pNs0obOcXSsw0IaFzhVaRBm39JsG1IFhUISs1jHRbLc40cof5FAoX9q8L19CBZS3XSUC9NT2aFOzAGBCAHur9Lu/b6TcHVzpUKPL1tmM3wK9irOWCyOO5nVzetGqqgFaN06B5OQ/tVILvSAC3X0rgJ++oixuLjx9ReMSh5V2tW1lvOxlLWeIoYDltj54g+SIE1F9x6C9tXZORL4+mcHr9tQWQBUehPui4xosNbClW1FZtViQVD4CYVLW2i1P+ddotX7JZts+qU2VHUMaZgBlblM4PYQo="],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"},{"kty":"RSA","use":"sig","kid":"dd55f7QP3HZky-ekQBDWKe7ADN0","x5t":"dd55f7QP3HZky-ekQBDWKe7ADN0","n":"s950MOniBjjiB82Zx5FehPuYJKtguUAP5vpMNZ6JxYLGzn5h3FPplx0kkPEezg7dnH-sFxGwq_C7P14ja0YmHUkmozqdbg4GRS8xp9Ue0-KWs4BTTkXvMPnc3xBU6be4SmHK-c7A0E54YfokiY1KTRCaq_RRPDlVZJuc6s6rK4nRoPo0eOCj7H4b--QnKmPXiFSPNp40NC7FRfx9m7SYSltubIDzq1f4j2Qlrv90f2kT5DP81_2DGdHZ_ao7ypPX2_z_m4ycQgfAhIYhNHRai6iGtbU6NPkw38LS29bVbGycVCaBoq6_re8k_UiOM7gP9l96QDFnkMmNhpRxT7t34w","e":"AQAB","x5c":["MIIDCzCCAfOgAwIBAgIRAMTs5DVuj/oHplagHKvwLhswDQYJKoZIhvcNAQELBQAwKTEnMCUGA1UEAxMeTGl2ZSBJRCBTVFMgU2lnbmluZyBQdWJsaWMgS2V5MB4XDTI1MDMxNzE1MzEwMloXDTMwMDMxNzE1MzEwMlowKTEnMCUGA1UEAxMeTGl2ZSBJRCBTVFMgU2lnbmluZyBQdWJsaWMgS2V5MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAs950MOniBjjiB82Zx5FehPuYJKtguUAP5vpMNZ6JxYLGzn5h3FPplx0kkPEezg7dnH+sFxGwq/C7P14ja0YmHUkmozqdbg4GRS8xp9Ue0+KWs4BTTkXvMPnc3xBU6be4SmHK+c7A0E54YfokiY1KTRCaq/RRPDlVZJuc6s6rK4nRoPo0eOCj7H4b++QnKmPXiFSPNp40NC7FRfx9m7SYSltubIDzq1f4j2Qlrv90f2kT5DP81/2DGdHZ/ao7ypPX2/z/m4ycQgfAhIYhNHRai6iGtbU6NPkw38LS29bVbGycVCaBoq6/re8k/UiOM7gP9l96QDFnkMmNhpRxT7t34wIDAQABoy4wLDAdBgNVHQ4EFgQUFPr+ao1bn0+7HqNjit54vxkyDnMwCwYDVR0PBAQDAgLEMA0GCSqGSIb3DQEBCwUAA4IBAQApk5hLeFI7bGJiIv6P+Ct74+mGdDYycRJAGbW/ihurk/NlDZWQ095ik3TGprTQ/4C+7ks8qtQOyVLhC2qR6ESZsBhgwWr7dxAHH2Q1DUBZN51N6xYo4adRWi6o0tNLZr7ZVEglV5e03Smu/rC9E04hocHpYiWKcZ9bpsVjK6Q1gcWcWnw1iAAZpGVD2xkpoFvssn0xiz0fCO0b6JT9l6YWeKJgBMz9LIFGoMaY1iREkCQryw6YGjdhEsSRUDvsDg5ICW4pJT9FOOthbsRm6H3reCa768AFImWaIESL3pqOXOppGDPL7dKY0Tt2uy0iQIRl0TkGHgrzoSa3q3Sl6VfT"],"cloud_instance_name":"microsoftonline.com","issuer":"https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"}]}"#;
    mock_certs_response(env, url, mock_certs);
}

pub fn mock_certs_response(env: &PocketIc, url: &str, mock_certs: &str) {
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

pub fn one_openid_microsoft_test_data(
) -> (String, [u8; 32], Claims, u64, Principal, AuthnMethodData) {
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

// ===========================================================================
// IdP-side per-app SSO gating
// (docs/ongoing/enterprise-sso-idp-side-gating.md)
//
// These drive the new `sso_prepare_delegation` / `sso_get_delegation` gate path
// end to end against a mocked well-known that declares `app_clients` /
// `gate_all_apps` / `stable_identifier_claim`. Tokens are signed with the
// shared fake-JWT builder so their `aud` can be set to a per-app client.
// ===========================================================================
mod sso_gating {
    use super::*;
    use crate::attributes::{build_fake_google_jwt_and_jwks, FakeJwtInput};
    use canister_tests::api::internet_identity::api_v2::{
        check_captcha, identity_registration_start, openid_identity_registration_finish,
        prepare_account_delegation, AccountDelegationParams,
    };
    use internet_identity_interface::internet_identity::types::attributes::{
        AttributeSpec, PrepareIcrc3AttributeError, PrepareIcrc3AttributeRequest,
    };
    use internet_identity_interface::internet_identity::types::{
        AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
        AuthnMethodSecuritySettings, IdRegFinishError, OpenIDRegFinishArg, PublicKeyAuthn,
        RegistrationFlowNextStep, SsoDiscoveryState,
    };

    const GATE_DOMAIN: &str = "gate.example.org";
    const GATE_ISSUER: &str = "https://idp.gate.example.org";
    const PRIMARY_CLIENT: &str = "primary-client-id";
    const PER_APP_CLIENT: &str = "per-app-client-id";
    const GATED_ORIGIN: &str = "https://payroll.example";
    const UNGATED_ORIGIN: &str = "https://public.example";
    const PRIMARY_SUB: &str = "sso-primary-sub-0001";
    /// Entra-style pairwise (per-client) sub for the gated login: differs from
    /// the primary credential's sub, bridged via the `oid` aux lookup (§6.5).
    const PER_APP_SUB: &str = "entra-pairwise-sub-0002";
    const STABLE_OID: &str = "entra-oid-stable-0003";
    const TEST_TIME_MS: u64 = 1_800_000_000_000;

    fn test_salt() -> [u8; 32] {
        [42u8; 32]
    }

    // Deterministic caller: the fake JWT binds its nonce to SHA256(salt ||
    // principal), so we reuse a fixed pubkey/principal (from the attributes
    // integration tests) as both the anchor's passkey and the JWT caller.
    fn test_pubkey() -> Vec<u8> {
        vec![
            48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38,
            32, 1, 33, 88, 32, 252, 182, 240, 218, 160, 61, 178, 176, 17, 228, 185, 84, 148, 45,
            86, 216, 171, 120, 72, 246, 212, 55, 212, 167, 142, 59, 227, 0, 242, 182, 129, 211, 34,
            88, 32, 158, 197, 96, 131, 51, 156, 176, 65, 128, 29, 75, 98, 163, 187, 104, 38, 255,
            65, 92, 234, 229, 245, 221, 74, 40, 202, 29, 83, 162, 84, 177, 204,
        ]
    }

    fn test_principal() -> Principal {
        Principal::from_slice(&[
            211, 40, 186, 145, 43, 2, 6, 17, 232, 23, 22, 44, 51, 178, 233, 163, 131, 231, 82, 174,
            66, 201, 203, 1, 102, 109, 20, 75, 2,
        ])
    }

    fn test_authn_method() -> AuthnMethodData {
        AuthnMethodData {
            authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
                pubkey: ByteBuf::from(test_pubkey()),
            }),
            metadata: Default::default(),
            security_settings: AuthnMethodSecuritySettings {
                protection: AuthnMethodProtection::Unprotected,
                purpose: AuthnMethodPurpose::Authentication,
            },
            last_authentication: None,
        }
    }

    /// Build a JWT (and matching JWKS) for the gate issuer with the given `aud` /
    /// `sub` and optional extra claims (e.g. `oid`).
    fn token(aud: &str, sub: &str, extra_claims: &[(&str, &str)]) -> (String, String) {
        let iat = (TEST_TIME_MS / 1_000) - 300;
        build_fake_google_jwt_and_jwks(FakeJwtInput {
            salt: &test_salt(),
            principal: &test_principal(),
            issuer: GATE_ISSUER,
            aud,
            sub,
            email: "user@gate.example.org",
            name: "Gate User",
            email_verified: true,
            iat_secs: iat,
            exp_secs: iat + 3_600,
            extra_claims,
        })
    }

    /// Assemble the well-known (`app_clients` map JSON, gate flag, stable claim).
    fn well_known(app_clients_json: &str, gate_all_apps: bool, stable_claim: &str) -> String {
        format!(
            r#"{{"client_id":"{PRIMARY_CLIENT}","openid_configuration":"{GATE_ISSUER}/.well-known/openid-configuration","name":"Gate","app_clients":{app_clients_json},"gate_all_apps":{gate_all_apps},"stable_identifier_claim":"{stable_claim}"}}"#
        )
    }

    /// The three mocked outcalls (hop-1 well-known, hop-2 OIDC discovery, JWKS).
    fn responses(well_known_json: String, jwks_json: String) -> Vec<(String, String)> {
        vec![
            (
                format!("https://{GATE_DOMAIN}/.well-known/ii-openid-configuration"),
                well_known_json,
            ),
            (
                format!("{GATE_ISSUER}/.well-known/openid-configuration"),
                format!(
                    r#"{{"issuer":"{GATE_ISSUER}","jwks_uri":"{GATE_ISSUER}/jwks","authorization_endpoint":"{GATE_ISSUER}/authorize","scopes_supported":["openid","email","profile"]}}"#
                ),
            ),
            (format!("{GATE_ISSUER}/jwks"), jwks_json),
        ]
    }

    fn install(env: &PocketIc) -> Principal {
        let args = InternetIdentityInit {
            sso_discoverable_domains: Some(vec![GATE_DOMAIN.to_string()]),
            canister_creation_cycles_cost: Some(0),
            ..Default::default()
        };
        install_ii_canister_with_arg_and_cycles(
            env,
            II_WASM.clone(),
            Some(args),
            10_000_000_000_000,
        )
    }

    /// Register an anchor with the test passkey and link its **primary** SSO
    /// credential (the identity all per-app clients collapse to). Warms the
    /// discovery cache with `responses`.
    fn register_with_primary_credential(
        env: &PocketIc,
        canister_id: Principal,
        responses: &[(String, String)],
        primary_jwt: &str,
    ) -> u64 {
        let identity_number =
            create_identity_with_authn_method(env, canister_id, &test_authn_method());
        sync_time(env, TEST_TIME_MS);
        drive_sso_until_ready(env, responses, || {
            api::openid_credential_add_with_discovery(
                env,
                canister_id,
                test_principal(),
                identity_number,
                primary_jwt,
                &test_salt(),
                Some(GATE_DOMAIN),
            )
            .unwrap()
        })
        .expect("primary SSO credential add failed");
        // Initialize the canister salt so the synchronous SSO-session seed
        // computation in `prepare_icrc3_attributes` (`matching_sso_session`,
        // §6.3) has a salt to hash against. In production a passkey session has
        // always run a delegation-prepare (which calls `ensure_salt_set`) before
        // reaching the attribute path; here nothing has, so a passkey-first
        // `prepare_icrc3_attributes` would otherwise trap with "Salt is not set".
        api::init_salt(env, canister_id).unwrap();
        identity_number
    }

    fn expect_ready<T, E: std::fmt::Debug>(result: Result<T, E>) -> T {
        result.expect("expected a settled Ok SSO delegation")
    }

    /// Linchpin: whether an origin is gated (per-app client) or ungated (primary
    /// client), the login resolves to the **same anchor**, so the dapp principal
    /// `f(account, origin)` is identical. Also checks the gated SSO session can
    /// mint that origin's account delegation, matching a passkey session's.
    #[test]
    fn gated_and_ungated_resolve_to_same_dapp_principal() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);

        let identity_number =
            register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("dapp session key");

        // Gated login: the per-app token (aud = per-app client) at the gated
        // origin passes the gate and resolves to the primary anchor.
        let (gated_jwt, _) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let gated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(gated.anchor_number, identity_number);

        // Ungated login: the primary token at an unlisted origin resolves to the
        // same anchor via the primary client.
        let ungated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                UNGATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(ungated.anchor_number, identity_number);

        // The dapp principal for the gated origin, minted by the gated SSO
        // session, equals the one a normal (passkey) session mints — proving
        // `f(account, origin)` is independent of the gate.
        let sso_session_principal = Principal::self_authenticating(gated.user_key.as_ref());
        let sso_params = AccountDelegationParams::new(
            &env,
            canister_id,
            sso_session_principal,
            identity_number,
            GATED_ORIGIN.to_string(),
            None,
            session_key.clone(),
        );
        let via_sso = prepare_account_delegation(&sso_params, None)?
            .expect("gated SSO session must mint the account delegation");

        let passkey_params = AccountDelegationParams::new(
            &env,
            canister_id,
            test_principal(),
            identity_number,
            GATED_ORIGIN.to_string(),
            None,
            session_key.clone(),
        );
        let via_passkey = prepare_account_delegation(&passkey_params, None)?
            .expect("passkey session mints the account delegation");

        assert_eq!(
            via_sso.user_key, via_passkey.user_key,
            "same dapp principal f(account, origin) regardless of the login path"
        );

        Ok(())
    }

    /// A token minted for the primary (or any other) client is refused at a
    /// gated origin: the gate is mint-or-refuse, so no SSO delegation exists and
    /// the origin-scoped calls have no principal to authenticate with.
    #[test]
    fn refused_gate_mints_no_delegation() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("dapp session key");

        // Present the PRIMARY token at the gated origin (which requires the
        // per-app client). The gate refuses — verification fails.
        let result = drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        });
        assert!(
            matches!(result, Err(OpenIdDelegationError::JwtVerificationFailed)),
            "gate must refuse a token minted for a different client, got {result:?}"
        );
        Ok(())
    }

    /// A passkey (non-SSO) session gets no `sso:<domain>` attributes, while the
    /// SSO session that signed in through that domain does.
    #[test]
    fn passkey_session_gets_no_sso_attributes() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        let identity_number =
            register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let sso_attr = format!("sso:{GATE_DOMAIN}:email");
        let request = |origin: &str| PrepareIcrc3AttributeRequest {
            identity_number,
            origin: origin.to_string(),
            unmapped_origin: None,
            account_number: None,
            attributes: vec![AttributeSpec {
                key: sso_attr.clone(),
                value: None,
                omit_scope: false,
            }],
            nonce: vec![7u8; 32],
        };

        // Passkey session -> refused (no SSO sign-in for this domain).
        let passkey = api::prepare_icrc3_attributes(
            &env,
            canister_id,
            test_principal(),
            request(GATED_ORIGIN),
        )?;
        assert!(
            matches!(
                passkey,
                Err(PrepareIcrc3AttributeError::AttributeMismatch { .. })
            ),
            "passkey session must not obtain sso:<domain> attributes, got {passkey:?}"
        );

        // SSO session -> the attribute is certified. The session is now the
        // plain credential principal; the SSO context rides in the certified
        // bundle attached as `sender_info` (signer = this canister).
        let session_key = ByteBuf::from("dapp session key");
        let (gated_jwt, _) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let gated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        let sso_session_principal = Principal::self_authenticating(gated.user_key.as_ref());
        let sso = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            sso_session_principal,
            request(GATED_ORIGIN),
            gated.sso_attr_bundle.as_ref(),
            canister_id,
        )?;
        assert!(
            sso.is_ok(),
            "SSO session must certify sso:<domain> attributes, got {sso:?}"
        );

        // Same authorized session, but NO bundle attached (a plain openid
        // session): no `sso:<domain>` attribute is certified (test category 4,
        // bundle-absent).
        let no_bundle = api::prepare_icrc3_attributes(
            &env,
            canister_id,
            sso_session_principal,
            request(GATED_ORIGIN),
        )?;
        assert!(
            matches!(
                no_bundle,
                Err(PrepareIcrc3AttributeError::AttributeMismatch { .. })
            ),
            "a bundle-less openid session must not obtain sso:<domain> attributes, got {no_bundle:?}"
        );
        Ok(())
    }

    /// `gate_all_apps: true` denies an origin absent from `app_clients`.
    #[test]
    fn gate_all_apps_denies_unlisted_origin() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        // Gate everything: only the explicitly listed origin is servable.
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, true, "sub"), jwks);
        register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("dapp session key");
        let result = drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                UNGATED_ORIGIN,
            )
            .unwrap()
        });
        assert!(
            result.is_err(),
            "gate_all_apps must deny an unlisted origin, got {result:?}"
        );
        Ok(())
    }

    /// Entra-style org (`stable_identifier_claim = "oid"`, pairwise `sub`): a
    /// first gated login before any normal login can't resolve (§6.5) and must
    /// prompt a normal sign-in first; after the normal login populates the aux
    /// index, the gated login resolves to the same anchor.
    #[test]
    fn entra_oid_first_gated_needs_normal_then_resolves() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[("oid", STABLE_OID)]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "oid"), jwks);
        let identity_number =
            register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("dapp session key");
        // Pairwise per-app token: its sub differs from the primary credential's.
        let (gated_jwt, _) = token(PER_APP_CLIENT, PER_APP_SUB, &[("oid", STABLE_OID)]);

        // First gated login, before any normal (primary) login populated the
        // aux index: no anchor resolves -> "sign in normally first".
        let first = drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        });
        assert!(
            matches!(first, Err(OpenIdDelegationError::NoSuchAnchor)),
            "first gated login (non-sub) must need a normal login first, got {first:?}"
        );

        // A normal (primary) login carries both the primary sub and the oid,
        // populating the aux index.
        let normal = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                UNGATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(normal.anchor_number, identity_number);

        // Now the gated login bridges (iss, oid) -> primary sub and resolves.
        let resolved = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(
            resolved.anchor_number, identity_number,
            "gated login resolves to the primary anchor after aux is populated"
        );
        Ok(())
    }

    /// The aux bridge is PERSISTED (its own stable memory region), so it survives
    /// a canister upgrade: a non-`sub` (Entra `oid`) user who signed in normally
    /// once keeps resolving gated logins after an upgrade — WITHOUT being sent
    /// through the "sign in normally first" step again (§6.5). The former in-heap
    /// map was wiped on every upgrade, forcing that step ~weekly.
    #[test]
    fn aux_bridge_survives_upgrade() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[("oid", STABLE_OID)]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "oid"), jwks);
        let identity_number =
            register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("dapp session key");
        let (gated_jwt, _) = token(PER_APP_CLIENT, PER_APP_SUB, &[("oid", STABLE_OID)]);

        // A normal (primary) login records the
        // (iss, primary_client, oid) -> primary_sub bridge in stable memory.
        let normal = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                UNGATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(normal.anchor_number, identity_number);

        // Upgrade the canister. The former in-heap bridge would be wiped here; the
        // stable bridge survives. (The replica clock stays where it is — already
        // at/just past `TEST_TIME_MS`, well within the fake JWTs' validity.)
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        // The gated per-app login (pairwise sub) resolves via the PERSISTED bridge
        // — no fresh primary login needed. (The discovery/JWKS caches are cold
        // after the upgrade, so `drive_sso_until_ready` re-warms them; the bridge
        // itself is not re-populated.)
        let resolved = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(
            resolved.anchor_number, identity_number,
            "gated login must resolve via the persisted bridge after an upgrade"
        );
        Ok(())
    }

    /// `get_sso_discovery` reports the per-origin `resolved_client_id`: the
    /// per-app client for a listed origin, the primary for an unlisted one.
    #[test]
    fn get_sso_discovery_reports_resolved_client_id() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let resolved = |origin: Option<&str>| match api::get_sso_discovery_for_origin(
            &env,
            canister_id,
            GATE_DOMAIN,
            origin,
        )
        .unwrap()
        {
            SsoDiscoveryState::Resolved(d) => d,
            other => panic!("expected Resolved, got {other:?}"),
        };

        // Listed (gated) origin -> per-app client.
        assert_eq!(
            resolved(Some(GATED_ORIGIN)).resolved_client_id,
            Some(PER_APP_CLIENT.to_string())
        );
        // Unlisted origin -> primary client.
        assert_eq!(
            resolved(Some(UNGATED_ORIGIN)).resolved_client_id,
            Some(PRIMARY_CLIENT.to_string())
        );
        // No origin -> mirrors the primary client.
        assert_eq!(
            resolved(None).resolved_client_id,
            Some(PRIMARY_CLIENT.to_string())
        );
        Ok(())
    }

    /// Warm the gate's discovery + JWKS caches (driving the mocked outcalls)
    /// WITHOUT creating an anchor, so a following `openid_identity_registration_finish`
    /// — whose test wrapper collapses `Pending` into a panic — settles in one
    /// shot. A gated `sso_prepare_delegation` verifies the token (which reads
    /// both caches, warming them) before the anchor lookup fails, which is
    /// exactly the warming the registration path needs. `warm_jwt`/`warm_origin`
    /// must pass the gate so the JWKS fetch is actually reached (a
    /// `gate_all_apps`-denied origin returns before `read_jwks`).
    fn warm_gate_caches(
        env: &PocketIc,
        canister_id: Principal,
        responses: &[(String, String)],
        warm_jwt: &str,
        warm_origin: &str,
    ) {
        let session_key = ByteBuf::from("cache-warming session key");
        let _ = drive_sso_until_ready(env, responses, || {
            api::sso_prepare_delegation(
                env,
                canister_id,
                test_principal(),
                warm_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                warm_origin,
            )
            .unwrap()
        });
    }

    /// Run the SSO-aware registration finish for `(jwt, GATE_DOMAIN, origin)`
    /// (the §6.2 registration gate). Caches must already be warm (see
    /// [`warm_gate_caches`]); the flow's captcha step is answered here.
    fn register_via_sso_gate(
        env: &PocketIc,
        canister_id: Principal,
        jwt: &str,
        origin: &str,
    ) -> Result<u64, IdRegFinishError> {
        let start = identity_registration_start(env, canister_id, test_principal())
            .expect("API call failed")
            .expect("registration start failed");
        if let RegistrationFlowNextStep::CheckCaptcha { .. } = start.next_step {
            check_captcha(env, canister_id, test_principal(), "a".to_string())
                .expect("API call failed")
                .expect("check_captcha failed");
        }
        openid_identity_registration_finish(
            env,
            canister_id,
            test_principal(),
            &OpenIDRegFinishArg {
                jwt: jwt.to_owned(),
                salt: test_salt(),
                name: "Gate User".into(),
                discovery_domain: Some(GATE_DOMAIN.to_string()),
                origin: Some(origin.to_string()),
            },
        )
        .expect("API call failed")
        .map(|result| result.identity_number)
    }

    /// A `sub`-based org's FIRST gated login registers DIRECTLY in one IdP trip
    /// (§6.1): the per-app (gated) token registration-finish stores the
    /// PRIMARY-keyed credential (no per-app credential, no double trip). Proof:
    /// after registering via the gate, BOTH a gated per-app login and an ungated
    /// primary login resolve to the same, just-registered anchor.
    #[test]
    fn sub_org_first_gated_login_registers_directly() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        // `sub` org: the gated per-app token shares the primary credential's sub.
        let (gated_jwt, jwks) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let (primary_jwt, _) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);

        sync_time(&env, TEST_TIME_MS);
        warm_gate_caches(&env, canister_id, &responses, &gated_jwt, GATED_ORIGIN);

        let identity_number = register_via_sso_gate(&env, canister_id, &gated_jwt, GATED_ORIGIN)
            .expect("sub-org first gated login must register directly");

        let session_key = ByteBuf::from("dapp session key");

        // The gated login now resolves to the just-registered anchor.
        let gated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(gated.anchor_number, identity_number);

        // The ungated primary login resolves to the SAME anchor — proof the
        // stored credential is the primary-keyed one, not a per-app credential.
        let ungated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &primary_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                UNGATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(
            ungated.anchor_number, identity_number,
            "gate-registered credential must be primary-keyed (ungated login resolves to it)"
        );
        Ok(())
    }

    /// A non-`sub` (Entra-style `oid`) org's FIRST gated login canNOT register
    /// directly: the per-app token's pairwise sub has no aux bridge yet, so no
    /// primary sub can be derived. Registration FAILS SAFE with the typed
    /// `SsoNormalLoginRequired` (the FE routes the user through a normal
    /// primary-client sign-in first, §6.5) — it creates nothing.
    #[test]
    fn non_sub_org_first_gated_registration_fails_safe() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (gated_jwt, jwks) = token(PER_APP_CLIENT, PER_APP_SUB, &[("oid", STABLE_OID)]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "oid"), jwks);

        sync_time(&env, TEST_TIME_MS);
        warm_gate_caches(&env, canister_id, &responses, &gated_jwt, GATED_ORIGIN);

        let result = register_via_sso_gate(&env, canister_id, &gated_jwt, GATED_ORIGIN);
        assert!(
            matches!(result, Err(IdRegFinishError::SsoNormalLoginRequired)),
            "non-sub first gated registration must fail safe, got {result:?}"
        );
        Ok(())
    }

    /// The registration gate denies a token the delegation gate would deny: with
    /// `gate_all_apps` on, a registration attempt from an UNLISTED origin is
    /// refused (§6.2). Fails with a handled verification error, creating nothing.
    #[test]
    fn gate_all_apps_denies_registration_from_unlisted_origin() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);

        let (gated_jwt, jwks) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let (primary_jwt, _) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        // `gate_all_apps` on: only the listed GATED_ORIGIN is allowed.
        let responses = responses(well_known(&app_clients, true, "sub"), jwks);

        // Warm via the ALLOWED (gated) origin so discovery AND JWKS are cached
        // before we probe the denied origin (whose gate check returns before the
        // JWKS fetch, so it must not need to warm anything).
        sync_time(&env, TEST_TIME_MS);
        warm_gate_caches(&env, canister_id, &responses, &gated_jwt, GATED_ORIGIN);

        let result = register_via_sso_gate(&env, canister_id, &primary_jwt, UNGATED_ORIGIN);
        assert!(
            matches!(result, Err(IdRegFinishError::InvalidAuthnMethod(_))),
            "gate_all_apps must deny registration from an unlisted origin, got {result:?}"
        );
        Ok(())
    }

    // ── certified SSO attribute bundle consumer (`read_certified_sso_bundle`) ──
    //
    // These drive `prepare_icrc3_attributes` with a `sender_info` bundle, the way
    // the SDK `AttributesIdentity` does in production. PocketIC impersonates
    // senders and does NOT verify the `sender_info` canister signature (the
    // `RawSenderInfo` wire form carries no signature), so the crypto binding of
    // the bundle to the caller's credential seed — the cross-identity-replay
    // guarantee — is enforced by the real replica / SDK (the VERIFIED SAFETY
    // PROPERTY), not reproducible here. What the CANISTER itself controls, and
    // what these tests assert, is the fail-closed consumer logic:
    //   - `signer` must be THIS canister (else `read_certified_sso_bundle` = None),
    //   - the bundle must not be expired,
    //   - `bundle.origin` must equal the serving origin,
    //   - a caller with no bundle (or an unauthorized caller) gets no SSO cert.

    /// Prepare a primary anchor + a gated SSO bundle for `GATED_ORIGIN`. Returns
    /// `(identity_number, bundle_message)`.
    fn gated_bundle(
        env: &PocketIc,
        canister_id: Principal,
        responses: &[(String, String)],
        primary_jwt: &str,
    ) -> (u64, Vec<u8>) {
        let identity_number =
            register_with_primary_credential(env, canister_id, responses, primary_jwt);
        let session_key = ByteBuf::from("dapp session key");
        let (gated_jwt, _) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let gated = expect_ready(drive_sso_until_ready(env, responses, || {
            api::sso_prepare_delegation(
                env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                GATED_ORIGIN,
            )
            .unwrap()
        }));
        assert_eq!(gated.anchor_number, identity_number);
        (identity_number, gated.sso_attr_bundle.into_vec())
    }

    fn sso_attr_request(identity_number: u64, origin: &str) -> PrepareIcrc3AttributeRequest {
        PrepareIcrc3AttributeRequest {
            identity_number,
            origin: origin.to_string(),
            unmapped_origin: None,
            account_number: None,
            attributes: vec![AttributeSpec {
                key: format!("sso:{GATE_DOMAIN}:email"),
                value: None,
                omit_scope: false,
            }],
            nonce: vec![9u8; 32],
        }
    }

    /// Test category 1 — cross-identity replay / forged signer. A valid bundle
    /// presented by an UNAUTHORIZED principal is rejected (`AuthorizationError`),
    /// and a bundle whose `signer` is NOT this canister yields no `sso:<domain>`
    /// cert even for the authorized session (the `signer == id()` gate). The
    /// crypto binding that also stops an *authorized* other identity from
    /// replaying A's bundle is a replica guarantee (see the block comment).
    #[test]
    fn cross_identity_or_forged_signer_bundle_gets_no_sso_cert() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);
        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        let (identity_number, bundle) = gated_bundle(&env, canister_id, &responses, &primary_jwt);

        let session_principal = test_principal();

        // Unauthorized caller (not on the anchor) presenting a valid bundle:
        // `check_authorization` refuses before any SSO attribute is considered.
        let stranger = Principal::anonymous();
        let unauthorized = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            stranger,
            sso_attr_request(identity_number, GATED_ORIGIN),
            &bundle,
            canister_id,
        )?;
        assert!(
            matches!(
                unauthorized,
                Err(PrepareIcrc3AttributeError::AuthorizationError(_))
            ),
            "an unauthorized caller must be rejected regardless of the bundle, got {unauthorized:?}"
        );

        // Authorized session, but the bundle claims a signer other than this
        // canister: `read_certified_sso_bundle` fails the `signer == id()` gate,
        // so no `sso:<domain>` attribute is certified.
        let forged_signer = Principal::management_canister();
        let forged = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            session_principal,
            sso_attr_request(identity_number, GATED_ORIGIN),
            &bundle,
            forged_signer,
        )?;
        assert!(
            matches!(
                forged,
                Err(PrepareIcrc3AttributeError::AttributeMismatch { .. })
            ),
            "a bundle not signed by this canister must yield no sso:<domain> cert, got {forged:?}"
        );
        Ok(())
    }

    /// Test category 2 — expiry. A bundle whose `expiry` has passed yields no
    /// `sso:<domain>` cert (the producer stamps `expiry = now + 30min`; we
    /// advance the replica clock past it and re-present the same bundle).
    #[test]
    fn expired_bundle_gets_no_sso_cert() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);
        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        let (identity_number, bundle) = gated_bundle(&env, canister_id, &responses, &primary_jwt);

        // Fresh bundle certifies now.
        let fresh = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            test_principal(),
            sso_attr_request(identity_number, GATED_ORIGIN),
            &bundle,
            canister_id,
        )?;
        assert!(fresh.is_ok(), "fresh bundle must certify, got {fresh:?}");

        // Advance the clock past the bundle's 30-minute expiry.
        sync_time(&env, TEST_TIME_MS + 31 * 60 * 1_000);

        let expired = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            test_principal(),
            sso_attr_request(identity_number, GATED_ORIGIN),
            &bundle,
            canister_id,
        )?;
        assert!(
            matches!(
                expired,
                Err(PrepareIcrc3AttributeError::AttributeMismatch { .. })
            ),
            "an expired bundle must yield no sso:<domain> cert, got {expired:?}"
        );
        Ok(())
    }

    /// Test category 3 — icp0.io → ic0.app remap. The bundle-origin match and the
    /// account seed both key on the CANONICAL (mapped) origin: an attribute
    /// prepared with `unmapped_origin` set to the icp0.io form still certifies
    /// (the bundle matches the mapped origin), and the same session's account
    /// principal `f(account, mapped-origin)` is identical to a passkey session's
    /// for that mapped origin — i.e. the account seed uses the canonical origin
    /// identically on both paths.
    #[test]
    fn sso_attributes_survive_icp0_to_ic0_remap() -> Result<(), RejectResponse> {
        // A dapp served on `*.icp0.io`: the frontend remaps it to the canonical
        // `*.ic0.app` origin and passes THAT as `origin` to both the SSO
        // ceremony and the attribute/account calls, carrying the icp0.io form as
        // `unmapped_origin` only for the certified `implicit:origin`.
        const REMAP_MAPPED: &str = "https://payroll.ic0.app";
        const REMAP_UNMAPPED: &str = "https://payroll.icp0.io";

        let env = env();
        let canister_id = install(&env);
        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        // Gate the mapped origin (the canonical form the ceremony binds to).
        let app_clients = format!(r#"{{"{REMAP_MAPPED}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        let identity_number =
            register_with_primary_credential(&env, canister_id, &responses, &primary_jwt);

        let session_key = ByteBuf::from("remap session key");
        let (gated_jwt, _) = token(PER_APP_CLIENT, PRIMARY_SUB, &[]);
        let gated = expect_ready(drive_sso_until_ready(&env, &responses, || {
            api::sso_prepare_delegation(
                &env,
                canister_id,
                test_principal(),
                &gated_jwt,
                &test_salt(),
                &session_key,
                GATE_DOMAIN,
                REMAP_MAPPED,
            )
            .unwrap()
        }));
        assert_eq!(gated.anchor_number, identity_number);
        let bundle = gated.sso_attr_bundle.into_vec();

        // Certify with the unmapped (icp0.io) origin set: it must still verify,
        // because the bundle-origin comparison and the account seed both key on
        // the mapped (ic0.app) origin, not the unmapped one.
        let mut request = sso_attr_request(identity_number, REMAP_MAPPED);
        request.unmapped_origin = Some(REMAP_UNMAPPED.to_string());
        let remapped = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            test_principal(),
            request,
            &bundle,
            canister_id,
        )?;
        assert!(
            remapped.is_ok(),
            "an attribute with unmapped_origin set must still certify against the mapped origin, got {remapped:?}"
        );

        // The account principal the certified attribute is bound to — keyed on
        // the mapped origin — matches a passkey session's for the same origin.
        let sso_session_principal = Principal::self_authenticating(gated.user_key.as_ref());
        let via_sso = prepare_account_delegation(
            &AccountDelegationParams::new(
                &env,
                canister_id,
                sso_session_principal,
                identity_number,
                REMAP_MAPPED.to_string(),
                None,
                session_key.clone(),
            ),
            None,
        )?
        .expect("SSO session mints the mapped-origin account delegation");
        let via_passkey = prepare_account_delegation(
            &AccountDelegationParams::new(
                &env,
                canister_id,
                test_principal(),
                identity_number,
                REMAP_MAPPED.to_string(),
                None,
                session_key.clone(),
            ),
            None,
        )?
        .expect("passkey session mints the mapped-origin account delegation");
        assert_eq!(
            via_sso.user_key, via_passkey.user_key,
            "account seed keys on the canonical (mapped) origin on both paths"
        );
        Ok(())
    }

    /// Test category 5 — cross-origin. A bundle certified for `GATED_ORIGIN`
    /// presented on a DIFFERENT serving origin yields no `sso:<domain>` cert
    /// (`bundle.origin != serving origin`).
    #[test]
    fn cross_origin_bundle_gets_no_sso_cert() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id = install(&env);
        // `gate_all_apps` off so the primary/ungated origin is also servable, and
        // so the anchor legitimately owns the SSO domain credential.
        let (primary_jwt, jwks) = token(PRIMARY_CLIENT, PRIMARY_SUB, &[]);
        let app_clients = format!(r#"{{"{GATED_ORIGIN}":"{PER_APP_CLIENT}"}}"#);
        let responses = responses(well_known(&app_clients, false, "sub"), jwks);
        let (identity_number, bundle) = gated_bundle(&env, canister_id, &responses, &primary_jwt);

        // Same authorized session and valid bundle (origin = GATED_ORIGIN), but
        // the call serves a DIFFERENT origin: the origin filter drops the bundle.
        let cross = api::prepare_icrc3_attributes_with_bundle(
            &env,
            canister_id,
            test_principal(),
            sso_attr_request(identity_number, UNGATED_ORIGIN),
            &bundle,
            canister_id,
        )?;
        assert!(
            matches!(
                cross,
                Err(PrepareIcrc3AttributeError::AttributeMismatch { .. })
            ),
            "a bundle for another origin must yield no sso:<domain> cert, got {cross:?}"
        );
        Ok(())
    }
}
