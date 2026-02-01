//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::attributes::{
    CertifiedAttribute, CertifiedAttributes, GetAttributesRequest, PrepareAttributeRequest,
};
use internet_identity_interface::internet_identity::types::OpenIdConfig;
use pretty_assertions::assert_eq;
use std::time::Duration;

#[test]
fn should_get_certified_attributes() {
    let env = env();
    let (jwt, salt, claims, test_time, test_principal, test_authn_method) =
        crate::openid::openid_google_test_data();

    let mut init_args = arg_with_wasm_hash(ARCHIVE_WASM.clone()).unwrap();
    init_args.openid_configs = Some(vec![OpenIdConfig {
        name: "Google".into(),
        logo: "logo".into(),
        issuer: "https://accounts.google.com".into(),
        client_id: "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com"
            .into(),
        jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".into(),
        auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".into(),
        auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
        fedcm_uri: Some("https://accounts.google.com/gsi/fedcm.json".into()),
    }]);

    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(init_args),
        1_000_000_000_000_000, // 1P cycles for HTTP outcalls
    );
    // Mock certs response so openid_credential_add can verify the JWT
    // This also handles the fetch triggered by initialize()
    crate::openid::mock_google_certs_response(&env);

    deploy_archive_via_ii(&env, canister_id);

    // Create an identity with the required authn method
    let user_number = crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method(
        &env,
        canister_id,
        &test_authn_method,
    );

    // Sync time to the JWT iat
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    // Add OpenID credential
    api::openid_credential_add(&env, canister_id, test_principal, user_number, &jwt, &salt)
        .expect("failed to add openid credential")
        .expect("openid_credential_add error");

    let origin = "https://some-dapp.com";

    // 1. Prepare attributes
    let prepare_request = PrepareAttributeRequest {
        identity_number: user_number,
        origin: origin.to_string(),
        account_number: None,
        attribute_keys: vec![
            "openid:https://accounts.google.com:email".into(),
            "openid:https://accounts.google.com:name".into(),
        ],
    };

    let prepare_response =
        api::prepare_attributes(&env, canister_id, test_principal, prepare_request)
            .expect("failed to call prepare_attributes")
            .expect("prepare_attributes error");

    assert_eq!(prepare_response.attributes.len(), 2);

    // 2. Get attributes
    let get_request = GetAttributesRequest {
        identity_number: user_number,
        origin: origin.to_string(),
        account_number: None,
        issued_at_timestamp_ns: prepare_response.issued_at_timestamp_ns,
        attributes: prepare_response.attributes.clone(),
    };

    let get_response = api::get_attributes(&env, canister_id, test_principal, get_request)
        .expect("failed to call get_attributes")
        .expect("get_attributes error");

    let mut actual_response = get_response;
    actual_response.certified_attributes.sort();

    // Check the signatures (not a full verification step, just a smoke test)
    for attribute in actual_response.certified_attributes.iter() {
        assert!(attribute
            .signature
            .starts_with(&[217, 217, 247, 162, 107, 99, 101, 114]));
    }

    // Redact the signatures so we can compare the response
    actual_response
        .certified_attributes
        .iter_mut()
        .for_each(|attr| attr.signature = vec![]);

    assert_eq!(
        actual_response,
        CertifiedAttributes {
            certified_attributes: vec![
                CertifiedAttribute {
                    key: "openid:https://accounts.google.com:email".into(),
                    value: "andri.schatz@dfinity.org".into(),
                    signature: vec![] // redacted
                },
                CertifiedAttribute {
                    key: "openid:https://accounts.google.com:name".into(),
                    value: "Andri Schatz".into(),
                    signature: vec![],
                },
            ],
            expires_at_timestamp_ns: prepare_response.issued_at_timestamp_ns
                + Duration::from_secs(30 * 60).as_nanos() as u64,
        }
    );
}
