//! Tests related to prepare_attributes and get_attributes II canister calls.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use ic_canister_sig_creation::extract_raw_canister_sig_pk_from_der;
use internet_identity_interface::internet_identity::types::attributes::{
    CertifiedAttribute, CertifiedAttributes, GetAttributesRequest, PrepareAttributeRequest,
};
use internet_identity_interface::internet_identity::types::{
    GetDelegationResponse, OpenIdConfig, SignedDelegation,
};
use pocket_ic::PocketIc;
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

fn verify_delegation_and_attribute_signatures(
    env: &PocketIc,
    session_public_key: ByteBuf,
    user_key: ByteBuf,
    ii_backend_canister_id: Principal,
    signed_delegation: SignedDelegation,
    attributes: Vec<CertifiedAttribute>,
    attribute_expiration: u64,
) {
    let root_key = env.root_key().unwrap();

    let now_timestamp_ns = env.get_time().as_nanos_since_unix_epoch();

    assert_eq!(signed_delegation.delegation.pubkey, session_public_key);
    assert!(
        signed_delegation.delegation.expiration > now_timestamp_ns,
        "Delegation has expired: {} <= {}",
        signed_delegation.delegation.expiration,
        now_timestamp_ns
    );

    assert_eq!(signed_delegation.delegation.targets, None);
    assert!(attribute_expiration > now_timestamp_ns);

    // Ensure that the user key is a canister signature (we rely on `user_key` being DER-encoded)
    let canister_id_bytes = extract_raw_canister_sig_pk_from_der(&user_key).unwrap();

    let canister_id = {
        let canister_id_bytes_len = canister_id_bytes[0] as usize;
        let bound = canister_id_bytes_len + 1;
        Principal::from_slice(&canister_id_bytes[1..bound])
    };

    assert_eq!(canister_id, ii_backend_canister_id);

    verify_delegation(env, user_key.clone(), &signed_delegation, &root_key);

    for attribute in &attributes {
        verify_attribute(
            env,
            user_key.clone(),
            attribute,
            attribute_expiration,
            &root_key,
        );
    }
}

#[test]
fn should_get_certified_attributes() {
    let env = env();
    #[allow(unused_variables)]
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
        email_verification: None,
    }]);

    let ii_backend_canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(init_args),
        1_000_000_000_000_000, // 1P cycles for HTTP outcalls
    );
    // Mock certs response so openid_credential_add can verify the JWT
    // This also handles the fetch triggered by initialize()
    crate::openid::mock_google_certs_response(&env);

    deploy_archive_via_ii(&env, ii_backend_canister_id);

    // Create an identity with the required authn method
    let identity_number =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method(
            &env,
            ii_backend_canister_id,
            &test_authn_method,
        );

    // Sync time to the JWT iat
    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

    // Add OpenID credential
    api::openid_credential_add(
        &env,
        ii_backend_canister_id,
        test_principal,
        identity_number,
        &jwt,
        &salt,
    )
    .expect("failed to add openid credential")
    .expect("openid_credential_add error");

    let origin = "https://some-dapp.com";

    // 1. Prepare attributes

    env.advance_time(Duration::from_secs(15));

    let prepare_request = PrepareAttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attribute_keys: vec![
            "openid:https://accounts.google.com:email".into(),
            "openid:https://accounts.google.com:name".into(),
        ],
    };

    let prepare_response = api::prepare_attributes(
        &env,
        ii_backend_canister_id,
        test_principal,
        prepare_request,
    )
    .expect("failed to call prepare_attributes")
    .expect("prepare_attributes error");

    assert_eq!(prepare_response.attributes.len(), 2);

    // 2. Get attributes
    env.advance_time(Duration::from_secs(5));

    let get_request = GetAttributesRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        issued_at_timestamp_ns: prepare_response.issued_at_timestamp_ns,
        attributes: prepare_response.attributes.clone(),
    };

    let get_response =
        api::get_attributes(&env, ii_backend_canister_id, test_principal, get_request)
            .expect("failed to call get_attributes")
            .expect("get_attributes error");

    let mut redacted_response = get_response.clone();
    redacted_response.certified_attributes.sort();

    // Check the signatures (not a full verification step, just a smoke test)
    for attribute in redacted_response.certified_attributes.iter() {
        assert!(attribute
            .signature
            .starts_with(&[217, 217, 247, 162, 107, 99, 101, 114]));
    }

    // Redact the signatures so we can compare the response
    redacted_response
        .certified_attributes
        .iter_mut()
        .for_each(|attr| attr.signature = vec![]);

    assert_eq!(
        redacted_response,
        CertifiedAttributes {
            certified_attributes: vec![
                CertifiedAttribute {
                    key: "openid:https://accounts.google.com:email".into(),
                    value: b"andri.schatz@dfinity.org".to_vec(),
                    signature: vec![], // redacted
                },
                CertifiedAttribute {
                    key: "openid:https://accounts.google.com:name".into(),
                    value: b"Andri Schatz".to_vec(),
                    signature: vec![], // redacted
                },
            ],
            expires_at_timestamp_ns: prepare_response.issued_at_timestamp_ns
                + Duration::from_secs(30 * 60).as_nanos() as u64,
        }
    );

    // Verify the signatures; this relies on delegation verification for the same (origin, user).

    let session_public_key = ByteBuf::from("session public key");

    env.advance_time(Duration::from_secs(35));

    let (canister_sig_key, expiration) = api::prepare_delegation(
        &env,
        ii_backend_canister_id,
        test_principal,
        identity_number,
        origin,
        &session_public_key,
        None,
    )
    .unwrap();

    env.advance_time(Duration::from_secs(5));

    let signed_delegation = match api::get_delegation(
        &env,
        ii_backend_canister_id,
        test_principal,
        identity_number,
        origin,
        &session_public_key,
        expiration,
    )
    .unwrap()
    {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation_and_attribute_signatures(
        &env,
        session_public_key,
        canister_sig_key,
        ii_backend_canister_id,
        signed_delegation,
        get_response.certified_attributes,
        get_response.expires_at_timestamp_ns,
    );
}
