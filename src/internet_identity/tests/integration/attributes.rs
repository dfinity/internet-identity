//! Tests related to prepare_attributes and get_attributes II canister calls.

use candid::{Decode, Principal};
use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use ic_canister_sig_creation::extract_raw_canister_sig_pk_from_der;
use internet_identity_interface::internet_identity::types::attributes::{
    AttributeSpec, CertifiedAttribute, CertifiedAttributes, GetAttributesRequest,
    GetIcrc3AttributeError, GetIcrc3AttributeRequest, PrepareAttributeRequest,
    PrepareIcrc3AttributeError, PrepareIcrc3AttributeRequest,
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

/// Regression test: Microsoft credentials use a template issuer with `{tid}` placeholder.
/// `get_attributes` must use the template issuer (from the OpenID config) as the scope key,
/// NOT the resolved issuer (from the JWT `iss` claim with the actual tenant ID).
/// Without the fix, `get_attributes` would look up signatures under the resolved issuer scope
/// but `prepare_attributes` stores them under the template issuer scope, resulting in
/// no certified attributes being returned.
#[test]
fn should_get_certified_attributes_microsoft() {
    let env = env();
    #[allow(unused_variables)]
    let (jwt, salt, claims, test_time, test_principal, test_authn_method) =
        crate::openid::one_openid_microsoft_test_data();

    let mut init_args = arg_with_wasm_hash(ARCHIVE_WASM.clone()).unwrap();
    init_args.openid_configs = Some(vec![OpenIdConfig {
        name: "Microsoft".into(),
        logo: "logo".into(),
        issuer: "https://login.microsoftonline.com/{tid}/v2.0".into(),
        client_id: "d948c073-eebd-4ab8-861d-055f7ab49e17".into(),
        jwks_uri: "https://login.microsoftonline.com/common/discovery/v2.0/keys".into(),
        auth_uri: "https://login.microsoftonline.com/common/oauth2/v2.0/authorize".into(),
        auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
        fedcm_uri: Some("".into()),
        email_verification: None,
    }]);

    let ii_backend_canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(init_args),
        1_000_000_000_000_000, // 1P cycles for HTTP outcalls
    );
    // Mock certs response so openid_credential_add can verify the JWT
    crate::openid::mock_microsoft_certs_response(&env);

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

    // The attribute key MUST use the template issuer with {tid}, not the resolved tenant ID.
    let microsoft_template_issuer = "https://login.microsoftonline.com/{tid}/v2.0";

    // 1. Prepare attributes

    env.advance_time(Duration::from_secs(15));

    let prepare_request = PrepareAttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attribute_keys: vec![format!("openid:{microsoft_template_issuer}:name")],
    };

    let prepare_response = api::prepare_attributes(
        &env,
        ii_backend_canister_id,
        test_principal,
        prepare_request,
    )
    .expect("failed to call prepare_attributes")
    .expect("prepare_attributes error");

    assert_eq!(prepare_response.attributes.len(), 1);

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

    // Check that we actually got a certified attribute back (the regression would return 0)
    assert_eq!(
        get_response.certified_attributes.len(),
        1,
        "get_attributes should return 1 certified attribute for Microsoft name; \
         if 0, get_attributes is likely using the resolved issuer instead of the template issuer"
    );

    // Verify the CBOR signature prefix (smoke test)
    assert!(get_response.certified_attributes[0]
        .signature
        .starts_with(&[217, 217, 247, 162, 107, 99, 101, 114]));

    // Redact signatures for comparison
    let mut redacted_response = get_response.clone();
    redacted_response
        .certified_attributes
        .iter_mut()
        .for_each(|attr| attr.signature = vec![]);

    assert_eq!(
        redacted_response,
        CertifiedAttributes {
            certified_attributes: vec![CertifiedAttribute {
                key: format!("openid:{microsoft_template_issuer}:name"),
                value: "Llorenç Muntaner Perello".as_bytes().to_vec(),
                signature: vec![], // redacted
            }],
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

#[test]
fn should_get_icrc3_certified_attributes() {
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
        1_000_000_000_000_000,
    );
    crate::openid::mock_google_certs_response(&env);

    deploy_archive_via_ii(&env, ii_backend_canister_id);

    let identity_number =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method(
            &env,
            ii_backend_canister_id,
            &test_authn_method,
        );

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

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

    // 1. Prepare ICRC-3 attributes (with correct values)

    env.advance_time(Duration::from_secs(15));

    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![
            AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: Some(b"andri.schatz@dfinity.org".to_vec()),
                omit_scope: false,
            },
            AttributeSpec {
                key: "openid:https://accounts.google.com:name".into(),
                value: Some(b"Andri Schatz".to_vec()),
                omit_scope: false,
            },
        ],
    };

    let prepare_response = api::prepare_icrc3_attributes(
        &env,
        ii_backend_canister_id,
        test_principal,
        prepare_request,
    )
    .expect("failed to call prepare_icrc3_attributes")
    .expect("prepare_icrc3_attributes error");

    assert!(!prepare_response.message.is_empty());

    // 2. Get ICRC-3 attributes

    env.advance_time(Duration::from_secs(5));

    let get_request = GetIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        message: prepare_response.message.clone(),
    };

    let get_response =
        api::get_icrc3_attributes(&env, ii_backend_canister_id, test_principal, get_request)
            .expect("failed to call get_icrc3_attributes")
            .expect("get_icrc3_attributes error");

    // Check that signature is non-empty and has CBOR prefix
    assert!(!get_response.signature.is_empty());
    assert!(get_response
        .signature
        .starts_with(&[217, 217, 247, 162, 107, 99, 101, 114]));

    // 3. Verify the signature

    let session_public_key = ByteBuf::from("session public key");

    env.advance_time(Duration::from_secs(35));

    let (canister_sig_key, _expiration) = api::prepare_delegation(
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

    let root_key = env.root_key().unwrap();
    verify_icrc3_attributes(
        &env,
        canister_sig_key,
        &prepare_response.message,
        &get_response.signature,
        &root_key,
    );
}

/// Sets up an environment with a Google OpenID credential and returns all context needed for tests.
fn setup_icrc3_test_env() -> (
    PocketIc,
    Principal, // ii_backend_canister_id
    Principal, // test_principal
    u64,       // identity_number
) {
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
        1_000_000_000_000_000,
    );
    crate::openid::mock_google_certs_response(&env);
    deploy_archive_via_ii(&env, ii_backend_canister_id);

    let identity_number =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method(
            &env,
            ii_backend_canister_id,
            &test_authn_method,
        );

    let time_to_advance = Duration::from_millis(test_time) - Duration::from_nanos(time(&env));
    env.advance_time(time_to_advance);

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

    env.advance_time(Duration::from_secs(15));

    (env, ii_backend_canister_id, test_principal, identity_number)
}

#[test]
fn should_certify_icrc3_attributes_mixed_omit_scope() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // Mix: email with omit_scope=true, name with omit_scope=false
    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![
            AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: None,
                omit_scope: true,
            },
            AttributeSpec {
                key: "openid:https://accounts.google.com:name".into(),
                value: None,
                omit_scope: false,
            },
        ],
    };

    let prepare_response =
        api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
            .expect("failed to call prepare_icrc3_attributes")
            .expect("prepare_icrc3_attributes error");

    let icrc3_value: internet_identity_interface::internet_identity::types::icrc3::Icrc3Value =
        candid::Decode!(
            &prepare_response.message,
            internet_identity_interface::internet_identity::types::icrc3::Icrc3Value
        )
        .expect("failed to decode ICRC-3 value");

    match icrc3_value {
        internet_identity_interface::internet_identity::types::icrc3::Icrc3Value::Map(entries) => {
            assert_eq!(entries.len(), 2);
            let keys: Vec<&str> = entries.iter().map(|(k, _)| k.as_str()).collect();
            // email should have scope omitted
            assert!(
                keys.contains(&"email"),
                "Expected unscoped 'email' key, got {:?}",
                keys
            );
            // name should have full scope
            assert!(
                keys.contains(&"openid:https://accounts.google.com:name"),
                "Expected scoped 'name' key, got {:?}",
                keys
            );
        }
        other => panic!("Expected Map, got {:?}", other),
    }
}

#[test]
fn should_reject_icrc3_attributes_with_correct_and_wrong_value() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // One correct value, one wrong value
    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![
            AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: Some(b"andri.schatz@dfinity.org".to_vec()), // correct
                omit_scope: false,
            },
            AttributeSpec {
                key: "openid:https://accounts.google.com:name".into(),
                value: Some(b"Wrong Name".to_vec()), // wrong
                omit_scope: false,
            },
        ],
    };

    let result = api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
        .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
            assert_eq!(problems.len(), 1);
            assert!(problems[0].contains("name"));
        }
        other => panic!("Expected AttributeMismatch error, got {:?}", other),
    }
}

#[test]
fn should_return_no_such_signature_for_unknown_message() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // First prepare a valid message so the account exists
    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![AttributeSpec {
            key: "openid:https://accounts.google.com:email".into(),
            value: None,
            omit_scope: false,
        }],
    };

    api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
        .expect("failed to call prepare_icrc3_attributes")
        .expect("prepare_icrc3_attributes error");

    env.advance_time(Duration::from_secs(5));

    // Try to get signature for a message that was never prepared
    let get_request = GetIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        message: vec![1, 2, 3, 4, 5], // bogus message
    };

    let result = api::get_icrc3_attributes(&env, canister_id, principal, get_request)
        .expect("failed to call get_icrc3_attributes");

    assert_eq!(result, Err(GetIcrc3AttributeError::NoSuchSignature));
}

#[test]
fn should_produce_different_messages_for_different_omit_scope() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // Prepare with omit_scope = false
    let prepare_scoped = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![AttributeSpec {
            key: "openid:https://accounts.google.com:email".into(),
            value: None,
            omit_scope: false,
        }],
    };

    let response_scoped =
        api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_scoped)
            .expect("failed to call prepare_icrc3_attributes")
            .expect("prepare_icrc3_attributes error");

    // Prepare with omit_scope = true
    let prepare_unscoped = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![AttributeSpec {
            key: "openid:https://accounts.google.com:email".into(),
            value: None,
            omit_scope: true,
        }],
    };

    let response_unscoped =
        api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_unscoped)
            .expect("failed to call prepare_icrc3_attributes")
            .expect("prepare_icrc3_attributes error");

    // The messages should be different because the keys in the ICRC-3 map differ
    assert_ne!(
        response_scoped.message, response_unscoped.message,
        "Messages with different omit_scope settings should differ"
    );
}

#[test]
fn should_reject_icrc3_attributes_for_unknown_issuer() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![AttributeSpec {
            key: "openid:https://unknown-issuer.com:email".into(),
            value: None,
            omit_scope: false,
        }],
    };

    let result = api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
        .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
            assert!(
                problems[0].contains("No credential found"),
                "Expected 'No credential found' error, got: {}",
                problems[0]
            );
        }
        other => panic!("Expected AttributeMismatch error, got {:?}", other),
    }
}

#[test]
fn should_reject_icrc3_attributes_without_scope() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // Attribute without a scope (just "email" instead of "openid:...:email")
    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![AttributeSpec {
            key: "email".into(),
            value: None,
            omit_scope: false,
        }],
    };

    let result = api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
        .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
            assert!(
                problems[0].contains("no scope"),
                "Expected 'no scope' error, got: {}",
                problems[0]
            );
        }
        other => panic!("Expected AttributeMismatch error, got {:?}", other),
    }
}
