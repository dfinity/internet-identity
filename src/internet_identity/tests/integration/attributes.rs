//! Tests related to prepare_attributes and get_attributes II canister calls.

use candid::{Decode, Principal};
use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use ic_canister_sig_creation::extract_raw_canister_sig_pk_from_der;
use internet_identity_interface::internet_identity::types::attributes::{
    AttributeSpec, CertifiedAttribute, CertifiedAttributes, GetAttributesRequest,
    GetIcrc3AttributeError, GetIcrc3AttributeRequest, ListAvailableAttributesRequest,
    PrepareAttributeRequest, PrepareIcrc3AttributeError, PrepareIcrc3AttributeRequest,
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
        nonce: vec![0u8; 32],
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
    use internet_identity_interface::internet_identity::types::icrc3::Icrc3Value;

    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    let nonce = hex::decode("5f87b8f041d8e1121d5a7d0360a02213e4b7b3b44b25d0c7f070c7e2b694b29c")
        .expect("failed to decode nonce hex");

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
        nonce: nonce.clone(),
    };

    let time_before = env.get_time().as_nanos_since_unix_epoch();

    let prepare_response =
        api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
            .expect("failed to call prepare_icrc3_attributes")
            .expect("prepare_icrc3_attributes error");

    let time_after = env.get_time().as_nanos_since_unix_epoch();

    let icrc3_value: Icrc3Value = candid::Decode!(&prepare_response.message, Icrc3Value)
        .expect("failed to decode ICRC-3 value");

    match &icrc3_value {
        Icrc3Value::Map(entries) => {
            assert_eq!(entries.len(), 5);
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
            // nonce should be included with the expected value
            let nonce_entry = entries
                .iter()
                .find(|(k, _)| k == "implicit:nonce")
                .expect("Expected 'implicit:nonce' key in message map");
            assert_eq!(
                nonce_entry.1,
                Icrc3Value::Blob(nonce.clone()),
                "Nonce value in certified message does not match the provided nonce"
            );
            // origin should be included as Text
            let origin_entry = entries
                .iter()
                .find(|(k, _)| k == "implicit:origin")
                .expect("Expected 'implicit:origin' key in message map");
            assert_eq!(
                origin_entry.1,
                Icrc3Value::Text(origin.to_string()),
                "Origin value does not match"
            );
            // issued_at_timestamp_ns should be a Nat within the time window of the call
            let timestamp_entry = entries
                .iter()
                .find(|(k, _)| k == "implicit:issued_at_timestamp_ns")
                .expect("Expected 'implicit:issued_at_timestamp_ns' key in message map");
            match &timestamp_entry.1 {
                Icrc3Value::Nat(nat) => {
                    let ts: u64 = nat
                        .0
                        .clone()
                        .try_into()
                        .expect("timestamp should fit in u64");
                    assert!(
                        ts >= time_before && ts <= time_after,
                        "Timestamp {} should be between {} and {}",
                        ts,
                        time_before,
                        time_after
                    );
                }
                other => panic!("Expected Nat for timestamp, got {:?}", other),
            }
        }
        other => panic!("Expected Map, got {:?}", other),
    }

    // Verify the signature over the message containing the nonce

    env.advance_time(Duration::from_secs(5));

    let get_request = GetIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        message: prepare_response.message.clone(),
    };

    let get_response = api::get_icrc3_attributes(&env, canister_id, principal, get_request)
        .expect("failed to call get_icrc3_attributes")
        .expect("get_icrc3_attributes error");

    let session_public_key = ByteBuf::from("session public key");

    env.advance_time(Duration::from_secs(35));

    let (canister_sig_key, _expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal,
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
        nonce: vec![0u8; 32],
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
fn should_list_all_available_attributes() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();

    let request = ListAvailableAttributesRequest {
        identity_number,
        attributes: None,
    };

    let result = api::list_available_attributes(&env, canister_id, principal, request)
        .expect("failed to call list_available_attributes")
        .expect("list_available_attributes error");

    // The test setup creates a Google OpenID credential with email and name
    let keys: Vec<&str> = result.iter().map(|(k, _)| k.as_str()).collect();
    assert!(
        keys.contains(&"openid:https://accounts.google.com:email"),
        "Expected email attribute, got {:?}",
        keys
    );
    assert!(
        keys.contains(&"openid:https://accounts.google.com:name"),
        "Expected name attribute, got {:?}",
        keys
    );
    assert!(
        result.len() >= 2,
        "Expected at least 2 attributes, got {}",
        result.len()
    );
}

#[test]
fn icrc3_test_vectors() {
    use internet_identity_interface::internet_identity::types::icrc3::Icrc3Value;

    // The test vectors use a synthetic OpenID provider with a JWT signed by an RSA key
    // generated deterministically inside the test. This keeps the produced vectors free
    // of real personal information while still exercising the full prepare/get/verify
    // pipeline in the canister.
    let (env, canister_id, principal, identity_number, fake_email, fake_name) =
        setup_icrc3_test_env_with_fake_openid();
    let origin = "https://some-dapp.com";

    // Obtain the canister signature public key (DER-encoded) once. The key is derived from
    // the (canister_id, identity_number, origin) triple and does not change.
    let session_public_key = ByteBuf::from("session public key");
    let (canister_sig_key, _) = api::prepare_delegation(
        &env,
        canister_id,
        principal,
        identity_number,
        origin,
        &session_public_key,
        None,
    )
    .unwrap();

    env.advance_time(Duration::from_secs(5));

    let root_key = env.root_key().unwrap();

    // ---- vector definitions ----
    // Each tuple: (label, nonce, attribute_specs)
    let vectors: Vec<(&str, Vec<u8>, Vec<AttributeSpec>)> = vec![
        // 1. Single email, scoped key
        (
            "Single email attribute with scoped key",
            vec![0u8; 32],
            vec![AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: None,
                omit_scope: false,
            }],
        ),
        // 2. Single email, unscoped key (omit_scope = true)
        (
            "Single email attribute with unscoped key",
            vec![0u8; 32],
            vec![AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: None,
                omit_scope: true,
            }],
        ),
        // 3. Single name, scoped key
        (
            "Single name attribute with scoped key",
            vec![0u8; 32],
            vec![AttributeSpec {
                key: "openid:https://accounts.google.com:name".into(),
                value: None,
                omit_scope: false,
            }],
        ),
        // 4. Email + name, both scoped
        (
            "Email and name with scoped keys",
            vec![0u8; 32],
            vec![
                AttributeSpec {
                    key: "openid:https://accounts.google.com:email".into(),
                    value: None,
                    omit_scope: false,
                },
                AttributeSpec {
                    key: "openid:https://accounts.google.com:name".into(),
                    value: None,
                    omit_scope: false,
                },
            ],
        ),
        // 5. Email + name, both unscoped
        (
            "Email and name with unscoped keys",
            vec![0u8; 32],
            vec![
                AttributeSpec {
                    key: "openid:https://accounts.google.com:email".into(),
                    value: None,
                    omit_scope: true,
                },
                AttributeSpec {
                    key: "openid:https://accounts.google.com:name".into(),
                    value: None,
                    omit_scope: true,
                },
            ],
        ),
        // 6. Mixed scoping: email unscoped, name scoped
        (
            "Email unscoped and name scoped",
            vec![0u8; 32],
            vec![
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
        ),
        // 7. Email with explicit value validation
        (
            "Email with value validation",
            vec![0u8; 32],
            vec![AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: Some(fake_email.as_bytes().to_vec()),
                omit_scope: false,
            }],
        ),
        // 8. Single email with non-zero nonce
        (
            "Single email with specific nonce",
            hex::decode("5f87b8f041d8e1121d5a7d0360a02213e4b7b3b44b25d0c7f070c7e2b694b29c")
                .expect("failed to decode specific nonce"),
            vec![AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: None,
                omit_scope: false,
            }],
        ),
        // 9. Email + name, mixed scoping, non-zero nonce
        (
            "Email unscoped + name scoped with specific nonce",
            hex::decode("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                .expect("failed to decode specific nonce"),
            vec![
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
        ),
        // 10. Only implicit attributes (no user attributes requested).
        // An empty attribute list is valid and produces a message containing only
        // the three implicit entries.
        (
            "No user attributes, only implicit entries",
            vec![0u8; 32],
            vec![],
        ),
    ];

    let mut vectors_json: Vec<serde_json::Value> = Vec::new();

    for (label, nonce, attributes) in &vectors {
        let prepare_request = PrepareIcrc3AttributeRequest {
            identity_number,
            origin: origin.to_string(),
            account_number: None,
            attributes: attributes.clone(),
            nonce: nonce.clone(),
        };

        let prepare_response =
            api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
                .expect("failed to call prepare_icrc3_attributes")
                .expect("prepare_icrc3_attributes error");

        env.advance_time(Duration::from_secs(5));

        let get_request = GetIcrc3AttributeRequest {
            identity_number,
            origin: origin.to_string(),
            account_number: None,
            message: prepare_response.message.clone(),
        };

        let get_response = api::get_icrc3_attributes(&env, canister_id, principal, get_request)
            .expect("failed to call get_icrc3_attributes")
            .expect("get_icrc3_attributes error");

        // Build the domain-separated signed message. Uses the same constant that
        // `verify_icrc3_attributes` relies on, so the emitted `signed_message_hex`
        // stays in sync with the canister's verification envelope.
        let mut signed_message: Vec<u8> = Vec::with_capacity(
            1 + ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN.len() + prepare_response.message.len(),
        );
        signed_message.push(ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN.len() as u8);
        signed_message.extend_from_slice(ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN);
        signed_message.extend_from_slice(&prepare_response.message);

        // Verify the signature.
        verify_icrc3_attributes(
            &env,
            canister_sig_key.clone(),
            &prepare_response.message,
            &get_response.signature,
            &root_key,
        );

        // Decode the ICRC-3 value for the human-readable representation.
        let icrc3_value: Icrc3Value =
            Decode!(&prepare_response.message, Icrc3Value).expect("failed to decode");

        let icrc3_repr = format_icrc3_value(&icrc3_value);

        vectors_json.push(serde_json::json!({
            "label": label,
            "icrc3_value": icrc3_repr,
            "message_hex": hex::encode(&prepare_response.message),
            "signed_message_hex": hex::encode(&signed_message),
            "certificate_cbor_hex": hex::encode(&get_response.signature),
        }));
    }

    let snapshot = serde_json::json!({
        "canister_sig_pk_hex": hex::encode(&canister_sig_key),
        "root_key_hex": hex::encode(&root_key),
        "origin": origin,
        "issuer": "https://accounts.google.com",
        "email": fake_email,
        "name": fake_name,
        "vectors": vectors_json,
    });

    // Serialize the full snapshot (including `certificate_cbor_hex`) with 4-space
    // indentation and a trailing newline. The committed file contains a real
    // certificate so readers have a runnable end-to-end example.
    let generated = serialize_snapshot_pretty(&snapshot);

    let snapshot_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../docs/icrc3-test-vectors.json");

    if std::env::var_os("UPDATE_ICRC3_VECTORS").is_some() {
        std::fs::write(&snapshot_path, &generated)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", snapshot_path.display()));
        eprintln!("Updated {}", snapshot_path.display());
        return;
    }

    let expected_raw = std::fs::read_to_string(&snapshot_path).unwrap_or_else(|e| {
        panic!(
            "failed to read {}: {e}\nIf this file is missing, regenerate with:\n  UPDATE_ICRC3_VECTORS=1 cargo test -p internet_identity --test integration icrc3_test_vectors",
            snapshot_path.display()
        )
    });
    let expected_value: serde_json::Value = serde_json::from_str(&expected_raw)
        .unwrap_or_else(|e| panic!("failed to parse {}: {e}", snapshot_path.display()));

    // Strip `certificate_cbor_hex` from both sides before comparing: the CBOR
    // certificate's BLS signature is not stable across PocketIC versions /
    // platforms, while the canister-produced `message_hex` and
    // `signed_message_hex` bytes are fully deterministic and are what dapps
    // actually rely on.
    let generated_stable = serialize_snapshot_pretty(&strip_cert(&snapshot));
    let expected_stable = serialize_snapshot_pretty(&strip_cert(&expected_value));

    assert_eq!(
        generated_stable, expected_stable,
        "ICRC-3 test vectors changed. If this is intentional, regenerate the snapshot with:\n  UPDATE_ICRC3_VECTORS=1 cargo test -p internet_identity --test integration icrc3_test_vectors\nand commit the updated docs/icrc3-test-vectors.json."
    );
}

fn serialize_snapshot_pretty(value: &serde_json::Value) -> String {
    use serde::Serialize;
    let mut buf: Vec<u8> = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"    ");
    let mut serializer = serde_json::Serializer::with_formatter(&mut buf, formatter);
    value
        .serialize(&mut serializer)
        .expect("failed to serialize ICRC-3 test vectors");
    buf.push(b'\n');
    String::from_utf8(buf).expect("serde_json produced non-UTF8 output")
}

fn strip_cert(value: &serde_json::Value) -> serde_json::Value {
    let mut cloned = value.clone();
    if let Some(vectors) = cloned.get_mut("vectors").and_then(|v| v.as_array_mut()) {
        for vector in vectors {
            if let Some(obj) = vector.as_object_mut() {
                obj.remove("certificate_cbor_hex");
            }
        }
    }
    cloned
}

/// Formats an `Icrc3Value` into a human-readable Candid-style string representation.
fn format_icrc3_value(
    value: &internet_identity_interface::internet_identity::types::icrc3::Icrc3Value,
) -> String {
    use internet_identity_interface::internet_identity::types::icrc3::Icrc3Value;
    match value {
        Icrc3Value::Nat(n) => format!("Nat({})", n),
        Icrc3Value::Int(i) => format!("Int({})", i),
        Icrc3Value::Blob(b) => format!("Blob(hex\"{}\")", hex::encode(b)),
        Icrc3Value::Text(s) => format!("Text({:?})", s),
        Icrc3Value::Array(items) => {
            let inner: Vec<String> = items.iter().map(format_icrc3_value).collect();
            format!("Array([{}])", inner.join(", "))
        }
        Icrc3Value::Map(entries) => {
            let inner: Vec<String> = entries
                .iter()
                .map(|(k, v)| format!("{:?}: {}", k, format_icrc3_value(v)))
                .collect();
            format!("Map({{ {} }})", inner.join(", "))
        }
    }
}

/// Builds an integration-test environment whose Google OpenID credential is backed by a
/// JWT signed by an RSA key generated deterministically inside the test (instead of Google).
///
/// The returned tuple mirrors `setup_icrc3_test_env` but additionally exposes the fake email
/// and name so the caller can build `AttributeSpec`s that reference them.
fn setup_icrc3_test_env_with_fake_openid() -> (
    PocketIc,
    Principal, // ii_backend_canister_id
    Principal, // test_principal
    u64,       // identity_number
    String,    // fake email embedded in the JWT
    String,    // fake name embedded in the JWT
) {
    use internet_identity_interface::internet_identity::types::{
        AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
        AuthnMethodSecuritySettings, PublicKeyAuthn,
    };

    let env = env();

    // Synthetic values: no personal info, chosen just for the test vectors.
    let fake_email = "alice.example@icrc3-test.invalid".to_string();
    let fake_name = "Alice Example".to_string();
    let fake_sub = "1000000000000000001".to_string();
    let issuer = "https://accounts.google.com".to_string();
    let client_id =
        "360587991668-63bpc1gngp1s5gbo1aldal4a50c1j0bb.apps.googleusercontent.com".to_string();

    // Deterministic caller: we reuse the public key from `openid_google_test_data` so the
    // principal is the same well-known value, and we pick a fixed salt.
    let salt: [u8; 32] = [42u8; 32];
    let test_pubkey: [u8; 96] = [
        48, 94, 48, 12, 6, 10, 43, 6, 1, 4, 1, 131, 184, 67, 1, 1, 3, 78, 0, 165, 1, 2, 3, 38, 32,
        1, 33, 88, 32, 252, 182, 240, 218, 160, 61, 178, 176, 17, 228, 185, 84, 148, 45, 86, 216,
        171, 120, 72, 246, 212, 55, 212, 167, 142, 59, 227, 0, 242, 182, 129, 211, 34, 88, 32, 158,
        197, 96, 131, 51, 156, 176, 65, 128, 29, 75, 98, 163, 187, 104, 38, 255, 65, 92, 234, 229,
        245, 221, 74, 40, 202, 29, 83, 162, 84, 177, 204,
    ];
    let test_principal = Principal::from_slice(&[
        211, 40, 186, 145, 43, 2, 6, 17, 232, 23, 22, 44, 51, 178, 233, 163, 131, 231, 82, 174, 66,
        201, 203, 1, 102, 109, 20, 75, 2,
    ]);
    let test_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: ByteBuf::from(test_pubkey.to_vec()),
        }),
        metadata: Default::default(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    // Pick a test time around 2027-01-15T00:00:00Z. The JWT `iat` is this time minus 5
    // minutes and `exp` is this time plus one hour, matching typical Google JWTs.
    let test_time_ms: u64 = 1_800_000_000_000;
    let iat_secs: u64 = (test_time_ms / 1_000) - 300;
    let exp_secs: u64 = iat_secs + 3_600;

    let (jwt, jwks_json) = build_fake_google_jwt_and_jwks(FakeJwtInput {
        salt: &salt,
        principal: &test_principal,
        issuer: &issuer,
        aud: &client_id,
        sub: &fake_sub,
        email: &fake_email,
        name: &fake_name,
        email_verified: true,
        iat_secs,
        exp_secs,
    });

    // Install canister with a Google-flavoured OpenID config whose JWKS URL we override below.
    let mut init_args = arg_with_wasm_hash(ARCHIVE_WASM.clone()).unwrap();
    init_args.openid_configs = Some(vec![OpenIdConfig {
        name: "Google".into(),
        logo: "logo".into(),
        issuer: issuer.clone(),
        client_id: client_id.clone(),
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

    // Respond to the canister's JWKS fetch with our fake public key.
    crate::openid::mock_certs_response(
        &env,
        "https://www.googleapis.com/oauth2/v3/certs",
        &jwks_json,
    );

    deploy_archive_via_ii(&env, ii_backend_canister_id);

    let identity_number =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method(
            &env,
            ii_backend_canister_id,
            &test_authn_method,
        );

    let time_to_advance = Duration::from_millis(test_time_ms) - Duration::from_nanos(time(&env));
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

    (
        env,
        ii_backend_canister_id,
        test_principal,
        identity_number,
        fake_email,
        fake_name,
    )
}

struct FakeJwtInput<'a> {
    salt: &'a [u8; 32],
    principal: &'a Principal,
    issuer: &'a str,
    aud: &'a str,
    sub: &'a str,
    email: &'a str,
    name: &'a str,
    email_verified: bool,
    iat_secs: u64,
    exp_secs: u64,
}

/// Generates an RSA-2048 key pair from a fixed seed, signs a synthetic JWT, and returns both
/// the JWT (compact serialization) and a JWKS JSON document containing the matching public key.
///
/// The JWT `nonce` is computed as `BASE64_URL_SAFE_NO_PAD(SHA256(salt || caller_principal_bytes))`,
/// matching the binding that II's generic OpenID provider enforces.
fn build_fake_google_jwt_and_jwks(input: FakeJwtInput) -> (String, String) {
    use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
    use rand_chacha::rand_core::SeedableRng;
    use rsa::pkcs1v15::SigningKey;
    use rsa::signature::{RandomizedSigner, SignatureEncoding};
    use rsa::traits::PublicKeyParts;
    use rsa::{RsaPrivateKey, RsaPublicKey};
    use serde_json::json;
    use sha2::{Digest, Sha256};

    // Deterministic RSA keygen. 2048 bits matches the RSA key size Google uses for its
    // JWT signing keys. The seed is arbitrary.
    let mut rng = rand_chacha::ChaCha20Rng::seed_from_u64(0xDEAD_BEEF_CAFE_BABE_u64);
    let private_key = RsaPrivateKey::new(&mut rng, 2048).expect("failed to generate test RSA key");
    let public_key = RsaPublicKey::from(&private_key);

    // Compute the nonce the canister expects:
    //   BASE64URL_NO_PAD(SHA256(salt || caller_principal_bytes))
    let mut hasher = Sha256::new();
    hasher.update(input.salt);
    hasher.update(input.principal.as_slice());
    let nonce_hash: [u8; 32] = hasher.finalize().into();
    let nonce = URL_SAFE_NO_PAD.encode(nonce_hash);

    // Build the JWT header and claims.
    let kid = "icrc3-test-key-1";
    let header = json!({
        "alg": "RS256",
        "kid": kid,
        "typ": "JWT",
    });
    let claims = json!({
        "iss": input.issuer,
        "azp": input.aud,
        "aud": input.aud,
        "sub": input.sub,
        "email": input.email,
        "email_verified": input.email_verified,
        "nonce": nonce,
        "nbf": input.iat_secs,
        "name": input.name,
        "given_name": input.name.split_whitespace().next().unwrap_or(""),
        "family_name": input.name.split_whitespace().nth(1).unwrap_or(""),
        "iat": input.iat_secs,
        "exp": input.exp_secs,
        "jti": "icrc3-test-vectors-jti-0001",
    });

    let header_b64 = URL_SAFE_NO_PAD.encode(serde_json::to_vec(&header).unwrap());
    let claims_b64 = URL_SAFE_NO_PAD.encode(serde_json::to_vec(&claims).unwrap());
    let signing_input = format!("{header_b64}.{claims_b64}");

    let signing_key = SigningKey::<Sha256>::new(private_key);
    let signature = signing_key.sign_with_rng(&mut rng, signing_input.as_bytes());
    let signature_b64 = URL_SAFE_NO_PAD.encode(signature.to_bytes());

    let jwt = format!("{signing_input}.{signature_b64}");

    // Build JWKS with the matching public key.
    let n_b64 = URL_SAFE_NO_PAD.encode(public_key.n().to_bytes_be());
    let e_b64 = URL_SAFE_NO_PAD.encode(public_key.e().to_bytes_be());
    let jwks = json!({
        "keys": [{
            "kty": "RSA",
            "use": "sig",
            "alg": "RS256",
            "kid": kid,
            "n": n_b64,
            "e": e_b64,
        }]
    });

    (jwt, serde_json::to_string(&jwks).unwrap())
}

#[test]
fn should_return_error_for_unavailable_icrc3_attributes() {
    let (env, canister_id, principal, identity_number) = setup_icrc3_test_env();
    let origin = "https://some-dapp.com";

    // Request unavailable verified_email and unknown issuer alongside available email
    let prepare_request = PrepareIcrc3AttributeRequest {
        identity_number,
        origin: origin.to_string(),
        account_number: None,
        attributes: vec![
            AttributeSpec {
                key: "openid:https://accounts.google.com:email".into(),
                value: None,
                omit_scope: false,
            },
            // The test user has no verified_email
            AttributeSpec {
                key: "openid:https://accounts.google.com:verified_email".into(),
                value: None,
                omit_scope: false,
            },
            // Unknown issuer
            AttributeSpec {
                key: "openid:https://unknown-issuer.com:email".into(),
                value: None,
                omit_scope: false,
            },
        ],
        nonce: vec![0u8; 32],
    };

    let result = api::prepare_icrc3_attributes(&env, canister_id, principal, prepare_request)
        .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
            assert_eq!(problems.len(), 2, "Expected 2 problems, got {:?}", problems);
            assert!(
                problems.iter().any(|p| p.contains("verified_email")),
                "Expected a problem about verified_email, got {:?}",
                problems
            );
            assert!(
                problems.iter().any(|p| p.contains("unknown-issuer.com")),
                "Expected a problem about unknown issuer, got {:?}",
                problems
            );
        }
        other => panic!("Expected AttributeMismatch error, got {:?}", other),
    }
}
