use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, II_WASM,
};
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodMetadataReplaceError, MetadataEntryV2,
};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

#[test]
fn should_write_authn_method_metadata() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_metadata = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .metadata;
    assert!(actual_metadata.is_empty());

    let metadata = HashMap::from([(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("some value".to_string()),
    )]);

    api_v2::authn_method_metadata_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &authn_method.public_key(),
        &metadata,
    )?
    .expect("identity metadata replace failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let actual_metadata = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .metadata;
    assert_eq!(actual_metadata, &metadata);
    Ok(())
}

#[test]
fn should_require_authentication_to_replace_identity_metadata() {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let metadata = HashMap::from_iter(vec![(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("some value".to_string()),
    )]);

    let result = api_v2::authn_method_metadata_replace(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &authn_method.public_key(),
        &metadata,
    );
    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

#[test]
fn should_not_write_too_large_identity_metadata_map() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.metadata.is_empty());

    let metadata = HashMap::from([(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("a".repeat(3000)),
    )]);

    let result = api_v2::authn_method_metadata_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &authn_method.public_key(),
        &metadata,
    );
    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("failed to modify device: Cumulative size of variable sized fields exceeds limit: length \\d+, limit \\d+\\.").unwrap(),
    );
    Ok(())
}

#[test]
fn should_not_allow_reserved_metadata_keys() -> Result<(), CallError> {
    const RESERVED_KEYS: &[&str] = &[
        "pubkey",
        "credential_id",
        "purpose",
        "key_type",
        "protection",
        "last_usage_timestamp",
        "metadata",
    ];

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    for reserved_key in RESERVED_KEYS {
        let metadata = HashMap::from([(
            reserved_key.to_string(),
            MetadataEntryV2::String("some value".to_string()),
        )]);

        let result = api_v2::authn_method_metadata_replace(
            &env,
            canister_id,
            authn_method.principal(),
            identity_number,
            &authn_method.public_key(),
            &metadata,
        );
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new(&format!(
                "Metadata key '{}' is reserved and cannot be used\\.",
                reserved_key
            ))
            .unwrap(),
        );
    }
    Ok(())
}

#[test]
fn should_enforce_string_type_for_legacy_keys() -> Result<(), CallError> {
    const RESERVED_KEYS: &[&str] = &["alias", "usage", "authenticator_attachment", "origin"];

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    for reserved_key in RESERVED_KEYS {
        let metadata = HashMap::from([(
            reserved_key.to_string(),
            MetadataEntryV2::Bytes(ByteBuf::from(*b"some value")),
        )]);

        let result = api_v2::authn_method_metadata_replace(
            &env,
            canister_id,
            authn_method.principal(),
            identity_number,
            &authn_method.public_key(),
            &metadata,
        )?;
        assert!(matches!(
            result,
            Err(AuthnMethodMetadataReplaceError::InvalidMetadata(_))
        ));
    }
    Ok(())
}
