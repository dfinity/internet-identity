use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_pubkey_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, expect_user_error_with_message, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, AuthnMethodMetadataReplaceError, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, MetadataEntryV2,
};
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

#[test]
fn should_write_authn_method_metadata() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
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
fn should_replace_authn_method_metadata() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = AuthnMethodData {
        metadata: HashMap::from([
            (
                "recovery_metadata_1".to_string(),
                MetadataEntryV2::String("recovery data 1".to_string()),
            ),
            (
                "origin".to_string(),
                MetadataEntryV2::String("https://identity.ic0.app".to_string()),
            ),
            (
                "usage".to_string(),
                MetadataEntryV2::String("recovery_phrase".to_string()),
            ),
        ]),
        security_settings: AuthnMethodSecuritySettings {
            purpose: AuthnMethodPurpose::Recovery,
            protection: AuthnMethodProtection::Unprotected,
        },
        ..sample_pubkey_authn_method(0)
    };
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    let stored_metadata = &identity_info
        .authn_methods
        .first()
        .expect("expect authn_methods not to be empty")
        .metadata;
    assert_eq!(stored_metadata, &authn_method.metadata);

    let new_metadata = HashMap::from([(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("some value".to_string()),
    )]);

    api_v2::authn_method_metadata_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &authn_method.public_key(),
        &new_metadata,
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
    assert_eq!(actual_metadata, &new_metadata);
    Ok(())
}

#[test]
fn should_require_authentication_to_replace_identity_metadata() {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
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
    // Variable fields limit as it is enforced by the II canister.
    // This limit ensures that users cannot spend all their allocated space on just metadata.
    const VARIABLE_FIELDS_LIMIT: usize = 2500;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.metadata.is_empty());

    let metadata = HashMap::from([(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("a".repeat(VARIABLE_FIELDS_LIMIT + 1)),
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

    // verify that the metadata was not changed
    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.metadata.is_empty());
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
    let canister_id = install_ii_with_archive(&env, None, None);
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
            Regex::new(
                format!(r"Metadata key '{reserved_key}' is reserved and cannot be used\.").as_str(),
            )
            .unwrap(),
        );
    }
    Ok(())
}

#[test]
fn should_enforce_string_type_for_legacy_keys() -> Result<(), CallError> {
    const RESERVED_KEYS: &[&str] = &["alias", "usage", "authenticator_attachment", "origin"];

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
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
