use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::{
    IdentityMetadataReplaceError, MetadataEntryV2,
};
use pocket_ic::CallError;
use std::collections::HashMap;

#[test]
fn should_write_metadata() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.metadata.is_empty());

    let metadata = HashMap::from_iter(vec![(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("some value".to_string()),
    )]);

    api_v2::identity_metadata_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &metadata,
    )?
    .expect("identity metadata replace failed");

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert_eq!(identity_info.metadata, metadata);
    Ok(())
}

#[test]
fn should_require_authentication_to_replace_identity_metadata() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let metadata = HashMap::from_iter(vec![(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("some value".to_string()),
    )]);

    let result = api_v2::identity_metadata_replace(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &metadata,
    )?;
    assert!(matches!(
        result,
        Err(IdentityMetadataReplaceError::Unauthorized(_))
    ));
    Ok(())
}

#[test]
fn should_not_write_too_large_identity_metadata_map() -> Result<(), CallError> {
    const METADATA_KEY: &str = "some-key";

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    let identity_info =
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_number)?
            .expect("identity info failed");
    assert!(identity_info.metadata.is_empty());

    let metadata = HashMap::from_iter(vec![(
        METADATA_KEY.to_string(),
        MetadataEntryV2::String("a".repeat(3000)),
    )]);

    let result = api_v2::identity_metadata_replace(
        &env,
        canister_id,
        authn_method.principal(),
        identity_number,
        &metadata,
    )?;
    assert!(matches!(
        result,
        Err(IdentityMetadataReplaceError::StorageSpaceExceeded { .. })
    ));
    Ok(())
}
