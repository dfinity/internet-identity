use crate::openid::OpenIdCredential;
use crate::storage::anchor::{Anchor, AnchorError, Device};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DeviceData, DeviceProtection, KeyType, MetadataEntry, MetadataEntryV2, Purpose,
    Timestamp,
};
use serde_bytes::ByteBuf;
use std::collections::HashMap;

const TEST_CALLER_PUBKEY: [u8; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

const ANCHOR_NUMBER: AnchorNumber = 10_000;

#[test]
fn should_add_device() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();

    assert_eq!(anchor.devices, vec![sample_device()])
}

#[test]
fn should_remove_device() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();
    assert_eq!(anchor.devices, vec![sample_device()]);

    anchor.remove_device(&sample_device().pubkey).unwrap();

    assert_eq!(anchor.devices, vec![]);
}

#[test]
fn should_modify_device() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let mut device = sample_device();
    anchor.add_device(device.clone()).unwrap();
    device.alias = "modified alias".to_string();

    anchor
        .modify_device(&device.pubkey, device.clone())
        .unwrap();

    assert_eq!(anchor.devices, vec![device]);
}

#[test]
fn should_enforce_max_number_of_devices() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    for i in 0..10 {
        anchor.add_device(device(i)).unwrap();
    }

    let result = anchor.add_device(device(10));

    assert!(matches!(result, Err(AnchorError::TooManyDevices { .. })));
    assert_eq!(anchor.devices().len(), 10);
}

#[test]
fn should_enforce_pubkey_limit() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let mut device = sample_device();
    device.pubkey = ByteBuf::from([0; 301]);

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::DeviceLimitExceeded { .. })
    ));
    assert!(anchor.devices().is_empty());
}

#[test]
fn should_enforce_credential_id_limit() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let mut device = sample_device();
    device.credential_id = Some(ByteBuf::from([0; 351]));

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::DeviceLimitExceeded { .. })
    ));
    assert!(anchor.devices().is_empty());
}

#[test]
fn should_enforce_alias_limit() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let mut device = sample_device();
    device.alias = "a".repeat(65);

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::DeviceLimitExceeded { .. })
    ));
    assert!(anchor.devices().is_empty());
}

#[test]
fn should_enforce_unique_device_keys() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.add_device(sample_device());

    assert!(matches!(result, Err(AnchorError::DuplicateDevice { .. })));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_enforce_cumulative_device_limit() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);

    for i in 0..4 {
        anchor.add_device(large_device(i)).unwrap();
    }
    let device = Device {
        pubkey: Default::default(),
        alias: "a".repeat(64),
        credential_id: Some(ByteBuf::from([0; 100])),
        aaguid: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: None,
        last_usage_timestamp: None,
        metadata: None,
    };

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::CumulativeDataLimitExceeded { .. })
    ));
    assert_eq!(anchor.devices().len(), 4);
}

#[test]
fn should_enforce_cumulative_size_limit_on_identity_metadata() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);

    let metadata = HashMap::from_iter(vec![(
        "some key".to_string(),
        MetadataEntry::String("a".repeat(3000)),
    )]);

    let result = anchor.replace_identity_metadata(metadata);

    assert!(matches!(
        result,
        Err(AnchorError::CumulativeDataLimitExceeded { .. })
    ));
    assert!(anchor.identity_metadata().is_none());
}

#[test]
fn should_enforce_cumulative_size_limit_on_device_and_metadata() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);

    for i in 0..4 {
        anchor.add_device(large_device(i)).unwrap();
    }
    let metadata = HashMap::from_iter(vec![(
        "some key".to_string(),
        MetadataEntry::String("a".repeat(2000)),
    )]);

    let result = anchor.replace_identity_metadata(metadata);

    assert!(matches!(
        result,
        Err(AnchorError::CumulativeDataLimitExceeded { .. })
    ));
    assert!(anchor.identity_metadata().is_none());
}

#[test]
fn should_enforce_single_recovery_phrase() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);

    anchor
        .add_device(recovery_phrase(0, DeviceProtection::Unprotected))
        .unwrap();
    let result = anchor.add_device(recovery_phrase(1, DeviceProtection::Unprotected));

    assert!(matches!(result, Err(AnchorError::MultipleRecoveryPhrases)));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_allow_protection_only_on_recovery_phrases() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);

    let result = anchor.add_device(Device {
        pubkey: Default::default(),
        alias: "".to_string(),
        credential_id: None,
        aaguid: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Protected,
        origin: None,
        last_usage_timestamp: None,
        metadata: None,
    });

    assert!(matches!(
        result,
        Err(AnchorError::InvalidDeviceProtection { .. })
    ));
    assert!(anchor.devices().is_empty());
}

#[test]
fn should_prevent_mutation_when_invariants_are_violated() {
    let mut device1 = recovery_phrase(1, DeviceProtection::Unprotected);
    let mut anchor = Anchor {
        anchor_number: ANCHOR_NUMBER,
        devices: vec![
            device1.clone(),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
        openid_credentials: vec![],
        metadata: None,
        name: None,
        created_at: None,
    };

    device1.alias = "new alias".to_string();
    let result = anchor.modify_device(&device1.pubkey.clone(), device1);
    assert!(matches!(result, Err(AnchorError::MultipleRecoveryPhrases)));
    assert_eq!(anchor.devices().len(), 2);
}

#[test]
fn should_prevent_addition_when_invariants_are_violated() {
    let mut anchor = Anchor {
        anchor_number: ANCHOR_NUMBER,
        devices: vec![
            recovery_phrase(1, DeviceProtection::Unprotected),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
        openid_credentials: vec![],
        metadata: None,
        name: None,
        created_at: None,
    };

    let result = anchor.add_device(sample_device());
    assert!(matches!(result, Err(AnchorError::MultipleRecoveryPhrases)));
    assert_eq!(anchor.devices().len(), 2);
}

#[test]
fn should_allow_removal_when_invariants_are_violated() {
    let device1 = recovery_phrase(1, DeviceProtection::Unprotected);
    let mut anchor = Anchor {
        anchor_number: ANCHOR_NUMBER,
        devices: vec![
            device1.clone(),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
        openid_credentials: vec![],
        metadata: None,
        name: None,
        created_at: None,
    };

    anchor.remove_device(&device1.pubkey).unwrap();

    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_enforce_caller_on_removal_of_protected_devices() {
    let device1 = recovery_phrase(1, DeviceProtection::Protected);
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(device1.clone()).unwrap();

    let result = anchor.remove_device(&device1.pubkey);

    assert!(matches!(
        result,
        Err(AnchorError::MutationNotAllowed { .. })
    ));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_enforce_caller_on_modification_of_protected_devices() {
    let mut device1 = recovery_phrase(1, DeviceProtection::Protected);
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(device1.clone()).unwrap();

    device1.alias = "new alias".to_string();

    let result = anchor.modify_device(&device1.pubkey.clone(), device1);

    assert!(matches!(
        result,
        Err(AnchorError::MutationNotAllowed { .. })
    ));
    assert_eq!(anchor.devices()[0].alias, "recovery phrase 1");
}

#[test]
fn should_allow_removal_of_protected_device_with_matching_caller() {
    let mut device1 = recovery_phrase(1, DeviceProtection::Protected);
    device1.pubkey = ByteBuf::from(TEST_CALLER_PUBKEY);

    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(device1.clone()).unwrap();

    anchor.remove_device(&device1.pubkey).unwrap();

    assert!(anchor.devices.is_empty());
}

#[test]
fn should_allow_modification_of_protected_device_with_matching_caller() {
    let mut device1 = recovery_phrase(1, DeviceProtection::Protected);
    device1.pubkey = ByteBuf::from(TEST_CALLER_PUBKEY);

    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(device1.clone()).unwrap();

    device1.alias = "new alias".to_string();

    anchor
        .modify_device(&device1.pubkey.clone(), device1)
        .unwrap();

    assert_eq!(anchor.devices()[0].alias, "new alias");
}

#[test]
fn should_not_remove_unknown_device() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.remove_device(&device(1).pubkey);

    assert!(matches!(result, Err(AnchorError::NotFound { .. })));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_not_modify_unknown_device() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.modify_device(&device(1).pubkey, device(1));

    assert!(matches!(result, Err(AnchorError::NotFound { .. })));
    assert_eq!(anchor.devices()[0], sample_device());
}

#[test]
fn should_not_allow_modification_of_device_key() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.modify_device(&sample_device().pubkey, device(1));

    assert!(matches!(result, Err(AnchorError::CannotModifyDeviceKey)));
    assert_eq!(anchor.devices()[0], sample_device());
}

#[test]
fn should_not_allow_to_add_recovery_phrase_with_credential_id() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let device = Device {
        key_type: KeyType::SeedPhrase,
        credential_id: Some(ByteBuf::from(vec![1, 2, 3])),
        ..sample_device()
    };

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::RecoveryPhraseCredentialIdMismatch)
    ));
}

#[test]
fn should_not_allow_to_modify_recovery_phrase_to_add_credential_id() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let mut device = Device {
        key_type: KeyType::SeedPhrase,
        credential_id: None,
        ..sample_device()
    };
    anchor.add_device(device.clone()).unwrap();
    device.credential_id = Some(ByteBuf::from(vec![1, 2, 3]));

    let result = anchor.modify_device(&device.pubkey.clone(), device);

    assert!(matches!(
        result,
        Err(AnchorError::RecoveryPhraseCredentialIdMismatch)
    ));
}

#[test]
fn should_update_timestamp() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let device = sample_device();
    const TIMESTAMP: Timestamp = 7896546556;
    anchor.add_device(device.clone()).unwrap();

    anchor
        .set_device_usage_timestamp(&device.pubkey, TIMESTAMP)
        .unwrap();

    assert_eq!(
        anchor.device(&device.pubkey).unwrap().last_usage_timestamp,
        Some(TIMESTAMP)
    );
}

/// Tests that `apply_data` actually applies all the writeable fields.
#[test]
fn should_apply_all_fields() {
    let mut metadata = HashMap::new();
    metadata.insert(
        "entry1".to_string(),
        MetadataEntry::String("bar".to_string()),
    );
    metadata.insert(
        "entry2".to_string(),
        MetadataEntry::Bytes(ByteBuf::from("foo")),
    );
    metadata.insert("entry3".to_string(), MetadataEntry::Map(metadata.clone()));

    let device_data = DeviceData {
        pubkey: ByteBuf::from("some different public key"),
        alias: "some different alias".to_string(),
        credential_id: Some(ByteBuf::from("some different credential id")),
        aaguid: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::CrossPlatform,
        protection: DeviceProtection::Protected,
        origin: Some("https://some.other.origin".to_string()),
        metadata: Some(metadata.clone()),
    };
    let mut device = sample_device();
    device.apply_device_data(device_data.clone());

    assert_eq!(DeviceData::from(device), device_data);
}

/// Tests that the reserved metadata keys are not allowed to be written to.
#[test]
fn should_not_allow_reserved_metadata_key() {
    const RESERVED_KEYS: [&str; 9] = [
        "pubkey",
        "alias",
        "credential_id",
        "purpose",
        "key_type",
        "protection",
        "origin",
        "last_usage_timestamp",
        "metadata",
    ];

    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    for key in RESERVED_KEYS {
        let mut device = sample_device();
        device.metadata = Some(HashMap::from([(
            key.to_string(),
            MetadataEntry::String("some value".to_string()),
        )]));

        let result = anchor.add_device(device);

        assert!(matches!(
            result,
            Err(AnchorError::ReservedMetadataKey { .. })
        ));
    }
}

#[test]
fn should_add_openid_credential() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let openid_credential_0 = openid_credential(0);
    let openid_credential_1 = openid_credential(1);

    // Check if OpenID credential is added
    assert_eq!(
        anchor.add_openid_credential(openid_credential_0.clone()),
        Ok(())
    );
    assert_eq!(anchor.openid_credentials, vec![openid_credential_0.clone()]);

    // Check if a different OpenID credential is added
    assert_eq!(
        anchor.add_openid_credential(openid_credential_1.clone()),
        Ok(())
    );
    assert_eq!(
        anchor.openid_credentials,
        vec![openid_credential_0.clone(), openid_credential_1]
    );

    // Check if an already registered OpenID credential results in an error
    assert_eq!(
        anchor.add_openid_credential(openid_credential_0.clone()),
        Err(AnchorError::OpenIdCredentialAlreadyRegistered)
    );
}

#[test]
fn should_remove_openid_credential() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let openid_credential_0 = openid_credential(0);
    let openid_credential_1 = openid_credential(1);

    // Check if correct OpenID credential is removed
    anchor
        .add_openid_credential(openid_credential_0.clone())
        .unwrap();
    anchor
        .add_openid_credential(openid_credential_1.clone())
        .unwrap();
    assert_eq!(
        anchor.remove_openid_credential(&openid_credential_1.key()),
        Ok(())
    );
    assert_eq!(anchor.openid_credentials, vec![openid_credential_0.clone()]);

    // Check if removing a non-existent OpenID credential results in an error
    assert_eq!(
        anchor.remove_openid_credential(&openid_credential_1.key()),
        Err(AnchorError::OpenIdCredentialNotFound)
    );
}

#[test]
fn should_update_openid_credential() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    let openid_credential_0 = openid_credential(0);
    let openid_credential_1 = openid_credential(1);
    let mut openid_credential_1_updated = openid_credential_1.clone();
    openid_credential_1_updated.metadata = HashMap::from([(
        "name".to_string(),
        MetadataEntryV2::String("Updated Name".to_string()),
    )]);
    let openid_credential_2 = openid_credential(2);

    // Check if correct OpenID credential is updated
    anchor
        .add_openid_credential(openid_credential_0.clone())
        .unwrap();
    anchor
        .add_openid_credential(openid_credential_1.clone())
        .unwrap();
    assert_eq!(
        anchor.update_openid_credential(openid_credential_1_updated.clone()),
        Ok(())
    );
    assert_eq!(
        anchor.openid_credentials,
        vec![openid_credential_0.clone(), openid_credential_1_updated]
    );

    // Check if updating a non-existent OpenID credential results in an error
    assert_eq!(
        anchor.update_openid_credential(openid_credential_2),
        Err(AnchorError::OpenIdCredentialNotFound)
    );
}

#[test]
fn should_enforce_max_number_of_openid_credentials() {
    let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
    for i in 0..100 {
        anchor.add_openid_credential(openid_credential(i)).unwrap();
    }

    let result = anchor.add_openid_credential(openid_credential(101));
    println!("{result:?}");

    assert!(matches!(
        result,
        Err(AnchorError::TooManyOpenIdCredentials { .. })
    ));
    assert_eq!(anchor.openid_credentials().len(), 100);
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("public key of some sample device"),
        alias: "Test alias".to_string(),
        credential_id: Some(ByteBuf::from("credential id of some sample device")),
        aaguid: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Platform,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://fooo.bar".to_string()),
        last_usage_timestamp: Some(465789),
        metadata: None,
    }
}

fn device(n: u8) -> Device {
    Device {
        pubkey: ByteBuf::from([n; 100]),
        alias: format!("test alias {n}"),
        credential_id: Some(ByteBuf::from([n; 64])),
        aaguid: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Platform,
        protection: DeviceProtection::Unprotected,
        origin: Some(format!("https://foo{n}.bar")),
        last_usage_timestamp: Some(n as u64),
        metadata: None,
    }
}

/// Device with variable fields size of 512 bytes.
fn large_device(n: u8) -> Device {
    Device {
        pubkey: ByteBuf::from(vec![n; 300]),
        alias: "alias12chars".to_string(),
        credential_id: Some(ByteBuf::from(vec![n; 200])),
        aaguid: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://rdmx6-jaaaa-aaaaa-aaadq-cai.foobar.icp0.io".to_string()),
        last_usage_timestamp: Some(12345679),
        metadata: Some(HashMap::from([(
            "key".to_string(),
            MetadataEntry::String("a".repeat(40)),
        )])),
    }
}

fn recovery_phrase(n: u8, protection: DeviceProtection) -> Device {
    Device {
        pubkey: ByteBuf::from(vec![n; 96]),
        alias: format!("recovery phrase {n}"),
        credential_id: None,
        aaguid: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::SeedPhrase,
        protection,
        origin: None,
        last_usage_timestamp: None,
        metadata: None,
    }
}

fn openid_credential(n: u8) -> OpenIdCredential {
    OpenIdCredential {
        iss: "https://example.com".into(),
        sub: n.to_string(),
        aud: "example-aud".into(),
        last_usage_timestamp: Some(n.into()),
        metadata: HashMap::default(),
    }
}

pub fn test_caller() -> Principal {
    Principal::self_authenticating(TEST_CALLER_PUBKEY)
}

#[cfg(test)]
mod from_conversion_tests {
    use super::*;
    use crate::storage::storable::anchor::StorableAnchor;
    use crate::storage::storable::fixed_anchor::StorableFixedAnchor;
    use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
    use crate::storage::storable::recovery_key::StorableRecoveryKey;
    use crate::storage::storable::special_device_migration::SpecialDeviceMigration;
    use pretty_assertions::assert_eq;
    use serde_bytes::ByteBuf;

    /// Verifies that devices with authentication purpose and credential ID are correctly
    /// converted to StorablePasskeyCredential with all fields mapped properly.
    #[test]
    fn should_convert_passkey_credentials_with_authentication_purpose() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let passkey_device = Device {
            pubkey: ByteBuf::from(vec![1, 2, 3]),
            alias: "My Passkey".to_string(),
            credential_id: Some(ByteBuf::from(vec![4, 5, 6])),
            aaguid: Some([7u8; 16]),
            purpose: Purpose::Authentication,
            key_type: KeyType::Platform,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://id.ai".to_string()),
            last_usage_timestamp: Some(987654321),
            metadata: None,
        };
        anchor.add_device(passkey_device.clone()).unwrap();

        let (fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let mut passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 1);

        let StorablePasskeyCredential {
            pubkey,
            credential_id,
            origin,
            created_at_ns,
            last_usage_timestamp_ns,
            alias,
            aaguid,
            special_device_migration,
        } = passkeys.remove(0);

        assert_eq!(pubkey, vec![1, 2, 3]);
        assert_eq!(credential_id, vec![4, 5, 6]);
        assert_eq!(alias, Some("My Passkey".to_string()));
        assert_eq!(origin, "https://id.ai");
        assert_eq!(last_usage_timestamp_ns, Some(987654321));
        assert_eq!(aaguid, Some(vec![7u8; 16]));
        assert_eq!(created_at_ns, None);
        assert_eq!(special_device_migration, None);

        assert_eq!(fixed.devices.len(), 1);
    }

    /// Verifies that passkey devices with recovery purpose are added to the passkey_credentials
    /// list and placed at the end (after authentication passkeys).
    #[test]
    fn should_convert_passkey_credentials_with_recovery_purpose() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let recovery_passkey = Device {
            pubkey: ByteBuf::from(vec![10, 11, 12]),
            alias: "Recovery Passkey".to_string(),
            credential_id: Some(ByteBuf::from(vec![13, 14, 15])),
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::CrossPlatform,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://identity.ic0.app".to_string()),
            last_usage_timestamp: Some(111111111),
            metadata: None,
        };
        anchor.add_device(recovery_passkey.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let mut passkeys = storable.passkey_credentials.unwrap();

        // Recovery passkeys should still be in passkey_credentials list (at the end),
        // not in the list of recovery *keys* (unlike, e.g., recovery phrases).
        assert_eq!(passkeys.len(), 1);

        let StorablePasskeyCredential {
            pubkey,
            credential_id,
            origin,
            created_at_ns,
            last_usage_timestamp_ns,
            alias,
            aaguid,
            special_device_migration,
        } = passkeys.remove(0);

        assert_eq!(pubkey, vec![10, 11, 12]);
        assert_eq!(credential_id, vec![13, 14, 15]);
        assert_eq!(&origin, "https://identity.ic0.app");
        assert_eq!(created_at_ns, None);
        assert_eq!(last_usage_timestamp_ns, Some(111111111));
        assert_eq!(alias, Some("Recovery Passkey".to_string()));
        assert_eq!(aaguid, None);
        assert_eq!(
            special_device_migration,
            Some(SpecialDeviceMigration {
                credential_id: Some(vec![13, 14, 15]),
                purpose: Purpose::Recovery.into(),
                key_type: KeyType::CrossPlatform.into(),
                origin: Some("https://identity.ic0.app".to_string()),
            })
        );
    }

    /// Verifies that seed phrase devices are converted to StorableRecoveryKey and excluded
    /// from the passkey_credentials list.
    #[test]
    fn should_convert_recovery_keys_from_seed_phrase() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let seed_phrase = Device {
            pubkey: ByteBuf::from(vec![20, 21, 22]),
            alias: "Recovery Phrase".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Unprotected,
            origin: None,
            last_usage_timestamp: Some(222222222),
            metadata: None,
        };
        anchor.add_device(seed_phrase.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys.len(), 1);
        assert_eq!(recovery_keys[0].pubkey, vec![20, 21, 22]);
        assert_eq!(recovery_keys[0].last_usage_timestamp_ns, Some(222222222));
        assert_eq!(recovery_keys[0].is_protected, None);
        assert_eq!(recovery_keys[0].created_at_ns, None);

        // Passkey credentials should not include seed phrases
        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 0);
    }

    /// Verifies that DeviceProtection::Protected is correctly converted to is_protected: Some(true)
    /// in the StorableRecoveryKey.
    #[test]
    fn should_convert_protected_recovery_key() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let protected_seed = Device {
            pubkey: ByteBuf::from(vec![30, 31, 32]),
            alias: "Protected Recovery".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Protected,
            origin: None,
            last_usage_timestamp: Some(333333333),
            metadata: None,
        };
        anchor.add_device(protected_seed.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys.len(), 1);
        assert_eq!(recovery_keys[0].is_protected, Some(true));
    }

    /// Verifies that empty device aliases are converted to None in StorablePasskeyCredential
    /// rather than Some("").
    #[test]
    fn should_handle_empty_alias() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let device_empty_alias = Device {
            pubkey: ByteBuf::from(vec![40, 41, 42]),
            alias: "".to_string(),
            credential_id: Some(ByteBuf::from(vec![43, 44, 45])),
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Platform,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://identity.ic0.app".to_string()),
            last_usage_timestamp: None,
            metadata: None,
        };
        anchor.add_device(device_empty_alias.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 1);
        assert_eq!(passkeys[0].alias, None);
    }

    /// Verifies that passkeys without an origin default to "https://identity.ic0.app" for
    /// backward compatibility with legacy devices.
    #[test]
    fn should_use_default_origin_for_passkeys_without_origin() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let device_no_origin = Device {
            pubkey: ByteBuf::from(vec![50, 51, 52]),
            alias: "Old Device".to_string(),
            credential_id: Some(ByteBuf::from(vec![53, 54, 55])),
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Platform,
            protection: DeviceProtection::Unprotected,
            origin: None, // No origin specified
            last_usage_timestamp: Some(444444444),
            metadata: None,
        };
        anchor.add_device(device_no_origin.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 1);
        // Should default to II_LEGACY_ORIGIN
        assert_eq!(passkeys[0].origin, "https://identity.ic0.app");
    }

    /// Verifies correct conversion when an anchor contains multiple device types:
    /// authentication passkeys, recovery passkeys, and seed phrases. Tests proper ordering
    /// and separation into passkey_credentials and recovery_keys.
    #[test]
    fn should_convert_mixed_device_types() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);

        // Add an authentication passkey
        let auth_passkey = Device {
            pubkey: ByteBuf::from(vec![60, 61, 62]),
            alias: "Auth Passkey".to_string(),
            credential_id: Some(ByteBuf::from(vec![63, 64, 65])),
            aaguid: Some([1u8; 16]),
            purpose: Purpose::Authentication,
            key_type: KeyType::Platform,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://identity.ic0.app".to_string()),
            last_usage_timestamp: Some(100),
            metadata: None,
        };
        anchor.add_device(auth_passkey).unwrap();

        // Add a recovery passkey
        let recovery_passkey = Device {
            pubkey: ByteBuf::from(vec![70, 71, 72]),
            alias: "Recovery Device".to_string(),
            credential_id: Some(ByteBuf::from(vec![73, 74, 75])),
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::CrossPlatform,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://identity.ic0.app".to_string()),
            last_usage_timestamp: Some(200),
            metadata: None,
        };
        anchor.add_device(recovery_passkey).unwrap();

        // Add a seed phrase
        let seed_phrase = Device {
            pubkey: ByteBuf::from(vec![80, 81, 82]),
            alias: "Seed Phrase".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Protected,
            origin: None,
            last_usage_timestamp: Some(300),
            metadata: None,
        };
        anchor.add_device(seed_phrase).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        // Check passkey credentials: auth passkey first, then recovery passkey
        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 2);
        assert_eq!(passkeys[0].pubkey, vec![60, 61, 62]); // Auth passkey
        assert_eq!(passkeys[1].pubkey, vec![70, 71, 72]); // Recovery passkey (at the end)

        // Check recovery keys
        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys.len(), 1);
        assert_eq!(recovery_keys[0].pubkey, vec![80, 81, 82]);
        assert_eq!(recovery_keys[0].is_protected, Some(true));
    }

    /// Verifies that OpenID credentials are properly converted and preserved during the
    /// Anchor to StorableAnchor transformation.
    #[test]
    fn should_convert_openid_credentials() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let openid = openid_credential(5);
        anchor.add_openid_credential(openid.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        assert_eq!(storable.openid_credentials.len(), 1);
        assert_eq!(storable.openid_credentials[0].iss, "https://example.com");
        assert_eq!(storable.openid_credentials[0].sub, "5");
    }

    /// Verifies that StorableFixedAnchor correctly preserves the original devices vector,
    /// identity metadata, and created_at timestamp.
    #[test]
    fn should_preserve_fixed_anchor_fields() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let device = sample_device();
        anchor.add_device(device.clone()).unwrap();

        let metadata = HashMap::from([(
            "key1".to_string(),
            MetadataEntry::String("value1".to_string()),
        )]);
        anchor.replace_identity_metadata(metadata.clone()).unwrap();

        let (fixed, _storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        assert_eq!(fixed.devices.len(), 1);
        assert_eq!(fixed.devices[0], device);
        assert_eq!(fixed.metadata, Some(metadata));
        assert_eq!(fixed.created_at, Some(123456789));
    }

    /// Verifies that the anchor name and created_at_ns fields are correctly set in
    /// StorableAnchor during conversion.
    #[test]
    fn should_set_name_and_created_at_in_storable_anchor() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        anchor.set_name(Some("My Identity".to_string())).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        assert_eq!(storable.name, Some("My Identity".to_string()));
        assert_eq!(storable.created_at_ns, Some(123456789));
    }

    /// Verifies that BrowserStorageKey devices without credential_id are migrated to
    /// passkey_credentials with special migration metadata.
    #[test]
    fn should_handle_browser_storage_key_without_credential_id() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);
        let device = Device {
            pubkey: ByteBuf::from(vec![100, 101, 102]),
            alias: "Browser Storage".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::BrowserStorageKey,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://id.ai".to_string()),
            last_usage_timestamp: Some(123456789),
            metadata: None,
        };
        anchor.add_device(device.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        // Should be migrated as passkey with special migration metadata
        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys.len(), 0);

        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 1);
        assert_eq!(
            passkeys[0].special_device_migration,
            Some(SpecialDeviceMigration {
                credential_id: None,
                purpose: Purpose::Authentication.into(),
                key_type: KeyType::BrowserStorageKey.into(),
                origin: Some("https://id.ai".to_string()),
            })
        );
        assert_eq!(passkeys[0].credential_id, vec![0xde, 0xad, 0xbe, 0xef]);
    }

    /// Tests that passkey devices with unknown key type but having a credential_id are
    /// treated as normal passkeys during migration (regardless of the purpose).
    #[test]
    fn should_handle_unknown_key_type_with_credential_id() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);

        for purpose in [Purpose::Authentication, Purpose::Recovery] {
            // Device with unknown key type but has credential_id
            let device = Device {
                pubkey: ByteBuf::from(vec![80, 81, 82, purpose.clone() as u8]),
                alias: "Unknown Key Type Device".to_string(),
                credential_id: Some(ByteBuf::from(vec![83, 84, 85])),
                aaguid: None,
                purpose,
                key_type: KeyType::Unknown,
                protection: DeviceProtection::Unprotected,
                origin: Some("https://id.ai".to_string()),
                last_usage_timestamp: Some(123456789),
                metadata: None,
            };
            anchor.add_device(device.clone()).unwrap();
        }

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        // Device with unknown key type but has credential_id is migrated as passkey.
        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(
            passkeys,
            vec![
                StorablePasskeyCredential {
                    pubkey: vec![80, 81, 82, Purpose::Authentication as u8],
                    credential_id: vec![83, 84, 85],
                    origin: "https://id.ai".to_string(),
                    created_at_ns: None,
                    last_usage_timestamp_ns: Some(123456789),
                    alias: Some("Unknown Key Type Device".to_string()),
                    aaguid: None,
                    // Happy case, therefore no special migration data.
                    special_device_migration: None,
                },
                StorablePasskeyCredential {
                    pubkey: vec![80, 81, 82, Purpose::Recovery as u8],
                    credential_id: vec![83, 84, 85],
                    origin: "https://id.ai".to_string(),
                    created_at_ns: None,
                    last_usage_timestamp_ns: Some(123456789),
                    alias: Some("Unknown Key Type Device".to_string()),
                    aaguid: None,
                    // Special case (recovery passkey); device migration data should be persisted.
                    special_device_migration: Some(SpecialDeviceMigration {
                        credential_id: Some(vec![83, 84, 85]),
                        purpose: Purpose::Recovery.into(),
                        key_type: KeyType::Unknown.into(),
                        origin: Some("https://id.ai".to_string()),
                    }),
                }
            ]
        );

        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys, vec![]);
    }

    /// Verifies that devices without a credential_id and not marked as seed phrase are included
    /// in the recovery_keys list.
    #[test]
    fn should_handle_devices_without_credential_id() {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 123456789);

        // Device without credential_id (not a passkey, but also not a proper seed phrase)
        let device = Device {
            pubkey: ByteBuf::from(vec![90, 91, 92]),
            alias: "Unknown Device".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://id.ai".to_string()),
            last_usage_timestamp: Some(123456789),
            metadata: None,
        };
        anchor.add_device(device.clone()).unwrap();

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        // Device without credential_id is migrated as recovery phrase, so no passkeys are expected.
        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys, vec![]);

        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(
            recovery_keys,
            vec![StorableRecoveryKey {
                pubkey: vec![90, 91, 92],
                created_at_ns: None,
                last_usage_timestamp_ns: Some(123456789),
                is_protected: None,
                special_device_migration: Some(SpecialDeviceMigration {
                    credential_id: None,
                    purpose: Purpose::Authentication.into(),
                    key_type: KeyType::Unknown.into(),
                    origin: Some("https://id.ai".to_string()),
                })
            }]
        );
    }

    /// Verifies that an empty anchor (with no devices) converts to empty passkey_credentials
    /// and recovery_keys vectors.
    #[test]
    fn should_handle_anchor_with_no_devices() {
        let anchor = Anchor::new(ANCHOR_NUMBER, 123456789);

        let (_fixed, storable): (StorableFixedAnchor, StorableAnchor) = anchor.into();

        let passkeys = storable.passkey_credentials.unwrap();
        assert_eq!(passkeys.len(), 0);

        let recovery_keys = storable.recovery_keys.unwrap();
        assert_eq!(recovery_keys.len(), 0);
    }
}
