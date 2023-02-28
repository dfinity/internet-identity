use crate::storage::anchor::{Anchor, AnchorError, Device};
use candid::Principal;
use internet_identity_interface::{DeviceData, DeviceProtection, KeyType, Purpose, Timestamp};
use serde_bytes::ByteBuf;

const TEST_CALLER_PUBKEY: [u8; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

#[test]
fn should_add_device() {
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();

    assert_eq!(anchor.devices, vec![sample_device()])
}

#[test]
fn should_remove_device() {
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();
    assert_eq!(anchor.devices, vec![sample_device()]);

    anchor.remove_device(&sample_device().pubkey).unwrap();

    assert_eq!(anchor.devices, vec![]);
}

#[test]
fn should_modify_device() {
    let mut anchor = Anchor::new();
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
    let mut anchor = Anchor::new();
    for i in 0..10 {
        anchor.add_device(device(i)).unwrap();
    }

    let result = anchor.add_device(device(10));

    assert!(matches!(result, Err(AnchorError::TooManyDevices { .. })));
    assert_eq!(anchor.devices().len(), 10);
}

#[test]
fn should_enforce_pubkey_limit() {
    let mut anchor = Anchor::new();
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
    let mut anchor = Anchor::new();
    let mut device = sample_device();
    device.credential_id = Some(ByteBuf::from([0; 201]));

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::DeviceLimitExceeded { .. })
    ));
    assert!(anchor.devices().is_empty());
}

#[test]
fn should_enforce_alias_limit() {
    let mut anchor = Anchor::new();
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
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.add_device(sample_device());

    assert!(matches!(result, Err(AnchorError::DuplicateDevice { .. })));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_enforce_cumulative_device_limit() {
    let mut anchor = Anchor::new();

    for i in 0..4 {
        anchor.add_device(large_device(i)).unwrap();
    }
    let device = Device {
        pubkey: Default::default(),
        alias: "a".repeat(64),
        credential_id: Some(ByteBuf::from([0; 100])),
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: None,
        last_usage_timestamp: None,
    };

    let result = anchor.add_device(device);

    assert!(matches!(
        result,
        Err(AnchorError::CumulativeDataLimitExceeded { .. })
    ));
    assert_eq!(anchor.devices().len(), 4);
}

#[test]
fn should_enforce_single_recovery_phrase() {
    let mut anchor = Anchor::new();

    anchor
        .add_device(recovery_phrase(0, DeviceProtection::Unprotected))
        .unwrap();
    let result = anchor.add_device(recovery_phrase(1, DeviceProtection::Unprotected));

    assert!(matches!(
        result,
        Err(AnchorError::MultipleRecoveryPhrases { .. })
    ));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_allow_protection_only_on_recovery_phrases() {
    let mut anchor = Anchor::new();

    let result = anchor.add_device(Device {
        pubkey: Default::default(),
        alias: "".to_string(),
        credential_id: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Protected,
        origin: None,
        last_usage_timestamp: None,
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
        devices: vec![
            device1.clone(),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
    };

    device1.alias = "new alias".to_string();
    let result = anchor.modify_device(&device1.pubkey.clone(), device1);
    assert!(matches!(result, Err(AnchorError::MultipleRecoveryPhrases)));
    assert_eq!(anchor.devices().len(), 2);
}

#[test]
fn should_prevent_addition_when_invariants_are_violated() {
    let mut anchor = Anchor {
        devices: vec![
            recovery_phrase(1, DeviceProtection::Unprotected),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
    };

    let result = anchor.add_device(sample_device());
    assert!(matches!(result, Err(AnchorError::MultipleRecoveryPhrases)));
    assert_eq!(anchor.devices().len(), 2);
}

#[test]
fn should_allow_removal_when_invariants_are_violated() {
    let device1 = recovery_phrase(1, DeviceProtection::Unprotected);
    let mut anchor = Anchor {
        devices: vec![
            device1.clone(),
            recovery_phrase(2, DeviceProtection::Unprotected),
        ],
    };

    anchor.remove_device(&device1.pubkey).unwrap();

    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_enforce_caller_on_removal_of_protected_devices() {
    let device1 = recovery_phrase(1, DeviceProtection::Protected);
    let mut anchor = Anchor::new();
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
    let mut anchor = Anchor::new();
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

    let mut anchor = Anchor::new();
    anchor.add_device(device1.clone()).unwrap();

    anchor.remove_device(&device1.pubkey).unwrap();

    assert!(anchor.devices.is_empty());
}

#[test]
fn should_allow_modification_of_protected_device_with_matching_caller() {
    let mut device1 = recovery_phrase(1, DeviceProtection::Protected);
    device1.pubkey = ByteBuf::from(TEST_CALLER_PUBKEY);

    let mut anchor = Anchor::new();
    anchor.add_device(device1.clone()).unwrap();

    device1.alias = "new alias".to_string();

    anchor
        .modify_device(&device1.pubkey.clone(), device1)
        .unwrap();

    assert_eq!(anchor.devices()[0].alias, "new alias");
}

#[test]
fn should_not_remove_unknown_device() {
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.remove_device(&device(1).pubkey);

    assert!(matches!(result, Err(AnchorError::NotFound { .. })));
    assert_eq!(anchor.devices().len(), 1);
}

#[test]
fn should_not_modify_unknown_device() {
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.modify_device(&device(1).pubkey, device(1));

    assert!(matches!(result, Err(AnchorError::NotFound { .. })));
    assert_eq!(anchor.devices()[0], sample_device());
}

#[test]
fn should_not_allow_modification_of_device_key() {
    let mut anchor = Anchor::new();
    anchor.add_device(sample_device()).unwrap();

    let result = anchor.modify_device(&sample_device().pubkey, device(1));

    assert!(matches!(
        result,
        Err(AnchorError::CannotModifyDeviceKey { .. })
    ));
    assert_eq!(anchor.devices()[0], sample_device());
}

#[test]
fn should_update_timestamp() {
    let mut anchor = Anchor::new();
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
    let device_data = DeviceData {
        pubkey: ByteBuf::from("some different public key"),
        alias: "some different alias".to_string(),
        credential_id: Some(ByteBuf::from("some different credential id")),
        purpose: Purpose::Recovery,
        key_type: KeyType::CrossPlatform,
        protection: DeviceProtection::Protected,
        origin: Some("https://some.other.origin".to_string()),
    };
    let mut device = sample_device();
    device.apply_device_data(device_data.clone());

    assert_eq!(DeviceData::from(device), device_data);
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("public key of some sample device"),
        alias: "Test alias".to_string(),
        credential_id: Some(ByteBuf::from("credential id of some sample device")),
        purpose: Purpose::Authentication,
        key_type: KeyType::Platform,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://fooo.bar".to_string()),
        last_usage_timestamp: Some(465789),
    }
}

fn device(n: u8) -> Device {
    Device {
        pubkey: ByteBuf::from([n; 100]),
        alias: format!("test alias {n}"),
        credential_id: Some(ByteBuf::from([n; 64])),
        purpose: Purpose::Authentication,
        key_type: KeyType::Platform,
        protection: DeviceProtection::Unprotected,
        origin: Some(format!("https://foo{n}.bar")),
        last_usage_timestamp: Some(n as u64),
    }
}

/// Device with variable fields size of 512 bytes.
fn large_device(n: u8) -> Device {
    Device {
        pubkey: ByteBuf::from(vec![n; 300]),
        alias: "alias12chars".to_string(),
        credential_id: Some(ByteBuf::from(vec![n; 200])),
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://rdmx6-jaaaa-aaaaa-aaadq-cai.foobar.icp0.io".to_string()),
        last_usage_timestamp: Some(12345679),
    }
}

fn recovery_phrase(n: u8, protection: DeviceProtection) -> Device {
    Device {
        pubkey: ByteBuf::from(vec![n; 96]),
        alias: format!("recovery phrase {n}"),
        credential_id: Some(ByteBuf::from(vec![n; 64])),
        purpose: Purpose::Recovery,
        key_type: KeyType::SeedPhrase,
        protection,
        origin: None,
        last_usage_timestamp: None,
    }
}

pub fn test_caller() -> Principal {
    Principal::self_authenticating(TEST_CALLER_PUBKEY)
}
