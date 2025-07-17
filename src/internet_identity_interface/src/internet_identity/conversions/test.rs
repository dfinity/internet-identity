use crate::internet_identity::conversions::AuthnMethodConversionError;
use crate::internet_identity::types as ii_types;
use crate::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, DeviceProtection, DeviceWithUsage, KeyType, MetadataEntry,
    MetadataEntryV2, PublicKeyAuthn, Purpose, WebAuthn,
};
use ii_types::{DeviceData, WebAuthnCredential};
use serde_bytes::ByteBuf;
use std::collections::HashMap;

#[test]
fn should_get_webauthn_credential_from_device_with_credential_id() {
    let device_data = DeviceData {
        credential_id: Some(ByteBuf::from("credential id")),
        ..DeviceData::auth_test_device()
    };

    let credential = WebAuthnCredential::try_from(device_data);
    assert!(credential.is_ok())
}

#[test]
fn should_not_get_webauthn_credential_from_device_without_credential_id() {
    let device_data = DeviceData {
        credential_id: None,
        ..DeviceData::auth_test_device()
    };

    let credential = WebAuthnCredential::try_from(device_data);
    assert!(credential.is_err())
}

#[test]
fn should_convert_device_data_to_authn_method_data() {
    for (device_with_usage, expected_authn_method_data) in test_conversion_pairs() {
        let converted = AuthnMethodData::from(device_with_usage.clone());
        assert_eq!(converted, expected_authn_method_data);

        assert_eq!(
            AuthnMethodData::from(DeviceData::from(device_with_usage.clone())),
            AuthnMethodData {
                last_authentication: None,
                ..expected_authn_method_data
            }
        );

        let converted_back = DeviceWithUsage::try_from(converted).unwrap();
        assert_eq!(converted_back, device_with_usage);
    }
}

#[test]
fn should_fail_to_convert_to_device_on_bad_metadata_types() {
    const KEYS: &[&str] = &["alias", "origin", "usage", "authenticator_attachment"];

    for key in KEYS {
        let (_, mut authn_method) = test_conversion_pairs().pop().unwrap();
        authn_method.metadata.insert(
            key.to_string(),
            MetadataEntryV2::Bytes(ByteBuf::from([1, 2, 3])),
        );
        assert_eq!(
            DeviceWithUsage::try_from(authn_method).unwrap_err(),
            AuthnMethodConversionError::InvalidMetadataType {
                key: key.to_string(),
                expected_type: "string".to_string(),
                actual_value: "Bytes([1, 2, 3])".to_string(),
            }
        );
    }
}

#[test]
fn should_convert_authn_method_purpose() {
    let authn_method_purpose = AuthnMethodPurpose::Authentication;
    let purpose = Purpose::from(authn_method_purpose.clone());
    assert_eq!(purpose, Purpose::Authentication);
    assert_eq!(AuthnMethodPurpose::from(purpose), authn_method_purpose);

    let authn_method_purpose = AuthnMethodPurpose::Recovery;
    let purpose = Purpose::from(authn_method_purpose.clone());
    assert_eq!(purpose, Purpose::Recovery);
    assert_eq!(AuthnMethodPurpose::from(purpose), authn_method_purpose);
}

#[test]
fn should_convert_metadata_entry() {
    let test_string = "some data";
    let test_bytes = ByteBuf::from(*b"some data");

    let entry_v2 = MetadataEntryV2::String(test_string.to_string());
    let entry = MetadataEntry::from(entry_v2.clone());
    assert_eq!(entry, MetadataEntry::String(test_string.to_string()));
    assert_eq!(MetadataEntryV2::from(entry), entry_v2);

    let entry_v2 = MetadataEntryV2::Bytes(test_bytes.clone());
    let entry = MetadataEntry::from(entry_v2.clone());
    assert_eq!(entry, MetadataEntry::Bytes(test_bytes));
    assert_eq!(MetadataEntryV2::from(entry), entry_v2);

    let entry_v2 = MetadataEntryV2::Map(HashMap::new());
    let entry = MetadataEntry::from(entry_v2.clone());
    assert_eq!(entry, MetadataEntry::Map(HashMap::new()));
    assert_eq!(MetadataEntryV2::from(entry), entry_v2);
}

fn test_conversion_pairs() -> Vec<(DeviceWithUsage, AuthnMethodData)> {
    const ORIGIN: &str = "origin";
    const ALIAS: &str = "alias";
    const AUTHENTICATOR_ATTACHMENT: &str = "authenticator_attachment";
    const USAGE: &str = "usage";

    let pubkey = ByteBuf::from([123; 32]);
    let alias = "test device".to_string();
    let origin = "https://some.origin.com".to_string();
    let credential_id = ByteBuf::from([47; 48]);
    let device1 = DeviceWithUsage {
        pubkey: pubkey.clone(),
        alias: alias.clone(),
        credential_id: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Protected,
        origin: Some(origin.clone()),
        last_usage: Some(123456789),
        metadata: Some(HashMap::from([(
            "some_key".to_string(),
            MetadataEntry::String("some data".to_string()),
        )])),
    };
    let authn_method_data1 = AuthnMethodData {
        authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
            pubkey: pubkey.clone(),
        }),
        metadata: HashMap::from([
            (ALIAS.to_string(), MetadataEntryV2::String(alias.clone())),
            (ORIGIN.to_string(), MetadataEntryV2::String(origin.clone())),
            (
                "some_key".to_string(),
                MetadataEntryV2::String("some data".to_string()),
            ),
        ]),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Protected,
            purpose: AuthnMethodPurpose::Recovery,
        },
        last_authentication: Some(123456789),
    };
    let device2 = DeviceWithUsage {
        pubkey: pubkey.clone(),
        alias: alias.clone(),
        credential_id: Some(credential_id.clone()),
        purpose: Purpose::Authentication,
        key_type: KeyType::CrossPlatform,
        protection: DeviceProtection::Unprotected,
        origin: None,
        last_usage: None,
        metadata: Some(HashMap::from([(
            "some_key2".to_string(),
            MetadataEntry::String("some data".to_string()),
        )])),
    };
    let authn_method_data2 = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: pubkey.clone(),
            credential_id: credential_id.clone(),
        }),
        metadata: HashMap::from([
            (ALIAS.to_string(), MetadataEntryV2::String(alias.clone())),
            (
                AUTHENTICATOR_ATTACHMENT.to_string(),
                MetadataEntryV2::String("cross_platform".to_string()),
            ),
            (
                "some_key2".to_string(),
                MetadataEntryV2::String("some data".to_string()),
            ),
        ]),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let device3 = DeviceWithUsage {
        alias: "".to_string(),
        metadata: Some(HashMap::from([(
            "some_key2".to_string(),
            MetadataEntry::String("some data".to_string()),
        )])),
        ..device2.clone()
    };
    let authn_method_data3 = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey,
            credential_id,
        }),
        metadata: HashMap::from([
            (
                AUTHENTICATOR_ATTACHMENT.to_string(),
                MetadataEntryV2::String("cross_platform".to_string()),
            ),
            (
                "some_key2".to_string(),
                MetadataEntryV2::String("some data".to_string()),
            ),
        ]),
        security_settings: AuthnMethodSecuritySettings {
            purpose: AuthnMethodPurpose::Authentication,
            protection: AuthnMethodProtection::Unprotected,
        },
        last_authentication: None,
    };

    let device4 = DeviceWithUsage {
        key_type: KeyType::SeedPhrase,
        ..device1.clone()
    };
    let mut authn_method_data4 = authn_method_data1.clone();
    authn_method_data4.metadata.insert(
        USAGE.to_string(),
        MetadataEntryV2::String("recovery_phrase".to_string()),
    );

    let device5 = DeviceWithUsage {
        key_type: KeyType::BrowserStorageKey,
        ..device1.clone()
    };
    let mut authn_method_data5 = authn_method_data1.clone();
    authn_method_data5.metadata.insert(
        USAGE.to_string(),
        MetadataEntryV2::String("browser_storage_key".to_string()),
    );

    vec![
        (device1, authn_method_data1),
        (device2, authn_method_data2),
        (device3, authn_method_data3),
        (device4, authn_method_data4),
        (device5, authn_method_data5),
    ]
}
