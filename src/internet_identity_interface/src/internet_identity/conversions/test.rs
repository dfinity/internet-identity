use crate::internet_identity::types as ii_types;
use crate::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, DeviceProtection, DeviceWithUsage,
    KeyType, MetadataEntry, PublicKeyAuthn, Purpose, WebAuthn,
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

        let converted = AuthnMethodData::from(DeviceData::from(device_with_usage));
        assert_eq!(
            converted,
            AuthnMethodData {
                last_authentication: None,
                ..expected_authn_method_data
            }
        );
    }
}

fn test_conversion_pairs() -> Vec<(DeviceWithUsage, AuthnMethodData)> {
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
            ("alias".to_string(), MetadataEntry::String(alias.clone())),
            ("origin".to_string(), MetadataEntry::String(origin)),
            (
                "some_key".to_string(),
                MetadataEntry::String("some data".to_string()),
            ),
        ]),
        protection: AuthnMethodProtection::Protected,
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
            ("alias".to_string(), MetadataEntry::String(alias.clone())),
            (
                "key_type".to_string(),
                MetadataEntry::String("cross_platform".to_string()),
            ),
            (
                "some_key2".to_string(),
                MetadataEntry::String("some data".to_string()),
            ),
        ]),
        protection: AuthnMethodProtection::Unprotected,
        last_authentication: None,
    };

    let device3 = DeviceWithUsage {
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
            ("alias".to_string(), MetadataEntry::String(alias)),
            (
                "key_type".to_string(),
                MetadataEntry::String("cross_platform".to_string()),
            ),
            (
                "some_key2".to_string(),
                MetadataEntry::String("some data".to_string()),
            ),
        ]),
        protection: AuthnMethodProtection::Unprotected,
        last_authentication: None,
    };

    vec![
        (device1, authn_method_data1),
        (device2, authn_method_data2),
        (device3, authn_method_data3),
    ]
}
