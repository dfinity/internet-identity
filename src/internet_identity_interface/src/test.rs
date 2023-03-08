use crate::{DeviceData, DeviceProtection, KeyType, Purpose, WebAuthnCredential};
use serde_bytes::ByteBuf;

#[test]
fn should_get_webauthn_credential_from_device_with_credential_id() {
    let device_data = DeviceData {
        pubkey: ByteBuf::from("public key"),
        alias: "alias".to_string(),
        credential_id: Some(ByteBuf::from("credential id")),
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Protected,
        origin: None,
    };

    let credential = WebAuthnCredential::try_from(device_data);
    assert!(credential.is_ok())
}

#[test]
fn should_not_get_webauthn_credential_from_device_without_credential_id() {
    let device_data = DeviceData {
        pubkey: ByteBuf::from("public key"),
        alias: "alias".to_string(),
        credential_id: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Protected,
        origin: None,
    };

    let credential = WebAuthnCredential::try_from(device_data);
    assert!(credential.is_err())
}
