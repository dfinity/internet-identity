use crate::internet_identity::types as ii_types;
use ii_types::{DeviceData, WebAuthnCredential};
use serde_bytes::ByteBuf;

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
