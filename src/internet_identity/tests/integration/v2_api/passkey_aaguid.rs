use crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method;
use candid::Principal;
use canister_tests::api::internet_identity::api_v2::authn_method_add;
use canister_tests::api::internet_identity::api_v2::authn_method_confirm;
use canister_tests::api::internet_identity::api_v2::authn_method_registration_mode_enter;
use canister_tests::api::internet_identity::api_v2::authn_method_registration_mode_exit;
use canister_tests::api::internet_identity::api_v2::authn_method_remove;
use canister_tests::api::internet_identity::api_v2::authn_method_replace;
use canister_tests::api::internet_identity::api_v2::authn_method_session_register;
use canister_tests::api::internet_identity::api_v2::identity_info;
use canister_tests::framework::*;
use pretty_assertions::assert_eq;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::RejectResponse;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

fn test_aaguid() -> Aaguid {
    [
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
        0x10,
    ]
}

fn test_pubkey(n: u8) -> ByteBuf {
    ByteBuf::from(vec![n; 32])
}

fn test_credential_id(n: u8) -> Option<ByteBuf> {
    Some(ByteBuf::from(vec![n; 32]))
}

/// Verifies that set and get aaguid via identity_registration_finish works correctly
#[test]
fn should_set_and_get_aaguid_via_identity_registration_finish() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let aaguid = test_aaguid();

    // Create identity with passkey including AAGUID
    let authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(1),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: Some(aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    // Get identity info and verify AAGUID
    let identity_info = identity_info(&env, canister_id, authn_method.principal(), identity_number)
        .unwrap()
        .unwrap();

    assert_eq!(identity_info.authn_methods.len(), 1);

    if let AuthnMethod::WebAuthn(webauthn) = &identity_info.authn_methods[0].authn_method {
        assert_eq!(webauthn.aaguid, Some(aaguid.to_vec()));
    } else {
        panic!("Expected WebAuthn authentication method");
    }
}

/// Verifies that set and get aaguid via authn_method_add works correctly
#[test]
fn should_set_and_get_aaguid_via_authn_method_add() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let aaguid = test_aaguid();

    // First create an identity without AAGUID
    let initial_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(1),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: None,
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &initial_authn_method);

    // Add new device with AAGUID
    let new_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(2),
            credential_id: test_credential_id(2).unwrap(),
            aaguid: Some(aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    authn_method_add(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
        &new_authn_method,
    )
    .unwrap()
    .unwrap();

    // Verify AAGUID is stored
    let info = identity_info(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
    )
    .unwrap()
    .unwrap();

    assert_eq!(info.authn_methods.len(), 2);
    let second_method = info
        .authn_methods
        .iter()
        .find(|m| {
            if let AuthnMethod::WebAuthn(wa) = &m.authn_method {
                wa.pubkey == test_pubkey(2)
            } else {
                false
            }
        })
        .expect("Second authentication method not found");

    if let AuthnMethod::WebAuthn(webauthn) = &second_method.authn_method {
        assert_eq!(webauthn.aaguid, Some(aaguid.to_vec()));
    } else {
        panic!("Expected WebAuthn authentication method");
    }
}

/// Verifies that set and get aaguid via authn_method_replace works correctly
#[test]
fn should_set_and_get_aaguid_via_authn_method_replace() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let old_aaguid = test_aaguid();
    let new_aaguid = [0xff; 16];
    assert_ne!(old_aaguid, new_aaguid);

    // Create identity with device having AAGUID
    let initial_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(1),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: Some(old_aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &initial_authn_method);

    // Replace with new device having different AAGUID
    let replacement_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(2),
            credential_id: test_credential_id(2).unwrap(),
            aaguid: Some(new_aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    authn_method_replace(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
        &test_pubkey(1),
        &replacement_authn_method,
    )
    .unwrap()
    .unwrap();

    // Verify new AAGUID is stored
    let info = identity_info(
        &env,
        canister_id,
        replacement_authn_method.principal(),
        identity_number,
    )
    .unwrap()
    .unwrap();

    assert_eq!(info.authn_methods.len(), 1);

    if let AuthnMethod::WebAuthn(webauthn) = &info.authn_methods[0].authn_method {
        assert_eq!(webauthn.aaguid, Some(new_aaguid.to_vec()));
        assert_ne!(webauthn.aaguid, Some(old_aaguid.to_vec()));
    } else {
        panic!("Expected WebAuthn authentication method");
    }
}

/// Verifies that set and get aaguid via authn_method_registration_mode_exit works correctly
#[test]
fn should_set_and_get_aaguid_via_authn_method_registration_mode_exit() -> Result<(), RejectResponse>
{
    let registration_mode_id = "0fZr4".to_string();
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let aaguid = test_aaguid();

    // Create identity without AAGUID
    let initial_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(3),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: None,
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number =
        create_identity_with_authn_method(&env, canister_id, &initial_authn_method);

    let info = identity_info(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
    )?
    .unwrap();
    assert_eq!(info.authn_methods.len(), 1);

    // Enter registration mode
    authn_method_registration_mode_enter(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
        Some(registration_mode_id),
    )
    .unwrap()
    .unwrap();

    let intermediate_sender = Principal::self_authenticating([7; 32]);

    let AuthnMethodConfirmationCode {
        confirmation_code,
        expiration: _,
    } = authn_method_session_register(&env, canister_id, intermediate_sender, identity_number)?
        .unwrap();

    authn_method_confirm(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
        &confirmation_code,
    )
    .unwrap()
    .unwrap();

    // Add device via registration mode with AAGUID
    let new_authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(2),
            credential_id: test_credential_id(2).unwrap(),
            aaguid: Some(aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    authn_method_registration_mode_exit(
        &env,
        canister_id,
        intermediate_sender,
        identity_number,
        Some(new_authn_method),
    )
    .unwrap()
    .unwrap();

    // Verify AAGUID is stored
    let info = identity_info(
        &env,
        canister_id,
        initial_authn_method.principal(),
        identity_number,
    )?
    .unwrap();

    assert_eq!(info.authn_methods.len(), 2);
    let added_method = info
        .authn_methods
        .iter()
        .find(|m| {
            if let AuthnMethod::WebAuthn(wa) = &m.authn_method {
                wa.pubkey == test_pubkey(2)
            } else {
                false
            }
        })
        .expect("New authentication method not found");

    if let AuthnMethod::WebAuthn(webauthn) = &added_method.authn_method {
        assert_eq!(webauthn.aaguid, Some(aaguid.to_vec()));
    } else {
        panic!("Expected WebAuthn authentication method");
    }

    Ok(())
}

#[test]
fn should_get_aaguid_via_identity_info() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let aaguid = test_aaguid();

    // Create identity with AAGUID
    let authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(1),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: Some(aaguid.to_vec()),
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    // Query via identity_info to verify AAGUID is stored
    let info = identity_info(&env, canister_id, authn_method.principal(), identity_number)
        .unwrap()
        .unwrap();

    assert_eq!(info.authn_methods.len(), 1);

    if let AuthnMethod::WebAuthn(webauthn) = &info.authn_methods[0].authn_method {
        assert_eq!(webauthn.aaguid, Some(aaguid.to_vec()));
    } else {
        panic!("Expected WebAuthn authentication method");
    }
}

#[test]
fn should_handle_missing_aaguid_gracefully() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    // Create identity without AAGUID
    let authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: test_pubkey(1),
            credential_id: test_credential_id(1).unwrap(),
            aaguid: None,
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    // Verify no AAGUID is present
    let info = identity_info(&env, canister_id, authn_method.principal(), identity_number)
        .unwrap()
        .unwrap();

    let authn_method_data = &info.authn_methods[0];

    if let AuthnMethod::WebAuthn(webauthn) = &authn_method_data.authn_method {
        assert_eq!(webauthn.aaguid, None);
    } else {
        panic!("Expected WebAuthn authentication method");
    }
}

#[test]
fn should_enforce_unique_passkey_pubkeys_and_free_them_on_change_and_remove(
) -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let webauthn_authn_method = |pubkey: ByteBuf, credential_id: ByteBuf| AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey,
            credential_id,
            aaguid: None,
        }),
        metadata: HashMap::new(),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    // 1. Register an anchor with a passkey (anchor A0 with pubkey P0).
    let p0 = test_pubkey(1);
    let a0_initial = webauthn_authn_method(p0.clone(), test_credential_id(1).unwrap());
    let identity_a0 = create_identity_with_authn_method(&env, canister_id, &a0_initial);

    // 2. Another anchor cannot be registered with a passkey with the same pubkey.
    let a1_same_p0 = webauthn_authn_method(p0.clone(), test_credential_id(2).unwrap());
    let result_a1 =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method_and_name(
            &env,
            canister_id,
            &a1_same_p0,
            None,
        );
    assert_eq!(
        result_a1,
        Err("InvalidAuthnMethod(\"passkey with this public key is already used\")".to_string())
    );

    // 3. This anchor's passkey can be updated to have a different pubkey (P1).
    let p1 = test_pubkey(2);
    let a0_replacement = webauthn_authn_method(p1.clone(), test_credential_id(3).unwrap());
    authn_method_replace(
        &env,
        canister_id,
        a0_initial.principal(),
        identity_a0,
        &p0,
        &a0_replacement,
    )?
    .expect("authn_method_replace failed");

    // 4a. Changing a passkey frees up its pubkey for other devices in the same anchor.
    let a0_reuse_p0_same_anchor = webauthn_authn_method(p0.clone(), test_credential_id(4).unwrap());
    authn_method_add(
        &env,
        canister_id,
        a0_replacement.principal(),
        identity_a0,
        &a0_reuse_p0_same_anchor,
    )?
    .expect("authn_method_add with freed pubkey on same anchor failed");

    // Remove the re-used device again so that P0 becomes globally free for the next step.
    authn_method_remove(
        &env,
        canister_id,
        a0_replacement.principal(),
        identity_a0,
        &p0,
    )?
    .expect("authn_method_remove failed");

    // 4b. Changing a passkey frees up its pubkey for other devices in distinct anchors.
    let result_a1_after_change =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method_and_name(
            &env,
            canister_id,
            &a1_same_p0,
            None,
        );
    assert!(
        result_a1_after_change.is_ok(),
        "identity registration with freed passkey pubkey on another anchor should succeed"
    );

    // 5. Removing a passkey frees up its pubkey for other devices (same and distinct anchors).

    // Create an anchor B0 with two devices: the passkey we care about (Q0) and another passkey (Q1)
    // which will be used for authorization after Q0 is removed.
    let q0 = test_pubkey(3);
    let q1 = test_pubkey(4);
    let b0_primary_q0 = webauthn_authn_method(q0.clone(), test_credential_id(5).unwrap());
    let b0_secondary_q1 = webauthn_authn_method(q1.clone(), test_credential_id(6).unwrap());
    let identity_b0 = crate::v2_api::authn_method_test_helpers::create_identity_with_authn_methods(
        &env,
        canister_id,
        &[b0_primary_q0.clone(), b0_secondary_q1.clone()],
    );

    // Show that Q0 cannot be used for a different anchor while it is still present on B0.
    let c0_same_q0 = webauthn_authn_method(q0.clone(), test_credential_id(7).unwrap());
    let result_c0 =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method_and_name(
            &env,
            canister_id,
            &c0_same_q0,
            None,
        );
    assert_eq!(
        result_c0,
        Err("InvalidAuthnMethod(\"passkey with this public key is already used\")".to_string())
    );

    // Remove Q0 from B0 using the secondary device Q1 for authorization.
    authn_method_remove(
        &env,
        canister_id,
        b0_secondary_q1.principal(),
        identity_b0,
        &q0,
    )?
    .expect("authn_method_remove for Q0 on B0 failed");

    // 5a. After removal, Q0 can be added again as a new device on the same anchor B0.
    let b0_readd_q0 = webauthn_authn_method(q0.clone(), test_credential_id(8).unwrap());
    authn_method_add(
        &env,
        canister_id,
        b0_secondary_q1.principal(),
        identity_b0,
        &b0_readd_q0,
    )?
    .expect("authn_method_add with freed pubkey on same anchor after removal failed");

    // Remove Q0 again from B0 so that Q0 is no longer used by any anchor.
    authn_method_remove(
        &env,
        canister_id,
        b0_secondary_q1.principal(),
        identity_b0,
        &q0,
    )?
    .expect("second authn_method_remove for Q0 on B0 failed");

    // 5b. After removal from all anchors, Q0 can be used to register a new anchor.
    let result_c0_after_removal =
        crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method_and_name(
            &env,
            canister_id,
            &c0_same_q0,
            None,
        );
    assert!(
        result_c0_after_removal.is_ok(),
        "identity registration with a passkey pubkey that was removed from all anchors should succeed"
    );

    Ok(())
}
