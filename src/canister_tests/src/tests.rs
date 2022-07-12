use crate::framework::device_data_1;
use crate::{api, flows, framework};
use ic_state_machine_tests::StateMachine;

#[test]
fn ii_canister_can_be_installed() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_works() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_retains_anchors() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

    let retrieved_device_data = api::lookup(&env, canister_id, user_number).expect("lookup failed");

    assert_eq!(retrieved_device_data, vec![device_data_1()]);
}

/// Tests for making sure that any release can be rolled back. This tests stable memory compatibility and pre / post install hooks.
#[cfg(test)]
mod rollback_tests {
    use crate::framework::{device_data_1, device_data_2, principal_1, CallError};
    use crate::{api, flows, framework};
    use ic_state_machine_tests::StateMachine;
    use sdk_ic_types::Principal;
    use serde_bytes::ByteBuf;

    /// Tests simple upgrade and downgrade.
    #[test]
    fn ii_canister_can_be_upgraded_and_rolled_back() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
        api::health_check(&env, canister_id);
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM_PREVIOUS.clone());
        api::health_check(&env, canister_id);
    }

    /// Tests that the devices can still be read after upgrade and rollback.
    #[test]
    fn upgrade_and_rollback_keeps_anchor_intact() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let mut devices_before = api::compat::lookup(&env, canister_id, user_number).unwrap();
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
        api::health_check(&env, canister_id);
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM_PREVIOUS.clone());
        api::health_check(&env, canister_id);
        let mut devices_after =
            api::compat::get_anchor_info(&env, canister_id, principal_1(), user_number)
                .unwrap()
                .devices;

        devices_before.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        devices_after.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

        assert_eq!(devices_before, devices_after);
    }

    /// Verifies that an anchor that was created with the new version of II can still be used when
    /// II is rolled back to the previous version.
    #[test]
    fn should_keep_new_anchor_across_rollback() -> Result<(), CallError> {
        let frontend_hostname = "frontend.com";
        let env = StateMachine::new();

        // use the new version to register an anchor
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let principal = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
        )?;

        // roll back
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM_PREVIOUS.clone());

        // use anchor
        let devices = api::compat::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices, [device_data_1()]);

        let (user_key, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            ByteBuf::from("session key"),
            None,
        )?;
        assert_eq!(Principal::self_authenticating(user_key), principal);

        // make sure devices can also be modified
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        Ok(())
    }
}

/// Tests for the user registration flow. The registration process consists of two canister calls:
/// 1. create_challenge: retrieve a captcha
/// 2. register: submit the captcha solution and device information to create a new anchor
#[cfg(test)]
mod registration_tests {
    use crate::framework::{
        device_data_1, expect_user_error_with_message, principal_1, principal_2, CallError,
    };
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode::CanisterCalledTrap;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface::{
        ChallengeAttempt, DeviceProtection, InternetIdentityInit, RegisterResponse,
    };
    use regex::Regex;
    use sdk_ic_types::Principal;
    use std::time::Duration;

    /// Tests user registration with cross checks for lookup, get_anchor_info and get_principal.
    #[test]
    fn should_register_new_anchor() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices, vec![device_data_1()]);
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.devices, vec![device_data_1()]);
        let principal = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            "https://some-frontend.com".to_string(),
        )?;
        assert_ne!(principal, Principal::anonymous());
        Ok(())
    }

    /// Tests that multiple anchors can be registered (even with the same device / keys). This is useful
    /// for users to separate different contexts using multiple anchors.
    #[test]
    fn should_allow_multiple_registrations() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number_1 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());

        assert_ne!(user_number_1, user_number_2);
    }

    /// Tests that the user numbers start at the beginning of the init range and are capped at the end (exclusive).
    #[test]
    fn should_assign_correct_user_numbers() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister_with_arg(
            &env,
            framework::II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: (127, 129),
            }),
        );

        let user_number = flows::register_anchor(&env, canister_id);
        assert_eq!(user_number, 127);

        let user_number = flows::register_anchor(&env, canister_id);
        assert_eq!(user_number, 128);

        let challenge = api::create_challenge(&env, canister_id)?;
        let result = api::register(
            &env,
            canister_id,
            principal_1(),
            &device_data_1(),
            ChallengeAttempt {
                chars: "a".to_string(),
                key: challenge.challenge_key,
            },
        )?;
        assert!(matches!(result, RegisterResponse::CanisterFull));
        Ok(())
    }

    /// Tests that the call to register needs to be signed by the device that is being registered.
    /// This is to make sure that the initial public key belongs to a private key that can be used to sign requests.
    #[test]
    fn registration_with_mismatched_sender_fails() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let challenge = api::create_challenge(&env, canister_id)?;
        let result = api::register(
            &env,
            canister_id,
            principal_2(),
            &device_data_1(),
            ChallengeAttempt {
                chars: "a".to_string(),
                key: challenge.challenge_key,
            },
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated against").unwrap(),
        );
        Ok(())
    }

    /// Verifies that non-recovery devices cannot be registered as protected.
    #[test]
    fn should_not_register_non_recovery_device_as_protected() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let mut device1 = device_data_1();
        device1.protection = DeviceProtection::Protected;

        let challenge = api::create_challenge(&&env, canister_id)?;
        let result = api::register(
            &env,
            canister_id,
            principal_1(),
            &device1,
            ChallengeAttempt {
                chars: "a".to_string(),
                key: challenge.challenge_key,
            },
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Only recovery phrases can be protected but key type is Unknown").unwrap(),
        );
        Ok(())
    }

    /// Tests that the solution to the captcha needs to be correct.
    #[test]
    fn should_not_allow_wrong_captcha() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let challenge = api::create_challenge(&env, canister_id)?;
        let result = api::register(
            &env,
            canister_id,
            principal_1(),
            &device_data_1(),
            ChallengeAttempt {
                chars: "wrong solution".to_string(),
                key: challenge.challenge_key,
            },
        )?;

        assert!(matches!(result, RegisterResponse::BadChallenge));
        Ok(())
    }

    /// Tests that there is a time limit for captchas.
    /// Currently only checked by create_challenge, see L2-766.
    #[test]
    fn should_not_allow_expired_captcha() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let challenge = api::create_challenge(&env, canister_id)?;
        env.advance_time(Duration::from_secs(301)); // one second longer than captcha validity

        // required because register does not check captcha expiry
        api::create_challenge(&env, canister_id)?;
        let result = api::register(
            &env,
            canister_id,
            principal_1(),
            &device_data_1(),
            ChallengeAttempt {
                chars: "a".to_string(),
                key: challenge.challenge_key,
            },
        )?;

        assert!(matches!(result, RegisterResponse::BadChallenge));
        Ok(())
    }

    /// Tests that there is a maximum number of captchas that can be created in a given timeframe.
    #[test]
    fn should_limit_captcha_creation() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        for _ in 0..500 {
            api::create_challenge(&env, canister_id)?;
        }
        let result = api::create_challenge(&env, canister_id);

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("too many inflight captchas").unwrap(),
        );
        Ok(())
    }
}

/// Tests related to stable memory. In particular, the tests in this module make sure that II can be recovered from a stable memory backup.
mod stable_memory_tests {
    use crate::framework::{
        expect_user_error_with_message, principal_1, principal_recovery_1, principal_recovery_2,
        recovery_device_data_1, recovery_device_data_2, CallError,
    };
    use crate::{api, framework};
    use ic_error_types::ErrorCode::CanisterCalledTrap;
    use ic_state_machine_tests::{PrincipalId, StateMachine};
    use internet_identity_interface::DeviceData;
    use internet_identity_interface::DeviceProtection::Unprotected;
    use internet_identity_interface::KeyType::Unknown;
    use internet_identity_interface::Purpose::Authentication;
    use regex::Regex;
    use sdk_ic_types::Principal;
    use serde_bytes::ByteBuf;
    use std::path::PathBuf;

    /// Tests that some known anchors with their respective devices are available after stable memory restore.
    #[test]
    fn should_recover_anchors_and_devices_from_backup() -> Result<(), CallError> {
        const CREDENTIAL_ID_1: &str = "63b8afb386dd757dfa5ba9550bca66936717766f395bafad9052a384edc446b11228bcb9cb684980bb5a81270b31d4b9561787296d40204d31e96c1b386b4984";
        const PUB_KEY_1: &str = "305e300c060a2b0601040183b8430101034e00a5010203262001215820ee6f212d1b94fcc014f050b087f06ad34157ff53c19981e3976842b1644b0a1c2258200d6bc5ee077bd2300b3c86df87aa5fdf90d256d0131efbe44424330de8b00471";
        const CREDENTIAL_ID_2: &str = "01bf2325d975f7b24c2d4bb6fef94a2e6dbbb35f490689f460a36f0f96717ac5487ad63899efd59fd01ef38aab8a228badaa1b94cd819572695c446e2c379792af7f";
        const PUB_KEY_2: &str = "305e300c060a2b0601040183b8430101034e00a5010203262001215820724d6d2ae54244650134e475aaced8d82d45520ba672d9892c8de34d2a40e6f3225820e1f89fbff2e05a3ef1a35c1d9bb2de8b5e8856fd710b1a534a0841835cb793aa";
        const CREDENTIAL_ID_3: &str = "2ceb7800078c607e94f8e9432fb1cd9e033081a7";
        const PUB_KEY_3: &str = "305e300c060a2b0601040183b8430101034e00a50102032620012158208ca9cb400318172cb199b3df2f7c601f02fc72be73282ebc88ab0fb5562aae40225820f53cae416256e1ea0592f781f506c8cdd9fa67dbb329c5fca469ac6b5868b4cd";
        const CREDENTIAL_ID_4: &str = "0192eea062df84cde762eff346aaa3a7fb44f1aa19d888ae407295b77c4c754b755b2b7b90d9174c0cf41d3eb3928f1eb310e3b3a4bc00445179df0f84b7f8b1db";
        const PUB_KEY_4: &str = "305e300c060a2b0601040183b8430101034e00a5010203262001215820c8423e7f1df8dc91f599dd3683f37541514341643e916b0a77e935da1a7e5ff42258204f5d37a73d6e1b1ac6ebd0d7739ebf477a8f88ed6992cb36b6c481efee01b462";
        const PUB_KEY_5: &str =
        "302a300506032b6570032100f1ba3b80ce24f382fa32fd07233ceb8e305d57dafe6ad3d1c00e401315692631";
        const PUB_KEY_6: &str = "305e300c060a2b0601040183b8430101034e00a50102032620012158206c52bead5df52c208a9b1c7be0a60847573e5be4ac4fe08ea48036d0ba1d2acf225820b33daeb83bc9c77d8ad762fd68e3eab08684e463c49351b3ab2a14a400138387";

        let device1 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_1).unwrap()),
            alias: "Desktop".to_string(),
            purpose: Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_1).unwrap())),
            key_type: Unknown,
            protection: Unprotected,
        };
        let device2 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_2).unwrap()),
            alias: "andrew-mbp".to_string(),
            purpose: Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_2).unwrap())),
            key_type: Unknown,
            protection: Unprotected,
        };
        let device3 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_3).unwrap()),
            alias: "andrew phone chrome".to_string(),
            purpose: Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_3).unwrap())),
            key_type: Unknown,
            protection: Unprotected,
        };
        let device4 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_4).unwrap()),
            alias: "Pixel".to_string(),
            purpose: Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_4).unwrap())),
            key_type: Unknown,
            protection: Unprotected,
        };
        let device5 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_5).unwrap()),
            alias: "dfx".to_string(),
            purpose: Authentication,
            credential_id: None,
            key_type: Unknown,
            protection: Unprotected,
        };
        let device6 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_6).unwrap()),
            alias: "testkey".to_string(),
            purpose: Authentication,
            credential_id: None,
            key_type: Unknown,
            protection: Unprotected,
        };

        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/genesis-memory-layout.bin")).unwrap();
        env.set_stable_memory(canister_id, &stable_memory_backup);
        // upgrade again to reset cached header info in II storage module
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        // check known anchors in the backup
        let devices = api::lookup(&env, canister_id, 10_000)?;
        assert_eq!(devices, vec![device1]);

        let mut devices = api::lookup(&env, canister_id, 10_002)?;
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(devices, vec![device2, device3]);

        let devices = api::lookup(&env, canister_id, 10_029)?;
        assert_eq!(devices, vec![device4]);

        let devices = api::lookup(&env, canister_id, 10_030)?;
        assert_eq!(devices, vec![device5, device6]);

        Ok(())
    }

    /// Tests that II will issue the same principals after stable memory restore.
    #[test]
    fn should_issue_same_principal_after_restoring_backup() -> Result<(), CallError> {
        const PUBLIC_KEY: &str = "305e300c060a2b0601040183b8430101034e00a50102032620012158206c52bead5df52c208a9b1c7be0a60847573e5be4ac4fe08ea48036d0ba1d2acf225820b33daeb83bc9c77d8ad762fd68e3eab08684e463c49351b3ab2a14a400138387";
        const DELEGATION_PRINCIPAL: &str = "303c300c060a2b0601040183b8430102032c000a000000000000000001013a8926914dd1c836ec67ba66ac6425c21dffd3ca5c5968855f87780a1ec57985";
        let principal = PrincipalId(Principal::self_authenticating(
            hex::decode(PUBLIC_KEY).unwrap(),
        ));

        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/genesis-memory-layout.bin")).unwrap();
        env.set_stable_memory(canister_id, &stable_memory_backup);
        // upgrade again to reset cached header info in II storage module
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let (user_key, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal,
            10_030,
            "example.com".to_string(),
            ByteBuf::from("dummykey"),
            None,
        )?;

        // check that we get the same user key; this proves that the salt was recovered from the backup
        assert_eq!(
            user_key.clone().into_vec(),
            hex::decode(DELEGATION_PRINCIPAL).unwrap()
        );

        let principal = api::get_principal(
            &env,
            canister_id,
            principal,
            10_030,
            "example.com".to_string(),
        )?;
        assert_eq!(Principal::self_authenticating(user_key), principal);
        Ok(())
    }

    /// Tests that anchors can still be modified after stable memory restore.
    #[test]
    fn should_modify_devices_after_restoring_backup() -> Result<(), CallError> {
        let public_key = hex::decode("305e300c060a2b0601040183b8430101034e00a50102032620012158206c52bead5df52c208a9b1c7be0a60847573e5be4ac4fe08ea48036d0ba1d2acf225820b33daeb83bc9c77d8ad762fd68e3eab08684e463c49351b3ab2a14a400138387").unwrap();
        let principal = PrincipalId(Principal::self_authenticating(public_key.clone()));
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/genesis-memory-layout.bin")).unwrap();
        env.set_stable_memory(canister_id, &stable_memory_backup);
        // upgrade again to reset cached header info in II storage module
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let devices = api::lookup(&env, canister_id, 10_030)?;

        assert_eq!(devices.len(), 2);
        api::remove(
            &env,
            canister_id,
            principal,
            10_030,
            ByteBuf::from(public_key),
        )?;

        let devices = api::lookup(&env, canister_id, 10_030)?;
        assert_eq!(devices.len(), 1);
        Ok(())
    }

    /// Verifies that an anchor with two recovery phrases can still use both.
    /// This anchor is recovered from stable memory because the current version of II does not allow to create such anchors.
    #[test]
    fn should_not_break_on_multiple_legacy_recovery_phrases() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let frontend_hostname = "frontend_hostname".to_string();
        let session_key = ByteBuf::from("session_key");

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/multiple-recovery-phrases.bin")).unwrap();
        env.set_stable_memory(canister_id, &stable_memory_backup);
        // upgrade again to reset cached header info in II storage module
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        api::prepare_delegation(
            &env,
            canister_id,
            principal_recovery_1(),
            10_000,
            frontend_hostname.clone(),
            session_key.clone(),
            None,
        )?;
        api::prepare_delegation(
            &env,
            canister_id,
            principal_recovery_2(),
            10_000,
            frontend_hostname,
            session_key,
            None,
        )?;
        Ok(())
    }

    /// Verifies that an existing account with two recovery phrases can only make changes after deleting one.
    /// This anchor is recovered from stable memory because the current version of II does not allow to create such anchors.
    #[test]
    fn should_allow_modification_after_deleting_second_recovery_phrase() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/multiple-recovery-phrases.bin")).unwrap();
        env.set_stable_memory(canister_id, &stable_memory_backup);
        // upgrade again to reset cached header info in II storage module
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let mut recovery_1 = recovery_device_data_1();
        recovery_1.alias = "new alias".to_string();
        let result = api::update(
            &env,
            canister_id,
            principal_1(),
            10_000,
            recovery_1.pubkey.clone(),
            recovery_1.clone(),
        );
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("There is already a recovery phrase and only one is allowed\\.").unwrap(),
        );

        api::remove(
            &env,
            canister_id,
            principal_1(),
            10_000,
            recovery_device_data_2().pubkey,
        )?;

        // successful after removing the other one
        api::update(
            &env,
            canister_id,
            principal_1(),
            10_000,
            recovery_1.pubkey.clone(),
            recovery_1,
        )?;
        Ok(())
    }
}

/// Tests related to local device management (add, remove, lookup, get_anchor_info).
/// Tests for the 'add remote device flow' are in the module [remote_device_registration_tests].
#[cfg(test)]
mod device_management_tests {
    use crate::framework::{
        device_data_1, device_data_2, expect_user_error_with_message, principal_1, principal_2,
        recovery_device_data_1, recovery_device_data_2, CallError,
    };
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode::CanisterCalledTrap;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface as types;
    use regex::Regex;

    /// Verifies that a new device can be added.
    #[test]
    fn should_add_additional_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let mut devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices.len(), 2);
        assert!(devices.iter().any(|device| device == &device_data_1()));
        assert!(devices.iter().any(|device| device == &device_data_2()));

        let mut anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

        // sort devices to not fail on different orderings
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        anchor_info.devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(devices, anchor_info.devices);
        Ok(())
    }

    /// Verifies that the same device cannot be added twice.
    #[test]
    fn should_not_add_existing_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result = api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_1(), // this device was already added during registration
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Device already added\\.").unwrap(),
        );
        Ok(())
    }

    /// Verifies that a second recovery phrase cannot be added.
    #[test]
    fn should_not_add_second_recovery_phrase() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;
        let result = api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_2(),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("There is already a recovery phrase and only one is allowed\\.").unwrap(),
        );
        Ok(())
    }

    /// Verifies that the devices cannot be added for other users.
    #[test]
    fn should_not_add_device_for_different_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

        let result = api::add(
            &env,
            canister_id,
            principal_1(),
            user_number_2,
            device_data_1(),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Verifies that a new device can be added to anchors that were registered using the previous II release.
    #[test]
    fn should_add_additional_device_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices.len(), 2);
        assert!(devices.iter().any(|device| device == &device_data_2()));
        Ok(())
    }

    #[cfg(test)]
    mod update {

        use crate::framework::expect_user_error_with_message;
        use crate::framework::{
            device_data_1, device_data_2, principal_1, principal_2, CallError, PUBKEY_2,
        };
        use crate::{api, flows, framework};
        use ic_error_types::ErrorCode::CanisterCalledTrap;
        use ic_state_machine_tests::StateMachine;
        use internet_identity_interface as types;
        use regex::Regex;
        use serde_bytes::ByteBuf;

        /// Verifies that a device can be updated
        #[test]
        fn should_update_device() -> Result<(), CallError> {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            let principal = principal_1();
            let mut device = device_data_1();

            let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

            let devices = api::lookup(&env, canister_id, user_number)?;
            assert_eq!(devices, vec![device.clone()]);

            device.alias.push_str("some suffix");

            api::update(
                &env,
                canister_id,
                principal,
                user_number,
                device.clone().pubkey,
                device.clone(),
            )?;

            let devices = api::lookup(&env, canister_id, user_number)?;
            assert_eq!(devices, vec![device]);

            Ok(())
        }

        /// Verifies that a protected device can be updated
        #[test]
        fn should_update_protected_device() -> Result<(), CallError> {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            let principal = principal_1();
            let mut device = device_data_1();
            device.protection = types::DeviceProtection::Protected;
            device.key_type = types::KeyType::SeedPhrase;

            let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

            let devices = api::lookup(&env, canister_id, user_number)?;
            assert_eq!(devices, vec![device.clone()]);

            device.alias.push_str("some suffix");

            api::update(
                &env,
                canister_id,
                principal,
                user_number,
                device.clone().pubkey,
                device.clone(),
            )?;

            let devices = api::lookup(&env, canister_id, user_number)?;
            assert_eq!(devices, vec![device]);

            Ok(())
        }

        /// Verifies that the device key (i.e. the device ID) cannot be updated
        #[test]
        fn should_not_modify_pubkey() {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            let principal = principal_1();
            let mut device = device_data_1();

            let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

            let original_pubkey = device.pubkey;
            let pubkey_2 = ByteBuf::from(PUBKEY_2);
            assert_ne!(original_pubkey, pubkey_2);
            device.pubkey = pubkey_2;

            let result = api::update(
                &env,
                canister_id,
                principal,
                user_number,
                original_pubkey,
                device.clone(),
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("device key may not be updated").unwrap(),
            );
        }

        /// Verifies that users can only update their own devices.
        #[test]
        fn should_not_update_device_of_different_user() {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
            let user_number_2 =
                flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

            let result = api::update(
                &env,
                canister_id,
                principal_1(),
                user_number_2,
                device_data_2().pubkey,
                device_data_2(),
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
            );
        }

        /// Verifies that unprotected devices can only be updated from themselves
        #[test]
        fn should_not_update_protected_with_different_device() {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            let mut device1 = device_data_1();
            device1.protection = types::DeviceProtection::Protected;
            device1.key_type = types::KeyType::SeedPhrase;

            let user_number =
                flows::register_anchor_with(&env, canister_id, principal_1(), &device1);

            api::add(
                &env,
                canister_id,
                principal_1(),
                user_number,
                device_data_2(),
            )
            .unwrap();

            let result = api::update(
                &env,
                canister_id,
                principal_2(),
                user_number,
                device1.pubkey.clone(),
                device1.clone(), // data here doesnt' actually matter
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("Device is protected. Must be authenticated with this device to mutate")
                    .unwrap(),
            );
        }

        /// Verifies that non-recovery devices cannot be updated to be protected.
        #[test]
        fn should_not_update_non_recovery_device_to_be_protected() {
            let env = StateMachine::new();
            let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
            let user_number = flows::register_anchor(&env, canister_id);

            let mut device1 = device_data_1();
            device1.protection = types::DeviceProtection::Protected;
            let result = api::update(
                &env,
                canister_id,
                principal_1(),
                user_number,
                device1.pubkey.clone(),
                device1.clone(), // data here doesnt' actually matter
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("Only recovery phrases can be protected but key type is Unknown")
                    .unwrap(),
            );
        }
    }

    /// Verifies that a device can be removed.
    #[test]
    fn should_remove_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let devices = api::lookup(&env, canister_id, user_number)?;
        assert!(devices.iter().any(|device| device == &device_data_2()));

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices.len(), 1);
        assert!(!devices.iter().any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that a protected device can be removed.
    #[test]
    fn should_remove_protected_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let mut device2 = device_data_2();
        device2.protection = types::DeviceProtection::Protected;
        device2.key_type = types::KeyType::SeedPhrase;

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let devices = api::lookup(&env, canister_id, user_number)?;
        assert!(devices.iter().any(|device| device == &device_data_2()));

        api::remove(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2().pubkey,
        )?;

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices.len(), 1);
        assert!(!devices.iter().any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that the even last device can be removed.
    /// This behaviour should be changed because it makes anchors unusable, see L2-745.
    #[test]
    fn should_remove_last_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_1().pubkey,
        )?;

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert!(devices.is_empty());
        Ok(())
    }

    /// Verifies that users can only remove their own devices.
    #[test]
    fn should_not_remove_device_of_different_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

        let result = api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number_2,
            device_data_2().pubkey,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Verifies that unprotected devices can not be removed with another device
    #[test]
    fn should_not_remove_protected_with_different_device() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let mut device1 = device_data_1();
        device1.protection = types::DeviceProtection::Protected;
        device1.key_type = types::KeyType::SeedPhrase;

        let user_number = flows::register_anchor_with(&env, canister_id, principal_1(), &device1);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )
        .unwrap();

        let result = api::remove(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device1.pubkey.clone(),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Device is protected. Must be authenticated with this device to mutate")
                .unwrap(),
        );
    }

    /// Verifies that a device can be removed if it has been added using the previous II release.
    #[test]
    fn should_remove_device_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let devices = api::compat::lookup(&env, canister_id, user_number)?;
        assert!(devices.iter().any(|device| device == &device_data_2()));

        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;

        let devices = api::lookup(&env, canister_id, user_number)?;
        assert_eq!(devices.len(), 1);
        assert!(!devices.iter().any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that get_anchor_info requires authentication.
    #[test]
    fn should_not_allow_get_anchor_info_for_different_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result = api::get_anchor_info(&env, canister_id, principal_2(), user_number);

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }
}

/// Tests related to prepare_delegation, get_delegation and get_principal II canister calls.
#[cfg(test)]
mod delegation_tests {
    use crate::framework::{
        device_data_1, device_data_2, expect_user_error_with_message, principal_1, principal_2,
        CallError,
    };
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode::CanisterCalledTrap;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface::GetDelegationResponse;
    use regex::Regex;
    use sdk_ic_types::Principal;
    use serde_bytes::ByteBuf;
    use std::ops::Add;
    use std::time::{Duration, UNIX_EPOCH};

    /// Verifies that valid delegations are issued.
    #[test]
    fn should_get_valid_delegation() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that non-default expirations are respected.
    #[test]
    fn should_get_valid_delegation_with_custom_expiration() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            Some(3_600_000_000_000), // 1 hour
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(60 * 60)) // 1 hour
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that the delegations are valid at most for 30 days.
    #[test]
    fn should_shorten_expiration_greater_max_ttl() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            Some(Duration::from_secs(31 * 24 * 60 * 60).as_nanos() as u64), // 31 days
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 24 * 60 * 60)) // 30 days
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that delegations can be requested in parallel.
    #[test]
    fn should_get_multiple_valid_delegations() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname_1 = "https://dapp1.com";
        let frontend_hostname_2 = "https://dapp2.com";
        let pub_session_key_1 = ByteBuf::from("session public key 1");
        let pub_session_key_2 = ByteBuf::from("session public key 2");
        let delegation_params = vec![
            (
                &pub_session_key_1,
                frontend_hostname_1,
                Duration::from_secs(0),
            ),
            (
                &pub_session_key_1,
                frontend_hostname_2,
                Duration::from_secs(0),
            ),
            (
                &pub_session_key_2,
                frontend_hostname_1,
                Duration::from_secs(0),
            ),
            (
                &pub_session_key_1,
                frontend_hostname_1,
                Duration::from_secs(30),
            ),
        ];

        // prepare multiple delegations in parallel before calling get_delegation
        let prepare_delegation_results =
            delegation_params
                .into_iter()
                .map(|(session_key, frontend_hostname, time_shift)| {
                    env.advance_time(time_shift);
                    let (canister_sig_key, expiration) = api::prepare_delegation(
                        &env,
                        canister_id,
                        principal_1(),
                        user_number,
                        frontend_hostname.to_string(),
                        session_key.clone(),
                        None,
                    )
                    .expect("prepare_delegation failed");

                    assert_eq!(
                        expiration,
                        env.time()
                            .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_nanos() as u64
                    );
                    (session_key, frontend_hostname, canister_sig_key, expiration)
                });

        for (session_key, frontend_hostname, canister_sig_key, expiration) in
            prepare_delegation_results
        {
            let signed_delegation = match api::get_delegation(
                &env,
                canister_id,
                principal_1(),
                user_number,
                frontend_hostname.to_string(),
                session_key.clone(),
                expiration,
            )? {
                GetDelegationResponse::SignedDelegation(delegation) => delegation,
                GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
            };

            framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
            assert_eq!(signed_delegation.delegation.pubkey, session_key.clone());
            assert_eq!(signed_delegation.delegation.expiration, expiration);
        }
        Ok(())
    }

    /// Verifies that an anchor that was registered using II_WASM_PREVIOUS gets valid delegations after upgrading to the current version.
    #[test]
    fn should_get_valid_delegation_for_old_anchor_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that different front-ends yield different principals.
    #[test]
    fn should_issue_different_principals() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname_1 = "https://dapp1.com";
        let frontend_hostname_2 = "https://dapp2.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key_1, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_1.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        let (canister_sig_key_2, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_2.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        assert_ne!(canister_sig_key_1, canister_sig_key_2);
        Ok(())
    }

    /// Verifies that there is a graceful failure if II gets upgraded between prepare_delegation and get_delegation.
    #[test]
    fn should_not_get_prepared_delegation_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        // upgrade, even with the same WASM clears non-stable memory
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
            GetDelegationResponse::NoSuchDelegation => {}
        };
        Ok(())
    }

    /// Verifies that there is a graceful failure if get_delegation is called after the expiration of the delegation.
    #[test]
    fn should_not_get_delegation_after_expiration() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        env.advance_time(Duration::from_secs(30 * 60 + 1)); // one second more than delegation validity of 30 min

        // we have to call prepare again, because expired signatures can only be pruned in update calls
        api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
            GetDelegationResponse::NoSuchDelegation => {}
        };
        Ok(())
    }

    /// Verifies that delegations can only be prepared by the matching user.
    #[test]
    fn can_not_prepare_delegation_for_different_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result = api::prepare_delegation(
            &env,
            canister_id,
            principal_2(),
            user_number, // belongs to principal_1
            "https://some-dapp.com".to_string(),
            ByteBuf::from("session key"),
            None,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Verifies that get_delegation can only be called by the matching user.
    #[test]
    fn can_not_get_delegation_for_different_user() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        let result = api::get_delegation(
            &env,
            canister_id,
            principal_2(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
        Ok(())
    }

    /// Verifies that get_principal and prepare_delegation return the same principal.
    #[test]
    fn get_principal_should_match_prepare_delegation() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        let principal = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
        )?;
        assert_eq!(Principal::self_authenticating(canister_sig_key), principal);
        Ok(())
    }

    /// Verifies that get_principal returns different principals for different front end host names.
    #[test]
    fn should_return_different_principals_for_different_frontends() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname_1 = "https://dapp-1.com";
        let frontend_hostname_2 = "https://dapp-2.com";

        let dapp_principal_1 = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_1.to_string(),
        )?;

        let dapp_principal_2 = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_2.to_string(),
        )?;

        assert_ne!(dapp_principal_1, dapp_principal_2);
        Ok(())
    }

    /// Verifies that get_principal returns different principals for different anchors.
    #[test]
    fn should_return_different_principals_for_different_users() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number_1 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let frontend_hostname = "https://dapp-1.com";

        let dapp_principal_1 = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number_1,
            frontend_hostname.to_string(),
        )?;

        let dapp_principal_2 = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number_2,
            frontend_hostname.to_string(),
        )?;

        assert_ne!(dapp_principal_1, dapp_principal_2);
        Ok(())
    }

    /// Verifies that get_principal requires authentication.
    #[test]
    fn should_not_allow_get_principal_for_other_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());
        let frontend_hostname_1 = "https://dapp-1.com";

        let result = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number_2,
            frontend_hostname_1.to_string(),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated\\.").unwrap(),
        );
    }
}

/// Tests for the HTTP interactions according to the HTTP gateway spec: https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway
#[cfg(test)]
mod http_tests {
    use crate::certificate_validation::validate_certification;
    use crate::framework::{
        assert_metric, device_data_1, device_data_2, principal_1, principal_2, CallError,
    };
    use crate::{api, flows, framework};
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface::{ChallengeAttempt, HttpRequest, InternetIdentityInit};
    use serde_bytes::ByteBuf;
    use std::time::{Duration, SystemTime};

    /// Verifies that expected assets are delivered, certified and have security headers.
    #[test]
    fn ii_canister_serves_http_assets() -> Result<(), CallError> {
        let assets: Vec<(&str, Option<&str>)> = vec![
            ("/", None),
            ("/index.html", None),
            ("/index.js", Some("gzip")),
            ("/loader.webp", None),
            ("/favicon.ico", None),
            ("/ic-badge.svg", None),
        ];
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        // for each asset, fetch the asset, check the HTTP status code, headers and certificate.
        for (asset, encoding) in assets {
            let http_response = api::http_request(
                &env,
                canister_id,
                HttpRequest {
                    method: "GET".to_string(),
                    url: asset.to_string(),
                    headers: vec![],
                    body: ByteBuf::new(),
                },
            )?;

            assert_eq!(http_response.status_code, 200);

            // check the appropriate Content-Encoding header is set
            if let Some(enc) = encoding {
                let (_, content_encoding) = http_response
                    .headers
                    .iter()
                    .find(|(name, _)| name.to_lowercase() == "content-encoding")
                    .expect("Content-Encoding header not found");
                assert_eq!(
                    content_encoding, enc,
                    "unexpected Content-Encoding header value"
                );
            }

            // 1. It searches for a response header called Ic-Certificate (case-insensitive).
            let (_, ic_certificate) = http_response
                .headers
                .iter()
                .find(|(name, _)| name.to_lowercase() == "ic-certificate")
                .expect("IC-Certificate header not found");

            validate_certification(
                ic_certificate,
                canister_id,
                asset,
                &http_response.body,
                None, // should really be `encoding`, but cannot use it because II certifies encoded response bodies, see L2-722 for details
                env.root_key(),
                env.time(),
            )
            .expect(&format!("validation for asset \"{}\" failed", asset));
            framework::verify_security_headers(&http_response.headers);
        }
        Ok(())
    }

    /// Verifies that all expected metrics are available via the HTTP endpoint.
    #[test]
    fn ii_canister_serves_http_metrics() -> Result<(), CallError> {
        let metrics = vec![
            "internet_identity_user_count",
            "internet_identity_min_user_number",
            "internet_identity_max_user_number",
            "internet_identity_signature_count",
            "internet_identity_stable_memory_pages",
            "internet_identity_last_upgrade_timestamp",
            "internet_identity_inflight_challenges",
            "internet_identity_users_in_registration_mode",
        ];
        let env = StateMachine::new();
        env.advance_time(Duration::from_secs(300)); // advance time to see it reflected on the metrics endpoint
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let metrics_body = flows::get_metrics(&env, canister_id);
        for metric in metrics {
            let (_, metric_timestamp) = framework::parse_metric(&metrics_body, metric);
            assert_eq!(
                metric_timestamp,
                env.time(),
                "metric timestamp did not match state machine time"
            )
        }
        Ok(())
    }

    /// Verifies that the metrics list the expected user range.
    #[test]
    fn metrics_should_list_expected_user_range() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let metrics = flows::get_metrics(&env, canister_id);

        let (min_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_min_user_number");
        let (max_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_max_user_number");
        assert_eq!(min_user_number, 10_000);
        assert_eq!(max_user_number, 3_784_872);
        Ok(())
    }

    #[test]
    fn should_widen_user_range_on_upgrade() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister_with_arg(
            &env,
            framework::II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: (127, 129),
            }),
        );

        let metrics = flows::get_metrics(&env, canister_id);
        let (min_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_min_user_number");
        let (max_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_max_user_number");
        assert_eq!(min_user_number, 127);
        assert_eq!(max_user_number, 128);

        // The storage updates the upper bound on upgrade if it doesn't use the
        // full capacity. This is a hack that has to go away when we start using
        // multiple backend canisters.
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let metrics = flows::get_metrics(&env, canister_id);
        let (min_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_min_user_number");
        let (max_user_number, _) =
            framework::parse_metric(&metrics, "internet_identity_max_user_number");
        assert_eq!(min_user_number, 127);
        assert_eq!(max_user_number, 3_774_999);
    }

    /// Verifies that the user count metric is updated correctly.
    #[test]
    fn metrics_user_count_should_increase_after_register() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        assert_metric(&env, canister_id, "internet_identity_user_count", 0);
        for count in 0..2 {
            flows::register_anchor(&env, canister_id);
            assert_metric(&env, canister_id, "internet_identity_user_count", count + 1);
        }
        Ok(())
    }

    /// Verifies that the signature count metric is updated correctly.
    #[test]
    fn metrics_signature_count() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let frontend_hostname = "https://some-dapp.com";
        let user_number = flows::register_anchor(&env, canister_id);

        assert_metric(&env, canister_id, "internet_identity_signature_count", 0);
        for count in 0..3 {
            api::prepare_delegation(
                &env,
                canister_id,
                principal_1(),
                user_number,
                frontend_hostname.to_string(),
                ByteBuf::from(format!("session key {}", count)),
                None,
            )?;

            assert_metric(
                &env,
                canister_id,
                "internet_identity_signature_count",
                count + 1,
            );
        }

        // long after expiry (we don't want this test to break, if we change the default delegation expiration)
        env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
        // we need to make an update call to prune expired delegations
        api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            ByteBuf::from("last session key"),
            None,
        )?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (signature_count, _) =
            framework::parse_metric(&metrics, "internet_identity_signature_count");
        assert_eq!(signature_count, 1); // old ones pruned and a new one created
        Ok(())
    }

    /// Verifies that the stable memory pages count metric is updated correctly.
    #[test]
    fn metrics_stable_memory_pages_should_increase_with_more_users() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let metrics = flows::get_metrics(&env, canister_id);
        let (signature_count, _) =
            framework::parse_metric(&metrics, "internet_identity_stable_memory_pages");
        // empty II has some metadata in stable memory which requires at least one page
        assert_eq!(signature_count, 1);

        // a wasm page is 64kb and a single user takes up 2kb -> 32 users require a complete wasm page
        for _ in 0..32 {
            flows::register_anchor(&env, canister_id);
        }

        let metrics = flows::get_metrics(&env, canister_id);
        let (signature_count, _) =
            framework::parse_metric(&metrics, "internet_identity_stable_memory_pages");
        assert_eq!(signature_count, 2);
        Ok(())
    }

    /// Verifies that the last II wasm upgrade timestamp is updated correctly.
    #[test]
    fn metrics_last_upgrade_timestamp_should_update_after_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        // immediately upgrade because installing the canister does not set the metric
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        assert_metric(
            &env,
            canister_id,
            "internet_identity_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
        );

        env.advance_time(Duration::from_secs(300)); // the state machine does not advance time on its own
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        assert_metric(
            &env,
            canister_id,
            "internet_identity_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
        );
        Ok(())
    }

    /// Verifies that the inflight challenges metric is updated correctly.
    #[test]
    fn metrics_inflight_challenges() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 0);

        let challenge_1 = api::create_challenge(&env, canister_id)?;
        api::create_challenge(&env, canister_id)?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 2);

        // solving a challenge removes it from the inflight pool
        api::register(
            &env,
            canister_id,
            principal_1(),
            &device_data_1(),
            ChallengeAttempt {
                chars: "a".to_string(),
                key: challenge_1.challenge_key,
            },
        )?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 1);

        // long after expiry (we don't want this test to break, if we change the captcha expiration)
        env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
        // the only call that prunes expired captchas
        api::create_challenge(&env, canister_id)?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 1); // 1 pruned due to expiry, but also one created

        Ok(())
    }

    /// Verifies that the users in registration mode metric is updated correctly.
    #[test]
    fn metrics_device_registration_mode() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number_1 = flows::register_anchor(&env, canister_id);
        let user_number_2 = flows::register_anchor(&env, canister_id);

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 0);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;
        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_2)?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 2);

        api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 1);

        // long after expiry (we don't want this test to break, if we change the registration mode expiration)
        env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
        // make an update call related to tentative devices so that registration mode expiry gets checked
        api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number_2,
            device_data_2(),
        )?;

        let metrics = flows::get_metrics(&env, canister_id);
        let (challenge_count, _) =
            framework::parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 0);

        Ok(())
    }
}

/// Tests concerning the device registration flow for remote devices (i.e. authenticators on another computer).
/// The flow has the following steps:
/// 1. on device 1: enter registration mode
/// 2. on device 2: register the new device tentatively -> this returns a verification code
/// 3. on device 1: enter the verification code
///
/// Additionally, there are the following bounds on the registration flow:
/// 1. registration mode expires after 15 minutes
/// 2. there is a limit of 3 attempts for step 3 in the above process
#[cfg(test)]
mod remote_device_registration_tests {
    use crate::framework::{
        device_data_2, expect_user_error_with_message, principal_1, principal_2, CallError,
    };
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface as types;
    use regex::Regex;
    use std::ops::Add;
    use std::time::{Duration, UNIX_EPOCH};

    /// Test entering registration mode including returned expiration time.
    #[test]
    fn can_enter_device_registration_mode() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result =
            api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

        assert_eq!(
            result,
            env.time()
                .add(Duration::from_secs(900)) // 900 seconds -> 15 min
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );
        Ok(())
    }

    /// Tests that only an authenticated user can enter device registration mode for themselves.
    #[test]
    fn can_not_enter_device_registration_mode_for_other_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result =
            api::enter_device_registration_mode(&env, canister_id, principal_2(), user_number);

        expect_user_error_with_message(
            result,
            ErrorCode::CanisterCalledTrap,
            Regex::new("[a-z0-9-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Tests that the device registration flow can be completed successfully.
    #[test]
    fn can_register_remote_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let add_response = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        let verification_code = match add_response {
            types::AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {:?}", err),
        };
        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            verification_code,
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the device registration flow can be completed successfully after submitting an invalid code.
    #[test]
    fn can_verify_remote_device_after_failed_attempt() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let add_response = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        let verification_code = match add_response {
            types::AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {:?}", err),
        };

        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code".to_string()
            )?,
            types::VerifyTentativeDeviceResponse::WrongCode { retries_left: 2 }
        ));

        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            verification_code,
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the anchor info call returns information about tentative devices.
    /// This enables the front-end to continue an in progress flow (e.g. after a refresh of the page).
    #[test]
    fn anchor_info_should_return_tentative_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let new_device = device_data_2();
        api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            new_device.clone(),
        )?;
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

        assert!(matches!(
            anchor_info.device_registration,
            Some(types::DeviceRegistrationInfo {
                tentative_device: Some(tenative_device),
                ..
            }) if tenative_device == new_device
        ));
        Ok(())
    }

    /// Tests that devices cannot be registered tentatively if the registration mode is not enabled.
    #[test]
    fn reject_tentative_device_if_not_in_registration_mode() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let result = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;

        assert!(matches!(
            result,
            types::AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests device registration mode expiration.
    #[test]
    fn reject_tentative_device_if_registration_mode_is_expired() -> Result<(), CallError> {
        const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        env.advance_time(REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1));
        let result = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;

        assert!(matches!(
            result,
            types::AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests that an appropriate result is returned when a verification code is submitted without a
    /// corresponding tentative device.
    #[test]
    fn reject_verification_without_tentative_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            "some code".to_string(),
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::NoDeviceToVerify
        ));
        Ok(())
    }

    /// Tests that the flow is aborted after the expected number of failed verification attempts.
    #[test]
    fn reject_verification_with_wrong_code() -> Result<(), CallError> {
        const MAX_RETRIES: u8 = 3;
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        for expected_retries in (0..MAX_RETRIES).rev() {
            assert!(matches!(
                api::verify_tentative_device(
                    &env,
                    canister_id,
                    principal_1(),
                    user_number,
                    "invalid code".to_string()
                )?,
                types::VerifyTentativeDeviceResponse::WrongCode {
                    retries_left
                } if retries_left == expected_retries
            ));
        }

        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code".to_string()
            )?,
            types::VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }
}
