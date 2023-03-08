use candid::Principal;
use canister_tests::api::http_request;
use canister_tests::api::internet_identity as api;
use canister_tests::certificate_validation::validate_certification;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::http_gateway::HttpRequest;
use internet_identity_interface::*;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::ops::Add;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

#[test]
fn ii_canister_can_be_installed() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::health_check(&env, canister_id);
}

/// Tests for making sure that the current version can be upgrade to from the last release. This tests stable memory compatibility and pre / post install hooks.
#[cfg(test)]
mod upgrade_tests {
    use super::*;

    /// Basic upgrade test.
    #[test]
    fn ii_upgrade_works() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        env.upgrade_canister(
            canister_id,
            II_WASM.clone(),
            candid::encode_one(None::<InternetIdentityInit>).unwrap(),
        )?;
        api::health_check(&env, canister_id);
        Ok(())
    }

    /// Test to verify that anchors are kept across upgrades.
    #[test]
    fn ii_upgrade_retains_anchors() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
            .expect("get_anchor_info failed");
        assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
    }

    /// Test to verify that anchor numbers are unchanged by changing the user range.
    #[test]
    fn should_retain_anchor_on_user_range_change() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((10_000, 11_000)),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        )?;

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

        assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
        Ok(())
    }

    /// Test to verify that anchors number range can be changed on upgrade.
    #[test]
    fn should_allow_change_of_user_range_on_upgrade() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((2000, 4000)),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        )?;

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(stats.assigned_user_number_range, (2000, 4000));
        Ok(())
    }

    /// Test to verify that the user range cannot be changed to modify anchor numbers of existing anchors.
    #[test]
    fn should_not_affect_existing_anchors_on_user_range_change() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        flows::register_anchor(&env, canister_id);

        let result = upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((2000, 4000)),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("specified range \\[\\d+, \\d+\\) does not start from the same number \\(\\d+\\) as the existing range thus would make existing anchors invalid")
                .unwrap(),
        );
    }

    /// Test to verify that the user range cannot be changed to allow fewer anchors than are already registered.
    #[test]
    fn should_not_allow_fewer_users_than_existing_on_user_range_change() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        flows::register_anchor(&env, canister_id);

        let result = upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((10_000, 10_000)),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("specified range \\[\\d+, \\d+\\) does not accommodate all \\d+ anchors thus would make existing anchors invalid")
                .unwrap(),
        );
    }

    /// Test to verify that the user range cannot be changed to exceed the max capacity of the II canister.
    #[test]
    fn should_not_allow_user_range_exceeding_capacity() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        flows::register_anchor(&env, canister_id);

        let result = upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((10_000, 10_000_000_000_000)),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("specified range \\[\\d+, \\d+\\) is too large for this canister \\(max \\d+ entries\\)")
                .unwrap(),
        );
    }

    /// Test to verify that the same anchor range is allowed on upgrade.
    #[test]
    fn ii_upgrade_should_allow_same_user_range() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

        let stats = api::stats(&env, canister_id)?;

        let result = upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some(stats.assigned_user_number_range),
                archive_config: None,
                canister_creation_cycles_cost: None,
            }),
        );

        assert!(result.is_ok());
        Ok(())
    }
}

/// Tests for making sure that any release can be rolled back. This tests stable memory compatibility and pre / post install hooks.
#[cfg(test)]
mod rollback_tests {
    use super::*;

    /// Tests simple upgrade and downgrade.
    #[test]
    fn ii_canister_can_be_upgraded_and_rolled_back() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());
        api::health_check(&env, canister_id);
        upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
        api::health_check(&env, canister_id);
    }

    /// Tests that the devices can still be read after upgrade and rollback.
    #[test]
    fn upgrade_and_rollback_keeps_anchor_intact() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let mut devices_before =
            api::get_anchor_info(&env, canister_id, principal_1(), user_number)
                .unwrap()
                .devices;
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());
        api::health_check(&env, canister_id);
        upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
        api::health_check(&env, canister_id);
        let mut devices_after = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
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
        let env = env();

        // start with the previous release to initialize v1 layout
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        api::init_salt(&env, canister_id)?;

        // use the new version to register an anchor
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let principal = api::get_principal(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
        )?;

        // roll back
        upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());

        // use anchor
        let devices =
            api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
        assert_eq!(devices, vec![device_data_1()]);

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
    use super::*;

    /// Tests user registration with cross checks for get_anchor_credentials, get_anchor_info and get_principal.
    #[test]
    fn should_register_new_anchor() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        api::init_salt(&env, canister_id)?;
        let user_number = flows::register_anchor(&env, canister_id);

        let anchor_credentials = api::get_anchor_credentials(&env, canister_id, user_number)?;
        assert_eq!(
            anchor_credentials.credentials,
            vec![WebAuthnCredential::try_from(device_data_1()).unwrap()]
        );
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number_1 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
        let user_number_2 =
            flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());

        assert_ne!(user_number_1, user_number_2);
    }

    /// Tests that the user numbers start at the beginning of the init range and are capped at the end (exclusive).
    #[test]
    fn should_assign_correct_user_numbers() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: Some((127, 129)),
                archive_config: None,
                canister_creation_cycles_cost: None,
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let mut device1 = device_data_1();
        device1.protection = DeviceProtection::Protected;

        let challenge = api::create_challenge(&env, canister_id)?;
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
            Regex::new("Only recovery phrases can be locked but key type is Unknown").unwrap(),
        );
        Ok(())
    }

    /// Tests that the solution to the captcha needs to be correct.
    #[test]
    fn should_not_allow_wrong_captcha() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

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

/// Tests related to the last_usage_timestamp.
mod last_usage_timestamp_tests {
    use super::*;

    /// Tests that get_anchor_info updates the last usage on the added device.
    #[test]
    fn should_set_last_usage_on_get_anchor_info() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        env.advance_time(Duration::from_secs(1));

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, time(&env));
        Ok(())
    }

    /// Tests that add only updates last usage on the device used to authenticate the call (not the newly added one).
    #[test]
    fn should_set_last_usage_on_add() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));

        // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info =
            api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

        assert!(anchor_info
            .devices
            .contains(&DeviceWithUsage::from(device_data_2()))); // without last usage timestamp

        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }

    /// Tests that remove updates last usage on the device used to authenticate the call.
    #[test]
    fn should_set_last_usage_on_remove() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;

        env.advance_time(Duration::from_secs(1));

        // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info =
            api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }

    /// Tests that update only updates last usage on the device used to authenticate the call (not the updated one).
    #[test]
    fn should_set_last_usage_on_update() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;
        let mut device_to_be_updated = device_data_2();
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_to_be_updated.clone(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp_1 = time(&env);

        // use the device_to_be_updated to create a last usage timestamp
        api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp_2 = time(&env);

        device_to_be_updated.alias = "changed value".to_string();

        // this update is should keep the last usage timestamp of device_to_be_updated and update
        // the last usage of device_1
        api::update(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_to_be_updated.pubkey.clone(),
            device_to_be_updated.clone(),
        )?;

        env.advance_time(Duration::from_secs(1));

        // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info =
            api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

        assert!(anchor_info
            .devices
            .clone()
            .into_iter()
            .map(DeviceData::from)
            .any(|d| d == device_to_be_updated));

        assert_device_last_used(&anchor_info, &device_data_2().pubkey, expected_timestamp_1);
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp_2);

        Ok(())
    }

    /// Tests that replace only updates last usage on the device used to authenticate the call (not the replacement device).
    #[test]
    fn should_set_last_usage_on_replace() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::replace(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
            max_size_device(),
        )?;

        env.advance_time(Duration::from_secs(1));

        // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info =
            api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

        assert!(anchor_info
            .devices
            .contains(&DeviceWithUsage::from(max_size_device()))); // without last usage timestamp

        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }

    /// Tests that prepare_delegation updates the last usage timestamp.
    #[test]
    fn should_set_last_usage_on_prepare_delegation() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com".to_string();
        let pub_session_key = ByteBuf::from("session public key");

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname,
            pub_session_key,
            None,
        )?;

        env.advance_time(Duration::from_secs(1));

        // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }

    /// Tests that calls related to tentative device registration update the last usage timestamp.
    #[test]
    fn should_update_last_usage_on_tentative_device_registration() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

        env.advance_time(Duration::from_secs(1));

        // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        let verification_code = match api::add_tentative_device(
            &env,
            canister_id,
            principal_recovery_1(),
            user_number,
            recovery_device_data_1(),
        )? {
            AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            _ => panic!("unexpected response"),
        };

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            verification_code,
        )?;

        // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }

    /// Tests that exit_device_registration_mode updates the last usage timestamp.
    #[test]
    fn should_update_last_usage_on_exit_device_registration_mode() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        env.advance_time(Duration::from_secs(1));
        let expected_timestamp = time(&env);

        api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

        // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
        assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

        Ok(())
    }
}

/// Tests related to stable memory. In particular, the tests in this module make sure that II can be recovered from a stable memory backup.
mod stable_memory_tests {
    use super::*;

    /// Known devices that exist in the genesis memory backups.
    fn known_devices() -> [DeviceData; 6] {
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
            purpose: Purpose::Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_1).unwrap())),
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        let device2 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_2).unwrap()),
            alias: "andrew-mbp".to_string(),
            purpose: Purpose::Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_2).unwrap())),
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        let device3 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_3).unwrap()),
            alias: "andrew phone chrome".to_string(),
            purpose: Purpose::Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_3).unwrap())),
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        let device4 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_4).unwrap()),
            alias: "Pixel".to_string(),
            purpose: Purpose::Authentication,
            credential_id: Some(ByteBuf::from(hex::decode(CREDENTIAL_ID_4).unwrap())),
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        let device5 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_5).unwrap()),
            alias: "dfx".to_string(),
            purpose: Purpose::Authentication,
            credential_id: None,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        let device6 = DeviceData {
            pubkey: ByteBuf::from(hex::decode(PUB_KEY_6).unwrap()),
            alias: "testkey".to_string(),
            purpose: Purpose::Authentication,
            credential_id: None,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
        };
        [device1, device2, device3, device4, device5, device6]
    }

    /// Tests that some known anchors with their respective devices are available after stable memory restore.
    /// Uses the same data initially created using the genesis layout and then migrated until v6.    
    #[test]
    fn should_load_genesis_migrated_to_v6_backup() -> Result<(), CallError> {
        let [device1, device2, device3, device4, device5, device6]: [DeviceData; 6] =
            known_devices();

        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/genesis-layout-migrated-to-v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        // check known anchors in the backup
        let devices = api::get_anchor_info(
            &env,
            canister_id,
            Principal::self_authenticating(&device1.pubkey),
            10_000,
        )?
        .into_device_data();
        assert_eq!(devices, vec![device1]);

        let mut devices = api::get_anchor_info(
            &env,
            canister_id,
            Principal::self_authenticating(&device2.pubkey),
            10_002,
        )?
        .into_device_data();
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(devices, vec![device2, device3]);

        let devices = api::get_anchor_info(
            &env,
            canister_id,
            Principal::self_authenticating(&device4.pubkey),
            10_029,
        )?
        .into_device_data();
        assert_eq!(devices, vec![device4]);

        let mut devices = api::get_anchor_info(
            &env,
            canister_id,
            Principal::self_authenticating(&device5.pubkey),
            10_030,
        )?
        .into_device_data();
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(devices, vec![device5, device6]);
        Ok(())
    }

    /// Tests that II will issue the same principals after stable memory restore.
    #[test]
    fn should_issue_same_principal_after_restoring_backup() -> Result<(), CallError> {
        const PUBLIC_KEY: &str = "305e300c060a2b0601040183b8430101034e00a50102032620012158206c52bead5df52c208a9b1c7be0a60847573e5be4ac4fe08ea48036d0ba1d2acf225820b33daeb83bc9c77d8ad762fd68e3eab08684e463c49351b3ab2a14a400138387";
        const DELEGATION_PRINCIPAL: &str = "303c300c060a2b0601040183b8430102032c000a000000000000000001013a8926914dd1c836ec67ba66ac6425c21dffd3ca5c5968855f87780a1ec57985";
        let principal = Principal::self_authenticating(hex::decode(PUBLIC_KEY).unwrap());

        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/genesis-layout-migrated-to-v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

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
        let [_, _, _, _, device5, device6] = known_devices();
        let principal = Principal::self_authenticating(device6.pubkey);
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/genesis-layout-migrated-to-v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let devices =
            api::get_anchor_info(&env, canister_id, principal, 10_030)?.into_device_data();

        assert_eq!(devices.len(), 2);
        api::remove(&env, canister_id, principal, 10_030, device5.pubkey)?;

        let devices =
            api::get_anchor_info(&env, canister_id, principal, 10_030)?.into_device_data();
        assert_eq!(devices.len(), 1);
        Ok(())
    }

    /// Verifies that an anchor with two recovery phrases can still use both.
    /// This anchor is recovered from stable memory because the current version of II does not allow to create such anchors.
    #[test]
    fn should_not_break_on_multiple_legacy_recovery_phrases() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());
        let frontend_hostname = "frontend_hostname".to_string();
        let session_key = ByteBuf::from("session_key");

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/multiple-recovery-phrases-v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

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
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/multiple-recovery-phrases-v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

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

    /// Verifies that a stable memory backup with persistent state v1 can be used for an upgrade.
    #[test]
    fn should_read_persistent_state() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/persistent_state_no_archive_v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let devices =
            api::get_anchor_info(&env, canister_id, principal_1(), 10_005)?.into_device_data();
        assert_eq!(devices.len(), 4);

        let stats = api::stats(&env, canister_id)?;
        assert!(stats.archive_info.archive_canister.is_none());
        assert!(stats.archive_info.archive_config.is_none());
        Ok(())
    }

    /// Verifies that a stable memory backup with persistent state containing archive information is restored correctly.
    #[test]
    fn should_read_persistent_state_with_archive() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/persistent_state_archive_v6.bin.gz",
        );
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let devices =
            api::get_anchor_info(&env, canister_id, principal_1(), 10_000)?.into_device_data();
        assert_eq!(devices.len(), 1);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats.archive_info.archive_canister.unwrap(),
            Principal::from_text("rrkah-fqaaa-aaaaa-aaaaq-cai").unwrap()
        );

        assert_eq!(
            stats
                .archive_info
                .archive_config
                .unwrap()
                .module_hash
                .to_vec(),
            hex::decode("12e2c2bd05dfcd86e3004ecd5f00533e6120e7bcf82bac0753af0a7fe14bfea1")
                .unwrap()
        );
        assert_eq!(stats.storage_layout_version, 6);
        Ok(())
    }

    /// Tests that II will refuse to install on a stable memory layout that is no longer supported.  
    #[test]
    fn should_trap_on_old_stable_memory() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        let stable_memory_backup =
            std::fs::read(PathBuf::from("stable_memory/genesis-memory-layout.bin")).unwrap();
        env.set_stable_memory(canister_id, ByteBuf::from(stable_memory_backup));
        let result = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None);
        assert!(result.is_err());
        let err = result.err().unwrap();
        match err {
            CallError::Reject(err) => panic!("unexpected error {err}"),
            CallError::UserError(err) => {
                assert_eq!(err.code, CanisterCalledTrap);
                assert!(err.description.contains("stable memory layout version 1 is no longer supported:\nEither reinstall (wiping stable memory) or migrate using a previous II version"));
            }
        }
        Ok(())
    }

    /// Tests that II will refuse to upgrade on stable memory without persistent state.
    #[test]
    fn should_trap_on_missing_persistent_state() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());
        restore_compressed_stable_memory(
            &env,
            canister_id,
            "stable_memory/no-persistent-state-v6.bin.gz",
        );

        let result = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None);

        assert!(result.is_err());
        let err = result.err().unwrap();
        match err {
            CallError::Reject(err) => panic!("unexpected error {err}"),
            CallError::UserError(err) => {
                assert_eq!(err.code, CanisterCalledTrap);
                assert!(err
                    .description
                    .contains("failed to recover persistent state! Err: NotFound"));
            }
        }
        Ok(())
    }
}

/// Tests related to local device management (add, remove, update, replace, lookup, get_anchor_info and get_anchor_credentials).
/// Tests for the 'add remote device flow' are in the module [remote_device_registration_tests].
#[cfg(test)]
mod device_management_tests {
    use super::*;

    /// Tests that lookup is consistent with get anchor info, but without alias.
    #[test]
    fn should_lookup() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        api::init_salt(&env, canister_id)?;
        let user_number = flows::register_anchor(&env, canister_id);
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            recovery_device_data_1(),
        )?;

        let mut devices = api::lookup(&env, canister_id, user_number)?;
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

        let mut anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        // clear alias to make it consistent with lookup
        anchor_info
            .devices
            .iter_mut()
            .for_each(|device| device.alias = "".to_string());
        anchor_info.devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

        assert_eq!(devices, anchor_info.into_device_data());
        Ok(())
    }

    /// Verifies that a new device can be added.
    #[test]
    fn should_add_additional_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let mut devices =
            api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
        devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(devices, vec![device_data_2(), device_data_1()]);

        let mut credentials =
            api::get_anchor_credentials(&env, canister_id, user_number)?.credentials;
        // sort devices to not fail on different orderings
        credentials.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
        assert_eq!(
            credentials,
            vec![
                WebAuthnCredential::try_from(device_data_2()).unwrap(),
                WebAuthnCredential::try_from(device_data_1()).unwrap()
            ]
        );
        Ok(())
    }

    /// Verifies that the same device cannot be added twice.
    #[test]
    fn should_not_add_existing_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            Regex::new("Device with key \\w+ already exists on this anchor\\.").unwrap(),
        );
        Ok(())
    }

    /// Verifies that a second recovery phrase cannot be added.
    #[test]
    fn should_not_add_second_recovery_phrase() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());
        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.devices.len(), 2);
        assert!(anchor_info
            .into_device_data()
            .iter()
            .any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that the total size of all devices stays under the variable length fields limit.
    #[test]
    fn should_respect_total_size_limit() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        for i in 0..3u8 {
            let mut device = max_size_device();
            device.pubkey = ByteBuf::from([i; 300]);
            api::add(&env, canister_id, principal_1(), user_number, device)?;
        }

        let result = api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            max_size_device(),
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Cumulative size of variable sized fields exceeds limit: length \\d+, limit \\d+\\. Either use shorter aliases or remove an existing device\\.").unwrap(),
        );
        Ok(())
    }

    #[cfg(test)]
    mod update {
        use super::*;

        /// Verifies that a device can be updated
        #[test]
        fn should_update_device() -> Result<(), CallError> {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let principal = principal_1();
            let mut device = device_data_1();

            let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

            let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
            assert_eq!(anchor_info.into_device_data(), vec![device.clone()]);

            device.alias.push_str("some suffix");

            api::update(
                &env,
                canister_id,
                principal,
                user_number,
                device.clone().pubkey,
                device.clone(),
            )?;

            let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
            assert_eq!(anchor_info.into_device_data(), vec![device]);

            Ok(())
        }

        /// Verifies that a protected device can be updated
        #[test]
        fn should_update_protected_device() -> Result<(), CallError> {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let principal = principal_1();
            let mut device = device_data_1();
            device.protection = DeviceProtection::Protected;
            device.key_type = KeyType::SeedPhrase;

            let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

            let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
            assert_eq!(anchor_info.into_device_data(), vec![device.clone()]);

            device.alias.push_str("some suffix");

            api::update(
                &env,
                canister_id,
                principal,
                user_number,
                device.clone().pubkey,
                device.clone(),
            )?;

            let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
            assert_eq!(anchor_info.into_device_data(), vec![device.clone()]);

            Ok(())
        }

        /// Verifies that the device key (i.e. the device ID) cannot be updated
        #[test]
        fn should_not_modify_pubkey() {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
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
                device,
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("Device key cannot be updated\\.").unwrap(),
            );
        }

        /// Verifies that users can only update their own devices.
        #[test]
        fn should_not_update_device_of_different_user() {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let mut device1 = device_data_1();
            device1.protection = DeviceProtection::Protected;
            device1.key_type = KeyType::SeedPhrase;

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
                device1, // data here doesnt' actually matter
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("Device is locked. Must be authenticated with this device to mutate")
                    .unwrap(),
            );
        }

        /// Verifies that non-recovery devices cannot be updated to be protected.
        #[test]
        fn should_not_update_non_recovery_device_to_be_protected() {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let user_number = flows::register_anchor(&env, canister_id);

            let mut device1 = device_data_1();
            device1.protection = DeviceProtection::Protected;
            let result = api::update(
                &env,
                canister_id,
                principal_1(),
                user_number,
                device1.pubkey.clone(),
                device1, // data here doesnt' actually matter
            );

            expect_user_error_with_message(
                result,
                CanisterCalledTrap,
                Regex::new("Only recovery phrases can be locked but key type is Unknown").unwrap(),
            );
        }
    }

    #[cfg(test)]
    mod get_anchor_credentials {
        use super::*;

        /// Verifies that get_anchor_credentials returns the expected credentials.
        #[test]
        fn should_get_credentials() -> Result<(), CallError> {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let user_number = flows::register_anchor(&env, canister_id);

            let recovery_webauthn_device = DeviceData {
                pubkey: ByteBuf::from("recovery device"),
                alias: "Recovery Device".to_string(),
                credential_id: Some(ByteBuf::from("recovery credential id")),
                purpose: Purpose::Recovery,
                key_type: KeyType::CrossPlatform,
                protection: DeviceProtection::Unprotected,
                origin: None,
            };

            api::add(
                &env,
                canister_id,
                principal_1(),
                user_number,
                device_data_2(),
            )?;
            api::add(
                &env,
                canister_id,
                principal_1(),
                user_number,
                recovery_device_data_1(),
            )?;
            api::add(
                &env,
                canister_id,
                principal_1(),
                user_number,
                recovery_webauthn_device.clone(),
            )?;

            let response = api::get_anchor_credentials(&env, canister_id, user_number)?;

            assert_eq!(response.credentials.len(), 2);
            assert!(response.credentials.contains(&WebAuthnCredential {
                pubkey: device_data_1().pubkey,
                credential_id: device_data_1().credential_id.unwrap()
            }));
            assert!(response.credentials.contains(&WebAuthnCredential {
                pubkey: device_data_2().pubkey,
                credential_id: device_data_2().credential_id.unwrap()
            }));

            assert_eq!(
                response.recovery_credentials,
                vec![WebAuthnCredential {
                    pubkey: recovery_webauthn_device.pubkey.clone(),
                    credential_id: recovery_webauthn_device.credential_id.unwrap()
                }]
            );

            assert_eq!(
                response.recovery_phrases,
                vec![recovery_device_data_1().pubkey]
            );

            Ok(())
        }

        /// Verifies that get_anchor_credentials returns the expected credentials (i.e. no recovery credentials if there are none).
        #[test]
        fn should_not_get_recovery_credentials_if_there_are_none() -> Result<(), CallError> {
            let env = env();
            let canister_id = install_ii_canister(&env, II_WASM.clone());
            let user_number = flows::register_anchor(&env, canister_id);

            let response = api::get_anchor_credentials(&env, canister_id, user_number)?;

            assert_eq!(
                response.credentials,
                vec![WebAuthnCredential {
                    pubkey: device_data_1().pubkey,
                    credential_id: device_data_1().credential_id.unwrap()
                }]
            );
            assert_eq!(response.recovery_credentials, vec![]);
            assert_eq!(response.recovery_phrases, Vec::<PublicKey>::new());

            Ok(())
        }
    }

    /// Verifies that a device can be removed.
    #[test]
    fn should_remove_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.devices.len(), 2);
        assert!(anchor_info
            .into_device_data()
            .iter()
            .any(|device| device == &device_data_2()));

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.devices.len(), 1);
        assert!(!anchor_info
            .into_device_data()
            .iter()
            .any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that a protected device can be removed.
    #[test]
    fn should_remove_protected_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let mut device2 = device_data_2();
        device2.protection = DeviceProtection::Protected;
        device2.key_type = KeyType::SeedPhrase;

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert!(anchor_info
            .into_device_data()
            .iter()
            .any(|device| device == &device_data_2()));

        api::remove(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2().pubkey,
        )?;

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.devices.len(), 1);
        assert!(!anchor_info
            .into_device_data()
            .iter()
            .any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that the even last device can be removed.
    /// This behaviour should be changed because it makes anchors unusable, see L2-745.
    #[test]
    fn should_remove_last_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_1().pubkey,
        )?;

        let anchor_credentials = api::get_anchor_credentials(&env, canister_id, user_number)?;
        assert!(anchor_credentials.credentials.is_empty());
        Ok(())
    }

    /// Verifies that users can only remove their own devices.
    #[test]
    fn should_not_remove_device_of_different_user() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let mut device1 = device_data_1();
        device1.protection = DeviceProtection::Protected;
        device1.key_type = KeyType::SeedPhrase;

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
            device1.pubkey,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Device is locked. Must be authenticated with this device to mutate")
                .unwrap(),
        );
    }

    /// Verifies that a device can be removed if it has been added using the previous II release.
    #[test]
    fn should_remove_device_after_ii_upgrade() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        let devices =
            api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
        assert!(devices.iter().any(|device| device == &device_data_2()));

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;

        let devices =
            api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
        assert_eq!(devices.len(), 1);
        assert!(!devices.iter().any(|device| device == &device_data_2()));
        Ok(())
    }

    /// Verifies that get_anchor_info requires authentication.
    #[test]
    fn should_not_allow_get_anchor_info_for_different_user() {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result = api::get_anchor_info(&env, canister_id, principal_2(), user_number);

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Verifies that a device can be replaced with another device.
    #[test]
    fn should_replace_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
        assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);

        api::replace(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_1().pubkey,
            device_data_2(),
        )?;

        let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
        assert_eq!(anchor_info.into_device_data(), vec![device_data_2()]);
        Ok(())
    }
}

/// Tests related to prepare_delegation, get_delegation and get_principal II canister calls.
#[cfg(test)]
mod delegation_tests {
    use super::*;

    /// Verifies that valid delegations are issued.
    #[test]
    fn should_get_valid_delegation() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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

        verify_delegation(canister_sig_key, &signed_delegation, &env.root_key());
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that non-default expirations are respected.
    #[test]
    fn should_get_valid_delegation_with_custom_expiration() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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

        verify_delegation(canister_sig_key, &signed_delegation, &env.root_key());
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that the delegations are valid at most for 30 days.
    #[test]
    fn should_shorten_expiration_greater_max_ttl() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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

        verify_delegation(canister_sig_key, &signed_delegation, &env.root_key());
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that delegations can be requested in parallel.
    #[test]
    fn should_get_multiple_valid_delegations() -> Result<(), CallError> {
        let env = env();
        let root_key = env.root_key();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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

            verify_delegation(canister_sig_key, &signed_delegation, &root_key);
            assert_eq!(signed_delegation.delegation.pubkey, session_key.clone());
            assert_eq!(signed_delegation.delegation.expiration, expiration);
        }
        Ok(())
    }

    /// Verifies that an anchor that was registered using II_WASM_PREVIOUS gets valid delegations after upgrading to the current version.
    #[test]
    fn should_get_valid_delegation_for_old_anchor_after_ii_upgrade() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

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

        verify_delegation(canister_sig_key, &signed_delegation, &env.root_key());
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that different front-ends yield different principals.
    #[test]
    fn should_issue_different_principals() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            pub_session_key,
            None,
        )?;

        assert_ne!(canister_sig_key_1, canister_sig_key_2);
        Ok(())
    }

    /// Verifies that there is a graceful failure if II gets upgraded between prepare_delegation and get_delegation.
    #[test]
    fn should_not_get_prepared_delegation_after_ii_upgrade() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key,
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            pub_session_key,
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            pub_session_key,
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key,
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        api::init_salt(&env, canister_id)?;
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        api::init_salt(&env, canister_id)?;
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
    use super::*;

    /// Verifies that expected assets are delivered, certified and have security headers.
    #[test]
    fn ii_canister_serves_http_assets() -> Result<(), CallError> {
        let assets: Vec<(&str, Option<&str>)> = vec![
            ("/", None),
            ("/index.html", None),
            ("/index.js", Some("gzip")),
            ("/loader.webp", None),
            ("/favicon.ico", None),
        ];
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        // for each asset, fetch the asset, check the HTTP status code, headers and certificate.
        for (asset, encoding) in assets {
            let http_response = http_request(
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
                &env.root_key(),
                env.time(),
            )
            .unwrap_or_else(|_| panic!("validation for asset \"{asset}\" failed"));
            verify_security_headers(&http_response.headers);
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
            "internet_identity_buffered_archive_entries",
        ];
        let env = env();
        env.advance_time(Duration::from_secs(300)); // advance time to see it reflected on the metrics endpoint

        // spawn an archive so that we also get the archive related metrics
        let canister_id = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );
        deploy_archive_via_ii(&env, canister_id);

        let metrics_body = get_metrics(&env, canister_id);
        for metric in metrics {
            let (_, metric_timestamp) = parse_metric(&metrics_body, metric);
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let metrics = get_metrics(&env, canister_id);

        let (min_user_number, _) = parse_metric(&metrics, "internet_identity_min_user_number");
        let (max_user_number, _) = parse_metric(&metrics, "internet_identity_max_user_number");
        assert_eq!(min_user_number, 10_000f64);
        assert_eq!(max_user_number, 8_188_859f64);
        Ok(())
    }

    /// Verifies that the user count metric is updated correctly.
    #[test]
    fn metrics_user_count_should_increase_after_register() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_user_count",
            0f64,
        );
        for count in 0..2 {
            flows::register_anchor(&env, canister_id);
            assert_metric(
                &get_metrics(&env, canister_id),
                "internet_identity_user_count",
                (count + 1) as f64,
            );
        }
        Ok(())
    }

    /// Verifies that the signature count metric is updated correctly.
    #[test]
    fn metrics_signature_and_delegation_count() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let frontend_hostname = "https://some-dapp.com";
        let user_number = flows::register_anchor(&env, canister_id);

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_signature_count",
            0f64,
        );
        for count in 0..3 {
            api::prepare_delegation(
                &env,
                canister_id,
                principal_1(),
                user_number,
                frontend_hostname.to_string(),
                ByteBuf::from(format!("session key {count}")),
                None,
            )?;

            assert_metric(
                &get_metrics(&env, canister_id),
                "internet_identity_signature_count",
                (count + 1) as f64,
            );
            assert_metric(
                &get_metrics(&env, canister_id),
                "internet_identity_delegation_counter",
                (count + 1) as f64,
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

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_signature_count",
            1f64, // old ones pruned and a new one created
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_delegation_counter",
            4f64, // delegation counter is not affected by pruning
        );
        Ok(())
    }

    /// Verifies that the stable memory pages count metric is updated correctly.
    #[test]
    fn metrics_stable_memory_pages_should_increase_with_more_users() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let metrics = get_metrics(&env, canister_id);
        let (stable_memory_pages, _) =
            parse_metric(&metrics, "internet_identity_stable_memory_pages");
        // empty II has some metadata in stable memory which requires at least one page
        assert_eq!(stable_memory_pages, 1f64);

        // the anchor offset is 2 pages -> adding a single anchor increases stable memory usage to
        // 3 pages
        flows::register_anchor(&env, canister_id);

        let metrics = get_metrics(&env, canister_id);
        let (stable_memory_pages, _) =
            parse_metric(&metrics, "internet_identity_stable_memory_pages");
        assert_eq!(stable_memory_pages, 3f64);
        Ok(())
    }

    /// Verifies that the last II wasm upgrade timestamp is updated correctly.
    #[test]
    fn metrics_last_upgrade_timestamp_should_update_after_upgrade() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        // immediately upgrade because installing the canister does not set the metric
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as f64,
        );

        env.advance_time(Duration::from_secs(300)); // the state machine does not advance time on its own
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as f64,
        );
        Ok(())
    }

    /// Verifies that the inflight challenges metric is updated correctly.
    #[test]
    fn metrics_inflight_challenges() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 0f64);

        let challenge_1 = api::create_challenge(&env, canister_id)?;
        api::create_challenge(&env, canister_id)?;

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 2f64);

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

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 1f64);

        // long after expiry (we don't want this test to break, if we change the captcha expiration)
        env.advance_time(Duration::from_secs(365 * 24 * 60 * 60));
        // the only call that prunes expired captchas
        api::create_challenge(&env, canister_id)?;

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) = parse_metric(&metrics, "internet_identity_inflight_challenges");
        assert_eq!(challenge_count, 1f64); // 1 pruned due to expiry, but also one created

        Ok(())
    }

    /// Verifies that the users in registration mode metric is updated correctly.
    #[test]
    fn metrics_device_registration_mode() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number_1 = flows::register_anchor(&env, canister_id);
        let user_number_2 = flows::register_anchor(&env, canister_id);

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) =
            parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 0f64);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;
        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number_2)?;

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) =
            parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 2f64);

        api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number_1)?;

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) =
            parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 1f64);

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

        let metrics = get_metrics(&env, canister_id);
        let (challenge_count, _) =
            parse_metric(&metrics, "internet_identity_users_in_registration_mode");
        assert_eq!(challenge_count, 0f64);

        Ok(())
    }

    /// Verifies that the anchor operation count metric is updated correctly.
    #[test]
    fn metrics_anchor_operations() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_anchor_operations_counter",
            0f64,
        );

        let user_number = flows::register_anchor(&env, canister_id);
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_anchor_operations_counter",
            1f64,
        );

        api::add(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2(),
        )?;
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_anchor_operations_counter",
            2f64,
        );

        let mut device = device_data_2();
        device.alias = "new alias".to_string();
        api::update(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device.pubkey.clone(),
            device,
        )?;
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_anchor_operations_counter",
            3f64,
        );

        api::remove(
            &env,
            canister_id,
            principal_1(),
            user_number,
            device_data_2().pubkey,
        )?;
        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_anchor_operations_counter",
            4f64,
        );

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
    use super::*;

    /// Test entering registration mode including returned expiration time.
    #[test]
    fn can_enter_device_registration_mode() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result =
            api::enter_device_registration_mode(&env, canister_id, principal_2(), user_number);

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z0-9-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Tests that the device registration flow can be completed successfully.
    #[test]
    fn can_register_remote_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {err:?}"),
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
            VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the device registration flow can be completed successfully after submitting an invalid code.
    #[test]
    fn can_verify_remote_device_after_failed_attempt() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {err:?}"),
        };

        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code".to_string()
            )?,
            VerifyTentativeDeviceResponse::WrongCode { retries_left: 2 }
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
            VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the anchor info call returns information about tentative devices.
    /// This enables the front-end to continue an in progress flow (e.g. after a refresh of the page).
    #[test]
    fn anchor_info_should_return_tentative_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            Some(DeviceRegistrationInfo {
                tentative_device: Some(tenative_device),
                ..
            }) if tenative_device == new_device
        ));
        Ok(())
    }

    /// Tests that devices cannot be registered tentatively if the registration mode is not enabled.
    #[test]
    fn reject_tentative_device_if_not_in_registration_mode() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests device registration mode expiration.
    #[test]
    fn reject_tentative_device_if_registration_mode_is_expired() -> Result<(), CallError> {
        const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests that an appropriate result is returned when a verification code is submitted without a
    /// corresponding tentative device.
    #[test]
    fn reject_verification_without_tentative_device() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
            VerifyTentativeDeviceResponse::NoDeviceToVerify
        ));
        Ok(())
    }

    /// Tests that the flow is aborted after the expected number of failed verification attempts.
    #[test]
    fn reject_verification_with_wrong_code() -> Result<(), CallError> {
        const MAX_RETRIES: u8 = 3;
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
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
                VerifyTentativeDeviceResponse::WrongCode {
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
            VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }
}

/// Tests related to the daily and monthly active users statistics.
mod active_anchors_tests {
    use super::*;

    const DAY_SECONDS: u64 = 24 * 60 * 60;
    const MONTH_SECONDS: u64 = 30 * DAY_SECONDS;

    /// Tests that daily active anchors are counted correctly.
    #[test]
    fn should_report_daily_active_anchors() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let anchor_number = flows::register_anchor(&env, canister_id);
        flows::register_anchor(&env, canister_id);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .daily_active_anchors
                .counter,
            2
        );

        // repeated activity within the 24h collection period should not increase the counter
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .daily_active_anchors
                .counter,
            2
        );

        env.advance_time(Duration::from_secs(DAY_SECONDS));

        // some activity is required to update the stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_daily_active_anchors",
            2f64,
        );

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .daily_active_anchors
                .counter,
            1
        );
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .completed
                .daily_active_anchors
                .unwrap()
                .counter,
            2
        );
        Ok(())
    }

    /// Tests that monthly active anchors are counted correctly.
    #[test]
    fn should_report_monthly_active_anchors() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let anchor_number = flows::register_anchor(&env, canister_id);
        flows::register_anchor(&env, canister_id);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            2
        );

        // repeated activity within the 30-day collection period should not increase the counter
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            2
        );

        env.advance_time(Duration::from_secs(MONTH_SECONDS));

        // some activity is required to update the stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_monthly_active_anchors",
            2f64,
        );

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            1
        );
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .completed
                .monthly_active_anchors
                .unwrap()
                .counter,
            2
        );
        Ok(())
    }

    /// Tests that monthly active anchors are updated every 24h.
    #[test]
    fn should_update_monthly_active_anchors_every_day() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        let anchor_number = flows::register_anchor(&env, canister_id);
        flows::register_anchor(&env, canister_id);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            2
        );

        // advance by 24h to record in the shifted 30-day period
        env.advance_time(Duration::from_secs(DAY_SECONDS));

        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            2
        );
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .get(1)
                .unwrap()
                .counter,
            1
        );

        // advance time to complete the first 30-day collection period
        env.advance_time(Duration::from_secs(29 * DAY_SECONDS));

        // some activity is required to update the stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_monthly_active_anchors",
            2f64,
        );

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .first()
                .unwrap()
                .counter,
            1
        );
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .completed
                .monthly_active_anchors
                .unwrap()
                .counter,
            2
        );

        // advance by 24h to complete the next 30-day period
        env.advance_time(Duration::from_secs(DAY_SECONDS));

        // some activity is required to update the stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "internet_identity_monthly_active_anchors",
            1f64,
        );

        Ok(())
    }

    /// Tests that active anchor stats are kept across upgrades.
    #[test]
    fn should_keep_stats_across_upgrades() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        flows::register_anchor(&env, canister_id);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .daily_active_anchors
                .counter,
            1
        );

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .daily_active_anchors
                .counter,
            1
        );

        Ok(())
    }

    /// Tests that the ongoing monthly collection periods are rolled over correctly.
    #[test]
    fn should_have_30_parallel_monthly_collection_windows() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());
        let anchor_number = flows::register_anchor(&env, canister_id);

        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .len(),
            1
        );

        for _ in 0..30 {
            // advance time to trigger the next ongoing 30-day collection period
            env.advance_time(Duration::from_secs(DAY_SECONDS));
            // some activity is required to update the stats
            api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;
        }
        let stats = api::stats(&env, canister_id)?;
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .len(),
            30
        );

        // after 30 days the first monthly collection period should be completed
        assert_eq!(
            stats
                .active_anchor_stats
                .as_ref()
                .unwrap()
                .completed
                .monthly_active_anchors
                .as_ref()
                .unwrap()
                .counter,
            1
        );

        Ok(())
    }

    /// Tests that the stats are updated correctly even with long periods of no activity at all.
    #[test]
    fn should_have_correct_stats_after_long_inactivity() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM.clone());

        // some activity
        let anchor_number = flows::register_anchor(&env, canister_id);

        let t0 = env.time();

        // more than 24h without activity
        env.advance_time(Duration::from_secs(2 * DAY_SECONDS + 1));

        // some activity to update stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        let stats = api::stats(&env, canister_id)?;
        let daily_stats = stats
            .active_anchor_stats
            .unwrap()
            .completed
            .daily_active_anchors
            .unwrap();
        assert_eq!(daily_stats.counter, 0);
        assert_eq!(
            daily_stats.start_timestamp,
            t0.duration_since(UNIX_EPOCH)
                .unwrap()
                .add(Duration::from_secs(DAY_SECONDS))
                .as_nanos() as u64
        );

        // more than 30 days without activity
        env.advance_time(Duration::from_secs(31 * DAY_SECONDS));

        // some activity to update stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

        let stats = api::stats(&env, canister_id)?;
        let monthly_stats = stats
            .active_anchor_stats
            .as_ref()
            .unwrap()
            .completed
            .monthly_active_anchors
            .as_ref()
            .unwrap();
        assert_eq!(monthly_stats.counter, 0);
        assert_eq!(
            monthly_stats.start_timestamp,
            t0.duration_since(UNIX_EPOCH)
                .unwrap()
                .add(Duration::from_secs(3 * DAY_SECONDS))
                .as_nanos() as u64
        );

        // there is one ongoing collection period setup
        assert_eq!(
            stats
                .active_anchor_stats
                .unwrap()
                .ongoing
                .monthly_active_anchors
                .len(),
            1
        );

        Ok(())
    }
}
