use canister_tests::api::archive as archive_api;
use canister_tests::api::internet_identity as ii_api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::archive::types::*;
use internet_identity_interface::internet_identity::types::*;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;
use std::time::SystemTime;

/// Tests related to archive deployment (using II).
#[cfg(test)]
mod deployment_tests {
    use super::*;

    /// Test to verify that II can spawn an archive canister.
    #[test]
    fn should_deploy_archive() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));
        Ok(())
    }

    /// Test to verify that II can spawn an archive canister when cycles are required.
    #[test]
    fn should_deploy_archive_with_cycles() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: None,
                archive_config: Some(ArchiveConfig {
                    module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                    entries_buffer_limit: 0,
                    polling_interval_ns: 0,
                    entries_fetch_limit: 0,
                }),
                canister_creation_cycles_cost: Some(100_000_000_000), // current cost in application subnets
                register_rate_limit: None,
                max_num_latest_delegation_origins: None,
            }),
        );
        env.add_cycles(ii_canister, 150_000_000_000);

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));
        assert_eq!(env.cycle_balance(ii_canister), 50_000_000_000);
        Ok(())
    }

    /// Test to verify that II will not deploy wasm modules that have the wrong hash.
    #[test]
    fn should_not_deploy_wrong_wasm() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, &EMPTY_WASM)?;
        match result {
            DeployArchiveResult::Failed(msg) => {
                assert_eq!(msg, "invalid wasm module".to_string())
            }
            unexpected => panic!("unexpected result: {unexpected:?}"),
        }

        let stats = ii_api::stats(&env, ii_canister)?;
        assert!(stats.archive_info.archive_canister.is_none());
        Ok(())
    }

    /// Test to verify that II will not spawn the archive canister if no hash is configured.
    #[test]
    fn should_not_deploy_archive_when_disabled() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister(&env, II_WASM.clone());

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        match result {
            DeployArchiveResult::Failed(msg) => {
                assert_eq!(msg, "archive deployment disabled".to_string())
            }
            unexpected => panic!("unexpected result: {unexpected:?}"),
        }

        let stats = ii_api::stats(&env, ii_canister)?;
        assert!(stats.archive_info.archive_canister.is_none());
        Ok(())
    }

    /// Test to verify that II will not lose information about the archive wasm hash on upgrade.
    #[test]
    fn should_keep_archive_module_hash_across_upgrades() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );
        upgrade_ii_canister(&env, ii_canister, II_WASM.clone());

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));
        Ok(())
    }

    /// Test to verify that the archive WASM hash can be changed on II upgrade and a the archive can be upgraded.
    #[test]
    fn should_upgrade_the_archive() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(EMPTY_WASM.clone()),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, &EMPTY_WASM)?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));

        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        )
        .unwrap();

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        let DeployArchiveResult::Success(archive_canister) = result else {
            panic!("Unexpected result")
        };

        // interact with the archive to make sure it is no longer the empty wasm
        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 0);
        Ok(())
    }

    /// Test to verify that II can spawn an archive canister when cycles are required.
    #[test]
    fn should_upgrade_archive_with_only_config_changed() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        let DeployArchiveResult::Success(archive_canister) = result else {
            panic!("Unexpected result")
        };

        let status = archive_api::status(&env, archive_canister)?;
        assert_eq!(status.init.polling_interval_ns, 1_000_000_000);

        // change archive config
        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                assigned_user_number_range: None,
                archive_config: Some(ArchiveConfig {
                    module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                    entries_buffer_limit: 10,
                    polling_interval_ns: 5_000,
                    entries_fetch_limit: 10,
                }),
                canister_creation_cycles_cost: None, // current cost in application subnets
                register_rate_limit: None,
                max_num_latest_delegation_origins: None,
            }),
        )
        .unwrap();

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));

        let status = archive_api::status(&env, archive_canister)?;
        assert_eq!(status.init.polling_interval_ns, 5_000);
        Ok(())
    }
}

/// Test the functionality of pulling entries from II.
#[cfg(test)]
mod pull_entries_tests {
    use super::*;

    /// Test to verify that the archive pulls the anchor operations from II.
    #[test]
    fn should_record_anchor_operations() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let anchor = flows::register_anchor(&env, ii_canister);

        let mut device = device_data_2();
        ii_api::add(&env, ii_canister, principal_1(), anchor, &device)?;

        device.purpose = Purpose::Recovery;
        let pubkey = device.pubkey.clone();
        ii_api::update(&env, ii_canister, principal_1(), anchor, &pubkey, &device)?;

        ii_api::replace(
            &env,
            ii_canister,
            principal_1(),
            anchor,
            &pubkey,
            &device_data_2(),
        )?;

        ii_api::remove(&env, ii_canister, principal_1(), anchor, &pubkey)?;
        let timestamp = env
            .time()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 5);

        let register_entry = Entry {
            anchor,
            operation: Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias {
                    pubkey: device_data_1().pubkey,
                    credential_id: device_data_1().credential_id,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                    origin: device_data_1().origin,
                    metadata_keys: None,
                },
            },
            timestamp,
            caller: principal_1(),
            sequence_number: 0,
        };
        assert_eq!(
            entries.entries.get(0).unwrap().as_ref().unwrap(),
            &register_entry
        );

        let add_entry = Entry {
            anchor,
            operation: Operation::AddDevice {
                device: DeviceDataWithoutAlias::from(device_data_2()),
            },
            timestamp,
            caller: principal_1(),
            sequence_number: 1,
        };
        assert_eq!(
            entries.entries.get(1).unwrap().as_ref().unwrap(),
            &add_entry
        );

        let update_entry = Entry {
            anchor,
            operation: Operation::UpdateDevice {
                device: device_data_2().pubkey,
                new_values: DeviceDataUpdate {
                    alias: None,
                    credential_id: None,
                    purpose: Some(Purpose::Recovery),
                    key_type: None,
                    protection: None,
                    origin: None,
                    metadata_keys: None,
                },
            },
            timestamp,
            caller: principal_1(),
            sequence_number: 2,
        };
        assert_eq!(
            entries.entries.get(2).unwrap().as_ref().unwrap(),
            &update_entry
        );

        let replace_entry = Entry {
            anchor,
            operation: Operation::ReplaceDevice {
                old_device: device_data_2().pubkey,
                new_device: DeviceDataWithoutAlias::from(device_data_2()),
            },
            timestamp,
            caller: principal_1(),
            sequence_number: 3,
        };
        assert_eq!(
            entries.entries.get(3).unwrap().as_ref().unwrap(),
            &replace_entry
        );

        let delete_entry = Entry {
            anchor,
            operation: Operation::RemoveDevice { device: pubkey },
            timestamp,
            caller: principal_1(),
            sequence_number: 4,
        };
        assert_eq!(
            entries.entries.get(4).unwrap().as_ref().unwrap(),
            &delete_entry
        );
        Ok(())
    }

    /// Test to verify that the archive pulls device metadata keys for new devices from II.
    #[test]
    fn should_record_metadata_for_new_device() -> Result<(), CallError> {
        const METADATA_KEY: &str = "key";
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );
        let timestamp = env
            .time()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let mut device = DeviceData::auth_test_device();
        device.metadata = Some(HashMap::from_iter(vec![(
            METADATA_KEY.to_string(),
            MetadataEntry::String("some value".to_string()),
        )]));
        let anchor = flows::register_anchor_with_device(&env, ii_canister, &device);

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        let expected_register_entry = Entry {
            anchor,
            operation: Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias::from(device.clone()),
            },
            timestamp,
            caller: device.principal(),
            sequence_number: 0,
        };
        assert_eq!(
            entries.entries.get(0).unwrap().as_ref().unwrap(),
            &expected_register_entry
        );

        Ok(())
    }

    /// Test to verify that the archive pulls device metadata changes from II.
    #[test]
    fn should_record_metadata_change() -> Result<(), CallError> {
        const METADATA_KEY: &str = "key";
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );
        let timestamp = env
            .time()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let mut device = DeviceData::auth_test_device();
        let anchor = flows::register_anchor_with_device(&env, ii_canister, &device);

        device.metadata = Some(HashMap::from_iter(vec![(
            METADATA_KEY.to_string(),
            MetadataEntry::String("some value".to_string()),
        )]));

        ii_api::update(
            &env,
            ii_canister,
            device.principal(),
            anchor,
            &device.pubkey,
            &device,
        )?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 2);

        let expected_update_entry = Entry {
            anchor,
            operation: Operation::UpdateDevice {
                device: device.pubkey.clone(),
                new_values: DeviceDataUpdate {
                    alias: None,
                    credential_id: None,
                    purpose: None,
                    key_type: None,
                    protection: None,
                    origin: None,
                    metadata_keys: Some(vec![METADATA_KEY.to_string()]),
                },
            },
            timestamp,
            caller: device.principal(),
            sequence_number: 1,
        };
        assert_eq!(
            entries.entries.get(1).unwrap().as_ref().unwrap(),
            &expected_update_entry
        );

        Ok(())
    }

    /// Test to verify that the archive pulls identity metadata changes from II.
    #[test]
    fn should_record_identity_metadata_change() -> Result<(), CallError> {
        const METADATA_KEY: &str = "some-metadata-key";
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );
        let timestamp = env
            .time()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        let device = DeviceData::auth_test_device();
        let anchor = flows::register_anchor_with_device(&env, ii_canister, &device);

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let metadata = HashMap::from_iter(vec![(
            METADATA_KEY.to_string(),
            MetadataEntry::String("some value".to_string()),
        )]);

        ii_api::api_v2::identity_metadata_write(
            &env,
            ii_canister,
            device.principal(),
            anchor,
            &metadata,
        )?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        let expected_metadata_entry = Entry {
            anchor,
            operation: Operation::IdentityMetadataWrite {
                metadata_keys: metadata.keys().cloned().collect(),
            },
            timestamp,
            caller: device.principal(),
            sequence_number: 0,
        };
        assert_eq!(
            entries.entries.get(0).unwrap().as_ref().unwrap(),
            &expected_metadata_entry
        );

        Ok(())
    }

    /// Test to verify that the archive pulls the anchor operations from II periodically.
    #[test]
    fn should_fetch_multiple_times() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));
        flows::register_anchor(&env, ii_canister);

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 4);
        Ok(())
    }

    /// Tests integration if II has no new messages to archive.
    #[test]
    fn should_succeed_on_empty_fetch_result() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert!(status.call_info.call_errors.is_empty());
        assert_eq!(
            status.call_info.last_successful_fetch,
            Some(FetchInfo {
                timestamp: env
                    .time()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos() as u64,
                number_of_entries: 0,
            })
        );
        Ok(())
    }

    /// Tests that the archive fetch info entries count is accurate.
    #[test]
    fn should_report_correct_number_of_fetched_entries() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert!(status.call_info.call_errors.is_empty());
        assert_eq!(
            status.call_info.last_successful_fetch,
            Some(FetchInfo {
                timestamp: env
                    .time()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos() as u64,
                number_of_entries: 3,
            })
        );

        assert_metric(
            &get_metrics(&env, archive_canister),
            "ii_archive_last_successful_fetch_timestamp_seconds",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        );
        assert_metric(
            &get_metrics(&env, archive_canister),
            "ii_archive_last_successful_fetch_entries_count",
            3f64,
        );
        assert_metric(
            &get_metrics(&env, archive_canister),
            "ii_archive_highest_sequence_number",
            2f64,
        );
        Ok(())
    }

    /// Tests that II exposes metrics regarding the archive config.
    #[test]
    fn should_report_archive_config_metrics() -> Result<(), CallError> {
        let env = env();

        let init_arg = InternetIdentityInit {
            assigned_user_number_range: None,
            archive_config: Some(ArchiveConfig {
                module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                entries_buffer_limit: 20_000,
                polling_interval_ns: Duration::from_secs(3).as_nanos() as u64,
                entries_fetch_limit: 10,
            }),
            canister_creation_cycles_cost: Some(0),
            register_rate_limit: None,
            max_num_latest_delegation_origins: None,
        };

        let ii_canister = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(init_arg));
        deploy_archive_via_ii(&env, ii_canister);

        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_config_entries_buffer_limit",
            20_000f64,
        );
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_config_fetch_limit",
            10f64,
        );
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_config_polling_interval_seconds",
            3f64,
        );
        Ok(())
    }

    /// Tests that the archive reports on fetch errors.
    #[test]
    fn should_report_call_errors() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // stop the II canister to provoke failures
        env.stop_canister(ii_canister, None)?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert_eq!(status.call_info.call_errors.len(), 1);
        assert_eq!(status.call_info.last_successful_fetch, None);

        let expected_error = CallErrorInfo {
            time: env
                .time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
            canister: ii_canister,
            method: "fetch_entries".to_string(),
            argument: ByteBuf::from(candid::encode_one(()).unwrap()),
            rejection_code: 4,
            message: format!("Canister {} is stopped", ii_canister.to_text()),
        };
        assert_eq!(
            status.call_info.call_errors.get(0).unwrap(),
            &expected_error
        );
        Ok(())
    }

    /// Tests that the archive recovers after fetch errors.
    #[test]
    fn should_recover_after_error() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // stop the II canister to provoke failures
        env.stop_canister(ii_canister, None)?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert_eq!(status.call_info.call_errors.len(), 1);
        assert_eq!(status.call_info.last_successful_fetch, None);

        // start II again to resolve the issue
        env.start_canister(ii_canister, None)?;
        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert!(matches!(status.call_info.last_successful_fetch, Some(_)));
        Ok(())
    }

    /// Tests that II provides the entries ordered by sequence number.
    #[test]
    fn should_return_entries_ordered() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        let entries = ii_api::fetch_entries(&env, ii_canister, archive_canister)?;
        let mut entries_sorted = entries.clone();
        entries_sorted.sort_by(|a, b| a.sequence_number.cmp(&b.sequence_number));

        assert_eq!(entries, entries_sorted);
        Ok(())
    }

    /// Tests that only the archive canister can fetch entries.
    #[test]
    fn should_not_allow_wrong_caller_to_fetch_entries() {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let result = ii_api::fetch_entries(&env, ii_canister, principal_1());
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new(
                "only the archive canister [a-z0-9-]+ is allowed to fetch and acknowledge entries",
            )
            .unwrap(),
        );
    }

    /// Tests that only the archive canister can acknowledge entries.
    #[test]
    fn should_not_allow_wrong_caller_to_acknowledge_entries() {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let result = ii_api::acknowledge_entries(&env, ii_canister, principal_1(), 37);
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new(
                "only the archive canister [a-z0-9-]+ is allowed to fetch and acknowledge entries",
            )
            .unwrap(),
        );
    }
}
