use canister_tests::api::archive as archive_api;
use canister_tests::api::internet_identity as ii_api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::archive::*;
use internet_identity_interface::*;
use serde_bytes::ByteBuf;
use state_machine_client::CallError;
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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let result =
            ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
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
                    archive_integration: Some(ArchiveIntegration::Pull),
                }),
                canister_creation_cycles_cost: Some(100_000_000_000), // current cost in application subnets
                upgrade_persistent_state: None,
            }),
        );
        env.add_cycles(ii_canister, 150_000_000_000);

        let result =
            ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(EMPTY_WASM.clone()))?;
        match result {
            DeployArchiveResult::Failed(msg) => {
                assert_eq!(msg, "invalid wasm module".to_string())
            }
            unexpected => panic!("unexpected result: {:?}", unexpected),
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

        let result =
            ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
        match result {
            DeployArchiveResult::Failed(msg) => {
                assert_eq!(msg, "archive deployment disabled".to_string())
            }
            unexpected => panic!("unexpected result: {:?}", unexpected),
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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );
        upgrade_ii_canister(&env, ii_canister, II_WASM.clone());

        let result =
            ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
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
            arg_with_wasm_hash(EMPTY_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(EMPTY_WASM.clone()))?;
        assert!(matches!(result, DeployArchiveResult::Success(_)));

        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        )
        .unwrap();

        let result =
            ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
        let DeployArchiveResult::Success(archive_canister) = result else {
            panic!("Unexpected result")
        };

        // interact with the archive to make sure it is no longer the empty wasm
        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 0);
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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let anchor = flows::register_anchor(&env, ii_canister);

        let mut device = device_data_2();
        ii_api::add(&env, ii_canister, principal_1(), anchor, device.clone())?;

        device.purpose = Purpose::Recovery;
        let pubkey = device.pubkey.clone();
        ii_api::update(
            &env,
            ii_canister,
            principal_1(),
            anchor,
            pubkey.clone(),
            device,
        )?;

        ii_api::remove(&env, ii_canister, principal_1(), anchor, pubkey.clone())?;
        let timestamp = env
            .time()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 4);

        let register_entry = Entry {
            anchor,
            operation: Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias {
                    pubkey: device_data_1().pubkey,
                    credential_id: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
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

        let delete_entry = Entry {
            anchor,
            operation: Operation::RemoveDevice { device: pubkey },
            timestamp,
            caller: principal_1(),
            sequence_number: 3,
        };
        assert_eq!(
            entries.entries.get(3).unwrap().as_ref().unwrap(),
            &delete_entry
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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));
        flows::register_anchor(&env, ii_canister);

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 4);
        Ok(())
    }

    /// Tests integration if II has no new messages to archive.
    #[test]
    fn should_not_fail_on_empty_fetch_result() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
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
        // run until finished
        env.run_until_completion(100);

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
        Ok(())
    }

    /// Tests that the archive reports on fetch errors.
    #[test]
    fn should_report_call_errors() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // stop the II canister to provoke failures
        env.stop_canister(ii_canister)?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

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
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // stop the II canister to provoke failures
        env.stop_canister(ii_canister)?;

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        let status = archive_api::status(&env, archive_canister)?;
        assert_eq!(status.call_info.call_errors.len(), 1);
        assert_eq!(status.call_info.last_successful_fetch, None);

        // start II again to resolve the issue
        env.start_canister(ii_canister)?;
        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        let status = archive_api::status(&env, archive_canister)?;
        assert!(matches!(status.call_info.last_successful_fetch, Some(_)));
        Ok(())
    }
}

/// Tests the migration from push to pull end to end.
#[cfg(test)]
mod push_to_pull_transition_tests {
    use super::*;

    /// Tests the migration from push to pull.
    #[test]
    fn should_switch_from_push_to_pull() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        flows::register_anchor(&env, ii_canister);

        // buffer is unused due to push integration
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            0f64,
        );

        // one entry has been pushed
        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        )?;
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);

        flows::register_anchor(&env, ii_canister);

        // entry is now buffered
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            1f64,
        );

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        // buffer is empty again after the archive has pulled entries
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            0f64,
        );

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 2);
        Ok(())
    }

    /// Tests that II can also go back from pull to push.
    #[test]
    fn should_switch_from_pull_to_push() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Pull)),
        );

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        flows::register_anchor(&env, ii_canister);

        // buffer has one entry
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            1f64,
        );

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // run until finished
        env.run_until_completion(100);

        // one entry has been pulled
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            0f64,
        );
        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone(), Some(ArchiveIntegration::Push)),
        )?;
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);

        flows::register_anchor(&env, ii_canister);

        // entry is no longer buffered
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            0f64,
        );

        // entry was pushed to the archive directly
        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 2);
        Ok(())
    }
}
