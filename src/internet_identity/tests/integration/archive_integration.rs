use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_webauthn_authn_method, test_authn_method,
};
use candid::Principal;
use canister_tests::api::archive as archive_api;
use canister_tests::api::internet_identity as ii_api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::archive::types::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::{CallError, PocketIc};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

fn setup_ii(env: &PocketIc, arg: Option<InternetIdentityInit>) -> CanisterId {
    let ii_canister = install_ii_canister_with_arg(env, II_WASM.clone(), arg);
    assert_eq!(
        ii_api::stats(env, ii_canister)
            .unwrap()
            .storage_layout_version,
        9
    );
    ii_canister
}

/// Tests related to archive deployment (using II).
#[cfg(test)]
mod deployment_tests {
    use super::*;

    #[test]
    fn should_deploy_archive() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment failed");

        assert!(matches!(result, DeployArchiveResult::Success(_)));
    }

    #[test]
    fn should_deploy_archive_with_cycles() {
        let env = env();
        let arg = Some(InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                entries_buffer_limit: 0,
                polling_interval_ns: 0,
                entries_fetch_limit: 0,
            }),
            canister_creation_cycles_cost: Some(100_000_000_000), // current cost in application subnets
            ..InternetIdentityInit::default()
        });
        let ii_canister = setup_ii(&env, arg);
        env.add_cycles(ii_canister, 150_000_000_000);

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment failed");

        assert!(matches!(result, DeployArchiveResult::Success(_)));
        assert_eq!(env.cycle_balance(ii_canister), 50_000_000_000);
    }

    #[test]
    fn should_not_deploy_wrong_wasm() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));

        let result = ii_api::deploy_archive(&env, ii_canister, &EMPTY_WASM)
            .expect("archive deployment API call failed");

        assert!(matches!(result, DeployArchiveResult::Failed(_)));
        let stats = ii_api::stats(&env, ii_canister).expect("failed to get stats");
        assert!(stats.archive_info.archive_canister.is_none());
    }

    #[test]
    fn should_not_deploy_archive_when_disabled() {
        let env = env();
        let ii_canister = setup_ii(&env, Some(InternetIdentityInit::default()));

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment API call failed");

        assert!(matches!(result, DeployArchiveResult::Failed(_)));
        let stats = ii_api::stats(&env, ii_canister).expect("failed to get stats");
        assert!(stats.archive_info.archive_canister.is_none());
    }

    #[test]
    fn should_keep_archive_module_hash_across_upgrades() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));

        upgrade_ii_canister(&env, ii_canister, II_WASM.clone());
        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment failed");

        assert!(matches!(result, DeployArchiveResult::Success(_)));
    }

    #[test]
    fn should_upgrade_the_archive() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(EMPTY_WASM.clone()));
        let result = ii_api::deploy_archive(&env, ii_canister, &EMPTY_WASM)
            .expect("archive deployment failed");
        assert!(matches!(result, DeployArchiveResult::Success(_)));

        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        )
        .unwrap();

        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment failed");
        let DeployArchiveResult::Success(archive_canister) = result else {
            panic!("Unexpected result")
        };

        // interact with the archive to make sure it is no longer the empty wasm
        let entries = archive_api::get_entries(&env, archive_canister, None, None)
            .expect("failed to get entries");
        assert_eq!(entries.entries.len(), 0);
    }

    #[test]
    fn should_upgrade_archive_with_only_config_changed() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("failed to deploy archive");
        let DeployArchiveResult::Success(archive_canister) = result else {
            panic!("Unexpected result")
        };

        let status = archive_api::status(&env, archive_canister).expect("failed to get status");
        assert_eq!(status.init.polling_interval_ns, 1_000_000_000);

        // change archive config
        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                archive_config: Some(ArchiveConfig {
                    module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                    entries_buffer_limit: 10,
                    polling_interval_ns: 5_000,
                    entries_fetch_limit: 10,
                }),
                ..InternetIdentityInit::default()
            }),
        )
        .unwrap();
        let result = ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("failed to deploy archive");

        assert!(matches!(result, DeployArchiveResult::Success(_)));
        let status = archive_api::status(&env, archive_canister).expect("failed to get status");
        assert_eq!(status.init.polling_interval_ns, 5_000);
    }
}

/// Test the functionality of pulling entries from II.
#[cfg(test)]
mod pull_entries_tests {
    use super::*;

    #[test]
    fn should_record_anchor_operations() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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
        let timestamp = time(&env);

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_expected_entries(timestamp, entries)?;
        Ok(())
    }

    #[test]
    fn should_restore_archive_buffer_from_stable_memory_backup() -> Result<(), CallError> {
        let env = env();
        let ii_canister = install_ii_canister(&env, EMPTY_WASM.clone());
        // re-create the archive canister with II as the controller and the canister id matching the restored backup
        env.create_canister_with_id(
            Some(ii_canister),
            None,
            Principal::from_text("rrkah-fqaaa-aaaaa-aaaaq-cai").unwrap(),
        )
        .expect("failed to create archive canister");

        // restore stable memory backup with buffered entries in persistent state
        restore_compressed_stable_memory(
            &env,
            ii_canister,
            "stable_memory/buffered_archive_entries_v9.bin.gz",
        );
        upgrade_ii_canister_with_arg(
            &env,
            ii_canister,
            II_WASM.clone(),
            arg_with_wasm_hash(ARCHIVE_WASM.clone()),
        )
        .expect("II upgrade failed");
        // the upgrade auto-migrates the storage layout to v9
        assert_eq!(
            ii_api::stats(&env, ii_canister)
                .unwrap()
                .storage_layout_version,
            9
        );
        // deploy the actual archive wasm
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        let timestamp = time(&env);

        // have the archive fetch the (restored) buffered entries
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // progress after the inter-canister call
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_expected_entries(timestamp, entries)
    }

    fn assert_expected_entries(timestamp: u64, entries: Entries) -> Result<(), CallError> {
        let anchor = 10_000;
        let pubkey = device_data_2().pubkey.clone();
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
        assert_eq_with_lenient_timestamp(
            entries.entries.first().unwrap().as_ref().unwrap(),
            &register_entry,
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
        assert_eq_with_lenient_timestamp(
            entries.entries.get(1).unwrap().as_ref().unwrap(),
            &add_entry,
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
        assert_eq_with_lenient_timestamp(
            entries.entries.get(2).unwrap().as_ref().unwrap(),
            &update_entry,
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
        assert_eq_with_lenient_timestamp(
            entries.entries.get(3).unwrap().as_ref().unwrap(),
            &replace_entry,
        );

        let delete_entry = Entry {
            anchor,
            operation: Operation::RemoveDevice { device: pubkey },
            timestamp,
            caller: principal_1(),
            sequence_number: 4,
        };
        assert_eq_with_lenient_timestamp(
            entries.entries.get(4).unwrap().as_ref().unwrap(),
            &delete_entry,
        );
        Ok(())
    }

    /// Check that the archived entry is as expected with up to 1 second leniency in the timestamp.
    /// The state machine increases the time every execution round, so the timestamp of the entry
    /// is brittle to predict exactly.
    /// For the functionality to be considered correct, the timestamp should be close to the expected
    /// timestamp, but not to nanosecond precision.
    fn assert_eq_with_lenient_timestamp(actual: &Entry, expected: &Entry) {
        assert_eq!(actual.operation, expected.operation);
        assert_eq!(actual.anchor, expected.anchor);
        assert_eq!(actual.caller, expected.caller);
        assert_eq!(actual.sequence_number, expected.sequence_number);
        assert!(
            actual.timestamp.abs_diff(expected.timestamp)
                < Duration::from_secs(1).as_nanos() as u64,
            "Timestamp difference is bigger than 1 second! actual: {}, expected: {}",
            actual.timestamp,
            expected.timestamp
        );
    }

    #[test]
    fn should_record_metadata_for_new_device() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        const METADATA_KEY: &str = "key";
        let timestamp = time(&env);

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
        assert_eq_with_lenient_timestamp(
            entries.entries.first().unwrap().as_ref().unwrap(),
            &expected_register_entry,
        );
        Ok(())
    }

    #[test]
    fn should_record_metadata_change() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        const METADATA_KEY: &str = "key";
        let timestamp = time(&env);

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
        assert_eq_with_lenient_timestamp(
            entries.entries.get(1).unwrap().as_ref().unwrap(),
            &expected_update_entry,
        );
        Ok(())
    }

    #[test]
    fn should_record_identity_metadata_replace() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        const METADATA_KEY: &str = "some-metadata-key";
        let timestamp = time(&env);

        let device = DeviceData::auth_test_device();
        let anchor = flows::register_anchor_with_device(&env, ii_canister, &device);

        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        let metadata = HashMap::from_iter(vec![(
            METADATA_KEY.to_string(),
            MetadataEntryV2::String("some value".to_string()),
        )]);

        ii_api::api_v2::identity_metadata_replace(
            &env,
            ii_canister,
            device.principal(),
            anchor,
            &metadata,
        )?
        .expect("identity_metadata_replace failed");

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
        assert_eq!(entries.entries.len(), 1);

        let expected_metadata_entry = Entry {
            anchor,
            operation: Operation::IdentityMetadataReplace {
                metadata_keys: metadata.keys().cloned().collect(),
            },
            timestamp,
            caller: device.principal(),
            sequence_number: 0,
        };
        assert_eq_with_lenient_timestamp(
            entries.entries.first().unwrap().as_ref().unwrap(),
            &expected_metadata_entry,
        );
        Ok(())
    }

    #[test]
    fn should_fetch_multiple_times() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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

    #[test]
    fn should_succeed_on_empty_fetch_result() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert!(status.call_info.call_errors.is_empty());
        let fetch_info = status
            .call_info
            .last_successful_fetch
            .expect("no fetch info");
        assert_eq!(fetch_info.number_of_entries, 0);
        assert!(
            fetch_info.timestamp.abs_diff(time(&env)) < Duration::from_secs(1).as_nanos() as u64
        );
        Ok(())
    }

    #[test]
    fn should_report_correct_number_of_fetched_entries() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        assert!(env.canister_exists(archive_canister));

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        // the archive polls for entries once per second
        env.advance_time(Duration::from_secs(2));
        // execute the timer
        env.tick();
        // resume after inter-canister call
        env.tick();

        let status = archive_api::status(&env, archive_canister)?;
        assert!(status.call_info.call_errors.is_empty());
        let fetch_info = status
            .call_info
            .last_successful_fetch
            .expect("no fetch info");
        assert_eq!(fetch_info.number_of_entries, 3);
        assert!(
            fetch_info.timestamp.abs_diff(time(&env)) < Duration::from_secs(1).as_nanos() as u64
        );

        assert_metric(
            &get_metrics(&env, archive_canister),
            "ii_archive_last_successful_fetch_timestamp_seconds",
            Duration::from_nanos(time(&env)).as_secs() as f64,
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

    #[test]
    fn should_report_archive_config_metrics() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        let init_arg = InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                entries_buffer_limit: 20_000,
                polling_interval_ns: Duration::from_secs(3).as_nanos() as u64,
                entries_fetch_limit: 10,
            }),
            ..InternetIdentityInit::default()
        };
        upgrade_ii_canister_with_arg(&env, ii_canister, II_WASM.clone(), Some(init_arg))
            .expect("II upgrade failed");
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
    }

    #[test]
    fn should_report_archive_entries_metrics() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
        let init_arg = InternetIdentityInit {
            archive_config: Some(ArchiveConfig {
                module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                entries_buffer_limit: 20_000,
                polling_interval_ns: Duration::from_secs(60).as_nanos() as u64,
                entries_fetch_limit: 2, // only fetch 2 entries at a time
            }),
            ..InternetIdentityInit::default()
        };
        upgrade_ii_canister_with_arg(&env, ii_canister, II_WASM.clone(), Some(init_arg))
            .expect("II upgrade failed");
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);

        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_sequence_number",
            0f64,
        );
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            0f64,
        );

        // stop the archive so that the entries start piling up in the buffer
        env.stop_canister(archive_canister, Some(ii_canister))
            .expect("failed to stop archive");

        for _ in 0..3 {
            flows::register_anchor(&env, ii_canister);
        }

        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_sequence_number",
            3f64,
        );
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            3f64,
        );

        // start the archive again so that the entries are fetched from the buffer
        env.start_canister(archive_canister, Some(ii_canister))
            .expect("failed to start archive");

        // allow the archive to poll once
        env.advance_time(Duration::from_secs(60));
        env.tick();

        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_archive_sequence_number",
            3f64,
        );
        assert_metric(
            &get_metrics(&env, ii_canister),
            "internet_identity_buffered_archive_entries",
            1f64, // one entry should be left in the buffer as the fetch limit is 2
        );
    }

    #[test]
    fn should_report_call_errors() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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
            time: time(&env),
            canister: ii_canister,
            method: "fetch_entries".to_string(),
            argument: ByteBuf::from(candid::encode_one(()).unwrap()),
            rejection_code: 5,
            message: format!("Canister {} is stopped", ii_canister.to_text()),
        };
        let actual_error = status.call_info.call_errors.first().unwrap();
        assert_eq!(actual_error.canister, expected_error.canister);
        assert_eq!(actual_error.method, expected_error.method);
        assert_eq!(actual_error.argument, expected_error.argument);
        assert_eq!(actual_error.rejection_code, expected_error.rejection_code);
        assert_eq!(actual_error.message, expected_error.message);
        assert!(
            actual_error.time.abs_diff(expected_error.time)
                < Duration::from_secs(1).as_nanos() as u64,
            "Timestamp difference is bigger than 1 second! actual: {}, expected: {}",
            actual_error.time,
            expected_error.time
        );
        Ok(())
    }

    #[test]
    fn should_recover_after_error() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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
        assert!(status.call_info.last_successful_fetch.is_some());
        Ok(())
    }

    #[test]
    fn should_return_entries_ordered() -> Result<(), CallError> {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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

    #[test]
    fn should_not_allow_wrong_caller_to_fetch_entries() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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

    #[test]
    fn should_not_allow_wrong_caller_to_acknowledge_entries() {
        let env = env();
        let ii_canister = setup_ii(&env, arg_with_wasm_hash(ARCHIVE_WASM.clone()));
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

    #[test]
    fn should_not_accept_changes_if_archive_buffer_full() {
        let env = env();
        let ii_canister = install_ii_canister_with_arg(
            &env,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                archive_config: Some(ArchiveConfig {
                    module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                    entries_buffer_limit: 3,
                    polling_interval_ns: 0,
                    entries_fetch_limit: 0,
                }),
                ..InternetIdentityInit::default()
            }),
        );
        let archive_canister = deploy_archive_via_ii(&env, ii_canister);
        // stop the archive so that the entries start piling up in the buffer
        env.stop_canister(archive_canister, Some(ii_canister))
            .expect("failed to stop archive");

        let authn_method = test_authn_method();
        let identity = create_identity_with_authn_method(&env, ii_canister, &authn_method);
        for i in 0..2 {
            api_v2::authn_method_add(
                &env,
                ii_canister,
                authn_method.principal(),
                identity,
                &sample_webauthn_authn_method(i + 1),
            )
            .expect("API call failed")
            .expect("authn_method_add failed");
        }

        let result = api_v2::authn_method_add(
            &env,
            ii_canister,
            authn_method.principal(),
            identity,
            &sample_webauthn_authn_method(3),
        );
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("cannot archive operation, archive entries buffer limit reached").unwrap(),
        );
    }
}
