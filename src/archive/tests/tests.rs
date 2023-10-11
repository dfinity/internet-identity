use canister_tests::api::archive as api;
use canister_tests::framework::*;
use internet_identity_interface::archive::types::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::{Duration, SystemTime};

/// Verifies that the canister can be installed successfully.
#[test]
fn should_install() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());
    let logs = api::get_entries(&env, canister_id, None, None)?;
    assert_eq!(logs.entries.len(), 0);
    Ok(())
}

/// Verifies that log entries are not lost when upgrading.
#[test]
fn should_keep_entries_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

    let entry = log_entry_1();
    api::add_entry(
        &env,
        canister_id,
        principal_1(),
        ANCHOR_NUMBER_1,
        TIMESTAMP_1,
        candid::encode_one(entry.clone()).expect("failed to encode entry"),
    )?;

    upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM.clone());

    let logs = api::get_entries(&env, canister_id, None, None)?;
    assert_eq!(logs.entries.len(), 1);
    assert_eq!(logs.entries.get(0).unwrap().as_ref().unwrap(), &entry);
    let user_logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_1, None, None)?;
    assert_eq!(user_logs.entries.len(), 1);
    Ok(())
}

/// Should expose status to anonymous callers.
#[test]
fn should_expose_status() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());
    let status = api::status(&env, canister_id)?;
    assert_eq!(status.canister_status.cycles, 0);
    Ok(())
}

#[cfg(test)]
mod rollback_tests {
    use super::*;
    use canister_tests::api::archive::compat::CompatEntry;

    /// Verifies that the archive can be rolled back
    #[test]
    fn should_rollback() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM_PREVIOUS.clone());

        let entry = CompatEntry::from(log_entry_1());
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(entry.clone()).expect("failed to encode entry"),
        )?;

        // upgrade
        upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM.clone());

        // rollback
        upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM_PREVIOUS.clone());

        let logs = api::get_entries(&env, canister_id, None, None)?;
        assert_eq!(logs.entries.len(), 1);
        assert_eq!(
            CompatEntry::from(logs.entries.get(0).unwrap().clone().unwrap()),
            entry
        );
        Ok(())
    }

    /// Verifies that the archive keeps entries made with the newer version when doing a rollback.
    #[test]
    fn should_keep_entries_across_rollback() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        let entry1 = CompatEntry::from(log_entry_1());
        let entry2 = CompatEntry::from(log_entry_2());
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(entry1.clone()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_2,
            TIMESTAMP_2,
            candid::encode_one(entry2.clone()).expect("failed to encode entry"),
        )?;

        // rollback
        upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM_PREVIOUS.clone());

        let logs = api::get_entries(&env, canister_id, None, None)?;
        assert_eq!(logs.entries.len(), 2);
        assert_eq!(
            CompatEntry::from(logs.entries.get(0).unwrap().clone().unwrap()),
            entry1
        );
        assert_eq!(
            CompatEntry::from(logs.entries.get(1).unwrap().clone().unwrap()),
            entry2
        );
        Ok(())
    }
}

/// Verifies the write functionality of the archive canister.
#[cfg(test)]
mod write_tests {
    use super::*;

    /// Verifies that log entries can be written to the canister.
    /// The canister does intentionally not check the payload on write so that it will never reject a log entry for compatibility reasons.
    #[test]
    fn should_write_entry() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            vec![1, 2, 3, 4], // not candid
        )?;

        // assert logs have been written without decoding entries
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_entries_count{source=\"log\"}",
            2f64,
        );
        Ok(())
    }

    /// Verifies that only the configured ii_canister principal can write entries.
    #[test]
    fn should_reject_write_by_wrong_principal() {
        let env = env();

        // Configures principal_1 as the allowed principal for writing.
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        let result = api::add_entry(
            &env,
            canister_id,
            principal_2(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        );
        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Only [\\w-]+ is allowed to write entries\\.").unwrap(),
        );
    }
}

/// Verifies the read functionality of the archive canister.
#[cfg(test)]
mod read_tests {
    use super::*;

    /// Verifies that a previously written entry can be read again.
    #[test]
    fn should_read_previously_written_entry() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        let logs = api::get_entries(&env, canister_id, None, None)?;
        assert_eq!(logs.entries.len(), 1);
        assert_eq!(
            logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_1()
        );
        Ok(())
    }

    /// Verifies that entries can be retrieved by user_number.
    #[test]
    fn should_return_logs_per_user() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_2,
            TIMESTAMP_2,
            candid::encode_one(log_entry_2()).expect("failed to encode entry"),
        )?;

        let logs = api::get_entries(&env, canister_id, None, None)?;
        assert_eq!(logs.entries.len(), 2);

        let user_1_logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_1, None, None)?;
        assert_eq!(user_1_logs.entries.len(), 1);
        assert_eq!(
            user_1_logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_1()
        );

        let user_2_logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_2, None, None)?;
        assert_eq!(user_2_logs.entries.len(), 1);
        assert_eq!(
            user_2_logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_2()
        );

        let user_3_logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_3, None, None)?;
        assert!(user_3_logs.entries.is_empty());

        Ok(())
    }

    /// Verifies that additional entries can be retrieved by supplying next_idx.
    #[test]
    fn should_return_only_limit_many_entries() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        for n in 0..=10 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                n,
                n,
                candid::encode_one(log_entry(n, n, n)).expect("failed to encode entry"),
            )?;
        }

        let logs = api::get_entries(&env, canister_id, Some(0), Some(3))?;
        assert_eq!(logs.entries.len(), 3);
        Ok(())
    }

    /// Verifies that additional user specific entries can be retrieved by supplying the cursor.
    #[test]
    fn should_return_user_cursor() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        for n in 0..24 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                n % 2,
                n,
                candid::encode_one(log_entry(n, n, n % 2)).expect("failed to encode entry"),
            )?;
        }

        for n in 0..2 {
            let logs = api::get_anchor_entries(&env, canister_id, n, None, None)?;
            assert_eq!(logs.entries.len(), 10);
            assert!(matches!(
                logs.clone().cursor,
                Some(Cursor::NextToken { next_token: _ })
            ));

            let logs = api::get_anchor_entries(&env, canister_id, n, logs.cursor, None)?;
            assert_eq!(logs.entries.len(), 2);
            assert_eq!(
                logs.entries
                    .get(0)
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .sequence_number,
                20 + n
            );
        }

        Ok(())
    }

    /// Verifies that only entries belonging to the same anchor are returned (even if there are less than limit many).
    #[test]
    fn should_only_return_entries_belonging_to_anchor() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        for n in 0..22 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                n % 2,
                n,
                candid::encode_one(log_entry(n, n, n % 2)).expect("failed to encode entry"),
            )?;
        }

        let logs = api::get_anchor_entries(
            &env,
            canister_id,
            0,
            Some(Cursor::Timestamp { timestamp: 10 }),
            None,
        )?;
        assert_eq!(logs.entries.len(), 6);
        assert_eq!(logs.entries.get(5).unwrap().as_ref().unwrap().anchor, 0);

        Ok(())
    }

    /// Verifies that entries can be retrieved filtered by timestamp.
    #[test]
    fn should_filter_by_timestamp() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_2,
            candid::encode_one(log_entry_2()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_3,
            candid::encode_one(log_entry(3, TIMESTAMP_3, ANCHOR_NUMBER_1))
                .expect("failed to encode entry"),
        )?;

        let logs = api::get_anchor_entries(
            &env,
            canister_id,
            ANCHOR_NUMBER_1,
            Some(Cursor::Timestamp {
                timestamp: TIMESTAMP_2,
            }),
            None,
        )?;
        assert_eq!(logs.entries.len(), 2);
        assert_eq!(
            logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_2()
        );
        assert_eq!(
            logs.entries.get(1).unwrap().as_ref().unwrap(),
            &log_entry(3, TIMESTAMP_3, ANCHOR_NUMBER_1)
        );
        Ok(())
    }

    /// Verifies that entries retrieved by anchor are correctly ordered by timestamp.
    #[test]
    fn should_order_by_timestamp() -> Result<(), CallError> {
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            1u64,
            candid::encode_one(log_entry(0, 1, ANCHOR_NUMBER_1)).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            1u64 << 8,
            candid::encode_one(log_entry(1, 1u64 << 8, ANCHOR_NUMBER_1))
                .expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            1u64 << 16,
            candid::encode_one(log_entry(2, 1u64 << 16, ANCHOR_NUMBER_1))
                .expect("failed to encode entry"),
        )?;

        let logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_1, None, None)?;
        assert_eq!(logs.entries.len(), 3);
        assert_eq!(logs.entries.get(0).unwrap().as_ref().unwrap().timestamp, 1);
        assert_eq!(
            logs.entries.get(1).unwrap().as_ref().unwrap().timestamp,
            1u64 << 8
        );
        Ok(())
    }

    /// Verifies that entries retrieved by anchor are correctly ordered by index.
    #[test]
    fn should_order_by_index() -> Result<(), CallError> {
        let env = env();
        // use high max_entries_per_call so that we get all the entries in one go
        let config = candid::encode_one(ArchiveInit {
            ii_canister: principal_1(),
            max_entries_per_call: 1000,
            polling_interval_ns: Duration::from_secs(1).as_nanos() as u64,
            error_buffer_limit: 1,
        })
        .unwrap();
        let canister_id = env.create_canister(None);
        env.install_canister(canister_id, ARCHIVE_WASM.clone(), config, None);

        // 257 entries because we need the index to not fit in a single byte
        for i in 0..257 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                ANCHOR_NUMBER_1,
                TIMESTAMP_1,
                candid::encode_one(log_entry(i, TIMESTAMP_1, ANCHOR_NUMBER_1))
                    .expect("failed to encode entry"),
            )?;
        }

        let logs = api::get_anchor_entries(&env, canister_id, ANCHOR_NUMBER_1, None, None)?;
        assert_eq!(logs.entries.len(), 257);

        for i in 0..257 {
            assert_eq!(
                logs.entries
                    .get(i)
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .sequence_number,
                i as u64
            );
        }
        Ok(())
    }
}

/// Tests the metrics exposed via for the HTTP.
#[cfg(test)]
mod metrics_tests {
    use super::*;

    /// Verifies that all metrics are present and have the correct timestamp.
    #[test]
    fn should_return_metrics() -> Result<(), CallError> {
        let metrics = vec![
            "ii_archive_last_upgrade_timestamp_seconds",
            "ii_archive_entries_count{source=\"log\"}",
            "ii_archive_entries_count{source=\"anchor_index\"}",
            "ii_archive_log_bytes{type=\"entries\"}",
            "ii_archive_log_bytes{type=\"index\"}",
            "ii_archive_virtual_memory_pages{kind=\"log_index\"}",
            "ii_archive_virtual_memory_pages{kind=\"log_data\"}",
            "ii_archive_virtual_memory_pages{kind=\"anchor_index\"}",
            "ii_archive_stable_memory_pages",
            // The metrics
            //   * ii_archive_last_successful_fetch_timestamp_seconds
            //   * ii_archive_last_successful_fetch_entries_count
            //   * ii_archive_highest_sequence_number
            // are only provided, if there is a successful fetch, which requires II to be deployed.
            // These metrics are tested in src/internet_identity/tests/archive_integration_tests_pull.rs.
        ];
        let env = env();
        env.advance_time(Duration::from_secs(300)); // advance time to see it reflected on the metrics endpoint
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        let metrics_body = get_metrics(&env, canister_id);
        for metric in metrics {
            let (_, metric_timestamp) = parse_metric(&metrics_body, metric);
            assert_eq!(
                metric_timestamp,
                env.get_time(),
                "metric timestamp did not match PocketIC time"
            )
        }
        Ok(())
    }

    /// Verifies that the last upgrade timestamp is updated correctly.
    #[test]
    fn should_update_upgrade_timestamp() -> Result<(), CallError> {
        let env = env();
        env.advance_time(Duration::from_secs(300));
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_last_upgrade_timestamp_seconds",
            env.get_time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        );
        println!("{}", get_metrics(&env, canister_id));

        env.advance_time(Duration::from_secs(300));
        upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM.clone());

        println!("{}", get_metrics(&env, canister_id));
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_last_upgrade_timestamp_seconds",
            env.get_time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        );
        Ok(())
    }

    /// Verifies that the log entries count is updated correctly.
    #[test]
    fn should_update_log_entries_count() -> Result<(), CallError> {
        let metrics = vec![
            "ii_archive_entries_count{source=\"log\"}",
            "ii_archive_entries_count{source=\"anchor_index\"}",
        ];

        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());
        for metric in metrics.clone() {
            assert_metric(&get_metrics(&env, canister_id), metric, 0f64);
        }

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;

        for metric in metrics {
            assert_metric(&get_metrics(&env, canister_id), metric, 1f64);
        }
        Ok(())
    }

    /// Verifies that the log sizes are updated correctly.
    #[test]
    fn should_update_log_size_metrics() -> Result<(), CallError> {
        const INDEX_OVERHEAD: f64 = 40f64;
        const DATA_OVERHEAD: f64 = 32f64;

        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_bytes{type=\"index\"}",
            INDEX_OVERHEAD, // empty index
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_bytes{type=\"entries\"}",
            DATA_OVERHEAD, // empty log
        );

        let entry = candid::encode_one(log_entry_1()).expect("failed to encode entry");
        let entry_size = entry.len() as f64;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            entry,
        )?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_bytes{type=\"index\"}",
            INDEX_OVERHEAD + 8f64, // 8 bytes per entry
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_bytes{type=\"entries\"}",
            DATA_OVERHEAD + entry_size,
        );
        Ok(())
    }

    /// Verifies that the memory metrics are present.
    /// Update is only tested for the ii_archive_log_data_virtual_memory_size metric due to high
    /// pre-allocation factor and the number of entries required to make the index grow.
    #[test]
    fn should_show_memory_metrics() -> Result<(), CallError> {
        const WASM_PAGE_SIZE: usize = 65536;
        let env = env();
        let canister_id = install_archive_canister(&env, ARCHIVE_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"log_index\"}",
            1f64,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"log_data\"}",
            1f64,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"anchor_index\"}",
            1f64,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_stable_memory_pages",
            3074f64, // the memory_manager pre-allocates a lot of memory (1024 page buckets per virtual memory and some overhead)
        );

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            ANCHOR_NUMBER_1,
            TIMESTAMP_1,
            vec![0; WASM_PAGE_SIZE], // large entry to ensure at least the data pages metric changes
        )?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"log_index\"}",
            1f64, // does not change because the index additions are small
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"log_data\"}",
            2f64,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_virtual_memory_pages{kind=\"anchor_index\"}",
            1f64, // does not change because the index additions are small
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_stable_memory_pages",
            3074f64, // does not change due to pre-allocation
        );

        Ok(())
    }
}

/// Verifies that the archive is compatible to stable memory backups.
#[cfg(test)]
mod stable_memory_tests {
    use super::*;

    /// Tests a backup of the initial stable memory layout with all the operations existing at that time.
    #[test]
    fn should_restore_backup() {
        const TIMESTAMP: Timestamp = 1620328630000000000;
        const ANCHOR: AnchorNumber = 10_000;
        let env = env();
        let canister_id = install_archive_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(&env, canister_id, "stable_memory/archive_v1.bin.gz");
        upgrade_archive_canister(&env, canister_id, ARCHIVE_WASM.clone());

        let entries = api::get_entries(&env, canister_id, None, None).unwrap();
        assert_eq!(entries.entries.len(), 4);

        let register_entry = Entry {
            anchor: ANCHOR,
            operation: Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias {
                    pubkey: device_data_1().pubkey,
                    credential_id: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    metadata_keys: None,
                },
            },
            timestamp: TIMESTAMP,
            caller: principal_1(),
            sequence_number: 0,
        };
        assert_eq!(
            entries.entries.get(0).unwrap().as_ref().unwrap(),
            &register_entry
        );

        let add_entry = Entry {
            anchor: ANCHOR,
            operation: Operation::AddDevice {
                device: DeviceDataWithoutAlias {
                    pubkey: ByteBuf::from(PUBKEY_2),
                    credential_id: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    metadata_keys: None,
                },
            },
            timestamp: TIMESTAMP,
            caller: principal_1(),
            sequence_number: 1,
        };
        assert_eq!(
            entries.entries.get(1).unwrap().as_ref().unwrap(),
            &add_entry
        );

        let update_entry = Entry {
            anchor: ANCHOR,
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
            timestamp: TIMESTAMP,
            caller: principal_1(),
            sequence_number: 2,
        };
        assert_eq!(
            entries.entries.get(2).unwrap().as_ref().unwrap(),
            &update_entry
        );

        let delete_entry = Entry {
            anchor: ANCHOR,
            operation: Operation::RemoveDevice {
                device: device_data_2().pubkey,
            },
            timestamp: TIMESTAMP,
            caller: principal_1(),
            sequence_number: 3,
        };
        assert_eq!(
            entries.entries.get(3).unwrap().as_ref().unwrap(),
            &delete_entry
        );
    }
}
