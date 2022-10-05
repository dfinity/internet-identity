use canister_tests::framework;
use canister_tests::framework::{
    assert_metric, expect_user_error_with_message, log_entry, log_entry_1, log_entry_2,
    principal_1, principal_2, CallError, TIMESTAMP_1, TIMESTAMP_2, USER_NUMBER_1, USER_NUMBER_2,
    USER_NUMBER_3,
};
use ic_state_machine_tests::ErrorCode::CanisterCalledTrap;
use ic_state_machine_tests::{CanisterId, StateMachine};
use internet_identity_interface::{Cursor, HttpRequest};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::{Duration, SystemTime};
mod api;

/// Verifies that the canister can be installed successfully.
#[test]
fn should_install() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());
    let logs = api::get_entries(&env, canister_id, principal_1(), None, None)?;
    assert_eq!(logs.entries.len(), 0);
    Ok(())
}

/// Verifies that log entries are not lost when upgrading.
#[test]
fn should_keep_entries_across_upgrades() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

    let entry = log_entry_1();
    api::add_entry(
        &env,
        canister_id,
        principal_1(),
        USER_NUMBER_1,
        TIMESTAMP_1,
        candid::encode_one(entry.clone()).expect("failed to encode entry"),
    )?;

    framework::upgrade_archive_canister(&env, canister_id, framework::ARCHIVE_WASM.clone());

    let logs = api::get_entries(&env, canister_id, principal_1(), None, None)?;
    assert_eq!(logs.entries.len(), 1);
    assert_eq!(logs.entries.get(0).unwrap().as_ref().unwrap(), &entry);
    let user_logs =
        api::get_user_entries(&env, canister_id, principal_1(), USER_NUMBER_1, None, None)?;
    assert_eq!(user_logs.entries.len(), 1);
    Ok(())
}

/// Verifies the write functionality of the archive canister.
#[cfg(test)]
mod write_tests {
    use super::*;

    /// Verifies that log entries can be written to the canister.
    /// The canister does intentionally not check the payload on write so that it will never reject a log entry for compatibility reasons.
    #[test]
    fn should_write_entry() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            vec![1, 2, 3, 4], // not candid
        )?;

        // assert logs have been written without decoding entries
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_entries_count",
            2,
        );
        Ok(())
    }

    /// Verifies that only the configured ii_canister principal can write entries.
    #[test]
    fn should_reject_write_by_wrong_principal() {
        let env = StateMachine::new();

        // Configures principal_1 as the allowed principal for writing.
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        let result = api::add_entry(
            &env,
            canister_id,
            principal_2(),
            USER_NUMBER_1,
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
        let env = StateMachine::new();
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        let logs = api::get_entries(&env, canister_id, principal_1(), None, None)?;
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
        let env = StateMachine::new();
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_2,
            TIMESTAMP_2,
            candid::encode_one(log_entry_2()).expect("failed to encode entry"),
        )?;

        let logs = api::get_entries(&env, canister_id, principal_1(), None, None)?;
        assert_eq!(logs.entries.len(), 2);

        let user_1_logs =
            api::get_user_entries(&env, canister_id, principal_1(), USER_NUMBER_1, None, None)?;
        assert_eq!(user_1_logs.entries.len(), 1);
        assert_eq!(
            user_1_logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_1()
        );

        let user_2_logs =
            api::get_user_entries(&env, canister_id, principal_1(), USER_NUMBER_2, None, None)?;
        assert_eq!(user_2_logs.entries.len(), 1);
        assert_eq!(
            user_2_logs.entries.get(0).unwrap().as_ref().unwrap(),
            &log_entry_2()
        );

        let user_3_logs =
            api::get_user_entries(&env, canister_id, principal_1(), USER_NUMBER_3, None, None)?;
        assert!(user_3_logs.entries.is_empty());

        Ok(())
    }

    /// Verifies that additional entries can be retrieved by supplying next_idx.
    #[test]
    fn should_return_cursor() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        for n in 0..=10 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                n,
                n,
                candid::encode_one(log_entry(n)).expect("failed to encode entry"),
            )?;
        }

        let logs = api::get_entries(&env, canister_id, principal_1(), Some(0), None)?;
        assert_eq!(logs.entries.len(), 10);
        assert_eq!(logs.next_idx, Some(10));
        let logs = api::get_entries(&env, canister_id, principal_1(), logs.next_idx, None)?;
        assert_eq!(logs.entries.len(), 1);
        Ok(())
    }

    /// Verifies that additional user specific entries can be retrieved by supplying the cursor.
    #[test]
    fn should_return_user_cursor() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id =
            framework::install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        for n in 0..22 {
            api::add_entry(
                &env,
                canister_id,
                principal_1(),
                n % 2,
                n,
                candid::encode_one(log_entry(n)).expect("failed to encode entry"),
            )?;
        }

        for n in 0..2 {
            let logs = api::get_user_entries(&env, canister_id, principal_1(), n, None, None)?;
            assert_eq!(logs.entries.len(), 10);
            assert!(matches!(
                logs.clone().cursor,
                Some(Cursor::NextToken { next_token: _ })
            ));

            let logs =
                api::get_user_entries(&env, canister_id, principal_1(), n, logs.cursor, None)?;
            assert_eq!(logs.entries.len(), 1);
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
}

/// Tests the metrics exposed via for the HTTP.
#[cfg(test)]
mod metrics_tests {
    use super::*;
    use canister_tests::framework::{
        install_archive_canister, log_entry_1, upgrade_archive_canister,
    };

    /// Verifies that all metrics are present and have the correct timestamp.
    #[test]
    fn should_return_metrics() -> Result<(), CallError> {
        let metrics = vec![
            "ii_archive_last_upgrade_timestamp",
            "ii_archive_log_entries_count",
            "ii_archive_log_entries_size",
            "ii_archive_log_index_memory_size",
            "ii_archive_log_data_memory_size",
            "ii_archive_anchor_index_entries_count",
            "ii_archive_log_index_virtual_memory_size",
            "ii_archive_log_data_virtual_memory_size",
            "ii_archive_anchor_index_virtual_memory_size",
            "ii_archive_stable_memory_pages",
        ];
        let env = StateMachine::new();
        env.advance_time(Duration::from_secs(300)); // advance time to see it reflected on the metrics endpoint
        let canister_id = install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        let metrics_body = get_metrics(&env, canister_id);
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

    /// Verifies that the last upgrade timestamp is updated correctly.
    #[test]
    fn should_update_upgrade_timestamp() -> Result<(), CallError> {
        let env = StateMachine::new();
        env.advance_time(Duration::from_secs(300));
        let canister_id = install_archive_canister(&env, framework::ARCHIVE_WASM.clone());
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
        );
        println!("{}", get_metrics(&env, canister_id));

        env.advance_time(Duration::from_secs(300));
        upgrade_archive_canister(&env, canister_id, framework::ARCHIVE_WASM.clone());

        println!("{}", get_metrics(&env, canister_id));
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_last_upgrade_timestamp",
            env.time()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64,
        );
        Ok(())
    }

    /// Verifies that the log entries count is updated correctly.
    #[test]
    fn should_update_log_entries_count() -> Result<(), CallError> {
        let metrics = vec![
            "ii_archive_log_entries_count",
            "ii_archive_anchor_index_entries_count",
        ];

        let env = StateMachine::new();
        let canister_id = install_archive_canister(&env, framework::ARCHIVE_WASM.clone());
        for metric in metrics.clone() {
            assert_metric(&get_metrics(&env, canister_id), metric, 0);
        }

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            candid::encode_one(log_entry_1()).expect("failed to encode entry"),
        )?;

        for metric in metrics {
            assert_metric(&get_metrics(&env, canister_id), metric, 1);
        }
        Ok(())
    }

    /// Verifies that the log sizes are updated correctly.
    #[test]
    fn should_update_log_size_metrics() -> Result<(), CallError> {
        const INDEX_OVERHEAD: u64 = 40;
        const DATA_OVERHEAD: u64 = 32;

        let env = StateMachine::new();
        let canister_id = install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_index_memory_size",
            INDEX_OVERHEAD, // empty index
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_data_memory_size",
            DATA_OVERHEAD, // empty log
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_entries_size",
            0, // size of actual entries
        );

        let entry = candid::encode_one(log_entry_1()).expect("failed to encode entry");
        let entry_size = entry.len() as u64;
        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            entry,
        )?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_index_memory_size",
            INDEX_OVERHEAD + 8, // 8 bytes per entry
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_data_memory_size",
            DATA_OVERHEAD + entry_size,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_entries_size",
            entry_size, // size of actual entries
        );

        Ok(())
    }

    /// Verifies that the memory metrics are present.
    /// Update is only tested for the ii_archive_log_data_virtual_memory_size metric due to high
    /// pre-allocation factor and the number of entries required to make the index grow.
    #[test]
    fn should_show_memory_metrics() -> Result<(), CallError> {
        const WASM_PAGE_SIZE: usize = 65536;
        let env = StateMachine::new();
        let canister_id = install_archive_canister(&env, framework::ARCHIVE_WASM.clone());

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_index_virtual_memory_size",
            1,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_data_virtual_memory_size",
            1,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_anchor_index_virtual_memory_size",
            1,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_stable_memory_pages",
            3074, // the memory_manager pre-allocates a lot of memory (1000 page buckets per virtual memory and some overhead)
        );

        api::add_entry(
            &env,
            canister_id,
            principal_1(),
            USER_NUMBER_1,
            TIMESTAMP_1,
            vec![0; WASM_PAGE_SIZE], // large entry to ensure at least the data pages metric changes
        )?;

        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_index_virtual_memory_size",
            1, // does not change because the index additions are small
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_log_data_virtual_memory_size",
            2,
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_anchor_index_virtual_memory_size",
            1, // does not change because the index additions are small
        );
        assert_metric(
            &get_metrics(&env, canister_id),
            "ii_archive_stable_memory_pages",
            3074, // does not change due to pre-allocation
        );

        Ok(())
    }
}

pub fn get_metrics(env: &StateMachine, canister_id: CanisterId) -> String {
    let response = api::http_request(
        &env,
        canister_id,
        HttpRequest {
            method: "GET".to_string(),
            url: "/metrics".to_string(),
            headers: vec![],
            body: ByteBuf::new(),
        },
    )
    .expect("HTTP request to /metrics failed");
    String::from_utf8_lossy(&*response.body).to_string()
}
