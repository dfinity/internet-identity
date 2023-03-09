//! # Archive canister to Record Anchor Operations Made on Internet Identity
//!
//! This canister stores data sent to it by Internet Identity. This data should consist of candid
//! encoded [Entry] objects. In order to decouple the schema of II and this canister (which might be
//! useful in case of a rollback) the data is not decoded on write.
//!
//! ## Stable Memory Layout
//! ```text
//! ---------------------------------------- <- Page 0
//! Config Memory
//! ---------------------------------------- <- Page 1
//! Memory managed by the memory manager:
//!   - Log Index
//!   - Log Data
//!   - Anchor Index
//! ----------------------------------------
//! Unallocated space
//! ```
//!
//! ## Data Structures
//!
//! ### Log
//! The archive data is kept in a [Log] ([memory layout described here](https://docs.rs/ic-stable-structures/latest/ic_stable_structures/log/index.html))
//! with an additional index to efficiently retrieve log entries by anchor (see below).
//!
//! ### Anchor Index
//! The anchor index is a [StableBTreeMap] for the following reasons:
//! - it operates directly on stable memory
//! - it offers prefix scanning on ordered entries
//!
//! The entries are key value pairs of (anchor, timestamp, log index) -> (). The log index is chosen
//! as part of the key (rather than the value) in order to ensure uniqueness of the keys (on the IC
//! time is not guaranteed to increase between two calls).
//!
//! The index enables the following access patterns:
//! - prefix scan with anchor to retrieve entries by anchor
//! - prefix scan with (anchor, timestamp) to narrow down on the time period for a specific anchor
//! - prefix scan with (anchor, timestamp, log index) to do pagination (with the key of the first entry not included in the previous set)
use candid::{candid_method, CandidType, Deserialize, Principal};
use ic_cdk::api::call::CallResult;
use ic_cdk::api::management_canister::main::{canister_status, CanisterIdRecord};
use ic_cdk::api::stable::stable64_size;
use ic_cdk::api::time;
use ic_cdk::{call, caller, id, print, trap};
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_cdk_timers::set_timer_interval;
use ic_metrics_encoder::MetricsEncoder;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::{
    cell::Cell as StableCell, log::Log, BoundedStorable, DefaultMemoryImpl, Memory as StableMemory,
    RestrictedMemory, StableBTreeMap, Storable,
};
use internet_identity_interface::archive::*;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::*;
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::cell::RefCell;
use std::time::Duration;

#[cfg(test)]
mod anchor_index_key_tests;

/// We use restricted memory in order to ensure the separation between non-managed config memory (first page)
/// and the managed memory for the archived data & indices.
type Memory = RestrictedMemory<DefaultMemoryImpl>;
type StableLog = Log<Vec<u8>, VirtualMemory<Memory>, VirtualMemory<Memory>>;
type ConfigCell = StableCell<ConfigState, Memory>;
/// Type of the index to efficiently retrieve entries by anchor.
type LogIndex = u64;
type AnchorIndex = StableBTreeMap<AnchorIndexKey, (), VirtualMemory<Memory>>;

const GIB: u64 = 1 << 30;
const WASM_PAGE_SIZE: u64 = 65536;
const MAX_STABLE_MEMORY_SIZE: u64 = 32 * GIB;
/// The maximum number of Wasm pages that we allow to use for the stable storage.
const MAX_WASM_PAGES: u64 = MAX_STABLE_MEMORY_SIZE / WASM_PAGE_SIZE;

/// Memory ids of memory managed by the memory manager.
const LOG_INDEX_MEMORY_ID: MemoryId = MemoryId::new(0);
const LOG_DATA_MEMORY_ID: MemoryId = MemoryId::new(1);
const ANCHOR_ACCESS_INDEX_MEMORY_ID: MemoryId = MemoryId::new(2);

thread_local! {
    /// Static configuration of the archive set by init() or post_upgrade().
    static CONFIG: RefCell<ConfigCell> = RefCell::new(ConfigCell::init(config_memory(), ConfigState::Uninitialized).expect("failed to initialize stable cell"));

    /// Static memory manager to manage the memory available for blocks.
    /// To avoid a bug in the stable structures crate, we fix the bucket size to the previous default value (the new default would not have had an effect for the already deployed archive anyway).
    static MEMORY_MANAGER: RefCell<MemoryManager<Memory>> = RefCell::new(MemoryManager::init_with_bucket_size(managed_memory(), 1024));

    /// Append-only list of candid encoded entries stored in stable memory.
    static LOG: RefCell<StableLog> = with_memory_manager(|memory_manager| {
        RefCell::new(Log::init(memory_manager.get(LOG_INDEX_MEMORY_ID), memory_manager.get(LOG_DATA_MEMORY_ID)).expect("failed to initialize stable log"))
    });

    /// Index to efficiently retrieve entries by anchor.
    static ANCHOR_INDEX: RefCell<AnchorIndex> = with_memory_manager(|memory_manager| {
        RefCell::new(StableBTreeMap::init(memory_manager.get(ANCHOR_ACCESS_INDEX_MEMORY_ID)))
    });

    /// Information about the calls the archive is making to II. Not persistent in stable memory.
    static CALL_INFO: RefCell<CallInfo> = RefCell::new(CallInfo::default());
}

/// Reserve the first stable memory page for the configuration stable cell.
fn config_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 0..1)
}

/// All the memory after the initial config page is managed by the [MemoryManager].
fn managed_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 1..MAX_WASM_PAGES)
}

/// A helper function to access the configuration.
fn with_config<R>(f: impl FnOnce(&ArchiveConfig) -> R) -> R {
    CONFIG.with(|cell| f(cell.borrow().get().get()))
}

/// A helper function to access the memory manager.
fn with_memory_manager<R>(f: impl FnOnce(&MemoryManager<Memory>) -> R) -> R {
    MEMORY_MANAGER.with(|cell| f(&cell.borrow()))
}

/// A helper function to access the log.
fn with_log<R>(f: impl FnOnce(&StableLog) -> R) -> R {
    LOG.with(|cell| f(&cell.borrow()))
}

/// A helper function to access the anchor-based index.
fn with_anchor_index_mut<R>(f: impl FnOnce(&mut AnchorIndex) -> R) -> R {
    ANCHOR_INDEX.with(|cell| f(&mut cell.borrow_mut()))
}

/// A helper function to access the call info.
fn with_call_info<R>(f: impl FnOnce(&CallInfo) -> R) -> R {
    CALL_INFO.with(|cell| f(&cell.borrow_mut()))
}

/// A helper function to mutate call info.
fn with_call_info_mut<R>(f: impl FnOnce(&mut CallInfo) -> R) -> R {
    CALL_INFO.with(|cell| f(&mut cell.borrow_mut()))
}

/// Configuration state of the archive.
enum ConfigState {
    Uninitialized, // This state is only used between wasm module initialization and init().
    Initialized(ArchiveConfig),
}

impl ConfigState {
    fn get(&self) -> &ArchiveConfig {
        match &self {
            ConfigState::Uninitialized => trap("archive config not initialized"),
            ConfigState::Initialized(config) => config,
        }
    }
}

/// Configuration of the archive canister.
#[derive(Clone, Debug, CandidType, Deserialize)]
struct ArchiveConfig {
    /// This canister will accept entries only from this principal.
    ii_canister: Principal,
    /// The maximum number of entries returned in a single read canister call.
    max_entries_per_call: u16,
    /// Timestamp of the last install / upgrade of this canister.
    last_upgrade_timestamp: Timestamp,
    /// Polling interval to fetch new entries from II (in nanoseconds).
    polling_interval_ns: Option<u64>,
    /// Number of call errors to keep.
    error_buffer_limit: Option<u16>,
    /// Highest sequence number of any entry that was archived.
    highest_sequence_number: Option<u64>,
}

impl Storable for ConfigState {
    fn to_bytes(&self) -> Cow<[u8]> {
        match &self {
            ConfigState::Uninitialized => Cow::Borrowed(&[]),
            ConfigState::Initialized(config) => {
                let buf = candid::encode_one(config).expect("failed to encode archive config");
                Cow::Owned(buf)
            }
        }
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        if bytes.is_empty() {
            return ConfigState::Uninitialized;
        }
        ConfigState::Initialized(
            candid::decode_one::<ArchiveConfig>(&bytes).expect("failed to decode archive config"),
        )
    }
}

/// Index key for the anchor index.
/// Changing the (serialized) size of this value requires a stable memory migration.
#[derive(Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
struct AnchorIndexKey {
    // Attention: order of fields MUST NOT be changed because Ord is derived!
    anchor: AnchorNumber,
    timestamp: Timestamp,
    log_index: LogIndex,
}

/// Storable implementation for the index key.
/// Note: byte ordering is very important as the keys are sorted on a byte level (lower to higher index)
/// --> use big endian to ensure that the most significant bytes are compared first
impl Storable for AnchorIndexKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buf = Vec::with_capacity(std::mem::size_of::<AnchorIndexKey>());
        buf.extend(self.anchor.to_be_bytes());
        buf.extend(self.timestamp.to_be_bytes());
        buf.extend(self.log_index.to_be_bytes());
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        AnchorIndexKey {
            anchor: u64::from_be_bytes(
                TryFrom::try_from(&bytes[0..8]).expect("failed to read anchor"),
            ),
            timestamp: u64::from_be_bytes(
                TryFrom::try_from(&bytes[8..16]).expect("failed to read timestamp"),
            ),
            log_index: u64::from_be_bytes(
                TryFrom::try_from(&bytes[16..]).expect("failed to read log_index"),
            ),
        }
    }
}

impl BoundedStorable for AnchorIndexKey {
    const MAX_SIZE: u32 = std::mem::size_of::<AnchorIndexKey>() as u32;
    const IS_FIXED_SIZE: bool = true;
}

/// This method is kept for legacy compatibility and easier testability of the archive.
/// I.e. this allows rolling back Internet Identity from pull to push without rolling back the
/// archive.
#[update]
#[candid_method]
fn write_entry(anchor_number: AnchorNumber, timestamp: Timestamp, entry: ByteBuf) {
    with_config(|config| {
        if config.ii_canister != caller() {
            trap(&format!(
                "Only {} is allowed to write entries.",
                config.ii_canister
            ))
        }
    });
    write_entry_internal(anchor_number, timestamp, entry)
}

/// Fetches, archives and acknowledges a batch of entries.
/// *Note:* Must be written in a way that nothing breaks on overlapping executions of [fetch_entries].
async fn fetch_entries() {
    const FETCH_ENTRIES_METHOD: &str = "fetch_entries";
    const ACKNOWLEDGE_ENTRIES_METHOD: &str = "acknowledge_entries";

    let ii_canister = with_config(|config| config.ii_canister);
    let call_time = time();
    let fetch_result: CallResult<(Vec<BufferedEntry>,)> =
        call(ii_canister, FETCH_ENTRIES_METHOD, ()).await;

    let entries = match fetch_result {
        Ok((entries,)) => entries,
        Err((code, message)) => {
            // failed to fetch entries --> store failure information and exit early
            store_call_error(CallErrorInfo {
                time: call_time,
                canister: ii_canister,
                method: FETCH_ENTRIES_METHOD.to_string(),
                argument: ByteBuf::from(candid::encode_one(()).unwrap()),
                rejection_code: code as i32,
                message,
            });
            return;
        }
    };

    if entries.is_empty() {
        // empty fetch is considered successful
        with_call_info_mut(|info| {
            info.last_successful_fetch = Some(FetchInfo {
                timestamp: time(),
                number_of_entries: 0,
            })
        });
        // nothing to do --> exit early
        return;
    }

    let entries_count = entries.len();
    let lowest_seq_nr = entries.first().unwrap().sequence_number;
    let highest_seq_nr = entries.last().unwrap().sequence_number;
    // For the very first fetch the sequence number is not known thus we default on the one we get back from II.
    let expected_seq_nr = highest_archived_sequence_number()
        .map(|seq_nr| seq_nr + 1)
        .unwrap_or(lowest_seq_nr);

    if lowest_seq_nr > expected_seq_nr {
        // Unfortunately there is nothing further we can do as the missing entries have already been
        // pruned on the II side.
        print(format!(
            "Gap in archive entries: entries {} to {} were never archived!",
            expected_seq_nr,
            lowest_seq_nr - 1
        ))
    }

    // If this condition is false, all entries have already been archived by another invocation of fetch_entries.
    // This can happen if the fetch interval is too short or on call failures, e.g. if the last acknowledge message got rejected.
    // In such cases just the acknowledge is sent again.
    if highest_seq_nr >= expected_seq_nr {
        entries
            .into_iter()
            // due to the overlapping calls, also just parts of the entries could already have been archived
            // --> filter those out
            .filter(|e| e.sequence_number >= expected_seq_nr)
            .for_each(|e| write_entry_internal(e.anchor_number, e.timestamp, e.entry));
        set_highest_archived_sequence_number(highest_seq_nr);
    }

    let call_time = time();
    let result: CallResult<()> =
        call(ii_canister, ACKNOWLEDGE_ENTRIES_METHOD, (highest_seq_nr,)).await;

    match result {
        Ok(_) => {
            with_call_info_mut(|info| {
                info.last_successful_fetch = Some(FetchInfo {
                    timestamp: time(),
                    number_of_entries: entries_count as u16,
                })
            });
        }
        Err((code, message)) => {
            // failed to acknowledge entries --> store failure information
            store_call_error(CallErrorInfo {
                time: call_time,
                canister: ii_canister,
                method: ACKNOWLEDGE_ENTRIES_METHOD.to_string(),
                argument: ByteBuf::from(candid::encode_one(highest_seq_nr).unwrap()),
                rejection_code: code as i32,
                message,
            });
        }
    };
}

fn write_entry_internal(anchor: AnchorNumber, timestamp: Timestamp, entry: ByteBuf) {
    let idx = with_log(|log| {
        log.append(&entry.into_vec())
            .expect("failed to append log entry")
    });

    with_anchor_index_mut(|index| {
        let key = AnchorIndexKey {
            anchor,
            timestamp,
            log_index: idx,
        };

        index.insert(key, ());
    })
}

fn store_call_error(call_error: CallErrorInfo) {
    let error_limit = with_config(|config| config.error_buffer_limit.unwrap()) as usize;

    with_call_info_mut(|info| {
        if info.call_errors.len() >= error_limit {
            info.call_errors.remove(0);
        }
        info.call_errors.push(call_error);
    })
}

#[query]
#[candid_method(query)]
fn get_entries(index: Option<u64>, limit: Option<u16>) -> Entries {
    let limit = limit_or_default(limit);

    with_log(|log| {
        let length = log.len();
        let start_idx = match index {
            None => length.saturating_sub(limit as u64),
            Some(idx) => idx,
        };

        let mut entries = Vec::with_capacity(limit);
        for idx in start_idx..start_idx + limit as u64 {
            let entry = match log.get(idx) {
                None => break,
                Some(entry) => entry,
            };
            entries.push(Some(
                candid::decode_one(&entry).expect("failed to decode log entry"),
            ))
        }
        Entries { entries }
    })
}

#[query]
#[candid_method(query)]
fn get_anchor_entries(
    anchor: AnchorNumber,
    cursor: Option<Cursor>,
    limit: Option<u16>,
) -> AnchorEntries {
    let limit = limit_or_default(limit);

    with_anchor_index_mut(|index| {
        // Here we take advantage of the range scan and how the index keys are structured.
        // The index key is a concatenation of (anchor, timestamp, idx), ordered lexicographically.
        //
        // When scanning through index entries, the log index can be recovered by deserializing the
        // key using the structure above.
        //
        // A range scan with (prefix, offset) allows to iterate over the index while only returning
        // entries that have the same prefix starting with keys also matching offset and increasing
        // from there.
        //
        // Setting the prefix to anchor limits the returned keys to only those corresponding to the
        // anchor. And depending on the parameters supplied we start iterating at index key
        // - (anchor, 0, 0): given no cursor
        // - (anchor, timestamp, 0): given a Timestamp cursor
        // - (anchor, timestamp, idx): given a NextToken cursor
        let start_key = match cursor {
            None => AnchorIndexKey {
                anchor,
                timestamp: 0,
                log_index: 0,
            },
            Some(Cursor::NextToken { next_token }) => {
                let index_key = AnchorIndexKey::from_bytes(Cow::from(next_token.into_vec()));
                assert_eq!(
                    anchor, index_key.anchor,
                    "anchor does not match the next_token"
                );
                index_key
            }
            Some(Cursor::Timestamp { timestamp }) => AnchorIndexKey {
                anchor,
                timestamp,
                log_index: 0,
            },
        };
        // End of the range (exclusive) of applicable entries
        let end_key = AnchorIndexKey {
            anchor: anchor + 1,
            timestamp: 0,
            log_index: 0,
        };
        with_log(|log| {
            // Take one too many from the iterator to extract the cursor. This avoids having to
            // iterate twice or use next explicitly.
            let mut entries: Vec<(AnchorIndexKey, Vec<u8>)> = index
                .range(start_key..end_key)
                .take(limit + 1)
                .map(|(anchor_key, _)| {
                    let entry = log
                        .get(anchor_key.log_index)
                        .expect("bug: index to non-existing entry");
                    (anchor_key, entry)
                })
                .collect();

            let cursor = if entries.len() > limit {
                entries.pop().map(|(key, _)| Cursor::NextToken {
                    next_token: ByteBuf::from(key.to_bytes()),
                })
            } else {
                None
            };

            let entries = entries
                .iter()
                .map(|(_, entry)| candid::decode_one(entry).expect("failed to decode log entry"))
                .collect();

            AnchorEntries { entries, cursor }
        })
    })
}

fn limit_or_default(limit: Option<u16>) -> usize {
    with_config(|config| {
        limit
            .map(|l| l.min(config.max_entries_per_call))
            .unwrap_or(config.max_entries_per_call) as usize
    })
}

fn highest_archived_sequence_number() -> Option<u64> {
    CONFIG.with(|config| match config.borrow().get() {
        ConfigState::Uninitialized => None,
        ConfigState::Initialized(config) => config.highest_sequence_number,
    })
}

fn set_highest_archived_sequence_number(sequence_number: u64) {
    // stable cell does not allow modifying values in place --> copy and swap
    let mut config = with_config(|config| config.clone());
    config.highest_sequence_number = Some(sequence_number);
    write_config(config);
}

#[init]
#[post_upgrade]
fn initialize(arg: ArchiveInit) {
    write_config(ArchiveConfig {
        ii_canister: arg.ii_canister,
        max_entries_per_call: arg.max_entries_per_call,
        last_upgrade_timestamp: time(),
        polling_interval_ns: Some(arg.polling_interval_ns),
        error_buffer_limit: Some(arg.error_buffer_limit),
        highest_sequence_number: highest_archived_sequence_number(),
    });

    set_timer_interval(Duration::from_nanos(arg.polling_interval_ns), || {
        ic_cdk::spawn(fetch_entries())
    });
}

fn write_config(config: ArchiveConfig) {
    CONFIG.with(|cell| {
        cell.borrow_mut()
            .set(ConfigState::Initialized(config))
            .expect("failed to write archive config");
    });
}

#[query]
#[candid_method(query)]
fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split('?').collect();
    match parts[0] {
        "/metrics" => {
            let mut writer = MetricsEncoder::new(vec![], time() as i64 / 1_000_000);
            match encode_metrics(&mut writer) {
                Ok(()) => {
                    let body = writer.into_inner();
                    let headers = vec![
                        ("Content-Type".to_string(), "text/plain".to_string()),
                        ("Content-Length".to_string(), body.len().to_string()),
                    ];
                    HttpResponse {
                        status_code: 200,
                        headers,
                        body: Cow::Owned(ByteBuf::from(body)),
                        upgrade: None,
                        streaming_strategy: None,
                    }
                }
                Err(err) => HttpResponse {
                    status_code: 500,
                    headers: vec![],
                    body: Cow::Owned(ByteBuf::from(format!("Failed to encode metrics: {err}"))),
                    upgrade: None,
                    streaming_strategy: None,
                },
            }
        }
        path => HttpResponse {
            status_code: 404,
            headers: vec![],
            body: Cow::Owned(ByteBuf::from(format!("Asset {path} not found."))),
            upgrade: None,
            streaming_strategy: None,
        },
    }
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    with_config(|config| {
        w.encode_gauge(
            "ii_archive_last_upgrade_timestamp_seconds",
            Duration::from_nanos(config.last_upgrade_timestamp).as_secs_f64(),
            "Timestamp of the last upgrade of this canister.",
        )?;
        if let Some(sequence_number) = config.highest_sequence_number {
            w.encode_gauge(
                "ii_archive_highest_sequence_number",
                sequence_number as f64,
                "Highest sequence number of any archived entry.",
            )?;
        }
        Ok::<(), std::io::Error>(())
    })?;
    with_log(|log| {
        with_anchor_index_mut(|index| {
            w.gauge_vec(
                "ii_archive_entries_count",
                "Number of log entries stored in this canister.",
            )
            .unwrap()
            .value(&[("source", "log")], log.len() as f64)
            .unwrap()
            .value(&[("source", "anchor_index")], index.len() as f64)
        })?;
        w.gauge_vec("ii_archive_log_bytes", "Size of log data in bytes.")
            .unwrap()
            .value(&[("type", "entries")], log.data_size_bytes() as f64)
            .unwrap()
            .value(&[("type", "index")], log.index_size_bytes() as f64)
    })?;
    MEMORY_MANAGER.with(|cell| {
        let manager = cell.borrow();
        w.gauge_vec(
            "ii_archive_virtual_memory_pages",
            "Number of allocated virtual memory pages.",
        )
        .unwrap()
        .value(
            &[("kind", "log_index")],
            manager.get(LOG_INDEX_MEMORY_ID).size() as f64,
        )
        .unwrap()
        .value(
            &[("kind", "log_data")],
            manager.get(LOG_DATA_MEMORY_ID).size() as f64,
        )
        .unwrap()
        .value(
            &[("kind", "anchor_index")],
            manager.get(ANCHOR_ACCESS_INDEX_MEMORY_ID).size() as f64,
        )
    })?;
    w.encode_gauge(
        "ii_archive_stable_memory_pages",
        stable64_size() as f64,
        "Number of stable memory pages used by this canister.",
    )?;
    with_call_info(|call_info| {
        if let Some(successful_fetch) = &call_info.last_successful_fetch {
            w.encode_gauge(
                "ii_archive_last_successful_fetch_timestamp_seconds",
                Duration::from_nanos(successful_fetch.timestamp).as_secs_f64(),
                "Timestamp of the last successful fetch of entries from the II canister.",
            )?;
            w.encode_gauge(
                "ii_archive_last_successful_fetch_entries_count",
                successful_fetch.number_of_entries as f64,
                "Number of entries that were fetched  from the II canister with the last successful fetch.",
            )?;
        };
        Ok::<(), std::io::Error>(())
    })?;
    Ok(())
}

/// Publicly exposes the status of the archive canister.
/// This is useful to check operations or for debugging purposes.
#[update]
#[candid_method]
async fn status() -> ArchiveStatus {
    let canister_id = id();
    let (canister_status,) = canister_status(CanisterIdRecord { canister_id })
        .await
        .expect("failed to retrieve canister status");
    let config = with_config(|config| ArchiveInit {
        ii_canister: config.ii_canister,
        max_entries_per_call: config.max_entries_per_call,

        // these config parameters are only opt for candid compatibility and will be initialized
        // --> unwrap is safe to call
        polling_interval_ns: config.polling_interval_ns.unwrap(),
        error_buffer_limit: config.error_buffer_limit.unwrap(),
    });
    let call_info = with_call_info(|info| info.clone());
    ArchiveStatus {
        canister_status,
        call_info,
        init: config,
    }
}

/// This makes this Candid service self-describing, so that for example Candid UI, but also other
/// tools, can seamlessly integrate with it. The concrete interface (method name etc.) is
/// provisional, but works.
#[query]
fn __get_candid_interface_tmp_hack() -> String {
    include_str!("../archive.did").to_string()
}

fn main() {}

// Order dependent: do not move above any function annotated with #[candid_method]!
candid::export_service!();

#[cfg(test)]
mod test {
    use crate::__export_service;
    use candid::utils::{service_compatible, CandidSource};
    use std::path::Path;

    #[test]
    fn check_candid_interface_compatibility() {
        let canister_interface = __export_service();
        service_compatible(
            CandidSource::Text(&canister_interface),
            CandidSource::File(Path::new("archive.did")),
        )
        .unwrap_or_else(|e| panic!("the canister code is incompatible to the did file: {:?}", e));

        service_compatible(
            CandidSource::File(Path::new("archive.did")),
            CandidSource::Text(&canister_interface),
        )
        .unwrap_or_else(|e| panic!("the did file is incompatible to the canister code: {:?}", e));
    }
}
