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
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api::stable::stable64_size;
use ic_cdk::api::time;
use ic_cdk::{caller, trap};
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::{
    cell::Cell as StableCell, log::Log, DefaultMemoryImpl, Memory as StableMemory,
    RestrictedMemory, StableBTreeMap, Storable,
};
use internet_identity_interface::*;
use metrics_encoder::MetricsEncoder;
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::cell::RefCell;

/// We use restricted memory in order to ensure the separation between non-managed config memory (first page)
/// and the managed memory for the archived data & indices.
type Memory = RestrictedMemory<DefaultMemoryImpl>;
type StableLog = Log<VirtualMemory<Memory>, VirtualMemory<Memory>>;
type ConfigCell = StableCell<ConfigState, Memory>;
/// Type of the index to efficiently retrieve entries by anchor.
type AnchorIndex = StableBTreeMap<VirtualMemory<Memory>, AnchorIndexKey, ()>;

const GIB: u64 = 1 << 30;
const WASM_PAGE_SIZE: u64 = 65536;
const MAX_STABLE_MEMORY_SIZE: u64 = 8 * GIB;
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
    static MEMORY_MANAGER: RefCell<MemoryManager<Memory>> = RefCell::new(MemoryManager::init(managed_memory()));

    /// Append-only list of candid encoded entries stored in stable memory.
    static LOG: RefCell<StableLog> = with_memory_manager(|memory_manager| {
        RefCell::new(Log::init(memory_manager.get(LOG_INDEX_MEMORY_ID), memory_manager.get(LOG_DATA_MEMORY_ID)).expect("failed to initialize stable log"))
    });

    /// Index to efficiently retrieve entries by anchor.
    static ANCHOR_INDEX: RefCell<AnchorIndex> = with_memory_manager(|memory_manager| {
        RefCell::new(StableBTreeMap::init(memory_manager.get(ANCHOR_ACCESS_INDEX_MEMORY_ID), std::mem::size_of::<AnchorIndexKey>() as u32, 0))
    });
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
    CONFIG.with(|cell| f(&cell.borrow().get().get()))
}

/// A helper function to access the memory manager.
fn with_memory_manager<R>(f: impl FnOnce(&MemoryManager<Memory>) -> R) -> R {
    MEMORY_MANAGER.with(|cell| f(&*cell.borrow()))
}

/// A helper function to access the log.
fn with_log<R>(f: impl FnOnce(&StableLog) -> R) -> R {
    LOG.with(|cell| f(&*cell.borrow()))
}

/// A helper function to access the anchor-based index.
fn with_anchor_index_mut<R>(f: impl FnOnce(&mut AnchorIndex) -> R) -> R {
    ANCHOR_INDEX.with(|cell| f(&mut *cell.borrow_mut()))
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

    fn from_bytes(bytes: Vec<u8>) -> Self {
        if bytes.len() == 0 {
            return ConfigState::Uninitialized;
        }
        ConfigState::Initialized(
            candid::decode_one::<ArchiveConfig>(&bytes).expect("failed to decode archive config"),
        )
    }
}

/// Index key for the anchor index.
/// Changing the (serialized) size of this value requires a stable memory migration.
#[derive(Debug)]
struct AnchorIndexKey {
    anchor: Anchor,
    timestamp: Timestamp,
    log_index: u64,
}

impl AnchorIndexKey {
    fn to_anchor_offset(&self) -> Vec<u8> {
        let mut buf =
            Vec::with_capacity(std::mem::size_of::<Timestamp>() + std::mem::size_of::<u64>());
        buf.extend(&self.timestamp.to_le_bytes());
        buf.extend(&self.log_index.to_le_bytes());
        buf
    }
}

impl Storable for AnchorIndexKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buf = Vec::with_capacity(std::mem::size_of::<AnchorIndexKey>());
        buf.extend(&self.anchor.to_le_bytes());
        buf.extend(&self.timestamp.to_le_bytes());
        buf.extend(&self.log_index.to_le_bytes());
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Vec<u8>) -> Self {
        AnchorIndexKey {
            anchor: u64::from_le_bytes(
                TryFrom::try_from(&bytes[0..8]).expect("failed to read anchor"),
            ),
            timestamp: u64::from_le_bytes(
                TryFrom::try_from(&bytes[8..16]).expect("failed to read timestamp"),
            ),
            log_index: u64::from_le_bytes(
                TryFrom::try_from(&bytes[16..]).expect("failed to read log_index"),
            ),
        }
    }
}

#[update]
fn write_entry(anchor: Anchor, timestamp: Timestamp, entry: ByteBuf) {
    with_config(|config| {
        if config.ii_canister != caller() {
            trap(&format!(
                "Only {} is allowed to write entries.",
                config.ii_canister
            ))
        }
    });
    let idx = with_log(|log| {
        log.append(entry.as_ref())
            .expect("failed to append log entry")
    });

    with_anchor_index_mut(|index| {
        let key = AnchorIndexKey {
            anchor,
            timestamp,
            log_index: idx as u64,
        };

        // the only way this expect can trigger is when the key size is wrong.
        // On other failures (e.g. no more stable memory available) the underlying StableBTreeMap will panic directly.
        index.insert(key, ()).expect("bug: key size mismatch");
    })
}

#[query]
fn get_entries(index: Option<u64>, limit: Option<u16>) -> Entries {
    let limit = limit_or_default(limit);

    with_log(|log| {
        let length = log.len();
        let start_idx = match index {
            None => length.saturating_sub(limit),
            Some(idx) => idx as usize,
        };

        let next_idx = if start_idx + limit < length {
            Some((start_idx + limit) as u64)
        } else {
            None
        };

        let mut entries = Vec::with_capacity(limit);
        for idx in start_idx..start_idx + limit {
            let entry = match log.get(idx) {
                None => break,
                Some(entry) => entry,
            };
            entries.push(Some(
                candid::decode_one(&entry).expect("failed to decode log entry"),
            ))
        }
        Entries { entries, next_idx }
    })
}

#[query]
fn get_anchor_entries(anchor: Anchor, cursor: Option<Cursor>, limit: Option<u16>) -> AnchorEntries {
    let limit = limit_or_default(limit);

    with_anchor_index_mut(|index| {
        let iterator = match cursor {
            None => index.range(anchor.to_le_bytes().to_vec(), None),
            Some(Cursor::NextToken { next_token }) => {
                let index_key = AnchorIndexKey::from_bytes(next_token.into_vec());
                assert_eq!(
                    anchor, index_key.anchor,
                    "anchor does not match the next_token"
                );
                index.range(
                    anchor.to_le_bytes().to_vec(),
                    Some(index_key.to_anchor_offset()),
                )
            }
            Some(Cursor::Timestamp { timestamp }) => index.range(
                anchor.to_le_bytes().to_vec(),
                Some(timestamp.to_le_bytes().to_vec()),
            ),
        };

        with_log(|log| {
            let mut entries: Vec<(AnchorIndexKey, Vec<u8>)> = iterator
                .take(limit + 1) // take one too many to extract the cursor
                .map(|(anchor_key, _)| {
                    let entry = log
                        .get(anchor_key.log_index as usize)
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
                .map(|(_, entry)| candid::decode_one(&entry).expect("failed to decode log entry"))
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

#[init]
#[post_upgrade]
fn initialize(arg: ArchiveInit) {
    write_config(ArchiveConfig {
        ii_canister: arg.ii_canister,
        max_entries_per_call: arg.max_entries_per_call,
        last_upgrade_timestamp: time(),
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
                        streaming_strategy: None,
                    }
                }
                Err(err) => HttpResponse {
                    status_code: 500,
                    headers: vec![],
                    body: Cow::Owned(ByteBuf::from(format!("Failed to encode metrics: {}", err))),
                    streaming_strategy: None,
                },
            }
        }
        path => HttpResponse {
            status_code: 404,
            headers: vec![],
            body: Cow::Owned(ByteBuf::from(format!("Asset {} not found.", path))),
            streaming_strategy: None,
        },
    }
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    with_config(|config| {
        w.encode_gauge(
            "ii_archive_last_upgrade_timestamp",
            config.last_upgrade_timestamp as f64,
            "Timestamp of the last upgrade of this canister.",
        )
    })?;
    with_log(|log| {
        w.encode_gauge(
            "ii_archive_log_entries_count",
            log.len() as f64,
            "Number of log entries stored in this canister.",
        )?;
        w.encode_gauge(
            "ii_archive_log_entries_size",
            log.log_size_bytes() as f64,
            "Total size of all logged entries in bytes, not counting data structure overhead.",
        )?;
        w.encode_gauge(
            "ii_archive_log_index_memory_size",
            log.index_size_bytes() as f64,
            "Total size of the log index in bytes.",
        )?;
        w.encode_gauge(
            "ii_archive_log_data_memory_size",
            log.data_size_bytes() as f64,
            "Total size of the log data in bytes.",
        )
    })?;
    with_anchor_index_mut(|index| {
        w.encode_gauge(
            "ii_archive_anchor_index_entries_count",
            index.len() as f64,
            "Number of entries in the anchor index.",
        )
    })?;
    MEMORY_MANAGER.with(|cell| {
        let manager = cell.borrow();
        w.encode_gauge(
            "ii_archive_log_index_virtual_memory_size",
            manager.get(LOG_INDEX_MEMORY_ID).size() as f64,
            "Number of stable memory pages allocated to the log index virtual memory.",
        )?;
        w.encode_gauge(
            "ii_archive_log_data_virtual_memory_size",
            manager.get(LOG_DATA_MEMORY_ID).size() as f64,
            "Number of stable memory pages allocated to the log data virtual memory.",
        )?;
        w.encode_gauge(
            "ii_archive_anchor_index_virtual_memory_size",
            manager.get(ANCHOR_ACCESS_INDEX_MEMORY_ID).size() as f64,
            "Number of stable memory pages allocated to the anchor index virtual memory.",
        )
    })?;
    w.encode_gauge(
        "ii_archive_stable_memory_pages",
        stable64_size() as f64,
        "Number of stable memory pages used by this canister.",
    )?;
    Ok(())
}

/// This makes this Candid service self-describing, so that for example Candid UI, but also other
/// tools, can seamlessly integrate with it. The concrete interface (method name etc.) is
/// provisional, but works.
#[query]
fn __get_candid_interface_tmp_hack() -> String {
    include_str!("../archive.did").to_string()
}

fn main() {}
