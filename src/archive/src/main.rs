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

type Memory = RestrictedMemory<DefaultMemoryImpl>;
type StableLog = Log<VirtualMemory<Memory>, VirtualMemory<Memory>>;
type ConfigCell = StableCell<ArchiveConfig, Memory>;
type UserIndex = StableBTreeMap<VirtualMemory<Memory>, UserIndexKey, ()>;

const GIB: u64 = 1 << 30;
const WASM_PAGE_SIZE: u64 = 65536;
/// The maximum number of Wasm pages that we allow to use for the stable storage.
const MAX_WASM_PAGES: u64 = 8 * GIB / WASM_PAGE_SIZE;

const LOG_INDEX_MEMORY_ID: MemoryId = MemoryId::new(0);
const LOG_DATA_MEMORY_ID: MemoryId = MemoryId::new(1);
const USER_INDEX_MEMORY_ID: MemoryId = MemoryId::new(2);

/// Length of the user index key. Changing this value requires a stable memory migration.
const USER_INDEX_KEY_LENGTH: usize = 24;

thread_local! {
    /// Static configuration of the archive that init() sets once.
    static CONFIG: RefCell<ConfigCell> = RefCell::new(ConfigCell::init(
        config_memory(),
        ArchiveConfig::default(),
    ).expect("failed to initialize stable cell"));

    /// Static memory manager to manage the memory available for blocks.
    static MEMORY_MANAGER: RefCell<MemoryManager<Memory>> = RefCell::new(MemoryManager::init(log_memory()));

    /// Append-only list of encoded blocks stored in stable memory.
    static LOG: RefCell<StableLog> = with_memory_manager(|memory_manager| {
        RefCell::new(Log::init(memory_manager.get(LOG_INDEX_MEMORY_ID), memory_manager.get(LOG_DATA_MEMORY_ID)).expect("failed to initialize stable log"))
    });

    /// Index to efficiently filter entries by user number.
    static USER_INDEX: RefCell<UserIndex> = with_memory_manager(|memory_manager| {
        RefCell::new(StableBTreeMap::init(memory_manager.get(USER_INDEX_MEMORY_ID), USER_INDEX_KEY_LENGTH as u32, 0))
    });
}

/// Creates a memory region for the configuration stable cell.
fn config_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 0..1)
}

/// Creates a memory region for the append-only block list.
fn log_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 1..MAX_WASM_PAGES)
}

/// A helper function to access the configuration.
fn with_config<R>(f: impl FnOnce(&ArchiveConfig) -> R) -> R {
    CONFIG.with(|cell| f(cell.borrow().get()))
}

/// A helper function to access the memory manager.
fn with_memory_manager<R>(f: impl FnOnce(&MemoryManager<Memory>) -> R) -> R {
    MEMORY_MANAGER.with(|cell| f(&*cell.borrow()))
}

/// A helper function to access the log.
fn with_log<R>(f: impl FnOnce(&StableLog) -> R) -> R {
    LOG.with(|cell| f(&*cell.borrow()))
}

/// A helper function to access the user index.
fn with_user_index<R>(f: impl FnOnce(&mut UserIndex) -> R) -> R {
    USER_INDEX.with(|cell| f(&mut *cell.borrow_mut()))
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

impl Storable for ArchiveConfig {
    fn to_bytes(&self) -> Cow<[u8]> {
        let buf = candid::encode_one(&self).expect("failed to encode log config");
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Vec<u8>) -> Self {
        candid::decode_one::<ArchiveConfig>(&bytes).expect("failed to decode log options")
    }
}

impl Default for ArchiveConfig {
    fn default() -> Self {
        Self {
            ii_canister: Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap(),
            max_entries_per_call: 1000,
            last_upgrade_timestamp: 0,
        }
    }
}

/// Index key for the user index.
#[derive(Debug)]
struct UserIndexKey {
    user_number: UserNumber,
    timestamp: Timestamp,
    log_index: u64,
}

impl Storable for UserIndexKey {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buf = Vec::with_capacity(USER_INDEX_KEY_LENGTH);
        buf.extend(&self.user_number.to_le_bytes());
        buf.extend(&self.timestamp.to_le_bytes());
        buf.extend(&self.log_index.to_le_bytes());
        assert_eq!(buf.len(), USER_INDEX_KEY_LENGTH);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Vec<u8>) -> Self {
        assert_eq!(bytes.len(), USER_INDEX_KEY_LENGTH);
        UserIndexKey {
            user_number: u64::from_le_bytes(
                TryFrom::try_from(&bytes[0..8]).expect("failed to read user_number"),
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
fn write_entry(user_number: UserNumber, timestamp: Timestamp, entry: ByteBuf) {
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

    with_user_index(|index| {
        let key = UserIndexKey {
            user_number,
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
    let num_entries = sanitize_limit(limit);

    with_log(|log| {
        let length = log.len();
        let start_idx = match index {
            None => length.saturating_sub(num_entries),
            Some(idx) => idx as usize,
        };

        let next_idx = if start_idx + num_entries < length {
            Some((start_idx + num_entries) as u64)
        } else {
            None
        };

        let mut entries = Vec::with_capacity(log.len().min(num_entries));
        for idx in start_idx..start_idx + num_entries {
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
fn get_user_entries(user_number: u64, cursor: Option<Cursor>, limit: Option<u16>) -> UserEntries {
    let num_entries = sanitize_limit(limit);

    with_user_index(|index| {
        let iterator = match cursor {
            None => index.range(user_number.to_le_bytes().to_vec(), None),
            Some(Cursor::NextToken { next_token }) => index.range(next_token.into_vec(), None),
            Some(Cursor::Timestamp { timestamp }) => index.range(
                user_number.to_le_bytes().to_vec(),
                Some(timestamp.to_le_bytes().to_vec()),
            ),
        };

        with_log(|log| {
            let mut entries: Vec<(UserIndexKey, Vec<u8>)> = iterator
                .take(num_entries + 1) // take one too many to extract the cursor
                .map(|(user_key, _)| {
                    let entry = log
                        .get(user_key.log_index as usize)
                        .expect("bug: index to non-existing entry");
                    (user_key, entry)
                })
                .collect();

            let cursor = if entries.len() > num_entries {
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

            UserEntries { entries, cursor }
        })
    })
}

fn sanitize_limit(limit: Option<u16>) -> usize {
    with_config(|config| {
        limit
            .map(|l| l.min(config.max_entries_per_call))
            .unwrap_or(config.max_entries_per_call) as usize
    })
}

#[init]
#[post_upgrade]
fn init(maybe_arg: Option<ArchiveInit>) {
    if let Some(arg) = maybe_arg {
        CONFIG.with(|cell| {
            cell.borrow_mut()
                .set(ArchiveConfig {
                    ii_canister: arg.ii_canister,
                    max_entries_per_call: arg.max_entries_per_call,
                    last_upgrade_timestamp: time(),
                })
                .expect("failed to store archive config");
        });
    } else {
        CONFIG.with(|cell| {
            let mut config_cell = cell.borrow_mut();
            let mut config = config_cell.get().clone();
            config.last_upgrade_timestamp = time();
            config_cell
                .set(config)
                .expect("failed to update last_upgrade_timestamp in archive config");
        });
    }
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
            "Total size of all logged entries in bytes.",
        )?;
        w.encode_gauge(
            "ii_archive_log_data_memory_size",
            log.data_size_bytes() as f64,
            "Number of users registered in this canister.",
        )
    })?;
    with_user_index(|index| {
        w.encode_gauge(
            "ii_archive_user_index_entries_count",
            index.len() as f64,
            "Number of entries in the user index.",
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
            "ii_archive_user_index_virtual_memory_size",
            manager.get(USER_INDEX_MEMORY_ID).size() as f64,
            "Number of stable memory pages allocated to the user index virtual memory.",
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
