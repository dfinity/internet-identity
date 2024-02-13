//! This module implements all the stable memory interactions of Internet Identity.
//! It uses the [Reader] and [Writer] implementations of the `stable_structures` crate.
//!
//! ## Stable Memory Layout
//!
//! Variables used below:
//! * HEADER_SIZE: 66 bytes
//! * ENTRY_OFFSET: 131 072 bytes = 2 WASM Pages
//! * Anchor size: 4096 bytes
//!
//! Within the first page of the raw stable memory, the layout is as follows:
//!
//! ```text
//! ------------------------------------------- <- Address 0
//! Magic "IIC"                 ↕ 3 bytes
//! -------------------------------------------
//! Layout version              ↕ 1 byte
//! -------------------------------------------
//! Number of anchors           ↕ 4 bytes
//! -------------------------------------------
//! id_range_lo (A_0)           ↕ 8 bytes
//! -------------------------------------------
//! id_range_hi (A_MAX)         ↕ 8 bytes
//! -------------------------------------------
//! entry_size (SIZE_MAX)       ↕ 2 bytes
//! -------------------------------------------
//! Salt                        ↕ 32 bytes
//! -------------------------------------------
//! Entry offset (ENTRY_OFFSET) ↕ 8 bytes
//! ------------------------------------------- <- HEADER_SIZE
//! Unused space                ↕
//! ------------------------------------------- <- Start of wasm memory page 1
//! ```
//!
//! The second page and onwards is managed by the [MemoryManager] and is currently split into two
//! managed memories:
//! * Anchor memory: used to store the candid encoded anchors
//! * Archive buffer memory: used to store the archive entries yet to be pulled by the archive canister
//!
//! ### Anchor memory
//!
//! The layout within the (virtual) anchor memory is as follows:
//!
//! ```text
//! ------------------------------------------- <- Address 0
//! A_0_size                    ↕ 2 bytes
//! -------------------------------------------
//! Candid encoded entry        ↕ A_0_size bytes
//! -------------------------------------------
//! Unused space A_0            ↕ (SIZE_MAX - A_0_size - 2) bytes
//! ------------------------------------------- <- A_1_offset = ENTRY_OFFSET + (A_1 - A_0) * SIZE_MAX  ┬
//! A_1_size                    ↕ 2 bytes                                                              │
//! -------------------------------------------                                                        │
//! Candid encoded entry        ↕ A_1_size bytes                                            anchor A_1 │
//! -------------------------------------------                                                        │
//! Unused space A_1            ↕ (SIZE_MAX - A_1_size - 2) bytes                                      │
//! -------------------------------------------                                                        ┴
//! ...
//! ------------------------------------------- <- A_MAX_offset = ENTRY_OFFSET + (A_MAX - A_0) * SIZE_MAX
//! A_MAX_size                  ↕ 2 bytes
//! -------------------------------------------
//! Candid encoded entry        ↕ A_MAX_size bytes
//! -------------------------------------------
//! Unallocated space
//! -------------------------------------------
//! ```
//!
//! ## Persistent State
//!
//! In order to keep state across upgrades that is not related to specific anchors (such as archive
//! information) Internet Identity will serialize the [PersistentState] into the first unused memory
//! location (after the anchor record of the highest allocated anchor number). The [PersistentState]
//! will be read in `post_upgrade` after which the data can be safely overwritten by the next anchor
//! to be registered.
//!
//! The [PersistentState] is serialized at the end of stable memory to allow for variable sized data
//! without the risk of running out of space (which might easily happen if the RESERVED_HEADER_BYTES
//! were used instead).
//!
//! ### Archive buffer memory
//!
//! The archive buffer memory is entirely owned by a [StableBTreeMap] used to store the buffered
//! entries. The entries are indexed by their sequence number.
//!
//! The archive buffer memory is managed by the [MemoryManager] and is currently limited to a single
//! bucket of 128 pages.
use candid::{CandidType, Deserialize};
use ic_cdk::api::stable::WASM_PAGE_SIZE_IN_BYTES;
use std::borrow::Cow;
use std::fmt;
use std::io::{Read, Write};
use std::ops::RangeInclusive;

use ic_cdk::api::trap;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::reader::{BufferedReader, Reader};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::writer::{BufferedWriter, Writer};
use ic_stable_structures::{Memory, RestrictedMemory, StableBTreeMap, Storable};
use internet_identity_interface::archive::types::BufferedEntry;

use internet_identity_interface::internet_identity::types::*;

use crate::state::PersistentState;
use crate::storage::anchor::Anchor;
use crate::storage::storable_anchor::StorableAnchor;

pub mod anchor;

/// module for the internal serialization format of anchors
mod storable_anchor;
#[cfg(test)]
mod tests;

/// * version   0: invalid
/// * version 1-6: no longer supported
/// * version   7: 4KB anchors, candid anchor record layout, persistent state with archive pull config,
///                with memory manager (from 2nd page on)
/// * version   8: same as 7, but archive entries buffer in stable memory
const SUPPORTED_LAYOUT_VERSIONS: RangeInclusive<u8> = 7..=8;

/// Reserved space for the header before the anchor records start.
const ENTRY_OFFSET: u64 = 2 * WASM_PAGE_SIZE_IN_BYTES as u64; // 1 page reserved for II config, 1 for memory manager
const DEFAULT_ENTRY_SIZE: u16 = 4096;
const EMPTY_SALT: [u8; 32] = [0; 32];
const GB: u64 = 1 << 30;

const MAX_STABLE_MEMORY_SIZE: u64 = 32 * GB;
const MAX_WASM_PAGES: u64 = MAX_STABLE_MEMORY_SIZE / WASM_PAGE_SIZE_IN_BYTES as u64;

/// In practice, II has 48 GB of stable memory available.
/// This limit has last been raised when it was still 32 GB.
const STABLE_MEMORY_SIZE: u64 = 32 * GB;

const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS"; // II Persistent State

/// MemoryManager parameters.
const ANCHOR_MEMORY_INDEX: u8 = 0u8;
const ARCHIVE_BUFFER_MEMORY_INDEX: u8 = 1u8;
const ANCHOR_MEMORY_ID: MemoryId = MemoryId::new(ANCHOR_MEMORY_INDEX);
const ARCHIVE_BUFFER_MEMORY_ID: MemoryId = MemoryId::new(ARCHIVE_BUFFER_MEMORY_INDEX);
// The bucket size 128 is relatively low, to avoid wasting memory when using
// multiple virtual memories for smaller amounts of data.
// This value results in 256 GB of total managed memory, which should be enough
// for the foreseeable future.
const BUCKET_SIZE_IN_PAGES: u16 = 128;

/// The maximum number of anchors this canister can store.
pub const DEFAULT_RANGE_SIZE: u64 = (STABLE_MEMORY_SIZE - ENTRY_OFFSET) / DEFAULT_ENTRY_SIZE as u64;

pub type Salt = [u8; 32];

type ArchiveBufferMemory<M> = RestrictedMemory<VirtualMemory<RestrictedMemory<M>>>;

/// The [BufferedEntry] is wrapped to allow this crate to implement [Storable].
#[derive(Clone, Debug, CandidType, Deserialize)]
struct BufferedEntryWrapper(BufferedEntry);

impl Storable for BufferedEntryWrapper {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(candid::encode_one(&self.0).expect("failed to serialize archive entry"))
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        BufferedEntryWrapper(
            candid::decode_one(&bytes).expect("failed to deserialize archive entry"),
        )
    }

    const BOUND: Bound = Bound::Unbounded;
}

/// Data type responsible for managing anchor data in stable memory.
pub struct Storage<M: Memory> {
    header: Header,
    header_memory: RestrictedMemory<M>,
    anchor_memory: VirtualMemory<RestrictedMemory<M>>,
    archive_entries_buffer: StableBTreeMap<u64, BufferedEntryWrapper, ArchiveBufferMemory<M>>,
}

#[repr(packed)]
#[derive(Copy, Clone, Debug, PartialEq)]
struct Header {
    magic: [u8; 3],
    /// See [SUPPORTED_LAYOUT_VERSIONS]
    version: u8,
    num_anchors: u32,
    id_range_lo: u64,
    id_range_hi: u64,
    entry_size: u16,
    salt: [u8; 32],
    first_entry_offset: u64,
}

impl<M: Memory + Clone> Storage<M> {
    /// Creates a new empty storage that manages the data of anchors in
    /// the specified range.
    pub fn new((id_range_lo, id_range_hi): (AnchorNumber, AnchorNumber), memory: M) -> Self {
        if id_range_hi < id_range_lo {
            trap(&format!(
                "improper Identity Anchor range: [{id_range_lo}, {id_range_hi})",
            ));
        }

        if (id_range_hi - id_range_lo) > DEFAULT_RANGE_SIZE {
            trap(&format!(
                "id range [{id_range_lo}, {id_range_hi}) is too large for a single canister (max {DEFAULT_RANGE_SIZE} entries)",
            ));
        }
        let header_memory = RestrictedMemory::new(memory.clone(), 0..1);
        let memory_manager = MemoryManager::init_with_bucket_size(
            RestrictedMemory::new(memory, 1..MAX_WASM_PAGES),
            BUCKET_SIZE_IN_PAGES,
        );
        let anchor_memory = memory_manager.get(ANCHOR_MEMORY_ID);
        let version: u8 = 8;

        let mut storage = Self {
            header: Header {
                magic: *b"IIC",
                version,
                num_anchors: 0,
                id_range_lo,
                id_range_hi,
                entry_size: DEFAULT_ENTRY_SIZE,
                salt: EMPTY_SALT,
                first_entry_offset: ENTRY_OFFSET,
            },
            header_memory,
            anchor_memory,
            archive_entries_buffer: Self::init_archive_entries_buffer(&memory_manager),
        };
        storage.flush();
        storage
    }

    pub fn salt(&self) -> Option<&Salt> {
        if self.header.salt == EMPTY_SALT {
            None
        } else {
            Some(&self.header.salt)
        }
    }

    pub fn update_salt(&mut self, salt: Salt) {
        if self.salt().is_some() {
            trap("Attempted to set the salt twice.");
        }
        self.header.salt = salt;
        self.flush();
    }

    /// Initializes storage by reading the given memory.
    ///
    /// Returns None if the memory is empty.
    ///
    /// Panics if the memory is not empty but cannot be
    /// decoded.
    pub fn from_memory(memory: M) -> Option<Self> {
        if memory.size() < 1 {
            return None;
        }

        let mut header: Header = unsafe { std::mem::zeroed() };

        unsafe {
            let slice = std::slice::from_raw_parts_mut(
                &mut header as *mut _ as *mut u8,
                std::mem::size_of::<Header>(),
            );
            memory.read(0, slice);
        }

        if &header.magic != b"IIC" {
            trap(&format!(
                "stable memory header: invalid magic: {:?}",
                &header.magic,
            ));
        }
        if &header.version < SUPPORTED_LAYOUT_VERSIONS.start() {
            trap(&format!(
                "stable memory layout version {} is no longer supported:\n\
            Either reinstall (wiping stable memory) or migrate using a previous II version\n\
            See https://github.com/dfinity/internet-identity#stable-memory-compatibility for more information.",
                header.version
            ));
        }
        if !SUPPORTED_LAYOUT_VERSIONS.contains(&header.version) {
            trap(&format!("unsupported header version: {}", header.version));
        }

        match header.version {
            7 | 8 => {
                let header_memory = RestrictedMemory::new(memory.clone(), 0..1);
                let managed_memory = RestrictedMemory::new(memory, 1..MAX_WASM_PAGES);
                let memory_manager =
                    MemoryManager::init_with_bucket_size(managed_memory, BUCKET_SIZE_IN_PAGES);
                let anchor_memory = memory_manager.get(ANCHOR_MEMORY_ID);
                Some(Self {
                    header,
                    header_memory,
                    anchor_memory,
                    archive_entries_buffer: Self::init_archive_entries_buffer(&memory_manager),
                })
            }
            _ => trap(&format!("unsupported header version: {}", header.version)),
        }
    }

    /// Allocates a fresh Identity Anchor.
    ///
    /// Returns None if the range of Identity Anchor assigned to this
    /// storage is exhausted.
    pub fn allocate_anchor(&mut self) -> Option<Anchor> {
        let anchor_number = self.header.id_range_lo + self.header.num_anchors as u64;
        if anchor_number >= self.header.id_range_hi {
            return None;
        }
        self.header.num_anchors += 1;
        self.flush();
        Some(Anchor::new(anchor_number))
    }

    /// Writes the data of the specified anchor to stable memory.
    pub fn write(&mut self, data: Anchor) -> Result<(), StorageError> {
        let anchor_number = data.anchor_number();
        let storable_anchor = StorableAnchor::from(data);
        let buf = storable_anchor.to_bytes();
        if buf.len() > self.header.entry_size as usize {
            return Err(StorageError::EntrySizeLimitExceeded {
                space_required: buf.len() as u64,
                space_available: self.header.entry_size as u64,
            });
        }

        let record_number = self.anchor_number_to_record(anchor_number)?;
        let address = self.record_address(record_number);
        let mut writer = Writer::new(&mut self.anchor_memory, address);

        writer.write_all(&buf).expect("memory write failed");
        writer.flush().expect("memory write failed");
        Ok(())
    }

    /// Reads the data of the specified anchor from stable memory.
    pub fn read(&self, anchor_number: AnchorNumber) -> Result<Anchor, StorageError> {
        let record_number = self.anchor_number_to_record(anchor_number)?;
        let address = self.record_address(record_number);

        let mut reader = Reader::new(&self.anchor_memory, address);
        let mut buf = vec![0; self.header.entry_size as usize];

        reader.read_exact(&mut buf).expect("failed to read memory");
        let storable_anchor = StorableAnchor::from_bytes(Cow::Owned(buf));
        Ok(Anchor::from((anchor_number, storable_anchor)))
    }

    /// Make sure all the required metadata is recorded to stable memory.
    pub fn flush(&mut self) {
        let slice = unsafe {
            std::slice::from_raw_parts(
                &self.header as *const _ as *const u8,
                std::mem::size_of::<Header>(),
            )
        };
        let mut writer = Writer::new(&mut self.header_memory, 0);

        // this should never fail as this write only requires a memory of size 1
        writer.write_all(slice).expect("bug: failed to grow memory");
    }

    pub fn anchor_count(&self) -> usize {
        self.header.num_anchors as usize
    }

    /// Returns the maximum number of entries that this storage can fit.
    pub fn max_entries(&self) -> usize {
        ((STABLE_MEMORY_SIZE - self.header.first_entry_offset) / self.header.entry_size as u64)
            as usize
    }

    pub fn assigned_anchor_number_range(&self) -> (AnchorNumber, AnchorNumber) {
        (self.header.id_range_lo, self.header.id_range_hi)
    }

    pub fn set_anchor_number_range(&mut self, (lo, hi): (AnchorNumber, AnchorNumber)) {
        if hi < lo {
            trap(&format!(
                "set_anchor_number_range: improper Identity Anchor range [{lo}, {hi})"
            ));
        }
        let max_entries = self.max_entries() as u64;
        if (hi - lo) > max_entries {
            trap(&format!(
                "set_anchor_number_range: specified range [{lo}, {hi}) is too large for this canister \
                 (max {max_entries} entries)"
            ));
        }

        // restrict further if II has users to protect existing anchors
        if self.header.num_anchors > 0 {
            if self.header.id_range_lo != lo {
                trap(&format!(
                    "set_anchor_number_range: specified range [{lo}, {hi}) does not start from the same number ({}) \
                     as the existing range thus would make existing anchors invalid"
                    , {self.header.id_range_lo}));
            }
            // Check that all _existing_ anchors fit into the new range. I.e. making the range smaller
            // is ok as long as the range reduction only affects _unused_ anchor number.
            if (hi - lo) < self.header.num_anchors as u64 {
                trap(&format!(
                    "set_anchor_number_range: specified range [{lo}, {hi}) does not accommodate all {} anchors \
                     thus would make existing anchors invalid"
                    , {self.header.num_anchors}));
            }
        }

        self.header.id_range_lo = lo;
        self.header.id_range_hi = hi;
        self.flush();
    }

    /// Add a new archive entry to the buffer.
    pub fn add_archive_entry(&mut self, entry: BufferedEntry) {
        self.archive_entries_buffer
            .insert(entry.sequence_number, BufferedEntryWrapper(entry));
    }

    /// Get the first `max_entries` archive entries from the buffer.
    pub fn get_archive_entries(&mut self, max_entries: u16) -> Vec<BufferedEntry> {
        self.archive_entries_buffer
            .iter()
            .take(max_entries as usize)
            .map(|(_, v)| v.0.clone())
            .collect()
    }

    /// Prune all archive entries with sequence numbers less than or equal to the given sequence number.
    pub fn prune_archive_entries(&mut self, sequence_number: u64) {
        let entries_to_prune = self
            .archive_entries_buffer
            .range(..=sequence_number)
            .map(|(k, _)| k)
            .collect::<Vec<_>>();
        entries_to_prune.iter().for_each(|k| {
            self.archive_entries_buffer.remove(k);
        });
    }

    /// Prune all archive entries with sequence numbers less than or equal to the given sequence number.
    pub fn archive_entries_count(&self) -> usize {
        self.archive_entries_buffer.iter().count()
    }

    fn anchor_number_to_record(&self, anchor_number: u64) -> Result<u32, StorageError> {
        if anchor_number < self.header.id_range_lo || anchor_number >= self.header.id_range_hi {
            return Err(StorageError::AnchorNumberOutOfRange {
                anchor_number,
                range: self.assigned_anchor_number_range(),
            });
        }

        let record_number = (anchor_number - self.header.id_range_lo) as u32;
        if record_number >= self.header.num_anchors {
            return Err(StorageError::BadAnchorNumber(anchor_number));
        }
        Ok(record_number)
    }

    fn record_address(&self, record_number: u32) -> u64 {
        record_number as u64 * self.header.entry_size as u64
    }

    fn init_archive_entries_buffer<T: Memory>(
        memory_manager: &MemoryManager<T>,
    ) -> StableBTreeMap<u64, BufferedEntryWrapper, RestrictedMemory<VirtualMemory<T>>> {
        // A single archive entry takes on average 476 bytes of space.
        // To have space for 10_000 entries (accounting for ~10% overhead) we need 82 pages or 5 MB.
        // Since the memory manager allocates memory in buckets of 128 pages, we round up to 128 pages.
        StableBTreeMap::init(RestrictedMemory::new(
            memory_manager.get(ARCHIVE_BUFFER_MEMORY_ID),
            0..BUCKET_SIZE_IN_PAGES as u64,
        ))
    }

    /// Returns the address of the first byte not yet allocated to a anchor.
    /// This address exists even if the max anchor number has been reached, because there is a memory
    /// reserve at the end of stable memory.
    fn unused_memory_start(&self) -> u64 {
        self.record_address(self.header.num_anchors)
    }

    /// Writes the persistent state to stable memory just outside of the space allocated to the highest anchor number.
    /// This is only used to _temporarily_ save state during upgrades. It will be overwritten on next anchor registration.
    pub fn write_persistent_state(&mut self, state: &PersistentState) {
        let address = self.unused_memory_start();

        // In practice, candid encoding is infallible. The Result is an artifact of the serde API.
        let encoded_state = candid::encode_one(state).unwrap();

        // In practice, for all reasonably sized persistent states (<800MB) the writes are
        // infallible because we have a stable memory reserve (i.e. growing the memory will succeed).
        let mut writer = BufferedWriter::new(
            self.header.entry_size as usize,
            Writer::new(&mut self.anchor_memory, address),
        );
        writer.write_all(&PERSISTENT_STATE_MAGIC).unwrap();
        writer
            .write_all(&(encoded_state.len() as u64).to_le_bytes())
            .unwrap();
        writer.write_all(&encoded_state).unwrap();
    }

    /// Reads the persistent state from stable memory just outside of the space allocated to the highest anchor number.
    /// This is only used to restore state in `post_upgrade`.
    pub fn read_persistent_state(&self) -> Result<PersistentState, PersistentStateError> {
        const WASM_PAGE_SIZE: u64 = 65536;
        let address = self.unused_memory_start();
        if address > self.anchor_memory.size() * WASM_PAGE_SIZE {
            // the address where the persistent state would be is not allocated yet
            return Err(PersistentStateError::NotFound);
        }

        let mut reader = BufferedReader::new(
            self.header.entry_size as usize,
            Reader::new(&self.anchor_memory, address),
        );
        let mut magic_buf: [u8; 4] = [0; 4];
        reader
            .read_exact(&mut magic_buf)
            // if we hit out of bounds here, this means that the persistent state has not been
            // written at the expected location and thus cannot be found
            .map_err(|_| PersistentStateError::NotFound)?;

        if magic_buf != PERSISTENT_STATE_MAGIC {
            // magic does not match --> this is not the persistent state
            return Err(PersistentStateError::NotFound);
        }

        let mut size_buf: [u8; 8] = [0; 8];
        reader
            .read_exact(&mut size_buf)
            .map_err(PersistentStateError::ReadError)?;

        let size = u64::from_le_bytes(size_buf);
        let mut data_buf = vec![0; size as usize];
        reader
            .read_exact(data_buf.as_mut_slice())
            .map_err(PersistentStateError::ReadError)?;

        candid::decode_one(&data_buf).map_err(PersistentStateError::CandidError)
    }

    pub fn version(&self) -> u8 {
        self.header.version
    }
}

#[derive(Debug)]
pub enum PersistentStateError {
    CandidError(candid::error::Error),
    NotFound,
    ReadError(std::io::Error),
}

#[derive(Debug)]
pub enum StorageError {
    AnchorNumberOutOfRange {
        anchor_number: AnchorNumber,
        range: (AnchorNumber, AnchorNumber),
    },
    BadAnchorNumber(u64),
    DeserializationError(candid::error::Error),
    SerializationError(candid::error::Error),
    EntrySizeLimitExceeded {
        space_required: u64,
        space_available: u64,
    },
}

impl fmt::Display for StorageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AnchorNumberOutOfRange {
                anchor_number,
                range,
            } => write!(
                f,
                "Identity Anchor {} is out of range [{}, {})",
                anchor_number, range.0, range.1
            ),
            Self::BadAnchorNumber(n) => write!(f, "bad Identity Anchor {n}"),
            Self::DeserializationError(err) => {
                write!(f, "failed to deserialize a Candid value: {err}")
            }
            Self::SerializationError(err) => {
                write!(f, "failed to serialize a Candid value: {err}")
            }
            Self::EntrySizeLimitExceeded {
                space_required,
                space_available,
            } => write!(
                f,
                "attempted to store an entry of size {space_required} \
                 which is larger then the max allowed entry size {space_available}"
            ),
        }
    }
}
