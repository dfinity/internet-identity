//! This module implements all the stable memory interactions of Internet Identity.
//! It uses the [Reader] and [Writer] implementations of the `stable_structures` crate.
//!
//! ## Stable Memory Layout
//!
//! Variables used below with different values depending on memory layout version:
//!
//! * HEADER_SIZE
//!     * v1: 58 bytes
//!     * v2: 84 bytes
//!     * v3: 66 bytes
//! * ENTRY_OFFSET
//!     * v1: 512 bytes
//!     * v2: n/a (v2 denotes the state of v1 and v3 anchor records existing in parallel)
//!     * v3: 131 072 bytes = 2 WASM Pages
//! * Anchor size
//!     * v1: 2048 bytes
//!     * v2: n/a (v2 denotes the state of v1 and v3 anchor records existing in parallel)
//!     * v3: 4096 bytes
//!
//! ```text
//! ------------------------------------------- <- Address 0
//! Magic "IIC"                 ↕ 3 bytes
//! -------------------------------------------
//! Layout version              ↕ 1 byte
//! -------------------------------------------
//! Number of anchors           ↕ 4 bytes
//! -------------------------------------------
//! anchor_range_lower (A_0)    ↕ 8 bytes
//! -------------------------------------------
//! anchor_range_upper (A_MAX)  ↕ 8 bytes
//! -------------------------------------------
//! max_entry_size (SIZE_MAX)   ↕ 2 bytes
//! -------------------------------------------
//! Salt                        ↕ 32 bytes
//! -------------------------------------------
//! Entry offset (ENTRY_OFFSET) ↕ 8 bytes
//! ------------------------------------------- <- HEADER_SIZE
//! Reserved space              ↕ (RESERVED_HEADER_BYTES - HEADER_SIZE) bytes
//! ------------------------------------------- <- ENTRY_OFFSET
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
//! Unused space A_MAX          ↕ (SIZE_MAX - A_MAX_size - 2) bytes
//! -------------------------------------------
//! Unallocated space           ↕ STABLE_MEMORY_RESERVE bytes
//! -------------------------------------------
//! ```
//! ### Stable Memory Migration
//!
//! This release supports a stable memory migration which will shift all anchors to higher memory
//! addresses.
//! For any individual anchor the following changes are made:
//! * entry size increased from 2kiB to 4kiB
//!
//! During the migration, the header version will be set to 2, when finished it will be set to 3.
//! The anchors are migrated in configurable batches after any write operation.
//!
//! After this migration is complete, the following additional changes need to be made:
//! * change the individual anchor candid types from `Vec<DeviceDataInternal>` to [Anchor]
//! * clean up no longer needed infrastructure to handle versions < 3
//! * start using virtual memory managed by the memory manager (stable-structures crate)
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

use crate::state::{Anchor, DeviceDataInternal, PersistentState};
use candid;
use ic_cdk::api::trap;
use ic_stable_structures::reader::{BufferedReader, OutOfBounds, Reader};
use ic_stable_structures::writer::{BufferedWriter, Writer};
use ic_stable_structures::Memory;
use internet_identity_interface::{MigrationState, UserNumber};
use std::convert::TryInto;
use std::fmt;
use std::io::{Read, Write};
use std::ops::RangeInclusive;

#[cfg(test)]
mod tests;

// version 0: invalid
// version 1: genesis layout, might have persistent state
// version 2: migration in progress genesis -> 4KB anchors layout, must have persistent state
// version 3: 4KB anchors layout
// version 4+: invalid
const SUPPORTED_LAYOUT_VERSIONS: RangeInclusive<u8> = 1..=3;

const WASM_PAGE_SIZE: u64 = 65_536;

/// Reserved space for the header before the anchor records start.
const ENTRY_OFFSET_V1: u64 = 512;
const ENTRY_OFFSET_V3: u64 = 2 * WASM_PAGE_SIZE; // 1 page reserved for II config, 1 for memory manager

const DEFAULT_ENTRY_SIZE_V1: u16 = 2048;
const DEFAULT_ENTRY_SIZE_V3: u16 = 4096;
const EMPTY_SALT: [u8; 32] = [0; 32];
const GB: u64 = 1 << 30;

/// In practice, II has 32 GB of stable memory available. But we want to keep the default
/// user range until the stable memory migration is complete. Thus we keep this value for anchor
/// range checking for the time being.
const LEGACY_STABLE_MEMORY_SIZE: u64 = 8 * GB;
/// We reserve last ~10% of the (legacy) stable memory for later new features.
const STABLE_MEMORY_RESERVE: u64 = LEGACY_STABLE_MEMORY_SIZE / 10;

const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS"; // II Persistent State

/// The maximum number of users this canister can store.
pub const DEFAULT_RANGE_SIZE_V1: u64 =
    (LEGACY_STABLE_MEMORY_SIZE - ENTRY_OFFSET_V1 - STABLE_MEMORY_RESERVE)
        / DEFAULT_ENTRY_SIZE_V1 as u64;

pub type Salt = [u8; 32];

/// Data type responsible for managing user data in stable memory.
pub struct Storage<M> {
    header: Header,
    memory: M,
}

#[repr(packed)]
struct Header {
    magic: [u8; 3],
    // version 0: invalid
    // version 1: genesis layout, might have persistent state
    // version 2: migration in progress genesis -> 4KB anchors layout, must have persistent state
    // version 3: 4KB anchors layout
    // version 4+: invalid
    version: u8,
    num_users: u32,
    id_range_lo: u64,
    id_range_hi: u64,
    entry_size: u16,
    salt: [u8; 32],
    first_entry_offset: u64,     // offset of first entry
    first_entry_offset_new: u64, // post-migration offset of first entry
    entry_size_new: u16,         // post-migration entry size
    new_layout_start: u32,       // all records < this value are still using the old anchor size
    migration_batch_size: u32,   // batch size for incremental anchor migration
}

/// Small struct to keep record information required to read and write anchors separated from the
/// layout information.
#[derive(Debug, Eq, PartialEq)]
struct RecordMeta {
    offset: u64,
    entry_size: u16,
}

impl RecordMeta {
    pub fn new(record_number: u32, entry_size: u16, entry_offset: u64) -> Self {
        RecordMeta {
            offset: entry_offset + record_number as u64 * entry_size as u64,
            entry_size,
        }
    }

    /// The anchor space is divided into two parts:
    /// * 2 bytes of candid length (u16 little endian)
    /// * length bytes of encoded candid
    ///
    /// This function returns the length limit of the candid part.
    pub fn candid_size_limit(&self) -> usize {
        self.entry_size as usize - std::mem::size_of::<u16>()
    }
}

impl<M: Memory> Storage<M> {
    /// Creates a new empty storage that manages the data of users in
    /// the specified range.
    pub fn new((id_range_lo, id_range_hi): (UserNumber, UserNumber), memory: M) -> Self {
        if id_range_hi < id_range_lo {
            trap(&format!(
                "improper Identity Anchor range: [{}, {})",
                id_range_lo, id_range_hi,
            ));
        }

        if (id_range_hi - id_range_lo) > DEFAULT_RANGE_SIZE_V1 {
            trap(&format!(
                "id range [{}, {}) is too large for a single canister (max {} entries)",
                id_range_lo, id_range_hi, DEFAULT_RANGE_SIZE_V1,
            ));
        }

        Self {
            header: Header {
                magic: *b"IIC",
                version: 3,
                num_users: 0,
                id_range_lo,
                id_range_hi,
                entry_size: DEFAULT_ENTRY_SIZE_V3,
                salt: EMPTY_SALT,
                first_entry_offset: ENTRY_OFFSET_V3,

                // the following fields are no longer relevant in the post-migration state:
                first_entry_offset_new: 0,
                entry_size_new: 0,
                new_layout_start: 0,
                migration_batch_size: 0,
            },
            memory,
        }
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
        if !SUPPORTED_LAYOUT_VERSIONS.contains(&header.version) {
            trap(&format!("unsupported header version: {}", header.version));
        }

        // header version 1 did not have this field
        if header.version == 1 {
            header.first_entry_offset = ENTRY_OFFSET_V1;
        }

        Some(Self { header, memory })
    }

    /// Allocates a fresh Identity Anchor.
    ///
    /// Returns None if the range of Identity Anchor assigned to this
    /// storage is exhausted.
    pub fn allocate_user_number(&mut self) -> Option<UserNumber> {
        let user_number = self.header.id_range_lo + self.header.num_users as u64;
        if user_number >= self.header.id_range_hi {
            return None;
        }
        self.header.num_users += 1;
        self.flush();
        Some(user_number)
    }

    /// Writes the data of the specified user to stable memory.
    pub fn write(&mut self, user_number: UserNumber, data: Anchor) -> Result<(), StorageError> {
        let record_number = self.user_number_to_record(user_number)?;
        let record_meta = self.record_meta(record_number);
        let buf = candid::encode_one(data.devices).map_err(StorageError::SerializationError)?;
        self.write_entry_bytes(&record_meta, buf)?;
        self.migrate_record_batch()?;
        Ok(())
    }

    fn write_entry_bytes(
        &mut self,
        record_meta: &RecordMeta,
        buf: Vec<u8>,
    ) -> Result<(), StorageError> {
        if buf.len() > record_meta.candid_size_limit() {
            return Err(StorageError::EntrySizeLimitExceeded(buf.len()));
        }

        // use buffered writer to minimize expensive stable memory operations
        let mut writer = BufferedWriter::new(
            record_meta.entry_size as usize,
            Writer::new(&mut self.memory, record_meta.offset),
        );
        writer
            .write(&(buf.len() as u16).to_le_bytes())
            .expect("memory write failed");
        writer.write(&buf).expect("memory write failed");
        writer.flush().expect("memory write failed");
        Ok(())
    }

    /// Reads the data of the specified user from stable memory.
    pub fn read(&self, user_number: UserNumber) -> Result<Anchor, StorageError> {
        let record_number = self.user_number_to_record(user_number)?;
        let record_meta = self.record_meta(record_number);
        let data_buf = self.read_entry_bytes(&record_meta);
        let devices: Vec<DeviceDataInternal> =
            candid::decode_one(&data_buf).map_err(StorageError::DeserializationError)?;
        Ok(Anchor { devices })
    }

    fn read_entry_bytes(&self, record_meta: &RecordMeta) -> Vec<u8> {
        // the reader will check stable memory bounds
        // use buffered reader to minimize expensive stable memory operations
        let mut reader = BufferedReader::new(
            record_meta.entry_size as usize,
            Reader::new(&self.memory, record_meta.offset),
        );

        let mut len_buf = vec![0; 2];
        reader
            .read(&mut len_buf.as_mut_slice())
            .expect("failed to read memory");
        let len = u16::from_le_bytes(len_buf.try_into().unwrap()) as usize;

        // This error most likely indicates stable memory corruption.
        if len > record_meta.candid_size_limit() {
            trap(&format!(
                "persisted value size {} exceeds maximum size {}",
                len,
                record_meta.candid_size_limit()
            ))
        }

        let mut data_buf = vec![0; len];
        reader
            .read(&mut data_buf.as_mut_slice())
            .expect("failed to read memory");
        data_buf
    }

    /// Make sure all the required metadata is recorded to stable memory.
    pub fn flush(&mut self) {
        let slice = unsafe {
            std::slice::from_raw_parts(
                &self.header as *const _ as *const u8,
                std::mem::size_of::<Header>(),
            )
        };
        let mut writer = Writer::new(&mut self.memory, 0);

        // this should never fail as this write only requires a memory of size 1
        writer.write(slice).expect("bug: failed to grow memory");
    }

    pub fn user_count(&self) -> usize {
        self.header.num_users as usize
    }

    /// Returns the maximum number of entries that this storage can fit.
    pub fn max_entries(&self) -> usize {
        // Always return layout v1 max entries even when migration is completed.
        // This will be adapted in the subsequent clean-up after successful migration.
        ((LEGACY_STABLE_MEMORY_SIZE - ENTRY_OFFSET_V1 - STABLE_MEMORY_RESERVE)
            / DEFAULT_ENTRY_SIZE_V1 as u64) as usize
    }

    pub fn assigned_user_number_range(&self) -> (UserNumber, UserNumber) {
        (self.header.id_range_lo, self.header.id_range_hi)
    }

    pub fn set_user_number_range(&mut self, (lo, hi): (UserNumber, UserNumber)) {
        if hi < lo {
            trap(&format!(
                "set_user_number_range: improper Identity Anchor range [{}, {})",
                lo, hi
            ));
        }
        let max_entries = self.max_entries() as u64;
        if (hi - lo) > max_entries {
            trap(&format!(
                "set_user_number_range: specified range [{}, {}) is too large for this canister \
                 (max {} entries)",
                lo, hi, max_entries
            ));
        }
        self.header.id_range_lo = lo;
        self.header.id_range_hi = hi;
        self.flush();
    }

    fn user_number_to_record(&self, user_number: u64) -> Result<u32, StorageError> {
        if user_number < self.header.id_range_lo || user_number >= self.header.id_range_hi {
            return Err(StorageError::UserNumberOutOfRange {
                user_number,
                range: self.assigned_user_number_range(),
            });
        }

        let record_number = (user_number - self.header.id_range_lo) as u32;
        if record_number >= self.header.num_users {
            return Err(StorageError::BadUserNumber(user_number));
        }
        Ok(record_number)
    }

    fn record_meta(&self, record_number: u32) -> RecordMeta {
        if self.header.version == 1 || self.header.version == 3 {
            return RecordMeta::new(
                record_number,
                self.header.entry_size,
                self.header.first_entry_offset,
            );
        }
        assert_eq!(self.header.version, 2);

        // we only have to check the layout pointer if we are in the middle of a migration
        if record_number < self.header.new_layout_start {
            RecordMeta::new(
                record_number,
                self.header.entry_size,
                self.header.first_entry_offset,
            )
        } else {
            RecordMeta::new(
                record_number,
                self.header.entry_size_new,
                self.header.first_entry_offset_new,
            )
        }
    }

    /// Returns the address of the first byte not yet allocated to a user.
    /// This address exists even if the max user number has been reached, because there is a memory
    /// reserve at the end of stable memory.
    fn unused_memory_start(&self) -> u64 {
        self.record_meta(self.header.num_users).offset
    }

    /// Writes the persistent state to stable memory just outside of the space allocated to the highest user number.
    /// This is only used to _temporarily_ save state during upgrades. It will be overwritten on next user registration.
    pub fn write_persistent_state(&mut self, state: &PersistentState) {
        let address = self.unused_memory_start();

        // In practice, candid encoding is infallible. The Result is an artifact of the serde API.
        let encoded_state = candid::encode_one(state).unwrap();

        // In practice, for all reasonably sized persistent states (<800MB) the writes are
        // infallible because we have a stable memory reserve (i.e. growing the memory will succeed).
        let mut writer = Writer::new(&mut self.memory, address);
        writer.write(&PERSISTENT_STATE_MAGIC).unwrap();
        writer
            .write(&(encoded_state.len() as u64).to_le_bytes())
            .unwrap();
        writer.write(&encoded_state).unwrap();
    }

    /// Reads the persistent state from stable memory just outside of the space allocated to the highest user number.
    /// This is only used to restore state in `post_upgrade`.
    pub fn read_persistent_state(&self) -> Result<PersistentState, PersistentStateError> {
        const WASM_PAGE_SIZE: u64 = 65536;
        let address = self.unused_memory_start();

        if address > self.memory.size() * WASM_PAGE_SIZE {
            // the address where the persistent state would be is not allocated yet
            return Err(PersistentStateError::NotFound);
        }

        let mut reader = Reader::new(&self.memory, address);
        let mut magic_buf: [u8; 4] = [0; 4];
        let bytes_read = reader
            .read(&mut magic_buf)
            // if we hit out of bounds here, this means that the persistent state has not been
            // written at the expected location and thus cannot be found
            .map_err(|_| PersistentStateError::NotFound)?;

        if bytes_read != 4 || magic_buf != PERSISTENT_STATE_MAGIC {
            // less than the expected number of bytes were read or the magic does not match
            // --> this is not the persistent state
            return Err(PersistentStateError::NotFound);
        }

        let mut size_buf: [u8; 8] = [0; 8];
        let bytes_read = reader
            .read(&mut size_buf)
            .map_err(|err| PersistentStateError::ReadError(err))? as u64;

        // check if we actually read the required amount of data
        // note: this will only happen if we hit the memory bounds during read
        if bytes_read != 8 {
            let max_address = address + 4 + bytes_read;
            return Err(PersistentStateError::ReadError(OutOfBounds {
                max_address,
                attempted_read_address: max_address + 1,
            }));
        }

        let size = u64::from_le_bytes(size_buf);
        let mut data_buf = Vec::new();
        data_buf.resize(size as usize, 0);
        let bytes_read = reader
            .read(data_buf.as_mut_slice())
            .map_err(|err| PersistentStateError::ReadError(err))? as u64;

        // check if we actually read the required amount of data
        // note: this will only happen if we hit the memory bounds during read
        if bytes_read != size {
            let max_address = address + 4 + 8 + bytes_read;
            return Err(PersistentStateError::ReadError(OutOfBounds {
                max_address,
                attempted_read_address: max_address + 1,
            }));
        }

        candid::decode_one(&data_buf).map_err(|err| PersistentStateError::CandidError(err))
    }

    pub fn version(&self) -> u8 {
        self.header.version
    }

    pub fn configure_migration(&mut self, batch_size: u32) {
        if self.header.version == 3 {
            // nothing to do, we're done
            return;
        }

        if batch_size > 0 && self.header.version == 1 {
            // initialize header for migration
            self.header.version = 2;
            self.header.first_entry_offset_new = ENTRY_OFFSET_V3;
            self.header.entry_size_new = DEFAULT_ENTRY_SIZE_V3;
            // the next user will start using the new layout
            self.header.new_layout_start = self.header.num_users;
        }

        self.header.migration_batch_size = batch_size;
        self.flush();
    }

    fn migrate_record_batch(&mut self) -> Result<(), StorageError> {
        if self.header.version == 1 || self.header.version == 3 {
            // migration not started or already finished --> nothing to do
            return Ok(());
        }
        assert_eq!(self.header.version, 2);
        assert!(self.header.new_layout_start > 0);

        for _ in 0..self.header.migration_batch_size {
            let record = self.header.new_layout_start - 1;
            self.migrate_record(record)?;

            if self.header.new_layout_start == 0 {
                self.finalize_migration();
                return Ok(());
            }
        }

        // write the modified migration state back to stable memory
        self.flush();
        Ok(())
    }

    fn migrate_record(&mut self, record: u32) -> Result<(), StorageError> {
        let pre_migration_meta = self.record_meta(record);
        let entry_data = self.read_entry_bytes(&pre_migration_meta);

        // modifying this pointer will make record_meta switch to the new layout.
        self.header.new_layout_start -= 1;

        assert_eq!(
            record,
            { self.header.new_layout_start },
            "the migrated record must be the lowest record using the new layout"
        );

        let post_migration_meta = self.record_meta(record);

        assert_ne!(
            pre_migration_meta, post_migration_meta,
            "anchor record meta information must change during migration"
        );

        self.write_entry_bytes(&post_migration_meta, entry_data)
    }

    fn finalize_migration(&mut self) {
        self.header.version = 3;
        self.header.first_entry_offset = self.header.first_entry_offset_new;
        self.header.entry_size = self.header.entry_size_new;

        // reset now obsolete fields
        self.header.migration_batch_size = 0;
        self.header.new_layout_start = 0;
        self.header.first_entry_offset_new = 0;
        self.header.entry_size_new = 0;
        self.flush();
    }

    /// External view into the migration progress. Only used for the `stats` query call.
    pub fn migration_state(&self) -> MigrationState {
        match self.header.version {
            1 => MigrationState::NotStarted,
            2 => MigrationState::Started {
                anchors_left: self.header.new_layout_start as u64,
                batch_size: self.header.migration_batch_size as u64,
            },
            3 => MigrationState::Finished,
            _ => trap("unsupported header version"),
        }
    }
}

#[derive(Debug)]
pub enum PersistentStateError {
    CandidError(candid::error::Error),
    NotFound,
    ReadError(OutOfBounds),
}

#[derive(Debug)]
pub enum StorageError {
    UserNumberOutOfRange {
        user_number: UserNumber,
        range: (UserNumber, UserNumber),
    },
    BadUserNumber(u64),
    DeserializationError(candid::error::Error),
    SerializationError(candid::error::Error),
    EntrySizeLimitExceeded(usize),
}

impl fmt::Display for StorageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UserNumberOutOfRange { user_number, range } => write!(
                f,
                "Identity Anchor {} is out of range [{}, {})",
                user_number, range.0, range.1
            ),
            Self::BadUserNumber(n) => write!(f, "bad Identity Anchor {}", n),
            Self::DeserializationError(err) => {
                write!(f, "failed to deserialize a Candid value: {}", err)
            }
            Self::SerializationError(err) => {
                write!(f, "failed to serialize a Candid value: {}", err)
            }
            Self::EntrySizeLimitExceeded(n) => write!(
                f,
                "attempted to store an entry of size {} \
                 which is larger then the max allowed entry size",
                n
            ),
        }
    }
}
