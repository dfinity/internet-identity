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

use crate::state::{Anchor, Device, PersistentState};
use candid;
use candid::{CandidType, Deserialize};
use ic_cdk::api::trap;
use ic_stable_structures::reader::{BufferedReader, OutOfBounds, Reader};
use ic_stable_structures::writer::{BufferedWriter, Writer};
use ic_stable_structures::Memory;
use internet_identity_interface::{
    CredentialId, DeviceKey, DeviceProtection, KeyType, MigrationState, Purpose, UserNumber,
};
use std::cmp::min;
use std::convert::TryInto;
use std::fmt;
use std::io::{Read, Write};
use std::ops::RangeInclusive;

#[cfg(test)]
mod tests;

// version   0: invalid
// version 1-2: no longer supported
// version   3: 4KB anchors layout (current), vec<device> layout
// version   4: migration from vec<devices> to anchor record in progress
// version   5: candid anchor record layout
// version  6+: invalid
const SUPPORTED_LAYOUT_VERSIONS: RangeInclusive<u8> = 3..=5;

const WASM_PAGE_SIZE: u64 = 65_536;

/// Reserved space for the header before the anchor records start.
const ENTRY_OFFSET: u64 = 2 * WASM_PAGE_SIZE; // 1 page reserved for II config, 1 for memory manager
const DEFAULT_ENTRY_SIZE: u16 = 4096;
const EMPTY_SALT: [u8; 32] = [0; 32];
const GB: u64 = 1 << 30;

/// In practice, II has 32 GB of stable memory available. But we want to keep the default
/// user range until the stable memory migration is complete. Thus we keep this value for anchor
/// range checking for the time being.
const STABLE_MEMORY_SIZE: u64 = 32 * GB;
/// We reserve the last ~800 MB of stable memory for later new features.
const STABLE_MEMORY_RESERVE: u64 = 8 * GB / 10;

const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS"; // II Persistent State

/// The maximum number of users this canister can store.
pub const DEFAULT_RANGE_SIZE: u64 =
    (STABLE_MEMORY_SIZE - ENTRY_OFFSET - STABLE_MEMORY_RESERVE) / DEFAULT_ENTRY_SIZE as u64;

pub type Salt = [u8; 32];

/// Legacy candid schema for devices stored in stable memory.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
struct DeviceDataInternal {
    pubkey: DeviceKey,
    alias: String,
    credential_id: Option<CredentialId>,
    purpose: Option<Purpose>,
    key_type: Option<KeyType>,
    protection: Option<DeviceProtection>,
}

impl From<Device> for DeviceDataInternal {
    fn from(device_data: Device) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            purpose: Some(device_data.purpose),
            key_type: Some(device_data.key_type),
            protection: Some(device_data.protection),
        }
    }
}

impl From<DeviceDataInternal> for Device {
    fn from(device_data: DeviceDataInternal) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose.unwrap_or(Purpose::Authentication),
            key_type: device_data.key_type.unwrap_or(KeyType::Unknown),
            protection: device_data
                .protection
                .unwrap_or(DeviceProtection::Unprotected),
        }
    }
}

/// Data type responsible for managing user data in stable memory.
pub struct Storage<M> {
    header: Header,
    memory: M,
}

#[repr(packed)]
struct Header {
    magic: [u8; 3],
    // version   0: invalid
    // version 1-2: no longer supported
    // version   3: 4KB anchors layout (current), vec<device> layout
    // version   4: migration from vec<devices> to anchor record in progress
    // version   5: candid anchor record layout
    // version  6+: invalid
    version: u8,
    num_users: u32,
    id_range_lo: u64,
    id_range_hi: u64,
    entry_size: u16,
    salt: [u8; 32],
    first_entry_offset: u64,
    new_layout_start: u32, // record number of the first entry using the new candid layout
    migration_batch_size: u32,
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

        if (id_range_hi - id_range_lo) > DEFAULT_RANGE_SIZE {
            trap(&format!(
                "id range [{}, {}) is too large for a single canister (max {} entries)",
                id_range_lo, id_range_hi, DEFAULT_RANGE_SIZE,
            ));
        }

        Self {
            header: Header {
                magic: *b"IIC",
                version: 5,
                num_users: 0,
                id_range_lo,
                id_range_hi,
                entry_size: DEFAULT_ENTRY_SIZE,
                salt: EMPTY_SALT,
                first_entry_offset: ENTRY_OFFSET,
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
        if &header.version < SUPPORTED_LAYOUT_VERSIONS.start() {
            trap(&format!("stable memory layout version {} is no longer supported:\nEither reinstall (wiping stable memory) or migrate using a previous II version", header.version));
        }
        if !SUPPORTED_LAYOUT_VERSIONS.contains(&header.version) {
            trap(&format!("unsupported header version: {}", header.version));
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

        let buf = if self.header.version > 3 && record_number >= self.header.new_layout_start {
            // use the new candid layout
            candid::encode_one(data).map_err(StorageError::SerializationError)?
        } else {
            // use the old candid layout
            candid::encode_one(
                data.devices
                    .into_iter()
                    .map(|d| DeviceDataInternal::from(d))
                    .collect::<Vec<DeviceDataInternal>>(),
            )
            .map_err(StorageError::SerializationError)?
        };
        self.write_entry_bytes(record_number, buf)?;

        // piggy back on update call and migrate a batch of anchors
        self.migrate_record_batch()?;
        Ok(())
    }

    fn write_entry_bytes(&mut self, record_number: u32, buf: Vec<u8>) -> Result<(), StorageError> {
        if buf.len() > self.candid_entry_size_limit() {
            return Err(StorageError::EntrySizeLimitExceeded(buf.len()));
        }

        let address = self.record_address(record_number);
        // use buffered writer to minimize expensive stable memory operations
        let mut writer = BufferedWriter::new(
            self.header.entry_size as usize,
            Writer::new(&mut self.memory, address),
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
        let data_buf = self.read_entry_bytes(record_number);

        let anchor = if self.header.version > 3 && record_number >= self.header.new_layout_start {
            // use the new candid layout
            candid::decode_one(&data_buf).map_err(StorageError::DeserializationError)?
        } else {
            // use the old candid layout
            let devices: Vec<DeviceDataInternal> =
                candid::decode_one(&data_buf).map_err(StorageError::DeserializationError)?;
            Anchor {
                devices: devices.into_iter().map(|d| Device::from(d)).collect(),
            }
        };

        Ok(anchor)
    }

    fn read_entry_bytes(&self, record_number: u32) -> Vec<u8> {
        let address = self.record_address(record_number);
        // the reader will check stable memory bounds
        // use buffered reader to minimize expensive stable memory operations
        let mut reader = BufferedReader::new(
            self.header.entry_size as usize,
            Reader::new(&self.memory, address),
        );

        let mut len_buf = vec![0; 2];
        reader
            .read(&mut len_buf.as_mut_slice())
            .expect("failed to read memory");
        let len = u16::from_le_bytes(len_buf.try_into().unwrap()) as usize;

        // This error most likely indicates stable memory corruption.
        if len > self.candid_entry_size_limit() {
            trap(&format!(
                "persisted value size {} exceeds maximum size {}",
                len,
                self.candid_entry_size_limit()
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
        ((STABLE_MEMORY_SIZE - self.header.first_entry_offset - STABLE_MEMORY_RESERVE)
            / self.header.entry_size as u64) as usize
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

    fn record_address(&self, record_number: u32) -> u64 {
        self.header.first_entry_offset + record_number as u64 * self.header.entry_size as u64
    }

    /// The anchor space is divided into two parts:
    /// * 2 bytes of candid length (u16 little endian)
    /// * length bytes of encoded candid
    ///
    /// This function returns the length limit of the candid part.
    fn candid_entry_size_limit(&self) -> usize {
        self.header.entry_size as usize - std::mem::size_of::<u16>()
    }

    /// Returns the address of the first byte not yet allocated to a user.
    /// This address exists even if the max user number has been reached, because there is a memory
    /// reserve at the end of stable memory.
    fn unused_memory_start(&self) -> u64 {
        self.record_address(self.header.num_users)
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

    pub fn migration_state(&self) -> MigrationState {
        match self.header.version {
            3 => MigrationState::NotStarted,
            4 => MigrationState::Started {
                anchors_left: self.header.new_layout_start as u64,
                batch_size: self.header.migration_batch_size as u64,
            },
            5 => MigrationState::Finished,
            version => trap(&format!("unexpected header version: {}", version)),
        }
    }

    pub fn configure_migration(&mut self, migration_batch_size: u32) {
        if self.header.version == 5 {
            // migration is already done, nothing to do
            return;
        }

        if self.header.version == 3 {
            // only initialize this on the first migration configuration
            self.header.version = 4;
            self.header.new_layout_start = self.header.num_users;
        }

        self.header.migration_batch_size = migration_batch_size;
        self.flush();
    }

    fn migrate_record_batch(&mut self) -> Result<(), StorageError> {
        if self.header.version == 3 || self.header.version == 5 {
            // migration is not started or already done, nothing to do
            return Ok(());
        }

        for _ in 0..min(
            self.header.new_layout_start,
            self.header.migration_batch_size,
        ) {
            self.migrate_next_record()?;
        }

        if self.header.new_layout_start == 0 {
            self.finalize_migration();
        }
        self.flush();
        Ok(())
    }

    fn migrate_next_record(&mut self) -> Result<(), StorageError> {
        let record_number = self.header.new_layout_start - 1;
        let old_schema_bytes = self.read_entry_bytes(record_number);

        if old_schema_bytes.len() == 0 {
            // This anchor was only allocated but never written
            // --> nothing to migrate just update pointer.
            self.header.new_layout_start -= 1;
            return Ok(());
        }

        let devices: Vec<DeviceDataInternal> =
            candid::decode_one(&old_schema_bytes).map_err(StorageError::DeserializationError)?;
        let anchor = Anchor {
            devices: devices.into_iter().map(|d| Device::from(d)).collect(),
        };

        let new_schema_bytes =
            candid::encode_one(anchor).map_err(StorageError::SerializationError)?;
        self.write_entry_bytes(record_number, new_schema_bytes)?;
        self.header.new_layout_start -= 1;
        Ok(())
    }

    fn finalize_migration(&mut self) {
        if self.header.version == 5 {
            // migration is already done, nothing to do
            return;
        }

        assert_eq!(
            self.header.version, 4,
            "unexpected header version during migration finalization {}",
            self.header.version
        );
        assert_eq!({ self.header.new_layout_start }, 0, "cannot finalize migration when not all anchors were migrated! Remaining anchors to migrate: {}", { self.header.new_layout_start });

        self.header.version = 5;
        // clear now unused header fields
        self.header.migration_batch_size = 0;
        self.header.migration_batch_size = 0;
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
