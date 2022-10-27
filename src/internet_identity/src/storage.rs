//! This module implements all the stable memory interactions of Internet Identity.
//! It uses the [Reader] and [Writer] implementations of the `stable_structures` crate.
//!
//! ## Stable Memory Layout
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
//! ------------------------------------------- <- Address 58 (header size)
//! Reserved space              ↕ 454 bytes
//! ------------------------------------------- <- Address 512 = A_0_offset = RESERVED_HEADER_BYTES
//! A_0_size                    ↕ 2 bytes
//! -------------------------------------------
//! Candid encoded entry        ↕ A_0_size bytes
//! -------------------------------------------
//! Unused space A_0            ↕ (SIZE_MAX - A_0_size - 2) bytes
//! ------------------------------------------- <- A_1_offset = A_0_offset + (A_1 - A_0) * SIZE_MAX  ┬
//! A_1_size                    ↕ 2 bytes                                                            │
//! -------------------------------------------                                                      │
//! Candid encoded entry        ↕ A_1_size bytes                                          anchor A_1 │
//! -------------------------------------------                                                      │
//! Unused space A_1            ↕ (SIZE_MAX - A_1_size - 2) bytes                                    │
//! -------------------------------------------                                                      ┴
//! ...
//! ------------------------------------------- <- A_MAX_offset = A_0_offset + (A_MAX - A_0) * SIZE_MAX
//! A_MAX_size                  ↕ 2 bytes
//! -------------------------------------------
//! Candid encoded entry        ↕ A_MAX_size bytes
//! -------------------------------------------
//! Unused space A_MAX          ↕ (SIZE_MAX - A_MAX_size - 2) bytes
//! -------------------------------------------
//! Unallocated space           ↕ STABLE_MEMORY_RESERVE bytes
//! -------------------------------------------
//! ```

use crate::state::PersistentStateV1;
use candid;
use ic_cdk::api::trap;
use ic_stable_structures::reader::{BufferedReader, OutOfBounds, Reader};
use ic_stable_structures::writer::{BufferedWriter, Writer};
use ic_stable_structures::{GrowFailed, Memory};
use internet_identity_interface::UserNumber;
use std::convert::TryInto;
use std::fmt;
use std::io::{Read, Write};
use std::marker::PhantomData;

#[cfg(test)]
mod tests;

/// Reserved space for the header before the anchor records start.
const RESERVED_HEADER_BYTES: u64 = 512;
const DEFAULT_ENTRY_SIZE: u16 = 2048;
const EMPTY_SALT: [u8; 32] = [0; 32];
const GB: u64 = 1 << 30;
const STABLE_MEMORY_SIZE: u64 = 8 * GB;
/// We reserve last ~10% of the stable memory for later new features.
const STABLE_MEMORY_RESERVE: u64 = STABLE_MEMORY_SIZE / 10;

const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS"; // II Persistent State

/// The maximum number of users this canister can store.
pub const DEFAULT_RANGE_SIZE: u64 =
    (STABLE_MEMORY_SIZE - RESERVED_HEADER_BYTES - STABLE_MEMORY_RESERVE)
        / DEFAULT_ENTRY_SIZE as u64;

pub type Salt = [u8; 32];

/// Data type responsible for managing user data in stable memory.
pub struct Storage<T, M> {
    header: Header,
    memory: M,
    _marker: PhantomData<T>,
}

#[repr(packed)]
struct Header {
    magic: [u8; 3],
    version: u8,
    num_users: u32,
    id_range_lo: u64,
    id_range_hi: u64,
    entry_size: u16,
    salt: [u8; 32],
}

impl<T: candid::CandidType + serde::de::DeserializeOwned, M: Memory> Storage<T, M> {
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
                version: 1,
                num_users: 0,
                id_range_lo,
                id_range_hi,
                entry_size: DEFAULT_ENTRY_SIZE,
                salt: EMPTY_SALT,
            },
            memory,
            _marker: PhantomData,
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
        if header.version != 1 {
            trap(&format!("unsupported header version: {}", header.version));
        }

        Some(Self {
            header,
            memory,
            _marker: PhantomData,
        })
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
    pub fn write(&mut self, user_number: UserNumber, data: T) -> Result<(), StorageError> {
        let record_number = self.user_number_to_record(user_number)?;

        let stable_offset =
            RESERVED_HEADER_BYTES + record_number as u64 * self.header.entry_size as u64;
        let buf = candid::encode_one(data).map_err(StorageError::SerializationError)?;

        if buf.len() > self.value_size_limit() {
            return Err(StorageError::EntrySizeLimitExceeded(buf.len()));
        }

        // use buffered writer to minimize expensive stable memory operations
        let mut writer = BufferedWriter::new(
            self.header.entry_size as usize,
            Writer::new(&mut self.memory, stable_offset),
        );
        writer
            .write(&(buf.len() as u16).to_le_bytes())
            .expect("memory write failed");
        writer.write(&buf).expect("memory write failed");
        writer.flush().expect("memory write failed");
        Ok(())
    }

    /// Reads the data of the specified user from stable memory.
    pub fn read(&self, user_number: UserNumber) -> Result<T, StorageError> {
        let record_number = self.user_number_to_record(user_number)?;
        let stable_offset =
            RESERVED_HEADER_BYTES + record_number as u64 * self.header.entry_size as u64;

        // the reader will check stable memory bounds
        // use buffered reader to minimize expensive stable memory operations
        let mut reader = BufferedReader::new(
            self.header.entry_size as usize,
            Reader::new(&self.memory, stable_offset),
        );

        let mut len_buf = vec![0; 2];
        reader
            .read(&mut len_buf.as_mut_slice())
            .expect("failed to read memory");
        let len = u16::from_le_bytes(len_buf.try_into().unwrap()) as usize;

        // This error most likely indicates stable memory corruption.
        if len > self.value_size_limit() {
            trap(&format!(
                "persisted value size {} exeeds maximum size {}",
                len,
                self.value_size_limit()
            ))
        }

        let mut data_buf = vec![0; len];
        reader
            .read(&mut data_buf.as_mut_slice())
            .expect("failed to read memory");
        let data: T = candid::decode_one(&data_buf).map_err(StorageError::DeserializationError)?;

        Ok(data)
    }

    /// Make sure all the required metadata is recorded to stable memory.
    pub fn flush(&self) {
        if self.memory.size() < 1 {
            let result = self.memory.grow(1);
            if result == -1 {
                trap("failed to grow stable memory to 1 page");
            }
        }
        unsafe {
            let slice = std::slice::from_raw_parts(
                &self.header as *const _ as *const u8,
                std::mem::size_of::<Header>(),
            );
            self.memory.write(0, &slice);
        }
    }

    pub fn user_count(&self) -> usize {
        self.header.num_users as usize
    }

    /// Returns the maximum number of entries that this storage can fit.
    pub fn max_entries(&self) -> usize {
        ((STABLE_MEMORY_SIZE - RESERVED_HEADER_BYTES - STABLE_MEMORY_RESERVE)
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

    fn value_size_limit(&self) -> usize {
        self.header.entry_size as usize - std::mem::size_of::<u16>()
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

    /// Returns the address of the first byte not yet allocated to a user.
    /// This address exists even if the max user number has been reached, because there is a memory
    /// reserve at the end of stable memory.
    fn unused_memory_start(&self) -> u64 {
        let record_number = self.header.num_users as u64;
        RESERVED_HEADER_BYTES + record_number * self.header.entry_size as u64
    }

    /// Writes the persistent state to stable memory just outside of the space allocated to the highest user number.
    /// This is only used to _temporarily_ save state during upgrades. It will be overwritten on next user registration.
    pub fn write_persistent_state(
        &mut self,
        state: &PersistentStateV1,
    ) -> Result<(), PersistentStateError> {
        const WASM_PAGE_SIZE: u64 = 65536;
        let address = self.unused_memory_start();
        let encoded_state =
            candid::encode_one(state).map_err(|err| PersistentStateError::CandidError(err))?;

        // This block is an additional sanity check to make sure that the calculated address is in
        // the expected range so that no user data gets overridden.
        {
            // plus 1 because memory could lie right on the boundary of one wasm page to the next
            let max_wasm_pages_affected = div_ceil(encoded_state.len() as u64, WASM_PAGE_SIZE) + 1;
            let address_page = div_ceil(address, WASM_PAGE_SIZE);
            // compare with >= to allow for growth when actually writing the data
            assert!(address_page + max_wasm_pages_affected >= self.memory.size());
        }

        let mut writer = Writer::new(&mut self.memory, address);
        writer
            .write(&PERSISTENT_STATE_MAGIC)
            .map_err(|err| PersistentStateError::WriteError(err))?;
        writer
            .write(&(encoded_state.len() as u64).to_le_bytes())
            .map_err(|err| PersistentStateError::WriteError(err))?;
        writer
            .write(&encoded_state)
            .map_err(|err| PersistentStateError::WriteError(err))?;
        Ok(())
    }

    /// Reads the persistent state from stable memory just outside of the space allocated to the highest user number.
    /// This is only used to restore state in `post_upgrade`.
    pub fn read_persistent_state(&self) -> Result<PersistentStateV1, PersistentStateError> {
        let address = self.unused_memory_start();
        let mut reader = Reader::new(&self.memory, address);

        let mut magic_buf: [u8; 4] = [0; 4];
        reader
            .read(&mut magic_buf)
            .map_err(|err| PersistentStateError::ReadError(err))?;

        if magic_buf != PERSISTENT_STATE_MAGIC {
            return Err(PersistentStateError::NotFound);
        }

        let mut size_buf: [u8; 8] = [0; 8];
        reader
            .read(&mut size_buf)
            .map_err(|err| PersistentStateError::ReadError(err))?;

        let size = u64::from_le_bytes(size_buf);
        let mut data_buf = Vec::new();
        data_buf.resize(size as usize, 0);
        reader
            .read(data_buf.as_mut_slice())
            .map_err(|err| PersistentStateError::ReadError(err))?;

        candid::decode_one(&data_buf).map_err(|err| PersistentStateError::CandidError(err))
    }
}

/// Manual implementation because https://doc.rust-lang.org/std/primitive.u64.html#method.div_ceil is not stable yet.
fn div_ceil(a: u64, b: u64) -> u64 {
    (a + b - 1) / b
}

#[derive(Debug)]
pub enum PersistentStateError {
    CandidError(candid::error::Error),
    NotFound,
    ReadError(OutOfBounds),
    WriteError(GrowFailed),
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
