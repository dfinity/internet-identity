use super::UserNumber;
use ic_cdk::api::{
    stable::{stable_grow, stable_read, stable_size, stable_write},
    trap,
};
use ic_cdk::export::candid;
use std::convert::TryInto;
use std::fmt;
use std::marker::PhantomData;

const HEADER_SIZE: u32 = 512;
const DEFAULT_ENTRY_SIZE: u16 = 2048;
const EMPTY_SALT: [u8; 32] = [0; 32];
const WASM_PAGE_SIZE: u32 = 65536;

pub type Salt = [u8; 32];

/// Data type responsible for managing user data in stable memory.
pub struct Storage<T> {
    header: Header,
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

impl<T: candid::CandidType + serde::de::DeserializeOwned> Storage<T> {
    /// Creates a new empty storage that manages the data of users in
    /// the specified range.
    pub fn new(user_number_range: (UserNumber, UserNumber)) -> Self {
        let storage = Self {
            header: Header {
                magic: b"IIC".clone(),
                version: 1,
                num_users: 0,
                id_range_lo: user_number_range.0,
                id_range_hi: user_number_range.1,
                entry_size: DEFAULT_ENTRY_SIZE,
                salt: EMPTY_SALT,
            },
            _marker: PhantomData,
        };
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

    /// Initializes storage by reading stable memory.
    ///
    /// Returns None if the stable memory is empty.
    ///
    /// Panics if the stable memory is not empty but cannot be
    /// decoded.
    pub fn from_stable_memory() -> Option<Self> {
        if stable_size() < 1 {
            return None;
        }

        let mut header: Header = unsafe { std::mem::zeroed() };

        unsafe {
            let slice = std::slice::from_raw_parts_mut(
                &mut header as *mut _ as *mut u8,
                std::mem::size_of::<Header>(),
            );
            stable_read(0, slice);
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
            _marker: PhantomData,
        })
    }

    /// Allocates a fresh user number.
    ///
    /// Returns None if the range of user number assigned to this
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
    pub fn write(&self, user_number: UserNumber, data: T) -> Result<(), StorageError> {
        let record_number = self.user_number_to_record(user_number)?;

        let stable_offset = HEADER_SIZE + record_number * self.header.entry_size as u32;
        let buf = candid::ser::encode_one(data).map_err(StorageError::SerializationError)?;

        if buf.len() > self.value_size_limit() {
            return Err(StorageError::EntrySizeLimitExceeded(buf.len()));
        }

        let current_size = stable_size();
        let pages =
            (stable_offset + self.header.entry_size as u32 + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
        if pages > current_size {
            let pages_to_grow = pages - current_size;
            let result = stable_grow(pages - current_size);
            if !result.is_ok() {
                trap(&format!(
                    "failed to grow stable memory by {} pages",
                    pages_to_grow
                ))
            }
        }
        stable_write(stable_offset, &(buf.len() as u16).to_le_bytes());
        stable_write(stable_offset + std::mem::size_of::<u16>() as u32, &buf);
        Ok(())
    }

    /// Reads the data of the specified user from stable memory.
    pub fn read(&self, user_number: UserNumber) -> Result<T, StorageError> {
        let record_number = self.user_number_to_record(user_number)?;

        let stable_offset = HEADER_SIZE + record_number * self.header.entry_size as u32;
        if stable_offset + self.header.entry_size as u32 > stable_size() * WASM_PAGE_SIZE {
            trap("a record for a valid user number is out of stable memory bounds");
        }

        let mut buf = vec![0; self.header.entry_size as usize];
        stable_read(stable_offset, &mut buf);
        let len = u16::from_le_bytes(buf[0..2].try_into().unwrap()) as usize;

        // This error most likely indicates stable memory corruption.
        if len > self.value_size_limit() {
            trap(&format!(
                "persisted value size {} exeeds maximum size {}",
                len,
                self.value_size_limit()
            ))
        }

        let data: T =
            candid::de::decode_one(&buf[2..2 + len]).map_err(StorageError::DeserializationError)?;

        Ok(data)
    }

    /// Make sure all the required metadata is recorded to stable memory.
    pub fn flush(&self) {
        if stable_size() < 1 {
            let result = stable_grow(1);
            if !result.is_ok() {
                trap("failed to grow stable memory by 1 page");
            }
        }
        unsafe {
            let slice = std::slice::from_raw_parts(
                &self.header as *const _ as *const u8,
                std::mem::size_of::<Header>(),
            );
            stable_write(0, &slice);
        }
    }

    pub fn user_count(&self) -> usize {
        self.header.num_users as usize
    }

    pub fn assigned_user_number_range(&self) -> (UserNumber, UserNumber) {
        (self.header.id_range_lo, self.header.id_range_hi)
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
}

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
                "user number {} is out of range [{}, {})",
                user_number, range.0, range.1
            ),
            Self::BadUserNumber(n) => write!(f, "bad user number {}", n),
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
