use super::{DeviceData, UserNumber};
use ic_cdk::api::stable::{stable_grow, stable_read, stable_size, stable_write};
use ic_cdk::export::candid;
use std::convert::TryInto;
use std::fmt;

const HEADER_SIZE: u32 = 512;
const ENTRY_SIZE: u32 = 512;
const WASM_PAGE_SIZE: u32 = 65536;

/// Data type responsible for managing user device data in stable
/// memory.
pub struct Storage {
    num_users: u32,
    user_number_range: (UserNumber, UserNumber),
}

impl Storage {
    /// Create a new empty storage that manages the data of users in
    /// the specified range.
    pub fn new(user_number_range: (UserNumber, UserNumber)) -> Self {
        let storage = Self {
            num_users: 0,
            user_number_range,
        };
        storage
    }

    /// Initialize storage by reading stable memory.
    ///
    /// Returns None if the stable memory is empty.
    ///
    /// Panics if the stable memory is not empty but cannot be
    /// decoded.
    pub fn from_stable_memory() -> Option<Self> {
        if stable_size() < 1 {
            return None;
        }

        let mut buf: [u8; 24] = [0; 24];
        stable_read(0, &mut buf);
        if &buf[0..3] != b"IIC" {
            panic!("stable memory header: invalid magic: {:?}", &buf[0..3]);
        }
        if buf[3] != 1 {
            panic!("unsupported header version: {}", buf[3]);
        }
        let num_users = u32::from_le_bytes(buf[4..8].try_into().unwrap());
        let id_range_lo = u64::from_le_bytes(buf[8..16].try_into().unwrap());
        let id_range_hi = u64::from_le_bytes(buf[16..24].try_into().unwrap());
        Some(Self {
            num_users,
            user_number_range: (id_range_lo, id_range_hi),
        })
    }

    /// Allocate a fresh user number.
    pub fn allocate_user_number(&mut self) -> UserNumber {
        let user_number = self.user_number_range.0 + self.num_users as u64;
        self.num_users += 1;
        self.write_header();
        user_number
    }

    /// Write the device data of the specified user to stable memory.
    pub fn write_device_data(
        &self,
        user_number: UserNumber,
        data: Vec<DeviceData>,
    ) -> Result<(), StorageError> {
        let record_number = self.user_number_to_record(user_number)?;

        let stable_offset = HEADER_SIZE + record_number * ENTRY_SIZE;
        let buf = candid::ser::encode_args((data,)).map_err(StorageError::SerializationError)?;

        if buf.len() > ENTRY_SIZE as usize - std::mem::size_of::<u16>() {
            return Err(StorageError::EntrySizeLimitExceeded(buf.len()));
        }

        let current_size = stable_size();
        let pages = (stable_offset + ENTRY_SIZE) / WASM_PAGE_SIZE + 1;
        if pages > current_size {
            let pages_to_grow = pages - current_size;
            let result = stable_grow(pages - current_size);
            assert!(
                result.is_ok(),
                "failed to grow stable memory by {} pages",
                pages_to_grow
            );
        }
        stable_write(stable_offset, &(buf.len() as u16).to_le_bytes());
        stable_write(stable_offset + std::mem::size_of::<u16>() as u32, &buf);
        Ok(())
    }

    /// Read the device data of the specified user from stable memory.
    pub fn read_device_data(
        &self,
        user_number: UserNumber,
    ) -> Result<Vec<DeviceData>, StorageError> {
        let record_number = self.user_number_to_record(user_number)?;

        let stable_offset = HEADER_SIZE + record_number * ENTRY_SIZE;
        if stable_offset + ENTRY_SIZE > stable_size() * WASM_PAGE_SIZE {
            panic!("a record for a valid user number is out of stable memory bounds");
        }

        let mut buf: [u8; ENTRY_SIZE as usize] = [0; 512];
        stable_read(stable_offset, &mut buf);
        let len = u16::from_le_bytes(buf[0..2].try_into().unwrap()) as usize;

        let (data,): (Vec<DeviceData>,) = candid::de::decode_args(&buf[2..2 + len])
            .map_err(StorageError::DeserializationError)?;

        Ok(data)
    }

    /// Make sure all the required metadata is recorded to stable memory.
    pub fn flush(&self) {
        self.write_header()
    }

    fn user_number_to_record(&self, user_number: u64) -> Result<u32, StorageError> {
        if user_number < self.user_number_range.0 || user_number >= self.user_number_range.1 {
            return Err(StorageError::UserNumberOutOfRange {
                user_number,
                range: self.user_number_range,
            });
        }

        let record_number = (user_number - self.user_number_range.0) as u32;
        if record_number >= self.num_users {
            return Err(StorageError::BadUserNumber(user_number));
        }
        Ok(record_number)
    }

    fn write_header(&self) {
        if stable_size() < 1 {
            let result = stable_grow(1);
            assert!(result.is_ok(), "failed to grow stable memory by 1 page");
        }
        let mut buf: [u8; 24] = [0; 24];
        buf[0..4].copy_from_slice(b"IIC\x01");
        buf[4..8].copy_from_slice(&self.num_users.to_le_bytes());
        buf[8..16].copy_from_slice(&self.user_number_range.0.to_le_bytes());
        buf[16..24].copy_from_slice(&self.user_number_range.1.to_le_bytes());
        stable_write(0, &buf);
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
