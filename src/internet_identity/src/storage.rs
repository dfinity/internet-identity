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
//! The second page and onwards is managed by the [MemoryManager] and is currently split into the
//! following managed memories:
//! * Anchor memory: used to store the candid encoded anchors
//! * Archive buffer memory: used to store the archive entries yet to be pulled by the archive canister
//! * Persistent state memory: used to store the [PersistentState]
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
//! Internet Identity maintains a [PersistentState] for config and stats purposes which stored in a
//! [StableCell] in the virtual memory with id 2 managed using the [MemoryManager].
//! The [PersistentState] is currently only written to stable memory in the pre_upgrade hook.
//!
//! ## Archive buffer memory
//!
//! The archive buffer memory is entirely owned by a [StableBTreeMap] used to store the buffered
//! entries. The entries are indexed by their sequence number.
//!
//! The archive buffer memory is managed by the [MemoryManager] and is currently limited to a single
//! bucket of 128 pages.
use candid::{CandidType, Deserialize};
use ic_cdk::api::stable::WASM_PAGE_SIZE_IN_BYTES;
use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::io::{Read, Write};
use std::ops::RangeInclusive;

use ic_cdk::api::trap;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::reader::Reader;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::writer::Writer;
use ic_stable_structures::{
    Memory, MinHeap, RestrictedMemory, StableBTreeMap, StableCell, Storable,
};
use internet_identity_interface::archive::types::BufferedEntry;

use crate::openid::{OpenIdCredential, OpenIdCredentialKey};
use crate::state::PersistentState;
use crate::stats::event_stats::AggregationKey;
use crate::stats::event_stats::{EventData, EventKey};
use crate::storage::anchor::{Anchor, Device};
use crate::storage::memory_wrapper::MemoryWrapper;
use crate::storage::registration_rates::RegistrationRates;
use crate::storage::stable_anchor::StableAnchor;
use crate::storage::storable_anchor::StorableAnchor;
use crate::storage::storable_anchor_number_list::StorableAnchorNumberList;
use crate::storage::storable_credential_id::StorableCredentialId;
use crate::storage::storable_openid_credential_key::StorableOpenIdCredentialKey;
use crate::storage::storable_persistent_state::StorablePersistentState;
use internet_identity_interface::internet_identity::types::*;

pub mod anchor;
pub mod registration_rates;

pub mod stable_anchor;
/// module for the internal serialization format of anchors
mod storable_anchor;
mod storable_anchor_number_list;
mod storable_credential_id;
mod storable_openid_credential_key;
mod storable_persistent_state;
#[cfg(test)]
mod tests;

/// * version   0: invalid
/// * version 1-8: no longer supported
/// * version   9: 4KB anchors, candid anchor record layout, persistent state in virtual memory,
///   with memory manager (from 2nd page on), archive entries buffer in stable memory
const SUPPORTED_LAYOUT_VERSIONS: RangeInclusive<u8> = 9..=9;

const DEFAULT_ENTRY_SIZE: u16 = 4096;
const EMPTY_SALT: [u8; 32] = [0; 32];
const GB: u64 = 1 << 30;

/// MemoryManager parameters.
const ANCHOR_MEMORY_INDEX: u8 = 0u8;
const ARCHIVE_BUFFER_MEMORY_INDEX: u8 = 1u8;
const PERSISTENT_STATE_MEMORY_INDEX: u8 = 2u8;
const EVENT_DATA_MEMORY_INDEX: u8 = 3u8;
const STATS_AGGREGATIONS_MEMORY_INDEX: u8 = 4u8;
const REGISTRATION_REFERENCE_RATE_MEMORY_INDEX: u8 = 5u8;
const REGISTRATION_CURRENT_RATE_MEMORY_INDEX: u8 = 6u8;
const STABLE_ANCHOR_MEMORY_INDEX: u8 = 7u8;
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX: u8 = 8u8;
const LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX: u8 = 9u8;
const ANCHOR_MEMORY_ID: MemoryId = MemoryId::new(ANCHOR_MEMORY_INDEX);
const ARCHIVE_BUFFER_MEMORY_ID: MemoryId = MemoryId::new(ARCHIVE_BUFFER_MEMORY_INDEX);
const PERSISTENT_STATE_MEMORY_ID: MemoryId = MemoryId::new(PERSISTENT_STATE_MEMORY_INDEX);
const EVENT_DATA_MEMORY_ID: MemoryId = MemoryId::new(EVENT_DATA_MEMORY_INDEX);
const STATS_AGGREGATIONS_MEMORY_ID: MemoryId = MemoryId::new(STATS_AGGREGATIONS_MEMORY_INDEX);
const REGISTRATION_REFERENCE_RATE_MEMORY_ID: MemoryId =
    MemoryId::new(REGISTRATION_REFERENCE_RATE_MEMORY_INDEX);
const REGISTRATION_CURRENT_RATE_MEMORY_ID: MemoryId =
    MemoryId::new(REGISTRATION_CURRENT_RATE_MEMORY_INDEX);
const STABLE_ANCHOR_MEMORY_ID: MemoryId = MemoryId::new(STABLE_ANCHOR_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX);
// The bucket size 128 is relatively low, to avoid wasting memory when using
// multiple virtual memories for smaller amounts of data.
// This value results in 256 GB of total managed memory, which should be enough
// for the foreseeable future.
const BUCKET_SIZE_IN_PAGES: u16 = 128;
const MAX_MANAGED_MEMORY_SIZE: u64 = 256 * GB;
const MAX_MANAGED_WASM_PAGES: u64 = MAX_MANAGED_MEMORY_SIZE / WASM_PAGE_SIZE_IN_BYTES;

/// The maximum number of anchors this canister can store.
pub const MAX_ENTRIES: u64 = (MAX_MANAGED_WASM_PAGES - BUCKET_SIZE_IN_PAGES as u64) // deduct one bucket for the archive entries buffer
    * WASM_PAGE_SIZE_IN_BYTES
    / DEFAULT_ENTRY_SIZE as u64;

pub type Salt = [u8; 32];

type ManagedMemory<M> = VirtualMemory<RestrictedMemory<M>>;

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
    anchor_memory: ManagedMemory<M>,
    /// Memory wrapper used to report the size of the archive buffer memory.
    archive_buffer_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    archive_entries_buffer: StableBTreeMap<u64, BufferedEntryWrapper, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the persistent state memory.
    persistent_state_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    persistent_state: StableCell<StorablePersistentState, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the event data memory.
    event_data_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    pub event_data: StableBTreeMap<EventKey, EventData, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stats aggregation memory.
    event_aggregations_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    pub event_aggregations: StableBTreeMap<AggregationKey, u64, ManagedMemory<M>>,
    /// Registration rates tracked for the purpose of toggling the dynamic captcha (if configured)
    /// This data is persisted as it potentially contains data collected over longer periods of time.
    pub registration_rates: RegistrationRates<ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the current registration rate memory.
    current_registration_rate_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the reference registration rate memory.
    reference_registration_rate_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stable anchor memory.
    stable_anchor_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_anchor_memory: StableBTreeMap<AnchorNumber, StableAnchor, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the lookup anchor with OpenID credential memory.
    lookup_anchor_with_openid_credential_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_anchor_with_openid_credential_memory:
        StableBTreeMap<StorableOpenIdCredentialKey, StorableAnchorNumberList, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the lookup anchor with device credential memory.
    lookup_anchor_with_device_credential_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_anchor_with_device_credential_memory:
        StableBTreeMap<StorableCredentialId, AnchorNumber, ManagedMemory<M>>,
}

#[repr(C, packed)]
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

        if (id_range_hi - id_range_lo) > MAX_ENTRIES {
            trap(&format!(
                "id range [{id_range_lo}, {id_range_hi}) is too large for a single canister (max {MAX_ENTRIES} entries)",
            ));
        }
        let version: u8 = 9;
        let header = Header {
            magic: *b"IIC",
            version,
            num_anchors: 0,
            id_range_lo,
            id_range_hi,
            entry_size: DEFAULT_ENTRY_SIZE,
            salt: EMPTY_SALT,
        };

        let mut storage = Self::init_with_header(memory, header);
        storage.flush();
        storage
    }

    fn init_with_header(memory: M, header: Header) -> Self {
        let header_memory = RestrictedMemory::new(memory.clone(), 0..1);
        let memory_manager = MemoryManager::init_with_bucket_size(
            RestrictedMemory::new(memory, 1..MAX_MANAGED_WASM_PAGES),
            BUCKET_SIZE_IN_PAGES,
        );
        let anchor_memory = memory_manager.get(ANCHOR_MEMORY_ID);
        let archive_buffer_memory = memory_manager.get(ARCHIVE_BUFFER_MEMORY_ID);
        let persistent_state_memory = memory_manager.get(PERSISTENT_STATE_MEMORY_ID);
        let event_data_memory = memory_manager.get(EVENT_DATA_MEMORY_ID);
        let stats_aggregations_memory = memory_manager.get(STATS_AGGREGATIONS_MEMORY_ID);
        let registration_ref_rate_memory =
            memory_manager.get(REGISTRATION_REFERENCE_RATE_MEMORY_ID);
        let registration_current_rate_memory =
            memory_manager.get(REGISTRATION_CURRENT_RATE_MEMORY_ID);
        let stable_anchor_memory = memory_manager.get(STABLE_ANCHOR_MEMORY_ID);
        let lookup_anchor_with_openid_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID);
        let lookup_anchor_with_device_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_ID);

        let registration_rates = RegistrationRates::new(
            MinHeap::init(registration_ref_rate_memory.clone())
                .expect("failed to initialize registration reference rate min heap"),
            MinHeap::init(registration_current_rate_memory.clone())
                .expect("failed to initialize registration current rate min heap"),
        );
        Self {
            header,
            header_memory,
            anchor_memory,
            registration_rates,
            reference_registration_rate_memory_wrapper: MemoryWrapper::new(
                registration_ref_rate_memory,
            ),
            current_registration_rate_memory_wrapper: MemoryWrapper::new(
                registration_current_rate_memory,
            ),
            archive_buffer_memory_wrapper: MemoryWrapper::new(archive_buffer_memory.clone()),
            archive_entries_buffer: StableBTreeMap::init(archive_buffer_memory),
            persistent_state_memory_wrapper: MemoryWrapper::new(persistent_state_memory.clone()),
            persistent_state: StableCell::init(
                persistent_state_memory,
                StorablePersistentState::default(),
            )
            .expect("failed to initialize persistent state"),
            event_data_memory_wrapper: MemoryWrapper::new(event_data_memory.clone()),
            event_data: StableBTreeMap::init(event_data_memory),
            event_aggregations_memory_wrapper: MemoryWrapper::new(
                stats_aggregations_memory.clone(),
            ),
            event_aggregations: StableBTreeMap::init(stats_aggregations_memory),
            stable_anchor_memory_wrapper: MemoryWrapper::new(stable_anchor_memory.clone()),
            stable_anchor_memory: StableBTreeMap::init(stable_anchor_memory),
            lookup_anchor_with_openid_credential_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_openid_credential_memory.clone(),
            ),
            lookup_anchor_with_openid_credential_memory: StableBTreeMap::init(
                lookup_anchor_with_openid_credential_memory,
            ),
            lookup_anchor_with_device_credential_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_device_credential_memory.clone(),
            ),
            lookup_anchor_with_device_credential_memory: StableBTreeMap::init(
                lookup_anchor_with_device_credential_memory,
            ),
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
    /// Panics if the memory is empty or cannot be
    /// decoded.
    pub fn from_memory(memory: M) -> Self {
        if memory.size() < 1 {
            trap("stable memory is empty, cannot initialize");
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
            Either reinstall (wiping stable memory) or upgrade sequentially to the latest version of II by installing each intermediate version in turn.\n\
            See https://github.com/dfinity/internet-identity#stable-memory-compatibility for more information.",
                header.version
            ));
        }
        if !SUPPORTED_LAYOUT_VERSIONS.contains(&header.version) {
            trap(&format!("unsupported header version: {}", header.version));
        }

        Self::init_with_header(memory, header)
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

    /// This method can be replaced with `write` once `anchor_memory` is removed.
    pub fn create(&mut self, data: Anchor) -> Result<(), StorageError> {
        self.write(data, false)
    }

    /// This method can be replaced with `write` once `anchor_memory` is removed.
    pub fn update(&mut self, data: Anchor) -> Result<(), StorageError> {
        self.write(data, true)
    }

    /// Writes the data of the specified anchor to stable memory.
    ///
    /// It's not possible to know if an anchor has been written before,
    /// but we need to know this to safely read the previous anchor.
    ///
    /// Therefore, this information is passed as an additional argument,
    /// this argument can be removed once `anchor_memory` is removed.
    fn write(&mut self, data: Anchor, is_previously_written: bool) -> Result<(), StorageError> {
        let anchor_number = data.anchor_number();
        let (storable_anchor, stable_anchor): (StorableAnchor, StableAnchor) = data.into();

        // Get anchor address
        let record_number = self.anchor_number_to_record(anchor_number)?;
        let address = self.record_address(record_number);

        // Read previous fixed 4KB stable memory anchor
        let previous_storable_anchor = is_previously_written.then(|| {
            let mut reader = Reader::new(&self.anchor_memory, address);
            let mut read_buf = vec![0; self.header.entry_size as usize];
            reader
                .read_exact(&mut read_buf)
                .expect("failed to read memory");
            StorableAnchor::from_bytes(Cow::Owned(read_buf))
        });

        // Write current fixed 4KB stable memory anchor
        let write_buf = storable_anchor.to_bytes();
        if write_buf.len() > self.header.entry_size as usize {
            return Err(StorageError::EntrySizeLimitExceeded {
                space_required: write_buf.len() as u64,
                space_available: self.header.entry_size as u64,
            });
        }
        let mut writer = Writer::new(&mut self.anchor_memory, address);
        writer.write_all(&write_buf).expect("memory write failed");
        writer.flush().expect("memory write failed");

        // Write current and read previous unbounded stable structures anchor
        let previous_stable_anchor = self
            .stable_anchor_memory
            .insert(anchor_number, stable_anchor.clone());

        // Update `OpenIdCredential` to `Vec<AnchorNumber>` lookup map
        let previous_openid_credentials = previous_stable_anchor
            .map(|anchor| anchor.openid_credentials)
            .unwrap_or_default();
        let current_openid_credentials = stable_anchor.openid_credentials;
        self.update_lookup_anchors_with_openid_credential(
            anchor_number,
            previous_openid_credentials,
            current_openid_credentials,
        );

        // Update `CredentialId` to `AnchorNumber` lookup map
        let previous_devices = previous_storable_anchor.map_or(vec![], |anchor| anchor.devices);
        let current_devices = storable_anchor.devices;
        self.update_lookup_anchors_with_device_credential(
            anchor_number,
            previous_devices,
            current_devices,
        );

        Ok(())
    }

    /// Reads the data of the specified anchor from stable memory.
    pub fn read(&self, anchor_number: AnchorNumber) -> Result<Anchor, StorageError> {
        // Read fixed 4KB anchor
        let record_number = self.anchor_number_to_record(anchor_number)?;
        let address = self.record_address(record_number);

        let mut reader = Reader::new(&self.anchor_memory, address);
        let mut buf = vec![0; self.header.entry_size as usize];

        reader.read_exact(&mut buf).expect("failed to read memory");

        // Read unbounded stable structures anchor
        let storable_anchor = StorableAnchor::from_bytes(Cow::Owned(buf));
        let stable_anchor = self.stable_anchor_memory.get(&anchor_number);
        Ok(Anchor::from((
            anchor_number,
            storable_anchor,
            stable_anchor,
        )))
    }

    /// Update `OpenIdCredential` to `Vec<AnchorNumber>` lookup map
    fn update_lookup_anchors_with_openid_credential(
        &mut self,
        anchor_number: AnchorNumber,
        previous: Vec<OpenIdCredential>,
        current: Vec<OpenIdCredential>,
    ) {
        let previous_set: BTreeSet<OpenIdCredentialKey> =
            previous.into_iter().map(|cred| cred.key()).collect();
        let current_set: BTreeSet<OpenIdCredentialKey> =
            current.into_iter().map(|cred| cred.key()).collect();
        let credential_to_be_removed = previous_set.difference(&current_set);
        let credential_to_be_added = current_set.difference(&previous_set);
        credential_to_be_removed.cloned().for_each(|key| {
            self.lookup_anchor_with_openid_credential_memory
                .remove(&key.into());
        });
        credential_to_be_added.cloned().for_each(|key| {
            self.lookup_anchor_with_openid_credential_memory
                .insert(key.into(), vec![anchor_number].into());
        });
    }

    pub fn lookup_anchor_with_openid_credential(
        &self,
        key: &OpenIdCredentialKey,
    ) -> Option<AnchorNumber> {
        let anchor_numbers: Vec<AnchorNumber> = self
            .lookup_anchor_with_openid_credential_memory
            .get(&key.clone().into())
            .map(Into::into)?;
        anchor_numbers.first().copied()
    }

    /// Update `CredentialId` to `AnchorNumber` lookup map
    fn update_lookup_anchors_with_device_credential(
        &mut self,
        anchor_number: AnchorNumber,
        previous: Vec<Device>,
        current: Vec<Device>,
    ) {
        let previous_set: BTreeSet<CredentialId> = previous
            .into_iter()
            .filter_map(|device| device.credential_id)
            .collect();
        let current_set: BTreeSet<CredentialId> = current
            .into_iter()
            .filter_map(|device| device.credential_id)
            .collect();
        let credential_to_be_removed = previous_set.difference(&current_set);
        let credential_to_be_added = current_set.difference(&previous_set);
        credential_to_be_removed.cloned().for_each(|key| {
            self.lookup_anchor_with_device_credential_memory
                .remove(&key.into());
        });
        credential_to_be_added.cloned().for_each(|key| {
            self.lookup_anchor_with_device_credential_memory
                .insert(key.into(), anchor_number);
        });
    }

    #[allow(dead_code)]
    pub fn lookup_anchor_with_device_credential(&self, key: &CredentialId) -> Option<AnchorNumber> {
        self.lookup_anchor_with_device_credential_memory
            .get(&key.clone().into())
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

    pub fn assigned_anchor_number_range(&self) -> (AnchorNumber, AnchorNumber) {
        (self.header.id_range_lo, self.header.id_range_hi)
    }

    pub fn set_anchor_number_range(&mut self, (lo, hi): (AnchorNumber, AnchorNumber)) {
        if hi < lo {
            trap(&format!(
                "set_anchor_number_range: improper Identity Anchor range [{lo}, {hi})"
            ));
        }
        if (hi - lo) > MAX_ENTRIES {
            trap(&format!(
                "set_anchor_number_range: specified range [{lo}, {hi}) is too large for this canister \
                 (max {MAX_ENTRIES} entries)"
            ));
        }

        // restrict further if II has users to protect existing anchors
        if self.header.num_anchors > 0 {
            if self.header.id_range_lo != lo {
                trap(&format!(
                    "set_anchor_number_range: specified range [{lo}, {hi}) does not start from the same number ({}) \
                     as the existing range thus would make existing anchors invalid"
                    , { self.header.id_range_lo }));
            }
            // Check that all _existing_ anchors fit into the new range. I.e. making the range smaller
            // is ok as long as the range reduction only affects _unused_ anchor number.
            if (hi - lo) < self.header.num_anchors as u64 {
                trap(&format!(
                    "set_anchor_number_range: specified range [{lo}, {hi}) does not accommodate all {} anchors \
                     thus would make existing anchors invalid"
                    , { self.header.num_anchors }));
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

    /// Returns the number of entries in the archive buffer.
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

    pub fn write_persistent_state(&mut self, state: &PersistentState) {
        // The virtual memory is not limited in size, so for the expected size of the persistent state
        // this operation is infallible. The size of the persistent state is monitored and an alert
        // is raised if the size exceeds the expected size.
        self.persistent_state
            .set(StorablePersistentState::from(state.clone()))
            .expect("failed to write persistent state");
    }

    pub fn read_persistent_state(&self) -> PersistentState {
        PersistentState::from(self.persistent_state.get().clone())
    }

    pub fn version(&self) -> u8 {
        self.header.version
    }

    pub fn memory_sizes(&self) -> HashMap<String, u64> {
        HashMap::from_iter(vec![
            ("header".to_string(), self.header_memory.size()),
            ("identities".to_string(), self.anchor_memory.size()),
            (
                "archive_buffer".to_string(),
                self.archive_buffer_memory_wrapper.size(),
            ),
            (
                "persistent_state".to_string(),
                self.persistent_state_memory_wrapper.size(),
            ),
            (
                "event_data".to_string(),
                self.event_data_memory_wrapper.size(),
            ),
            (
                "event_aggregations".to_string(),
                self.event_aggregations_memory_wrapper.size(),
            ),
            (
                "reference_registration_rate".to_string(),
                self.reference_registration_rate_memory_wrapper.size(),
            ),
            (
                "current_registration_rate".to_string(),
                self.current_registration_rate_memory_wrapper.size(),
            ),
            (
                "stable_identities".to_string(),
                self.stable_anchor_memory_wrapper.size(),
            ),
            (
                "lookup_anchor_with_openid_credential".to_string(),
                self.lookup_anchor_with_openid_credential_memory_wrapper
                    .size(),
            ),
            (
                "lookup_anchor_with_device_credential".to_string(),
                self.lookup_anchor_with_device_credential_memory_wrapper
                    .size(),
            ),
        ])
    }
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

/// Helper module to hide internal memory of the memory wrapper.
mod memory_wrapper {
    use ic_stable_structures::Memory;

    /// Struct that holds a memory with the sole purpose to provide a function to get
    /// the size of the memory.
    pub struct MemoryWrapper<M: Memory> {
        memory: M,
    }

    impl<M: Memory> MemoryWrapper<M> {
        pub fn new(memory: M) -> Self {
            Self { memory }
        }

        pub fn size(&self) -> u64 {
            self.memory.size()
        }
    }
}
