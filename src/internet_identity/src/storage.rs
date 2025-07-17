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
use account::{
    Account, AccountsCounter, CreateAccountParams, ReadAccountParams, UpdateAccountParams,
    UpdateExistingAccountParams,
};
use candid::{CandidType, Deserialize};
use ic_cdk::api::stable::WASM_PAGE_SIZE_IN_BYTES;
use ic_stable_structures::cell::ValueError;
use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::io::{Read, Write};
use std::ops::RangeInclusive;
use storable::account_reference_list::StorableAccountReferenceList;
use storable::anchor_number_list::StorableAnchorNumberList;

use ic_cdk::api::trap;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::reader::Reader;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::writer::Writer;
use ic_stable_structures::{
    Memory, MinHeap, RestrictedMemory, StableBTreeMap, StableCell, Storable,
};
use internet_identity_interface::archive::types::BufferedEntry;

use crate::delegation::check_frontend_length;
use crate::openid::OpenIdCredentialKey;
use crate::state::PersistentState;
use crate::stats::event_stats::AggregationKey;
use crate::stats::event_stats::{EventData, EventKey};
use crate::storage::account::AccountReference;
use crate::storage::anchor::{Anchor, Device};
use crate::storage::memory_wrapper::MemoryWrapper;
use crate::storage::registration_rates::RegistrationRates;
use crate::storage::storable::account::StorableAccount;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::account_reference::StorableAccountReference;
use crate::storage::storable::accounts_counter::{AccountType, StorableAccountsCounter};
use crate::storage::storable::application_number::StorableApplicationNumber;
use internet_identity_interface::internet_identity::types::*;
use storable::anchor::StorableAnchor;
use storable::anchor_number::StorableAnchorNumber;
use storable::application::{StorableApplication, StorableOriginHash};
use storable::credential_id::StorableCredentialId;
use storable::discrepancy_counter::{DiscrepancyType, StorableDiscrepancyCounter};
use storable::fixed_anchor::StorableFixedAnchor;
use storable::openid_credential::StorableOpenIdCredential;
use storable::openid_credential_key::StorableOpenIdCredentialKey;
use storable::storable_persistent_state::StorablePersistentState;

pub mod anchor;
pub mod registration_rates;

pub mod account;

mod storable;

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
// These memory indexes have been abandoned, do not use them
// const DEPRECATED_STABLE_ANCHOR_MEMORY_INDEX: u8 = 7u8;
// const DEPRECATED_LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX: u8 = 8u8;
const LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX: u8 = 9u8;
const STABLE_ACCOUNT_MEMORY_INDEX: u8 = 10u8;
const STABLE_APPLICATION_MEMORY_INDEX: u8 = 11u8;
const LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_INDEX: u8 = 12u8;
const STABLE_ACCOUNT_REFERENCE_LIST_MEMORY_INDEX: u8 = 13u8;
const STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_INDEX: u8 = 14u8;
const STABLE_ACCOUNT_COUNTER_MEMORY_INDEX: u8 = 15u8;
const STABLE_ANCHOR_MEMORY_INDEX: u8 = 16u8;
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX: u8 = 17u8;
const STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_INDEX: u8 = 18u8;

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
const STABLE_ACCOUNT_MEMORY_ID: MemoryId = MemoryId::new(STABLE_ACCOUNT_MEMORY_INDEX);
const STABLE_APPLICATION_MEMORY_ID: MemoryId = MemoryId::new(STABLE_APPLICATION_MEMORY_INDEX);
const STABLE_ACCOUNT_REFERENCE_LIST_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ACCOUNT_REFERENCE_LIST_MEMORY_INDEX);
const STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_INDEX);
const STABLE_ACCOUNT_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ACCOUNT_COUNTER_MEMORY_INDEX);
const STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX);
const LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_INDEX);
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
    stable_anchor_memory: StableBTreeMap<StorableAnchorNumber, StorableAnchor, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stable account memory.
    stable_account_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_account_memory: StableBTreeMap<StorableAccountNumber, StorableAccount, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stable application memory.
    stable_application_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_application_memory:
        StableBTreeMap<StorableApplicationNumber, StorableApplication, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stable account counter memory.
    stable_anchor_account_counter_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_anchor_account_counter_memory:
        StableBTreeMap<StorableAnchorNumber, StorableAccountsCounter, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the stable account reference list memory.
    stable_account_reference_list_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_account_reference_list_memory: StableBTreeMap<
        (StorableAnchorNumber, StorableApplicationNumber),
        StorableAccountReferenceList,
        ManagedMemory<M>,
    >,
    stable_account_counter_memory: StableCell<StorableAccountsCounter, ManagedMemory<M>>,
    /// Counter that counts how often there was a discrepancy between the anchor accounts counter and the actual number of accounts
    stable_account_counter_discrepancy_counter_memory:
        StableCell<StorableDiscrepancyCounter, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the lookup anchor with OpenID credential memory.
    lookup_anchor_with_openid_credential_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_anchor_with_openid_credential_memory:
        StableBTreeMap<StorableOpenIdCredentialKey, StorableAnchorNumberList, ManagedMemory<M>>,
    /// Memory wrapper used to report the size of the lookup anchor with device credential memory.
    lookup_anchor_with_device_credential_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_anchor_with_device_credential_memory:
        StableBTreeMap<StorableCredentialId, StorableAnchorNumber, ManagedMemory<M>>,
    lookup_application_with_origin_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_application_with_origin_memory:
        StableBTreeMap<StorableOriginHash, StorableApplicationNumber, ManagedMemory<M>>,
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
        let stable_account_memory = memory_manager.get(STABLE_ACCOUNT_MEMORY_ID);
        let stable_application_memory = memory_manager.get(STABLE_APPLICATION_MEMORY_ID);
        let stable_anchor_account_counter_memory =
            memory_manager.get(STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_ID);
        let stable_account_reference_list_memory =
            memory_manager.get(STABLE_ACCOUNT_REFERENCE_LIST_MEMORY_ID);
        let stable_account_counter_memory = memory_manager.get(STABLE_ACCOUNT_COUNTER_MEMORY_ID);
        let stable_account_counter_discrepancy_counter_memory =
            memory_manager.get(STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_ID);
        let lookup_anchor_with_openid_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID);
        let lookup_anchor_with_device_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_ID);
        let lookup_application_with_origin_memory =
            memory_manager.get(LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_ID);

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
            stable_account_memory_wrapper: MemoryWrapper::new(stable_account_memory.clone()),
            stable_account_memory: StableBTreeMap::init(stable_account_memory),
            stable_application_memory_wrapper: MemoryWrapper::new(
                stable_application_memory.clone(),
            ),
            stable_application_memory: StableBTreeMap::init(stable_application_memory),
            stable_anchor_account_counter_memory_wrapper: MemoryWrapper::new(
                stable_anchor_account_counter_memory.clone(),
            ),
            stable_anchor_account_counter_memory: StableBTreeMap::init(
                stable_anchor_account_counter_memory,
            ),
            stable_account_reference_list_memory_wrapper: MemoryWrapper::new(
                stable_account_reference_list_memory.clone(),
            ),
            stable_account_reference_list_memory: StableBTreeMap::init(
                stable_account_reference_list_memory,
            ),
            stable_account_counter_memory: StableCell::init(
                stable_account_counter_memory,
                StorableAccountsCounter::default(),
            )
            .expect("stable_account_counter_memory"),
            stable_account_counter_discrepancy_counter_memory: StableCell::init(
                stable_account_counter_discrepancy_counter_memory,
                StorableDiscrepancyCounter::default(),
            )
            .expect("failed to initialize discrepancy counter"),
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
            lookup_application_with_origin_memory_wrapper: MemoryWrapper::new(
                lookup_application_with_origin_memory.clone(),
            ),
            lookup_application_with_origin_memory: StableBTreeMap::init(
                lookup_application_with_origin_memory,
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
        let (storable_anchor, stable_anchor): (StorableFixedAnchor, StorableAnchor) = data.into();

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
            StorableFixedAnchor::from_bytes(Cow::Owned(read_buf))
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
        let storable_anchor = StorableFixedAnchor::from_bytes(Cow::Owned(buf));
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
        previous: Vec<StorableOpenIdCredential>,
        current: Vec<StorableOpenIdCredential>,
    ) {
        let previous_set: BTreeSet<StorableOpenIdCredentialKey> =
            previous.into_iter().map(|cred| cred.key()).collect();
        let current_set: BTreeSet<StorableOpenIdCredentialKey> =
            current.into_iter().map(|cred| cred.key()).collect();
        let credential_to_be_removed = previous_set.difference(&current_set);
        let credential_to_be_added = current_set.difference(&previous_set);
        credential_to_be_removed.cloned().for_each(|key| {
            self.lookup_anchor_with_openid_credential_memory
                .remove(&key);
        });
        credential_to_be_added.cloned().for_each(|key| {
            self.lookup_anchor_with_openid_credential_memory
                .insert(key, vec![anchor_number].into());
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
        credential_to_be_removed
            .cloned()
            .map(StorableCredentialId::from)
            .for_each(|credential_id| {
                // Only remove if the credential is assigned to this anchor
                if self
                    .lookup_anchor_with_device_credential_memory
                    .get(&credential_id)
                    .is_some_and(|other_anchor_number| other_anchor_number == anchor_number)
                {
                    self.lookup_anchor_with_device_credential_memory
                        .remove(&credential_id);
                }
            });
        credential_to_be_added
            .cloned()
            .map(StorableCredentialId::from)
            .for_each(|credential_id| {
                // Only insert if the credential id isn't assigned yet to an anchor
                if !self
                    .lookup_anchor_with_device_credential_memory
                    .contains_key(&credential_id)
                {
                    self.lookup_anchor_with_device_credential_memory
                        .insert(credential_id, anchor_number);
                }
            });
    }

    #[allow(dead_code)]
    pub fn lookup_anchor_with_device_credential(&self, key: &CredentialId) -> Option<AnchorNumber> {
        self.lookup_anchor_with_device_credential_memory
            .get(&key.clone().into())
    }

    /// Look up an application number per origin, create entry in applications and lookup table if it doesn't exist
    pub fn lookup_or_insert_application_number_with_origin(
        &mut self,
        origin: &FrontendHostname,
    ) -> ApplicationNumber {
        let origin_hash = StorableOriginHash::from_origin(origin);

        if let Some(existing_number) = self.lookup_application_with_origin_memory.get(&origin_hash)
        {
            existing_number
        } else {
            let new_number: ApplicationNumber = self.lookup_application_with_origin_memory.len();

            self.lookup_application_with_origin_memory
                .insert(origin_hash, new_number);

            let new_application = StorableApplication {
                origin: origin.to_string(),
                stored_accounts: 0u64,
                stored_account_references: 0u64,
            };

            self.stable_application_memory
                .insert(new_number, new_application);
            new_number
        }
    }

    pub fn lookup_application_number_with_origin(
        &self,
        origin: &FrontendHostname,
    ) -> Option<ApplicationNumber> {
        self.lookup_application_with_origin_memory
            .get(&StorableOriginHash::from_origin(origin))
    }

    #[allow(dead_code)]
    fn lookup_application_with_origin(
        &self,
        origin: &FrontendHostname,
    ) -> Option<StorableApplication> {
        self.lookup_application_number_with_origin(origin)
            .and_then(|application_number| self.stable_application_memory.get(&application_number))
    }

    fn lookup_account_references(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> Option<Vec<StorableAccountReference>> {
        self.stable_account_reference_list_memory
            .get(&(anchor_number, application_number))
            .map(|list| list.into_vec())
    }

    fn find_account_reference(
        &self,
        anchor_number: AnchorNumber,
        application_number: Option<ApplicationNumber>,
        account_number: Option<AccountNumber>,
    ) -> Option<StorableAccountReference> {
        application_number.and_then(|app_num| {
            self.lookup_account_references(anchor_number, app_num)
                .and_then(|acc_ref_vec| {
                    acc_ref_vec
                        .iter()
                        .find(|acc_ref| acc_ref.account_number == account_number)
                        .cloned()
                })
        })
    }

    /// Updates the anchor account, application and account counters.
    /// It doesn't update the account counter for Account type.
    /// Because that one is updated when a new account number is allocated with `allocate_account_number`.
    fn update_counters(
        &mut self,
        application_number: ApplicationNumber,
        anchor_number: AnchorNumber,
        account_type: AccountType,
    ) -> Result<(), StorageError> {
        let anchor_account_counter = self
            .stable_anchor_account_counter_memory
            .get(&anchor_number)
            .unwrap_or(StorableAccountsCounter {
                stored_accounts: 0,
                stored_account_references: 0,
            });
        self.stable_anchor_account_counter_memory.insert(
            anchor_number,
            anchor_account_counter.increment(&account_type),
        );

        // The account counter is updated when a new account number is allocated with `allocate_account_number`.
        if account_type == AccountType::AccountReference {
            let account_number = self.stable_account_counter_memory.get();
            self.stable_account_counter_memory
                .set(account_number.increment(&account_type))
                .map_err(|_| StorageError::ErrorUpdatingAccountCounter)?;
        }

        if let Some(mut application) = self.stable_application_memory.get(&application_number) {
            match account_type {
                AccountType::Account => application.stored_accounts += 1,
                AccountType::AccountReference => application.stored_account_references += 1,
            }
            self.stable_application_memory
                .insert(application_number, application);
        }
        Ok(())
    }

    /// This is for testing purposes only, DO NOT use anywhere else!
    #[cfg(test)]
    #[allow(dead_code)]
    pub fn set_counters_for_testing(
        &mut self,
        anchor_number: AnchorNumber,
        stored_accounts: u64,
        stored_account_references: u64,
    ) {
        self.stable_anchor_account_counter_memory.insert(
            anchor_number,
            StorableAccountsCounter {
                stored_accounts,
                stored_account_references,
            },
        );
    }

    /// Returns the account counter for a given anchor number.
    pub fn get_account_counter(&self, anchor_number: AnchorNumber) -> AccountsCounter {
        self.stable_anchor_account_counter_memory
            .get(&anchor_number)
            .unwrap_or(StorableAccountsCounter {
                stored_accounts: 0,
                stored_account_references: 0,
            })
            .into()
    }

    /// Returns the total account counter.
    pub fn get_total_accounts_counter(&self) -> AccountsCounter {
        self.stable_account_counter_memory.get().clone().into()
    }

    /// Returns the total application count.
    pub fn get_total_application_count(&self) -> u64 {
        self.stable_application_memory.len()
    }

    // Increments the `stable_account_counter_memory` account counter by one and returns the new number.
    fn allocate_account_number(&mut self) -> Result<AccountNumber, StorageError> {
        let account_counter = self.stable_account_counter_memory.get();
        let updated_accounts_counter = account_counter.increment(&AccountType::Account);
        let next_account_number = updated_accounts_counter.stored_accounts;
        self.stable_account_counter_memory
            .set(updated_accounts_counter)
            .map_err(|_| StorageError::ErrorUpdatingAccountCounter)?;
        Ok(next_account_number)
    }

    /// Returns all account references associated with a single anchor number, across all applications.
    pub fn list_identity_account_references(
        &self,
        anchor_number: AnchorNumber,
    ) -> Vec<AccountReference> {
        let range_start = (anchor_number, ApplicationNumber::MIN);
        let range_end = (anchor_number, ApplicationNumber::MAX);

        self.stable_account_reference_list_memory
            .range(range_start..=range_end)
            .flat_map(|(_, storable_account_ref_list_val)| storable_account_ref_list_val.into_vec())
            .map(AccountReference::from)
            .collect()
    }

    /// Rebuilds the account and account reference counters for a given identity
    pub fn rebuild_identity_account_counters(&mut self, anchor_number: AnchorNumber) {
        // increment metrics
        let _ = self.increment_discrepancy_counter(&DiscrepancyType::AccountRebuild);

        // get actual list of stored references and accounts
        let acc_ref_list = self.list_identity_account_references(anchor_number);

        let mut stored_accounts = 0;
        let mut stored_account_references = 0;

        acc_ref_list.iter().for_each(|acc_ref| {
            // for every reference, we increment the account references counter
            stored_account_references += 1;
            // if the account reference has an account number and is thus stored, also increment the stored accounts counter
            if acc_ref.account_number.is_some() {
                stored_accounts += 1;
            }
        });

        self.stable_anchor_account_counter_memory.insert(
            anchor_number,
            StorableAccountsCounter {
                stored_accounts,
                stored_account_references,
            },
        );
    }

    /// Increments the discrepancy counter (this is so we can ascertain correctness of our counters - ideally, this is never actually called)
    fn increment_discrepancy_counter(
        &mut self,
        discrepancy_type: &DiscrepancyType,
    ) -> Result<StorableDiscrepancyCounter, ValueError> {
        let counters = self.stable_account_counter_discrepancy_counter_memory.get();

        self.stable_account_counter_discrepancy_counter_memory
            .set(counters.increment(discrepancy_type))
    }

    /// Retrieves the discrepancy counter
    pub fn get_discrepancy_counter(&self) -> &StorableDiscrepancyCounter {
        self.stable_account_counter_discrepancy_counter_memory.get()
    }

    pub fn create_additional_account(
        &mut self,
        params: CreateAccountParams,
    ) -> Result<Account, StorageError> {
        check_frontend_length(&params.origin);
        let anchor_number = params.anchor_number;
        let origin = &params.origin;

        // Create and store account in stable memory
        let account_number = self.allocate_account_number()?;
        let storable_account = StorableAccount {
            name: params.name.clone(),
            seed_from_anchor: None,
        };
        self.stable_account_memory
            .insert(account_number, storable_account);

        // Update application data
        let app_num = self.lookup_or_insert_application_number_with_origin(origin);

        // Update counters with one more account.
        self.update_counters(app_num, anchor_number, AccountType::Account)?;

        // Process account references
        match self
            .stable_account_reference_list_memory
            .get(&(anchor_number, app_num))
        {
            None => {
                // Two new account references were created.
                self.update_counters(app_num, anchor_number, AccountType::AccountReference)?;
                self.update_counters(app_num, anchor_number, AccountType::AccountReference)?;
                // If no list exists for this anchor & application,
                // Create and insert the default and additional account.
                // This is because we don't create default accounts explicitly.
                let additional_account_reference = AccountReference {
                    account_number: Some(account_number),
                    last_used: None,
                };
                let default_account_reference = AccountReference {
                    account_number: None,
                    last_used: None,
                };
                self.stable_account_reference_list_memory.insert(
                    (anchor_number, app_num),
                    vec![default_account_reference, additional_account_reference].into(),
                );
            }
            Some(existing_storable_list) => {
                self.update_counters(app_num, anchor_number, AccountType::AccountReference)?;
                // If the list exists, push the new account and reinsert it to memory
                let mut refs_vec: Vec<AccountReference> = existing_storable_list.into();
                refs_vec.push(AccountReference {
                    account_number: Some(account_number),
                    last_used: None,
                });
                self.stable_account_reference_list_memory
                    .insert((anchor_number, app_num), refs_vec.into());
            }
        }

        // Return the new account
        Ok(Account::new(
            anchor_number,
            origin.to_string(),
            Some(params.name),
            Some(account_number),
        ))
    }

    #[allow(dead_code)]
    /// Returns a list of accounts for a given anchor and application.
    /// If the application doesn't exist, returns a list with a synthetic default account.
    /// If the account references don't exist, returns a list with a synthetic default account.
    pub fn list_accounts(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
    ) -> Vec<Account> {
        check_frontend_length(origin);
        match self.lookup_application_number_with_origin(origin) {
            None => vec![Account::new(anchor_number, origin.clone(), None, None)],
            Some(app_num) => match self.lookup_account_references(anchor_number, app_num) {
                None => vec![Account::new(anchor_number, origin.clone(), None, None)],
                Some(refs) => refs
                    .iter()
                    .filter_map(|acc_ref| {
                        self.read_account(ReadAccountParams {
                            account_number: acc_ref.account_number,
                            anchor_number,
                            origin,
                            known_app_num: Some(app_num),
                        })
                    })
                    .collect(),
            },
        }
    }

    /// Returns the requested `Account`.
    /// If the anchor doesn't own this `Account`, returns None.
    /// If the `Account` is default but has been moved/deleted, returns None.
    /// If the `Account` is default but ALL `Account`s for this origin have been moved or deleted, returns a default `Account`.
    /// If the `Account` number doesn't esist, returns a default `Account`.
    /// If the `Account` number exists but the `Account` doesn't exist, returns None.
    /// If the `Account` exists, returns it as `Account`.
    /// Optionally an application number can be passed if it is already known, so we don't look it up more than necessary.
    pub fn read_account(&self, params: ReadAccountParams) -> Option<Account> {
        check_frontend_length(params.origin);
        let application_number = params
            .known_app_num
            .or_else(|| self.lookup_application_number_with_origin(params.origin));

        match params.account_number {
            // If a default account is requested
            None => {
                // if there is no stored application, return a synthetic default account
                if application_number.is_none() {
                    return Some(Account::new(
                        params.anchor_number,
                        params.origin.clone(),
                        None,
                        None,
                    ));
                }
                // check if there is a stored account reference list
                if let Some(acc_ref_vec) =
                    // we can safely unwrap here
                    self.lookup_account_references(
                        params.anchor_number,
                        application_number.unwrap(),
                    )
                {
                    // if the list exists but is empty, we should still return a synthetic default account
                    // this should only happen if a named account was created, and then both it and the
                    // default account references were moved/deleted.
                    // XXX WARNING: this is done for the case that a user might have moved/deleted a default account
                    // and then reached the maximum accounts limit. If we don't return a synthetic default account here,
                    // they would be locked out of their account.
                    // However: if we implement account transfers at some point, and default accounts can be transfered,
                    // this would allow a user to regain access to their transferred default account.
                    if acc_ref_vec.is_empty() {
                        return Some(Account::new(
                            params.anchor_number,
                            params.origin.clone(),
                            None,
                            None,
                        ));
                    }

                    // if there is a default account in the list, we return it
                    // else we return None, account has been moved or deleted
                    // but there is another account in the list, so user can log in with that
                    acc_ref_vec
                        .iter()
                        .find(|acc_ref| acc_ref.account_number.is_none())
                        .map(|acc_ref| {
                            Account::new_with_last_used(
                                params.anchor_number,
                                params.origin.clone(),
                                None,
                                acc_ref.account_number,
                                acc_ref.last_used,
                            )
                        })
                } else {
                    //if there is no list, we return a synthetic default account
                    Some(Account::new(
                        params.anchor_number,
                        params.origin.clone(),
                        None,
                        None,
                    ))
                }
            }
            // if a named/stored account is requested
            Some(account_number) => match self.stable_account_memory.get(&account_number) {
                // if it does not exist, return None
                None => None,
                Some(storable_account) => {
                    // if it does exist, check whether it is owned by the caller anchor
                    // and belongs to the correct origin
                    self.find_account_reference(
                        params.anchor_number,
                        application_number,
                        params.account_number,
                    )
                    .map(|acc_ref| {
                        Account::new_full(
                            params.anchor_number,
                            params.origin.clone(),
                            Some(storable_account.name.clone()),
                            Some(account_number),
                            acc_ref.last_used,
                            storable_account.seed_from_anchor,
                        )
                    })
                }
            },
        }
    }

    /// Updates an account.
    /// If the account number exists, then updates that account.
    /// If the account number doesn't exist, then gets or creates an application and creates and stores a default account.
    pub fn update_account(&mut self, params: UpdateAccountParams) -> Result<Account, StorageError> {
        check_frontend_length(&params.origin);
        match params.account_number {
            Some(account_number) => self.update_existing_account(UpdateExistingAccountParams {
                account_number,
                anchor_number: params.anchor_number,
                name: params.name,
                origin: params.origin,
            }),
            None => {
                // Default accounts are not stored by default.
                // They are created only once they are updated.
                self.create_default_account(CreateAccountParams {
                    anchor_number: params.anchor_number,
                    name: params.name,
                    origin: params.origin.clone(),
                })
            }
        }
    }

    /// Used in `update_account` to update an existing account.
    fn update_existing_account(
        &mut self,
        params: UpdateExistingAccountParams,
    ) -> Result<Account, StorageError> {
        // Check if account reference exists for given anchor number, origin and account number,
        // if the account refence exists for a given anchor, that means the anchor has access.
        let application_number = self.lookup_application_number_with_origin(&params.origin);
        let account_reference = self
            .find_account_reference(
                params.anchor_number,
                application_number,
                Some(params.account_number),
            )
            .ok_or(StorageError::AccountNotFound {
                account_number: params.account_number,
            })?;
        // Check if the account reference has an account number,
        // throw error if it doesn't since we only want to update
        // accounts with an account number in this function.
        let account_number =
            account_reference
                .account_number
                .ok_or(StorageError::AccountNotFound {
                    account_number: params.account_number,
                })?;
        // Read account from storage
        let mut storable_account = self.stable_account_memory.get(&account_number).ok_or(
            StorageError::AccountNotFound {
                account_number: params.account_number,
            },
        )?;
        // Update account and write back to storage
        storable_account.name = params.name;
        self.stable_account_memory
            .insert(params.account_number, storable_account.clone());
        // Return updated account
        Ok(Account::new_full(
            params.anchor_number,
            params.origin,
            Some(storable_account.name),
            Some(account_number),
            account_reference.last_used,
            storable_account.seed_from_anchor,
        ))
    }

    /// Used in `update_account` to create a default account.
    /// Default account are not initially stored. They are stored when updated.
    /// If the default account reference does not exist, it must be created.
    /// If the default account reference exists, its account number must be updated.
    fn create_default_account(
        &mut self,
        params: CreateAccountParams,
    ) -> Result<Account, StorageError> {
        // Create and store the default account.
        let new_account_number = self.allocate_account_number()?;
        let storable_account = StorableAccount {
            name: params.name.clone(),
            // This was a default account which uses the anchor number for the seed.
            seed_from_anchor: Some(params.anchor_number),
        };
        self.stable_account_memory
            .insert(new_account_number, storable_account.clone());

        // Get or create an application number from the account's origin.
        let application_number =
            self.lookup_or_insert_application_number_with_origin(&params.origin);
        // Update counters with one more account.
        self.update_counters(
            application_number,
            params.anchor_number,
            AccountType::Account,
        )?;

        // Update the account references list.
        let account_references_key = (params.anchor_number, application_number);
        match self
            .stable_account_reference_list_memory
            .get(&account_references_key)
        {
            None => {
                // If no list exists for this anchor & application,
                // Create and insert the default account.
                // This is because we don't create default accounts explicitly.
                let new_ref = AccountReference {
                    account_number: Some(new_account_number),
                    last_used: None,
                };
                self.stable_account_reference_list_memory
                    .insert(account_references_key, vec![new_ref].into());
                // One new account reference was created.
                self.update_counters(
                    application_number,
                    params.anchor_number,
                    AccountType::AccountReference,
                )?;
            }
            Some(existing_storable_list) => {
                // If the list exists, update the default account reference with the new account number.
                let mut refs_vec: Vec<AccountReference> = existing_storable_list.into();
                let mut found_and_updated = false;
                for r_mut in refs_vec.iter_mut() {
                    if r_mut.account_number.is_none() {
                        // Found the default account reference.
                        r_mut.account_number = Some(new_account_number);
                        found_and_updated = true;
                        break;
                    }
                }

                // This could happen if the account was removed and now we try to update it.
                if !found_and_updated {
                    return Err(StorageError::MissingAccount {
                        anchor_number: params.anchor_number,
                        name: params.name.clone(),
                    });
                }
                self.stable_account_reference_list_memory
                    .insert(account_references_key, refs_vec.into());
            }
        }

        // Return created default account
        Ok(Account::new_full(
            params.anchor_number,
            params.origin,
            Some(storable_account.name),
            Some(new_account_number),
            None,
            storable_account.seed_from_anchor,
        ))
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
                "stable_accounts".to_string(),
                self.stable_account_memory_wrapper.size(),
            ),
            (
                "stable_applications".to_string(),
                self.stable_application_memory_wrapper.size(),
            ),
            (
                "stable_account_counter".to_string(),
                self.stable_anchor_account_counter_memory_wrapper.size(),
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
            (
                "lookup_application_with_origin".to_string(),
                self.lookup_application_with_origin_memory_wrapper.size(),
            ),
            (
                "stable_account_reference_list".to_string(),
                self.stable_account_reference_list_memory_wrapper.size(),
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
    AnchorNotFound {
        anchor_number: AnchorNumber,
    },
    ApplicationNotFound {
        origin: FrontendHostname,
    },
    MissingAccountName,
    MissingAccount {
        anchor_number: AnchorNumber,
        name: String,
    },
    AccountNotFound {
        account_number: AccountNumber,
    },
    OriginNotFoundForApplicationNumber {
        application_number: ApplicationNumber,
    },
    ErrorUpdatingAccountCounter,
}

impl fmt::Display for StorageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AnchorNumberOutOfRange {
                anchor_number,
                range,
            } => write!(
                f,
                "Identity Anchor {anchor_number} is out of range [{}, {})",
                range.0, range.1
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
            Self::AnchorNotFound { anchor_number } => {
                write!(
                    f,
                    "StableAnchor not found for anchor number {anchor_number}",
                )
            }
            Self::ApplicationNotFound { origin } => {
                write!(f, "Application not found for origin {origin}")
            }
            Self::MissingAccountName => write!(f, "Account name is missing"),
            Self::MissingAccount {
                anchor_number,
                name,
            } => {
                write!(
                    f,
                    "Account not found for anchor number {anchor_number} and name {name}",
                )
            }
            Self::AccountNotFound { account_number } => {
                write!(f, "Account not found for account number {account_number}")
            }
            Self::OriginNotFoundForApplicationNumber { application_number } => write!(
                f,
                "Origin not found for application number {application_number}",
            ),
            Self::ErrorUpdatingAccountCounter => write!(f, "Error updating account counter"),
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
