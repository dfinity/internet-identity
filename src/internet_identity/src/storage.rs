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
use candid::{CandidType, Deserialize, Principal};
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
use crate::storage::storable::anchor_application_config::AnchorApplicationConfig;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::application_number::StorableApplicationNumber;
use internet_identity_interface::internet_identity::types::*;
use storable::anchor::StorableAnchor;
use storable::anchor_number::StorableAnchorNumber;
use storable::application::StorableApplication;
use storable::credential_id::StorableCredentialId;
use storable::discrepancy_counter::{DiscrepancyType, StorableDiscrepancyCounter};
use storable::fixed_anchor::StorableFixedAnchor;
use storable::openid_credential::StorableOpenIdCredential;
use storable::openid_credential_key::StorableOpenIdCredentialKey;
use storable::storable_persistent_state::StorablePersistentState;

pub mod anchor;
pub mod registration_rates;

pub mod account;

pub(crate) mod storable;

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
// const LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_INDEX_OLD: u8 = 12u8;

const LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX: u8 = 9u8;
const STABLE_ACCOUNT_MEMORY_INDEX: u8 = 10u8;
const STABLE_APPLICATION_MEMORY_INDEX: u8 = 11u8;
const STABLE_ACCOUNT_REFERENCE_LIST_MEMORY_INDEX: u8 = 13u8;
const STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_INDEX: u8 = 14u8;
const STABLE_ACCOUNT_COUNTER_MEMORY_INDEX: u8 = 15u8;
const STABLE_ANCHOR_MEMORY_INDEX: u8 = 16u8;
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX: u8 = 17u8;
const STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_INDEX: u8 = 18u8;
const LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_INDEX: u8 = 19u8;
const STABLE_ANCHOR_APPLICATION_CONFIG_MEMORY_INDEX: u8 = 20u8;
const LOOKUP_ANCHOR_WITH_RECOVERY_PHRASE_PRINCIPAL_MEMORY_INDEX: u8 = 21u8;

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
const STABLE_DEFAULT_ACCOUNT_REFERENCE_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ANCHOR_APPLICATION_CONFIG_MEMORY_INDEX);
const STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_INDEX);
const STABLE_ACCOUNT_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ACCOUNT_COUNTER_MEMORY_INDEX);
const STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_ID: MemoryId =
    MemoryId::new(STABLE_ANCHOR_ACCOUNT_COUNTER_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_INDEX);
const LOOKUP_ANCHOR_WITH_PASSKEY_CREDENTIAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_DEVICE_CREDENTIAL_MEMORY_INDEX);

const LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_INDEX);

const LOOKUP_ANCHOR_WITH_RECOVERY_PHRASE_PRINCIPAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_RECOVERY_PHRASE_PRINCIPAL_MEMORY_INDEX);

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
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(candid::encode_one(&self.0).expect("failed to serialize archive entry"))
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
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
    pub(crate) stable_application_memory:
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
    /// Memory wrapper used to report the size of the stable (anchor, application)-config memory.
    stable_anchor_application_config_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    stable_anchor_application_config_memory: StableBTreeMap<
        (StorableAnchorNumber, StorableApplicationNumber),
        AnchorApplicationConfig,
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
    lookup_anchor_with_passkey_credential_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_anchor_with_passkey_credential_memory:
        StableBTreeMap<StorableCredentialId, StorableAnchorNumber, ManagedMemory<M>>,

    lookup_application_with_origin_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,

    pub(crate) lookup_application_with_origin_memory:
        StableBTreeMap<StorableOriginSha256, StorableApplicationNumber, ManagedMemory<M>>,

    lookup_anchor_with_recovery_phrase_principal_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    pub(crate) lookup_anchor_with_recovery_phrase_principal_memory:
        StableBTreeMap<Principal, StorableAnchorNumber, ManagedMemory<M>>,
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
        let stable_default_account_reference_memory =
            memory_manager.get(STABLE_DEFAULT_ACCOUNT_REFERENCE_MEMORY_ID);
        let stable_account_counter_memory = memory_manager.get(STABLE_ACCOUNT_COUNTER_MEMORY_ID);
        let stable_account_counter_discrepancy_counter_memory =
            memory_manager.get(STABLE_ACCOUNT_COUNTER_DISCREPANCY_COUNTER_MEMORY_ID);
        let lookup_anchor_with_openid_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_OPENID_CREDENTIAL_MEMORY_ID);
        let lookup_anchor_with_passkey_credential_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_PASSKEY_CREDENTIAL_MEMORY_ID);
        let lookup_application_with_origin_memory =
            memory_manager.get(LOOKUP_APPLICATION_WITH_ORIGIN_MEMORY_ID);
        let lookup_anchor_with_recovery_phrase_principal_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_RECOVERY_PHRASE_PRINCIPAL_MEMORY_ID);

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
            stable_anchor_application_config_memory_wrapper: MemoryWrapper::new(
                stable_default_account_reference_memory.clone(),
            ),
            stable_anchor_application_config_memory: StableBTreeMap::init(
                stable_default_account_reference_memory,
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
            lookup_anchor_with_passkey_credential_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_passkey_credential_memory.clone(),
            ),
            lookup_anchor_with_passkey_credential_memory: StableBTreeMap::init(
                lookup_anchor_with_passkey_credential_memory,
            ),

            lookup_application_with_origin_memory_wrapper: MemoryWrapper::new(
                lookup_application_with_origin_memory.clone(),
            ),
            lookup_application_with_origin_memory: StableBTreeMap::init(
                lookup_application_with_origin_memory,
            ),

            lookup_anchor_with_recovery_phrase_principal_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_recovery_phrase_principal_memory.clone(),
            ),
            lookup_anchor_with_recovery_phrase_principal_memory: StableBTreeMap::init(
                lookup_anchor_with_recovery_phrase_principal_memory,
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
    pub fn allocate_anchor(&mut self, now: Timestamp) -> Option<Anchor> {
        let anchor_number = self.header.id_range_lo + self.header.num_anchors as u64;
        if anchor_number >= self.header.id_range_hi {
            return None;
        }
        self.header.num_anchors += 1;
        self.flush();

        Some(Anchor::new(anchor_number, now))
    }

    /// Runs `f` over a new identity, allocating that identity in stable memory if `f` succeeds.
    ///
    /// Returns a `StorageError::AnchorNumberOutOfRange` error (converted to `E`) if the range
    /// of Identity Anchor assigned to this storage is exhausted, in which case `f` is not called
    /// and no state is modified.
    pub fn allocate_anchor_safe<F, T, E>(&mut self, now: Timestamp, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut Anchor) -> Result<T, E>,
        E: From<StorageError>,
    {
        let num_anchors = u64::from(self.header.num_anchors);

        let (id_range_lo, id_range_hi) = (self.header.id_range_lo, self.header.id_range_hi);

        let anchor_number = id_range_lo.saturating_add(num_anchors);

        let identity = if anchor_number >= id_range_hi {
            None
        } else {
            Some(Anchor::new(anchor_number, now))
        };

        let Some(mut identity) = identity else {
            return Err(StorageError::AnchorNumberOutOfRange {
                anchor_number,
                range: (id_range_lo, id_range_hi),
            }
            .into());
        };

        let result = f(&mut identity)?;

        self.create(identity).map_err(E::from)?;

        // Important! Only increment num_anchors after the anchor creation succeeds.
        self.header.num_anchors = self.header.num_anchors.saturating_add(1);
        self.flush();

        Ok(result)
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
    ///
    /// *TODO*: remove `pub(crate)` after the `sync_anchor_indices` migration. Do **NOT**,
    /// under _any_ circumstances, use this method directly. Use `create` or `update` instead.
    pub(crate) fn write(
        &mut self,
        data: Anchor,
        is_previously_written: bool,
    ) -> Result<(), StorageError> {
        let anchor_number = data.anchor_number();
        let (storable_fixed_anchor, storable_anchor): (StorableFixedAnchor, StorableAnchor) =
            data.into();

        // Get anchor address
        let record_number = self.anchor_number_to_record_number(anchor_number)?;

        let num_anchors = self.header.num_anchors;

        // Strict inequality allows for calling this function before allocating an anchor,
        // which is a safer way to create new anchors.
        // TODO: switch this condition to `!is_previously_written ==> record_number != num_anchors`.
        if record_number > num_anchors {
            ic_cdk::println!(
                "ERROR: Tried to write anchor number {} which maps to record number {}, \
                 but only {} anchors are allocated.",
                anchor_number,
                record_number,
                num_anchors,
            );
            return Err(StorageError::BadAnchorNumber(anchor_number));
        }

        let address = self.record_address(record_number);

        // Read previous fixed 4KB stable memory anchor (this is used only for synching device indices)
        let previous_devices = if is_previously_written {
            let mut reader = Reader::new(&self.anchor_memory, address);
            let mut read_buf = vec![0; self.header.entry_size as usize];
            reader
                .read_exact(&mut read_buf)
                .expect("failed to read memory");
            let anchor = StorableFixedAnchor::from_bytes(Cow::Owned(read_buf));
            anchor.devices
        } else {
            vec![]
        };

        // Write current fixed 4KB stable memory anchor
        {
            let write_buf = storable_fixed_anchor.to_bytes();
            if write_buf.len() > self.header.entry_size as usize {
                return Err(StorageError::EntrySizeLimitExceeded {
                    space_required: write_buf.len() as u64,
                    space_available: self.header.entry_size as u64,
                });
            }
            let mut writer = Writer::new(&mut self.anchor_memory, address);
            writer.write_all(&write_buf).expect("memory write failed");
            writer.flush().expect("memory write failed");
        }

        // If there was an anchor stored previously, we need to take its credentials and recovery keys into account
        // while synchronizing the respective indices.
        //
        // First, read the previous anchor and store the new anchor as-is in its place.
        let previous_anchor_maybe = self
            .stable_anchor_memory
            .insert(anchor_number, storable_anchor.clone());

        // Second, deconstruct the previous anchor, obtaining the previous credentials and recovery keys.
        let (previous_openid_credentials, _previous_passkey_credentials, _previous_recovery_keys) =
            if let Some(StorableAnchor {
                // The following fields need to be compared with the previous anchor
                openid_credentials,
                passkey_credentials,
                recovery_keys,

                // The following fields do not require merging.
                created_at_ns: _,
                name: _,
            }) = previous_anchor_maybe
            {
                (
                    openid_credentials,
                    passkey_credentials.unwrap_or_default(),
                    recovery_keys.unwrap_or_default(),
                )
            } else {
                // Should never happen in practice, since each anchor number should correspond to a `StorableAnchor`.
                (vec![], vec![], vec![])
            };

        // Right now, this is the only index that needs to be updated based on `StorableAnchor`.
        self.sync_anchor_with_openid_credential_index(
            anchor_number,
            previous_openid_credentials,
            storable_anchor.openid_credentials,
        );

        // Sync device-based indices with the legacy source of truth (`StorableFixedAnchor`).
        //
        // TODO: After all anchors are migrated to `StableAnchor`, switch the source of truth to it.
        //
        // Update `CredentialId` to `AnchorNumber` lookup map

        let current_devices = storable_fixed_anchor.devices;

        self.sync_anchor_with_recovery_phrase_principal_index(
            anchor_number,
            &previous_devices,
            &current_devices,
        );
        self.sync_anchors_with_passkey_credential_index(
            anchor_number,
            previous_devices,
            current_devices,
        );

        Ok(())
    }

    /// Reads the data of the specified anchor from legacy stable memory.
    fn read_legacy(&self, anchor_number: AnchorNumber) -> Result<Anchor, StorageError> {
        // Read fixed 4KB anchor
        let record_number = self.anchor_number_to_record_number(anchor_number)?;

        let address = self.record_address(record_number);

        let mut reader = Reader::new(&self.anchor_memory, address);
        let mut buf = vec![0; self.header.entry_size as usize];

        reader.read_exact(&mut buf).expect("failed to read memory");

        // Anchors that are allocated but have never been written to, due to a previously missing
        // allocation cleanup implementation, are handled as if they don't exist.
        if buf.iter().all(|&b| b == 0) {
            return Err(StorageError::AnchorNotFound { anchor_number });
        }

        // Read unbounded stable structures anchor
        let storable_fixed_anchor = StorableFixedAnchor::from_bytes(Cow::Owned(buf));

        Ok(Anchor::from((anchor_number, storable_fixed_anchor, None)))
    }

    /// Reads the data of the specified anchor from stable memory.
    pub fn read(&self, anchor_number: AnchorNumber) -> Result<Anchor, StorageError> {
        let record_number = self.anchor_number_to_record_number(anchor_number)?;

        // This value is no longer used for reading, but we keep the check for consistency.
        let num_anchors = self.header.num_anchors;

        if record_number >= num_anchors {
            ic_cdk::println!(
                "ERROR: Requested anchor number {} maps to record number {}, but only {} anchors \
                 are allocated.",
                anchor_number,
                record_number,
                num_anchors,
            );
            return Err(StorageError::BadAnchorNumber(anchor_number));
        }

        let Some(storable_anchor) = self.stable_anchor_memory.get(&anchor_number) else {
            ic_cdk::println!(
                "Falling back to legacy read for anchor number {}",
                anchor_number
            );
            return self.read_legacy(anchor_number);
        };

        Ok(Anchor::from((anchor_number, storable_anchor)))
    }

    /// Update `OpenIdCredential` to `Vec<AnchorNumber>` lookup map
    fn sync_anchor_with_openid_credential_index(
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

    pub fn lookup_anchor_with_recovery_phrase_principal(
        &self,
        key: Principal,
    ) -> Option<AnchorNumber> {
        self.lookup_anchor_with_recovery_phrase_principal_memory
            .get(&key)
    }

    fn sync_anchor_with_recovery_phrase_principal_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous_devices: &[Device],
        current_devices: &[Device],
    ) {
        let retain_recovery_phrase_device_principals = |device: &Device| {
            if !device.is_recovery_phrase() {
                // lookup_anchor_with_recovery_phrase_principal_memory is not affected by this device.
                return None;
            };
            Some(Principal::self_authenticating(&device.pubkey))
        };

        let previous_recovery_phrases = previous_devices
            .iter()
            .filter_map(retain_recovery_phrase_device_principals)
            .collect::<BTreeSet<_>>();

        let current_recovery_phrases = current_devices
            .iter()
            .filter_map(retain_recovery_phrase_device_principals)
            .collect::<BTreeSet<_>>();

        for key in previous_recovery_phrases.difference(&current_recovery_phrases) {
            let Some(existing_anchor_number) = self
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(key)
            else {
                // This principal is not indexed, nothing to do.
                continue;
            };
            if existing_anchor_number != anchor_number {
                // Ensure that a user can remove only their own recovery phrase device from the index.
                continue;
            }
            self.lookup_anchor_with_recovery_phrase_principal_memory
                .remove(key);
        }

        for key in current_recovery_phrases {
            if self
                .lookup_anchor_with_recovery_phrase_principal_memory
                .contains_key(&key)
            {
                // This principal is already occupied; do not overwrite it.
                continue;
            };

            self.lookup_anchor_with_recovery_phrase_principal_memory
                .insert(key, anchor_number);
        }
    }

    /// Update `CredentialId` to `AnchorNumber` lookup map
    fn sync_anchors_with_passkey_credential_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous: Vec<Device>,
        current: Vec<Device>,
    ) {
        let previous_credential_ids: BTreeSet<CredentialId> = previous
            .into_iter()
            .filter_map(|device| device.credential_id)
            .collect();

        let current_credential_ids: BTreeSet<CredentialId> = current
            .into_iter()
            .filter_map(|device| device.credential_id)
            .collect();

        for credential_id in previous_credential_ids.difference(&current_credential_ids) {
            let credential_id = StorableCredentialId::from(credential_id.clone());

            let Some(indexed_anchor_number) = self
                .lookup_anchor_with_passkey_credential_memory
                .get(&credential_id)
            else {
                continue;
            };

            // Only remove if the credential is assigned to *this* anchor.
            if indexed_anchor_number != anchor_number {
                continue;
            }

            self.lookup_anchor_with_passkey_credential_memory
                .remove(&credential_id);
        }

        for credential_id in current_credential_ids {
            let credential_id = StorableCredentialId::from(credential_id);

            // Only insert if the credential id isn't yet assigned to an anchor.
            if self
                .lookup_anchor_with_passkey_credential_memory
                .contains_key(&credential_id)
            {
                continue;
            }

            self.lookup_anchor_with_passkey_credential_memory
                .insert(credential_id, anchor_number);
        }
    }

    #[allow(dead_code)]
    pub fn lookup_anchor_with_device_credential(&self, key: &CredentialId) -> Option<AnchorNumber> {
        self.lookup_anchor_with_passkey_credential_memory
            .get(&key.clone().into())
    }

    /// Look up an application number per origin, create entry in applications and lookup table if it doesn't exist
    pub fn lookup_or_insert_application_number_with_origin(
        &mut self,
        origin: &FrontendHostname,
    ) -> ApplicationNumber {
        let origin_sha256 = StorableOriginSha256::from_origin(origin);

        if let Some(existing_number) = self
            .lookup_application_with_origin_memory
            .get(&origin_sha256)
        {
            existing_number
        } else {
            let new_number: ApplicationNumber = self.lookup_application_with_origin_memory.len();

            // Update the source of truth.
            self.lookup_application_with_origin_memory
                .insert(origin_sha256, new_number);

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
            .get(&StorableOriginSha256::from_origin(origin))
    }

    /// Only used in tests.
    // TODO: mark this code as test-only or adjust the tests to avoid using this functions.
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

    fn find_account_references(
        &self,
        anchor_number: AnchorNumber,
        application_number: Option<ApplicationNumber>,
    ) -> Option<(
        (AnchorNumber, ApplicationNumber),
        Vec<StorableAccountReference>,
    )> {
        let application_number = application_number?;

        let key = (anchor_number, application_number);

        let account_references = self.stable_account_reference_list_memory.get(&key)?;

        Some((key, account_references.into_vec()))
    }

    fn find_account_reference(
        &self,
        anchor_number: AnchorNumber,
        application_number: Option<ApplicationNumber>,
        account_number: Option<AccountNumber>,
    ) -> Option<StorableAccountReference> {
        let (_, account_references) =
            self.find_account_references(anchor_number, application_number)?;

        account_references
            .into_iter()
            .find(|account_reference| account_reference.account_number == account_number)
    }

    /// Search for an account and account_reference and applies the function `f` if found.
    ///
    ///
    /// The function `f` is called with a mutable reference to the account reference and an option to the mutable reference to the account.
    ///
    /// # Arguments
    ///
    /// * `anchor_number` - The anchor number of the account.
    /// * `application_number` - The application number of the account.
    /// * `account_number` - The account number of the account or None if synthetic account.
    /// * `f` - The function to apply to the accounts.
    ///
    /// If the `account_number` is None, it means the storable account doesn't exist and account reference might exist.
    /// * If the account reference exists, the function `f` is called with a mutable reference to the account reference and None as the second argument.
    /// * If the account reference does not exist, None is returned.
    ///
    /// If the `account_number` is Some, it means the storable account exists (or existed at some point) and account references exists (or existed at some point).
    /// * If the storable account exists, the function `f` is called with a mutable reference to the account reference and a mutable reference to the storable account.
    /// * If the storable account does not exist, None is returned.
    ///
    /// # Returns
    ///
    /// * `None` if both account and account_reference are not found
    /// * `Some(T)` if the account or account_reference are found where T is the result of the function `f`.
    fn with_account_mut<T, F>(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: Option<ApplicationNumber>,
        maybe_account_number: Option<AccountNumber>,
        f: F,
    ) -> Option<T>
    where
        F: FnOnce(&mut StorableAccountReference, Option<&mut StorableAccount>) -> T,
    {
        match maybe_account_number {
            None => {
                // We are looking for a synthetic account
                let (key, mut account_references) =
                    self.find_account_references(anchor_number, application_number)?;

                let mut result = None;

                for account_reference in &mut account_references {
                    if account_reference.account_number == maybe_account_number {
                        result = Some(f(account_reference, None));
                        break;
                    }
                }

                let value = StorableAccountReferenceList::from_vec(account_references);

                self.stable_account_reference_list_memory.insert(key, value);

                result
            }
            Some(account_number) => {
                // Account should be stored, otherwise, it was removed and we'll return `None`.
                let mut storable_account = self.stable_account_memory.get(&account_number)?;
                let (key, mut account_references) =
                    self.find_account_references(anchor_number, application_number)?;

                let mut result = None;

                for account_reference in &mut account_references {
                    if account_reference.account_number == maybe_account_number {
                        result = Some(f(account_reference, Some(&mut storable_account)));
                        break;
                    }
                }

                let value = StorableAccountReferenceList::from_vec(account_references);

                self.stable_account_reference_list_memory.insert(key, value);
                self.stable_account_memory
                    .insert(account_number, storable_account);

                result
            }
        }
    }

    pub fn set_account_last_used(
        &mut self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        account_number: Option<AccountNumber>,
        now: Timestamp,
    ) -> Option<()> {
        let application_number = self.lookup_application_number_with_origin(&origin);

        self.with_account_mut(
            anchor_number,
            application_number,
            account_number,
            |account_reference, _| {
                account_reference.last_used = Some(now);
            },
        )
    }

    pub fn lookup_anchor_application_config(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> AnchorApplicationConfig {
        if let Some(config) = self
            .stable_anchor_application_config_memory
            .get(&(anchor_number, application_number))
        {
            return config;
        }

        AnchorApplicationConfig::default()
    }

    pub fn set_anchor_application_config(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        anchor_application_config: AnchorApplicationConfig,
    ) {
        self.stable_anchor_application_config_memory.insert(
            (anchor_number, application_number),
            anchor_application_config,
        );
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

    /// Creates an account for that identity.
    /// If the identity doesn't yet have accounts, it will create the account reference for the synthetic account.
    /// But not a storable account for the synthetic one.
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

        // last_used will be set once the user signs in with the account.
        let last_used = None;

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
                    last_used,
                };
                let default_account_reference = AccountReference {
                    account_number: None,
                    last_used,
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
                    last_used,
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
            None => vec![Account::synthetic(anchor_number, origin.clone())],
            Some(app_num) => match self.lookup_account_references(anchor_number, app_num) {
                None => vec![Account::synthetic(anchor_number, origin.clone())],
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
        let UpdateAccountParams {
            account_number,
            anchor_number,
            name,
            origin,
        } = params;

        check_frontend_length(&origin);
        match account_number {
            Some(account_number) => self.update_existing_account(UpdateExistingAccountParams {
                account_number,
                anchor_number,
                name,
                origin,
            }),
            None => {
                // Default accounts are not stored by default.
                // They are created only once they are updated.
                self.create_default_account(CreateAccountParams {
                    anchor_number,
                    name,
                    origin,
                })
            }
        }
    }

    /// Used in `update_account` to update an existing account.
    fn update_existing_account(
        &mut self,
        params: UpdateExistingAccountParams,
    ) -> Result<Account, StorageError> {
        let UpdateExistingAccountParams {
            account_number,
            anchor_number,
            name,
            origin,
        } = params;

        // Check if account reference exists for given anchor number, origin and account number,
        // if the account refence exists for a given anchor, that means the anchor has access.
        let application_number = self.lookup_application_number_with_origin(&origin);

        let account_update_result = self.with_account_mut(
            anchor_number,
            application_number,
            Some(account_number),
            |account_reference, maybe_storable_account| {
                // Check if the account reference has an account number,
                // throw error if it doesn't since we only want to update
                // accounts with an account number in this function.
                let account_number = account_reference.account_number?;
                // Check if the storable_account exists.
                // throw error if it doesn't since we only want to update
                // existing accounts in this function.
                let storable_account = maybe_storable_account?;

                // Update account and write back to storage
                storable_account.name = name.clone();

                // Return a user-facing account structure
                Some(Account::new_full(
                    anchor_number,
                    origin,
                    Some(name),
                    Some(account_number),
                    account_reference.last_used,
                    storable_account.seed_from_anchor,
                ))
            },
        );

        let Some(Some(account_update_result)) = account_update_result else {
            return Err(StorageError::AccountNotFound { account_number });
        };

        Ok(account_update_result)
    }

    /// Used in `update_account` to create a default account.
    /// Default account are not initially stored. They are stored when updated.
    /// If the default account reference does not exist, it must be created.
    /// If the default account reference exists, its account number must be updated.
    fn create_default_account(
        &mut self,
        params: CreateAccountParams,
    ) -> Result<Account, StorageError> {
        let CreateAccountParams {
            anchor_number,
            name,
            origin,
        } = params;

        // Create and store the default account.
        let new_account_number = self.allocate_account_number()?;
        let storable_account = StorableAccount {
            name: name.clone(),
            // This was a default account which uses the anchor number for the seed.
            seed_from_anchor: Some(anchor_number),
        };
        self.stable_account_memory
            .insert(new_account_number, storable_account.clone());

        // Get or create an application number from the account's origin.
        let application_number = self.lookup_or_insert_application_number_with_origin(&origin);

        // Update default account in the (anchor, origin) config.
        {
            let mut config =
                self.lookup_anchor_application_config(anchor_number, application_number);

            config.default_account_number = Some(new_account_number);

            self.set_anchor_application_config(anchor_number, application_number, config);
        }

        // Update counters with one more account.
        self.update_counters(application_number, anchor_number, AccountType::Account)?;

        let account_references_key = (anchor_number, application_number);
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
                    // The `last_used` field will be set when the user signs with this account.
                    last_used: None,
                };
                self.stable_account_reference_list_memory
                    .insert(account_references_key, vec![new_ref].into());
                // One new account reference was created.
                self.update_counters(
                    application_number,
                    anchor_number,
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
                        anchor_number,
                        name: name.clone(),
                    });
                }
                self.stable_account_reference_list_memory
                    .insert(account_references_key, refs_vec.into());
            }
        }

        // Return created default account
        Ok(Account::new_full(
            anchor_number,
            origin,
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
                     as the existing range thus would make existing anchors invalid",
                    { self.header.id_range_lo }
                ));
            }
            // Check that all _existing_ anchors fit into the new range. I.e. making the range smaller
            // is ok as long as the range reduction only affects _unused_ anchor number.
            if (hi - lo) < self.header.num_anchors as u64 {
                trap(&format!(
                    "set_anchor_number_range: specified range [{lo}, {hi}) does not accommodate all {} anchors \
                     thus would make existing anchors invalid",
                    { self.header.num_anchors }
                ));
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

    fn anchor_number_to_record_number(&self, anchor_number: u64) -> Result<u32, StorageError> {
        if anchor_number < self.header.id_range_lo || anchor_number >= self.header.id_range_hi {
            return Err(StorageError::AnchorNumberOutOfRange {
                anchor_number,
                range: self.assigned_anchor_number_range(),
            });
        }

        let record_number = (anchor_number - self.header.id_range_lo) as u32;

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
                self.lookup_anchor_with_passkey_credential_memory_wrapper
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
            (
                "stable_anchor_application_config".to_string(),
                self.stable_anchor_application_config_memory_wrapper.size(),
            ),
            (
                "lookup_anchor_with_recovery_phrase_principal_memory".to_string(),
                self.lookup_anchor_with_recovery_phrase_principal_memory_wrapper
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
                    "StorableAnchor not found for anchor number {anchor_number}",
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

impl From<StorageError> for IdRegFinishError {
    fn from(err: StorageError) -> Self {
        IdRegFinishError::StorageError(err.to_string())
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

#[cfg(test)]
mod allocate_anchor_safe_tests {
    use super::*;
    use ic_stable_structures::DefaultMemoryImpl;

    #[derive(Debug)]
    enum TestError {
        Err(String),
        StorageErr(StorageError),
    }

    impl PartialEq for TestError {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (TestError::Err(s1), TestError::Err(s2)) => s1 == s2,
                (TestError::StorageErr(e1), TestError::StorageErr(e2)) => {
                    format!("{}", e1) == format!("{}", e2)
                }
                _ => false,
            }
        }
    }

    impl From<&str> for TestError {
        fn from(err: &str) -> Self {
            TestError::Err(err.to_string())
        }
    }

    impl From<String> for TestError {
        fn from(err: String) -> Self {
            TestError::Err(err)
        }
    }

    impl From<StorageError> for TestError {
        fn from(err: StorageError) -> Self {
            TestError::StorageErr(err)
        }
    }

    #[test]
    fn allocate_anchor_safe_runs_multiple_scenarios() {
        #[allow(clippy::type_complexity)]
        let test_cases: Vec<(
            &str,
            (u64, u64),
            usize,
            Box<dyn Fn(&mut Anchor) -> Result<String, TestError>>,
            Result<String, TestError>,
            usize,
        )> = vec![
            (
                "success case",
                (10000, 20000),
                0,
                Box::new(|a| Ok(format!("Anchor {}", a.anchor_number()))),
                Ok("Anchor 10000".to_string()),
                1,
            ),
            (
                "failure case with error",
                (10000, 20000),
                0,
                Box::new(|_| Err(TestError::Err("Intentional failure".to_string()))),
                Err(TestError::Err("Intentional failure".to_string())),
                0,
            ),
            (
                "success case that ignores anchor",
                (10000, 20000),
                0,
                Box::new(|_| Ok("Success without using anchor".to_string())),
                Ok("Success without using anchor".to_string()),
                1,
            ),
            (
                "allocation is safe at range limit",
                (10000, 10001),
                0,
                Box::new(|a| {
                    let anchor_number = a.anchor_number();
                    if anchor_number == 10000 {
                        Ok("Allocated at range limit".to_string())
                    } else {
                        Err(TestError::Err(format!(
                            "Allocated wrong anchor number {}",
                            anchor_number
                        )))
                    }
                }),
                Ok("Allocated at range limit".to_string()),
                1,
            ),
            (
                "exhausted range case (f errors out)",
                (10000, 10000),
                0,
                Box::new(|_| Err("Expected no anchor due to exhausted range".into())),
                Err(TestError::StorageErr(
                    StorageError::AnchorNumberOutOfRange {
                        anchor_number: 10000,
                        range: (10000, 10000),
                    },
                )),
                0,
            ),
            (
                "exhausted range case (f returns ok)",
                (10000, 10000),
                0,
                Box::new(|_| Ok("Expected no anchor due to exhausted range".to_string())),
                Err(TestError::StorageErr(
                    StorageError::AnchorNumberOutOfRange {
                        anchor_number: 10000,
                        range: (10000, 10000),
                    },
                )),
                0,
            ),
            (
                "no overflow at u64::MAX - 1",
                (u64::MAX - 1, u64::MAX),
                0,
                Box::new(|a| Ok(format!("Anchor {}", a.anchor_number()))),
                Ok(format!("Anchor {}", u64::MAX - 1)),
                1,
            ),
            (
                "overflow at u64::MAX",
                (u64::MAX - 1, u64::MAX),
                1,
                Box::new(|a| {
                    Err(TestError::Err(format!(
                        "Expected no anchor due to exhausted range, but got anchor {}",
                        a.anchor_number()
                    )))
                }),
                Err(TestError::StorageErr(
                    StorageError::AnchorNumberOutOfRange {
                        anchor_number: u64::MAX,
                        range: (u64::MAX - 1, u64::MAX),
                    },
                )),
                1,
            ),
        ];

        let now = 123456789;

        for (label, (id_range_lo, id_range_hi), initial_count, f, expected, expected_count) in
            test_cases
        {
            let mut storage =
                Storage::new((id_range_lo, id_range_hi), DefaultMemoryImpl::default());

            storage.header.num_anchors = initial_count as u32;

            let result = storage.allocate_anchor_safe(now, f);

            assert_eq!(
                result, expected,
                "Test case '{}' failed: result mismatch",
                label
            );

            let final_count = storage.anchor_count();

            assert_eq!(
                final_count, expected_count,
                "Test case '{}' failed: anchor count observed {} but expected {}",
                label, final_count, expected_count
            );
        }
    }
}
