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
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::io::Write;
use std::ops::RangeInclusive;
use storable::account_reference_list::StorableAccountReferenceList;
use storable::anchor_number_list::StorableAnchorNumberList;

use ic_cdk::api::trap;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::writer::Writer;
use ic_stable_structures::{
    Memory, MinHeap, RestrictedMemory, StableBTreeMap, StableCell, Storable,
};
use identity_jose::jwk::Jwk;
use internet_identity_interface::archive::types::BufferedEntry;

use crate::delegation::{self, check_frontend_length};
use crate::delegation::{calculate_session_seed_with_salt, canister_sig_principal};
use crate::openid::OpenIdCredentialKey;
use crate::state::PersistentState;
use crate::stats::event_stats::AggregationKey;
use crate::stats::event_stats::{EventData, EventKey};
use crate::storage::account::{AccountReference, SessionRecord};
use crate::storage::anchor::Anchor;
use crate::storage::memory_wrapper::MemoryWrapper;
use crate::storage::registration_rates::RegistrationRates;
use crate::storage::storable::account::StorableAccount;
use crate::storage::storable::account_locator::StorableAccountLocator;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::account_reference::StorableAccountReference;
use crate::storage::storable::accounts_counter::StorableAccountsCounter;
use crate::storage::storable::anchor_application_config::AnchorApplicationConfig;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
use crate::storage::storable::recovery_key::StorableRecoveryKey;
use crate::storage::storable::session_handle::StorableSessionHandle;
use internet_identity_interface::internet_identity::types::*;
use storable::anchor::StorableAnchor;
use storable::anchor_number::StorableAnchorNumber;
use storable::application::StorableApplication;
use storable::credential_id::StorableCredentialId;
use storable::discrepancy_counter::{DiscrepancyType, StorableDiscrepancyCounter};
use storable::email_recovery_address_hash::StorableEmailRecoveryAddressHash;
use storable::fixed_anchor::StorableFixedAnchor;
use storable::mcp_config::StorableMcpConfig;
use storable::mcp_grant::StorableMcpGrant;
use storable::mcp_registration::StorableMcpRegistration;
use storable::openid_credential::StorableOpenIdCredential;
use storable::openid_credential_key::StorableOpenIdCredentialKey;
use storable::openid_jwks::StorableJwks;
use storable::sso_stable_id_key::StorableSsoStableIdKey;
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
// (The abandoned MCP indexes 25/27/28 are documented with the current
// MCP_GRANT_MEMORY_INDEX below.)

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
const LOOKUP_ANCHOR_WITH_PASSKEY_PUBKEY_HASH_MEMORY_INDEX: u8 = 22u8;
const LOOKUP_ANCHOR_WITH_EMAIL_RECOVERY_MEMORY_INDEX: u8 = 23u8;
const OPENID_JWKS_CACHE_MEMORY_INDEX: u8 = 24u8;
// Indexes 25, 27 and 28 held earlier MCP maps: a `Principal -> AnchorNumber`
// reverse index (25), a parallel read-only set (27), and a combined
// `Principal -> {anchor, read_only}` access map (28). All were superseded by
// the session-grant map at index 29 (`Principal -> {anchor, expiry, read_only}`,
// keyed by the MCP server's own session-key principal). MCP was preview-only,
// so the old regions are abandoned (any preview grants are dropped and
// re-created on the next connect) rather than migrated.
// const DEPRECATED_LOOKUP_ANCHOR_WITH_MCP_PRINCIPAL_MEMORY_INDEX: u8 = 25u8;
const MCP_CONFIG_MEMORY_INDEX: u8 = 26u8;
// const DEPRECATED_LOOKUP_MCP_PRINCIPAL_READ_ONLY_MEMORY_INDEX: u8 = 27u8;
// const DEPRECATED_MCP_ACCESS_MEMORY_INDEX: u8 = 28u8;
const MCP_GRANT_MEMORY_INDEX: u8 = 29u8;
// Index 30 held the first registration index, whose value stored the trusted
// server URL verbatim. It is abandoned in favour of index 31, whose value
// stores only a hash of that URL (see [`StorableMcpRegistration`]). Registration
// entries are short-lived preview state (re-created on the next connect), so the
// region is abandoned rather than migrated, matching the retired MCP indexes above.
// const DEPRECATED_MCP_REGISTRATION_URL_MEMORY_INDEX: u8 = 30u8;
const MCP_REGISTRATION_MEMORY_INDEX: u8 = 31u8;
const SSO_STABLE_ID_INDEX_MEMORY_INDEX: u8 = 32u8;
const NEXT_APPLICATION_NUMBER_MEMORY_INDEX: u8 = 33u8;
const LOOKUP_ACCOUNT_WITH_PRINCIPAL_MEMORY_INDEX: u8 = 34u8;
const LOOKUP_SESSION_WITH_PRINCIPAL_MEMORY_INDEX: u8 = 35u8;

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

const LOOKUP_ANCHOR_WITH_PASSKEY_PUBKEY_HASH_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_PASSKEY_PUBKEY_HASH_MEMORY_INDEX);

const LOOKUP_ANCHOR_WITH_EMAIL_RECOVERY_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ANCHOR_WITH_EMAIL_RECOVERY_MEMORY_INDEX);

/// Persistent cache of OpenID provider JWKs, keyed by the provider's `issuer`.
/// Seeded from `OpenIdConfig.seed_jwks` and written through on every successful
/// periodic `jwks_uri` fetch, so a provider's keys survive canister upgrades
/// and are available for JWT verification before the first post-upgrade fetch.
const OPENID_JWKS_CACHE_MEMORY_ID: MemoryId = MemoryId::new(OPENID_JWKS_CACHE_MEMORY_INDEX);

/// MCP session grants: maps an MCP server's session-key principal to the
/// grant ([`StorableMcpGrant`]: the anchor that registered it, the expiry, and
/// whether its per-app delegations are read-only). Written by the connect flow
/// (`mcp_register_v2`); the server-facing `mcp_*` methods authorize a caller
/// by looking up its grant here (and checking expiry), recovering the anchor
/// without an `anchor_number` parameter. Bounded at one entry per anchor via
/// [`StorableMcpConfig::session_principal`].
const MCP_GRANT_MEMORY_ID: MemoryId = MemoryId::new(MCP_GRANT_MEMORY_INDEX);

/// Pending MCP registration delegations ([`StorableMcpRegistration`]), keyed by
/// the registration principal `P_reg` (the `caller()` of `mcp_register_v2`).
/// Entries are minted by `prepare_mcp_registration_delegation` and store the
/// whole consent — the anchor to bind, the read-only choice, the resolved grant
/// TTL, and the trusted server URL — so `mcp_register_v2` recovers all of it
/// server-side instead of taking any as an argument (the MCP server passes only
/// its session key and never learns the anchor). `P_reg` is seeded from a fresh
/// random nonce, so the consent can't be re-derived and is kept here in full.
/// The delegation is multi-use within its short expiry (a retry re-binds);
/// entries are removed when a lookup finds them expired.
const MCP_REGISTRATION_MEMORY_ID: MemoryId = MemoryId::new(MCP_REGISTRATION_MEMORY_INDEX);

/// Per-anchor trusted-MCP-server configuration (master toggle + trusted server
/// URL), keyed by anchor number. Written by the authenticated `mcp_set_config`
/// method and read by the `/mcp` connect flow (verify-at-connect) and the
/// Settings UI; persisting it on-chain is what makes the config sync across all
/// of the identity's devices. Kept in its own map so it never touches anchor
/// serialization.
const MCP_CONFIG_MEMORY_ID: MemoryId = MemoryId::new(MCP_CONFIG_MEMORY_INDEX);

/// SSO stable-id bridge:
/// `SHA-256(sso_domain, iss, ii_client_id, stable_id) -> AnchorNumber`.
const SSO_STABLE_ID_INDEX_MEMORY_ID: MemoryId = MemoryId::new(SSO_STABLE_ID_INDEX_MEMORY_INDEX);

/// Monotonic `ApplicationNumber` allocator. A removed number is retired, never reissued.
const NEXT_APPLICATION_NUMBER_MEMORY_ID: MemoryId =
    MemoryId::new(NEXT_APPLICATION_NUMBER_MEMORY_INDEX);

/// Reverse index from the principal a dapp sees to the account that produced it:
/// `self_authenticating(der_encode_canister_sig_key(seed)) -> (anchor, application, account)`.
const LOOKUP_SESSION_WITH_PRINCIPAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_SESSION_WITH_PRINCIPAL_MEMORY_INDEX);
const LOOKUP_ACCOUNT_WITH_PRINCIPAL_MEMORY_ID: MemoryId =
    MemoryId::new(LOOKUP_ACCOUNT_WITH_PRINCIPAL_MEMORY_INDEX);

// The bucket size 128 is relatively low, to avoid wasting memory when using
// multiple virtual memories for smaller amounts of data.
// This value results in 256 GB of total managed memory, which should be enough
// for the foreseeable future.
const BUCKET_SIZE_IN_PAGES: u16 = 128;
const MAX_MANAGED_MEMORY_SIZE: u64 = 256 * GB;
const MAX_MANAGED_WASM_PAGES: u64 = MAX_MANAGED_MEMORY_SIZE / WASM_PAGE_SIZE_IN_BYTES;

/// Per-anchor cap on reference-list rows that hold nothing but a tracked default
/// account.
const MAX_EVICTABLE_DEFAULT_ACCOUNTS: u64 = 500;

/// Eviction target, below the cap.
const EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK: u64 = MAX_EVICTABLE_DEFAULT_ACCOUNTS * 9 / 10;

/// Session records one identity may hold, counted as stored rather than as live.
///
/// Counting what is stored is what makes the cap cheap to trigger on: a session expires with
/// no write anywhere, so no counter can follow the live set — something would have to
/// decrement at the moment of expiry, and nothing runs then. An expired record holds its slot
/// until something reclaims it, and because it is the first thing reclaimed, a held slot is
/// never taken from a session in use.
///
/// A bound on concurrent activity, not on history: every session expires within 30 days, so
/// the set is the apps used in the last month times the browsers they were used from.
pub const MAX_SESSIONS_PER_ANCHOR: u32 = 500;
/// Reclaiming goes down to here rather than to the cap, so the pass that walks an identity's
/// rows runs once and then not again for the next fifty sign-ins.
pub const SESSIONS_WATERMARK_PER_ANCHOR: u32 = 450;

/// Bounds one message's eviction work.
const MAX_EVICTIONS_PER_CALL: u64 =
    MAX_EVICTABLE_DEFAULT_ACCOUNTS - EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK;

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
    next_application_number_memory: StableCell<StorableApplicationNumber, ManagedMemory<M>>,
    lookup_account_with_principal_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_account_with_principal_memory:
        StableBTreeMap<Principal, StorableAccountLocator, ManagedMemory<M>>,
    /// Where a session lives, keyed by the principal its chain is rooted at. An app-facing
    /// call carries nothing but that principal, so this is what turns `caller()` into a
    /// session.
    lookup_session_with_principal_memory:
        StableBTreeMap<Principal, StorableSessionHandle, ManagedMemory<M>>,
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

    lookup_anchor_with_passkey_pubkey_hash_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    pub(crate) lookup_anchor_with_passkey_pubkey_hash_memory:
        StableBTreeMap<Principal, StorableAnchorNumber, ManagedMemory<M>>,

    lookup_anchor_with_email_recovery_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// Reverse index for the email-recovery flow: maps
    /// `SHA-256(lowercase(address))` to the anchor that bound it. The
    /// hash key is fixed-size (32 bytes) so the per-entry footprint
    /// is bounded regardless of address length; the address itself
    /// already lives on the anchor's `email_recovery` credential, so
    /// there's no need to store it again here. See design §8.2.
    pub(crate) lookup_anchor_with_email_recovery_memory:
        StableBTreeMap<StorableEmailRecoveryAddressHash, StorableAnchorNumber, ManagedMemory<M>>,

    mcp_grant_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// See [`MCP_GRANT_MEMORY_ID`].
    pub(crate) mcp_grant_memory: StableBTreeMap<Principal, StorableMcpGrant, ManagedMemory<M>>,

    mcp_registration_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// See [`MCP_REGISTRATION_MEMORY_ID`].
    pub(crate) mcp_registration_memory:
        StableBTreeMap<Principal, StorableMcpRegistration, ManagedMemory<M>>,

    /// Memory wrapper used to report the size of the OpenID JWKS cache memory.
    openid_jwks_cache_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// Persistent per-provider JWK cache, keyed by the provider's `issuer`.
    /// See [`OPENID_JWKS_CACHE_MEMORY_ID`].
    openid_jwks_cache_memory: StableBTreeMap<String, StorableJwks, ManagedMemory<M>>,

    mcp_config_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// Per-anchor trusted-MCP-server config. See [`MCP_CONFIG_MEMORY_ID`].
    mcp_config_memory: StableBTreeMap<StorableAnchorNumber, StorableMcpConfig, ManagedMemory<M>>,

    sso_stable_id_index_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    /// SSO stable-id lookup index:
    /// `SHA-256(sso_domain, iss, ii_client_id, stable_id) ->
    /// AnchorNumber`. Storage-maintained — [`Storage::write`] reconciles it
    /// from the anchors' stored OpenID credentials on every write, so it
    /// self-cleans when a credential is removed or moved. Mirrors
    /// [`Storage::lookup_anchor_with_openid_credential`]'s value type. See
    /// [`SSO_STABLE_ID_INDEX_MEMORY_ID`].
    sso_stable_id_index_memory:
        StableBTreeMap<StorableSsoStableIdKey, StorableAnchorNumberList, ManagedMemory<M>>,
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
        let next_application_number_memory = memory_manager.get(NEXT_APPLICATION_NUMBER_MEMORY_ID);
        let lookup_account_with_principal_memory =
            memory_manager.get(LOOKUP_ACCOUNT_WITH_PRINCIPAL_MEMORY_ID);
        let lookup_session_with_principal_memory =
            memory_manager.get(LOOKUP_SESSION_WITH_PRINCIPAL_MEMORY_ID);
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
        let lookup_anchor_with_passkey_pubkey_hash_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_PASSKEY_PUBKEY_HASH_MEMORY_ID);
        let lookup_anchor_with_email_recovery_memory =
            memory_manager.get(LOOKUP_ANCHOR_WITH_EMAIL_RECOVERY_MEMORY_ID);
        let mcp_grant_memory = memory_manager.get(MCP_GRANT_MEMORY_ID);
        let mcp_registration_memory = memory_manager.get(MCP_REGISTRATION_MEMORY_ID);
        let openid_jwks_cache_memory = memory_manager.get(OPENID_JWKS_CACHE_MEMORY_ID);
        let mcp_config_memory = memory_manager.get(MCP_CONFIG_MEMORY_ID);
        let sso_stable_id_index_memory = memory_manager.get(SSO_STABLE_ID_INDEX_MEMORY_ID);

        let registration_rates = RegistrationRates::new(
            MinHeap::init(registration_ref_rate_memory.clone())
                .expect("failed to initialize registration reference rate min heap"),
            MinHeap::init(registration_current_rate_memory.clone())
                .expect("failed to initialize registration current rate min heap"),
        );
        let mut storage = Self {
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
            next_application_number_memory: StableCell::init(next_application_number_memory, 0)
                .expect("next_application_number_memory"),
            lookup_account_with_principal_memory_wrapper: MemoryWrapper::new(
                lookup_account_with_principal_memory.clone(),
            ),
            lookup_session_with_principal_memory: StableBTreeMap::init(
                lookup_session_with_principal_memory,
            ),
            lookup_account_with_principal_memory: StableBTreeMap::init(
                lookup_account_with_principal_memory,
            ),
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
            lookup_anchor_with_passkey_pubkey_hash_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_passkey_pubkey_hash_memory.clone(),
            ),
            lookup_anchor_with_passkey_pubkey_hash_memory: StableBTreeMap::init(
                lookup_anchor_with_passkey_pubkey_hash_memory,
            ),
            lookup_anchor_with_email_recovery_memory_wrapper: MemoryWrapper::new(
                lookup_anchor_with_email_recovery_memory.clone(),
            ),
            lookup_anchor_with_email_recovery_memory: StableBTreeMap::init(
                lookup_anchor_with_email_recovery_memory,
            ),
            mcp_grant_memory_wrapper: MemoryWrapper::new(mcp_grant_memory.clone()),
            mcp_grant_memory: StableBTreeMap::init(mcp_grant_memory),
            mcp_registration_memory_wrapper: MemoryWrapper::new(mcp_registration_memory.clone()),
            mcp_registration_memory: StableBTreeMap::init(mcp_registration_memory),
            openid_jwks_cache_memory_wrapper: MemoryWrapper::new(openid_jwks_cache_memory.clone()),
            openid_jwks_cache_memory: StableBTreeMap::init(openid_jwks_cache_memory),
            mcp_config_memory_wrapper: MemoryWrapper::new(mcp_config_memory.clone()),
            mcp_config_memory: StableBTreeMap::init(mcp_config_memory),
            sso_stable_id_index_memory_wrapper: MemoryWrapper::new(
                sso_stable_id_index_memory.clone(),
            ),
            sso_stable_id_index_memory: StableBTreeMap::init(sso_stable_id_index_memory),
        };
        storage.seed_application_number_allocator();
        storage
    }

    /// Existing application numbers are dense from zero, so the row count is the
    /// first free number.
    fn seed_application_number_allocator(&mut self) {
        let seeded = ApplicationNumber::max(
            *self.next_application_number_memory.get(),
            self.stable_application_memory.len(),
        );
        self.next_application_number_memory
            .set(seeded)
            .expect("failed to seed the application number allocator");
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

        self.write(identity).map_err(E::from)?;

        // Important! Only increment num_anchors after the anchor creation succeeds.
        self.header.num_anchors = self.header.num_anchors.saturating_add(1);
        self.flush();

        Ok(result)
    }

    /// Writes the data of the specified anchor to stable memory.
    pub(crate) fn write(&mut self, data: Anchor) -> Result<(), StorageError> {
        let anchor_number = data.anchor_number();
        let (_, storable_anchor): (StorableFixedAnchor, StorableAnchor) = data.into();

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

        // If there was an anchor stored previously, we need to take its credentials and recovery keys into account
        // while synchronizing the respective indices.
        //
        // Pre-validate the email-recovery binding against the reverse
        // index *before* inserting the new anchor — if the address is
        // already bound to a different anchor, returning an error
        // after `stable_anchor_memory.insert` would leave the anchor
        // store and the reverse index inconsistent. Same-anchor
        // rebinds are idempotent and accepted here.
        if let Some(curr) = storable_anchor
            .email_recovery
            .as_ref()
            .and_then(|v| v.first())
            .map(|c| c.address.as_str())
        {
            if let Some(existing) = self.lookup_anchor_with_email_recovery_address(curr) {
                if existing != anchor_number {
                    return Err(StorageError::EmailRecoveryAddressAlreadyBound {
                        existing_anchor: existing,
                    });
                }
            }
        }

        // Read the previous anchor and store the new anchor as-is in its place.
        let previous_anchor_maybe = self
            .stable_anchor_memory
            .insert(anchor_number, storable_anchor.clone());

        // Second, deconstruct the previous anchor, obtaining the previous credentials and recovery keys.
        let (
            previous_openid_credentials,
            previous_passkey_credentials,
            previous_recovery_keys,
            previous_email_recovery,
        ) = if let Some(StorableAnchor {
            // The following fields need to be compared with the previous anchor
            openid_credentials,
            passkey_credentials,
            recovery_keys,
            email_recovery,

            // The following fields do not require merging.
            created_at_ns: _,
            name: _,
            verified_emails: _,
            session_devices: _,
            next_session_device_id: _,
            session_count: _,
        }) = previous_anchor_maybe
        {
            (
                openid_credentials,
                passkey_credentials.unwrap_or_default(),
                recovery_keys.unwrap_or_default(),
                email_recovery.unwrap_or_default(),
            )
        } else {
            // Should never happen in practice, since each anchor number should correspond to a `StorableAnchor`.
            (vec![], vec![], vec![], vec![])
        };
        // `storable_anchor.verified_emails` is not synced through any
        // reverse-lookup index — verified emails are addressable only
        // via the owning anchor.

        // The SSO stable-id index is derived from the same credentials; sync it
        // first (with clones) since the openid-credential sync below consumes
        // the vecs.
        self.sync_anchor_with_sso_stable_id_index(
            anchor_number,
            previous_openid_credentials.clone(),
            storable_anchor.openid_credentials.clone(),
        );
        self.sync_anchor_with_openid_credential_index(
            anchor_number,
            previous_openid_credentials,
            storable_anchor.openid_credentials,
        );
        self.sync_anchor_with_recovery_phrase_principal_index(
            anchor_number,
            &previous_recovery_keys,
            &storable_anchor.recovery_keys.unwrap_or_default(),
        );

        let current_passkey_credentials = storable_anchor.passkey_credentials.unwrap_or_default();

        self.sync_anchor_with_passkey_credential_index(
            anchor_number,
            &previous_passkey_credentials,
            &current_passkey_credentials,
        );
        self.sync_anchor_with_passkey_pubkey_index(
            anchor_number,
            &previous_passkey_credentials,
            &current_passkey_credentials,
        );

        // The reverse address index for email recovery: map
        // SHA-256(lowercase(address)) → AnchorNumber. Each anchor
        // holds at most one recovery email (the API caps it; the
        // storage Vec is ≤ 1 in practice). Sync prev → curr so
        // address swaps and removals stay consistent.
        let previous_email_address = previous_email_recovery.first().map(|c| c.address.clone());
        let current_email_address = storable_anchor
            .email_recovery
            .as_ref()
            .and_then(|v| v.first())
            .map(|c| c.address.clone());
        if previous_email_address != current_email_address {
            self.update_email_recovery_lookup(
                anchor_number,
                previous_email_address.as_deref(),
                current_email_address.as_deref(),
            )
            .map_err(|existing_anchor| {
                StorageError::EmailRecoveryAddressAlreadyBound { existing_anchor }
            })?;
        }

        Ok(())
    }

    /// Reads the data of the specified anchor from stable memory.
    pub fn read(&self, anchor_number: AnchorNumber) -> Result<Anchor, StorageError> {
        // These values are no longer used for reading, but we keep the check for consistency.
        let record_number = self.anchor_number_to_record_number(anchor_number)?;
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

        // Read unbounded stable structures anchor
        let storable_anchor = self.stable_anchor_memory.get(&anchor_number);

        let Some(storable_anchor) = storable_anchor else {
            ic_cdk::println!(
                "Anchor not found in stable_anchor_memory for anchor number {}",
                anchor_number
            );
            return Err(StorageError::AnchorNotFound { anchor_number });
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

    /// Reconcile the SSO stable-id index against `anchor_number`'s stored
    /// credentials. Mirrors [`Storage::sync_anchor_with_openid_credential_index`]:
    /// derive the `(sso_domain, iss, ii_client_id, stable_id)` keyset from each
    /// credential that carries a `stable_id`, diff previous vs current, and
    /// apply only the delta — `remove` the entries that disappeared, `insert`
    /// the new ones pointing at this anchor. Because the keyset is derived from
    /// the stored credentials, removing or moving an SSO credential removes or
    /// moves its index entry too; there are no orphans.
    fn sync_anchor_with_sso_stable_id_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous: Vec<StorableOpenIdCredential>,
        current: Vec<StorableOpenIdCredential>,
    ) {
        fn keys(credentials: Vec<StorableOpenIdCredential>) -> BTreeSet<StorableSsoStableIdKey> {
            credentials
                .into_iter()
                .filter_map(|cred| {
                    // Both are set together on an SSO non-`sub` credential; a
                    // `stable_id` without an `sso_domain` can't be domain-scoped,
                    // so it isn't indexed.
                    let stable_id = cred.stable_id?;
                    let sso_domain = cred.sso_domain?;
                    Some(StorableSsoStableIdKey::new(
                        &sso_domain,
                        &cred.iss,
                        &cred.aud,
                        &stable_id,
                    ))
                })
                .collect()
        }

        let previous_set = keys(previous);
        let current_set = keys(current);

        previous_set.difference(&current_set).for_each(|key| {
            self.sso_stable_id_index_memory.remove(key);
        });
        current_set
            .difference(&previous_set)
            .cloned()
            .for_each(|key| {
                self.sso_stable_id_index_memory
                    .insert(key, vec![anchor_number].into());
            });
    }

    /// Resolve the anchor holding this credential for `discovery_domain`, the
    /// domain the login was verified through (`None` for a configured provider).
    pub fn lookup_anchor_with_openid_credential(
        &self,
        key: &OpenIdCredentialKey,
        discovery_domain: Option<&str>,
    ) -> Option<AnchorNumber> {
        let anchor_number = self.anchor_number_with_openid_credential(key)?;
        let anchor = self.read(anchor_number).ok()?;
        let (iss, sub, aud) = key;
        anchor
            .openid_credentials()
            .iter()
            .any(|cred| {
                &cred.iss == iss
                    && &cred.sub == sub
                    && &cred.aud == aud
                    && cred.sso_domain.as_deref() == discovery_domain
            })
            .then_some(anchor_number)
    }

    /// Whether this credential is registered on any anchor. Registration
    /// uniqueness spans all discovery domains.
    pub fn is_openid_credential_registered(&self, key: &OpenIdCredentialKey) -> bool {
        self.anchor_number_with_openid_credential(key).is_some()
    }

    /// The `(iss, sub, aud)` index read behind both lookups above.
    fn anchor_number_with_openid_credential(
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

    pub fn lookup_anchor_with_passkey_pubkey(&self, pubkey: &PublicKey) -> Option<AnchorNumber> {
        let principal = Principal::self_authenticating(pubkey);
        self.lookup_anchor_with_passkey_pubkey_hash_memory
            .get(&principal)
    }

    /// Look up the MCP session grant registered for `principal` (the caller
    /// of the server-facing `mcp_*` methods). Callers are responsible for
    /// checking `expires_at_ns`; the map itself never authorizes anything.
    pub fn lookup_mcp_grant(&self, principal: Principal) -> Option<StorableMcpGrant> {
        self.mcp_grant_memory.get(&principal)
    }

    /// Insert (or replace) the MCP session grant keyed by `principal`. The
    /// one-session-per-anchor invariant and the cross-anchor collision policy
    /// live in [`crate::mcp`], which mutates this map only together with the
    /// owning anchor's [`StorableMcpConfig::session_principal`] pointer.
    pub fn insert_mcp_grant(&mut self, principal: Principal, grant: StorableMcpGrant) {
        self.mcp_grant_memory.insert(principal, grant);
    }

    /// Remove the MCP session grant keyed by `principal`.
    pub fn remove_mcp_grant(&mut self, principal: Principal) {
        self.mcp_grant_memory.remove(&principal);
    }

    /// Total number of MCP session grant entries currently stored — live
    /// grants plus any expired residue that has not been superseded or
    /// removed yet (grants are replaced per anchor on re-registration and
    /// dropped on the config change that revokes them, but an expired grant
    /// whose anchor never returns lingers until then). O(1).
    pub fn mcp_grant_count(&self) -> u64 {
        self.mcp_grant_memory.len()
    }

    /// Number of *live* (non-expired at `now_ns`) MCP session grants: the
    /// currently-authorized MCP sessions, at most one per anchor. Scans the
    /// grant map and filters by `expires_at_ns`, since the map may also hold
    /// expired residue (see [`Self::mcp_grant_count`]); O(n) in stored grants.
    pub fn count_live_mcp_grants(&self, now_ns: u64) -> u64 {
        // Accumulate directly into a `u64`: `Iterator::count` returns `usize`,
        // which is 32-bit on wasm32 and would wrap in a release build.
        self.mcp_grant_memory
            .iter()
            .filter(|(_, grant)| grant.expires_at_ns > now_ns)
            .fold(0u64, |acc, _| acc + 1)
    }

    /// Number of pending MCP registration entries stored: in-flight
    /// registrations plus expired residue not yet reclaimed. Expired entries are
    /// swept by the bounded amortized GC that runs on each `prepare` write, so
    /// residue drains only while there is registration traffic. O(1).
    pub fn mcp_registration_count(&self) -> u64 {
        self.mcp_registration_memory.len()
    }

    /// Number of *live* (non-expired at `now_ns`) MCP registration entries.
    /// Mirrors [`Self::count_live_mcp_grants`]. The gap against
    /// [`Self::mcp_registration_count`] is residue awaiting the sweep, which is
    /// worth watching because the sweep is driven by writes: a deployment that
    /// goes quiet stops reclaiming and the gap persists. O(n) in stored entries.
    pub fn count_live_mcp_registrations(&self, now_ns: u64) -> u64 {
        // Accumulate directly into a `u64`: `Iterator::count` returns `usize`,
        // which is 32-bit on wasm32 and would wrap in a release build.
        self.mcp_registration_memory
            .iter()
            .filter(|(_, entry)| entry.expires_at_ns > now_ns)
            .fold(0u64, |acc, _| acc + 1)
    }

    /// Number of stored per-anchor MCP configs. Configs never expire, so there
    /// is no live/residue split: this is one row per identity that has the
    /// feature configured, and after the config migration one row per anchor.
    /// O(1).
    pub fn mcp_config_count(&self) -> u64 {
        self.mcp_config_memory.len()
    }

    /// Look up the pending MCP registration entry keyed by `principal` (the
    /// registration principal `P_reg`). Callers check `expires_at_ns`; the map
    /// itself never authorizes anything.
    pub fn lookup_mcp_registration(&self, principal: Principal) -> Option<StorableMcpRegistration> {
        self.mcp_registration_memory.get(&principal)
    }

    /// Insert (or replace) the pending MCP registration entry keyed by
    /// `principal`. Written by `prepare_mcp_registration_delegation` under user
    /// authorization; it records the whole consent (anchor, read-only choice,
    /// grant TTL, trusted URL, and the delegation's expiry), so
    /// `mcp_register_v2` recovers all of it without a call argument.
    pub fn insert_mcp_registration(
        &mut self,
        principal: Principal,
        registration: StorableMcpRegistration,
    ) {
        self.mcp_registration_memory.insert(principal, registration);
    }

    /// Remove the pending MCP registration entry keyed by `principal`. Called
    /// when a lookup finds the entry expired (the delegation is multi-use within
    /// its short lifetime, so a successful redemption *retains* the entry).
    pub fn remove_mcp_registration(&mut self, principal: Principal) {
        self.mcp_registration_memory.remove(&principal);
    }

    /// Reclaim expired registration entries, inspecting up to `budget` entries in
    /// one bounded window that begins at `start` and wraps around to the lowest
    /// key. Returns the number removed.
    ///
    /// This is the amortized *global* GC of the registration index: `prepare`
    /// prunes the calling anchor's own entries synchronously, but an anchor that
    /// mints an entry and never returns would otherwise leave it until it
    /// expired and then forever (nothing else looks it up). Scanning a bounded
    /// window keeps the work per write O(`budget`) rather than O(index size); a
    /// fresh random `start` on each call gives amortized coverage of the whole
    /// keyspace over successive writes, so expired entries anywhere are reclaimed
    /// without any single call scanning the whole map. Wrapping (`start..` then
    /// `..start`) means every call inspects a full `budget`-sized window even
    /// when `start` lands near the top of the keyspace, keeping the reclamation
    /// rate independent of where `start` falls.
    pub fn sweep_expired_mcp_registrations(
        &mut self,
        now_ns: u64,
        start: Principal,
        budget: usize,
    ) -> usize {
        let expired: Vec<Principal> = self
            .mcp_registration_memory
            .range(start..)
            .chain(self.mcp_registration_memory.range(..start))
            .take(budget)
            .filter(|(_, entry)| entry.expires_at_ns <= now_ns)
            .map(|(principal, _)| principal)
            .collect();
        for principal in &expired {
            self.mcp_registration_memory.remove(principal);
        }
        expired.len()
    }

    /// Read `anchor_number`'s synced trusted-MCP-server config. Returns the
    /// default (disabled, no server) for an anchor that never wrote one.
    pub fn read_mcp_config(&self, anchor_number: AnchorNumber) -> StorableMcpConfig {
        self.mcp_config_memory
            .get(&anchor_number)
            .unwrap_or_default()
    }

    /// `anchor_number`'s stored config, or `None` when it never wrote one —
    /// an identity that predates registration seeding and never used the
    /// feature, or one registered on a deployment with no official connector.
    pub fn lookup_mcp_config(&self, anchor_number: AnchorNumber) -> Option<StorableMcpConfig> {
        self.mcp_config_memory.get(&anchor_number)
    }

    /// Persist `anchor_number`'s trusted-MCP-server config (overwriting any
    /// previous value), so it syncs across the identity's devices.
    pub fn write_mcp_config(&mut self, anchor_number: AnchorNumber, config: StorableMcpConfig) {
        self.mcp_config_memory.insert(anchor_number, config);
    }

    /// Give `anchor_number` AI access on the official connector. Call it through
    /// `mcp::init_config_for_new_identity`, which holds the rule that this is
    /// only written when the deployment has a connector to trust.
    pub fn init_mcp_config(&mut self, anchor_number: AnchorNumber) {
        self.mcp_config_memory.insert(
            anchor_number,
            StorableMcpConfig {
                enabled: true,
                ..Default::default()
            },
        );
    }

    /// Resolve a non-`sub` SSO stable id to the anchor that carries the matching
    /// II-client credential, or `None` if no anchor does (never linked, or the
    /// credential has since been removed — the index self-cleans on `write()`,
    /// so a stale `Some` can't linger). Mirrors
    /// [`Storage::lookup_anchor_with_openid_credential`].
    pub fn lookup_anchor_by_sso_stable_id(
        &self,
        sso_domain: &str,
        iss: &str,
        ii_client_id: &str,
        stable_id: &str,
    ) -> Option<AnchorNumber> {
        let anchor_numbers: Vec<AnchorNumber> = self
            .sso_stable_id_index_memory
            .get(&StorableSsoStableIdKey::new(
                sso_domain,
                iss,
                ii_client_id,
                stable_id,
            ))
            .map(Into::into)?;
        anchor_numbers.first().copied()
    }

    /// Resolve the verified `From:` of an inbound recovery email to
    /// the anchor it was bound to at setup time. Returns `None` if
    /// the address has never been registered (or has been removed).
    /// The lookup is by `SHA-256(lowercase(address))` — see design
    /// §8.2 for why.
    pub fn lookup_anchor_with_email_recovery_address(&self, address: &str) -> Option<AnchorNumber> {
        let hash = StorableEmailRecoveryAddressHash::of(address);
        self.lookup_anchor_with_email_recovery_memory.get(&hash)
    }

    /// Apply a setup/replace: update the reverse index to reflect
    /// `anchor`'s current bound address. `previous` is the address
    /// that was bound before the operation (or `None` for an initial
    /// add); `current` is the new bound address (or `None` for a
    /// remove). Both transitions are exercised by the email-recovery
    /// flow:
    ///
    /// - `(None, Some(addr))` — first registration: insert the new hash.
    /// - `(Some(prev), Some(new))` — replacement (anchor swaps which
    ///   address recovers it): drop the old hash, insert the new.
    /// - `(Some(prev), None)` — remove: drop the old hash.
    ///
    /// `(None, None)` is a no-op. The two operations are sequenced
    /// so that during a swap the old entry is removed before the new
    /// one is written; an interleaving observer never sees both.
    ///
    /// Returns `Err(other_anchor)` if `current` is already bound to a
    /// different anchor — this enforces the "one anchor per address"
    /// invariant from design §8.2 at the storage layer regardless of
    /// what the caller checked. The caller is expected to surface
    /// `AddressAlreadyRegistered` (setup) or `AddressNotRegistered`
    /// (recovery) to the FE.
    pub fn update_email_recovery_lookup(
        &mut self,
        anchor_number: AnchorNumber,
        previous: Option<&str>,
        current: Option<&str>,
    ) -> Result<(), AnchorNumber> {
        // Enforce "one anchor per address" before mutating anything.
        // Same-anchor rebinds are idempotent (the API uses this to
        // re-confirm a binding); cross-anchor rebinds are rejected.
        if let Some(curr) = current {
            let hash = StorableEmailRecoveryAddressHash::of(curr);
            if let Some(existing) = self.lookup_anchor_with_email_recovery_memory.get(&hash) {
                if existing != anchor_number {
                    return Err(existing);
                }
            }
        }
        if let Some(prev) = previous {
            let hash = StorableEmailRecoveryAddressHash::of(prev);
            self.lookup_anchor_with_email_recovery_memory.remove(&hash);
        }
        if let Some(curr) = current {
            let hash = StorableEmailRecoveryAddressHash::of(curr);
            self.lookup_anchor_with_email_recovery_memory
                .insert(hash, anchor_number);
        }
        Ok(())
    }

    fn sync_anchor_with_passkey_pubkey_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous_passkeys: &[StorablePasskeyCredential],
        current_passkeys: &[StorablePasskeyCredential],
    ) {
        let previous_principals = previous_passkeys
            .iter()
            .map(|passkey| Principal::self_authenticating(&passkey.pubkey))
            .collect::<BTreeSet<_>>();

        let current_principals = current_passkeys
            .iter()
            .map(|passkey| Principal::self_authenticating(&passkey.pubkey))
            .collect::<BTreeSet<_>>();

        let principals_to_be_removed = previous_principals.difference(&current_principals);
        let principals_to_be_added = current_principals.difference(&previous_principals);

        for principal in principals_to_be_removed {
            let Some(existing_anchor_number) = self
                .lookup_anchor_with_passkey_pubkey_hash_memory
                .get(principal)
            else {
                // This principal is not indexed, nothing to do.
                continue;
            };
            if existing_anchor_number != anchor_number {
                // Ensure that a user can remove only their own passkey pubkey from the index.
                continue;
            }
            self.lookup_anchor_with_passkey_pubkey_hash_memory
                .remove(principal);
        }

        for principal in principals_to_be_added {
            if self
                .lookup_anchor_with_passkey_pubkey_hash_memory
                .contains_key(principal)
            {
                // This principal is already occupied; do not overwrite it.
                ic_cdk::println!(
                    "WARNING: Principal {:?} derived from a passkey credential pubkey is already \
                     indexed for another anchor; skipping indexing for anchor number {}",
                    principal,
                    anchor_number,
                );
                continue;
            };

            self.lookup_anchor_with_passkey_pubkey_hash_memory
                .insert(*principal, anchor_number);
        }
    }

    fn sync_anchor_with_recovery_phrase_principal_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous_recovery_keys: &[StorableRecoveryKey],
        current_recovery_keys: &[StorableRecoveryKey],
    ) {
        let previous_recovery_principals = previous_recovery_keys
            .iter()
            .map(|recovery_key| Principal::self_authenticating(&recovery_key.pubkey))
            .collect::<BTreeSet<_>>();
        let current_recovery_principals = current_recovery_keys
            .iter()
            .map(|recovery_key| Principal::self_authenticating(&recovery_key.pubkey))
            .collect::<BTreeSet<_>>();

        for recovery_principal in
            previous_recovery_principals.difference(&current_recovery_principals)
        {
            let Some(existing_anchor_number) = self
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(recovery_principal)
            else {
                // This principal is not indexed, nothing to do.
                continue;
            };
            if existing_anchor_number != anchor_number {
                // Ensure that a user can remove only their own recovery phrase device from the index.
                continue;
            }
            self.lookup_anchor_with_recovery_phrase_principal_memory
                .remove(recovery_principal);
        }

        for recovery_principal in current_recovery_principals {
            if self
                .lookup_anchor_with_recovery_phrase_principal_memory
                .contains_key(&recovery_principal)
            {
                // This principal is already occupied; do not overwrite it.
                continue;
            };

            self.lookup_anchor_with_recovery_phrase_principal_memory
                .insert(recovery_principal, anchor_number);
        }
    }

    /// Update `CredentialId` to `AnchorNumber` lookup map
    fn sync_anchor_with_passkey_credential_index(
        &mut self,
        anchor_number: AnchorNumber,
        previous_passkey_credentials: &[StorablePasskeyCredential],
        current_passkey_credentials: &[StorablePasskeyCredential],
    ) {
        let previous_passkey_credential_ids = previous_passkey_credentials
            .iter()
            .map(|passkey_credential| {
                StorableCredentialId::from_bytes(Cow::Borrowed(&passkey_credential.credential_id))
            })
            .collect::<BTreeSet<_>>();
        let current_passkey_credential_ids = current_passkey_credentials
            .iter()
            .map(|passkey_credential| {
                StorableCredentialId::from_bytes(Cow::Borrowed(&passkey_credential.credential_id))
            })
            .collect::<BTreeSet<_>>();

        for credential_id in
            previous_passkey_credential_ids.difference(&current_passkey_credential_ids)
        {
            let Some(indexed_anchor_number) = self
                .lookup_anchor_with_passkey_credential_memory
                .get(credential_id)
            else {
                continue;
            };

            // Only remove if the credential is assigned to *this* anchor.
            if indexed_anchor_number != anchor_number {
                continue;
            }

            self.lookup_anchor_with_passkey_credential_memory
                .remove(credential_id);
        }

        for credential_id in current_passkey_credential_ids {
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
            let new_number = self.allocate_application_number();

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

    fn allocate_application_number(&mut self) -> ApplicationNumber {
        let new_number = *self.next_application_number_memory.get();
        self.next_application_number_memory
            .set(new_number + 1)
            .expect("failed to advance the application number allocator");
        new_number
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
    /// * `Ok(None)` if both account and account_reference are not found
    /// * `Ok(Some(T))` if the account or account_reference are found where T is the result of the function `f`.
    /// * `Err` if writing the reference list back failed
    fn with_account_mut<T, F>(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: Option<ApplicationNumber>,
        maybe_account_number: Option<AccountNumber>,
        f: F,
    ) -> Result<Option<T>, StorageError>
    where
        F: FnOnce(&mut StorableAccountReference, Option<&mut StorableAccount>) -> T,
    {
        // A named account has a stored record to hand to `f`; a default is derived
        // and has none. A named account with no record was removed, so there is
        // nothing to update.
        let mut storable_account = match maybe_account_number {
            Some(account_number) => match self.stable_account_memory.get(&account_number) {
                Some(storable_account) => Some(storable_account),
                None => return Ok(None),
            },
            None => None,
        };

        let Some(((_, application_number), mut account_references)) =
            self.find_account_references(anchor_number, application_number)
        else {
            return Ok(None);
        };

        let Some(account_reference) = account_references
            .iter_mut()
            .find(|account_reference| account_reference.account_number == maybe_account_number)
        else {
            // `f` never ran, so nothing was modified and writing the list back would
            // store the bytes it already holds.
            return Ok(None);
        };
        let result = f(account_reference, storable_account.as_mut());

        self.write_reference_list(
            anchor_number,
            application_number,
            account_references.into_iter().map(Into::into).collect(),
        )?;
        if let (Some(account_number), Some(storable_account)) =
            (maybe_account_number, storable_account)
        {
            self.stable_account_memory
                .insert(account_number, storable_account);
        }

        Ok(Some(result))
    }

    /// Records that an account was used at `origin`.
    ///
    /// A default account has no reference stored until it is used, so recording a
    /// use is also what creates one. A named account's reference is its existence:
    /// where it is gone the account was removed, and writing one back would undo
    /// that, so only the default gets the pre-step.
    pub fn record_account_use(
        &mut self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        account_number: Option<AccountNumber>,
        now: Timestamp,
    ) -> Result<(), StorageError> {
        if account_number.is_none() {
            let application_number = self.lookup_or_insert_application_number_with_origin(&origin);
            self.ensure_account_reference_list(anchor_number, application_number)?;
        }
        self.stamp_account_reference(anchor_number, &origin, account_number, now)
    }

    /// Stamps `last_used` on the reference for `account_number` at `origin`.
    ///
    /// Does nothing when the identity has no such reference stored: nothing is
    /// recorded against an account it cannot reach.
    fn stamp_account_reference(
        &mut self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
        account_number: Option<AccountNumber>,
        now: Timestamp,
    ) -> Result<(), StorageError> {
        let application_number = self.lookup_application_number_with_origin(origin);
        self.with_account_mut(
            anchor_number,
            application_number,
            account_number,
            |account_reference, _| {
                account_reference.last_used = Some(now);
            },
        )?;
        Ok(())
    }

    /// Signs one browser out of everything, in a single message.
    pub fn revoke_device_sessions(
        &mut self,
        anchor_number: AnchorNumber,
        device_id: SessionDeviceId,
    ) -> Result<u64, StorageError> {
        let affected: Vec<(ApplicationNumber, Vec<AccountReference>)> = self
            .stable_account_reference_list_memory
            .range(
                (anchor_number, ApplicationNumber::MIN)..=(anchor_number, ApplicationNumber::MAX),
            )
            .filter_map(|((_, application_number), list)| {
                let references: Vec<AccountReference> = list.into();
                references
                    .iter()
                    .any(|reference| {
                        reference
                            .sessions
                            .iter()
                            .any(|session| session.device_id == device_id)
                    })
                    .then_some((application_number, references))
            })
            .collect();

        let mut removed = 0u64;
        for (application_number, mut references) in affected {
            let mut dropped: Vec<(Option<AccountNumber>, SessionRecord)> = vec![];
            for reference in &mut references {
                let account_number = reference.account_number;
                reference.sessions.retain(|session| {
                    if session.device_id == device_id {
                        dropped.push((account_number, session.clone()));
                        return false;
                    }
                    true
                });
            }
            removed += dropped.len() as u64;
            self.write_reference_list(anchor_number, application_number, references)?;
            for (account_number, session) in &dropped {
                self.unindex_sessions(
                    anchor_number,
                    application_number,
                    *account_number,
                    std::slice::from_ref(session),
                );
            }
        }
        if removed > 0 {
            self.change_session_count(anchor_number, removed as usize, 0)?;
        }

        Ok(removed)
    }

    /// The account a principal a dapp sees was derived for.
    pub fn lookup_account_with_principal(
        &self,
        principal: Principal,
    ) -> Option<StorableAccountLocator> {
        self.lookup_account_with_principal_memory.get(&principal)
    }

    /// Where the session a caller authenticates as is stored.
    pub fn lookup_session_with_principal(
        &self,
        principal: Principal,
    ) -> Option<StorableSessionHandle> {
        self.lookup_session_with_principal_memory.get(&principal)
    }

    /// The principal a session's chain is rooted at, which is what an app-facing call
    /// arrives as. `None` only when the salt is unset or the account is gone, both of
    /// which make the session unusable anyway.
    fn session_principal(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
        session: &SessionRecord,
    ) -> Option<Principal> {
        let salt = self.salt().copied()?;
        let account = self.read_account(ReadAccountParams {
            account_number,
            anchor_number,
            origin: &self
                .stable_application_memory
                .get(&application_number)?
                .origin,
            known_app_num: Some(application_number),
        })?;
        let seed = calculate_session_seed_with_salt(
            &salt,
            &account.calculate_seed_with_salt(&salt),
            session.created_at,
            session.device_id,
        );
        Some(canister_sig_principal(canister_id(), seed.to_vec()))
    }

    /// Frees a slot for one more session, and reports whether the anchor has one.
    ///
    /// The stored count is a trigger, never the thing the cap is enforced against: a
    /// session can expire with no write anywhere, so the count drifts upwards. Once it
    /// reaches the cap this recounts what the rows hold and reclaims against that, so an
    /// admission is only ever granted against a number that was just counted.
    fn ensure_session_slot(
        &mut self,
        anchor_number: AnchorNumber,
        now: Timestamp,
    ) -> Result<bool, StorageError> {
        if self.read(anchor_number)?.session_count < MAX_SESSIONS_PER_ANCHOR {
            return Ok(true);
        }
        Ok(self.reclaim_sessions(anchor_number, now)? < MAX_SESSIONS_PER_ANCHOR)
    }

    /// Replaces the count with a number that was counted rather than accumulated.
    fn set_session_count(
        &mut self,
        anchor_number: AnchorNumber,
        count: u32,
    ) -> Result<(), StorageError> {
        let mut anchor = self.read(anchor_number)?;
        if anchor.session_count == count {
            return Ok(());
        }
        anchor.session_count = count;
        self.write(anchor)
    }

    /// Moves the count without considering the cap, for the paths that only remove.
    fn change_session_count(
        &mut self,
        anchor_number: AnchorNumber,
        removed: usize,
        added: usize,
    ) -> Result<u32, StorageError> {
        let mut anchor = self.read(anchor_number)?;
        anchor.session_count = anchor
            .session_count
            .saturating_sub(removed as u32)
            .saturating_add(added as u32);
        let count = anchor.session_count;
        self.write(anchor)?;
        Ok(count)
    }

    /// Walks the anchor's rows once and reclaims down to the watermark, taking sessions in
    /// [`SessionRecord::reclaim_order`]: dead ones first, then the least recently used.
    ///
    /// Returns what the rows actually hold once it is done, which is the number the cap is
    /// enforced against. The stored counter is only ever a trigger for running this pass —
    /// it can drift, this cannot, because it counts the sessions themselves.
    ///
    /// One pass per fifty sign-ins, because it reclaims to the watermark rather than to the
    /// cap, and bounded by the same row limit account eviction uses.
    fn reclaim_sessions(
        &mut self,
        anchor_number: AnchorNumber,
        now: Timestamp,
    ) -> Result<u32, StorageError> {
        struct Candidate {
            order: (bool, Timestamp, SessionDeviceId),
            row: usize,
            account_number: Option<AccountNumber>,
            device_id: SessionDeviceId,
        }

        // Every row, not a bounded prefix of them: the number this returns is what the cap is
        // enforced against, and a truncated scan would undercount, lower the counter to the
        // undercount, and let the stored set climb past the cap from there. An identity's rows
        // are already bounded — the row cap holds the evictable ones and the account cap holds
        // the rest — and a sequential scan of them costs a fraction of the writes it saves.
        let mut rows: Vec<(ApplicationNumber, Vec<AccountReference>)> = self
            .stable_account_reference_list_memory
            .range(
                (anchor_number, ApplicationNumber::MIN)..=(anchor_number, ApplicationNumber::MAX),
            )
            .map(|((_, application_number), list)| (application_number, list.into()))
            .collect();

        let mut candidates: Vec<Candidate> = vec![];
        for (row, (_, references)) in rows.iter().enumerate() {
            for reference in references {
                for session in &reference.sessions {
                    candidates.push(Candidate {
                        order: session.reclaim_order(now),
                        row,
                        account_number: reference.account_number,
                        device_id: session.device_id,
                    });
                }
            }
        }
        let stored = candidates.len() as u32;
        candidates.sort_by_key(|candidate| candidate.order);

        let surplus = stored.saturating_sub(SESSIONS_WATERMARK_PER_ANCHOR) as usize;
        let victims = &candidates[..surplus.min(candidates.len())];
        if victims.is_empty() {
            self.set_session_count(anchor_number, stored)?;
            return Ok(stored);
        }

        // One write per row rather than one per victim: the row is a single blob, so
        // dropping several of its sessions one at a time would rewrite it several times.
        let mut touched: Vec<usize> = victims.iter().map(|victim| victim.row).collect();
        touched.sort_unstable();
        touched.dedup();

        let mut dropped_total = 0usize;
        for row in touched {
            let (application_number, references) = &mut rows[row];
            let application_number = *application_number;
            let mut removed: Vec<(Option<AccountNumber>, SessionRecord)> = vec![];
            for reference in references.iter_mut() {
                let account_number = reference.account_number;
                reference.sessions.retain(|session| {
                    // The row has to be part of the match: one browser holds one session per
                    // account, but the same browser and the same account number appear in
                    // every row, so matching on that pair alone reaches across applications.
                    let doomed = victims.iter().any(|victim| {
                        victim.row == row
                            && victim.account_number == account_number
                            && victim.device_id == session.device_id
                    });
                    if doomed {
                        removed.push((account_number, session.clone()));
                    }
                    !doomed
                });
            }
            if removed.is_empty() {
                continue;
            }
            self.write_reference_list(anchor_number, application_number, references.clone())?;
            for (account_number, session) in &removed {
                self.unindex_sessions(
                    anchor_number,
                    application_number,
                    *account_number,
                    std::slice::from_ref(session),
                );
            }
            dropped_total += removed.len();
        }

        let remaining = stored.saturating_sub(dropped_total as u32);
        self.set_session_count(anchor_number, remaining)?;
        Ok(remaining)
    }

    /// Records that a session was used. Reports whether a session matched. The
    /// reference's `last_used` rides on the same write.
    pub fn stamp_session_refresh(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
        created_at: Timestamp,
        device_id: SessionDeviceId,
        now: Timestamp,
    ) -> Result<bool, StorageError> {
        let Some(references) = self.lookup_account_references(anchor_number, application_number)
        else {
            return Ok(false);
        };
        let mut references: Vec<AccountReference> =
            references.into_iter().map(Into::into).collect();

        let Some(reference) = references
            .iter_mut()
            .find(|reference| reference.account_number == account_number)
        else {
            return Ok(false);
        };
        let Some(session) = reference
            .sessions
            .iter_mut()
            .find(|session| session.created_at == created_at && session.device_id == device_id)
        else {
            return Ok(false);
        };

        session.last_refreshed = Some(now);
        reference.last_used = Some(now);

        // This row is being rewritten anyway, so its dead sessions go now. It costs one
        // pass over a list already in memory and no write of its own, and it means every
        // row anyone still uses stays clean without anything having to sweep for it.
        let mut expired: Vec<(Option<AccountNumber>, SessionRecord)> = vec![];
        for reference in references.iter_mut() {
            let account_number = reference.account_number;
            reference.sessions.retain(|session| {
                if session.is_expired(now) {
                    expired.push((account_number, session.clone()));
                    return false;
                }
                true
            });
        }

        self.write_reference_list(anchor_number, application_number, references)?;
        for (account_number, session) in &expired {
            self.unindex_sessions(
                anchor_number,
                application_number,
                *account_number,
                std::slice::from_ref(session),
            );
        }
        if !expired.is_empty() {
            self.change_session_count(anchor_number, expired.len(), 0)?;
        }
        self.stamp_session_device_use(anchor_number, device_id, now)?;
        Ok(true)
    }

    /// Advances the device registry's `last_used` for the browser driving this session.
    fn stamp_session_device_use(
        &mut self,
        anchor_number: AnchorNumber,
        device_id: SessionDeviceId,
        now: Timestamp,
    ) -> Result<(), StorageError> {
        let mut anchor = self.read(anchor_number)?;
        if !anchor.stamp_session_device_use(device_id, now) {
            return Ok(());
        }
        self.write(anchor)
    }

    /// Drops the index entries of sessions that have just been removed from a row.
    fn unindex_sessions(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
        removed: &[SessionRecord],
    ) {
        for session in removed {
            if let Some(principal) =
                self.session_principal(anchor_number, application_number, account_number, session)
            {
                self.lookup_session_with_principal_memory.remove(&principal);
            }
        }
    }

    /// The account a session handle names, together with its sessions.
    pub fn account_with_sessions(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
    ) -> Option<(Account, Vec<SessionRecord>)> {
        let origin = self
            .stable_application_memory
            .get(&application_number)
            .map(|application| application.origin)?;
        let references: Vec<AccountReference> = self
            .lookup_account_references(anchor_number, application_number)?
            .into_iter()
            .map(Into::into)
            .collect();
        let reference = references
            .into_iter()
            .find(|reference| reference.account_number == account_number)?;
        let account = self.read_account(ReadAccountParams {
            account_number,
            anchor_number,
            origin: &origin,
            known_app_num: Some(application_number),
        })?;
        Some((account, reference.sessions))
    }

    pub fn account_sessions(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
        account_number: Option<AccountNumber>,
    ) -> Option<Vec<SessionRecord>> {
        let application_number = self.lookup_application_number_with_origin(origin)?;
        let references: Vec<AccountReference> = self
            .lookup_account_references(anchor_number, application_number)?
            .into_iter()
            .map(Into::into)
            .collect();
        references
            .into_iter()
            .find(|reference| reference.account_number == account_number)
            .map(|reference| reference.sessions)
    }

    /// Creates the session `prepare_account_session` mints an identity from, replacing
    /// whatever this browser already held at this account.
    pub fn create_session(
        &mut self,
        params: CreateSessionParams,
    ) -> Result<SessionRecord, StorageError> {
        let CreateSessionParams {
            anchor_number,
            origin,
            account_number,
            device_id,
            valid_till,
            read_only,
            now,
        } = params;

        // The row this session lands in has to exist first, but an existing one must not be
        // written here: the single write at the end of this function carries `last_used`.
        let application_number = match self.lookup_application_number_with_origin(&origin) {
            Some(application_number)
                if self
                    .lookup_account_references(anchor_number, application_number)
                    .is_some() =>
            {
                application_number
            }
            _ => {
                if account_number.is_some() {
                    return Err(StorageError::MissingAccount {
                        anchor_number,
                        name: origin,
                    });
                }
                let application_number =
                    self.lookup_or_insert_application_number_with_origin(&origin);
                self.write_reference_list(
                    anchor_number,
                    application_number,
                    vec![AccountReference::new(None, Some(now))],
                )?;
                self.evict_idle_tracked_defaults(anchor_number, application_number)?;
                application_number
            }
        };

        // Reclaiming before the session is admitted rather than after it: the stored set
        // never sits above the cap, not even for the rest of this message.
        if !self.ensure_session_slot(anchor_number, now)? {
            return Err(StorageError::SessionCapNotReclaimed { anchor_number });
        }

        let mut references: Vec<AccountReference> = self
            .lookup_account_references(anchor_number, application_number)
            .ok_or(StorageError::MissingAccount {
                anchor_number,
                name: origin,
            })?
            .into_iter()
            .map(Into::into)
            .collect();

        let reference = references
            .iter_mut()
            .find(|reference| reference.account_number == account_number)
            .ok_or(StorageError::MissingAccount {
                anchor_number,
                name: String::new(),
            })?;
        reference.last_used = Some(now);

        // A ceremony replaces whatever this browser held here, rather than reusing it: the
        // copy of an old session's chain stops working at the user's next sign-in instead of
        // at its expiry.
        let mut dropped: Vec<(Option<AccountNumber>, SessionRecord)> = vec![];
        reference.sessions.retain(|session| {
            if session.device_id == device_id {
                dropped.push((account_number, session.clone()));
                return false;
            }
            true
        });

        let session = SessionRecord {
            created_at: now,
            valid_till,
            last_refreshed: None,
            device_id,
            read_only,
        };
        reference.sessions.push(session.clone());

        // The whole row, not just the reference being written: this row is about to be
        // rewritten anyway, and a dead session on a sibling reference has nothing else
        // coming for it.
        for reference in references.iter_mut() {
            let account_number = reference.account_number;
            reference.sessions.retain(|session| {
                if session.is_expired(now) {
                    dropped.push((account_number, session.clone()));
                    return false;
                }
                true
            });
        }

        self.write_reference_list(anchor_number, application_number, references)?;
        for (account_number, session) in &dropped {
            self.unindex_sessions(
                anchor_number,
                application_number,
                *account_number,
                std::slice::from_ref(session),
            );
        }
        if let (Some(principal), Some(account_principal)) = (
            self.session_principal(anchor_number, application_number, account_number, &session),
            self.account_principal_of(anchor_number, application_number, account_number),
        ) {
            self.lookup_session_with_principal_memory.insert(
                principal,
                StorableSessionHandle {
                    account_principal: account_principal.as_slice().to_vec(),
                    device_id,
                    created_at: session.created_at,
                },
            );
        }
        self.change_session_count(anchor_number, dropped.len(), 1)?;

        Ok(session)
    }

    /// The principal an app sees for an account, which is what a session handle names.
    fn account_principal_of(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
    ) -> Option<Principal> {
        let salt = self.salt().copied()?;
        let account = self.read_account(ReadAccountParams {
            account_number,
            anchor_number,
            origin: &self
                .stable_application_memory
                .get(&application_number)?
                .origin,
            known_app_num: Some(application_number),
        })?;
        Some(canister_sig_principal(
            canister_id(),
            account.calculate_seed_with_salt(&salt).to_vec(),
        ))
    }

    /// Writes the reference-list row an `AnchorApplicationConfig` row implies, leaving
    /// `last_used` unset.
    pub fn ensure_account_reference_list(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> Result<(), StorageError> {
        if self
            .lookup_account_references(anchor_number, application_number)
            .is_some()
        {
            return Ok(());
        }

        self.write_reference_list(
            anchor_number,
            application_number,
            vec![AccountReference::new(None, None)],
        )?;
        self.evict_idle_tracked_defaults(anchor_number, application_number)
    }

    /// Removes a reference-list row and everything derived from it.
    fn remove_reference_list(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> Result<(), StorageError> {
        let key = (anchor_number, application_number);
        let Some(previous) = self
            .stable_account_reference_list_memory
            .get(&key)
            .map(Vec::<AccountReference>::from)
        else {
            return Ok(());
        };
        let application = self
            .stable_application_memory
            .get(&application_number)
            .ok_or(StorageError::OriginNotFoundForApplicationNumber { application_number })?;

        self.sync_account_principal_index(anchor_number, application_number, &previous, &[])?;

        // The row's sessions go with it, so their index entries have to go too. A browser
        // keeps its id, and evicting a row leaves the account's principal untouched, so an
        // entry left behind here would be waiting for the next sign-in at this origin.
        let mut dropped = 0usize;
        for reference in &previous {
            self.unindex_sessions(
                anchor_number,
                application_number,
                reference.account_number,
                &reference.sessions,
            );
            dropped += reference.sessions.len();
        }
        if dropped > 0 {
            self.change_session_count(anchor_number, dropped, 0)?;
        }

        self.stable_account_reference_list_memory.remove(&key);
        self.stable_anchor_application_config_memory.remove(&key);

        let deltas = ReferenceListDeltas::between(&previous, &[]);
        self.apply_reference_counter_deltas(anchor_number, application_number, application, deltas);

        Ok(())
    }

    /// Rows whose only reference is a tracked default.
    fn evictable_default_rows(
        &self,
        anchor_number: AnchorNumber,
    ) -> Vec<(ApplicationNumber, Option<Timestamp>)> {
        self.stable_account_reference_list_memory
            .range(
                (anchor_number, ApplicationNumber::MIN)..=(anchor_number, ApplicationNumber::MAX),
            )
            .filter_map(|((_, application_number), list)| {
                let references: Vec<AccountReference> = list.into();
                match references.as_slice() {
                    [tracked_default] if tracked_default.account_number.is_none() => {
                        Some((application_number, tracked_default.last_used))
                    }
                    _ => None,
                }
            })
            .collect()
    }

    /// Upper bound on an anchor's evictable rows, from counters that already exist.
    fn tracked_default_account_upper_bound(&self, anchor_number: AnchorNumber) -> u64 {
        let counter = self.get_account_counter(anchor_number);
        counter
            .stored_account_references
            .saturating_sub(counter.stored_accounts)
    }

    /// Drops the least recently used evictable defaults once the anchor is at the cap.
    fn evict_idle_tracked_defaults(
        &mut self,
        anchor_number: AnchorNumber,
        just_written: ApplicationNumber,
    ) -> Result<(), StorageError> {
        if self.tracked_default_account_upper_bound(anchor_number) < MAX_EVICTABLE_DEFAULT_ACCOUNTS
        {
            return Ok(());
        }

        let mut candidates: Vec<_> = self
            .evictable_default_rows(anchor_number)
            .into_iter()
            .filter(|(application_number, _)| *application_number != just_written)
            .collect();
        if candidates.len() as u64 <= EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK {
            return Ok(());
        }

        candidates.sort_by_key(|(application_number, last_used)| (*last_used, *application_number));

        let victims = u64::min(
            candidates.len() as u64 - EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK,
            MAX_EVICTIONS_PER_CALL,
        );
        for (application_number, _) in candidates.into_iter().take(victims as usize) {
            self.remove_reference_list(anchor_number, application_number)?;
        }

        Ok(())
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

    /// The single write path for an anchor's account reference list at one
    /// application, including the counters derived from it.
    fn write_reference_list(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        current: Vec<AccountReference>,
    ) -> Result<(), StorageError> {
        if current.is_empty() {
            return Err(StorageError::EmptyAccountReferenceList {
                anchor_number,
                application_number,
            });
        }

        let application = self
            .stable_application_memory
            .get(&application_number)
            .ok_or(StorageError::OriginNotFoundForApplicationNumber { application_number })?;

        let key = (anchor_number, application_number);
        let previous = self
            .stable_account_reference_list_memory
            .get(&key)
            .map(Vec::<AccountReference>::from)
            .unwrap_or_default();

        let deltas = ReferenceListDeltas::between(&previous, &current);

        self.sync_account_principal_index(anchor_number, application_number, &previous, &current)?;

        self.stable_account_reference_list_memory
            .insert(key, current.into());
        self.apply_reference_counter_deltas(anchor_number, application_number, application, deltas);

        Ok(())
    }

    /// Indexes one batch of existing reference-list rows. Entries are only inserted,
    /// never removed, so a batch that runs twice writes the same values.
    pub fn backfill_account_principal_index_batch(
        &mut self,
        cursor: Option<(AnchorNumber, ApplicationNumber)>,
        batch_size: u64,
    ) -> AccountPrincipalIndexBackfillOutcome {
        let mut outcome = AccountPrincipalIndexBackfillOutcome {
            next_cursor: cursor,
            ..Default::default()
        };

        if batch_size == 0 {
            outcome.is_done = true;
            return outcome;
        }
        let Some(salt) = self.salt().copied() else {
            return outcome;
        };

        use std::ops::Bound as RangeBound;
        let range = match cursor {
            Some(cursor) => (RangeBound::Excluded(cursor), RangeBound::Unbounded),
            None => (RangeBound::Unbounded, RangeBound::Unbounded),
        };

        let mut examined = 0u64;
        let rows: Vec<_> = self
            .stable_account_reference_list_memory
            .range(range)
            .take(batch_size as usize)
            .map(|(key, list)| {
                examined += 1;
                outcome.next_cursor = Some(key);
                (key, Vec::<AccountReference>::from(list))
            })
            .collect();

        for ((anchor_number, application_number), references) in rows {
            let Some(origin) = self
                .stable_application_memory
                .get(&application_number)
                .map(|application| application.origin)
            else {
                continue;
            };

            for (principal, locator) in self.account_principals(
                anchor_number,
                application_number,
                &origin,
                &salt,
                &references,
            ) {
                if self.lookup_account_with_principal_memory.get(&principal)
                    == Some(locator.clone())
                {
                    continue;
                }
                self.lookup_account_with_principal_memory
                    .insert(principal, locator);
                outcome.indexed += 1;
            }
        }

        outcome.is_done = examined < batch_size;
        outcome
    }

    /// Keeps the principal index in step with one reference-list write, diffing values
    /// rather than keys.
    fn sync_account_principal_index(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        previous: &[AccountReference],
        current: &[AccountReference],
    ) -> Result<(), StorageError> {
        let salt = *self.salt().ok_or(StorageError::SaltNotSet)?;
        let origin = self
            .stable_application_memory
            .get(&application_number)
            .map(|application| application.origin)
            .ok_or(StorageError::OriginNotFoundForApplicationNumber { application_number })?;

        let previous_entries =
            self.account_principals(anchor_number, application_number, &origin, &salt, previous);
        let current_entries =
            self.account_principals(anchor_number, application_number, &origin, &salt, current);

        for (principal, locator) in &previous_entries {
            if current_entries.contains_key(principal) {
                continue;
            }
            if self
                .lookup_account_with_principal_memory
                .get(principal)
                .is_some_and(|stored| stored.anchor_number == locator.anchor_number)
            {
                self.lookup_account_with_principal_memory.remove(principal);
            }
        }

        for (principal, locator) in current_entries {
            if self.lookup_account_with_principal_memory.get(&principal) == Some(locator.clone()) {
                continue;
            }
            self.lookup_account_with_principal_memory
                .insert(principal, locator);
        }

        Ok(())
    }

    /// The principals a set of references derives to. A reference whose account row is
    /// gone derives nothing and is skipped.
    fn account_principals(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        origin: &FrontendHostname,
        salt: &[u8; 32],
        references: &[AccountReference],
    ) -> BTreeMap<Principal, StorableAccountLocator> {
        references
            .iter()
            .filter_map(|reference| {
                let account = match reference.account_number {
                    None => Account::new(anchor_number, origin.clone(), None, None),
                    Some(account_number) => {
                        let stored = self.stable_account_memory.get(&account_number)?;
                        Account::new_full(
                            anchor_number,
                            origin.clone(),
                            Some(stored.name),
                            Some(account_number),
                            reference.last_used,
                            stored.seed_from_anchor,
                        )
                    }
                };
                let principal = delegation::canister_sig_principal(
                    canister_id(),
                    account.calculate_seed_with_salt(salt).to_vec(),
                );
                Some((
                    principal,
                    StorableAccountLocator {
                        anchor_number,
                        application_number,
                        account_number: reference.account_number,
                    },
                ))
            })
            .collect()
    }

    fn apply_reference_counter_deltas(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        application: StorableApplication,
        deltas: ReferenceListDeltas,
    ) {
        if deltas.is_empty() {
            return;
        }

        let anchor_counter = self
            .stable_anchor_account_counter_memory
            .get(&anchor_number)
            .unwrap_or_default();
        let (stored_accounts, stored_account_references) = deltas.apply(
            anchor_counter.stored_accounts,
            anchor_counter.stored_account_references,
        );
        self.stable_anchor_account_counter_memory.insert(
            anchor_number,
            StorableAccountsCounter {
                stored_accounts,
                stored_account_references,
            },
        );

        let global_counter = self.stable_account_counter_memory.get().clone();
        let (_, global_references) = deltas.apply(
            global_counter.stored_accounts,
            global_counter.stored_account_references,
        );
        self.stable_account_counter_memory
            .set(StorableAccountsCounter {
                stored_accounts: global_counter.stored_accounts,
                stored_account_references: global_references,
            })
            .expect("failed to update the global account counter");

        let (stored_accounts, stored_account_references) = deltas.apply(
            application.stored_accounts,
            application.stored_account_references,
        );
        if stored_account_references == 0 {
            self.remove_unreferenced_application(application_number, &application.origin);
        } else {
            self.stable_application_memory.insert(
                application_number,
                StorableApplication {
                    origin: application.origin,
                    stored_accounts,
                    stored_account_references,
                },
            );
        }
    }

    /// Retires an application no anchor references any more. The number is never
    /// reissued.
    fn remove_unreferenced_application(
        &mut self,
        application_number: ApplicationNumber,
        origin: &str,
    ) {
        self.stable_application_memory.remove(&application_number);

        let origin_key = StorableOriginSha256::from_origin(&origin.to_string());
        if self.lookup_application_with_origin_memory.get(&origin_key) == Some(application_number) {
            self.lookup_application_with_origin_memory
                .remove(&origin_key);
        }
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
        let updated_accounts_counter = account_counter.increment_accounts();
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

        // last_used will be set once the user signs in with the account.
        let last_used = None;

        // Process account references
        let references = match self
            .stable_account_reference_list_memory
            .get(&(anchor_number, app_num))
        {
            None => {
                // If no list exists for this anchor & application,
                // Create and insert the default and additional account.
                // This is because we don't create default accounts explicitly.
                let additional_account_reference =
                    AccountReference::new(Some(account_number), last_used);
                let default_account_reference = AccountReference::new(None, last_used);
                vec![default_account_reference, additional_account_reference]
            }
            Some(existing_storable_list) => {
                // If the list exists, push the new account and reinsert it to memory
                let mut refs_vec: Vec<AccountReference> = existing_storable_list.into();
                refs_vec.push(AccountReference::new(Some(account_number), last_used));
                refs_vec
            }
        };

        self.write_reference_list(anchor_number, app_num, references)?;

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
    /// If the `Account` is default and ALL `Account`s for this origin have been moved or deleted, returns None.
    /// If nothing has ever happened at this origin, returns a default `Account`.
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

        let Some(Some(account_update_result)) = account_update_result? else {
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

        let account_references_key = (anchor_number, application_number);
        let references = match self
            .stable_account_reference_list_memory
            .get(&account_references_key)
        {
            None => {
                // If no list exists for this anchor & application,
                // Create and insert the default account.
                // This is because we don't create default accounts explicitly.
                vec![AccountReference::new(Some(new_account_number), None)]
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
                refs_vec
            }
        };

        self.write_reference_list(anchor_number, application_number, references)?;

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

    /// Reads the persisted JWK cache for the given provider `issuer`, if any.
    pub fn read_openid_jwks(&self, issuer: &str) -> Option<Vec<Jwk>> {
        self.openid_jwks_cache_memory
            .get(&issuer.to_string())
            .map(|stored| stored.keys)
    }

    /// Writes (replacing any previous value) the JWK cache for the given
    /// provider `issuer`. Used both to seed the cache from
    /// `OpenIdConfig.seed_jwks` and to write through fetched keys so they
    /// survive canister upgrades.
    pub fn write_openid_jwks(&mut self, issuer: &str, keys: Vec<Jwk>) {
        self.openid_jwks_cache_memory
            .insert(issuer.to_string(), StorableJwks { keys });
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
                "lookup_account_with_principal".to_string(),
                self.lookup_account_with_principal_memory_wrapper.size(),
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
            (
                "lookup_anchor_with_passkey_pubkey_hash_memory".to_string(),
                self.lookup_anchor_with_passkey_pubkey_hash_memory_wrapper
                    .size(),
            ),
            (
                "lookup_anchor_with_email_recovery_memory".to_string(),
                self.lookup_anchor_with_email_recovery_memory_wrapper.size(),
            ),
            (
                "openid_jwks_cache".to_string(),
                self.openid_jwks_cache_memory_wrapper.size(),
            ),
            (
                "mcp_grant_memory".to_string(),
                self.mcp_grant_memory_wrapper.size(),
            ),
            (
                "mcp_registration_memory".to_string(),
                self.mcp_registration_memory_wrapper.size(),
            ),
            (
                "mcp_config_memory".to_string(),
                self.mcp_config_memory_wrapper.size(),
            ),
            (
                "sso_stable_id_index_memory".to_string(),
                self.sso_stable_id_index_memory_wrapper.size(),
            ),
        ])
    }
}

pub struct CreateSessionParams {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub device_id: SessionDeviceId,
    pub valid_till: Timestamp,
    pub read_only: bool,
    pub now: Timestamp,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct AccountPrincipalIndexBackfillOutcome {
    pub next_cursor: Option<(AnchorNumber, ApplicationNumber)>,
    pub indexed: u64,
    pub is_done: bool,
}

#[cfg(not(test))]
fn canister_id() -> Principal {
    ic_cdk::id()
}

/// `ic_cdk::id()` traps outside a canister, so the unit tests derive principals against
/// a fixed canister id.
#[cfg(test)]
fn canister_id() -> Principal {
    Principal::from_slice(&[0, 0, 0, 0, 0, 0, 0, 7, 1, 1])
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ReferenceListDeltas {
    accounts: i64,
    references: i64,
}

impl ReferenceListDeltas {
    fn between(previous: &[AccountReference], current: &[AccountReference]) -> Self {
        fn counts(references: &[AccountReference]) -> (i64, i64) {
            let stored = references
                .iter()
                .filter(|reference| reference.account_number.is_some())
                .count() as i64;
            (stored, references.len() as i64)
        }

        let (previous_accounts, previous_references) = counts(previous);
        let (current_accounts, current_references) = counts(current);

        Self {
            accounts: current_accounts - previous_accounts,
            references: current_references - previous_references,
        }
    }

    fn is_empty(&self) -> bool {
        self.accounts == 0 && self.references == 0
    }

    fn apply(&self, accounts: u64, references: u64) -> (u64, u64) {
        (
            accounts.saturating_add_signed(self.accounts),
            references.saturating_add_signed(self.references),
        )
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
    SaltNotSet,
    EmptyAccountReferenceList {
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    },
    /// Reclaiming ran and the identity is still at the session cap. Unreachable unless
    /// reclaiming stopped honouring its contract, which is why it is an error rather than a
    /// refused sign-in: the sign-in is the thing this cap must never fail.
    SessionCapNotReclaimed {
        anchor_number: AnchorNumber,
    },
    /// Tried to bind a recovery email that's already on a different
    /// anchor. The "one anchor per address" invariant from design
    /// §8.2 is enforced at the storage layer; the caller surfaces
    /// `EmailChallengeError::AddressAlreadyRegistered`.
    EmailRecoveryAddressAlreadyBound {
        existing_anchor: AnchorNumber,
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
            Self::SaltNotSet => write!(
                f,
                "the salt is not set, so an account principal cannot be derived"
            ),
            Self::EmptyAccountReferenceList {
                anchor_number,
                application_number,
            } => write!(
                f,
                "refusing to store an empty account reference list for anchor {anchor_number} at application {application_number}"
            ),
            Self::EmailRecoveryAddressAlreadyBound { existing_anchor } => write!(
                f,
                "recovery email is already bound to a different anchor ({existing_anchor})",
            ),
            Self::SessionCapNotReclaimed { anchor_number } => write!(
                f,
                "anchor {anchor_number} is at the session cap and reclaiming freed nothing"
            ),
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
