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
use account::{Account, AccountKey, AccountsCounter};
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api::stable::WASM_PAGE_SIZE_IN_BYTES;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::io::Write;
use std::ops::RangeInclusive;
use storable::account_reference_list::{
    StorableAccountReferenceList, StorableAccountReferenceListError,
};
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

use crate::delegation::check_frontend_length;
use crate::openid::OpenIdCredentialKey;
use crate::state::PersistentState;
use crate::stats::event_stats::AggregationKey;
use crate::stats::event_stats::{EventData, EventKey};
use crate::storage::account::AccountReference;
use crate::storage::anchor::Anchor;
use crate::storage::memory_wrapper::MemoryWrapper;
use crate::storage::registration_rates::RegistrationRates;
use crate::storage::storable::account::StorableAccount;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::accounts_counter::StorableAccountsCounter;
use crate::storage::storable::anchor_application_config::AnchorApplicationConfig;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
use crate::storage::storable::recovery_key::StorableRecoveryKey;
use internet_identity_interface::internet_identity::types::*;
use storable::anchor::StorableAnchor;
use storable::anchor_number::StorableAnchorNumber;
use storable::application::StorableApplication;
use storable::credential_id::StorableCredentialId;
use storable::discrepancy_counter::StorableDiscrepancyCounter;
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

// The bucket size 128 is relatively low, to avoid wasting memory when using
// multiple virtual memories for smaller amounts of data.
// This value results in 256 GB of total managed memory, which should be enough
// for the foreseeable future.
/// Named accounts one identity may hold, across every origin.
///
/// Each costs a stored record and a seed, so this is what bounds an identity's share of
/// the canister.
pub const MAX_ANCHOR_ACCOUNTS: u64 = 500;

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
    next_application_number_memory: StableCell<StorableApplicationNumber, ManagedMemory<M>>,
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
            next_application_number_memory: StableCell::init(next_application_number_memory, 0)
                .expect("next_application_number_memory"),
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

    /// This identity's account references at `application_number`.
    ///
    /// An absent list normalises to the derived default: nothing has happened at this
    /// origin, so the identity still has the default it has always had. A stored empty
    /// list is a tombstone and stays empty — everything here moved away and the default
    /// must never be derived again.
    ///
    /// Absence and emptiness are opposites, and this is the only place that knows it.
    fn account_references(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> Vec<AccountReference> {
        self.stored_account_references(anchor_number, application_number)
            .unwrap_or_else(Self::derived_default_references)
    }

    /// [`Self::account_references`] for a caller that has an origin rather than an
    /// application number. An origin nothing has ever been stored under has no list, so
    /// it normalises the same way.
    fn account_references_for_origin(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
    ) -> Vec<AccountReference> {
        match self.lookup_application_number_with_origin(origin) {
            Some(application_number) => self.account_references(anchor_number, application_number),
            None => Self::derived_default_references(),
        }
    }

    /// This identity's account state at one origin, in the shape a write takes it.
    ///
    /// An origin nothing has been stored under normalises to the derived default, so no
    /// caller builds one and the rule that absence means the derived default stays in
    /// here. Writing this value back unchanged changes nothing.
    fn account_state_for_origin(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
    ) -> AccountReferenceListWrite {
        (
            self.account_references_for_origin(anchor_number, origin)
                .into_iter()
                .map(AccountReferenceWrite::from)
                .collect(),
            None,
        )
    }

    /// Writes this identity's account state, at one origin or several.
    ///
    /// Every refusal for every origin happens before the first store, so a write that
    /// spans origins cannot half-happen. Returning `Err` on the IC commits whatever was
    /// written before it and only a trap rolls back, so a refusal here must leave
    /// nothing behind. There is deliberately no form of this that writes one origin at a
    /// time, because that is the shape that gets it wrong.
    ///
    /// A write says what the identity holds afterwards rather than patching what it
    /// holds now. Each origin's account references are diffed against what is stored to
    /// derive the counters and the application's own totals, so no caller states any of
    /// them and none can forget one.
    ///
    /// The gate speaks origins. An application number is storage's handle for an origin
    /// and this owns that mapping, so an origin nothing is stored under is created here,
    /// application and all, with its counters already right. A refusal cannot leave
    /// behind an application no counter will ever retire.
    ///
    /// An account reference carrying a record but no number is an account being named: a
    /// name is what a person gives an account, the number is what storage keys it by, so
    /// the number is minted here too and a refusal mints nothing.
    ///
    /// Returns what the identity now holds — the input with the minted account numbers
    /// filled in, each list in the order it was given.
    fn write_account_state(
        &mut self,
        anchor_number: AnchorNumber,
        writes: BTreeMap<FrontendHostname, AccountReferenceListWrite>,
    ) -> Result<BTreeMap<FrontendHostname, AccountReferenceListWrite>, StorageError> {
        let validated = self.validate_account_state(anchor_number, writes)?;
        Ok(self.apply_account_state(anchor_number, validated))
    }

    /// Everything that can refuse. Reads what is stored, works out what would be minted
    /// without minting it, and hands apply something that cannot fail.
    fn validate_account_state(
        &self,
        anchor_number: AnchorNumber,
        writes: BTreeMap<FrontendHostname, AccountReferenceListWrite>,
    ) -> Result<ValidatedAccountStateWrite, StorageError> {
        let mut minting = MintingState {
            next_application_number: self.next_application_number()?,
            global: self.stable_account_counter_memory.get().clone(),
        };

        let mut validated = Vec::with_capacity(writes.len());
        for (origin, write) in writes {
            validated.push(self.validate_account_reference_list(
                anchor_number,
                origin,
                write,
                &mut minting,
            )?);
        }

        // The anchor's counter and the global reference count are shared by every origin
        // in this call, so they are folded here rather than per write. Each delta
        // computed against the same stored value and applied on its own would keep only
        // the last of them, and deltas that each fit can still sum to one that does not:
        // applying them to a running total is what checks the sum rather than the parts.
        let stored_anchor = self
            .stable_anchor_account_counter_memory
            .get(&anchor_number)
            .unwrap_or_default();
        let mut anchor_accounts = stored_anchor.stored_accounts;
        let mut anchor_references = stored_anchor.stored_account_references;
        let mut global_references = minting.global.stored_account_references;
        for one in &validated {
            let (accounts, references) = one.deltas.apply(
                ReferenceCounter::Anchor { anchor_number },
                anchor_accounts,
                anchor_references,
            )?;
            anchor_accounts = accounts;
            anchor_references = references;
            global_references = one.deltas.apply_one(
                ReferenceCounter::Global,
                ReferenceCount::References,
                global_references,
            )?;
        }

        // The account cap is a rule about the state this write leaves the identity in,
        // not a question for a caller to ask first. Refusing here costs nothing, because
        // nothing has been stored — which is the only reason a rule can live at the end of
        // a write rather than in front of it.
        //
        // A cap bounds growth, so only a write that grows the count can be refused by it.
        // Asking about the resulting count alone would refuse a write that adds nothing —
        // a sign-in stamp has a zero delta and goes through here — and would refuse a write
        // that *removes* accounts while the count was still over, which is the one write
        // that would fix being over. Nothing reaches either state while this cap is fixed,
        // and lowering it is what would: the identities above the new cap would keep their
        // accounts, as a lowered cap should mean, rather than lose their sign-in.
        let accounts_delta: i64 = validated.iter().map(|one| one.deltas.accounts).sum();
        if accounts_delta > 0 && anchor_accounts > MAX_ANCHOR_ACCOUNTS {
            return Err(StorageError::AccountLimitReached { anchor_number });
        }

        Ok(ValidatedAccountStateWrite {
            writes: validated,
            anchor_counter: StorableAccountsCounter {
                stored_accounts: anchor_accounts,
                stored_account_references: anchor_references,
            },
            // Only the reference count moves here: the global account count is the
            // account-number allocator, which minting above has already advanced.
            global_counter: StorableAccountsCounter {
                stored_accounts: minting.global.stored_accounts,
                stored_account_references: global_references,
            },
            next_application_number: minting.next_application_number,
        })
    }

    /// One origin's worth of validation.
    fn validate_account_reference_list(
        &self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        (mut writes, config): AccountReferenceListWrite,
        minting: &mut MintingState,
    ) -> Result<ValidatedAccountReferenceListWrite, StorageError> {
        let stored_number = self.lookup_application_number_with_origin(&origin);
        let stored = stored_number.and_then(|application_number| {
            self.stored_account_references(anchor_number, application_number)
        });
        let previous_holds_tracked_default = match &stored {
            Some(references) => references
                .iter()
                .any(|reference| reference.account_number.is_none()),
            // An absent list normalises to the derived default, which is one.
            None => true,
        };

        // A record on an account reference with no number is an account being named.
        // Minting it here is what keeps the number out of the caller's hands, and what
        // makes a refusal cost nothing: the allocator only moves in apply.
        let mut minted = Vec::new();
        for write in &mut writes {
            if write.account_reference.account_number.is_none() && write.record.is_some() {
                let account_number = minting.allocate_account_number()?;
                write.account_reference.account_number = Some(account_number);
                minted.push(account_number);
            }
        }

        let references: Vec<AccountReference> = writes
            .iter()
            .map(|write| write.account_reference.clone())
            .collect();

        // Nothing changed, so nothing is written and nothing about it is checked. Two
        // ways of saying the same thing: the stored list already holds these bytes, or
        // nothing is stored and this says only what absence already says. The second is
        // what keeps a read-change-write that touched nothing from materialising a list.
        let unchanged = stored.as_deref() == Some(references.as_slice());
        // An empty list is not "nothing worth storing" — it is a tombstone, which nothing
        // may create yet, so it has to reach the refusal below rather than be skipped
        // here. `all` on an empty list is true, which is exactly how it would not.
        let records_nothing = stored.is_none()
            && !references.is_empty()
            && references
                .iter()
                .all(|reference| reference.account_number.is_none());
        let writes_a_list = !(unchanged || records_nothing);

        let records: Vec<(AccountNumber, StorableAccount)> = writes
            .iter()
            .filter_map(|write| {
                let record = write.record.clone()?;
                // Every record was given a number above where it lacked one.
                let account_number = write.account_reference.account_number?;
                Some((account_number, record))
            })
            .collect();

        // Naming a tracked default is what makes the named account this identity's
        // default here: the tracked default is gone and the account that replaced it is
        // what the identity signs in with. The caller cannot say so itself, because the
        // number was minted above — so it is derived from the write rather than stated.
        // Adding a named account beside a default that is still there is not this: the
        // list still holds a numberless account reference.
        let config = match config {
            Some(config) => Some(config),
            None if previous_holds_tracked_default
                && minted.len() == 1
                && !references
                    .iter()
                    .any(|reference| reference.account_number.is_none()) =>
            {
                Some(AnchorApplicationConfig {
                    default_account_number: Some(minted[0]),
                })
            }
            None => None,
        };

        // Only a list or a config is stored against an application: a record is keyed by
        // its account number and needs none. So a rename, which leaves every account
        // reference where it was, does not check the application either — which is what
        // keeps it as cheap as it looks.
        //
        // Nothing to store against one means nothing is checked, and that is what lets a
        // write that changed nothing cost nothing: reading the whole of an identity's
        // state and writing it straight back is a no-op rather than a sweep of writes.
        if !writes_a_list && config.is_none() {
            return Ok(ValidatedAccountReferenceListWrite {
                written: (writes, None),
                origin,
                application_number: None,
                application: None,
                list: None,
                records,
                config: None,
                deltas: ReferenceListDeltas::default(),
            });
        }

        // An origin nothing is stored under gets its application here. Creating one for a
        // write that left nothing behind would leave an application no counter will ever
        // retire, since retirement only ever runs off a write to its account state —
        // which is why this sits after the check above rather than before it.
        let application_number = match stored_number {
            Some(application_number) => application_number,
            None => minting.allocate_application_number()?,
        };

        // An origin index pointing at an application that is gone is a broken invariant
        // rather than a new origin, so it still refuses rather than quietly creating one.
        let application = match (
            stored_number,
            self.stable_application_memory.get(&application_number),
        ) {
            (Some(_), Some(application)) => application,
            (Some(_), None) => {
                return Err(StorageError::OriginNotFoundForApplicationNumber { application_number })
            }
            (None, _) => StorableApplication {
                origin: origin.clone(),
                stored_accounts: 0,
                stored_account_references: 0,
            },
        };

        let (list, deltas) = if writes_a_list {
            // Refuses a list this identity may not store — see
            // [`StorableAccountReferenceList::try_from`], which is where that is
            // enforced and why it is enforced there.
            let storable =
                StorableAccountReferenceList::try_from(references.clone()).map_err(|error| {
                    StorageError::UnstorableAccountReferenceList {
                        anchor_number,
                        application_number,
                        error,
                    }
                })?;
            let deltas =
                ReferenceListDeltas::between(stored.as_deref().unwrap_or_default(), &references);
            (Some(storable), deltas)
        } else {
            (None, ReferenceListDeltas::default())
        };

        let (application_accounts, application_references) = deltas.apply(
            ReferenceCounter::Application { application_number },
            application.stored_accounts,
            application.stored_account_references,
        )?;

        Ok(ValidatedAccountReferenceListWrite {
            written: (writes, config.clone()),
            origin,
            application_number: Some(application_number),
            application: Some(StorableApplication {
                origin: application.origin,
                stored_accounts: application_accounts,
                stored_account_references: application_references,
            }),
            list,
            records,
            config,
            deltas,
        })
    }

    /// Stores what was validated, and everything derived from it.
    ///
    /// Cannot refuse: every read it needed happened in validate, and the two cells it
    /// sets hold fixed-size values that were read out of them, so a failure to set one
    /// is a broken invariant rather than a case to report.
    fn apply_account_state(
        &mut self,
        anchor_number: AnchorNumber,
        validated: ValidatedAccountStateWrite,
    ) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
        let ValidatedAccountStateWrite {
            writes,
            anchor_counter,
            global_counter,
            next_application_number,
        } = validated;

        self.stable_account_counter_memory
            .set(global_counter)
            .expect("the account counter is a fixed-size value read from this cell");
        self.next_application_number_memory
            .set(next_application_number)
            .expect("the application number allocator is a fixed-size value read from this cell");
        self.stable_anchor_account_counter_memory
            .insert(anchor_number, anchor_counter);

        let mut written = BTreeMap::new();
        for one in writes {
            let ValidatedAccountReferenceListWrite {
                origin,
                application_number,
                application,
                list,
                records,
                config,
                written: result,
                ..
            } = one;

            let Some((application_number, application)) = application_number.zip(application)
            else {
                // No list and no config, so nothing is keyed by an application here. A
                // record still is — by its own account number — so a rename lands.
                for (account_number, record) in records {
                    self.stable_account_memory.insert(account_number, record);
                }
                written.insert(origin, result);
                continue;
            };

            // The application and its counters go in together, so a list is never stored
            // against an application whose totals do not know about it, and an
            // application is never stored without something holding it.
            self.lookup_application_with_origin_memory.insert(
                StorableOriginSha256::from_origin(&origin),
                application_number,
            );
            self.stable_application_memory
                .insert(application_number, application);

            if let Some(list) = list {
                self.stable_account_reference_list_memory
                    .insert((anchor_number, application_number), list);
            }
            for (account_number, record) in records {
                self.stable_account_memory.insert(account_number, record);
            }
            if let Some(config) = config {
                self.stable_anchor_application_config_memory
                    .insert((anchor_number, application_number), config);
            }

            written.insert(origin, result);
        }

        written
    }

    /// The number the next application would be given, without giving it out.
    ///
    /// The counter only ever climbs, so a number it has passed is never offered again
    /// even after the application is retired. Its value is not the whole answer only
    /// because it postdates the applications numbered before it existed, so the highest
    /// stored number is taken as a floor.
    fn next_application_number(&self) -> Result<ApplicationNumber, StorageError> {
        let above_highest_stored = match self.stable_application_memory.last_key_value() {
            Some((highest, _)) => highest
                .checked_add(1)
                .ok_or(StorageError::ApplicationsCounterOverflow)?,
            None => 0,
        };
        Ok(ApplicationNumber::max(
            *self.next_application_number_memory.get(),
            above_highest_stored,
        ))
    }

    /// What an identity holds where nothing is stored: the default it has always had,
    /// derived from the origin rather than kept.
    fn derived_default_references() -> Vec<AccountReference> {
        vec![AccountReference {
            account_number: None,
            last_used: None,
        }]
    }

    /// The list as stored, with no default derived for an absent one.
    ///
    /// Only the write path may see this. The counters describe stored lists, so a list
    /// that never existed must not be diffed against as though it held the default.
    fn stored_account_references(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
    ) -> Option<Vec<AccountReference>> {
        self.stable_account_reference_list_memory
            .get(&(anchor_number, application_number))
            .map(Vec::<AccountReference>::from)
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

    // Read by tests only: the caps are the write path's rules now, so nothing in
    // production asks a counter what it may do.
    #[cfg(test)]
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

    /// Returns all account references associated with a single anchor number, across all applications.
    #[cfg(test)]
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

    /// Retrieves the discrepancy counter
    pub fn get_discrepancy_counter(&self) -> &StorableDiscrepancyCounter {
        self.stable_account_counter_discrepancy_counter_memory.get()
    }

    /// One account this identity holds at `key.origin`, or `None` where it holds none.
    ///
    /// `key.account_number` names it, `None` being the tracked default. Answering
    /// `None` is the ownership check: an account belongs to whichever identity's list
    /// names it, so a caller that finds no reference here has no claim on the account
    /// whether or not it exists.
    ///
    /// The `Account` returned carries the seed the account signs with, so this is also
    /// the only place that capability is handed out — a caller that holds one has been
    /// through the check above.
    pub fn read_account(&self, key: &AccountKey) -> Option<Account> {
        check_frontend_length(&key.origin);

        let reference = self
            .account_references_for_origin(key.anchor_number, &key.origin)
            .into_iter()
            .find(|reference| reference.account_number == key.account_number)?;

        self.account_for_reference(key.anchor_number, &key.origin, &reference)
    }

    /// Every account this identity holds at `origin`.
    pub fn list_accounts(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
    ) -> Vec<Account> {
        check_frontend_length(origin);

        self.account_references_for_origin(anchor_number, origin)
            .iter()
            .filter_map(|reference| self.account_for_reference(anchor_number, origin, reference))
            .collect()
    }

    /// The account one reference names, or `None` where its record is gone.
    fn account_for_reference(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
        reference: &AccountReference,
    ) -> Option<Account> {
        // The tracked default has no record: its name is the origin's and its seed is
        // the anchor's, both derived rather than stored.
        let Some(account_number) = reference.account_number else {
            return Some(Account::new_with_last_used(
                anchor_number,
                origin.clone(),
                None,
                None,
                reference.last_used,
            ));
        };

        let storable_account = self.stable_account_memory.get(&account_number)?;
        Some(Account::new_full(
            anchor_number,
            origin.clone(),
            Some(storable_account.name),
            Some(account_number),
            reference.last_used,
            storable_account.seed_from_anchor,
        ))
    }

    /// Creates an account named `name` at `origin` for this identity.
    ///
    /// The only place an account number is minted, and it takes none. A number that
    /// nothing references can only be allocated here, never adopted — see
    /// [`Self::write_account`] for what adopting one would hand a caller.
    pub fn create_account(
        &mut self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: String,
    ) -> Result<Account, StorageError> {
        check_frontend_length(&origin);

        // An absent list normalises to the derived default, which is how the first named
        // account at an origin does not cost the identity the default it had. A
        // tombstone normalises to nothing and stays that way.
        let (mut account_references, config) =
            self.account_state_for_origin(anchor_number, &origin);
        // Where the write leaves it, and so where its minted number comes back.
        let created = account_references.len();
        account_references.push(AccountReferenceWrite {
            account_reference: AccountReference {
                account_number: None,
                // Set when the identity signs in with the account.
                last_used: None,
            },
            // A record on an account reference with no number is an account being named,
            // and naming it is what mints it one.
            record: Some(StorableAccount {
                name: name.clone(),
                seed_from_anchor: None,
            }),
        });

        let written = self.write_account_state(
            anchor_number,
            BTreeMap::from([(origin.clone(), (account_references, config))]),
        )?;

        Ok(Account::new(
            anchor_number,
            origin.clone(),
            Some(name),
            written[&origin].0[created].account_reference.account_number,
        ))
    }

    /// Stores an account read back from [`Self::read_account`].
    ///
    /// Renaming one, naming the tracked default, and recording that an account was used
    /// are the same read-modify-write: the account is the state to store, not a patch
    /// over it, so what it carries is what the list ends up holding.
    ///
    /// A number no reference names is [`StorageError::AccountNotFound`] and never a
    /// create. `update_account_for_origin` takes its account number straight from the
    /// client, so a write that adopted an unreferenced number would hand a caller a
    /// reference to another identity's account, and with it that account's principal.
    pub fn write_account(&mut self, account: Account) -> Result<Account, StorageError> {
        check_frontend_length(&account.origin);

        let Account {
            account_number,
            anchor_number,
            origin,
            last_used,
            name,
            ..
        } = account;

        // An origin nothing has been stored under still holds the derived default, so
        // naming that default lands there and creates the application. Anything else has
        // no account reference to write against.
        let names_the_tracked_default = account_number.is_none() && name.is_some();
        if self
            .lookup_application_number_with_origin(&origin)
            .is_none()
            && !names_the_tracked_default
        {
            return match account_number {
                Some(account_number) => Err(StorageError::AccountNotFound { account_number }),
                // The default here is still derived rather than stored, so there is no
                // account reference to record its use against.
                None => Ok(Account::new_with_last_used(
                    anchor_number,
                    origin,
                    None,
                    None,
                    last_used,
                )),
            };
        }

        let (mut account_references, config) =
            self.account_state_for_origin(anchor_number, &origin);
        let Some(position) = account_references
            .iter()
            .position(|write| write.account_reference.account_number == account_number)
        else {
            // Holding an account reference is what grants access, so a miss means this
            // identity does not have the account. For the tracked default it means the
            // list is a tombstone or the default was named and is no longer numberless —
            // neither can be reconstructed from the origin.
            return Err(match account_number {
                Some(account_number) => StorageError::AccountNotFound { account_number },
                None => StorageError::MissingAccount {
                    anchor_number,
                    name: name.unwrap_or_default(),
                },
            });
        };
        account_references[position].account_reference.last_used = last_used;

        match (account_number, name) {
            // A stored account, whose record carries the name. Only the tracked default
            // goes without one, so an account that has a number and no name is not a
            // state a read can hand back.
            (Some(_), None) => return Err(StorageError::MissingAccountName),
            (Some(account_number), Some(name)) => {
                let Some(mut storable_account) = self.stable_account_memory.get(&account_number)
                else {
                    return Err(StorageError::AccountNotFound { account_number });
                };
                storable_account.name = name;
                account_references[position].record = Some(storable_account);
            }
            // Naming the tracked default is what stores it, and the write mints its
            // number. Its seed stays the anchor's, so the principal this identity already
            // signs in with here is preserved — and the write makes it the default,
            // because that is what naming it means.
            (None, Some(name)) => {
                account_references[position].record = Some(StorableAccount {
                    name,
                    seed_from_anchor: Some(anchor_number),
                });
            }
            // The tracked default, unnamed: nothing to store but the use of an account
            // reference the list already holds.
            (None, None) => {}
        }

        let written = self.write_account_state(
            anchor_number,
            BTreeMap::from([(origin.clone(), (account_references, config))]),
        )?;
        let write = &written[&origin].0[position];
        let account_number = write.account_reference.account_number;

        Ok(match &write.record {
            Some(record) => Account::new_full(
                anchor_number,
                origin.clone(),
                Some(record.name.clone()),
                account_number,
                last_used,
                record.seed_from_anchor,
            ),
            None => Account::new_with_last_used(
                anchor_number,
                origin.clone(),
                None,
                account_number,
                last_used,
            ),
        })
    }

    /// Points this identity's default at `origin` to `account_number`, or clears it
    /// where that is `None`.
    ///
    /// The config and the reference list go through one write, so a config naming a
    /// number no reference names cannot be left behind.
    pub fn set_default_account(
        &mut self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        account_number: Option<AccountNumber>,
    ) -> Result<(), StorageError> {
        check_frontend_length(&origin);

        let (account_references, _) = self.account_state_for_origin(anchor_number, &origin);
        self.write_account_state(
            anchor_number,
            BTreeMap::from([(
                origin,
                (
                    account_references,
                    Some(AnchorApplicationConfig {
                        default_account_number: account_number,
                    }),
                ),
            )]),
        )?;
        Ok(())
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

/// Which of the counters derived from a reference list a delta is applied to.
///
/// Each carries what identifies its list, so a refusal points at the counter that
/// diverged rather than only saying that one did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReferenceCounter {
    /// One identity's totals across every application.
    Anchor { anchor_number: AnchorNumber },
    /// The canister-wide gauge.
    Global,
    /// One application's totals across every identity.
    Application {
        application_number: ApplicationNumber,
    },
}

impl fmt::Display for ReferenceCounter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Anchor { anchor_number } => write!(f, "anchor {anchor_number}"),
            Self::Global => write!(f, "global"),
            Self::Application { application_number } => {
                write!(f, "application {application_number}")
            }
        }
    }
}

/// Which of the two counts every reference-list counter holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReferenceCount {
    /// Named accounts — references that carry an account number.
    Accounts,
    /// References, named and tracked-default alike.
    References,
}

impl fmt::Display for ReferenceCount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Accounts => write!(f, "stored accounts"),
            Self::References => write!(f, "stored account references"),
        }
    }
}

/// How one write to a account reference list moves the counters derived from it.
///
/// Signed because these are differences rather than totals: a write that drops a
/// reference has to move the counters down, and there is no unsigned way to say so.
/// Both are applied to `u64` totals by [`ReferenceListDeltas::apply`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
struct ReferenceListDeltas {
    /// Change in named accounts — references that carry an account number.
    accounts: i64,
    /// Change in references, named and tracked-default alike.
    references: i64,
}

/// One account as this identity holds it at one application: the account reference by
/// which the identity holds the account, and the account's own stored record where this
/// write touches it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AccountReferenceWrite {
    pub account_reference: AccountReference,
    /// The account's stored record, where this write touches it. Together with the
    /// account reference's number this says what the write means:
    ///
    /// | number | record | meaning |
    /// | --- | --- | --- |
    /// | `None` | `None` | the tracked default, and it stays one |
    /// | `None` | `Some` | name it: the write mints a number and stores the record |
    /// | `Some` | `Some` | rename |
    /// | `Some` | `None` | leave the stored record alone |
    pub record: Option<StorableAccount>,
}

impl From<AccountReference> for AccountReferenceWrite {
    fn from(account_reference: AccountReference) -> Self {
        Self {
            account_reference,
            record: None,
        }
    }
}

/// One application's worth of a write: what the identity holds there afterwards, and the
/// config it holds it under.
///
/// The config is `None` where the write leaves the stored one alone.
pub type AccountReferenceListWrite = (Vec<AccountReferenceWrite>, Option<AnchorApplicationConfig>);

/// One origin's write, past every refusal it can make on its own.
struct ValidatedAccountReferenceListWrite {
    origin: FrontendHostname,
    /// `None` where this write stores nothing at this origin and the origin has no
    /// application, so none is created for it.
    application_number: Option<ApplicationNumber>,
    /// The application as it will be stored, counters included, whether it existed
    /// before this write or is created by it.
    application: Option<StorableApplication>,
    /// `None` where the list is not written: it already holds these bytes, or nothing is
    /// stored and this says only what absence already says.
    list: Option<StorableAccountReferenceList>,
    records: Vec<(AccountNumber, StorableAccount)>,
    config: Option<AnchorApplicationConfig>,
    deltas: ReferenceListDeltas,
    /// What this origin holds afterwards, handed back to the caller.
    written: AccountReferenceListWrite,
}

/// A whole write, past every refusal — including the ones only the whole call can make.
struct ValidatedAccountStateWrite {
    writes: Vec<ValidatedAccountReferenceListWrite>,
    anchor_counter: StorableAccountsCounter,
    global_counter: StorableAccountsCounter,
    next_application_number: ApplicationNumber,
}

/// The numbers a write will hand out, tracked across the whole call so that two origins
/// in one write cannot be given the same one.
///
/// Nothing here is stored until apply, which is what makes a refusal cost nothing.
struct MintingState {
    next_application_number: ApplicationNumber,
    /// `stored_accounts` is the account-number allocator as well as the global count.
    global: StorableAccountsCounter,
}

impl MintingState {
    fn allocate_application_number(&mut self) -> Result<ApplicationNumber, StorageError> {
        let application_number = self.next_application_number;
        self.next_application_number = application_number
            .checked_add(1)
            .ok_or(StorageError::ApplicationsCounterOverflow)?;
        Ok(application_number)
    }

    /// The counter is also the account number, so it must not wrap or saturate: either
    /// would re-issue a number already in use, and two accounts at one origin would
    /// derive the same principal.
    fn allocate_account_number(&mut self) -> Result<AccountNumber, StorageError> {
        let account_number = self
            .global
            .stored_accounts
            .checked_add(1)
            .ok_or(StorageError::AccountsCounterOverflow)?;
        self.global.stored_accounts = account_number;
        Ok(account_number)
    }
}

impl ReferenceListDeltas {
    /// What writing `new_references` over `previous_references` does to the counters.
    ///
    /// A list that does not exist and one holding nothing both count as no references,
    /// which is right for these totals: neither contributes any. It is also why
    /// retiring a list must not go through here — a tombstone's list is still alive while
    /// holding nothing, so a diff against it would report no change and leave the
    /// counters claiming references the removed list no longer has.
    fn between(
        previous_references: &[AccountReference],
        new_references: &[AccountReference],
    ) -> Self {
        /// Saturating rather than `as`: a list long enough to overflow `i64` cannot
        /// exist — `MAX_ANCHOR_ACCOUNTS` bounds it far below — and saturating says so
        /// without a cast that would wrap silently if that ever stopped being true.
        fn counts(references: &[AccountReference]) -> (i64, i64) {
            let mut named = 0i64;
            let mut total = 0i64;
            for reference in references {
                total = total.saturating_add(1);
                if reference.account_number.is_some() {
                    named = named.saturating_add(1);
                }
            }
            (named, total)
        }

        let (previous_named, previous_total) = counts(previous_references);
        let (new_named, new_total) = counts(new_references);

        Self {
            accounts: new_named.saturating_sub(previous_named),
            references: new_total.saturating_sub(previous_total),
        }
    }

    /// Both counts of `counter`, moved by this delta.
    ///
    /// Refuses rather than clamping: an under-run means the counters and the stored
    /// lists have already diverged, and a clamped zero reads as "no anchor references
    /// this application any more", which retires a list other anchors still point at.
    fn apply(
        &self,
        counter: ReferenceCounter,
        num_accounts: u64,
        num_references: u64,
    ) -> Result<(u64, u64), StorageError> {
        Ok((
            self.apply_one(counter, ReferenceCount::Accounts, num_accounts)?,
            self.apply_one(counter, ReferenceCount::References, num_references)?,
        ))
    }

    /// One count of one counter, moved by this delta.
    ///
    /// The refusal names the counter, the count, the value stored and the delta that
    /// would not fit, because that is the whole of what diverged and there is nothing
    /// left to read it off afterwards: the write is refused, so the counters keep the
    /// values that disagreed with the lists.
    fn apply_one(
        &self,
        counter: ReferenceCounter,
        count: ReferenceCount,
        stored: u64,
    ) -> Result<u64, StorageError> {
        let delta = match count {
            ReferenceCount::Accounts => self.accounts,
            ReferenceCount::References => self.references,
        };
        stored
            .checked_add_signed(delta)
            .ok_or(StorageError::AccountCounterOutOfBounds {
                counter,
                count,
                stored,
                delta,
            })
    }
}

#[derive(Debug)]
pub enum StorageError {
    AccountLimitReached {
        anchor_number: AnchorNumber,
    },
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
    AccountsCounterOverflow,
    /// No application numbers left to hand out. Refused rather than saturated: the
    /// number keys the application and the origin index, so reissuing one would
    /// put two origins on a single list.
    ApplicationsCounterOverflow,
    ErrorUpdatingApplicationNumberAllocator,
    /// The references a write assembled cannot be stored as they stand.
    UnstorableAccountReferenceList {
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        error: StorableAccountReferenceListError,
    },
    /// A counter derived from a reference list cannot move by the delta a write
    /// implies, which means it and the stored lists have already diverged.
    AccountCounterOutOfBounds {
        counter: ReferenceCounter,
        count: ReferenceCount,
        stored: u64,
        delta: i64,
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
            Self::AccountLimitReached { anchor_number } => write!(
                f,
                "identity {anchor_number} already holds as many named accounts as it may"
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
            Self::AccountsCounterOverflow => write!(f, "No account numbers left to allocate"),
            Self::ApplicationsCounterOverflow => {
                write!(f, "No application numbers left to allocate")
            }
            Self::ErrorUpdatingApplicationNumberAllocator => {
                write!(f, "Error updating the application number allocator")
            }
            Self::UnstorableAccountReferenceList {
                anchor_number,
                application_number,
                error,
            } => write!(
                f,
                "the account reference list for anchor {anchor_number} at application \
                 {application_number} cannot be stored: {error}"
            ),
            Self::AccountCounterOutOfBounds {
                counter,
                count,
                stored,
                delta,
            } => write!(
                f,
                "the {counter} {count} counter cannot move from {stored} by {delta}, \
                 so it no longer agrees with the stored reference lists"
            ),
            Self::EmailRecoveryAddressAlreadyBound { existing_anchor } => write!(
                f,
                "recovery email is already bound to a different anchor ({existing_anchor})",
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
