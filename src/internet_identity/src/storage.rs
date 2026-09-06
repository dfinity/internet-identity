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
use account::{Account, AccountKey, AccountsCounter, SessionRecordKey};
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

use crate::delegation::calculate_session_seed_with_salt;
use crate::delegation::{self, check_frontend_length};
use crate::openid::OpenIdCredentialKey;
use crate::state::PersistentState;
use crate::stats::event_stats::AggregationKey;
use crate::stats::event_stats::{EventData, EventKey};
use crate::storage::account::{
    AccountReference, SessionRecord, DEFAULT_SESSION_IDLE_NS, MIN_SESSION_IDLE_NS,
};
use crate::storage::anchor::{Anchor, BrowserError};
use crate::storage::memory_wrapper::MemoryWrapper;
use crate::storage::registration_rates::RegistrationRates;
use crate::storage::storable::account::StorableAccount;
use crate::storage::storable::account_key::StorableAccountKey;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::accounts_counter::StorableAccountsCounter;
use crate::storage::storable::anchor_application_config::AnchorApplicationConfig;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
use crate::storage::storable::recovery_key::StorableRecoveryKey;
use crate::storage::storable::session_handle::StorableSessionHandle;
use crate::storage::storable::session_id::StorableSessionId;
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
const LOOKUP_ACCOUNT_WITH_PRINCIPAL_MEMORY_INDEX: u8 = 34u8;
const LOOKUP_SESSION_WITH_PRINCIPAL_MEMORY_INDEX: u8 = 35u8;
const NEXT_SESSION_ID_MEMORY_INDEX: u8 = 36u8;

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

/// Monotonic [`SessionId`] allocator. A revoked session's id is retired, never reissued,
/// which is what makes the revocation final: the id is an input to the session seed.
const NEXT_SESSION_ID_MEMORY_ID: MemoryId = MemoryId::new(NEXT_SESSION_ID_MEMORY_INDEX);

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

/// Per-anchor cap on account reference lists that hold nothing but a tracked default
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
/// lists runs once and then not again for the next fifty sign-ins.
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
    next_session_id_memory: StableCell<StorableSessionId, ManagedMemory<M>>,
    lookup_account_with_principal_memory_wrapper: MemoryWrapper<ManagedMemory<M>>,
    lookup_account_with_principal_memory:
        StableBTreeMap<Principal, StorableAccountKey, ManagedMemory<M>>,
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
        let next_session_id_memory = memory_manager.get(NEXT_SESSION_ID_MEMORY_ID);
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
            next_session_id_memory: StableCell::init(next_session_id_memory, 0)
                .expect("next_session_id_memory"),
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
            browsers: _,
            next_browser_id: _,
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

    /// Drops what the identity no longer needs from the state a write is about to store:
    /// every session that is over, and — where that is still not enough — the ones least
    /// demonstrated to be in use, down to the watermark.
    ///
    /// Policy rather than a rule, which is why it shapes the write instead of living in
    /// the write path. Storage refuses a state over the cap; which live sessions give way
    /// to make room is this function's opinion, and [`SessionRecord::reclaim_order`] is
    /// where that opinion is written down. Clearing to the watermark rather than to the
    /// cap is what keeps the next few sign-ins from each sweeping again.
    ///
    /// Triggered on what is *stored*, expired records included, because that is what
    /// occupies the cap: a session can expire with no write anywhere, so nothing has
    /// removed it and it still holds its slot.
    fn reclaim_sessions(
        state: &mut BTreeMap<FrontendHostname, AccountReferenceListWrite>,
        now: Timestamp,
    ) {
        fn sessions_of(
            state: &BTreeMap<FrontendHostname, AccountReferenceListWrite>,
        ) -> impl Iterator<Item = &SessionRecord> {
            state
                .values()
                .flatten()
                .flat_map(|(account_references, _)| account_references)
                .flat_map(|write| &write.account_reference.sessions)
        }

        fn retain(
            state: &mut BTreeMap<FrontendHostname, AccountReferenceListWrite>,
            mut keep: impl FnMut(&SessionRecord) -> bool,
        ) {
            for held in state.values_mut() {
                let Some((account_references, _)) = held else {
                    continue;
                };
                for write in account_references.iter_mut() {
                    write.account_reference.sessions.retain(&mut keep);
                }
            }
        }

        if (sessions_of(state).count() as u32) < MAX_SESSIONS_PER_ANCHOR {
            return;
        }

        retain(state, |session| !session.is_over(now));

        let mut live: Vec<((bool, Timestamp, SessionId), SessionId)> = sessions_of(state)
            .map(|session| (session.reclaim_order(now), session.session_id))
            .collect();
        if (live.len() as u32) <= SESSIONS_WATERMARK_PER_ANCHOR {
            return;
        }

        // Ascending, so the least demonstrated use comes first and is given up first.
        live.sort();
        let over_watermark = live.len() - SESSIONS_WATERMARK_PER_ANCHOR as usize;
        let giving_up: BTreeSet<SessionId> = live
            .into_iter()
            .take(over_watermark)
            .map(|(_, session_id)| session_id)
            .collect();
        retain(state, |session| !giving_up.contains(&session.session_id));
    }

    /// How many sessions a set of account references holds.
    fn sessions_in(references: &[AccountReference]) -> u32 {
        references
            .iter()
            .map(|reference| reference.sessions.len() as u32)
            .sum()
    }

    /// How many sessions this identity holds, counted from its account reference lists.
    ///
    /// Counted rather than read off the anchor: a session can expire with no write
    /// anywhere, so the stored count drifts upwards and the cap must not be enforced
    /// against a number nobody counted.
    fn stored_session_count(&self, anchor_number: AnchorNumber) -> u32 {
        self.stable_account_reference_list_memory
            .range(
                (anchor_number, ApplicationNumber::MIN)..=(anchor_number, ApplicationNumber::MAX),
            )
            .map(|(_, list)| Self::sessions_in(&Vec::<AccountReference>::from(list)))
            .sum()
    }

    /// Everything this identity has stored, keyed the way a write takes it.
    ///
    /// The symmetry is the point: an operation that touches many origins reads this,
    /// changes what it means to change, and writes it back. Nothing has to range over
    /// storage itself, and no application number reaches the caller.
    ///
    /// Stored entries only. An origin nothing has been stored under is not in the map —
    /// there are unboundedly many of those — so a caller that means to write at one
    /// reaches for [`Self::account_state_for_origin`] and puts it there.
    fn account_state(
        &self,
        anchor_number: AnchorNumber,
    ) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
        self.stable_account_reference_list_memory
            .range(
                (anchor_number, ApplicationNumber::MIN)..=(anchor_number, ApplicationNumber::MAX),
            )
            .filter_map(|((_, application_number), list)| {
                // An origin index that no longer resolves leaves a list naming nothing.
                // It is not something a caller can write, so it is not something this
                // hands back.
                let application = self.stable_application_memory.get(&application_number)?;
                let account_references = Vec::<AccountReference>::from(list)
                    .into_iter()
                    .map(AccountReferenceWrite::from)
                    .collect();
                Some((application.origin, Some((account_references, None))))
            })
            .collect()
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
    ) -> (Vec<AccountReferenceWrite>, Option<AnchorApplicationConfig>) {
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
        anchor: Anchor,
        writes: BTreeMap<FrontendHostname, AccountReferenceListWrite>,
    ) -> Result<BTreeMap<FrontendHostname, AccountReferenceListWrite>, StorageError> {
        let validated =
            self.validate_account_state(anchor.anchor_number(), anchor.session_count, writes)?;
        Ok(self.apply_account_state(anchor, validated))
    }

    /// [`Self::write_account_state`] for a test that has an anchor number rather than the
    /// anchor. Production takes the anchor itself, so that a caller cannot hold a copy
    /// across the write and put the session count back afterwards.
    #[cfg(test)]
    fn write_account_state_for_testing(
        &mut self,
        anchor_number: AnchorNumber,
        writes: BTreeMap<FrontendHostname, AccountReferenceListWrite>,
    ) -> Result<BTreeMap<FrontendHostname, AccountReferenceListWrite>, StorageError> {
        let anchor = self.read(anchor_number)?;
        self.write_account_state(anchor, writes)
    }

    /// Everything that can refuse. Reads what is stored, works out what would be minted
    /// without minting it, and hands apply something that cannot fail.
    fn validate_account_state(
        &self,
        anchor_number: AnchorNumber,
        stored_sessions: u32,
        writes: BTreeMap<FrontendHostname, AccountReferenceListWrite>,
    ) -> Result<ValidatedAccountStateWrite, StorageError> {
        let mut minting = MintingState {
            next_application_number: self.next_application_number()?,
            global: self.stable_account_counter_memory.get().clone(),
        };

        let written_origins: BTreeSet<FrontendHostname> = writes.keys().cloned().collect();
        let mut validated = Vec::with_capacity(writes.len());
        for (origin, write) in writes {
            validated.push(self.validate_account_reference_list(
                anchor_number,
                origin,
                write,
                &mut minting,
            )?);
        }

        // Evicting idle tracked defaults belongs here rather than to a caller, for the
        // same reason the counters do: it is a consequence of the write, and a caller that
        // has to remember it is a caller that can forget.
        //
        // It also has to be *this* write. Eviction used to run as a second call once the
        // first had returned, so a write that pushed the identity over the cap was two
        // atomic units — and an `Err` from the second committed the first.
        let stored_anchor = self
            .stable_anchor_account_counter_memory
            .get(&anchor_number)
            .unwrap_or_default();
        for (origin, application_number) in
            self.evictable_after(anchor_number, &stored_anchor, &validated, &written_origins)
        {
            validated.push(self.validate_removal(
                anchor_number,
                origin,
                Some(application_number),
            )?);
        }

        // The anchor's counter and the global reference count are shared by every origin
        // in this call, so they are folded here rather than per write. Each delta
        // computed against the same stored value and applied on its own would keep only
        // the last of them, and deltas that each fit can still sum to one that does not:
        // applying them to a running total is what checks the sum rather than the parts.
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

        // The session cap, for the same reason as the account cap below.
        //
        // The stored count answers it on the common path: a delta over stored records is
        // exact for stored records, so the number on the anchor is the cheap way to ask,
        // and it is what the cap was enforced against before this existed. Counting every
        // list is the fallback for when that number says the identity is at the cap, where
        // it might be divergence rather than truth — and counting is also what brings the
        // stored number back to the truth.
        let session_delta: i64 = validated.iter().map(|one| one.session_delta).sum();
        let session_count = (session_delta != 0).then(|| {
            let moved = stored_sessions.saturating_add_signed(session_delta as i32);
            if moved > MAX_SESSIONS_PER_ANCHOR {
                self.stored_session_count(anchor_number)
                    .saturating_add_signed(session_delta as i32)
            } else {
                moved
            }
        });
        if session_count.is_some_and(|after| after > MAX_SESSIONS_PER_ANCHOR) {
            return Err(StorageError::SessionCapNotReclaimed { anchor_number });
        }

        // The account cap is a rule about the state this write leaves the identity in,
        // not a question for a caller to ask first. Refusing here costs nothing, because
        // nothing has been stored — which is the only reason a rule can live at the end of
        // a write rather than in front of it.
        if anchor_accounts > MAX_ANCHOR_ACCOUNTS {
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
            session_count,
        })
    }

    /// The tracked defaults this write leaves over the cap, as origins to remove.
    ///
    /// Selected against the state the write is about to produce rather than the state on
    /// disk: the deltas it carries are added to the counters here, and every origin the
    /// write touches is excluded, so a list it is in the middle of changing is never also
    /// a victim of it.
    fn evictable_after(
        &self,
        anchor_number: AnchorNumber,
        stored_anchor: &StorableAccountsCounter,
        validated: &[ValidatedAccountReferenceListWrite],
        written_origins: &BTreeSet<FrontendHostname>,
    ) -> Vec<(FrontendHostname, ApplicationNumber)> {
        let (accounts, references) = validated.iter().fold(
            (
                stored_anchor.stored_accounts as i64,
                stored_anchor.stored_account_references as i64,
            ),
            |(accounts, references), one| {
                (
                    accounts + one.deltas.accounts,
                    references + one.deltas.references,
                )
            },
        );
        // Numberless account references, bounded from counters rather than by looking:
        // every account reference that is not a named account is a tracked default.
        let upper_bound = references.saturating_sub(accounts).max(0) as u64;
        if upper_bound < MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            return Vec::new();
        }

        let mut candidates: Vec<_> = self
            .evictable_default_lists(anchor_number)
            .into_iter()
            .filter_map(|(application_number, last_used)| {
                // An origin this write is already changing is not a victim of it, and one
                // whose application is gone would refuse the whole call — housekeeping
                // does not get to fail the write it is riding on.
                let application = self.stable_application_memory.get(&application_number)?;
                (!written_origins.contains(&application.origin)).then_some((
                    last_used,
                    application_number,
                    application.origin,
                ))
            })
            .collect();
        if candidates.len() as u64 <= EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK {
            return Vec::new();
        }

        candidates.sort_by(|a, b| (a.0, a.1).cmp(&(b.0, b.1)));
        let victims = u64::min(
            candidates.len() as u64 - EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK,
            MAX_EVICTIONS_PER_CALL,
        );
        candidates
            .into_iter()
            .take(victims as usize)
            .map(|(_, application_number, origin)| (origin, application_number))
            .collect()
    }

    /// One origin's worth of validation.
    fn validate_account_reference_list(
        &self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        write: AccountReferenceListWrite,
        minting: &mut MintingState,
    ) -> Result<ValidatedAccountReferenceListWrite, StorageError> {
        let stored_number = self.lookup_application_number_with_origin(&origin);
        let Some((mut writes, config)) = write else {
            return self.validate_removal(anchor_number, origin, stored_number);
        };
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

        // Nothing changed, so nothing is written and nothing about it is checked.
        //
        // Against the *normalised* previous rather than the stored one: at an origin
        // nothing is stored under, what the identity holds is the derived default, so a
        // write that says exactly that says only what absence already says. Comparing
        // against the stored list instead would make reading an untouched origin and
        // writing it straight back materialise a tracked default nobody asked for.
        //
        // An empty list is not covered by this and must not be: it is a tombstone, the
        // opposite of absence, and nothing may create one yet. It differs from the
        // derived default, so it falls through to the refusal below.
        let unchanged = references
            == stored
                .clone()
                .unwrap_or_else(Self::derived_default_references);
        // A config names this identity's default at an origin, and a config with no
        // account reference list behind it names nothing — so setting one materialises
        // the list it implies, even where that list is only the derived default. This is
        // the one thing that stores a list a round trip would leave alone, and it is
        // asked for rather than incidental.
        let writes_a_list = !unchanged || (config.is_some() && stored.is_none());

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
                written: Some((writes, None)),
                origin,
                application_number: None,
                application: ApplicationWrite::Untouched,
                list: ListWrite::Untouched,
                records,
                config: None,
                deltas: ReferenceListDeltas::default(),
                principal_salt: None,
                accounts_changed: false,
                sessions_changed: false,
                session_delta: 0,
                previous_references: Vec::new(),
                current_references: Vec::new(),
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
                tombstones: 0,
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
            let deltas = ReferenceListDeltas::between(stored.as_deref(), &references);
            (ListWrite::Stored(storable), deltas)
        } else {
            (ListWrite::Untouched, ReferenceListDeltas::default())
        };

        // A derived principal is a function of the anchor, the origin and the account
        // number, so a write that leaves every account number in place — a `last_used`
        // stamp, which is every sign-in — cannot have changed one. Skipping the sync
        // there keeps the hottest write in the system off a per-account hash.
        let previous_references = stored.clone().unwrap_or_default();
        let accounts_changed = writes_a_list
            && (previous_references.len() != references.len()
                || previous_references
                    .iter()
                    .zip(&references)
                    .any(|(previous, new)| previous.account_number != new.account_number));
        // Resolved here, so a missing salt refuses with nothing written rather than
        // half-way through.
        // Sessions are held on the account references too, and their index and the
        // identity's session count both follow from the same pair of lists.
        let sessions_changed = writes_a_list
            && Self::session_ids_of(&previous_references) != Self::session_ids_of(&references);
        let principal_salt = if accounts_changed || sessions_changed {
            Some(*self.salt().ok_or(StorageError::SaltNotSet)?)
        } else {
            None
        };

        let (application_accounts, application_references) = deltas.apply(
            ReferenceCounter::Application { application_number },
            application.stored_accounts,
            application.stored_account_references,
        )?;
        let application_tombstones = deltas.apply_one(
            ReferenceCounter::Application { application_number },
            ReferenceCount::Tombstones,
            application.tombstones,
        )?;

        Ok(ValidatedAccountReferenceListWrite {
            written: Some((writes, config.clone())),
            origin,
            application_number: Some(application_number),
            application: Self::application_write(
                stored_number.is_none(),
                deltas,
                application_references,
                application_tombstones,
                StorableApplication {
                    origin: application.origin,
                    stored_accounts: application_accounts,
                    stored_account_references: application_references,
                    tombstones: application_tombstones,
                },
            ),
            list,
            records,
            config,
            deltas,
            principal_salt,
            accounts_changed,
            sessions_changed,
            session_delta: Self::sessions_in(&references) as i64
                - Self::sessions_in(&previous_references) as i64,
            previous_references,
            current_references: references,
        })
    }

    /// Retiring an application is the same decision wherever the counters land: nothing
    /// references it, and no tombstone is keeping its number alive on behalf of an
    /// identity that moved its default away.
    fn application_write(
        is_new: bool,
        deltas: ReferenceListDeltas,
        references: u64,
        tombstones: u64,
        application: StorableApplication,
    ) -> ApplicationWrite {
        if is_new {
            // Created by this write, and stored with the counters that hold it.
            return ApplicationWrite::Stored(application);
        }
        if deltas == ReferenceListDeltas::default() {
            // Nothing keyed by the application moved, so it keeps the bytes it has —
            // and, importantly, is not read as unreferenced. A write that only sets a
            // config moves no counter and must not retire the application it names.
            return ApplicationWrite::Untouched;
        }
        if references == 0 && tombstones == 0 {
            ApplicationWrite::Retired(application.origin)
        } else {
            ApplicationWrite::Stored(application)
        }
    }

    /// Removing what an identity holds at one origin.
    ///
    /// A list is retired only when a live tracked default is all it holds. Nothing else
    /// may be pruned, and the rule sits here rather than only in the caller that picks
    /// victims, because this is the irreversible step:
    ///
    /// - an absent list has nothing to remove;
    /// - an empty list is a tombstone, and taking it away would make the default it
    ///   stands for reconstructible again;
    /// - a list holding named accounts, or whose default was named or moved away, would
    ///   lose account references that nothing else records.
    ///
    /// Anything else is left alone rather than refused. Eviction is housekeeping that runs
    /// alongside a sign-in, and refusing the whole write because one victim went stale
    /// would fail the sign-in that triggered it.
    fn validate_removal(
        &self,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        stored_number: Option<ApplicationNumber>,
    ) -> Result<ValidatedAccountReferenceListWrite, StorageError> {
        let untouched = |origin| ValidatedAccountReferenceListWrite {
            written: None,
            origin,
            application_number: None,
            application: ApplicationWrite::Untouched,
            list: ListWrite::Untouched,
            records: Vec::new(),
            config: None,
            deltas: ReferenceListDeltas::default(),
            principal_salt: None,
            accounts_changed: false,
            sessions_changed: false,
            session_delta: 0,
            previous_references: Vec::new(),
            current_references: Vec::new(),
        };

        let Some(application_number) = stored_number else {
            return Ok(untouched(origin));
        };
        let previous = match self
            .stored_account_references(anchor_number, application_number)
            .as_deref()
        {
            Some([reference]) if reference.account_number.is_none() => vec![reference.clone()],
            _ => return Ok(untouched(origin)),
        };
        let application = self
            .stable_application_memory
            .get(&application_number)
            .ok_or(StorageError::OriginNotFoundForApplicationNumber { application_number })?;
        // Every account reference goes, so every principal it derived goes with it —
        // resolved before anything is removed, so a missing salt refuses with the list
        // intact.
        let principal_salt = Some(*self.salt().ok_or(StorageError::SaltNotSet)?);

        let deltas = ReferenceListDeltas::removing(&previous);
        let (application_accounts, application_references) = deltas.apply(
            ReferenceCounter::Application { application_number },
            application.stored_accounts,
            application.stored_account_references,
        )?;
        let application_tombstones = deltas.apply_one(
            ReferenceCounter::Application { application_number },
            ReferenceCount::Tombstones,
            application.tombstones,
        )?;

        Ok(ValidatedAccountReferenceListWrite {
            written: None,
            origin,
            application_number: Some(application_number),
            application: Self::application_write(
                false,
                deltas,
                application_references,
                application_tombstones,
                StorableApplication {
                    origin: application.origin,
                    stored_accounts: application_accounts,
                    stored_account_references: application_references,
                    tombstones: application_tombstones,
                },
            ),
            list: ListWrite::Removed,
            records: Vec::new(),
            config: None,
            deltas,
            principal_salt,
            accounts_changed: true,
            sessions_changed: true,
            session_delta: -(Self::sessions_in(&previous) as i64),
            previous_references: previous,
            current_references: Vec::new(),
        })
    }

    /// Stores what was validated, and everything derived from it.
    ///
    /// Cannot refuse: every read it needed happened in validate, and the two cells it
    /// sets hold fixed-size values that were read out of them, so a failure to set one
    /// is a broken invariant rather than a case to report.
    fn apply_account_state(
        &mut self,
        mut anchor: Anchor,
        validated: ValidatedAccountStateWrite,
    ) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
        let anchor_number = anchor.anchor_number();
        let ValidatedAccountStateWrite {
            writes,
            anchor_counter,
            global_counter,
            next_application_number,
            session_count,
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
                principal_salt,
                accounts_changed,
                sessions_changed,
                previous_references,
                current_references,
                written: result,
                ..
            } = one;

            // A record is keyed by its own account number and needs no application, so a
            // rename lands whether or not anything else here does.
            for (account_number, record) in records {
                self.stable_account_memory.insert(account_number, record);
            }

            let Some(application_number) = application_number else {
                written.insert(origin, result);
                continue;
            };
            let key = (anchor_number, application_number);

            // The application and its counters go in together, so a list is never stored
            // against an application whose totals do not know about it, and an
            // application is never stored without something holding it.
            match application {
                ApplicationWrite::Untouched => {}
                ApplicationWrite::Stored(application) => {
                    self.lookup_application_with_origin_memory.insert(
                        StorableOriginSha256::from_origin(&origin),
                        application_number,
                    );
                    self.stable_application_memory
                        .insert(application_number, application);
                }
                ApplicationWrite::Retired(retired_origin) => {
                    self.remove_unreferenced_application(application_number, &retired_origin);
                }
            }

            // The index goes in after the records and before the list: a principal is
            // derived from an account's stored record, so one that is not in yet derives
            // nothing and a newly named account would get no entry.
            if let Some(salt) = principal_salt {
                if accounts_changed {
                    self.sync_account_principal_index(
                        anchor_number,
                        application_number,
                        &origin,
                        &salt,
                        &previous_references,
                        &current_references,
                    );
                }
                if sessions_changed {
                    self.sync_session_index(
                        anchor_number,
                        application_number,
                        &origin,
                        &salt,
                        &previous_references,
                        &current_references,
                    );
                }
            }

            match list {
                ListWrite::Untouched => {}
                ListWrite::Stored(list) => {
                    self.stable_account_reference_list_memory.insert(key, list);
                }
                ListWrite::Removed => {
                    self.stable_account_reference_list_memory.remove(&key);
                    self.stable_anchor_application_config_memory.remove(&key);
                }
            }

            if let Some(config) = config {
                self.stable_anchor_application_config_memory
                    .insert(key, config);
            }

            written.insert(origin, result);
        }

        // The count is worked out by validation at this layer rather than accumulated,
        // so it is `Some` only where it moved.
        if let Some(session_count) = session_count {
            anchor.session_count = session_count;
        }

        // Taking the identity record is taking the storing of it, so it is stored whatever
        // was changed on it — the count above, or anything a caller changed before giving
        // it up. Storing it only where this function's own change landed would discard the
        // caller's, silently.
        //
        // Trapping rather than reporting: an `Err` on the IC commits everything above this
        // line, so a record that could not be stored has to take the whole message with it.
        self.write(anchor)
            .expect("the identity record this write was handed cannot be written back");

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
        vec![AccountReference::new(None, None)]
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

    /// Lists whose only reference is a tracked default.
    fn evictable_default_lists(
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

    /// Keeps the principal index in step with one reference-list write, diffing values
    /// rather than keys.
    /// Takes the salt and origin its caller already resolved, so everything that could
    /// refuse has refused before this writes anything.
    fn sync_account_principal_index(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        origin: &FrontendHostname,
        salt: &[u8; 32],
        previous: &[AccountReference],
        current: &[AccountReference],
    ) {
        let previous_entries =
            self.account_principals(anchor_number, application_number, origin, salt, previous);
        let current_entries =
            self.account_principals(anchor_number, application_number, origin, salt, current);

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
    }

    /// The principals a set of references derives to. A reference whose account list is
    /// gone derives nothing and is skipped.
    fn account_principals(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        origin: &FrontendHostname,
        salt: &[u8; 32],
        references: &[AccountReference],
    ) -> BTreeMap<Principal, StorableAccountKey> {
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
                    StorableAccountKey {
                        anchor_number,
                        application_number,
                        account_number: reference.account_number,
                    },
                ))
            })
            .collect()
    }

    /// Indexes one batch of existing account reference lists. Entries are only inserted,
    /// never removed, so a batch that runs twice writes the same values.
    ///
    /// `batch_size` bounds **derivations**, not lists. One list is an identity's references
    /// at one origin and holds up to [`MAX_ANCHOR_ACCOUNTS`] of them, each costing a seed
    /// hash, a principal derivation and a stable write — so a list-bounded batch is only
    /// bounded in the shape of data that happens to be common. A batch stops mid-list and
    /// the cursor says where, which is why it carries an offset into the list.
    pub fn backfill_account_principal_index_batch(
        &mut self,
        cursor: Option<AccountPrincipalIndexBackfillCursor>,
        batch_size: u64,
    ) -> AccountPrincipalIndexBackfillOutcome {
        let mut outcome = AccountPrincipalIndexBackfillOutcome {
            next_cursor: cursor,
            ..Default::default()
        };

        // Examining nothing is not finishing. Reporting completion here would stop a
        // sweep that has not read a single list, and a lookup miss would then be taken as
        // proof no account has that principal.
        if batch_size == 0 {
            return outcome;
        }

        use std::ops::Bound as RangeBound;
        // Inclusive of the cursor's own list: a batch may have stopped part-way through
        // it, and the offset says how far it got.
        let range = match cursor {
            Some(cursor) => (RangeBound::Included(cursor.list()), RangeBound::Unbounded),
            None => (RangeBound::Unbounded, RangeBound::Unbounded),
        };

        // Read far enough ahead to spend the budget and no further, so the lists behind
        // this batch are never materialised. The borrow ends here, which is what lets the
        // indexing below write.
        let mut outstanding = batch_size;
        let mut ran_out = false;
        let mut lists: Vec<(
            AnchorNumber,
            ApplicationNumber,
            Vec<AccountReference>,
            usize,
        )> = vec![];
        for (key, list) in self.stable_account_reference_list_memory.range(range) {
            let references = Vec::<AccountReference>::from(list);
            let already_done = match cursor {
                Some(cursor) if cursor.list() == key => cursor.references_done,
                _ => 0,
            };
            let left_in_list = references.len().saturating_sub(already_done) as u64;
            lists.push((key.0, key.1, references, already_done));
            if left_in_list >= outstanding {
                ran_out = true;
                break;
            }
            outstanding -= left_in_list;
        }

        // Nothing left to index, whatever else is true of this canister. Checked before
        // the salt, because a fresh install has no salt until its first sign-in and no
        // lists either — and a sweep that waits for the salt there never reports done and
        // ticks its timer for the life of the canister.
        if lists.is_empty() {
            outcome.is_done = true;
            return outcome;
        }

        // Not done, so the caller comes back. A canister whose salt is unset has not
        // finished starting up rather than finished backfilling.
        let Some(salt) = self.salt().copied() else {
            return outcome;
        };

        outcome.is_done = !ran_out;

        let mut budget = batch_size;
        for (anchor_number, application_number, references, already_done) in lists {
            let Some(origin) = self
                .stable_application_memory
                .get(&application_number)
                .map(|application| application.origin)
            else {
                outcome.skipped += 1;
                outcome.next_cursor = Some(AccountPrincipalIndexBackfillCursor {
                    anchor_number,
                    application_number,
                    references_done: references.len(),
                });
                continue;
            };

            let taking = (budget as usize).min(references.len().saturating_sub(already_done));
            for (principal, locator) in self.account_principals(
                anchor_number,
                application_number,
                &origin,
                &salt,
                &references[already_done..already_done + taking],
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

            budget -= taking as u64;
            outcome.next_cursor = Some(AccountPrincipalIndexBackfillCursor {
                anchor_number,
                application_number,
                references_done: already_done + taking,
            });
            if budget == 0 {
                break;
            }
        }

        outcome
    }

    /// The principals a set of references derives to. A reference whose account list is
    /// gone derives nothing and is skipped.
    /// The account one reference names, built from the reference and the record it
    /// points at.
    ///
    /// Not [`Self::read_account`], which reads the stored list and so answers `None` for
    /// a reference that is being removed. This derives from the list it is handed, which
    /// is what lets the index be diffed across a write.
    fn account_of_reference(
        &self,
        anchor_number: AnchorNumber,
        origin: &FrontendHostname,
        reference: &AccountReference,
    ) -> Option<Account> {
        match reference.account_number {
            None => Some(Account::new(anchor_number, origin.clone(), None, None)),
            Some(account_number) => {
                let stored = self.stable_account_memory.get(&account_number)?;
                Some(Account::new_full(
                    anchor_number,
                    origin.clone(),
                    Some(stored.name),
                    Some(account_number),
                    reference.last_used,
                    stored.seed_from_anchor,
                ))
            }
        }
    }

    // Called by the sign-in ceremony, which lands two PRs up.
    #[allow(dead_code)]
    /// The principal an app sees for an account, which is what a session handle names.
    fn account_principal_of(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        account_number: Option<AccountNumber>,
    ) -> Option<Principal> {
        let salt = self.salt().copied()?;
        let account = self.read_account(&AccountKey {
            anchor_number,
            origin: self
                .stable_application_memory
                .get(&application_number)?
                .origin
                .clone(),
            account_number,
        })?;
        Some(delegation::canister_sig_principal(
            canister_id(),
            account.calculate_seed_with_salt(&salt).to_vec(),
        ))
    }

    /// Hands out the next session id, which no session has held before.
    ///
    /// Refuses at the ceiling rather than saturating. The id is an input to the session
    /// seed, so reissuing one would let a revoked session's identity be arrived at a
    /// second time — the thing this counter exists to prevent.
    fn allocate_session_id(&mut self) -> Result<SessionId, StorageError> {
        let session_id = *self.next_session_id_memory.get();
        self.next_session_id_memory
            .set(
                session_id
                    .checked_add(1)
                    .ok_or(StorageError::SessionIdOverflow)?,
            )
            .map_err(|_| StorageError::ErrorUpdatingSessionIdAllocator)?;
        Ok(session_id)
    }

    // Called by the sign-in ceremony, which lands two PRs up.
    #[allow(dead_code)]
    /// Creates the session `prepare_account_session` mints an identity from, replacing
    /// whatever this browser already held at this account.
    pub fn create_session(
        &mut self,
        params: CreateSessionParams,
    ) -> Result<(SessionRecordKey, SessionRecord), StorageError> {
        let CreateSessionParams {
            anchor_number,
            origin,
            account_number,
            current_browser_key,
            next_browser_key,
            browser_name,
            valid_till_ns,
            max_idle_ns,
            read_only,
            now_ns,
        } = params;
        let mut anchor = self.read(anchor_number)?;

        // Defaulted and clamped here rather than at the caller, so every path that
        // creates a session gets the same answer whatever it asked for. The ceiling is
        // the life this session was actually granted: a bound longer than that could
        // never be reached, and storing one would say something untrue about it.
        //
        // Raised then lowered rather than clamped in one call: `clamp` panics when its
        // floor exceeds its ceiling, which a session granted less than the floor would
        // do, and a trap is a poor answer to a short session.
        let granted = valid_till_ns.saturating_sub(now_ns);
        let max_idle_ns = max_idle_ns
            .unwrap_or(DEFAULT_SESSION_IDLE_NS)
            .max(MIN_SESSION_IDLE_NS)
            .min(granted);

        // One write, not three. What the identity holds at this origin afterwards: the
        // account reference list, created by this write where the origin is new, the
        // session itself, and the dead sessions pruned off every reference beside it.
        // Everything that can refuse does so before any of it is stored.
        let stored = self
            .lookup_application_number_with_origin(&origin)
            .and_then(|application_number| {
                self.stored_account_references(anchor_number, application_number)
            });
        // A named account lives in a list that already exists, and an origin nothing has
        // been stored under has none.
        if stored.is_none() && account_number.is_some() {
            return Err(StorageError::MissingAccount {
                anchor_number,
                name: origin,
            });
        }

        // Resolved here rather than by a caller, because what follows from it is this
        // function's to work out: the registry may be at its cap, in which case a browser
        // is given up and every session it held has to go in the same write. A caller
        // handed that consequence is a caller that can forget it.
        //
        // After the refusals above, so a ceremony that cannot happen registers nothing —
        // the record reaches storage only through the write at the end.
        let (browser_id, dropped_browsers) = anchor
            .resolve_browser(current_browser_key, next_browser_key, browser_name, now_ns)
            .map_err(StorageError::Browser)?;

        // The whole of what the identity holds, not just this origin: a browser the
        // registry gave up to make room for this one may hold sessions anywhere, and those
        // have to go in the same write as the browser that held them.
        let mut state = self.account_state(anchor_number);
        if !state.contains_key(&origin) {
            let held = self.account_state_for_origin(anchor_number, &origin);
            state.insert(origin.clone(), Some(held));
        }
        if !dropped_browsers.is_empty() {
            for held in state.values_mut() {
                let Some((account_references, _)) = held else {
                    continue;
                };
                for write in account_references.iter_mut() {
                    write
                        .account_reference
                        .sessions
                        .retain(|session| !dropped_browsers.contains(&session.browser_id));
                }
            }
        }

        // Before the session is added, because the room has to exist for it: dead sessions
        // everywhere, and where that is not enough the least used give way down to the
        // watermark. Nothing is stored yet, so a state still over the cap after this
        // refuses in the write below rather than half-way through.
        Self::reclaim_sessions(&mut state, now_ns);

        let (account_references, _) = state
            .get_mut(&origin)
            .and_then(Option::as_mut)
            .expect("the origin was just put there");

        let position = account_references
            .iter()
            .position(|write| write.account_reference.account_number == account_number)
            .ok_or(StorageError::MissingAccount {
                anchor_number,
                name: String::new(),
            })?;
        let reference = &mut account_references[position].account_reference;
        reference.last_used = Some(now_ns);

        // A ceremony replaces whatever this browser held here, rather than reusing it: the
        // copy of an old session's chain stops working at the user's next sign-in instead of
        // at its expiry.
        let mut dropped: Vec<(Option<AccountNumber>, SessionRecord)> = vec![];
        reference.sessions.retain(|session| {
            if session.browser_id == browser_id {
                dropped.push((account_number, session.clone()));
                return false;
            }
            true
        });

        // After the checks that can refuse this ceremony, so a refused one does not burn
        // an id. Ids need not be contiguous, so a later failure leaving a gap is fine;
        // what must never happen is one being handed out twice.
        let session_id = self.allocate_session_id()?;
        let session = SessionRecord {
            session_id,
            created_at_ns: now_ns,
            valid_till_ns,
            max_idle_ns,
            last_refreshed_ns: None,
            browser_id,
            read_only,
        };
        reference.sessions.push(session.clone());

        // The whole list, not just the account reference being written: this list is
        // about to be rewritten anyway, and a dead session on a sibling reference has
        // nothing else coming for it. Unconditional, unlike the cap sweep above.
        for write in account_references.iter_mut() {
            let account_number = write.account_reference.account_number;
            write.account_reference.sessions.retain(|session| {
                if session.is_over(now_ns) {
                    dropped.push((account_number, session.clone()));
                    return false;
                }
                true
            });
        }

        // The list is the whole of it: the index entries for the session created here and
        // for the ones pruned above, and the identity's session count, all follow from it.
        // One write for all of it: the session created here, the dead ones pruned above,
        // the sessions of every browser the registry gave up, the account reference list
        // this origin gets if it did not have one, and the identity's session count.
        self.write_account_state(anchor, state)?;

        let key = SessionRecordKey {
            anchor_number,
            origin,
            account_number,
            session_id,
        };
        Ok((key, session))
    }

    /// The session `key` names, or `None` where the identity holds no such session.
    ///
    /// A key whose session was replaced reads as `None` rather than as its successor:
    /// the successor was allocated an id of its own.
    #[allow(dead_code)] // Used by the sign-in ceremony, which lands two PRs up.
    pub fn read_session(&self, key: &SessionRecordKey) -> Option<SessionRecord> {
        let application_number = self.lookup_application_number_with_origin(&key.origin)?;

        self.account_references(key.anchor_number, application_number)
            .into_iter()
            .find(|reference| reference.account_number == key.account_number)?
            .sessions
            .into_iter()
            .find(|session| session.session_id == key.session_id)
    }

    // Called by the sign-in ceremony, which lands two PRs up.
    #[allow(dead_code)]
    /// Signs one browser out of everything, in a single message.
    pub fn revoke_browser_sessions(
        &mut self,
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    ) -> Result<u64, StorageError> {
        // Read what the identity holds, take the browser's sessions out of it, write it
        // back. Nothing here ranges over storage itself and no application number reaches
        // this function: the sweep is one write, so an `Err` cannot sign the browser out
        // of some applications and report failure.
        let anchor = self.read(anchor_number)?;
        let mut state = self.account_state(anchor_number);

        let mut revoked = 0u64;
        for held in state.values_mut() {
            let Some((account_references, _)) = held else {
                continue;
            };
            for write in account_references.iter_mut() {
                write.account_reference.sessions.retain(|session| {
                    let keep = session.browser_id != browser_id;
                    if !keep {
                        revoked += 1;
                    }
                    keep
                });
            }
        }

        self.write_account_state(anchor, state)?;

        Ok(revoked)
    }

    /// The session index entries a reference list implies: one per session it holds,
    /// each with the account entry its handle needs in order to resolve.
    fn session_entries(
        &self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        origin: &FrontendHostname,
        salt: &[u8; 32],
        references: &[AccountReference],
    ) -> BTreeMap<Principal, (StorableSessionHandle, StorableAccountKey)> {
        let mut entries = BTreeMap::new();
        for reference in references {
            let Some(account) = self.account_of_reference(anchor_number, origin, reference) else {
                continue;
            };
            let account_seed = account.calculate_seed_with_salt(salt);
            let account_principal =
                delegation::canister_sig_principal(canister_id(), account_seed.to_vec());
            for session in &reference.sessions {
                let seed =
                    calculate_session_seed_with_salt(salt, &account_seed, session.session_id);
                entries.insert(
                    delegation::canister_sig_principal(canister_id(), seed.to_vec()),
                    (
                        StorableSessionHandle {
                            account_principal: account_principal.as_slice().to_vec(),
                            session_id: session.session_id,
                        },
                        StorableAccountKey {
                            anchor_number,
                            application_number,
                            account_number: reference.account_number,
                        },
                    ),
                );
            }
        }
        entries
    }

    /// The ids of every session a reference list holds, sorted, for comparing two
    /// versions of a list.
    fn session_ids_of(references: &[AccountReference]) -> Vec<SessionId> {
        let mut ids: Vec<SessionId> = references
            .iter()
            .flat_map(|reference| reference.sessions.iter().map(|session| session.session_id))
            .collect();
        ids.sort_unstable();
        ids
    }

    /// Keeps the session index in step with one reference-list write, and reports what
    /// the write does to the identity's session count.
    ///
    /// Sessions live on the reference, so a reference that goes takes its sessions with
    /// it and this sees them as removed without any caller saying so. That is the point:
    /// the list and everything derived from it move together, in the one place holding
    /// both versions of it.
    fn sync_session_index(
        &mut self,
        anchor_number: AnchorNumber,
        application_number: ApplicationNumber,
        origin: &FrontendHostname,
        salt: &[u8; 32],
        previous: &[AccountReference],
        current: &[AccountReference],
    ) -> i64 {
        let before =
            self.session_entries(anchor_number, application_number, origin, salt, previous);
        let after = self.session_entries(anchor_number, application_number, origin, salt, current);

        for principal in before.keys() {
            if !after.contains_key(principal) {
                self.lookup_session_with_principal_memory.remove(principal);
            }
        }
        for (principal, (handle, account)) in &after {
            if before.contains_key(principal) {
                continue;
            }
            // The account's entry goes in with the session's. A handle names its account
            // by principal, and that index gains entries only where a list's set of
            // account numbers changes or when the backfill reaches the list — neither of
            // which a sign-in does. Without this a session at a list that predates the
            // index resolves to nothing until the sweep happens to arrive.
            self.lookup_account_with_principal_memory.insert(
                Principal::from_slice(&handle.account_principal),
                account.clone(),
            );
            self.lookup_session_with_principal_memory
                .insert(*principal, handle.clone());
        }

        after.len() as i64 - before.len() as i64
    }

    /// The session a caller's principal names, or `None` where the index no longer
    /// leads to one.
    ///
    /// A resolution, not an authorisation: the session it names may be expired or
    /// read-only, which is the caller's to check. What it does rule out is a stale
    /// entry, since the key it builds carries the id the entry recorded and no later
    /// session is ever allocated that id.
    pub fn lookup_session_with_principal(&self, principal: Principal) -> Option<SessionRecordKey> {
        let handle = self.lookup_session_with_principal_memory.get(&principal)?;
        let account = self.lookup_account_with_principal(handle.account())?;

        Some(SessionRecordKey {
            anchor_number: account.anchor_number,
            origin: account.origin,
            account_number: account.account_number,
            session_id: handle.session_id,
        })
    }

    /// The account a principal a dapp sees was derived for, as an address.
    ///
    /// An [`Account`] carries the seed it signs with, so one only ever comes out of
    /// [`Self::read_account`], which is where the identity's claim on it is checked.
    pub fn lookup_account_with_principal(&self, principal: Principal) -> Option<AccountKey> {
        self.account_key_of(&self.lookup_account_with_principal_memory.get(&principal)?)
    }

    /// A stored account address resolved to the one callers use.
    ///
    /// `None` where the application is gone, which leaves the stored list naming
    /// nothing. Not a `From`, because the origin the number stands for comes out of
    /// storage.
    fn account_key_of(&self, stored: &StorableAccountKey) -> Option<AccountKey> {
        Some(AccountKey {
            anchor_number: stored.anchor_number,
            origin: self
                .stable_application_memory
                .get(&stored.application_number)?
                .origin,
            account_number: stored.account_number,
        })
    }

    /// Records that a session was used: its own stamp, its account reference's, and the
    /// browser's in the device registry.
    ///
    /// `false` where the identity holds no such session, which is not a failure — a
    /// session can be revoked between one call and the next.
    pub fn record_session_use(
        &mut self,
        key: &SessionRecordKey,
        now: Timestamp,
    ) -> Result<bool, StorageError> {
        let SessionRecordKey {
            anchor_number,
            origin,
            account_number,
            session_id,
        } = key;
        let (anchor_number, account_number, session_id) =
            (*anchor_number, *account_number, *session_id);

        if self.lookup_application_number_with_origin(origin).is_none() {
            return Ok(false);
        }
        let mut anchor = self.read(anchor_number)?;
        let (mut account_references, config) = self.account_state_for_origin(anchor_number, origin);

        let Some(write) = account_references
            .iter_mut()
            .find(|write| write.account_reference.account_number == account_number)
        else {
            return Ok(false);
        };
        let Some(session) = write
            .account_reference
            .sessions
            .iter_mut()
            .find(|session| session.session_id == session_id)
        else {
            return Ok(false);
        };

        session.last_refreshed_ns = Some(now);
        let browser_id = session.browser_id;
        write.account_reference.last_used = Some(now);

        // This list is being rewritten anyway, so its dead sessions go now. It costs one
        // pass over a list already in memory and no write of its own, and it means every
        // list anyone still uses stays clean without anything having to sweep for it.
        for write in account_references.iter_mut() {
            write
                .account_reference
                .sessions
                .retain(|session| !session.is_over(now));
        }

        // Stamped before the write rather than after, because the write is what stores the
        // record. There is no second store: handing it over is handing over the storing of
        // it, whatever was changed on it.
        anchor.stamp_browser_use(browser_id, now);
        self.write_account_state(
            anchor,
            BTreeMap::from([(origin.clone(), Some((account_references, config)))]),
        )?;
        Ok(true)
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
        // Read once and handed to the write, for two reasons: the gate moves this
        // identity's session count and must not be handed a copy that has already gone
        // stale, and an identity that does not exist has nothing to hold what is about to
        // be written — the counters, the account reference lists and the session count all
        // key on a record that would not be there.
        let anchor = self.read(anchor_number)?;
        let (mut account_references, config) =
            self.account_state_for_origin(anchor_number, &origin);
        // Where the write leaves it, and so where its minted number comes back.
        let created = account_references.len();
        account_references.push(AccountReferenceWrite {
            account_reference: AccountReference::new(None, None),
            // A record on an account reference with no number is an account being named,
            // and naming it is what mints it one.
            record: Some(StorableAccount {
                name: name.clone(),
                seed_from_anchor: None,
            }),
        });

        let written = self.write_account_state(
            anchor,
            BTreeMap::from([(origin.clone(), Some((account_references, config)))]),
        )?;

        Ok(Account::new(
            anchor_number,
            origin.clone(),
            Some(name),
            written[&origin]
                .as_ref()
                .expect("a write that holds something is handed back holding it")
                .0[created]
                .account_reference
                .account_number,
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

        // The tracked default is stored the first time it is named or used, so an origin
        // nothing has been stored under gets its application on either. A stored account
        // writes to a list that already exists, and such an origin has none.
        if let Some(account_number) = account_number {
            if self
                .lookup_application_number_with_origin(&origin)
                .is_none()
            {
                return Err(StorageError::AccountNotFound { account_number });
            }
        }

        let anchor = self.read(anchor_number)?;
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
            anchor,
            BTreeMap::from([(origin.clone(), Some((account_references, config)))]),
        )?;
        let write = &written[&origin]
            .as_ref()
            .expect("a write that holds something is handed back holding it")
            .0[position];
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

        let anchor = self.read(anchor_number)?;
        let (account_references, _) = self.account_state_for_origin(anchor_number, &origin);
        self.write_account_state(
            anchor,
            BTreeMap::from([(
                origin,
                Some((
                    account_references,
                    Some(AnchorApplicationConfig {
                        default_account_number: account_number,
                    }),
                )),
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
    /// What the browser proves it holds, and the successor it announces. Its registry
    /// entry, its id, and whatever the cap gives up to make room for it are all worked out
    /// inside the write, so no caller states any of them.
    pub current_browser_key: PublicKey,
    pub next_browser_key: PublicKey,
    pub browser_name: String,
    pub valid_till_ns: Timestamp,
    pub max_idle_ns: Option<u64>,
    pub read_only: bool,
    pub now_ns: Timestamp,
}

/// How far the sweep has got: which list, and how many of that list's references are
/// already indexed. The offset is what lets a batch stop inside a list that holds more
/// references than one message can derive principals for.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AccountPrincipalIndexBackfillCursor {
    pub anchor_number: AnchorNumber,
    pub application_number: ApplicationNumber,
    pub references_done: usize,
}

impl AccountPrincipalIndexBackfillCursor {
    fn list(&self) -> (AnchorNumber, ApplicationNumber) {
        (self.anchor_number, self.application_number)
    }
}

#[derive(Debug, Default)]
pub struct AccountPrincipalIndexBackfillOutcome {
    pub next_cursor: Option<AccountPrincipalIndexBackfillCursor>,
    pub indexed: u64,
    /// Lists whose application is gone, so no principal can be derived for them. A list
    /// in that state is an inconsistency rather than a normal skip, and a run that
    /// silently indexes nothing would otherwise look like a run with nothing to do.
    pub skipped: u64,
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
    /// Lists that exist while holding no reference.
    Tombstones,
}

impl fmt::Display for ReferenceCount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Accounts => write!(f, "stored accounts"),
            Self::References => write!(f, "stored account references"),
            Self::Tombstones => write!(f, "stored tombstones"),
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
    /// Change in lists that exist while holding no reference. Only ever -1, 0 or 1: one
    /// write touches one list.
    tombstones: i64,
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
/// `None` removes the account reference list, and the derived default comes back with it —
/// which is the same state an absent list already denotes on the read side. `Some` with an
/// empty list is the opposite: a tombstone, which nothing may create yet.
///
/// The inner config is `None` where the write leaves the stored one alone. A removal cannot
/// carry one, because there is nowhere in the type to put it.
pub type AccountReferenceListWrite =
    Option<(Vec<AccountReferenceWrite>, Option<AnchorApplicationConfig>)>;

/// What a write does to the stored account reference list.
enum ListWrite {
    /// Left where it is: nothing changed, or the write only touches a record.
    Untouched,
    Stored(StorableAccountReferenceList),
    /// Removed, and the config keyed by the same pair with it.
    Removed,
}

/// What a write does to the application.
enum ApplicationWrite {
    /// Left where it is, because nothing keyed by it changed.
    Untouched,
    Stored(StorableApplication),
    /// Nothing references it any more, and no tombstone keeps its number alive.
    Retired(FrontendHostname),
}

/// One origin's write, past every refusal it can make on its own.
struct ValidatedAccountReferenceListWrite {
    origin: FrontendHostname,
    /// `None` where this write stores nothing at this origin and the origin has no
    /// application, so none is created for it.
    application_number: Option<ApplicationNumber>,
    application: ApplicationWrite,
    list: ListWrite,
    records: Vec<(AccountNumber, StorableAccount)>,
    config: Option<AnchorApplicationConfig>,
    deltas: ReferenceListDeltas,
    /// The salt, where this write moved an account number and the principal index has to
    /// follow it. `None` where none moved — which is every `last_used` stamp, and so
    /// every sign-in. Resolved in validate because a missing salt must refuse with
    /// nothing written; the index itself is synced in apply, after the records, because a
    /// principal is derived from an account's stored record.
    principal_salt: Option<[u8; 32]>,
    /// Whether the set of account numbers moved, and whether the set of sessions did.
    /// They are tracked apart because a sign-in changes only the second, and recomputing
    /// account principals it did not touch would put the hottest write in the system
    /// through a per-account hash for nothing.
    accounts_changed: bool,
    sessions_changed: bool,
    /// How many sessions this write adds or takes away, counted from the lists rather
    /// than read off a counter.
    session_delta: i64,
    previous_references: Vec<AccountReference>,
    current_references: Vec<AccountReference>,
    /// What this origin holds afterwards, handed back to the caller.
    written: AccountReferenceListWrite,
}

/// A whole write, past every refusal — including the ones only the whole call can make.
struct ValidatedAccountStateWrite {
    writes: Vec<ValidatedAccountReferenceListWrite>,
    anchor_counter: StorableAccountsCounter,
    global_counter: StorableAccountsCounter,
    next_application_number: ApplicationNumber,
    /// What the identity's session count is afterwards, where this write moved it.
    session_count: Option<u32>,
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
        previous_references: Option<&[AccountReference]>,
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

        let (previous_named, previous_total) = counts(previous_references.unwrap_or_default());
        let (new_named, new_total) = counts(new_references);

        // A list that does not exist is not a tombstone — a tombstone is a list someone
        // stored, and absence is what normalisation reads as "derive the default".
        let was_tombstone = previous_references.is_some_and(<[_]>::is_empty);
        let is_tombstone = new_references.is_empty();
        let tombstones = match (was_tombstone, is_tombstone) {
            (false, true) => 1,
            (true, false) => -1,
            _ => 0,
        };

        Self {
            accounts: new_named.saturating_sub(previous_named),
            references: new_total.saturating_sub(previous_total),
            tombstones,
        }
    }

    /// What retiring a list holding `previous` does to the counters.
    ///
    /// Separate from [`Self::between`] rather than a write of an empty list, because
    /// an empty list cannot be written at all: a list holding nothing is a tombstone
    /// and stays, so only an outright removal gets to zero these out.
    fn removing(previous: &[AccountReference]) -> Self {
        let removed = Self::between(Some(&[]), previous);
        Self {
            accounts: removed.accounts.saturating_neg(),
            references: removed.references.saturating_neg(),
            // The list is gone, so a tombstone goes with it. Not the negation of what
            // `between` reported: that describes writing this list, and this describes
            // removing the list it was in.
            tombstones: if previous.is_empty() { -1 } else { 0 },
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
            ReferenceCount::Tombstones => self.tombstones,
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
    /// The browser presenting itself could not be resolved to a registry entry.
    Browser(BrowserError),
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
    AccountsCounterOverflow,
    /// No application numbers left to hand out. Refused rather than saturated: the
    /// number keys the application and the origin index, so reissuing one would
    /// put two origins on a single list.
    ApplicationsCounterOverflow,
    ErrorUpdatingApplicationNumberAllocator,
    /// No session ids left to hand out. Refused rather than saturated: the id is an
    /// input to the session seed, so reissuing one would resurrect a revoked session's
    /// identity.
    SessionIdOverflow,
    ErrorUpdatingSessionIdAllocator,
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
            Self::Browser(err) => write!(f, "the browser could not be resolved: {err:?}"),
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
            Self::SaltNotSet => write!(
                f,
                "the salt is not set, so an account principal cannot be derived"
            ),
            Self::AccountsCounterOverflow => write!(f, "No account numbers left to allocate"),
            Self::ApplicationsCounterOverflow => {
                write!(f, "No application numbers left to allocate")
            }
            Self::ErrorUpdatingApplicationNumberAllocator => {
                write!(f, "Error updating the application number allocator")
            }
            Self::SessionIdOverflow => write!(f, "No session ids left to allocate"),
            Self::ErrorUpdatingSessionIdAllocator => {
                write!(f, "Error updating the session id allocator")
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
