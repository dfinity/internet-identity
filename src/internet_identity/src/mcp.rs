//! Backend `/mcp` delegation path: lets an MCP server the user chooses to trust
//! act as one of the user's accounts at any app, without a per-app browser flow.
//!
//! At connect time the MCP server obtains (via the `/mcp` browser delegation
//! flow) a standing delegation `anchor -> MCP server key`, issued for the MCP
//! server's own origin and a specific account the user picks. Its principal is
//! therefore the principal II derives for that `(account, origin)` pair. When
//! the anchor enables MCP access for that pair, we record `that principal ->
//! anchor` in the reverse index (see [`crate::storage`]). The MCP server then
//! calls [`prepare_account_delegation`] / [`get_account_delegation`] *as that
//! principal*; we recover the anchor from `caller()` via the index, so no
//! `anchor_number` parameter is needed — being the right caller is the
//! authorization. The trusted origin comes from the connect request, so each
//! user trusts the server they choose. Issued per-app delegations are capped at
//! 5 minutes.

use candid::Principal;
use ic_cdk::caller;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, McpConfig, SessionKey, SignedDelegation,
    Timestamp,
};

use crate::{
    account_management,
    delegation::der_encode_canister_sig_key,
    state::{storage_borrow, storage_borrow_mut},
    storage::account::{
        Account, AccountDelegationError, PrepareAccountDelegation, ReadAccountParams,
    },
    storage::storable::mcp_config::StorableMcpConfig,
};

/// Maximum lifetime of an MCP-minted per-app account delegation: 5 minutes.
const MCP_MAX_EXPIRATION_PERIOD_NS: u64 = 5 * 60 * 1_000_000_000;

/// The anchor's default account at `origin` (synthetic when none is reserved).
fn default_account(anchor_number: AnchorNumber, origin: &FrontendHostname) -> Account {
    storage_borrow(|storage| {
        let Some(app_num) = storage.lookup_application_number_with_origin(origin) else {
            return Account::synthetic(anchor_number, origin.clone());
        };
        let Some(default_num) = storage
            .lookup_anchor_application_config(anchor_number, app_num)
            .default_account_number
        else {
            return Account::synthetic(anchor_number, origin.clone());
        };
        storage
            .read_account(ReadAccountParams {
                account_number: Some(default_num),
                anchor_number,
                origin,
                known_app_num: Some(app_num),
            })
            .unwrap_or_else(|| Account::synthetic(anchor_number, origin.clone()))
    })
}

/// The default-account number for `(anchor, origin)` (None = unreserved default).
fn default_account_number(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
) -> Option<AccountNumber> {
    default_account(anchor_number, origin).account_number
}

/// The anchor's account `account_number` at `origin` — or the synthetic default
/// (the unreserved account) when `account_number` is `None` or the account
/// can't be read.
fn account_for(
    anchor_number: AnchorNumber,
    account_number: Option<AccountNumber>,
    origin: &FrontendHostname,
) -> Account {
    let Some(account_number) = account_number else {
        return Account::synthetic(anchor_number, origin.clone());
    };
    storage_borrow(|storage| {
        let known_app_num = storage.lookup_application_number_with_origin(origin);
        storage
            .read_account(ReadAccountParams {
                account_number: Some(account_number),
                anchor_number,
                origin,
                known_app_num,
            })
            .unwrap_or_else(|| Account::synthetic(anchor_number, origin.clone()))
    })
}

/// The principal II derives for `anchor_number`'s `account_number` at `origin` —
/// the principal the MCP server's standing delegation carries.
fn mcp_principal_for(
    anchor_number: AnchorNumber,
    account_number: Option<AccountNumber>,
    origin: &FrontendHostname,
) -> Principal {
    let seed = account_for(anchor_number, account_number, origin).calculate_seed();
    Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec()))
}

/// Enable or disable MCP access for `anchor_number` at `mcp_server_origin` for
/// `account_number` (the unreserved default when `None`): bind/unbind the
/// principal II derives for that `(account, origin)` pair in the reverse index.
/// The caller must already be authorized for `anchor_number` (checked by the
/// canister method). Disabling re-derives the same principal from the supplied
/// origin+account, so it always unbinds exactly what enabling bound.
pub fn set_mcp_access(
    anchor_number: AnchorNumber,
    mcp_server_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    enabled: bool,
) -> Result<(), String> {
    let principal = mcp_principal_for(anchor_number, account_number, &mcp_server_origin);
    storage_borrow_mut(|storage| {
        if enabled {
            // Surface a cross-anchor collision instead of silently no-op'ing, so
            // the caller learns the (account, origin) pair couldn't be bound.
            storage
                .set_anchor_mcp_principal(principal, anchor_number)
                .map_err(|existing| {
                    format!(
                        "MCP access could not be enabled: the principal for this \
                         (account, origin) is already bound to identity {existing}."
                    )
                })
        } else {
            storage.remove_anchor_mcp_principal(principal);
            Ok(())
        }
    })
}

/// Read `anchor_number`'s synced trusted-MCP-server config (master toggle +
/// trusted server URL). Returns the disabled, no-server default for an anchor
/// that has never written one. The caller must already be authorized for
/// `anchor_number` (checked by the canister method).
pub fn get_mcp_config(anchor_number: AnchorNumber) -> McpConfig {
    let stored = storage_borrow(|storage| storage.read_mcp_config(anchor_number));
    McpConfig {
        enabled: stored.enabled,
        url: stored.url,
    }
}

/// Persist `anchor_number`'s trusted-MCP-server config, overwriting any previous
/// value. Storing it on-chain (keyed by anchor) is what makes the config sync
/// across the identity's devices. The caller must already be authorized for
/// `anchor_number` (checked by the canister method).
pub fn set_mcp_config(anchor_number: AnchorNumber, config: McpConfig) {
    let stored = StorableMcpConfig {
        enabled: config.enabled,
        url: config.url,
    };
    storage_borrow_mut(|storage| storage.write_mcp_config(anchor_number, stored));
}

/// Whether `anchor_number` has MCP access enabled for the
/// `(mcp_server_origin, account_number)` pair.
pub fn is_mcp_access_enabled(
    anchor_number: AnchorNumber,
    mcp_server_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
) -> bool {
    let principal = mcp_principal_for(anchor_number, account_number, &mcp_server_origin);
    storage_borrow(|storage| storage.lookup_anchor_with_mcp_principal(principal))
        == Some(anchor_number)
}

/// Recover the anchor that authorized the calling MCP-server principal.
fn caller_anchor() -> Result<AnchorNumber, AccountDelegationError> {
    storage_borrow(|storage| storage.lookup_anchor_with_mcp_principal(caller()))
        .ok_or(AccountDelegationError::Unauthorized(caller()))
}

/// `mcp_prepare_account_delegation`: mint a ≤5-minute account delegation for the
/// calling MCP server, as its anchor's default account at `target_origin`.
pub async fn prepare_account_delegation(
    target_origin: FrontendHostname,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<PrepareAccountDelegation, AccountDelegationError> {
    let anchor_number = caller_anchor()?;
    let capped_ttl = Some(u64::min(
        max_ttl.unwrap_or(MCP_MAX_EXPIRATION_PERIOD_NS),
        MCP_MAX_EXPIRATION_PERIOD_NS,
    ));
    let account_number = default_account_number(anchor_number, &target_origin);
    account_management::prepare_account_delegation(
        anchor_number,
        target_origin,
        account_number,
        session_key,
        capped_ttl,
        &None,
    )
    .await
}

/// `mcp_get_account_delegation`: fetch the signed delegation prepared above.
pub fn get_account_delegation(
    target_origin: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    let anchor_number = caller_anchor()?;
    let account_number = default_account_number(anchor_number, &target_origin);
    account_management::get_account_delegation(
        anchor_number,
        &target_origin,
        account_number,
        session_key,
        expiration,
    )
}
