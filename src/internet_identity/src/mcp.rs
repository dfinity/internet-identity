//! Backend `/mcp` delegation path: lets an MCP server the user chooses to trust
//! act on the user's behalf at any app, without a per-app browser flow.
//!
//! At connect time the user authorizes the MCP server for their identity: II
//! records `the principal it derives for the anchor at the MCP server's origin
//! -> anchor` in the reverse index (see [`crate::storage`]). No account is
//! chosen at connect — the MCP server's own origin is just the connector, and
//! accounts are per-origin. The MCP server then calls [`get_accounts`] /
//! [`prepare_account_delegation`] / [`get_account_delegation`] *as that
//! principal*; we recover the anchor from `caller()` via the index, so no
//! `anchor_number` parameter is needed — being the right caller is the
//! authorization. The trusted origin comes from the connect request, so each
//! user trusts the server they choose. Which app account the server acts as is
//! chosen per call against the *target app* origin (discover them with
//! [`get_accounts`]). Issued per-app delegations are capped at 5 minutes.

use candid::Principal;
use ic_cdk::caller;
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNumber, AnchorNumber, FrontendHostname, McpConfig, McpPrepareDelegation,
    SessionKey, SignedDelegation, Timestamp,
};

use crate::{
    account_management,
    delegation::der_encode_canister_sig_key,
    state::{storage_borrow, storage_borrow_mut},
    storage::account::{Account, AccountDelegationError, ReadAccountParams},
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

/// The MCP server's standing-delegation principal for `anchor_number` at
/// `origin`: the principal II derives for the anchor's synthetic account at that
/// origin (seed = anchor + origin). No account is chosen at connect — the
/// connector isn't an app — and the synthetic seed is stable, so enable and
/// disable always derive the same principal.
fn mcp_principal_for(anchor_number: AnchorNumber, origin: &FrontendHostname) -> Principal {
    let seed = Account::synthetic(anchor_number, origin.clone()).calculate_seed();
    Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec()))
}

/// Enable or disable MCP access for `anchor_number` at `mcp_server_origin`:
/// bind/unbind the principal II derives for the anchor at that origin in the
/// reverse index. The caller must already be authorized for `anchor_number`
/// (checked by the canister method). Disabling re-derives the same principal, so
/// it always unbinds exactly what enabling bound.
pub fn set_mcp_access(
    anchor_number: AnchorNumber,
    mcp_server_origin: FrontendHostname,
    enabled: bool,
) -> Result<(), String> {
    let principal = mcp_principal_for(anchor_number, &mcp_server_origin);
    storage_borrow_mut(|storage| {
        if enabled {
            // Surface a cross-anchor collision instead of silently no-op'ing, so
            // the caller learns the origin couldn't be bound.
            storage
                .set_anchor_mcp_principal(principal, anchor_number)
                .map_err(|existing| {
                    format!(
                        "MCP access could not be enabled: the principal for this \
                         origin is already bound to identity {existing}."
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

/// Whether `anchor_number` has MCP access enabled at `mcp_server_origin`.
pub fn is_mcp_access_enabled(
    anchor_number: AnchorNumber,
    mcp_server_origin: FrontendHostname,
) -> bool {
    let principal = mcp_principal_for(anchor_number, &mcp_server_origin);
    storage_borrow(|storage| storage.lookup_anchor_with_mcp_principal(principal))
        == Some(anchor_number)
}

/// Recover the anchor that authorized the calling MCP-server principal.
fn caller_anchor() -> Result<AnchorNumber, AccountDelegationError> {
    storage_borrow(|storage| storage.lookup_anchor_with_mcp_principal(caller()))
        .ok_or(AccountDelegationError::Unauthorized(caller()))
}

/// `mcp_get_accounts`: list the calling MCP server's anchor's accounts at
/// `target_origin`, so the agent can discover which `account_number` values it
/// may request a delegation for via [`prepare_account_delegation`]. Authorized
/// like prepare/get — the anchor is recovered from `caller()`, never passed.
pub fn get_accounts(
    target_origin: FrontendHostname,
) -> Result<Vec<AccountInfo>, AccountDelegationError> {
    let anchor_number = caller_anchor()?;
    Ok(
        account_management::get_accounts_for_origin(anchor_number, &target_origin)
            .iter()
            .map(|account| account.to_info())
            .collect(),
    )
}

/// `mcp_prepare_account_delegation`: mint a ≤5-minute account delegation for the
/// calling MCP server at `target_origin`, as `account_number` — one of the
/// anchor's accounts at that origin when given explicitly (discover them with
/// [`get_accounts`]), or the anchor's default account there when `None`.
/// (Accounts are per-origin, so this is an account at `target_origin`, the app
/// being acted on; no account is chosen at connect. An `account_number` that
/// isn't the anchor's at `target_origin` is rejected as `Unauthorized`.)
///
/// Returns the resolved `account_number` so the server can thread the *same*
/// account into [`get_account_delegation`]: the default account at an origin is
/// mutable, so if `get` re-resolved it independently and it had changed in
/// between, it would look under a different account's seed and `NoSuchDelegation`.
pub async fn prepare_account_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<McpPrepareDelegation, AccountDelegationError> {
    let anchor_number = caller_anchor()?;
    let capped_ttl = Some(u64::min(
        max_ttl.unwrap_or(MCP_MAX_EXPIRATION_PERIOD_NS),
        MCP_MAX_EXPIRATION_PERIOD_NS,
    ));
    // An explicit account wins; otherwise act as the anchor's default at the app.
    let account_number =
        account_number.or_else(|| default_account_number(anchor_number, &target_origin));
    let prepared = account_management::prepare_account_delegation(
        anchor_number,
        target_origin,
        account_number,
        session_key,
        capped_ttl,
        // MCP delegations are update-capable (unrestricted); the read-only
        // restriction is a feature of the ICRC-34 authorize flow only.
        false,
        &None,
    )
    .await?;
    Ok(McpPrepareDelegation {
        user_key: prepared.user_key,
        expiration: prepared.expiration,
        account_number,
    })
}

/// `mcp_get_account_delegation`: fetch the signed delegation prepared above.
/// `account_number` must be the value returned by the matching
/// `prepare_account_delegation`, so `get` reads the same account `prepare`
/// signed for (see the note there); the caller is still authorized only for its
/// own anchor (recovered from `caller()`), so it can only fetch what it prepared.
pub fn get_account_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    let anchor_number = caller_anchor()?;
    account_management::get_account_delegation(
        anchor_number,
        &target_origin,
        account_number,
        session_key,
        expiration,
        // Unrestricted; must match `prepare_account_delegation` above.
        false,
    )
}
