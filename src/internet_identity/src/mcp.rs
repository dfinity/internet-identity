//! Backend `/mcp` delegation path: lets a single, deploy-configured MCP server
//! act as an anchor's account at any app, without a per-app browser flow.
//!
//! At connect time the MCP server obtains (via the existing `/mcp` browser
//! delegation flow) a 60-minute standing delegation `anchor -> MCP server key`,
//! issued for `mcp_server_origin`. Its principal is therefore the principal II
//! derives for that anchor's default account at `mcp_server_origin`. When the
//! anchor enables MCP access, we record `that principal -> anchor` in the
//! reverse index (see [`crate::storage`]). The MCP server then calls
//! [`prepare_account_delegation`] / [`get_account_delegation`] *as that
//! principal*; we recover the anchor from `caller()` via the index, so no
//! `anchor_number` parameter is needed — being the right caller is the
//! authorization. Issued per-app delegations are capped at 5 minutes.

use candid::Principal;
use ic_cdk::caller;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, SessionKey, SignedDelegation, Timestamp,
};

use crate::{
    account_management,
    delegation::der_encode_canister_sig_key,
    state::{self, storage_borrow, storage_borrow_mut},
    storage::account::{
        Account, AccountDelegationError, PrepareAccountDelegation, ReadAccountParams,
    },
};

/// Maximum lifetime of an MCP-minted per-app account delegation: 5 minutes.
const MCP_MAX_EXPIRATION_PERIOD_NS: u64 = 5 * 60 * 1_000_000_000;

/// The configured MCP server origin, or an error if the path is disabled.
fn mcp_server_origin() -> Result<FrontendHostname, String> {
    state::persistent_state(|ps| ps.mcp_server_origin.clone())
        .ok_or_else(|| "MCP delegation is not enabled on this canister".to_string())
}

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

/// The principal II derives for `anchor_number`'s default account at `origin` —
/// the principal the MCP server's standing delegation carries.
fn mcp_principal_for(anchor_number: AnchorNumber, origin: &FrontendHostname) -> Principal {
    let seed = default_account(anchor_number, origin).calculate_seed();
    Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec()))
}

/// Enable or disable MCP access for `anchor_number`: bind/unbind its principal
/// at the configured `mcp_server_origin` in the reverse index. The caller must
/// already be authorized for `anchor_number` (checked by the canister method).
pub fn set_mcp_access(anchor_number: AnchorNumber, enabled: bool) -> Result<(), String> {
    let origin = mcp_server_origin()?;
    let principal = mcp_principal_for(anchor_number, &origin);
    storage_borrow_mut(|storage| {
        if enabled {
            storage.set_anchor_mcp_principal(principal, anchor_number);
        } else {
            storage.remove_anchor_mcp_principal(principal);
        }
    });
    Ok(())
}

/// Whether `anchor_number` currently has MCP access enabled.
pub fn is_mcp_access_enabled(anchor_number: AnchorNumber) -> bool {
    let Ok(origin) = mcp_server_origin() else {
        return false;
    };
    let principal = mcp_principal_for(anchor_number, &origin);
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
