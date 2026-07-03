//! Backend `/mcp` path: lets an MCP server the user chooses to trust act on
//! the user's behalf at any app, without a per-app browser flow.
//!
//! At connect time the user authorizes the MCP server for their identity: the
//! II frontend fetches the server's session public key from the trusted
//! server's callback (an origin-attested channel — the frontend only ever
//! contacts the origin the identity's synced config trusts, so nothing from
//! the unauthenticated connect link is ever registered) and registers it here
//! via [`register`]: a grant `session principal -> (anchor, expiry)` in the
//! grant map, mirrored by a per-anchor forward pointer in the synced
//! [`StorableMcpConfig`]. No delegation is minted for the server itself, and
//! no account is chosen at connect. The MCP server then calls
//! [`get_accounts`] / [`prepare_delegation`] / [`get_delegation`] signed with
//! that session key; we recover the anchor from `caller()`'s grant (checking
//! expiry), so no `anchor_number` parameter is needed — being the right
//! caller is the authorization.
//!
//! Session lifecycle: at most one live session per identity — registering
//! replaces the previous grant through the config's forward pointer — and a
//! session ends by expiry, by replacement, or by revocation: disabling MCP or
//! changing the trusted server URL in the synced config deletes the grant in
//! the same update message ([`set_mcp_config`]). Which app account the server
//! acts as is chosen per call against the *target app* origin (discover them
//! with [`get_accounts`]); issued per-app delegations are capped at 1 hour
//! and never outlive the grant.

use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::caller;
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNumber, AnchorNumber, FrontendHostname, McpConfig, McpPrepareDelegation,
    McpRegistration, SessionKey, SignedDelegation, Timestamp,
};

use crate::{
    account_management,
    state::{storage_borrow, storage_borrow_mut},
    storage::account::{Account, AccountDelegationError, ReadAccountParams},
    storage::storable::mcp_config::StorableMcpConfig,
    storage::storable::mcp_grant::StorableMcpGrant,
};

/// Maximum lifetime of an MCP-minted per-app account delegation: 1 hour.
const MCP_MAX_EXPIRATION_PERIOD_NS: u64 = 60 * 60 * 1_000_000_000;

/// Bounds for the MCP session grant lifetime: 10 minutes to 30 days.
/// Out-of-range requests are clamped (mirroring the frontend), not rejected.
const MCP_GRANT_MIN_TTL_NS: u64 = 10 * 60 * 1_000_000_000;
const MCP_GRANT_MAX_TTL_NS: u64 = 30 * 24 * 60 * 60 * 1_000_000_000;

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

/// `mcp_register`: register the trusted MCP server's session key for
/// `anchor_number`, granting its self-authenticating principal access to the
/// server-facing `mcp_*` methods until the grant expires. The caller must
/// already be authorized for `anchor_number` (checked by the canister
/// method); the key itself comes from the trusted server's callback, fetched
/// by the II frontend after user consent — never from the connect link.
///
/// `grant_ttl_ns` is clamped to [10 min, 30 days]. Registering while a
/// session is live replaces it (at most one session per identity), so the
/// previous key stops being authorized in the same message. A key holding a
/// live grant for a *different* identity is rejected — one key serves one
/// identity — without echoing whose it is.
pub fn register(
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    grant_ttl_ns: u64,
) -> Result<McpRegistration, String> {
    if session_key.is_empty() {
        return Err("MCP registration failed: empty session key.".to_string());
    }
    let principal = Principal::self_authenticating(&session_key);
    let now = time();
    let expires_at_ns =
        now.saturating_add(grant_ttl_ns.clamp(MCP_GRANT_MIN_TTL_NS, MCP_GRANT_MAX_TTL_NS));
    storage_borrow_mut(|storage| {
        let mut config = storage.read_mcp_config(anchor_number);
        // A grant can only exist under a live trusted-server config: this is
        // what makes config-driven revocation (see [`set_mcp_config`]) cover
        // every session ever registered.
        if !config.enabled || config.url.is_none() {
            return Err(
                "MCP registration failed: no trusted MCP server is enabled for this identity."
                    .to_string(),
            );
        }
        // One key serves one identity: reject a key with a live grant for a
        // different anchor (an expired one is fine to overwrite). Deliberately
        // does not echo the other anchor number.
        if let Some(existing) = storage.lookup_mcp_grant(principal) {
            if existing.anchor_number != anchor_number && existing.expires_at_ns > now {
                return Err(
                    "MCP registration failed: this session key is already registered.".to_string(),
                );
            }
        }
        // At most one session per identity: replace the previous grant through
        // the config's forward pointer. Ownership is verified so a stale
        // pointer can never delete another identity's session.
        if let Some(old_bytes) = &config.session_principal {
            if let Ok(old_principal) = Principal::try_from_slice(old_bytes) {
                if old_principal != principal
                    && storage
                        .lookup_mcp_grant(old_principal)
                        .is_some_and(|grant| grant.anchor_number == anchor_number)
                {
                    storage.remove_mcp_grant(old_principal);
                }
            }
        }
        storage.insert_mcp_grant(
            principal,
            StorableMcpGrant {
                anchor_number,
                expires_at_ns,
            },
        );
        config.session_principal = Some(principal.as_slice().to_vec());
        storage.write_mcp_config(anchor_number, config);
        Ok(McpRegistration {
            expiration: expires_at_ns,
        })
    })
}

/// Read `anchor_number`'s synced trusted-MCP-server config (master toggle +
/// trusted server URL). Returns the disabled, no-server default for an anchor
/// that has never written one. The caller must already be authorized for
/// `anchor_number` (checked by the canister method). The stored
/// `session_principal` pointer is internal bookkeeping and not exposed.
pub fn get_mcp_config(anchor_number: AnchorNumber) -> McpConfig {
    let stored = storage_borrow(|storage| storage.read_mcp_config(anchor_number));
    McpConfig {
        enabled: stored.enabled,
        url: stored.url,
    }
}

/// Persist `anchor_number`'s trusted-MCP-server config, overwriting any
/// previous value. Storing it on-chain (keyed by anchor) is what makes the
/// config sync across the identity's devices. The caller must already be
/// authorized for `anchor_number` (checked by the canister method).
///
/// Side effect: disabling MCP or changing the trusted server URL revokes the
/// identity's active session — the grant is deleted in the same update
/// message, so revocation is atomic with the config write. The URL comparison
/// is deliberately an exact string compare: it needs no URL parsing in the
/// canister, and erring towards revoking on any edit (even a path-only one)
/// is the safe direction — the agent just reconnects.
pub fn set_mcp_config(anchor_number: AnchorNumber, config: McpConfig) {
    storage_borrow_mut(|storage| {
        let old = storage.read_mcp_config(anchor_number);
        let revoke = !config.enabled || config.url != old.url;
        let session_principal = if revoke {
            if let Some(bytes) = &old.session_principal {
                if let Ok(principal) = Principal::try_from_slice(bytes) {
                    // Only remove a grant this anchor actually owns: a stale
                    // pointer must never delete another identity's session.
                    if storage
                        .lookup_mcp_grant(principal)
                        .is_some_and(|grant| grant.anchor_number == anchor_number)
                    {
                        storage.remove_mcp_grant(principal);
                    }
                }
            }
            None
        } else {
            old.session_principal
        };
        storage.write_mcp_config(
            anchor_number,
            StorableMcpConfig {
                enabled: config.enabled,
                url: config.url,
                session_principal,
            },
        );
    });
}

/// The calling MCP server's live session grant: present in the grant map, not
/// expired, and its identity's MCP toggle still on. Everything the
/// server-facing methods need for authorization — being the right caller is
/// the authorization.
fn caller_grant() -> Result<StorableMcpGrant, AccountDelegationError> {
    let caller = caller();
    let grant = storage_borrow(|storage| storage.lookup_mcp_grant(caller))
        .ok_or(AccountDelegationError::Unauthorized(caller))?;
    if grant.expires_at_ns <= time() {
        return Err(AccountDelegationError::Unauthorized(caller));
    }
    // Defense in depth: the master toggle is re-checked on every call even
    // though disabling already deletes the grant eagerly (see
    // [`set_mcp_config`]), so a future regression in the eager path can't
    // leave a disabled identity's session usable.
    let enabled = storage_borrow(|storage| storage.read_mcp_config(grant.anchor_number).enabled);
    if !enabled {
        return Err(AccountDelegationError::Unauthorized(caller));
    }
    Ok(grant)
}

/// Lazily delete `principal`'s grant if it has expired (clearing the owning
/// identity's forward pointer). Called from the update path when a caller
/// turns out to be unauthorized, so expired sessions don't linger until the
/// next connect; queries can't persist writes, so they only refuse.
fn cleanup_expired_grant(principal: Principal) {
    storage_borrow_mut(|storage| {
        let Some(grant) = storage.lookup_mcp_grant(principal) else {
            return;
        };
        if grant.expires_at_ns > time() {
            return;
        }
        storage.remove_mcp_grant(principal);
        let mut config = storage.read_mcp_config(grant.anchor_number);
        if config.session_principal.as_deref() == Some(principal.as_slice()) {
            config.session_principal = None;
            storage.write_mcp_config(grant.anchor_number, config);
        }
    });
}

/// `mcp_get_accounts`: list the calling MCP server's anchor's accounts at
/// `target_origin`, so the agent can discover which `account_number` values it
/// may request a delegation for via [`prepare_delegation`]. Authorized like
/// prepare/get — the anchor is recovered from `caller()`'s grant, never passed.
pub fn get_accounts(
    target_origin: FrontendHostname,
) -> Result<Vec<AccountInfo>, AccountDelegationError> {
    let anchor_number = caller_grant()?.anchor_number;
    Ok(
        account_management::get_accounts_for_origin(anchor_number, &target_origin)
            .iter()
            .map(|account| account.to_info())
            .collect(),
    )
}

/// `mcp_prepare_delegation`: mint a ≤1-hour account delegation for the calling
/// MCP server at `target_origin`, as `account_number` — one of the anchor's
/// accounts at that origin when given explicitly (discover them with
/// [`get_accounts`]), or the anchor's default account there when `None`.
/// (Accounts are per-origin, so this is an account at `target_origin`, the app
/// being acted on; no account is chosen at connect. An `account_number` that
/// isn't the anchor's at `target_origin` is rejected as `Unauthorized`.) The
/// delegation additionally never outlives the session grant.
///
/// Returns the resolved `account_number` so the server can thread the *same*
/// account into [`get_delegation`]: the default account at an origin is
/// mutable, so if `get` re-resolved it independently and it had changed in
/// between, it would look under a different account's seed and `NoSuchDelegation`.
pub async fn prepare_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<McpPrepareDelegation, AccountDelegationError> {
    let grant = match caller_grant() {
        Ok(grant) => grant,
        Err(err) => {
            // This is the update path, so take the chance to delete the grant
            // if the caller was an expired session.
            cleanup_expired_grant(caller());
            return Err(err);
        }
    };
    let anchor_number = grant.anchor_number;
    // Cap at 1 hour; the grant expiry is passed as an *absolute* cap so the
    // delegation can't outlive the session even by the time an await spans.
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
        Some(grant.expires_at_ns),
        &None,
    )
    .await?;
    Ok(McpPrepareDelegation {
        user_key: prepared.user_key,
        expiration: prepared.expiration,
        account_number,
    })
}

/// `mcp_get_delegation`: fetch the signed delegation prepared above.
/// `account_number` must be the value returned by the matching
/// [`prepare_delegation`], so `get` reads the same account `prepare` signed
/// for (see the note there); the caller is still authorized only for its own
/// anchor (recovered from `caller()`'s grant), so it can only fetch what it
/// prepared.
pub fn get_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    let anchor_number = caller_grant()?.anchor_number;
    account_management::get_account_delegation(
        anchor_number,
        &target_origin,
        account_number,
        session_key,
        expiration,
    )
}
