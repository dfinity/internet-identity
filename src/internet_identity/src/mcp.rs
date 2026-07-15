//! Backend `/mcp` path: lets an MCP server the user chooses to trust act on
//! the user's behalf at any app, without a per-app browser flow.
//!
//! At connect time the user authorizes the MCP server for their identity: the
//! II frontend fetches the server's session public key from the trusted
//! server's callback (an origin-attested channel — the frontend only ever
//! contacts the origin the identity's synced config trusts, so nothing from
//! the unauthenticated connect link is ever registered) and registers it here
//! via [`register`]: a grant `session principal -> (anchor, expiry, read_only)`
//! in the grant map, mirrored by a per-anchor forward pointer in the synced
//! [`StorableMcpConfig`]. No delegation is minted for the server itself, and
//! no account is chosen at connect. The MCP server then calls the
//! server-facing methods ([`McpSession::get_accounts`] /
//! [`McpSession::prepare_delegation`] / [`McpSession::get_delegation`]) signed
//! with that session key. Those operations live on [`McpSession`], a
//! capability obtainable *only* from [`authorize_mcp_session`] — the single
//! gate that recovers the anchor from `caller()`'s grant and checks expiry. So
//! being the right caller (the registered session key) is the only way to
//! reach them, and no `anchor_number` parameter is needed or trusted.
//!
//! Read-only is a property of the whole session: `read_only` is chosen once at
//! connect ([`register`]) and applied to every per-app delegation the session
//! mints (via [`crate::delegation::DelegationAccess`]), restricting them to
//! query calls.
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
    delegation::DelegationAccess,
    state::{storage_borrow, storage_borrow_mut},
    storage::account::{Account, AccountDelegationError, ReadAccountParams},
    storage::storable::mcp_config::StorableMcpConfig,
    storage::storable::mcp_grant::StorableMcpGrant,
};

/// Maximum lifetime of an MCP-minted per-app account delegation: 1 hour.
const MCP_MAX_EXPIRATION_PERIOD_NS: u64 = 60 * 60 * 1_000_000_000;

/// Longest trusted-server URL `set_mcp_config` accepts. Generous for a real MCP
/// endpoint (host + path) while bounding the one per-anchor config entry that
/// stores it verbatim (for display / re-probing). The per-connect registration
/// index never stores the URL verbatim — it stores a `SHA-256` hash — so this
/// is the only place URL length affects stored size.
pub(crate) const MCP_TRUSTED_URL_MAX_BYTES: usize = 2048;

/// Bounds for the MCP session grant lifetime: 10 minutes to 30 days.
/// Out-of-range requests are clamped (mirroring the frontend), not rejected.
/// `pub(crate)` because `mcp_registration` applies the same clamp *before*
/// deriving the registration principal, so the consent-bound TTL in the seed
/// is the effective one.
pub(crate) const MCP_GRANT_MIN_TTL_NS: u64 = 10 * 60 * 1_000_000_000;
pub(crate) const MCP_GRANT_MAX_TTL_NS: u64 = 30 * 24 * 60 * 60 * 1_000_000_000;

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
/// `grant_ttl_ns` is clamped to [10 min, 30 days]. `read_only` restricts every
/// per-app delegation this session mints to query calls (chosen at connect,
/// applied for the whole session). Registering while a session is live
/// replaces it (at most one session per identity), so the previous key stops
/// being authorized in the same message. A key holding a live grant for a
/// *different* identity is rejected — one key serves one identity — without
/// echoing whose it is.
pub fn register(
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    grant_ttl_ns: u64,
    read_only: bool,
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
                read_only,
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
///
/// Rejects a trusted URL longer than [`MCP_TRUSTED_URL_MAX_BYTES`]: nothing
/// legitimate needs a multi-KiB URL, and the bound keeps the stored config
/// entry small. The anchor's pending-registration bookkeeping is carried over
/// untouched — a config edit doesn't reset it (in-flight registrations are
/// invalidated at redemption by the URL-hash check, and pruned by expiry).
pub fn set_mcp_config(anchor_number: AnchorNumber, config: McpConfig) -> Result<(), String> {
    if let Some(url) = &config.url {
        if url.len() > MCP_TRUSTED_URL_MAX_BYTES {
            return Err(format!(
                "MCP configuration failed: trusted server URL must be at most {MCP_TRUSTED_URL_MAX_BYTES} bytes."
            ));
        }
    }
    storage_borrow_mut(|storage| {
        let old = storage.read_mcp_config(anchor_number);
        let pending_registrations = old.pending_registrations.clone();
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
                pending_registrations,
            },
        );
    });
    Ok(())
}

/// A live MCP session: proof that the current `caller()` is the MCP server
/// signing with the session key its user registered. Every server-facing
/// `mcp_*` operation ([`get_accounts`](Self::get_accounts),
/// [`prepare_delegation`](Self::prepare_delegation),
/// [`get_delegation`](Self::get_delegation)) is a method here, and the wrapped
/// grant is private, so an `McpSession` can be obtained *only* from
/// [`authorize_mcp_session`] / [`authorize_mcp_session_for_update`]. That makes
/// "these methods are reachable only via the registered session key" a
/// type-level invariant that holds across the `main.rs` endpoint boundary: an
/// endpoint cannot construct an `McpSession`, so it cannot skip the caller
/// check, and any newly added server-facing method — being a method on this
/// capability — inherits the same gate for free. The grant also carries the
/// session's `anchor_number` (recovered here, never taken from a call
/// argument) and its `read_only` flag (applied to every per-app delegation the
/// session mints).
pub struct McpSession {
    grant: StorableMcpGrant,
}

/// The single authorization gate for the server-facing `mcp_*` methods, and
/// the only constructor of [`McpSession`]. Succeeds exactly when `caller()` is
/// the self-authenticating principal of a session key that is
///   * present in the grant map — looking the caller up there *is* the proof
///     it signed with that key: the IC derives an ingress message's caller as
///     `self_authenticating(sender_pubkey)`, and the map is keyed by
///     `self_authenticating(session_key)`, so only the key's holder can match;
///   * not past its grant's expiry; and
///   * still the identity's *current* session under a live trusted-server
///     config: enabled, a trusted URL set, and the config's forward pointer
///     naming the caller. This re-checks on the read path everything the
///     write paths already maintain transactionally (registration requires
///     an enabled config with a URL and sets the pointer in the same message;
///     disable / URL-change / replacement delete the grant in the same
///     message), so it is behavior-neutral today — pure defense in depth: a
///     future regression in any write path can't leave a revoked, disabled,
///     or superseded session usable, because the one-session-per-identity
///     invariant is enforced here too, not just assumed from write-path
///     discipline.
///
/// Every other caller — the user's own authenticated principal, another
/// identity's session key, an anonymous or opaque principal — gets
/// [`Unauthorized`](AccountDelegationError::Unauthorized). This is pure (no
/// writes), so it is safe on the query path; the update path uses
/// [`authorize_mcp_session_for_update`], which additionally GCs an expired
/// grant.
pub fn authorize_mcp_session() -> Result<McpSession, AccountDelegationError> {
    let caller = caller();
    let grant = storage_borrow(|storage| storage.lookup_mcp_grant(caller))
        .ok_or(AccountDelegationError::Unauthorized(caller))?;
    if grant.expires_at_ns <= time() {
        return Err(AccountDelegationError::Unauthorized(caller));
    }
    let config = storage_borrow(|storage| storage.read_mcp_config(grant.anchor_number));
    let is_current_session = config
        .session_principal
        .as_deref()
        .is_some_and(|bytes| bytes == caller.as_slice());
    if !config.enabled || config.url.is_none() || !is_current_session {
        return Err(AccountDelegationError::Unauthorized(caller));
    }
    Ok(McpSession { grant })
}

/// [`authorize_mcp_session`] for the update path: identical authorization, but
/// on failure it also deletes the caller's grant if it has expired, so expired
/// sessions don't linger until the next connect. Queries can't persist writes,
/// so they use the pure gate and only refuse.
pub fn authorize_mcp_session_for_update() -> Result<McpSession, AccountDelegationError> {
    match authorize_mcp_session() {
        Ok(session) => Ok(session),
        Err(err) => {
            cleanup_expired_grant(caller());
            Err(err)
        }
    }
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

impl McpSession {
    /// `mcp_get_accounts`: list this session's anchor's accounts at
    /// `target_origin`, so the agent can discover which `account_number`
    /// values it may request a delegation for via
    /// [`prepare_delegation`](Self::prepare_delegation). The anchor is the
    /// session's (recovered from the caller's grant), never passed in.
    pub fn get_accounts(
        &self,
        target_origin: FrontendHostname,
    ) -> Result<Vec<AccountInfo>, AccountDelegationError> {
        Ok(
            account_management::get_accounts_for_origin(self.grant.anchor_number, &target_origin)
                .iter()
                .map(|account| account.to_info())
                .collect(),
        )
    }

    /// `mcp_prepare_delegation`: mint a ≤1-hour account delegation for this
    /// session at `target_origin`, as `account_number` — one of the anchor's
    /// accounts at that origin when given explicitly (discover them with
    /// [`get_accounts`](Self::get_accounts)), or the anchor's default account
    /// there when `None`. (Accounts are per-origin, so this is an account at
    /// `target_origin`, the app being acted on; no account is chosen at
    /// connect. An `account_number` that isn't the anchor's at `target_origin`
    /// is rejected as `Unauthorized`.) The delegation never outlives the
    /// session grant, and is restricted to query calls when the session was
    /// registered read-only.
    ///
    /// Returns the resolved `account_number` so the server can thread the
    /// *same* account into [`get_delegation`](Self::get_delegation): the
    /// default account at an origin is mutable, so if `get` re-resolved it
    /// independently and it had changed in between, it would look under a
    /// different account's seed and `NoSuchDelegation`.
    pub async fn prepare_delegation(
        self,
        target_origin: FrontendHostname,
        account_number: Option<AccountNumber>,
        session_key: SessionKey,
        max_ttl: Option<u64>,
    ) -> Result<McpPrepareDelegation, AccountDelegationError> {
        let anchor_number = self.grant.anchor_number;
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
            Some(self.grant.expires_at_ns),
            DelegationAccess::from_read_only(self.grant.read_only),
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
    /// [`prepare_delegation`](Self::prepare_delegation), so `get` reads the
    /// same account `prepare` signed for (see the note there); the caller is
    /// still authorized only for its own anchor (recovered from `caller()`'s
    /// grant), so it can only fetch what it prepared. The grant's `read_only`
    /// must match what `prepare` signed with, since the access level is folded
    /// into the delegation signature.
    pub fn get_delegation(
        &self,
        target_origin: FrontendHostname,
        account_number: Option<AccountNumber>,
        session_key: SessionKey,
        expiration: Timestamp,
    ) -> Result<SignedDelegation, AccountDelegationError> {
        account_management::get_account_delegation(
            self.grant.anchor_number,
            &target_origin,
            account_number,
            session_key,
            expiration,
            DelegationAccess::from_read_only(self.grant.read_only),
        )
    }
}
