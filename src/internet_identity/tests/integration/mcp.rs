//! Integration tests for the backend `/mcp` path: an MCP server the user
//! authorizes for their identity fetches per-app account delegations without a
//! per-app browser flow. No account is chosen at connect (the connector isn't
//! an app); the server's own session-key principal is registered as a grant
//! via `mcp_register`, recovered from `caller()` on every server-facing call
//! (checking expiry), and the app account is picked per call against the
//! target origin. At most one session exists per identity, and changing the
//! synced config (disable, or a different trusted URL) revokes it.

use candid::Principal;
use canister_tests::{
    api::internet_identity::api_v2::{
        create_account, get_mcp_registration_delegation, mcp_get_accounts, mcp_get_config,
        mcp_get_delegation, mcp_prepare_delegation, mcp_register, mcp_register_v2, mcp_set_config,
        prepare_account_delegation, prepare_mcp_registration_delegation, set_default_account,
        AccountDelegationParams,
    },
    flows,
    framework::{
        device_data_2, env, install_ii_canister_with_arg, principal_1, principal_2, time,
        upgrade_ii_canister, verify_delegation, II_WASM,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountDelegationError, AnchorNumber, McpConfig, McpPrepareDelegation, Permissions,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const MCP_ORIGIN: &str = "https://mcp.id.ai";
/// The backend caps MCP-minted per-app delegations at 1 hour.
const MCP_MAX_TTL_NS: u64 = 60 * 60 * 1_000_000_000;
/// The backend clamps session-grant lifetimes to [10 min, 30 days].
const GRANT_MIN_TTL_NS: u64 = 10 * 60 * 1_000_000_000;
const GRANT_MAX_TTL_NS: u64 = 30 * 24 * 60 * 60 * 1_000_000_000;
/// Grant lifetime used by tests that don't probe the bounds: 1 day.
const GRANT_TTL_NS: u64 = 24 * 60 * 60 * 1_000_000_000;

// The `/mcp` path is not gated by a global config; each identity trusts the
// server it chooses via its synced config. So a plain install suffices.
fn install_with_mcp(env: &PocketIc) -> Principal {
    install_ii_canister_with_arg(env, II_WASM.clone(), None)
}

/// Set the identity's synced config to trust `MCP_ORIGIN` — the precondition
/// for registering a session (and the lever that later revokes it).
fn trust_mcp_server(
    env: &PocketIc,
    canister_id: Principal,
    sender: Principal,
    anchor: AnchorNumber,
) {
    mcp_set_config(
        env,
        canister_id,
        sender,
        anchor,
        McpConfig {
            enabled: true,
            url: Some(format!("{MCP_ORIGIN}/mcp")),
        },
    )
    .unwrap()
    .unwrap();
}

/// Register `session_key` as the anchor's MCP session (called as the user,
/// like the `/mcp` connect flow does after consent) and return the session
/// principal the server now calls with — the self-authenticating principal of
/// the key, exactly as the canister derives it — plus the grant expiration.
/// Registers an unrestricted (not read-only) session; the read-only path has
/// its own test.
fn register_session(
    env: &PocketIc,
    canister_id: Principal,
    sender: Principal,
    anchor: AnchorNumber,
    session_key: &ByteBuf,
    grant_ttl_ns: u64,
) -> (Principal, u64) {
    let registration = mcp_register(
        env,
        canister_id,
        sender,
        anchor,
        session_key.clone(),
        grant_ttl_ns,
        None,
    )
    .unwrap()
    .unwrap();
    (
        Principal::self_authenticating(session_key),
        registration.expiration,
    )
}

/// The happy path: a registered session mints a per-app delegation by caller
/// alone (no `anchor_number` arg), capped at 1 hour, acting as the same
/// principal the anchor's default account at that app gets via the regular API.
#[test]
fn mcp_mints_per_app_delegation_authorized_by_caller() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("mcp per-app session key");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, grant_expiration) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );
    assert_eq!(grant_expiration, time(&env) + GRANT_TTL_NS);

    // Mint the per-app delegation as the MCP server — no anchor_number passed.
    let McpPrepareDelegation {
        user_key,
        expiration,
        account_number,
    } = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None, // account_number: use the anchor's default at the app
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    // Default TTL is the 1-hour cap.
    assert_eq!(expiration, time(&env) + MCP_MAX_TTL_NS);

    // `get` is handed back the same account `prepare` resolved.
    let signed = mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        account_number,
        session_key.clone(),
        expiration,
    )
    .unwrap()
    .unwrap();
    verify_delegation(&env, user_key.clone(), &signed, &env.root_key().unwrap());
    assert_eq!(signed.delegation.pubkey, session_key);
    assert_eq!(signed.delegation.expiration, expiration);

    // Parity: the MCP-minted delegation acts as the SAME principal the anchor's
    // default account at `target` gets via the regular account-delegation API.
    // (This is the per-app principal check that used to live in the e2e suite;
    // it is canister logic, so it belongs here.)
    let regular = AccountDelegationParams::new(
        &env,
        canister_id,
        principal_1(),
        anchor,
        target.clone(),
        None,
        ByteBuf::from("regular session key"),
    );
    let regular_user_key = prepare_account_delegation(&regular, None)
        .unwrap()
        .unwrap()
        .user_key;
    assert_eq!(
        Principal::self_authenticating(&user_key),
        Principal::self_authenticating(&regular_user_key),
    );

    Ok(())
}

/// A longer requested TTL is clamped to the 1-hour cap.
#[test]
fn mcp_delegation_ttl_capped_at_1_hour() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        None,
        ByteBuf::from("k"),
        Some(Duration::from_secs(2 * 3600).as_nanos() as u64), // request 2 hours
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.expiration, time(&env) + MCP_MAX_TTL_NS);

    Ok(())
}

/// A per-app delegation never outlives the session grant: with less than an
/// hour of grant left, the delegation expires exactly when the grant does.
#[test]
fn mcp_delegation_never_outlives_the_grant() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    // The whole grant lasts 10 minutes — shorter than the 1-hour delegation cap.
    let (mcp, grant_expiration) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_MIN_TTL_NS,
    );

    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        None,
        ByteBuf::from("k"),
        None, // default request would be the 1-hour cap
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.expiration, grant_expiration);

    Ok(())
}

/// Callers that hold no grant — a never-registered session key, or an
/// unrelated principal — are rejected.
#[test]
fn mcp_rejects_unregistered_callers() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    // A session key that was never registered.
    let unregistered = Principal::self_authenticating(ByteBuf::from("never registered"));
    match mcp_prepare_delegation(
        &env,
        canister_id,
        unregistered,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    {
        Err(AccountDelegationError::Unauthorized(p)) => assert_eq!(p, unregistered),
        Ok(_) => panic!("expected Unauthorized, got Ok"),
        Err(e) => panic!("expected Unauthorized, got {e:?}"),
    }

    // An unrelated principal is rejected too.
    match mcp_prepare_delegation(
        &env,
        canister_id,
        principal_2(),
        target,
        None,
        session_key,
        None,
    )
    .unwrap()
    {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized, got Ok"),
        Err(e) => panic!("expected Unauthorized, got {e:?}"),
    }

    Ok(())
}

/// Every server-facing `mcp_*` method is reachable *only* via the registered
/// session key. With a live session in place, a caller that is not that key —
/// the identity's own owner principal, an unrelated principal, or a
/// never-registered session key — is `Unauthorized` on `mcp_get_accounts`,
/// `mcp_prepare_delegation`, and `mcp_get_delegation` alike, and the rejection
/// names that caller. This locks in the single-gate invariant
/// (`authorize_mcp_session`) across the whole server-facing surface, so a new
/// method on that surface can't quietly ship without the check.
#[test]
fn mcp_server_facing_methods_reject_non_session_callers() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("per-app session key");

    // A live session exists; none of the callers probed below hold it.
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, expiration) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    // Sanity: the real session key IS authorized, so a blanket reject below
    // would be a false negative rather than the gate doing its job.
    assert!(mcp_get_accounts(&env, canister_id, mcp, target.clone())
        .unwrap()
        .is_ok());

    // Callers that are not the session key: the anchor's own owner principal
    // (even the user can't drive the server-facing methods directly), an
    // unrelated principal, and a session key that was never registered.
    let non_session_callers = [
        principal_1(),
        principal_2(),
        Principal::self_authenticating(ByteBuf::from("never registered")),
    ];

    for caller in non_session_callers {
        assert!(
            matches!(
                mcp_get_accounts(&env, canister_id, caller, target.clone()).unwrap(),
                Err(AccountDelegationError::Unauthorized(p)) if p == caller
            ),
            "mcp_get_accounts admitted a non-session caller: {caller}"
        );
        assert!(
            matches!(
                mcp_prepare_delegation(
                    &env,
                    canister_id,
                    caller,
                    target.clone(),
                    None,
                    session_key.clone(),
                    None,
                )
                .unwrap(),
                Err(AccountDelegationError::Unauthorized(p)) if p == caller
            ),
            "mcp_prepare_delegation admitted a non-session caller: {caller}"
        );
        assert!(
            matches!(
                mcp_get_delegation(
                    &env,
                    canister_id,
                    caller,
                    target.clone(),
                    None,
                    session_key.clone(),
                    expiration,
                )
                .unwrap(),
                Err(AccountDelegationError::Unauthorized(p)) if p == caller
            ),
            "mcp_get_delegation admitted a non-session caller: {caller}"
        );
    }

    Ok(())
}

/// The grant expires: the session can mint before its expiration, not after.
#[test]
fn mcp_grant_expires() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_MIN_TTL_NS, // 10 minutes
    );
    assert!(mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None
    )
    .unwrap()
    .is_ok());

    env.advance_time(Duration::from_secs(11 * 60));
    match mcp_prepare_delegation(&env, canister_id, mcp, target, None, session_key, None).unwrap() {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after grant expiry, got Ok"),
        Err(e) => panic!("expected Unauthorized after grant expiry, got {e:?}"),
    }

    Ok(())
}

/// Disabling MCP in the synced config revokes the session — and re-enabling
/// does not resurrect it: the user reconnects through a fresh consent flow.
#[test]
fn mcp_disabling_config_revokes_the_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );
    assert!(mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None
    )
    .unwrap()
    .is_ok());

    // Un-toggle (same URL): the grant is deleted in the same message.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: false,
            url: Some(format!("{MCP_ORIGIN}/mcp")),
        },
    )
    .unwrap()
    .unwrap();
    match mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after disabling, got Ok"),
        Err(e) => panic!("expected Unauthorized after disabling, got {e:?}"),
    }

    // Toggling back on restores nothing — the session is gone until the user
    // reconnects.
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    match mcp_prepare_delegation(&env, canister_id, mcp, target, None, session_key, None).unwrap() {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after re-enable, got Ok"),
        Err(e) => panic!("expected Unauthorized after re-enable, got {e:?}"),
    }

    Ok(())
}

/// Changing the trusted server URL revokes the session: the previous server's
/// key must not survive a change of trust.
#[test]
fn mcp_changing_server_url_revokes_the_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );
    assert!(mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None
    )
    .unwrap()
    .is_ok());

    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: true,
            url: Some("https://other-mcp.example.com/mcp".to_string()),
        },
    )
    .unwrap()
    .unwrap();
    match mcp_prepare_delegation(&env, canister_id, mcp, target, None, session_key, None).unwrap() {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after URL change, got Ok"),
        Err(e) => panic!("expected Unauthorized after URL change, got {e:?}"),
    }

    Ok(())
}

/// At most one session per identity: registering a new session key replaces
/// the previous grant immediately.
#[test]
fn mcp_registration_replaces_previous_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (first, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("first session key"),
        GRANT_TTL_NS,
    );
    let (second, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("second session key"),
        GRANT_TTL_NS,
    );

    match mcp_prepare_delegation(
        &env,
        canister_id,
        first,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected the replaced session to be Unauthorized, got Ok"),
        Err(e) => panic!("expected the replaced session to be Unauthorized, got {e:?}"),
    }
    assert!(
        mcp_prepare_delegation(&env, canister_id, second, target, None, session_key, None)
            .unwrap()
            .is_ok()
    );

    Ok(())
}

/// Registration requires a live trusted-server config (enabled + URL set) —
/// that coupling is what makes config-driven revocation cover every session —
/// and is gated to the identity itself.
#[test]
fn mcp_register_requires_enabled_config() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let session_key = ByteBuf::from("mcp server session key");

    // No config written at all.
    assert!(mcp_register(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key.clone(),
        GRANT_TTL_NS,
        None
    )
    .unwrap()
    .is_err());

    // Disabled config with a URL.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: false,
            url: Some(format!("{MCP_ORIGIN}/mcp")),
        },
    )
    .unwrap()
    .unwrap();
    assert!(mcp_register(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key.clone(),
        GRANT_TTL_NS,
        None
    )
    .unwrap()
    .is_err());

    // Enabled config without a URL.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: true,
            url: None,
        },
    )
    .unwrap()
    .unwrap();
    assert!(mcp_register(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key.clone(),
        GRANT_TTL_NS,
        None
    )
    .unwrap()
    .is_err());

    // An unrelated caller cannot register a session for the anchor even with a
    // live config.
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    assert!(mcp_register(
        &env,
        canister_id,
        principal_2(),
        anchor,
        session_key,
        GRANT_TTL_NS,
        None
    )
    .unwrap()
    .is_err());

    Ok(())
}

/// One key serves one identity: a session key with a live grant for another
/// anchor is rejected (without echoing whose it is). Once that grant expires
/// the key can be re-registered by the other anchor — and the first anchor's
/// stale config pointer must not damage the new owner's session.
#[test]
fn mcp_register_rejects_a_key_registered_to_another_identity() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor_1 = flows::register_anchor(&env, canister_id);
    let anchor_2 = flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());
    let shared_key = ByteBuf::from("shared session key");
    let target = "https://some-app.com".to_string();

    trust_mcp_server(&env, canister_id, principal_1(), anchor_1);
    trust_mcp_server(&env, canister_id, principal_2(), anchor_2);

    // Anchor 1 registers the key with a 10-minute grant.
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor_1,
        &shared_key,
        GRANT_MIN_TTL_NS,
    );

    // While that grant is live, anchor 2 cannot register the same key — and
    // the error must not leak whose it is.
    let err = mcp_register(
        &env,
        canister_id,
        principal_2(),
        anchor_2,
        shared_key.clone(),
        GRANT_TTL_NS,
        None,
    )
    .unwrap()
    .unwrap_err();
    assert!(!err.contains(&anchor_1.to_string()));

    // Once anchor 1's grant expires, anchor 2 can take the key over.
    env.advance_time(Duration::from_secs(11 * 60));
    register_session(
        &env,
        canister_id,
        principal_2(),
        anchor_2,
        &shared_key,
        GRANT_TTL_NS,
    );

    // Anchor 1's config still points at the same principal from before the
    // takeover; revoking through it must not delete anchor 2's session.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor_1,
        McpConfig {
            enabled: false,
            url: Some(format!("{MCP_ORIGIN}/mcp")),
        },
    )
    .unwrap()
    .unwrap();
    assert!(mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target,
        None,
        ByteBuf::from("k"),
        None
    )
    .unwrap()
    .is_ok());

    Ok(())
}

/// Requested grant lifetimes are clamped to [10 min, 30 days], mirroring the
/// frontend, so the backend never trusts the frontend for the bounds.
#[test]
fn mcp_grant_ttl_is_clamped() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    // Over the 30-day cap: clamped down.
    let (_, expiration) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("long key"),
        GRANT_MAX_TTL_NS + GRANT_TTL_NS,
    );
    assert_eq!(expiration, time(&env) + GRANT_MAX_TTL_NS);

    // Under the 10-minute floor: clamped up.
    let (_, expiration) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("short key"),
        1,
    );
    assert_eq!(expiration, time(&env) + GRANT_MIN_TTL_NS);

    Ok(())
}

/// The grant lives in stable memory: a registered session keeps working
/// across a canister upgrade (an upgrade must not disconnect agents).
#[test]
fn mcp_grant_persists_across_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert!(mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        None,
        ByteBuf::from("k"),
        None
    )
    .unwrap()
    .is_ok());

    Ok(())
}

/// Regression: `get` must read the account `prepare` signed for, even if the
/// anchor's default account at the target origin changes in between. `prepare`
/// returns the resolved `account_number`; threading it into `get` keeps the two
/// consistent. Re-resolving the (mutable) default in `get` — as the code used to
/// — would derive a different seed and return `NoSuchDelegation`.
#[test]
fn mcp_get_uses_prepared_account_despite_default_change() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    // Prepare without naming an account resolves the default at `target`
    // (None = the synthetic default, since none is reserved yet) and reports it.
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.account_number, None);

    // The user reserves a *different* default account at `target` after preparing,
    // so re-resolving the default would now point elsewhere.
    let new_default = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        target.clone(),
        "work".to_string(),
    )
    .unwrap()
    .unwrap()
    .account_number;
    set_default_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        target.clone(),
        new_default,
    )
    .unwrap()
    .unwrap();

    // `get` with the account `prepare` returned still finds the delegation...
    assert!(mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        prepared.account_number,
        session_key.clone(),
        prepared.expiration,
    )
    .unwrap()
    .is_ok());
    // ...while the now-current (changed) default has nothing prepared for it,
    // which is exactly the divergence the old re-resolving `get` would have hit.
    assert!(matches!(
        mcp_get_delegation(
            &env,
            canister_id,
            mcp,
            target,
            new_default,
            session_key,
            prepared.expiration,
        )
        .unwrap(),
        Err(AccountDelegationError::NoSuchDelegation)
    ));

    Ok(())
}

/// The MCP server can name a specific (non-default) account at the target app
/// when preparing; `prepare` mints for that account (validated as the anchor's
/// at that origin), reports it back, and `get` resolves it with the same number.
#[test]
fn mcp_prepares_for_an_explicitly_named_account() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    // A specific account the user holds at the target app.
    let account = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        target.clone(),
        "work".to_string(),
    )
    .unwrap()
    .unwrap()
    .account_number;
    assert!(account.is_some());

    // Naming it explicitly mints for exactly that account and echoes it back.
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        account,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.account_number, account);

    // `get` resolves that same account's delegation.
    assert!(mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target,
        account,
        session_key,
        prepared.expiration,
    )
    .unwrap()
    .is_ok());

    Ok(())
}

/// The agent discovers the anchor's accounts at an app via `mcp_get_accounts`
/// (authorized as the registered session principal) and then prepares a
/// delegation for one of them — closing the loop with no out-of-band
/// knowledge. An unrelated principal cannot list.
#[test]
fn mcp_lists_accounts_then_prepares_for_one() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );

    // The user holds a named account at the target app (created via their device).
    let created = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        target.clone(),
        "work".to_string(),
    )
    .unwrap()
    .unwrap()
    .account_number;
    assert!(created.is_some());

    // The agent lists the anchor's accounts at the app as the session principal.
    let accounts = mcp_get_accounts(&env, canister_id, mcp, target.clone())
        .unwrap()
        .unwrap();
    assert!(accounts.iter().any(|a| a.account_number == created));

    // It can then prepare a delegation for a listed account.
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        created,
        ByteBuf::from("k"),
        None,
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.account_number, created);

    // A principal that holds no grant cannot list.
    match mcp_get_accounts(&env, canister_id, principal_2(), target).unwrap() {
        Err(AccountDelegationError::Unauthorized(p)) => assert_eq!(p, principal_2()),
        Ok(_) => panic!("expected Unauthorized, got Ok"),
        Err(e) => panic!("expected Unauthorized, got {e:?}"),
    }

    Ok(())
}

/// The trusted-MCP-server config round-trips through the authenticated
/// get/set methods and survives a canister upgrade — it lives in stable
/// memory, which is what lets it sync across the identity's devices.
#[test]
fn mcp_config_round_trips_and_persists_across_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    // An anchor that never wrote a config reads the disabled, no-server default.
    assert_eq!(
        mcp_get_config(&env, canister_id, principal_1(), anchor).unwrap(),
        McpConfig::default()
    );

    let config = McpConfig {
        enabled: true,
        url: Some(format!("{MCP_ORIGIN}/mcp")),
    };
    mcp_set_config(&env, canister_id, principal_1(), anchor, config.clone())
        .unwrap()
        .unwrap();
    assert_eq!(
        mcp_get_config(&env, canister_id, principal_1(), anchor).unwrap(),
        config
    );

    // Persisted in stable memory: the same config reads back after an upgrade.
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    assert_eq!(
        mcp_get_config(&env, canister_id, principal_1(), anchor).unwrap(),
        config
    );

    Ok(())
}

/// Only the authenticated identity can change what it trusts. An unrelated
/// caller's write is rejected and an unrelated caller's read returns the
/// (safe) default rather than leaking the real config.
#[test]
fn mcp_config_is_gated_to_the_identity() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    let config = McpConfig {
        enabled: true,
        url: Some(format!("{MCP_ORIGIN}/mcp")),
    };
    mcp_set_config(&env, canister_id, principal_1(), anchor, config.clone())
        .unwrap()
        .unwrap();

    // A different caller can neither overwrite the config...
    assert!(mcp_set_config(
        &env,
        canister_id,
        principal_2(),
        anchor,
        McpConfig::default(),
    )
    .unwrap()
    .is_err());
    // ...nor read the real one back (gets the default instead).
    assert_eq!(
        mcp_get_config(&env, canister_id, principal_2(), anchor).unwrap(),
        McpConfig::default()
    );
    // The owner's config is unchanged.
    assert_eq!(
        mcp_get_config(&env, canister_id, principal_1(), anchor).unwrap(),
        config
    );

    Ok(())
}

/// Read-only is a property of the whole session: registering with
/// `read_only = true` makes every per-app delegation the session mints
/// queries-only (it carries `permissions = "queries"`), while an unrestricted
/// session's delegations carry no `permissions` field. The choice is made once
/// at `mcp_register` and stored on the grant — there is no per-call variant.
#[test]
fn mcp_read_only_grant_mints_queries_only_delegations() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("per-app session key");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    // Register a read-only session (read_only = Some(true)).
    let server_key = ByteBuf::from("mcp server session key");
    mcp_register(
        &env,
        canister_id,
        principal_1(),
        anchor,
        server_key.clone(),
        GRANT_TTL_NS,
        Some(true),
    )
    .unwrap()
    .unwrap();
    let mcp = Principal::self_authenticating(&server_key);

    // Every delegation the session mints is queries-only.
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    let signed = mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        prepared.account_number,
        session_key.clone(),
        prepared.expiration,
    )
    .unwrap()
    .unwrap();
    verify_delegation(&env, prepared.user_key, &signed, &env.root_key().unwrap());
    assert_eq!(signed.delegation.permissions, Some("queries".to_string()));

    // Contrast: an unrestricted session (the default helper) mints delegations
    // with no `permissions` restriction.
    let anchor_2 = flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());
    trust_mcp_server(&env, canister_id, principal_2(), anchor_2);
    let (mcp_2, _) = register_session(
        &env,
        canister_id,
        principal_2(),
        anchor_2,
        &ByteBuf::from("full-access server key"),
        GRANT_TTL_NS,
    );
    let prepared_2 = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp_2,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    let signed_2 = mcp_get_delegation(
        &env,
        canister_id,
        mcp_2,
        target,
        prepared_2.account_number,
        session_key,
        prepared_2.expiration,
    )
    .unwrap()
    .unwrap();
    assert_eq!(signed_2.delegation.permissions, None);

    Ok(())
}

/// Expiry must hold on the pure QUERY gate too, not only on the update path:
/// `mcp_get_accounts` / `mcp_get_delegation` are queries authorized by
/// `authorize_mcp_session` directly, while the existing expiry test drives
/// `mcp_prepare_delegation` through the update wrapper. A regression that
/// moved the expiry check into the update-only wrapper would let an expired
/// session keep reading accounts and fetching signed delegations — this pins
/// the query gate independently.
#[test]
fn mcp_query_methods_reject_expired_grant() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_MIN_TTL_NS, // 10 minutes
    );
    // While live: prepare once (so a delegation exists to fetch) and confirm
    // both queries answer.
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    assert!(mcp_get_accounts(&env, canister_id, mcp, target.clone())
        .unwrap()
        .is_ok());

    env.advance_time(Duration::from_secs(11 * 60));
    // Queries read the certified time of the last executed round, so tick once
    // for the advanced clock to reach the query gate (the update-path expiry
    // test observes the advance via the update call's own round instead).
    env.tick();

    // Both query methods refuse the expired session — including fetching a
    // delegation that was validly prepared while the grant was live.
    assert!(matches!(
        mcp_get_accounts(&env, canister_id, mcp, target.clone()).unwrap(),
        Err(AccountDelegationError::Unauthorized(p)) if p == mcp
    ));
    assert!(matches!(
        mcp_get_delegation(
            &env,
            canister_id,
            mcp,
            target,
            prepared.account_number,
            session_key,
            prepared.expiration,
        )
        .unwrap(),
        Err(AccountDelegationError::Unauthorized(p)) if p == mcp
    ));

    Ok(())
}

/// `get` fails closed on every mismatched lookup input, not just the account:
/// the signature is stored under (seed, session_key, expiration, permissions),
/// so a wrong `expiration` or a wrong per-app `session_key` must yield
/// `NoSuchDelegation` — never a delegation minted for different parameters.
/// (The mismatched-account case is covered by
/// `mcp_get_uses_prepared_account_despite_default_change`.)
#[test]
fn mcp_get_delegation_rejects_mismatched_expiration_and_session_key() -> Result<(), RejectResponse>
{
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("per-app session key");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let (mcp, _) = register_session(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from("mcp server session key"),
        GRANT_TTL_NS,
    );
    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    // Wrong expiration (off by one nanosecond) finds nothing.
    assert!(matches!(
        mcp_get_delegation(
            &env,
            canister_id,
            mcp,
            target.clone(),
            prepared.account_number,
            session_key.clone(),
            prepared.expiration + 1,
        )
        .unwrap(),
        Err(AccountDelegationError::NoSuchDelegation)
    ));
    // Wrong per-app session key finds nothing.
    assert!(matches!(
        mcp_get_delegation(
            &env,
            canister_id,
            mcp,
            target.clone(),
            prepared.account_number,
            ByteBuf::from("a different per-app key"),
            prepared.expiration,
        )
        .unwrap(),
        Err(AccountDelegationError::NoSuchDelegation)
    ));
    // Sanity: the exact prepared triple resolves.
    assert!(mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target,
        prepared.account_number,
        session_key,
        prepared.expiration,
    )
    .unwrap()
    .is_ok());

    Ok(())
}

/// A read-only grant stays read-only across a canister upgrade: the persisted
/// `read_only` flag on the stored grant — not just the grant's existence —
/// must survive, so delegations minted after the upgrade still carry
/// `permissions = "queries"`. Composes the persistence test (grant survives)
/// with the read-only test (queries-only minting), which individually don't
/// cover the flag's persistence.
#[test]
fn mcp_read_only_grant_stays_queries_only_across_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("per-app session key");

    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    let server_key = ByteBuf::from("mcp server session key");
    mcp_register(
        &env,
        canister_id,
        principal_1(),
        anchor,
        server_key.clone(),
        GRANT_TTL_NS,
        Some(true),
    )
    .unwrap()
    .unwrap();
    let mcp = Principal::self_authenticating(&server_key);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let prepared = mcp_prepare_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        None,
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();
    let signed = mcp_get_delegation(
        &env,
        canister_id,
        mcp,
        target,
        prepared.account_number,
        session_key,
        prepared.expiration,
    )
    .unwrap()
    .unwrap();
    verify_delegation(&env, prepared.user_key, &signed, &env.root_key().unwrap());
    assert_eq!(signed.delegation.permissions, Some("queries".to_string()));

    Ok(())
}

/// Phase-2 registration delegation, happy path. The user consents by minting a
/// `P_reg -> Y` registration delegation (`prepare` + `get`); the delegation is a
/// valid II canister signature over the browser-held registration key `Y`; the
/// MCP server then redeems it — authenticated as `P_reg` (the chain root) — via
/// `mcp_register_v2` to bind its long-lived session key `S`. Nothing is stored:
/// `P_reg` is derived from the consent tuple (anchor, read-only choice, grant
/// lifetime, trusted URL), so the server must echo exactly what the user
/// consented to for the derivation to land on `caller()`. The resulting grant
/// authorizes the server-facing `mcp_*` methods.
#[test]
fn mcp_register_v2_binds_session_via_registration_delegation() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    // The ephemeral registration key Y the II frontend generates for this
    // connect (browser-held; the chain is extended to the server's key
    // browser-side). The user consents (full auth) by minting the delegation.
    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key.clone(),
        Some(true), // read-only session
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();

    // The frontend fetches the signed P_reg -> Y delegation with the same
    // consent parameters (they determine the derived seed); it is a valid II
    // canister signature over exactly the registration key.
    let signed = get_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key.clone(),
        Some(true),
        Some(GRANT_TTL_NS),
        prepared.expiration,
    )
    .unwrap()
    .unwrap();
    verify_delegation(
        &env,
        prepared.user_key.clone(),
        &signed,
        &env.root_key().unwrap(),
    );
    assert_eq!(signed.delegation.pubkey, registration_key);
    assert_eq!(signed.delegation.permissions, None);

    // The MCP server redeems the delegation as P_reg, echoing the consented
    // parameters; the canister re-derives P_reg from them and matches caller().
    let p_reg = Principal::self_authenticating(&prepared.user_key);
    let server_key = ByteBuf::from("mcp server session key S");
    let registration = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key.clone(),
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    assert_eq!(registration.permissions, Permissions::Queries);

    // The grant is live and bound to `anchor`: the session key S now reaches
    // the server-facing methods.
    let accounts = mcp_get_accounts(
        &env,
        canister_id,
        Principal::self_authenticating(&server_key),
        MCP_ORIGIN.to_string(),
    )
    .unwrap();
    assert!(
        accounts.is_ok(),
        "the registered session should be authorized: {accounts:?}"
    );

    Ok(())
}

/// `mcp_register_v2` authorizes by derive-and-compare, so any parameter the
/// user did not consent to derives a different `P_reg` and is rejected: the
/// server cannot upgrade a read-only session to full access, stretch the grant
/// lifetime, or bind the session to a different anchor.
#[test]
fn mcp_register_v2_rejects_altered_consent_parameters() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);
    // A second anchor with the same trusted server, to rule out cross-anchor
    // redemption specifically (not just a missing config).
    let other_anchor = flows::register_anchor_with_device(&env, canister_id, &device_data_2());
    trust_mcp_server(&env, canister_id, principal_2(), other_anchor);

    // The user consents to a read-only session with the default lifetime.
    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key,
        Some(true), // read-only
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    let p_reg = Principal::self_authenticating(&prepared.user_key);
    let server_key = ByteBuf::from("mcp server session key S");

    // Upgraded access level: rejected.
    let full_access = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key.clone(),
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&full_access, Err(message) if message.contains("not authorized")),
        "an upgraded access level must derive a different principal: {full_access:?}"
    );

    // Stretched grant lifetime: rejected.
    let longer_ttl = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key.clone(),
        Some(true),
        Some(GRANT_TTL_NS * 2),
    )
    .unwrap();
    assert!(
        matches!(&longer_ttl, Err(message) if message.contains("not authorized")),
        "an altered grant lifetime must derive a different principal: {longer_ttl:?}"
    );

    // Different anchor (with its own valid config): rejected.
    let wrong_anchor = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        other_anchor,
        server_key.clone(),
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&wrong_anchor, Err(message) if message.contains("not authorized")),
        "a different anchor must derive a different principal: {wrong_anchor:?}"
    );

    // The exact consented tuple still works — the rejections above were the
    // parameters, not the delegation.
    let registration = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key,
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    assert_eq!(registration.permissions, Permissions::Queries);

    Ok(())
}

/// The trusted server URL is folded into the derived `P_reg`, so a config
/// change between consent and redemption — the user switching the trusted
/// server, or disabling MCP — invalidates an in-flight registration
/// delegation: the re-derivation no longer lands on `caller()`.
#[test]
fn mcp_register_v2_rejects_after_config_change() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key,
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    let p_reg = Principal::self_authenticating(&prepared.user_key);

    // The user switches the trusted server before the delegation is redeemed.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: true,
            url: Some("https://other.example.com/mcp".to_string()),
        },
    )
    .unwrap()
    .unwrap();

    let server_key = ByteBuf::from("mcp server session key S");
    let stale = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key.clone(),
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&stale, Err(message) if message.contains("not authorized")),
        "a delegation minted under the old trusted URL must not redeem under the new one: {stale:?}"
    );

    // Disabling MCP outright rejects too — with the same generic error as a
    // derivation mismatch: `mcp_register_v2` is callable by anyone, so a
    // distinct config error would let arbitrary callers probe which anchors
    // have MCP enabled.
    mcp_set_config(
        &env,
        canister_id,
        principal_1(),
        anchor,
        McpConfig {
            enabled: false,
            url: None,
        },
    )
    .unwrap()
    .unwrap();
    let disabled = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key,
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&disabled, Err(message) if message.contains("not authorized")),
        "redemption with MCP disabled must be rejected with the generic error: {disabled:?}"
    );

    Ok(())
}

/// The grant TTL is clamped to [10 min, 30 days] *before* the registration
/// principal is derived, so the number bound into `P_reg` is the effective
/// grant lifetime: a below-minimum request redeems (both sides resolve it the
/// same way) and the minted grant runs exactly the clamp minimum — never
/// longer than what the derivation authenticated.
#[test]
fn mcp_registration_ttl_is_clamped_before_derivation() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    // The user "consents" (via a direct canister call — the frontend clamps
    // client-side) to a 1-second grant, below the 10-minute minimum.
    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key,
        Some(true),
        Some(1),
    )
    .unwrap()
    .unwrap();
    let p_reg = Principal::self_authenticating(&prepared.user_key);

    // Echoing the same below-minimum value redeems, and the grant is exactly
    // the clamp minimum — the value the seed actually bound.
    let server_key = ByteBuf::from("mcp server session key S");
    let registration = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key.clone(),
        Some(true),
        Some(1),
    )
    .unwrap()
    .unwrap();
    assert_eq!(registration.expiration, time(&env) + GRANT_MIN_TTL_NS);

    // Because both sides resolve before deriving, the *clamped* value is the
    // same consent tuple: echoing it redeems identically.
    let clamped_echo = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        server_key,
        Some(true),
        Some(GRANT_MIN_TTL_NS),
    )
    .unwrap();
    assert!(
        clamped_echo.is_ok(),
        "the clamped TTL is the resolved consent value and must redeem: {clamped_echo:?}"
    );

    Ok(())
}

/// With nothing stored there is no single-use marker: within its ~5-minute
/// lifetime the delegation redeems repeatedly. A retry with the same session
/// key re-binds it (idempotent for boundary retries), and a redemption with a
/// different key replaces the anchor's single session — the previous session
/// key stops being authorized, so multi-use never yields concurrent sessions.
#[test]
fn mcp_register_v2_is_multi_use_but_sessions_replace() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key,
        Some(false), // full access
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    let p_reg = Principal::self_authenticating(&prepared.user_key);

    // First redemption succeeds (full access).
    let first_key = ByteBuf::from("mcp server session key S1");
    let first = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        first_key.clone(),
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    assert_eq!(first.permissions, Permissions::All);

    // A boundary retry with the SAME key re-binds it — idempotent in effect.
    let retry = mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        first_key.clone(),
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    assert_eq!(retry.permissions, first.permissions);

    // A second redemption with a DIFFERENT key succeeds — and replaces the
    // anchor's single session rather than adding one.
    let second_key = ByteBuf::from("mcp server session key S2");
    mcp_register_v2(
        &env,
        canister_id,
        p_reg,
        anchor,
        second_key.clone(),
        Some(false),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();
    let replaced = mcp_get_accounts(
        &env,
        canister_id,
        Principal::self_authenticating(&first_key),
        MCP_ORIGIN.to_string(),
    )
    .unwrap();
    assert!(
        matches!(&replaced, Err(AccountDelegationError::Unauthorized(_))),
        "the replaced session key must lose authorization: {replaced:?}"
    );
    let current = mcp_get_accounts(
        &env,
        canister_id,
        Principal::self_authenticating(&second_key),
        MCP_ORIGIN.to_string(),
    )
    .unwrap();
    assert!(
        current.is_ok(),
        "the latest registered session should be authorized: {current:?}"
    );

    Ok(())
}

/// The registration delegation expires after ~5 minutes; past that, the seed's
/// signature is gone from the certified map and redemption as `P_reg` is no
/// longer possible. (PocketIC validates ingress delegation expiry the same way
/// the IC does, so an expired chain never reaches the canister; here we assert
/// the certified signature itself disappears — `get` stops returning it.)
#[test]
fn mcp_registration_delegation_expires() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    let registration_key = ByteBuf::from("browser registration key Y");
    let prepared = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key.clone(),
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();

    // Registration delegations live 5 minutes; expired signatures are pruned
    // from the certified map as new signatures are added, so mint another
    // delegation after expiry to trigger the pruning.
    env.advance_time(Duration::from_secs(6 * 60));
    prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ByteBuf::from("a later registration key"),
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap()
    .unwrap();

    let expired = get_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        registration_key,
        Some(true),
        Some(GRANT_TTL_NS),
        prepared.expiration,
    )
    .unwrap();
    assert!(
        matches!(&expired, Err(message) if message.contains("no such delegation")),
        "an expired registration delegation must be gone: {expired:?}"
    );

    Ok(())
}

/// `prepare` requires a live trusted-server config: the trusted URL is part of
/// the derived seed, so no registration delegation can exist without one.
#[test]
fn mcp_prepare_registration_delegation_requires_trusted_server() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    // No trust_mcp_server: the anchor's config is the disabled default.

    let result = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ByteBuf::from("browser registration key Y"),
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&result, Err(message) if message.contains("no trusted MCP server")),
        "prepare without a trusted server must be rejected: {result:?}"
    );

    Ok(())
}

/// An empty registration key is rejected at `prepare`: hashing an empty `Y`
/// would derive a fixed, predictable `P_reg` per anchor, defeating the
/// per-connect uniqueness of the registration principal (mirrors
/// `mcp_register`'s empty-session-key check).
#[test]
fn mcp_prepare_registration_delegation_rejects_empty_key() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    trust_mcp_server(&env, canister_id, principal_1(), anchor);

    let result = prepare_mcp_registration_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ByteBuf::new(), // empty Y
        Some(true),
        Some(GRANT_TTL_NS),
    )
    .unwrap();
    assert!(
        matches!(&result, Err(message) if message.contains("empty registration key")),
        "an empty registration key must be rejected: {result:?}"
    );

    Ok(())
}
