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
        create_account, mcp_get_accounts, mcp_get_config, mcp_get_delegation,
        mcp_prepare_delegation, mcp_register, mcp_set_config, prepare_account_delegation,
        set_default_account, AccountDelegationParams,
    },
    flows,
    framework::{
        device_data_2, env, install_ii_canister_with_arg, principal_1, principal_2, time,
        upgrade_ii_canister, verify_delegation, II_WASM,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountDelegationError, AnchorNumber, McpConfig, McpPrepareDelegation,
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
        GRANT_TTL_NS
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
        GRANT_TTL_NS
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
        GRANT_TTL_NS
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
        GRANT_TTL_NS
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
