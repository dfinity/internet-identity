//! Integration tests for the backend `/mcp` delegation path: an MCP server the
//! user authorizes for their identity fetches per-app account delegations
//! without a per-app browser flow. No account is chosen at connect (the
//! connector isn't an app); the server is bound to the principal II derives for
//! the anchor at the server's origin, recovered from `caller()` via the opt-in
//! index, and picks the app account per call against the target origin.

use candid::Principal;
use canister_tests::{
    api::internet_identity::api_v2::{
        create_account, mcp_access_enabled, mcp_get_account_delegation, mcp_get_accounts,
        mcp_get_config, mcp_prepare_account_delegation, mcp_set_access,
        mcp_set_access_with_read_only, mcp_set_config, prepare_account_delegation,
        set_default_account, AccountDelegationParams,
    },
    flows,
    framework::{
        env, install_ii_canister_with_arg, principal_1, principal_2, time, upgrade_ii_canister,
        verify_delegation, II_WASM,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountDelegationError, AnchorNumber, McpConfig, McpPrepareDelegation, PrepareAccountDelegation,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const MCP_ORIGIN: &str = "https://mcp.id.ai";
/// The backend caps MCP-minted delegations at 5 minutes.
const MCP_MAX_TTL_NS: u64 = 5 * 60 * 1_000_000_000;

// The `/mcp` path is no longer gated by a global config; the trusted origin is
// supplied per opt-in. So a plain install suffices.
fn install_with_mcp(env: &PocketIc) -> Principal {
    install_ii_canister_with_arg(env, II_WASM.clone(), None)
}

/// The principal II derives for `anchor`'s default account at `MCP_ORIGIN` — the
/// principal the MCP server's standing delegation carries, and the caller the
/// `mcp_*_account_delegation` methods authorize. Derived exactly as the canister
/// does: the `user_key` of the anchor's default-account delegation for that
/// origin, made self-authenticating.
fn mcp_server_principal(env: &PocketIc, canister_id: Principal, anchor: AnchorNumber) -> Principal {
    let params = AccountDelegationParams::new(
        env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        None,
        ByteBuf::from("mcp standing session key"),
    );
    let PrepareAccountDelegation { user_key, .. } =
        prepare_account_delegation(&params, None).unwrap().unwrap();
    Principal::self_authenticating(user_key)
}

/// The happy path: an opted-in anchor's MCP server mints a per-app delegation by
/// caller alone (no `anchor_number` arg), capped at 5 min, acting as the same
/// principal the anchor's default account at that app gets via the regular API.
#[test]
fn mcp_mints_per_app_delegation_authorized_by_caller() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("mcp per-app session key");

    let mcp = mcp_server_principal(&env, canister_id, anchor);

    // Opt in: bind the MCP-server principal to the anchor.
    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();
    assert!(mcp_access_enabled(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string()
    )
    .unwrap());

    // Mint the per-app delegation as the MCP server — no anchor_number passed.
    let McpPrepareDelegation {
        user_key,
        expiration,
        account_number,
    } = mcp_prepare_account_delegation(
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

    // Default TTL is the 5-minute cap.
    assert_eq!(expiration, time(&env) + MCP_MAX_TTL_NS);

    // `get` is handed back the same account `prepare` resolved.
    let signed = mcp_get_account_delegation(
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

/// A read-only access grant (`mcp_set_access` with `read_only = true`) makes the
/// per-app delegations the server mints query-only: they carry
/// `permissions = "queries"`, and the signed delegation verifies against that
/// permissions-bound message. The standing delegation is untouched — read-only
/// is a property of the grant, applied per minted delegation — so the server can
/// still call the (update) prepare endpoint. (That the signature binds to the
/// `permissions` value, i.e. a differing value yields `NoSuchDelegation`, is
/// covered by `accounts::should_get_read_only_account_delegation_with_queries_permissions`,
/// where the lookup takes an explicit `read_only` argument; the MCP path derives
/// it from the persisted grant, so it has no per-call variant to contrast here.)
#[test]
fn mcp_read_only_grant_mints_queries_only_delegations() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("mcp per-app session key");

    let mcp = mcp_server_principal(&env, canister_id, anchor);

    // Opt in with read-only restriction.
    mcp_set_access_with_read_only(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
        Some(true),
    )
    .unwrap()
    .unwrap();

    let McpPrepareDelegation {
        user_key,
        expiration,
        account_number,
    } = mcp_prepare_account_delegation(
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

    let signed = mcp_get_account_delegation(
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

    // The minted per-app delegation is queries-only, and the signature verifies.
    assert_eq!(
        signed.delegation.permissions,
        Some("queries".to_string()),
        "a read-only MCP grant must mint queries-only per-app delegations"
    );
    verify_delegation(&env, user_key, &signed, &env.root_key().unwrap());

    Ok(())
}

/// Without the read-only restriction (the default), the per-app delegations the
/// server mints are unrestricted (`permissions` absent).
#[test]
fn mcp_full_access_grant_mints_unrestricted_delegations() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("mcp per-app session key");

    let mcp = mcp_server_principal(&env, canister_id, anchor);

    // Opt in without read-only (explicit `Some(false)`).
    mcp_set_access_with_read_only(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
        Some(false),
    )
    .unwrap()
    .unwrap();

    let McpPrepareDelegation {
        expiration,
        account_number,
        ..
    } = mcp_prepare_account_delegation(
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

    let signed = mcp_get_account_delegation(
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

    assert_eq!(signed.delegation.permissions, None);

    Ok(())
}

/// A longer requested TTL is clamped to the 5-minute cap.
#[test]
fn mcp_delegation_ttl_capped_at_5_minutes() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();

    let prepared = mcp_prepare_account_delegation(
        &env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        None,
        ByteBuf::from("k"),
        Some(Duration::from_secs(3600).as_nanos() as u64), // request 1 hour
    )
    .unwrap()
    .unwrap();
    assert_eq!(prepared.expiration, time(&env) + MCP_MAX_TTL_NS);

    Ok(())
}

/// Callers that aren't bound to an anchor — never opted in, or an unrelated
/// principal — are rejected.
#[test]
fn mcp_rejects_unregistered_callers() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    // Not opted in yet.
    match mcp_prepare_account_delegation(
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
        Err(AccountDelegationError::Unauthorized(p)) => assert_eq!(p, mcp),
        Ok(_) => panic!("expected Unauthorized, got Ok"),
        Err(e) => panic!("expected Unauthorized, got {e:?}"),
    }

    // An unrelated principal is rejected too.
    match mcp_prepare_account_delegation(
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

/// Disabling access revokes the MCP server: it can mint while enabled, not after.
#[test]
fn mcp_disabling_access_revokes_the_caller() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();
    assert!(mcp_prepare_account_delegation(
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

    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        false,
    )
    .unwrap()
    .unwrap();
    assert!(!mcp_access_enabled(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string()
    )
    .unwrap());
    match mcp_prepare_account_delegation(&env, canister_id, mcp, target, None, session_key, None)
        .unwrap()
    {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after disabling, got Ok"),
        Err(e) => panic!("expected Unauthorized after disabling, got {e:?}"),
    }

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
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();

    // Prepare without naming an account resolves the default at `target`
    // (None = the synthetic default, since none is reserved yet) and reports it.
    let prepared = mcp_prepare_account_delegation(
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
    assert!(mcp_get_account_delegation(
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
        mcp_get_account_delegation(
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
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    let target = "https://some-app.com".to_string();
    let session_key = ByteBuf::from("k");

    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();

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
    let prepared = mcp_prepare_account_delegation(
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
    assert!(mcp_get_account_delegation(
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
/// (authorized as the bound MCP principal) and then prepares a delegation for
/// one of them — closing the loop with no out-of-band knowledge. An unrelated
/// principal cannot list.
#[test]
fn mcp_lists_accounts_then_prepares_for_one() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_with_mcp(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let mcp = mcp_server_principal(&env, canister_id, anchor);
    let target = "https://some-app.com".to_string();

    mcp_set_access(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
    )
    .unwrap()
    .unwrap();

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

    // The agent lists the anchor's accounts at the app as the bound MCP principal.
    let accounts = mcp_get_accounts(&env, canister_id, mcp, target.clone())
        .unwrap()
        .unwrap();
    assert!(accounts.iter().any(|a| a.account_number == created));

    // It can then prepare a delegation for a listed account.
    let prepared = mcp_prepare_account_delegation(
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

    // A principal that isn't bound to the anchor cannot list.
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
