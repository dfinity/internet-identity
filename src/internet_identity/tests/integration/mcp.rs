//! Integration tests for the backend `/mcp` delegation path: an MCP server the
//! user chooses to trust, acting as the principal II derives for an anchor's
//! chosen account at that server's origin, fetches per-app account delegations
//! without a per-app browser flow. The anchor is recovered from `caller()` via
//! the opt-in index.

use candid::Principal;
use canister_tests::{
    api::internet_identity::api_v2::{
        mcp_access_enabled, mcp_get_account_delegation, mcp_prepare_account_delegation,
        mcp_set_access, prepare_account_delegation, AccountDelegationParams,
    },
    flows,
    framework::{
        env, install_ii_canister_with_arg, principal_1, principal_2, time, verify_delegation,
        II_WASM,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountDelegationError, AnchorNumber, PrepareAccountDelegation,
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
        None,
        true,
    )
    .unwrap()
    .unwrap();
    assert!(mcp_access_enabled(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        None
    )
    .unwrap());

    // Mint the per-app delegation as the MCP server — no anchor_number passed.
    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = mcp_prepare_account_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
        session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    // Default TTL is the 5-minute cap.
    assert_eq!(expiration, time(&env) + MCP_MAX_TTL_NS);

    let signed = mcp_get_account_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
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
        None,
        true,
    )
    .unwrap()
    .unwrap();

    let prepared = mcp_prepare_account_delegation(
        &env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
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
        None,
        true,
    )
    .unwrap()
    .unwrap();
    assert!(mcp_prepare_account_delegation(
        &env,
        canister_id,
        mcp,
        target.clone(),
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
        None,
        false,
    )
    .unwrap()
    .unwrap();
    assert!(!mcp_access_enabled(
        &env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        None
    )
    .unwrap());
    match mcp_prepare_account_delegation(&env, canister_id, mcp, target, session_key, None).unwrap()
    {
        Err(AccountDelegationError::Unauthorized(_)) => {}
        Ok(_) => panic!("expected Unauthorized after disabling, got Ok"),
        Err(e) => panic!("expected Unauthorized after disabling, got {e:?}"),
    }

    Ok(())
}
