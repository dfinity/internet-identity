use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2::{
    create_account, get_accounts, get_default_account, get_session_delegation,
    invalidate_session_delegations, prepare_session_delegation, set_default_account,
};
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, GetAccountsError, GetDefaultAccountError, IdentityInfoError,
    SessionDelegationError, SessionScope,
};
use pocket_ic::RejectResponse;
use serde_bytes::ByteBuf;
use std::time::Duration;

const ORIGIN: &str = "https://some-dapp.com";
const SESSION_KEY: &[u8] = b"session public key";

fn session_key() -> ByteBuf {
    ByteBuf::from(SESSION_KEY)
}

fn mint_session_delegation(
    env: &pocket_ic::PocketIc,
    canister_id: ic_cdk::api::management_canister::main::CanisterId,
    anchor: u64,
) -> (
    internet_identity_interface::internet_identity::types::PrepareSessionDelegation,
    candid::Principal,
) {
    let prepared = prepare_session_delegation(
        env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        None,
    )
    .expect("prepare_session_delegation call rejected")
    .expect("prepare_session_delegation returned Err");

    let session_principal = candid::Principal::self_authenticating(&prepared.user_key);

    get_session_delegation(
        env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        prepared.expiration,
    )
    .expect("get_session_delegation call rejected")
    .expect("get_session_delegation returned Err");

    (prepared, session_principal)
}

#[test]
fn session_delegation_happy_path() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);

    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.to_string(),
        "Alpha".to_string(),
    )
    .unwrap()
    .unwrap();

    let accounts = get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap();
    assert!(!accounts.is_empty(), "session caller should see accounts");

    let default = get_default_account(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap();
    assert_eq!(default.origin, ORIGIN);

    let created_account_number: Option<AccountNumber> = accounts[0].account_number;
    set_default_account(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
        created_account_number,
    )
    .unwrap()
    .unwrap();

    Ok(())
}

#[test]
fn session_delegation_default_deny() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);

    let create_err = create_account(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
        "Alpha".to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(
            create_err,
            internet_identity_interface::internet_identity::types::CreateAccountError::Unauthorized(
                _
            )
        ),
        "create_account must be rejected for session callers, got: {create_err:?}"
    );

    let update_err = canister_tests::api::internet_identity::api_v2::update_account(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
        None,
        internet_identity_interface::internet_identity::types::AccountUpdate {
            name: Some("name".to_string()),
        },
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(
            update_err,
            internet_identity_interface::internet_identity::types::UpdateAccountError::Unauthorized(
                _
            )
        ),
        "update_account must be rejected for session callers, got: {update_err:?}"
    );

    let info_err = canister_tests::api::internet_identity::api_v2::identity_info(
        &env,
        canister_id,
        session_principal,
        anchor,
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(info_err, IdentityInfoError::Unauthorized(_)),
        "identity_info must be rejected for session callers, got: {info_err:?}"
    );

    Ok(())
}

#[test]
fn invalidate_session_delegations_revokes_access() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);

    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.to_string(),
        "Alpha".to_string(),
    )
    .unwrap()
    .unwrap();

    get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .expect("session call should succeed before invalidation");

    invalidate_session_delegations(&env, canister_id, principal_1(), anchor)
        .unwrap()
        .unwrap();

    let err = get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(err, GetAccountsError::Unauthorized(_)),
        "session call must be rejected after explicit invalidation, got: {err:?}"
    );

    Ok(())
}

#[test]
fn device_removal_bumps_epoch_and_revokes_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    api::add(&env, canister_id, principal_1(), anchor, &device_data_2()).unwrap();

    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);

    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.to_string(),
        "Alpha".to_string(),
    )
    .unwrap()
    .unwrap();

    get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .expect("session call should succeed before device removal");

    api::remove(
        &env,
        canister_id,
        principal_1(),
        anchor,
        &ByteBuf::from(PUBKEY_2),
    )
    .unwrap();

    let err = get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(err, GetAccountsError::Unauthorized(_)),
        "session call must be rejected after device removal, got: {err:?}"
    );

    Ok(())
}

#[test]
fn prepare_then_invalidate_then_get_returns_no_such_delegation() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        None,
    )
    .unwrap()
    .unwrap();

    invalidate_session_delegations(&env, canister_id, principal_1(), anchor)
        .unwrap()
        .unwrap();

    let err = get_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        prepared.expiration,
    )
    .unwrap()
    .unwrap_err();

    assert_eq!(
        err,
        SessionDelegationError::NoSuchDelegation,
        "get_session_delegation after invalidation must return NoSuchDelegation"
    );

    Ok(())
}

#[test]
fn ttl_clamp_over_max() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let ninety_days_ns = 90 * 24 * 3600 * 1_000_000_000u64;

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        Some(ninety_days_ns),
    )
    .unwrap()
    .unwrap();

    let thirty_days_ns = 30u64 * 24 * 3600 * 1_000_000_000;
    let now = time(&env);
    assert!(
        prepared.expiration <= now + thirty_days_ns,
        "expiration {exp} exceeds 30-day cap from now {now}",
        exp = prepared.expiration
    );

    Ok(())
}

#[test]
fn ttl_default_is_seven_days() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        SessionScope::AccountManagement,
        session_key(),
        None,
    )
    .unwrap()
    .unwrap();

    let seven_days_ns = 7u64 * 24 * 3600 * 1_000_000_000;
    let now = time(&env);
    let tolerance_ns = Duration::from_secs(5).as_nanos() as u64;
    assert!(
        prepared.expiration >= now + seven_days_ns - tolerance_ns
            && prepared.expiration <= now + seven_days_ns + tolerance_ns,
        "default expiration should be ~7 days from now; got expiration={exp}, now={now}",
        exp = prepared.expiration
    );

    Ok(())
}

#[test]
fn epoch_survives_upgrade_and_revocation_survives_second_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);

    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.to_string(),
        "Alpha".to_string(),
    )
    .unwrap()
    .unwrap();

    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .expect("session call must succeed after upgrade: epoch should have survived");

    invalidate_session_delegations(&env, canister_id, principal_1(), anchor)
        .unwrap()
        .unwrap();

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let err = get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(err, GetAccountsError::Unauthorized(_)),
        "bumped epoch must survive second upgrade; session call must be rejected, got: {err:?}"
    );

    Ok(())
}

#[test]
fn scope_isolation_across_anchors() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let anchor_a = flows::register_anchor(&env, canister_id);

    let anchor_b = flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

    create_account(
        &env,
        canister_id,
        principal_2(),
        anchor_b,
        ORIGIN.to_string(),
        "Beta".to_string(),
    )
    .unwrap()
    .unwrap();

    let (_, session_principal_a) = mint_session_delegation(&env, canister_id, anchor_a);

    let err = get_accounts(
        &env,
        canister_id,
        session_principal_a,
        anchor_b,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(err, GetAccountsError::Unauthorized(_)),
        "session identity of anchor A must not read anchor B's accounts, got: {err:?}"
    );

    let err_default = get_default_account(
        &env,
        canister_id,
        session_principal_a,
        anchor_b,
        ORIGIN.to_string(),
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(err_default, GetDefaultAccountError::Unauthorized(_)),
        "session identity of anchor A must not read anchor B's default account, got: {err_default:?}"
    );

    Ok(())
}
