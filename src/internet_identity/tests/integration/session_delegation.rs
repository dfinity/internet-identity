use canister_tests::api::internet_identity::api_v2::{
    create_account, get_accounts, get_default_account, get_session_delegation,
    prepare_session_delegation, set_default_account,
};
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::{
    GetAccountsError, GetDefaultAccountError, IdentityInfoError, SessionDelegationError,
    SetDefaultAccountError,
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
    let prepared =
        prepare_session_delegation(env, canister_id, principal_1(), anchor, session_key(), None)
            .expect("prepare_session_delegation call rejected")
            .expect("prepare_session_delegation returned Err");

    let session_principal = candid::Principal::self_authenticating(&prepared.user_key);

    get_session_delegation(
        env,
        canister_id,
        principal_1(),
        anchor,
        session_key(),
        prepared.expiration,
    )
    .expect("get_session_delegation call rejected")
    .expect("get_session_delegation returned Err");

    (prepared, session_principal)
}

fn fresh_anchor() -> (
    pocket_ic::PocketIc,
    ic_cdk::api::management_canister::main::CanisterId,
    u64,
) {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor = flows::register_anchor(&env, canister_id);
    (env, canister_id, anchor)
}

fn fresh_session() -> (
    pocket_ic::PocketIc,
    ic_cdk::api::management_canister::main::CanisterId,
    u64,
    candid::Principal,
) {
    let (env, canister_id, anchor) = fresh_anchor();
    let (_, session_principal) = mint_session_delegation(&env, canister_id, anchor);
    (env, canister_id, anchor, session_principal)
}

#[test]
fn session_delegation_happy_path() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor, session_principal) = fresh_session();

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

    Ok(())
}

#[test]
fn session_delegation_default_deny() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor, session_principal) = fresh_session();

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

    let set_err = set_default_account(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
        None,
    )
    .unwrap()
    .unwrap_err();
    assert!(
        matches!(set_err, SetDefaultAccountError::Unauthorized(_)),
        "set_default_account must be rejected for session callers, got: {set_err:?}"
    );

    Ok(())
}

#[test]
fn ttl_clamp_over_max() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor) = fresh_anchor();

    let ninety_days_ns = 90 * 24 * 3600 * 1_000_000_000u64;

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
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
fn ttl_default_is_thirty_days() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor) = fresh_anchor();

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key(),
        None,
    )
    .unwrap()
    .unwrap();

    let thirty_days_ns = 30u64 * 24 * 3600 * 1_000_000_000;
    let now = time(&env);
    let tolerance_ns = Duration::from_secs(5).as_nanos() as u64;
    assert!(
        prepared.expiration >= now + thirty_days_ns - tolerance_ns
            && prepared.expiration <= now + thirty_days_ns + tolerance_ns,
        "default expiration should be ~30 days from now; got expiration={exp}, now={now}",
        exp = prepared.expiration
    );

    Ok(())
}

#[test]
fn get_session_delegation_returns_no_such_delegation_after_expiry() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor) = fresh_anchor();

    let prepared = prepare_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key(),
        None,
    )
    .unwrap()
    .unwrap();

    // Sanity: signature exists before expiry.
    get_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key(),
        prepared.expiration,
    )
    .unwrap()
    .expect("delegation must be retrievable before expiry");

    // Advance past the default 30-day TTL. The signature map prunes
    // expired entries on read, so the next lookup must miss.
    env.advance_time(Duration::from_secs(31 * 24 * 3600));

    let err = get_session_delegation(
        &env,
        canister_id,
        principal_1(),
        anchor,
        session_key(),
        prepared.expiration,
    )
    .unwrap()
    .unwrap_err();
    assert_eq!(err, SessionDelegationError::NoSuchDelegation);

    Ok(())
}

#[test]
fn session_survives_upgrade() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor, session_principal) = fresh_session();

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

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    get_accounts(
        &env,
        canister_id,
        session_principal,
        anchor,
        ORIGIN.to_string(),
    )
    .unwrap()
    .expect("session call must succeed after upgrade: derived principal is stable");

    Ok(())
}

#[test]
fn session_isolation_across_anchors() -> Result<(), RejectResponse> {
    let (env, canister_id, anchor_a) = fresh_anchor();
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
