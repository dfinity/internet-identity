//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::GetDelegationResponse;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::ops::Add;
use std::time::{Duration, UNIX_EPOCH};

/// Verifies that valid delegations are issued.
#[test]
fn should_get_valid_delegation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (canister_sig_key, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;
    assert_eq!(
        expiration,
        env.time()
            .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );

    let signed_delegation = match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation(&env, canister_sig_key, &signed_delegation, &env.root_key());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);
    Ok(())
}

/// Verifies that non-default expirations are respected.
#[test]
fn should_get_valid_delegation_with_custom_expiration() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (canister_sig_key, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        Some(3_600_000_000_000), // 1 hour
    )?;
    assert_eq!(
        expiration,
        env.time()
            .add(Duration::from_secs(60 * 60)) // 1 hour
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );

    let signed_delegation = match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation(&env, canister_sig_key, &signed_delegation, &env.root_key());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);
    Ok(())
}

/// Verifies that the delegations are valid at most for 30 days.
#[test]
fn should_shorten_expiration_greater_max_ttl() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (canister_sig_key, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        Some(Duration::from_secs(31 * 24 * 60 * 60).as_nanos() as u64), // 31 days
    )?;
    assert_eq!(
        expiration,
        env.time()
            .add(Duration::from_secs(30 * 24 * 60 * 60)) // 30 days
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );

    let signed_delegation = match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation(&env, canister_sig_key, &signed_delegation, &env.root_key());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);
    Ok(())
}

/// Verifies that delegations can be requested in parallel.
#[test]
fn should_get_multiple_valid_delegations() -> Result<(), CallError> {
    let env = env();
    let root_key = env.root_key();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname_1 = "https://dapp1.com";
    let frontend_hostname_2 = "https://dapp2.com";
    let pub_session_key_1 = ByteBuf::from("session public key 1");
    let pub_session_key_2 = ByteBuf::from("session public key 2");
    let delegation_params = vec![
        (
            &pub_session_key_1,
            frontend_hostname_1,
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_1,
            frontend_hostname_2,
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_2,
            frontend_hostname_1,
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_1,
            frontend_hostname_1,
            Duration::from_secs(30),
        ),
    ];

    // prepare multiple delegations in parallel before calling get_delegation
    let prepare_delegation_results =
        delegation_params
            .into_iter()
            .map(|(session_key, frontend_hostname, time_shift)| {
                env.advance_time(time_shift);
                let (canister_sig_key, expiration) = api::prepare_delegation(
                    &env,
                    canister_id,
                    principal_1(),
                    user_number,
                    frontend_hostname,
                    session_key,
                    None,
                )
                .expect("prepare_delegation failed");

                assert_eq!(
                    expiration,
                    env.time()
                        .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_nanos() as u64
                );
                (session_key, frontend_hostname, canister_sig_key, expiration)
            });

    for (session_key, frontend_hostname, canister_sig_key, expiration) in prepare_delegation_results
    {
        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname,
            session_key,
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        verify_delegation(&env, canister_sig_key, &signed_delegation, &root_key);
        assert_eq!(signed_delegation.delegation.pubkey, session_key.clone());
        assert_eq!(signed_delegation.delegation.expiration, expiration);
    }
    Ok(())
}

/// Verifies that an anchor that was registered using II_WASM_PREVIOUS gets valid delegations after upgrading to the current version.
#[test]
fn should_get_valid_delegation_for_old_anchor_after_ii_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let (canister_sig_key, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;
    assert_eq!(
        expiration,
        env.time()
            .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );

    let signed_delegation = match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation(&env, canister_sig_key, &signed_delegation, &env.root_key());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);
    Ok(())
}

/// Verifies that different front-ends yield different principals.
#[test]
fn should_issue_different_principals() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let pub_session_key = ByteBuf::from("session public key");
    let frontend_hostname_1 = "https://dapp1.com";
    let frontend_hostname_2 = "https://dapp2.com";

    let (canister_sig_key_1, _) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_1,
        &pub_session_key,
        None,
    )?;
    let (canister_sig_key_2, _) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_2,
        &pub_session_key,
        None,
    )?;

    assert_ne!(canister_sig_key_1, canister_sig_key_2);
    Ok(())
}

/// Verifies that there is a graceful failure if II gets upgraded between prepare_delegation and get_delegation.
#[test]
fn should_not_get_prepared_delegation_after_ii_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (_, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;

    // upgrade, even with the same WASM clears non-stable memory
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
        GetDelegationResponse::NoSuchDelegation => {}
    };
    Ok(())
}

/// Verifies that there is a graceful failure if get_delegation is called after the expiration of the delegation.
#[test]
fn should_not_get_delegation_after_expiration() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (_, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;

    env.advance_time(Duration::from_secs(30 * 60 + 1)); // one second more than delegation validity of 30 min

    // we have to call prepare again, because expired signatures can only be pruned in update calls
    api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;

    match api::get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
        GetDelegationResponse::NoSuchDelegation => {}
    };
    Ok(())
}

/// Verifies that delegations can only be prepared by the matching user.
#[test]
fn can_not_prepare_delegation_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let result = api::prepare_delegation(
        &env,
        canister_id,
        principal_2(),
        user_number, // belongs to principal_1
        "https://some-dapp.com",
        &ByteBuf::from("session key"),
        None,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

/// Verifies that get_delegation can only be called by the matching user.
#[test]
fn can_not_get_delegation_for_different_user() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (_, expiration) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;
    let result = api::get_delegation(
        &env,
        canister_id,
        principal_2(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        expiration,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
    Ok(())
}

/// Verifies that get_principal and prepare_delegation return the same principal.
#[test]
fn get_principal_should_match_prepare_delegation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com";
    let pub_session_key = ByteBuf::from("session public key");

    let (canister_sig_key, _) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &pub_session_key,
        None,
    )?;

    let principal = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
    )?;
    assert_eq!(Principal::self_authenticating(canister_sig_key), principal);
    Ok(())
}

/// Verifies that get_principal returns different principals for different front end host names.
#[test]
fn should_return_different_principals_for_different_frontends() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::init_salt(&env, canister_id)?;
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname_1 = "https://dapp1.com";
    let frontend_hostname_2 = "https://dapp2.com";

    let dapp_principal_1 = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_1,
    )?;

    let dapp_principal_2 = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_2,
    )?;

    assert_ne!(dapp_principal_1, dapp_principal_2);
    Ok(())
}

/// Verifies that get_principal returns different principals for different anchors.
#[test]
fn should_return_different_principals_for_different_users() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::init_salt(&env, canister_id)?;
    let user_number_1 =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let frontend_hostname = "https://dapp-1.com";

    let dapp_principal_1 = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number_1,
        frontend_hostname,
    )?;

    let dapp_principal_2 = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number_2,
        frontend_hostname,
    )?;

    assert_ne!(dapp_principal_1, dapp_principal_2);
    Ok(())
}

/// Verifies that get_principal requires authentication.
#[test]
fn should_not_allow_get_principal_for_other_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());
    let frontend_hostname_1 = "https://dapp-1.com";

    let result = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number_2,
        frontend_hostname_1,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated\\.").unwrap(),
    );
}
