use std::time::Duration;

use canister_tests::{
    api::internet_identity::{
        api_v2::{
            create_account, get_account_delegation, get_accounts, prepare_account_delegation,
            update_account,
        },
        get_delegation, prepare_delegation,
    },
    flows,
    framework::{
        device_data_2, env, install_ii_canister, principal_1, principal_2, time, verify_delegation,
        II_WASM,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountUpdate, GetDelegationResponse, PrepareAccountDelegation,
};
use pocket_ic::CallError;
use serde_bytes::ByteBuf;

/// Verifies that one account can be created
#[test]
fn should_create_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();
    let name = "Callisto".to_string();

    let created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        created_account,
        AccountInfo {
            account_number: Some(1),
            last_used: None,
            origin,
            name: Some(name)
        }
    );
    Ok(())
}

/// Verifies that multiple accounts can be created and read
#[test]
fn should_list_accounts() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();
    let name = "Ganymede".to_string();
    let name_two = "Laniakea".to_string();
    let name_three = "Shevek".to_string();

    let first_created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )
    .unwrap()
    .unwrap();

    let second_created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name_two.clone(),
    )
    .unwrap()
    .unwrap();

    let third_created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name_three.clone(),
    )
    .unwrap()
    .unwrap();

    let accounts_list = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        accounts_list,
        vec![
            AccountInfo {
                // default account
                account_number: None,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            AccountInfo {
                account_number: first_created_account.account_number,
                origin: origin.clone(),
                last_used: None,
                name: Some(name)
            },
            AccountInfo {
                account_number: second_created_account.account_number,
                origin: origin.clone(),
                last_used: None,
                name: Some(name_two)
            },
            AccountInfo {
                account_number: third_created_account.account_number,
                origin,
                last_used: None,
                name: Some(name_three)
            },
        ]
    );
    Ok(())
}

/// Verifies that a default account is returned even if no account is created
#[test]
fn should_list_default_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();

    let accounts_list = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        accounts_list,
        vec![AccountInfo {
            // default account
            account_number: None,
            origin: origin.clone(),
            last_used: None,
            name: None
        },]
    );
    Ok(())
}

/// Verifies that only owned accounts can be read
#[test]
fn should_list_only_own_accounts() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let another_identity_number =
        flows::register_anchor_with_device(&env, canister_id, &device_data_2());
    let origin = "https://some-dapp.com".to_string();
    let name = "Ganymede".to_string();
    let name_two = "Laniakea".to_string();
    let name_three = "Shevek".to_string();

    let first_created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )
    .unwrap()
    .unwrap();

    let second_created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name_two.clone(),
    )
    .unwrap()
    .unwrap();

    let another_identity_account = create_account(
        &env,
        canister_id,
        principal_2(),
        another_identity_number,
        origin.clone(),
        name_three.clone(),
    )
    .unwrap()
    .unwrap();

    let accounts_list = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    let another_accounts_list = get_accounts(
        &env,
        canister_id,
        principal_2(),
        another_identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        accounts_list,
        vec![
            AccountInfo {
                // default account
                account_number: None,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            AccountInfo {
                account_number: first_created_account.account_number,
                origin: origin.clone(),
                last_used: None,
                name: Some(name)
            },
            AccountInfo {
                account_number: second_created_account.account_number,
                origin: origin.clone(),
                last_used: None,
                name: Some(name_two)
            },
        ]
    );

    assert_eq!(
        another_accounts_list,
        vec![
            AccountInfo {
                // default account
                account_number: None,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            AccountInfo {
                account_number: another_identity_account.account_number,
                origin,
                last_used: None,
                name: Some(name_three)
            },
        ]
    );
    Ok(())
}

/// Verifies that a created account can be updated
#[test]
fn should_update_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();
    let name = "Callisto".to_string();

    let created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )
    .unwrap()
    .unwrap();

    let new_name = Some("Laniakea".to_string());

    let update = AccountUpdate {
        name: new_name.clone(),
    };

    let updated_account = update_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        created_account.account_number,
        update,
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        updated_account,
        AccountInfo {
            account_number: created_account.account_number,
            last_used: None,
            origin,
            name: new_name
        }
    );
    Ok(())
}

/// Verifies that a default account can be updated
#[test]
fn should_update_default_account() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://some-dapp.com".to_string();
    let name = "Callisto".to_string();

    let accounts_list = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        accounts_list,
        vec![AccountInfo {
            // default account
            account_number: None,
            origin: origin.clone(),
            last_used: None,
            name: None
        },]
    );

    let update = AccountUpdate {
        name: Some(name.clone()),
    };

    let updated_account = update_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        None,
        update,
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        updated_account,
        AccountInfo {
            account_number: Some(1),
            last_used: None,
            origin: origin.clone(),
            name: Some(name.clone())
        }
    );

    let updated_accounts_list = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        updated_accounts_list,
        vec![AccountInfo {
            // default account
            account_number: Some(1),
            origin,
            last_used: None,
            name: Some(name)
        },]
    );

    Ok(())
}

/// Verifies that only owned accounts can be updated
#[test]
#[should_panic]
fn should_only_update_owned_account() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let another_identity_number =
        flows::register_anchor_with_device(&env, canister_id, &device_data_2());
    let origin = "https://some-dapp.com".to_string();
    let name = "Ganymede".to_string();
    let name_two = "Io".to_string();

    let created_account = create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name.clone(),
    )
    .unwrap()
    .unwrap();

    let _ = create_account(
        &env,
        canister_id,
        principal_2(),
        another_identity_number,
        origin.clone(),
        name_two.clone(),
    )
    .unwrap()
    .unwrap();

    let new_name = Some("Europa".to_string());

    let update = AccountUpdate {
        name: new_name.clone(),
    };

    // Here we try to update the account created by the first identity using the second identity.
    // This should fail.
    let _ = update_account(
        &env,
        canister_id,
        principal_2(),
        another_identity_number,
        origin.clone(),
        created_account.account_number,
        update,
    )
    .unwrap()
    .unwrap();
}

/// Verifies that valid account delegations are issued.
#[test]
fn should_get_valid_account_delegation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    let signed_delegation = get_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        None,
        pub_session_key.clone(),
        expiration,
    )
    .unwrap()
    .unwrap();

    verify_delegation(&env, user_key, &signed_delegation, &env.root_key().unwrap());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);

    Ok(())
}

/// Verifies that default account delegation principals are identical to regular delegation principals.
#[test]
fn should_get_matching_principals() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    let signed_account_delegation = get_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        expiration,
    )
    .unwrap()
    .unwrap();

    verify_delegation(
        &env,
        user_key.clone(),
        &signed_account_delegation,
        &env.root_key().unwrap(),
    );
    assert_eq!(signed_account_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_account_delegation.delegation.expiration, expiration);

    let (canister_sig_key, expiration) = prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.as_str(),
        &pub_session_key,
        None,
    )?;
    assert_eq!(
        expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    let signed_delegation = match get_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.as_str(),
        &pub_session_key,
        expiration,
    )? {
        GetDelegationResponse::SignedDelegation(delegation) => delegation,
        GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
    };

    verify_delegation(
        &env,
        canister_sig_key.clone(),
        &signed_delegation,
        &env.root_key().unwrap(),
    );
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);

    assert_eq!(user_key, canister_sig_key);

    Ok(())
}

/// Verifies that valid account delegations are issued with custom expiration.
#[test]
fn should_get_valid_account_delegation_with_custom_expiration() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        Some(3_600_000_000_000), // 1 hour
    )
    .unwrap()
    .unwrap();

    assert_eq!(
        expiration,
        time(&env) + Duration::from_secs(60 * 60).as_nanos() as u64
    );

    let signed_delegation = get_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        None,
        pub_session_key.clone(),
        expiration,
    )
    .unwrap()
    .unwrap();

    verify_delegation(&env, user_key, &signed_delegation, &env.root_key().unwrap());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);

    Ok(())
}

/// Verifies that account delegations are valid at most for 30 days.
#[test]
fn should_shorten_account_delegation_expiration_greater_max_ttl() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        Some(Duration::from_secs(31 * 24 * 60 * 60).as_nanos() as u64), // 31 days
    )
    .unwrap()
    .unwrap();

    let month_seconds = 30 * 24 * 60 * 60; // 30 days
    assert_eq!(
        expiration,
        time(&env) + Duration::from_secs(month_seconds).as_nanos() as u64
    );

    let signed_delegation = get_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        None,
        pub_session_key.clone(),
        expiration,
    )
    .unwrap()
    .unwrap();

    verify_delegation(&env, user_key, &signed_delegation, &env.root_key().unwrap());
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(signed_delegation.delegation.expiration, expiration);

    Ok(())
}

/// Verifies that account delegations can be requested in parallel.
#[test]
fn should_get_multiple_valid_account_delegations() -> Result<(), CallError> {
    let env = env();
    let root_key = env.root_key().unwrap();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname_1 = "https://dapp1.com".to_string();
    let frontend_hostname_2 = "https://dapp2.com".to_string();
    let pub_session_key_1 = ByteBuf::from("session public key 1");
    let pub_session_key_2 = ByteBuf::from("session public key 2");
    let delegation_params = vec![
        (
            &pub_session_key_1,
            frontend_hostname_1.clone(),
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_1,
            frontend_hostname_2.clone(),
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_2,
            frontend_hostname_1.clone(),
            Duration::from_secs(0),
        ),
        (
            &pub_session_key_1,
            frontend_hostname_1.clone(),
            Duration::from_secs(30),
        ),
    ];

    // prepare multiple delegations in parallel before calling get_delegation
    let prepare_delegation_results =
        delegation_params
            .into_iter()
            .map(|(session_key, frontend_hostname, time_shift)| {
                env.advance_time(time_shift);
                let PrepareAccountDelegation {
                    user_key,
                    expiration,
                } = prepare_account_delegation(
                    &env,
                    canister_id,
                    principal_1(),
                    user_number,
                    frontend_hostname.clone(),
                    None,
                    session_key.clone(),
                    None,
                )
                .unwrap()
                .expect("prepare_account_delegation failed");

                assert_eq!(
                    expiration,
                    time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
                );
                (session_key, frontend_hostname, user_key, expiration)
            });

    for (session_key, frontend_hostname, user_key, expiration) in prepare_delegation_results {
        let signed_delegation = get_account_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname,
            None,
            session_key.clone(),
            expiration,
        )
        .unwrap()
        .unwrap();

        verify_delegation(&env, user_key, &signed_delegation, &root_key);
        assert_eq!(signed_delegation.delegation.pubkey, session_key.clone());
        assert_eq!(signed_delegation.delegation.expiration, expiration);
    }
    Ok(())
}

/// Verifies that different front-ends yield different principals for account delegations.
#[test]
fn should_issue_different_principals_for_account_delegations() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let pub_session_key = ByteBuf::from("session public key");
    let frontend_hostname_1 = "https://dapp1.com".to_string();
    let frontend_hostname_2 = "https://dapp2.com".to_string();

    let PrepareAccountDelegation {
        user_key: user_key_1,
        ..
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_1,
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    let PrepareAccountDelegation {
        user_key: user_key_2,
        ..
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname_2,
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    assert_ne!(user_key_1, user_key_2);
    Ok(())
}

/// Verifies that account delegations can only be prepared by the matching user.
#[test]
#[should_panic]
fn can_not_prepare_account_delegation_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let _ = prepare_account_delegation(
        &env,
        canister_id,
        principal_2(),
        user_number, // belongs to principal_1
        "https://some-dapp.com".to_string(),
        None,
        ByteBuf::from("session key"),
        None,
    )
    .unwrap()
    .unwrap();
}

/// Verifies that get_account_delegation can only be called by the matching user.
#[test]
#[should_panic]
fn can_not_get_account_delegation_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation { expiration, .. } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    let _ = get_account_delegation(
        &env,
        canister_id,
        principal_2(),
        user_number,
        frontend_hostname,
        None,
        pub_session_key,
        expiration,
    )
    .unwrap()
    .unwrap();
}

/// Verifies that there is a graceful failure if get_account_delegation is called after the expiration.
#[test]
fn should_not_get_account_delegation_after_expiration() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let frontend_hostname = "https://some-dapp.com".to_string();
    let pub_session_key = ByteBuf::from("session public key");

    let PrepareAccountDelegation { expiration, .. } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    env.advance_time(Duration::from_secs(30 * 60 + 1)); // one second more than delegation validity of 30 min

    // we have to call prepare again, because expired signatures can only be pruned in update calls
    prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        None,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    let result = get_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        None,
        pub_session_key,
        expiration,
    );

    assert!(result.unwrap().is_err());
    Ok(())
}

/// Verifies that different accounts on the same origin yield different principals.
#[test]
fn should_issue_different_principals_for_different_accounts() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let pub_session_key = ByteBuf::from("session public key");
    let frontend_hostname = "https://dapp1.com".to_string();

    // Create two different accounts
    let first_account = create_account(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        "First Account".to_string(),
    )
    .unwrap()
    .unwrap();

    let second_account = create_account(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        "Second Account".to_string(),
    )
    .unwrap()
    .unwrap();

    // Get delegations for both accounts
    let PrepareAccountDelegation {
        user_key: user_key_1,
        ..
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.clone(),
        first_account.account_number,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    let PrepareAccountDelegation {
        user_key: user_key_2,
        ..
    } = prepare_account_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        second_account.account_number,
        pub_session_key.clone(),
        None,
    )
    .unwrap()
    .unwrap();

    assert_ne!(user_key_1, user_key_2);
    Ok(())
}
