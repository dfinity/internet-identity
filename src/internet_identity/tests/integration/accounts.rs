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
