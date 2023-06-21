//! Tests related to prepare_id_alias and get_id_alias canister calls.

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasResponse, PrepareIdAliasResponse,
};

/// Verifies that valid id_alias is created.
#[test]
fn should_get_valid_id_alias() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    let canister_sig_key = if let PrepareIdAliasResponse::Ok(key) = prepare_response {
        key
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    assert_eq!(
        id_alias_credentials.rp_id_alias_credential.id_alias,
        id_alias_credentials.issuer_id_alias_credential.id_alias
    );

    verify_id_alias_credential(
        &env,
        canister_sig_key.clone(),
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
    verify_id_alias_credential(
        &env,
        canister_sig_key,
        &id_alias_credentials.issuer_id_alias_credential,
        &env.root_key(),
    );
    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_users() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number_1 = flows::register_anchor(&env, canister_id);
    let identity_number_2 = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number_1,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number_2,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials_1 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number_1,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    let id_alias_credentials_2 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number_2,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    assert_eq!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_1.issuer_id_alias_credential.id_alias
    );

    assert_eq!(
        id_alias_credentials_2.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.rp_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.issuer_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_relying_parties() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party_1 = "https://some-dapp-1.com";
    let relying_party_2 = "https://some-dapp-2.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party_1,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party_2,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials_1 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party_1,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    let id_alias_credentials_2 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party_2,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    assert_eq!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_1.issuer_id_alias_credential.id_alias
    );

    assert_eq!(
        id_alias_credentials_2.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.rp_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.issuer_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_issuers() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer_1 = "https://some-issuer-1.com";
    let issuer_2 = "https://some-issuer-2.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer_1,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer_2,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::Ok(_key) = prepare_response {
        // Ok
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials_1 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer_1,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    let id_alias_credentials_2 = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer_2,
    )?
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    assert_eq!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_1.issuer_id_alias_credential.id_alias
    );

    assert_eq!(
        id_alias_credentials_2.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.rp_id_alias_credential.id_alias,
        id_alias_credentials_2.rp_id_alias_credential.id_alias
    );

    assert_ne!(
        id_alias_credentials_1.issuer_id_alias_credential.id_alias,
        id_alias_credentials_2.issuer_id_alias_credential.id_alias
    );

    Ok(())
}

#[test]
fn should_not_prepare_id_alias_for_different_user() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_2(),
        identity_number, // belongs to principal_1
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    if let PrepareIdAliasResponse::AuthenticationFailed(_err) = response {
        Ok(())
    } else {
        panic!("Expected a failed authentication, got {:?}", response);
    }
}

#[test]
fn should_not_get_id_alias_for_different_user() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    let _canister_sig_key = if let PrepareIdAliasResponse::Ok(key) = prepare_response {
        key
    } else {
        panic!("prepare id_alias failed")
    };

    let response = api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_2(),
        identity_number, // belongs to principal_1
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias");

    if let GetIdAliasResponse::AuthenticationFailed(_err) = response {
        Ok(())
    } else {
        panic!("Expected a failed authentication, got {:?}", response);
    }
}

#[test]
fn should_not_get_id_alias_if_not_prepared() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::init_salt(&env, canister_id)?;
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let response = api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias");

    if let GetIdAliasResponse::NoSuchCredentials(_err) = response {
        Ok(())
    } else {
        panic!("Expected that credentials not found, got {:?}", response);
    }
}

/// Verifies that there is a graceful failure if II gets upgraded between prepare_id_alias
/// and get_id_alias.
#[test]
fn should_not_get_prepared_id_alias_after_ii_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from prepare_id_alias");

    let _canister_sig_key = if let PrepareIdAliasResponse::Ok(key) = prepare_response {
        key
    } else {
        panic!("prepare id_alias failed")
    };

    // upgrade, even with the same WASM clears non-stable memory
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let response = api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )?
    .expect("Got 'None' from get_id_alias");

    if let GetIdAliasResponse::NoSuchCredentials(_err) = response {
        Ok(())
    } else {
        panic!("Expected that credentials not found, got {:?}", response);
    }
}

#[test]
#[should_panic(expected = "id_alias signature invalid")]
fn should_not_validate_id_alias_with_wrong_canister_key() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = "https://some-dapp.com";
    let issuer = "https://some-issuer.com";

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )
    .expect("Result of prepare_id_alias is not Ok")
    .expect("Got 'None' from prepare_id_alias");

    let canister_sig_key = if let PrepareIdAliasResponse::Ok(key) = prepare_response {
        key
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials = match api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        relying_party,
        issuer,
    )
    .expect("Result of get_id_alias is not Ok")
    .expect("Got 'None' from get_id_alias")
    {
        GetIdAliasResponse::Ok(credentials) => credentials,
        GetIdAliasResponse::NoSuchCredentials(err) => {
            panic!("{}", format!("failed to get id_alias credentials: {}", err))
        }
        GetIdAliasResponse::AuthenticationFailed(err) => {
            panic!("{}", format!("failed authentication: {}", err))
        }
    };

    assert_eq!(
        id_alias_credentials.rp_id_alias_credential.id_alias,
        id_alias_credentials.issuer_id_alias_credential.id_alias
    );

    verify_id_alias_credential(
        &env,
        canister_sig_key.clone(),
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
    verify_id_alias_credential(
        &env,
        canister_sig_key.clone(),
        &id_alias_credentials.issuer_id_alias_credential,
        &env.root_key(),
    );

    let mut bad_canister_sig_key = canister_sig_key.clone();
    let index = canister_sig_key.as_ref().len() - 1;
    let last_byte = bad_canister_sig_key.as_ref()[index];
    bad_canister_sig_key.as_mut()[index] = last_byte + 1;
    assert_ne!(canister_sig_key, bad_canister_sig_key);

    verify_id_alias_credential(
        &env,
        bad_canister_sig_key,
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
}
