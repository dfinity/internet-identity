//! Tests related to prepare_id_alias and get_id_alias canister calls.
use canister_sig_util::{extract_raw_root_pk_from_der, CanisterSigPublicKey};
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use identity_jose::jwk::JwkType;
use identity_jose::jws::Decoder;
use identity_jose::jwu::encode_b64;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use std::ops::Deref;
use vc_util::verify_credential_jws_with_canister_id;

fn verify_canister_sig_pk(credential_jws: &str, canister_sig_pk_der: &[u8]) {
    let decoder: Decoder = Decoder::new();
    let jws = decoder
        .decode_compact_serialization(credential_jws.as_bytes(), None)
        .expect("Failure decoding JWS credential");
    let jws_header = jws.protected_header().expect("missing JWS header");
    let jwk = jws_header.deref().jwk().expect("missing JWK in JWS header");
    assert_eq!(jwk.alg(), Some("IcCs"));
    assert_eq!(jwk.kty(), JwkType::Oct);
    let jwk_params = jwk.try_oct_params().expect("missing JWK oct params");
    assert_eq!(jwk_params.k, encode_b64(canister_sig_pk_der));
}

// Verifies that a valid id_alias is created.
#[test]
fn should_get_valid_id_alias() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };

    let prepare_response =
        api::vc_mvp::prepare_id_alias(&env, canister_id, principal_1(), prepare_id_alias_req)?
            .expect("Got 'None' from prepare_id_alias");

    let prepared_id_alias = if let PrepareIdAliasResponse::Ok(prepared) = prepare_response {
        prepared
    } else {
        panic!("prepare id_alias failed")
    };

    let get_id_alias_req = GetIdAliasRequest {
        identity_number,
        relying_party,
        issuer,
        rp_id_alias_jwt: prepared_id_alias.rp_id_alias_jwt,
        issuer_id_alias_jwt: prepared_id_alias.issuer_id_alias_jwt,
    };
    let id_alias_credentials =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req)?
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

    // Verify that JWS-credentials contain correct canister signing PK.
    verify_canister_sig_pk(
        &id_alias_credentials.rp_id_alias_credential.credential_jws,
        prepared_id_alias.canister_sig_pk_der.as_ref(),
    );
    verify_canister_sig_pk(
        &id_alias_credentials
            .issuer_id_alias_credential
            .credential_jws,
        prepared_id_alias.canister_sig_pk_der.as_ref(),
    );

    // Verify the credentials in two ways: via env and via external function.
    let canister_sig_pk =
        CanisterSigPublicKey::try_from(prepared_id_alias.canister_sig_pk_der.as_ref())
            .expect("failed parsing canister sig pk");
    let root_pk_raw =
        extract_raw_root_pk_from_der(&env.root_key()).expect("Failed decoding IC root key.");
    verify_id_alias_credential_via_env(
        &env,
        prepared_id_alias.canister_sig_pk_der.clone(),
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
    verify_credential_jws_with_canister_id(
        &id_alias_credentials.rp_id_alias_credential.credential_jws,
        &canister_sig_pk.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_id_alias_credential_via_env(
        &env,
        prepared_id_alias.canister_sig_pk_der.clone(),
        &id_alias_credentials.issuer_id_alias_credential,
        &env.root_key(),
    );
    verify_credential_jws_with_canister_id(
        &id_alias_credentials
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_users() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number_1 = flows::register_anchor(&env, canister_id);
    let identity_number_2 = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req_1 = PrepareIdAliasRequest {
        identity_number: identity_number_1,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };
    let prepare_id_alias_req_2 = PrepareIdAliasRequest {
        identity_number: identity_number_2,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };

    let (get_id_alias_req_1, canister_sig_pk_1) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_1,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_1) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number: identity_number_1,
                    relying_party: relying_party.clone(),
                    issuer: issuer.clone(),
                    rp_id_alias_jwt: prepared_id_alias_1.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_1.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_1.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let (get_id_alias_req_2, canister_sig_pk_2) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_2,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_2) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number: identity_number_2,
                    relying_party,
                    issuer,
                    rp_id_alias_jwt: prepared_id_alias_2.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_2.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_2.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let id_alias_credentials_1 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_1)?
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

    let id_alias_credentials_2 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_2)?
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

    let root_pk_raw =
        extract_raw_root_pk_from_der(&env.root_key()).expect("Failed decoding IC root key.");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_relying_parties() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party_1 = FrontendHostname::from("https://some-dapp-1.com");
    let relying_party_2 = FrontendHostname::from("https://some-dapp-2.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req_1 = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party_1.clone(),
        issuer: issuer.clone(),
    };
    let prepare_id_alias_req_2 = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party_2.clone(),
        issuer: issuer.clone(),
    };

    let (get_id_alias_req_1, canister_sig_pk_1) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_1,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_1) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number,
                    relying_party: relying_party_1,
                    issuer: issuer.clone(),
                    rp_id_alias_jwt: prepared_id_alias_1.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_1.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_1.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let (get_id_alias_req_2, canister_sig_pk_2) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_2,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_2) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number,
                    relying_party: relying_party_2,
                    issuer,
                    rp_id_alias_jwt: prepared_id_alias_2.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_2.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_2.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let id_alias_credentials_1 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_1)?
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

    let id_alias_credentials_2 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_2)?
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

    let root_pk_raw =
        extract_raw_root_pk_from_der(&env.root_key()).expect("Failed decoding IC root key.");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");

    Ok(())
}

#[test]
fn should_get_different_id_alias_for_different_issuers() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer_1 = FrontendHostname::from("https://some-issuer-1.com");
    let issuer_2 = FrontendHostname::from("https://some-issuer-2.com");
    let prepare_id_alias_req_1 = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer_1.clone(),
    };
    let prepare_id_alias_req_2 = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer_2.clone(),
    };

    let (get_id_alias_req_1, canister_sig_pk_1) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_1,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_1) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number,
                    relying_party: relying_party.clone(),
                    issuer: issuer_1,
                    rp_id_alias_jwt: prepared_id_alias_1.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_1.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_1.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let (get_id_alias_req_2, canister_sig_pk_2) = {
        let prepare_response = api::vc_mvp::prepare_id_alias(
            &env,
            canister_id,
            principal_1(),
            prepare_id_alias_req_2,
        )?
        .expect("Got 'None' from prepare_id_alias");
        if let PrepareIdAliasResponse::Ok(prepared_id_alias_2) = prepare_response {
            (
                GetIdAliasRequest {
                    identity_number,
                    relying_party,
                    issuer: issuer_2,
                    rp_id_alias_jwt: prepared_id_alias_2.rp_id_alias_jwt,
                    issuer_id_alias_jwt: prepared_id_alias_2.issuer_id_alias_jwt,
                },
                CanisterSigPublicKey::try_from(prepared_id_alias_2.canister_sig_pk_der.as_ref())
                    .expect("failed parsing canister sig pk"),
            )
        } else {
            panic!("prepare id_alias failed")
        }
    };

    let id_alias_credentials_1 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_1)?
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

    let id_alias_credentials_2 =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req_2)?
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

    let root_pk_raw =
        extract_raw_root_pk_from_der(&env.root_key()).expect("Failed decoding IC root key.");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_1
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_1.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2.rp_id_alias_credential.credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");
    verify_credential_jws_with_canister_id(
        &id_alias_credentials_2
            .issuer_id_alias_credential
            .credential_jws,
        &canister_sig_pk_2.canister_id,
        &root_pk_raw,
    )
    .expect("external verification failed");

    Ok(())
}

#[test]
#[should_panic(expected = "could not be authenticated")]
fn should_not_prepare_id_alias_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");

    let _ = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_2(),
        PrepareIdAliasRequest {
            identity_number, // belongs to principal_1
            relying_party,
            issuer,
        },
    )
    .expect("Got 'None' from prepare_id_alias");
}

#[test]
fn should_not_get_id_alias_for_different_user() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        PrepareIdAliasRequest {
            identity_number,
            relying_party: relying_party.clone(),
            issuer: issuer.clone(),
        },
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
        GetIdAliasRequest {
            identity_number, // belongs to principal_1
            relying_party,
            issuer,
            rp_id_alias_jwt: "dummy_jwt".to_string(),
            issuer_id_alias_jwt: "another_dummy_jwt".to_string(),
        },
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
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");

    let response = api::vc_mvp::get_id_alias(
        &env,
        canister_id,
        principal_1(),
        GetIdAliasRequest {
            identity_number,
            relying_party,
            issuer,
            rp_id_alias_jwt: "dummy jwt".to_string(),
            issuer_id_alias_jwt: "another dummy jwt".to_string(),
        },
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
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        prepare_id_alias_req.clone(),
    )?
    .expect("Got 'None' from prepare_id_alias");

    let prepared_id_alias = if let PrepareIdAliasResponse::Ok(prepared_id_alias) = prepare_response
    {
        prepared_id_alias
    } else {
        panic!("prepare id_alias failed")
    };

    // upgrade, even with the same WASM clears non-stable memory
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let get_id_alias_req = GetIdAliasRequest {
        identity_number,
        relying_party,
        issuer,
        rp_id_alias_jwt: prepared_id_alias.rp_id_alias_jwt,
        issuer_id_alias_jwt: prepared_id_alias.issuer_id_alias_jwt,
    };
    let response = api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req)?
        .expect("Got 'None' from get_id_alias");
    assert!(matches!(
        response,
        GetIdAliasResponse::NoSuchCredentials(_err)
    ));
    Ok(())
}

#[test]
#[should_panic(expected = "id_alias signature invalid")]
fn should_not_validate_id_alias_with_wrong_canister_key() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };

    let prepare_response = api::vc_mvp::prepare_id_alias(
        &env,
        canister_id,
        principal_1(),
        prepare_id_alias_req.clone(),
    )
    .expect("Result of prepare_id_alias is not Ok")
    .expect("Got 'None' from prepare_id_alias");

    let prepared_id_alias = if let PrepareIdAliasResponse::Ok(prepared_id_alias) = prepare_response
    {
        prepared_id_alias
    } else {
        panic!("prepare id_alias failed")
    };

    let get_id_alias_req = GetIdAliasRequest {
        identity_number,
        relying_party,
        issuer,
        rp_id_alias_jwt: prepared_id_alias.rp_id_alias_jwt,
        issuer_id_alias_jwt: prepared_id_alias.issuer_id_alias_jwt,
    };

    let id_alias_credentials =
        match api::vc_mvp::get_id_alias(&env, canister_id, principal_1(), get_id_alias_req)
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

    verify_id_alias_credential_via_env(
        &env,
        prepared_id_alias.canister_sig_pk_der.clone(),
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
    verify_id_alias_credential_via_env(
        &env,
        prepared_id_alias.canister_sig_pk_der.clone(),
        &id_alias_credentials.issuer_id_alias_credential,
        &env.root_key(),
    );

    let mut bad_canister_sig_key = prepared_id_alias.canister_sig_pk_der.clone();
    let index = prepared_id_alias.canister_sig_pk_der.as_ref().len() - 1;
    let last_byte = bad_canister_sig_key.as_ref()[index];
    bad_canister_sig_key.as_mut()[index] = last_byte + 1;
    assert_ne!(prepared_id_alias.canister_sig_pk_der, bad_canister_sig_key);

    verify_id_alias_credential_via_env(
        &env,
        bad_canister_sig_key,
        &id_alias_credentials.rp_id_alias_credential,
        &env.root_key(),
    );
}
