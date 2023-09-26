//! Tests related to issue_credential canister call.

use candid::Principal;
use canister_sig_util::set_ic_root_public_key_for_testing;
use canister_tests::api::internet_identity::vc_mvp as ii_api;
use canister_tests::flows;
use canister_tests::framework::{env, get_wasm_path, principal_1, II_WASM};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_test_state_machine_client::call_candid_as;
use ic_test_state_machine_client::{query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    CredentialSpec, GetCredentialRequest, GetCredentialResponse, Icrc21ConsentMessageRequest,
    Icrc21ConsentMessageResponse, Icrc21ConsentPreferences, PrepareCredentialRequest,
    PrepareCredentialResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use serial_test::serial;
use std::path::PathBuf;
use vc_util::{verify_id_alias_credential_jws, AliasTuple};

lazy_static! {
    /** The gzipped Wasm module for the current VC_ISSUER build, i.e. the one we're testing */
    pub static ref VC_ISSUER_WASM: Vec<u8> = {
        let def_path = PathBuf::from("./").join("vc_issuer.wasm.gz");
        let err = format!("
        Could not find VC Issuer Wasm module for current build.
        I will look for it at {:?} (note that I run from {:?}).
        ", &def_path,
            &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or_else(|_|
                "an unknown directory".to_string()));
                get_wasm_path("VC_ISSUER_WASM".to_string(), &def_path).expect(&err)

    };
}

pub fn install_canister(env: &StateMachine, wasm: Vec<u8>) -> CanisterId {
    let canister_id = env.create_canister(None);
    let arg = candid::encode_one("()").expect("error encoding II installation arg as candid");
    env.install_canister(canister_id, wasm, arg, None);
    canister_id
}

mod api {
    use super::*;

    pub fn consent_message(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        consent_message_request: &Icrc21ConsentMessageRequest,
    ) -> Result<Option<Icrc21ConsentMessageResponse>, CallError> {
        call_candid_as(
            env,
            canister_id,
            sender,
            "consent_message",
            (consent_message_request,),
        )
        .map(|(x,)| x)
    }

    pub fn prepare_credential(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        prepare_credential_request: &PrepareCredentialRequest,
    ) -> Result<PrepareCredentialResponse, CallError> {
        call_candid_as(
            env,
            canister_id,
            sender,
            "prepare_credential",
            (prepare_credential_request,),
        )
        .map(|(x,)| x)
    }

    pub fn get_credential(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        get_credential_request: &GetCredentialRequest,
    ) -> Result<GetCredentialResponse, CallError> {
        query_candid_as(
            env,
            canister_id,
            sender,
            "get_credential",
            (get_credential_request,),
        )
        .map(|(x,)| x)
    }
}

/// Verifies that the consent message can be requested.
#[test]
fn should_return_consent_message() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let consent_message_request = Icrc21ConsentMessageRequest {
        method: "dummy_method".to_string(),
        arg: Default::default(),
        preferences: Icrc21ConsentPreferences {
            language: "en".to_string(),
        },
    };

    let _consent_message_response =
        api::consent_message(&env, canister_id, principal_1(), &consent_message_request)?
            .expect("Got 'None' from consent_message");
    Ok(())
}

/// Verifies that a credential is being created including II interactions.
#[test]
#[serial]
fn should_issue_credential_e2e() -> Result<(), CallError> {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let ii_id = install_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, ii_id);

    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let prepare_id_alias_req = PrepareIdAliasRequest {
        identity_number,
        relying_party: relying_party.clone(),
        issuer: issuer.clone(),
    };

    let prepare_response =
        ii_api::prepare_id_alias(&env, ii_id, principal_1(), prepare_id_alias_req)?
            .expect("Got 'None' from prepare_id_alias");

    let prepared_id_alias = if let PrepareIdAliasResponse::Ok(response) = prepare_response {
        response
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
        match ii_api::get_id_alias(&env, ii_id, principal_1(), get_id_alias_req)?
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
    set_ic_root_public_key_for_testing(env.root_key());
    verify_id_alias_credential_jws(
        &id_alias_credentials
            .issuer_id_alias_credential
            .credential_jws,
        &AliasTuple {
            id_alias: id_alias_credentials.issuer_id_alias_credential.id_alias,
            id_dapp: id_alias_credentials.issuer_id_alias_credential.id_dapp,
        },
    )
    .expect("Invalid ID alias");

    let prepare_credential_response = api::prepare_credential(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        &PrepareCredentialRequest {
            credential_spec: CredentialSpec {
                info: "foo".to_string(),
            },
            signed_id_alias: id_alias_credentials.issuer_id_alias_credential.clone(),
        },
    )?;
    let prepared_credential =
        if let PrepareCredentialResponse::Ok(data) = prepare_credential_response {
            data
        } else {
            panic!(
                "Prepare credential failed: {:?}",
                prepare_credential_response
            );
        };

    let get_credential_response = api::get_credential(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        &GetCredentialRequest {
            credential_spec: CredentialSpec {
                info: "foo".to_string(),
            },
            signed_id_alias: id_alias_credentials.issuer_id_alias_credential,
            vc_jwt: prepared_credential.vc_jwt,
        },
    )?;

    if let GetCredentialResponse::Ok(_data) = get_credential_response {
        // OK
    } else {
        panic!("Get credential failed: {:?}", get_credential_response);
    };
    Ok(())
}
