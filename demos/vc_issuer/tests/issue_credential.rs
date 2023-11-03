//! Tests related to issue_credential canister call.

use assert_matches::assert_matches;
use candid::Principal;
use canister_sig_util::extract_raw_root_pk_from_der;
use canister_tests::api::internet_identity::vc_mvp as ii_api;
use canister_tests::flows;
use canister_tests::framework::{env, get_wasm_path, principal_1, principal_2, II_WASM};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_test_state_machine_client::call_candid_as;
use ic_test_state_machine_client::{query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ArgumentValue, CredentialSpec, GetCredentialRequest, GetCredentialResponse,
    Icrc21ConsentMessageResponse, Icrc21ConsentPreferences, Icrc21Error,
    Icrc21VcConsentMessageRequest, PrepareCredentialRequest, PrepareCredentialResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
    SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::path::PathBuf;
use vc_util::{
    create_verifiable_presentation_jwt, verify_id_alias_credential_jws, AliasTuple,
    PresentationParams,
};

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

    pub fn vc_consent_message(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        consent_message_request: &Icrc21VcConsentMessageRequest,
    ) -> Result<Option<Icrc21ConsentMessageResponse>, CallError> {
        call_candid_as(
            env,
            canister_id,
            sender,
            "vc_consent_message",
            (consent_message_request,),
        )
        .map(|(x,)| x)
    }

    pub fn add_employee(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        employee_id: Principal,
    ) -> Result<String, CallError> {
        call_candid_as(env, canister_id, sender, "add_employee", (employee_id,)).map(|(x,)| x)
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
fn should_return_vc_consent_message() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let mut args = HashMap::new();
    args.insert(
        "employerName".to_string(),
        ArgumentValue::String("DFINITY Foundation".to_string()),
    );
    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_name: "VerifiedEmployee".to_string(),
            arguments: Some(args),
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed")
            .expect("Got 'None' from vc_consent_message");
    assert_matches!(response, Icrc21ConsentMessageResponse::Ok(_));
    if let Icrc21ConsentMessageResponse::Ok(info) = response {
        assert_eq!(info.language, "en-US");
        assert!(info
            .consent_message
            .contains("Issue credential 'VerifiedEmployee'"));
        assert!(info.consent_message.contains("employerName"));
        assert!(info.consent_message.contains("DFINITY Foundation"));
    }
}

#[test]
fn should_fail_vc_consent_message_if_not_supported() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_name: "VerifiedAdult".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed")
            .expect("Got 'None' from vc_consent_message");
    assert_matches!(
        response,
        Icrc21ConsentMessageResponse::Err(Icrc21Error::NotSupported(_))
    );
}

#[test]
fn should_fail_vc_consent_message_if_missing_arguments() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_name: "VerifiedEmployee".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed")
            .expect("Got 'None' from vc_consent_message");
    assert_matches!(
        response,
        Icrc21ConsentMessageResponse::Err(Icrc21Error::MalformedCall(_))
    );
}

#[test]
fn should_fail_vc_consent_message_if_missing_required_argument() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let mut args = HashMap::new();
    args.insert("wrongArgument".to_string(), ArgumentValue::Int(42));

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_name: "VerifiedEmployee".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed")
            .expect("Got 'None' from vc_consent_message");
    assert_matches!(
        response,
        Icrc21ConsentMessageResponse::Err(Icrc21Error::MalformedCall(_))
    );
}
fn employee_credential_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert(
        "employeeName".to_string(),
        ArgumentValue::String("DFINITY Foundation".to_string()),
    );
    CredentialSpec {
        credential_name: "VerifiedEmployee".to_string(),
        arguments: Some(args),
    }
}
#[test]
fn should_fail_prepare_credential_for_unauthorized_principal() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let unauthorized_principal = principal_1();
    let signed_id_alias = SignedIdAlias {
        id_alias: principal_2(),
        id_dapp: unauthorized_principal,
        credential_jws: "dummy jws".to_string(),
    };
    let response = api::prepare_credential(
        &env,
        issuer_id,
        principal_2(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, PrepareCredentialResponse::Err(e) if format!("{:?}", e).contains("unauthorized principal"));
}

#[test]
fn should_prepare_credential_for_authorized_principal() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let authorized_principal = principal_1();
    let signed_id_alias = SignedIdAlias {
        id_alias: principal_2(),
        id_dapp: authorized_principal,
        credential_jws: "dummy jws".to_string(),
    };
    let _add_employee_response =
        api::add_employee(&env, issuer_id, principal_2(), authorized_principal)
            .expect("API call failed");
    let response = api::prepare_credential(
        &env,
        issuer_id,
        principal_2(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, PrepareCredentialResponse::Ok(_));
}

/// Verifies that a credential is being created including II interactions.
#[test]
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
    let root_pk_raw =
        extract_raw_root_pk_from_der(&env.root_key()).expect("Failed decoding IC root key.");
    verify_id_alias_credential_jws(
        &id_alias_credentials
            .issuer_id_alias_credential
            .credential_jws,
        &AliasTuple {
            id_alias: id_alias_credentials.issuer_id_alias_credential.id_alias,
            id_dapp: id_alias_credentials.issuer_id_alias_credential.id_dapp,
        },
        &root_pk_raw,
    )
    .expect("Invalid ID alias");

    let _add_employee_response = api::add_employee(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
    )?;
    let prepare_credential_response = api::prepare_credential(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
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
            credential_spec: employee_credential_spec(),
            signed_id_alias: id_alias_credentials.issuer_id_alias_credential,
            prepared_context: prepared_credential.prepared_context,
        },
    )?;

    if let GetCredentialResponse::Ok(data) = get_credential_response {
        let params = PresentationParams {
            id_alias_vc_jws: id_alias_credentials.rp_id_alias_credential.credential_jws,
            requested_vc_jws: data.vc_jws,
        };
        let alias_tuple = AliasTuple {
            id_alias: id_alias_credentials.rp_id_alias_credential.id_alias,
            id_dapp: id_alias_credentials.rp_id_alias_credential.id_dapp,
        };
        let _vp = create_verifiable_presentation_jwt(params, &alias_tuple)
            .expect("failed creating presentation");
    } else {
        panic!("Get credential failed: {:?}", get_credential_response);
    };
    Ok(())
}
