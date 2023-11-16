//! Tests related to issue_credential canister call.

use assert_matches::assert_matches;
use candid::{CandidType, Deserialize, Principal};
use canister_sig_util::{extract_raw_root_pk_from_der, CanisterSigPublicKey};
use canister_tests::api::internet_identity::vc_mvp as ii_api;
use canister_tests::framework::{
    env, get_wasm_path, principal_1, principal_2, test_principal, II_WASM,
};
use canister_tests::{flows, match_value};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_test_state_machine_client::{call_candid, call_candid_as};
use ic_test_state_machine_client::{query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ArgumentValue, CredentialSpec, GetCredentialRequest, GetCredentialResponse,
    Icrc21ConsentMessageResponse, Icrc21ConsentPreferences, Icrc21Error,
    Icrc21VcConsentMessageRequest, IssueCredentialError, PrepareCredentialRequest,
    PrepareCredentialResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
    SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::path::PathBuf;
use vc_util_br::{
    create_verifiable_presentation_jwt, verify_id_alias_credential_jws, AliasTuple,
    PresentationParams,
};

const DUMMY_ROOT_KEY: &str ="308182301d060d2b0601040182dc7c0503010201060c2b0601040182dc7c05030201036100adf65638a53056b2222c91bb2457b0274bca95198a5acbdadfe7fd72178f069bdea8d99e9479d8087a2686fc81bf3c4b11fe275570d481f1698f79d468afe0e57acc1e298f8b69798da7a891bbec197093ec5f475909923d48bfed6843dbed1f";
const DUMMY_II_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";

/// Dummy alias JWS for testing, valid wrt DUMMY_ROOT_KEY and DUMMY_II_CANISTER_ID.
/// id dapp: nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe
/// id alias: vhbib-m4hm6-hpvyc-7prd2-siivo-nbd7r-67o5x-n3awh-qsmqz-wznjf-tqe
const DUMMY_ALIAS_JWS: &str ="eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCRVNzWHp2bTEzd1BkRTVZSndvLTBCYkdBTHdCN0J2bW1LZUxramFUUTdkQSJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbnRpdHkiLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaW50ZXJuZXRjb21wdXRlci5vcmcvY3JlZGVudGlhbC9pbnRlcm5ldC1pZGVudGl0eS8xNjIwMzI4NjMwMDAwMDAwMDAwIiwic3ViIjoiZGlkOmljcDpudWd2YS1zN2M2di00eXN6dC1rb3ljdi01YjYyMy1hbjdxNi1oYTJuei1rejZycy1oYXdnbC1uem5iZS1ycWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIkludGVybmV0SWRlbnRpdHlJZEFsaWFzIl0sImNyZWRlbnRpYWxTdWJqZWN0Ijp7Imhhc19pZF9hbGlhcyI6ImRpZDppY3A6dmhiaWItbTRobTYtaHB2eWMtN3ByZDItc2lpdm8tbmJkN3ItNjdvNXgtbjNhd2gtcXNtcXotd3puamYtdHFlIn19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIOAkKgS9cT41KYHV47u2mn5AvitWaQdv8gn6ge5ci0BsggRYIO7r3KlmuVuXzs32JOAFdYoRtdAxtXSBhWX6qon8WiWLggRYIMdGRB9oREkOrmIN0r91ZEz3otEQam_aTDyOsrVIUF0QggRYIFMfz_BuZNfLXDaT_C4J-nPkgfdZK97bx-1C39IZCU_qggRYIHXwf9kI-Wi-ecWYqm3FjzFuet1_XdqUfB7_zuYT8B7zggRYIIEuB_Yu2JxGEH0b4ggGbqAOctGe8yLQMizEg6HySFxUgwGCBFggOKD8BZWGPvG_n6V_d7qq49xnmuC2SLWSMpLBtagYH2eDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwslLEnD_WFgjR3FiyC0i1a-vSshC_3v_OdR_eZfjnPfhtcG3YG4F-9iVIwLVAoymQZHRyZWWDAYIEWCB_Shm7N22IqXER6QSfFu-unuQlt4SbQngHjAC7a7IWC4MCQ3NpZ4MCWCBXLGuHm_hzK055hljY3v0XxK0GItMDQibxg686ms6tDoMBgwJYIAE7yg3MaKLUw-Nyn1C9U_clWJoH-FZdTr5bD4YrQyQjggNAggRYIFBRoOJ2NL2_DzOMVQkK5KM--f2ZP7OtFik4_gmhc_nD";

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

    pub static ref DUMMY_ISSUER_INIT: IssuerInit = IssuerInit {
        ic_root_key_der: hex::decode(DUMMY_ROOT_KEY).unwrap(),
        idp_canister_ids: vec![Principal::from_text(DUMMY_II_CANISTER_ID).unwrap()],
    };

    pub static ref DUMMY_SIGNED_ID_ALIAS: SignedIdAlias = SignedIdAlias {
        id_alias: Principal::from_text("vhbib-m4hm6-hpvyc-7prd2-siivo-nbd7r-67o5x-n3awh-qsmqz-wznjf-tqe").unwrap(),
        id_dapp: Principal::from_text("nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe").unwrap(),
        credential_jws: DUMMY_ALIAS_JWS.to_string(),
    };
}

pub fn install_canister(env: &StateMachine, wasm: Vec<u8>) -> CanisterId {
    let canister_id = env.create_canister(None);
    let arg = candid::encode_one("()").expect("error encoding II installation arg as candid");
    env.install_canister(canister_id, wasm, arg, None);
    canister_id
}

#[derive(CandidType, Deserialize)]
pub struct IssuerInit {
    /// Root of trust for checking canister signatures.
    ic_root_key_der: Vec<u8>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
}

pub fn install_issuer(env: &StateMachine, init: &IssuerInit) -> CanisterId {
    let canister_id = env.create_canister(None);
    let arg = candid::encode_one(Some(init)).expect("error encoding II installation arg as candid");
    env.install_canister(canister_id, VC_ISSUER_WASM.clone(), arg, None);
    canister_id
}

mod api {
    use super::*;

    pub fn configure(
        env: &StateMachine,
        canister_id: CanisterId,
        config: &IssuerInit,
    ) -> Result<(), CallError> {
        call_candid(env, canister_id, "configure", (config,))
    }

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

    pub fn add_graduate(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        employee_id: Principal,
    ) -> Result<String, CallError> {
        call_candid_as(env, canister_id, sender, "add_graduate", (employee_id,)).map(|(x,)| x)
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
        Icrc21ConsentMessageResponse::Err(Icrc21Error::NotSupported(_))
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
        Icrc21ConsentMessageResponse::Err(Icrc21Error::NotSupported(_))
    );
}

fn employee_credential_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert(
        "employerName".to_string(),
        ArgumentValue::String("DFINITY Foundation".to_string()),
    );
    CredentialSpec {
        credential_name: "VerifiedEmployee".to_string(),
        arguments: Some(args),
    }
}

fn degree_credential_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert(
        "institutionName".to_string(),
        ArgumentValue::String("DFINITY College of Engineering".to_string()),
    );
    CredentialSpec {
        credential_name: "UniversityDegreeCredential".to_string(),
        arguments: Some(args),
    }
}

#[test]
fn should_fail_prepare_credential_for_unauthorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    let response = api::prepare_credential(
        &env,
        issuer_id,
        signed_id_alias.id_dapp,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, PrepareCredentialResponse::Err(e) if format!("{:?}", e).contains("unauthorized principal"));
}

#[test]
fn should_fail_prepare_credential_for_wrong_sender() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();

    let response = api::prepare_credential(
        &env,
        issuer_id,
        principal_1(), // not the same as signed_id_alias.id_dapp
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, 
        PrepareCredentialResponse::Err(IssueCredentialError::UnauthorizedSubject(e)) if e.contains("Caller 6epth-hmqup-wz4mv-svl2m-mhcbb-24skq-tbdhq-2txct-2qugv-xuzva-eqe does not match id alias dapp principal nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe."));
}

#[test]
fn should_fail_get_credential_for_wrong_sender() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    let authorized_principal = signed_id_alias.id_dapp;
    api::add_employee(
        &env,
        issuer_id,
        Principal::anonymous(),
        authorized_principal,
    )
    .expect("failed to add employee");
    let unauthorized_principal = test_principal(2);

    match_value!(
        api::prepare_credential(
            &env,
            issuer_id,
            authorized_principal,
            &PrepareCredentialRequest {
                credential_spec: employee_credential_spec(),
                signed_id_alias: signed_id_alias.clone(),
            },
        ),
        Ok(PrepareCredentialResponse::Ok(prepare_credential_response))
    );
    let get_credential_response = api::get_credential(
        &env,
        issuer_id,
        unauthorized_principal,
        &GetCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
            prepared_context: prepare_credential_response.prepared_context,
        },
    )
    .expect("API call failed");
    assert_matches!(get_credential_response, 
        GetCredentialResponse::Err(IssueCredentialError::UnauthorizedSubject(e)) if e.contains("Caller sl5og-mycaa-aaaaa-aaaap-4 does not match id alias dapp principal nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe."));
}

#[test]
fn should_fail_prepare_credential_for_anonymous_caller() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    let response = api::prepare_credential(
        &env,
        issuer_id,
        Principal::anonymous(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, 
        PrepareCredentialResponse::Err(IssueCredentialError::UnauthorizedSubject(e)) if e.contains("Caller 2vxsx-fae does not match id alias dapp principal nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe."));
}

#[test]
fn should_fail_prepare_credential_for_wrong_root_key() {
    let env = env();
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: canister_sig_util::IC_ROOT_PK_DER.to_vec(), // does not match the DUMMY_ROOT_KEY, which is used in DUMMY_ALIAS_JWS
            idp_canister_ids: vec![Principal::from_text(DUMMY_II_CANISTER_ID).unwrap()],
        },
    );
    let response = api::prepare_credential(
        &env,
        issuer_id,
        DUMMY_SIGNED_ID_ALIAS.clone().id_dapp,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(
        response,
        PrepareCredentialResponse::Err(IssueCredentialError::InvalidIdAlias(_))
    );
}

#[test]
fn should_fail_prepare_credential_for_wrong_idp_canister_id() {
    let env = env();
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: hex::decode(DUMMY_ROOT_KEY).unwrap(),
            idp_canister_ids: vec![Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap()], // does not match the DUMMY_II_CANISTER_ID, which is used in DUMMY_ALIAS_JWS
        },
    );
    let response = api::prepare_credential(
        &env,
        issuer_id,
        DUMMY_SIGNED_ID_ALIAS.clone().id_dapp,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(
        response,
        PrepareCredentialResponse::Err(IssueCredentialError::InvalidIdAlias(_))
    );
}

#[test]
fn should_prepare_employee_credential_for_authorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    api::add_employee(&env, issuer_id, principal_2(), signed_id_alias.id_dapp)
        .expect("API call failed");
    let response = api::prepare_credential(
        &env,
        issuer_id,
        signed_id_alias.id_dapp,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, PrepareCredentialResponse::Ok(_));
}

#[test]
fn should_prepare_degree_credential_for_authorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    api::add_graduate(&env, issuer_id, principal_2(), signed_id_alias.id_dapp)
        .expect("API call failed");
    let response = api::prepare_credential(
        &env,
        issuer_id,
        signed_id_alias.id_dapp,
        &PrepareCredentialRequest {
            credential_spec: degree_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response, PrepareCredentialResponse::Ok(_));
}

/// Verifies that different credentials are being created including II interactions.
#[test]
fn should_issue_credential_e2e() -> Result<(), CallError> {
    let env = env();
    let ii_id = install_canister(&env, II_WASM.clone());
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: env.root_key().to_vec(),
            idp_canister_ids: vec![ii_id],
        },
    );
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
    let canister_sig_pk =
        CanisterSigPublicKey::try_from(prepared_id_alias.canister_sig_pk_der.as_ref())
            .expect("failed parsing canister sig pk");

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
        &canister_sig_pk,
        &root_pk_raw,
    )
    .expect("Invalid ID alias");

    api::add_employee(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
    )?;
    api::add_graduate(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
    )?;

    for credential_spec in vec![employee_credential_spec(), degree_credential_spec()] {
        let prepare_credential_response = api::prepare_credential(
            &env,
            issuer_id,
            id_alias_credentials
                .issuer_id_alias_credential
                .id_dapp
                .clone(),
            &PrepareCredentialRequest {
                credential_spec: credential_spec.clone(),
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
            id_alias_credentials
                .issuer_id_alias_credential
                .id_dapp
                .clone(),
            &GetCredentialRequest {
                credential_spec,
                signed_id_alias: id_alias_credentials.issuer_id_alias_credential.clone(),
                prepared_context: prepared_credential.prepared_context,
            },
        )?;

        if let GetCredentialResponse::Ok(data) = get_credential_response {
            let params = PresentationParams {
                id_alias_vc_jws: id_alias_credentials
                    .rp_id_alias_credential
                    .credential_jws
                    .clone(),
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
    }

    Ok(())
}

#[test]
fn should_configure() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    api::configure(&env, issuer_id, &DUMMY_ISSUER_INIT).expect("API call failed");
}
