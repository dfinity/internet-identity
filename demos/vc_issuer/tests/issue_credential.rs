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
use identity_core::common::Value;
use identity_jose::jwt::JwtClaims;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::path::PathBuf;
use vc_util::issuer_api::{
    ArgumentValue, CredentialSpec, GetCredentialRequest, GetCredentialResponse,
    Icrc21ConsentMessageResponse, Icrc21ConsentPreferences, Icrc21Error,
    Icrc21VcConsentMessageRequest, IssueCredentialError, PrepareCredentialRequest,
    PrepareCredentialResponse, SignedIdAlias as SignedIssuerIdAlias,
};
use vc_util::{
    did_for_principal, verify_credential_jws_with_canister_id, verify_id_alias_credential_jws,
    AliasTuple,
};

const DUMMY_ROOT_KEY: &str ="308182301d060d2b0601040182dc7c0503010201060c2b0601040182dc7c05030201036100adf65638a53056b2222c91bb2457b0274bca95198a5acbdadfe7fd72178f069bdea8d99e9479d8087a2686fc81bf3c4b11fe275570d481f1698f79d468afe0e57acc1e298f8b69798da7a891bbec197093ec5f475909923d48bfed6843dbed1f";
const DUMMY_II_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";

/// Dummy alias JWS for testing, valid wrt DUMMY_ROOT_KEY and DUMMY_II_CANISTER_ID.
/// id dapp: nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe
/// id alias: vhbib-m4hm6-hpvyc-7prd2-siivo-nbd7r-67o5x-n3awh-qsmqz-wznjf-tqe
const DUMMY_ALIAS_JWS: &str ="eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCRVNzWHp2bTEzd1BkRTVZSndvLTBCYkdBTHdCN0J2bW1LZUxramFUUTdkQSJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOm51Z3ZhLXM3YzZ2LTR5c3p0LWtveWN2LTViNjIzLWFuN3E2LWhhMm56LWt6NnJzLWhhd2dsLW56bmJlLXJxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDp2aGJpYi1tNGhtNi1ocHZ5Yy03cHJkMi1zaWl2by1uYmQ3ci02N281eC1uM2F3aC1xc21xei13em5qZi10cWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggnk2d-80NLXpxOs-YszCLd4yvrGBtLEGqe6rp6khNthCCBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggaAMB9TDaAhXeQPY8DCCUq90vqJJDqpDAVwU-0WdA9OmCBFgghh7VsiTOqTlAiY8hcsbF1pFnG5t1x4kQ7rt2bae_6iGCBFgggcqzMKDpDQKcyRl6xrGy4SIYEtgVJgSLlHGFvHN6zuSCBFggBNxwNVuf0_gTaiM6hbpNNCcEIBfxLHoor0N1mpX-uNeCBFggICEcda6JC5WRFIbzoGGJdJINoas-EWtoCU0lysCe3OGDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCY_kVxXw7Wk8HlA0FqOpX-3WMdI0mmxAtY9DJv8xEkfitcTOR0FcE412IftkdH48hkdHJlZYMBggRYIPKxlnFAySvK4ahA_Q0IkEopYPh8H4_IRCFRGb2i23QRgwJDc2lngwJYIFcsa4eb-HMrTnmGWNje_RfErQYi0wNCJvGDrzqazq0OgwGCBFggg7ijRBePgPVau7zffNEvAXThew-FqcBH_cB-fF7722eDAlgg3ikzXLDphmWB8YbAxZDjZfLFd6bDS-sLAPzmVj0nlvSCA0A";

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

    pub static ref DUMMY_SIGNED_ID_ALIAS: SignedIssuerIdAlias = SignedIssuerIdAlias {
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
        &canister_sig_pk.canister_id,
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
                signed_id_alias: SignedIssuerIdAlias {
                    id_alias: id_alias_credentials.issuer_id_alias_credential.id_alias,
                    id_dapp: id_alias_credentials.issuer_id_alias_credential.id_dapp,
                    credential_jws: id_alias_credentials
                        .issuer_id_alias_credential
                        .credential_jws
                        .clone(),
                },
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
                credential_spec: credential_spec.clone(),
                signed_id_alias: SignedIssuerIdAlias {
                    id_alias: id_alias_credentials.issuer_id_alias_credential.id_alias,
                    id_dapp: id_alias_credentials.issuer_id_alias_credential.id_dapp,
                    credential_jws: id_alias_credentials
                        .issuer_id_alias_credential
                        .credential_jws
                        .clone(),
                },
                prepared_context: prepared_credential.prepared_context,
            },
        )?;
        match_value!(
            get_credential_response,
            GetCredentialResponse::Ok(credential_data)
        );
        let claims = verify_credential_jws_with_canister_id(
            &credential_data.vc_jws,
            &issuer_id,
            &root_pk_raw,
        )
        .expect("credential verification failed");
        validate_vc_claims(
            &claims,
            &credential_spec,
            id_alias_credentials.issuer_id_alias_credential.id_alias,
        );
    }

    Ok(())
}

/// Validates that the given claims are consistent with the credential spec and the
/// requesting principal.
fn validate_vc_claims(
    claims: &JwtClaims<Value>,
    credential_spec: &CredentialSpec,
    subject_principal: Principal,
) {
    assert_eq!(
        claims.sub(),
        Some(did_for_principal(subject_principal)).as_deref()
    );
    let vc = claims.vc().expect("missing vc in id_alias JWT claims");
    assert_eq!(
        vc.get("type"),
        Some(Value::Array(vec![
            Value::String("VerifiableCredential".to_string()),
            Value::String(credential_spec.credential_name.clone())
        ]))
        .as_ref()
    );
}

#[test]
fn should_configure() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    api::configure(&env, issuer_id, &DUMMY_ISSUER_INIT).expect("API call failed");
}
