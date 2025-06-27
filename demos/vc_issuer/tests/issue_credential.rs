//! Tests related to issue_credential canister call.

use assert_matches::assert_matches;
use candid::{CandidType, Deserialize, Principal};
use canister_tests::api::http_request;
use canister_tests::api::internet_identity::vc_mvp as ii_api;
use canister_tests::flows;
use canister_tests::framework::{env, get_wasm_path, principal_1, test_principal, time, II_WASM};
use ic_canister_sig_creation::{
    extract_raw_root_pk_from_der, CanisterSigPublicKey, IC_ROOT_PK_DER,
};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_response_verification::types::VerificationInfo;
use ic_response_verification::verify_request_response_pair;
use ic_verifiable_credentials::issuer_api::{
    ArgumentValue, CredentialSpec, DerivationOriginData, DerivationOriginError,
    DerivationOriginRequest, GetCredentialRequest, Icrc21ConsentInfo, Icrc21ConsentPreferences,
    Icrc21Error, Icrc21VcConsentMessageRequest, IssueCredentialError, IssuedCredentialData,
    PrepareCredentialRequest, PreparedCredentialData, SignedIdAlias as SignedIssuerIdAlias,
};
use ic_verifiable_credentials::{
    get_verified_id_alias_from_jws, validate_claims_match_spec,
    verify_credential_jws_with_canister_id,
};
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, PrepareIdAliasRequest,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use pocket_ic::{call_candid, call_candid_as};
use pocket_ic::{query_candid_as, CallError, PocketIc};
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

const DUMMY_ROOT_KEY: &str ="308182301d060d2b0601040182dc7c0503010201060c2b0601040182dc7c05030201036100adf65638a53056b2222c91bb2457b0274bca95198a5acbdadfe7fd72178f069bdea8d99e9479d8087a2686fc81bf3c4b11fe275570d481f1698f79d468afe0e57acc1e298f8b69798da7a891bbec197093ec5f475909923d48bfed6843dbed1f";
const DUMMY_II_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
const DUMMY_DERIVATION_ORIGIN: &str = "https://y2aaj-miaaa-aaaad-aacxq-cai.ic0.app";
const DUMMY_FRONTEND_HOSTNAME: &str = "https://y2aaj-miaaa-aaaad-aacxq-cai.ic0.app";

/// Dummy alias JWS for testing, valid wrt DUMMY_ROOT_KEY and DUMMY_II_CANISTER_ID.
/// id dapp: nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe
/// id alias: jkk22-zqdxc-kgpez-6sv2m-5pby4-wi4t2-prmoq-gf2ih-i2qtc-v37ac-5ae
const DUMMY_ALIAS_JWS: &str ="eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCMGd6TTVJeXFMYUhyMDhtQTRWd2J5SmRxQTFyRVFUX2xNQnVVbmN5UDVVYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6ImRhdGE6dGV4dC9wbGFpbjtjaGFyc2V0PVVURi04LHRpbWVzdGFtcF9uczoxNjIwMzI4NjMwMDAwMDAwMDAwLGFsaWFzX2hhc2g6YTI3YzU4NTQ0MmUwN2RkZWFkZTRjNWE0YTAzMjdkMzA4NTE5NDAzYzRlYTM3NDIxNzBhZTRkYzk1YjIyZTQ3MyIsInN1YiI6ImRpZDppY3A6bnVndmEtczdjNnYtNHlzenQta295Y3YtNWI2MjMtYW43cTYtaGEybnota3o2cnMtaGF3Z2wtbnpuYmUtcnFlIiwidmMiOnsiQGNvbnRleHQiOiJodHRwczovL3d3dy53My5vcmcvMjAxOC9jcmVkZW50aWFscy92MSIsInR5cGUiOlsiVmVyaWZpYWJsZUNyZWRlbnRpYWwiLCJJbnRlcm5ldElkZW50aXR5SWRBbGlhcyJdLCJjcmVkZW50aWFsU3ViamVjdCI6eyJJbnRlcm5ldElkZW50aXR5SWRBbGlhcyI6eyJoYXNJZEFsaWFzIjoiamtrMjItenFkeGMta2dwZXotNnN2Mm0tNXBieTQtd2k0dDItcHJtb3EtZ2YyaWgtaTJxdGMtdjM3YWMtNWFlIn19fX0.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggvlJBTZDgK1_9Vb3-18dWKIfy28WTjZ1YqdjFWWAIX96CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFgg_KZ0TVqubo_EGWoMUPA35BYZ4B5ZRkR_zDfNIQCwa46CBFggj_ZV-7o59iVEjztzZtpNnO9YC7GjbKmg2eDtJzGz1weCBFggXAzCWvb9h4qsVs41IUJBABzjSqAZ8DIzF_ghGHpGmHGCBFggJhbsbvKYt7rjLK5SI0NDc600o-ajSYQNuOXps6qUrdiCBFggBFQwZetJeY_gx6TQohTqUOskblddajS20DA0esxWoyWDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDC5cq4UxYy7cnkcw6yv5SCh4POY9u0iHecZuxO8E9oxIqXRdHmnYVF0Fv_R-aws0EBkdHJlZYMBggRYIOGnlc_3yXPTVrEJ1p3dKX5HxkMOziUnpA1HeXiQW4O8gwJDc2lngwJYIIOQR7wl3Ws9Jb8VP4rhIb37XKLMkkZ2P7WaZ5we60WGgwGCBFgg21-OewBgqt_-0AtHHHS4yPyQK9g6JTHaGUuSIw4QYgqDAlgg5bQnHHvS3FfM_BaiSL6n19qoXkuA1KoLWk963fOUMW-CA0A";
const DUMMY_ALIAS_ID_DAPP_PRINCIPAL: &str =
    "nugva-s7c6v-4yszt-koycv-5b623-an7q6-ha2nz-kz6rs-hawgl-nznbe-rqe";

lazy_static! {
    /** The gzipped Wasm module for the current VC_ISSUER build, i.e. the one we're testing */
    pub static ref VC_ISSUER_WASM: Vec<u8> = {
        let def_path = PathBuf::from("./").join("vc_demo_issuer.wasm.gz");
        let err = format!("
        Could not find VC Issuer Wasm module for current build.
        I will look for it at {:?} (note that I run from {:?}).
        ", &def_path,
            &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or_else(|_|
                "an unknown directory".to_string()));
                get_wasm_path("VC_ISSUER_WASM".to_string(), &def_path).expect(&err)

    };

    pub static ref DUMMY_ISSUER_INIT: IssuerInit = IssuerInit {
        ic_root_key_der: Some(hex::decode(DUMMY_ROOT_KEY).unwrap()),
        idp_canister_ids: vec![Principal::from_text(DUMMY_II_CANISTER_ID).unwrap()],
        derivation_origin: DUMMY_DERIVATION_ORIGIN.to_string(),
        frontend_hostname: DUMMY_FRONTEND_HOSTNAME.to_string(),
    };

    pub static ref DUMMY_SIGNED_ID_ALIAS: SignedIssuerIdAlias = SignedIssuerIdAlias {
        credential_jws: DUMMY_ALIAS_JWS.to_string(),
    };
}

pub fn install_canister_as(
    env: &PocketIc,
    wasm: Vec<u8>,
    controller: Option<Principal>,
) -> CanisterId {
    let canister_id = env.create_canister_with_settings(controller, None);
    let arg = candid::encode_one("()").expect("error encoding II installation arg as candid");
    env.install_canister(canister_id, wasm, arg, controller);
    canister_id
}

pub fn install_canister(env: &PocketIc, wasm: Vec<u8>) -> CanisterId {
    install_canister_as(env, wasm, None)
}

#[derive(CandidType, Deserialize)]
pub struct IssuerInit {
    /// Root of trust for checking canister signatures.
    ic_root_key_der: Option<Vec<u8>>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
    /// The derivation origin to be used by the issuer.
    derivation_origin: String,
    /// Frontend hostname be used by the issuer.
    frontend_hostname: String,
}

pub fn install_issuer(env: &PocketIc, init: &IssuerInit) -> CanisterId {
    let canister_id = env.create_canister();
    let arg = candid::encode_one(Some(init)).expect("error encoding II installation arg as candid");
    env.install_canister(canister_id, VC_ISSUER_WASM.clone(), arg, None);
    canister_id
}

mod api {
    use super::*;
    use pocket_ic::common::rest::RawEffectivePrincipal;

    pub fn configure(
        env: &PocketIc,
        canister_id: CanisterId,
        config: &IssuerInit,
    ) -> Result<(), CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "configure",
            (config,),
        )
    }

    pub fn vc_consent_message(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        consent_message_request: &Icrc21VcConsentMessageRequest,
    ) -> Result<Result<Icrc21ConsentInfo, Icrc21Error>, CallError> {
        call_candid_as(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            sender,
            "vc_consent_message",
            (consent_message_request,),
        )
        .map(|(x,)| x)
    }

    pub fn derivation_origin(
        env: &PocketIc,
        canister_id: CanisterId,
        derivation_origin_req: &DerivationOriginRequest,
    ) -> Result<Result<DerivationOriginData, DerivationOriginError>, CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "derivation_origin",
            (derivation_origin_req,),
        )
        .map(|(x,)| x)
    }

    pub fn set_derivation_origin(
        env: &PocketIc,
        canister_id: CanisterId,
        frontend_hostname: &str,
        derivation_origin: &str,
    ) -> Result<(), CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "set_derivation_origin",
            (frontend_hostname, derivation_origin),
        )
    }

    pub fn set_alternative_origins(
        env: &PocketIc,
        canister_id: CanisterId,
        alternative_origins: &str,
    ) -> Result<(), CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "set_alternative_origins",
            (alternative_origins,),
        )
    }

    pub fn add_employee(
        env: &PocketIc,
        canister_id: CanisterId,
        employee_id: Principal,
    ) -> Result<String, CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "add_employee",
            (employee_id,),
        )
        .map(|(x,)| x)
    }

    pub fn add_graduate(
        env: &PocketIc,
        canister_id: CanisterId,
        graduate_id: Principal,
    ) -> Result<String, CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "add_graduate",
            (graduate_id,),
        )
        .map(|(x,)| x)
    }

    pub fn add_adult(
        env: &PocketIc,
        canister_id: CanisterId,
        adult_id: Principal,
    ) -> Result<String, CallError> {
        call_candid(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            "add_adult",
            (adult_id,),
        )
        .map(|(x,)| x)
    }

    pub fn prepare_credential(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        prepare_credential_request: &PrepareCredentialRequest,
    ) -> Result<Result<PreparedCredentialData, IssueCredentialError>, CallError> {
        call_candid_as(
            env,
            canister_id,
            RawEffectivePrincipal::None,
            sender,
            "prepare_credential",
            (prepare_credential_request,),
        )
        .map(|(x,)| x)
    }

    pub fn get_credential(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        get_credential_request: &GetCredentialRequest,
    ) -> Result<Result<IssuedCredentialData, IssueCredentialError>, CallError> {
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
fn should_return_vc_consent_message_for_employment_vc() {
    let test_cases = [
        ("en-US", "en", "# DFINITY Foundation Employment Credential"),
        ("de-DE", "de", "# Beschäftigungsausweis DFINITY Foundation"),
        ("ja-JP", "en", "# DFINITY Foundation Employment Credential"), // test fallback language
    ];
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    for (requested_language, actual_language, consent_message_snippet) in test_cases {
        let mut args = HashMap::new();
        args.insert(
            "employerName".to_string(),
            ArgumentValue::String("DFINITY Foundation".to_string()),
        );
        let consent_message_request = Icrc21VcConsentMessageRequest {
            credential_spec: CredentialSpec {
                credential_type: "VerifiedEmployee".to_string(),
                arguments: Some(args),
            },
            preferences: Icrc21ConsentPreferences {
                language: requested_language.to_string(),
            },
        };

        let response =
            api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
                .expect("API call failed")
                .expect("Consent message error");

        assert_eq!(response.language, actual_language);
        assert!(response
            .consent_message
            .starts_with(consent_message_snippet));
    }
}

#[test]
fn should_return_vc_consent_message_for_adult_vc() {
    let test_cases = [
        ("en-US", "en", "# Verified Adult"),
        ("de-DE", "de", "# Erwachsene Person"),
        ("ja-JP", "en", "# Verified Adult"), // test fallback language
    ];
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    for (requested_language, actual_language, consent_message_snippet) in test_cases {
        let mut args = HashMap::new();
        args.insert("minAge".to_string(), ArgumentValue::Int(18));
        let consent_message_request = Icrc21VcConsentMessageRequest {
            credential_spec: CredentialSpec {
                credential_type: "VerifiedAdult".to_string(),
                arguments: Some(args),
            },
            preferences: Icrc21ConsentPreferences {
                language: requested_language.to_string(),
            },
        };

        let response =
            api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
                .expect("API call failed")
                .expect("Consent message error");
        assert_eq!(response.language, actual_language);
        assert!(response
            .consent_message
            .starts_with(consent_message_snippet));
    }
}

#[test]
fn should_fail_vc_consent_message_if_not_supported() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_type: "VerifiedResident".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed");
    assert_matches!(response, Err(Icrc21Error::UnsupportedCanisterCall(_)));
}

#[test]
fn should_fail_vc_consent_message_if_missing_arguments() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_type: "VerifiedEmployee".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed");
    assert_matches!(response, Err(Icrc21Error::UnsupportedCanisterCall(_)));
}

#[test]
fn should_fail_vc_consent_message_if_missing_required_argument() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let mut args = HashMap::new();
    args.insert("wrongArgument".to_string(), ArgumentValue::Int(42));

    let consent_message_request = Icrc21VcConsentMessageRequest {
        credential_spec: CredentialSpec {
            credential_type: "VerifiedEmployee".to_string(),
            arguments: None,
        },
        preferences: Icrc21ConsentPreferences {
            language: "en-US".to_string(),
        },
    };

    let response =
        api::vc_consent_message(&env, canister_id, principal_1(), &consent_message_request)
            .expect("API call failed");
    assert_matches!(response, Err(Icrc21Error::UnsupportedCanisterCall(_)));
}

#[test]
fn should_return_derivation_origin() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let frontend_hostname = format!("https://{}.icp0.io", canister_id.to_text());
    let req = DerivationOriginRequest { frontend_hostname };
    let response = api::derivation_origin(&env, canister_id, &req)
        .expect("API call failed")
        .expect("derivation_origin error");
    assert_eq!(response.origin, req.frontend_hostname);
}

#[test]
fn should_set_derivation_origin() {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let req = DerivationOriginRequest {
        frontend_hostname: "frontend_hostname.com".to_string(),
    };

    let response = api::derivation_origin(&env, canister_id, &req)
        .expect("API call failed")
        .expect("derivation_origin error");
    let default_derivation_origin = format!("https://{}.icp0.io", canister_id.to_text());
    assert_eq!(response.origin, default_derivation_origin);

    let derivation_origin = "https://derivation.origin";
    api::set_derivation_origin(
        &env,
        canister_id,
        "frontend_hostname.com".to_string().as_str(),
        derivation_origin,
    )
    .expect("failed to set derivation_origin");

    let response = api::derivation_origin(&env, canister_id, &req)
        .expect("API call failed")
        .expect("derivation_origin error");
    assert_eq!(response.origin, derivation_origin);
}

#[test]
fn should_return_derivation_origin_with_custom_init() {
    let env = env();
    let custom_init = IssuerInit {
        ic_root_key_der: Some(hex::decode(DUMMY_ROOT_KEY).unwrap()),
        idp_canister_ids: vec![Principal::from_text(DUMMY_II_CANISTER_ID).unwrap()],
        derivation_origin: "https://derivation_origin".to_string(),
        frontend_hostname: "https://frontend.host.name".to_string(),
    };
    let issuer_id = install_issuer(&env, &custom_init);
    let response = api::derivation_origin(
        &env,
        issuer_id,
        &DerivationOriginRequest {
            frontend_hostname: custom_init.frontend_hostname.clone(),
        },
    )
    .expect("API call failed")
    .expect("derivation_origin error");
    assert_eq!(response.origin, custom_init.derivation_origin);
}

fn employee_credential_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert(
        "employerName".to_string(),
        ArgumentValue::String("DFINITY Foundation".to_string()),
    );
    CredentialSpec {
        credential_type: "VerifiedEmployee".to_string(),
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
        credential_type: "UniversityDegreeCredential".to_string(),
        arguments: Some(args),
    }
}

fn adult_credential_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert("minAge".to_string(), ArgumentValue::Int(18));
    CredentialSpec {
        credential_type: "VerifiedAdult".to_string(),
        arguments: Some(args),
    }
}

#[test]
fn should_fail_prepare_credential_for_unauthorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let response = api::prepare_credential(
        &env,
        issuer_id,
        Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response, Err(e) if format!("{e:?}").contains("unauthorized principal"));
}

#[test]
fn should_fail_prepare_credential_for_wrong_sender() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();

    let response = api::prepare_credential(
        &env,
        issuer_id,
        principal_1(), // not the same as contained in signed_id_alias
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias,
        },
    )
    .expect("API call failed");
    assert_matches!(response,
        Err(IssueCredentialError::InvalidIdAlias(e)) if e.contains("id alias could not be verified")
    );
}

#[test]
fn should_fail_get_credential_for_wrong_sender() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let signed_id_alias = DUMMY_SIGNED_ID_ALIAS.clone();
    let authorized_principal = Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap();
    api::add_employee(&env, issuer_id, authorized_principal).expect("failed to add employee");
    let unauthorized_principal = test_principal(2);

    let prepare_credential_response = api::prepare_credential(
        &env,
        issuer_id,
        authorized_principal,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: signed_id_alias.clone(),
        },
    )
    .expect("API call failed")
    .expect("failed to prepare credential");

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
        Err(IssueCredentialError::InvalidIdAlias(e)) if e.contains("id alias could not be verified")
    );
}

#[test]
fn should_fail_prepare_credential_for_anonymous_caller() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let response = api::prepare_credential(
        &env,
        issuer_id,
        Principal::anonymous(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response,
        Err(IssueCredentialError::InvalidIdAlias(e)) if e.contains("id alias could not be verified")
    );
}

#[test]
fn should_fail_prepare_credential_for_wrong_root_key() {
    let env = env();
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: Some(IC_ROOT_PK_DER.to_vec()), // does not match the DUMMY_ROOT_KEY, which is used in DUMMY_ALIAS_JWS
            idp_canister_ids: vec![Principal::from_text(DUMMY_II_CANISTER_ID).unwrap()],
            derivation_origin: DUMMY_DERIVATION_ORIGIN.to_string(),
            frontend_hostname: DUMMY_FRONTEND_HOSTNAME.to_string(),
        },
    );
    let response = api::prepare_credential(
        &env,
        issuer_id,
        Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response, Err(IssueCredentialError::InvalidIdAlias(_)));
}

#[test]
fn should_fail_prepare_credential_for_wrong_idp_canister_id() {
    let env = env();
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: Some(hex::decode(DUMMY_ROOT_KEY).unwrap()),
            idp_canister_ids: vec![Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap()], // does not match the DUMMY_II_CANISTER_ID, which is used in DUMMY_ALIAS_JWS
            derivation_origin: DUMMY_DERIVATION_ORIGIN.to_string(),
            frontend_hostname: DUMMY_FRONTEND_HOSTNAME.to_string(),
        },
    );
    let response = api::prepare_credential(
        &env,
        issuer_id,
        Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap(),
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response, Err(IssueCredentialError::InvalidIdAlias(_)));
}

#[test]
fn should_prepare_employee_credential_for_authorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let authorized_principal = Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap();
    api::add_employee(&env, issuer_id, authorized_principal).expect("API call failed");
    let response = api::prepare_credential(
        &env,
        issuer_id,
        authorized_principal,
        &PrepareCredentialRequest {
            credential_spec: employee_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response, Ok(_));
}

#[test]
fn should_prepare_degree_credential_for_authorized_principal() {
    let env = env();
    let issuer_id = install_issuer(&env, &DUMMY_ISSUER_INIT);
    let authorized_principal = Principal::from_text(DUMMY_ALIAS_ID_DAPP_PRINCIPAL).unwrap();
    api::add_graduate(&env, issuer_id, authorized_principal).expect("API call failed");
    let response = api::prepare_credential(
        &env,
        issuer_id,
        authorized_principal,
        &PrepareCredentialRequest {
            credential_spec: degree_credential_spec(),
            signed_id_alias: DUMMY_SIGNED_ID_ALIAS.clone(),
        },
    )
    .expect("API call failed");
    assert_matches!(response, Ok(_));
}

/// Verifies that different credentials are being created including II interactions.
#[test]
fn should_issue_credential_e2e() -> Result<(), CallError> {
    let env = env();
    let ii_id = install_canister(&env, II_WASM.clone());
    let root_key = env.root_key().unwrap();
    let issuer_id = install_issuer(
        &env,
        &IssuerInit {
            ic_root_key_der: Some(root_key.to_vec()),
            idp_canister_ids: vec![ii_id],
            derivation_origin: DUMMY_DERIVATION_ORIGIN.to_string(),
            frontend_hostname: DUMMY_FRONTEND_HOSTNAME.to_string(),
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

    let prepared_id_alias =
        ii_api::prepare_id_alias(&env, ii_id, principal_1(), prepare_id_alias_req)?
            .expect("prepare id_alias failed");

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
    let id_alias_credentials = ii_api::get_id_alias(&env, ii_id, principal_1(), get_id_alias_req)?
        .expect("get id_alias failed");

    let root_pk_raw =
        extract_raw_root_pk_from_der(&root_key).expect("Failed decoding IC root key.");
    let alias_tuple = get_verified_id_alias_from_jws(
        &id_alias_credentials
            .issuer_id_alias_credential
            .credential_jws,
        &id_alias_credentials.issuer_id_alias_credential.id_dapp,
        &canister_sig_pk.canister_id,
        &root_pk_raw,
        time(&env) as u128,
    )
    .expect("Invalid ID alias");

    api::add_employee(&env, issuer_id, alias_tuple.id_dapp)?;
    api::add_graduate(&env, issuer_id, alias_tuple.id_dapp)?;
    api::add_adult(&env, issuer_id, alias_tuple.id_dapp)?;

    for credential_spec in [
        employee_credential_spec(),
        degree_credential_spec(),
        adult_credential_spec(),
    ] {
        let prepared_credential = api::prepare_credential(
            &env,
            issuer_id,
            id_alias_credentials.issuer_id_alias_credential.id_dapp,
            &PrepareCredentialRequest {
                credential_spec: credential_spec.clone(),
                signed_id_alias: SignedIssuerIdAlias {
                    credential_jws: id_alias_credentials
                        .issuer_id_alias_credential
                        .credential_jws
                        .clone(),
                },
            },
        )?
        .expect("failed to prepare credential");

        let get_credential_response = api::get_credential(
            &env,
            issuer_id,
            id_alias_credentials.issuer_id_alias_credential.id_dapp,
            &GetCredentialRequest {
                credential_spec: credential_spec.clone(),
                signed_id_alias: SignedIssuerIdAlias {
                    credential_jws: id_alias_credentials
                        .issuer_id_alias_credential
                        .credential_jws
                        .clone(),
                },
                prepared_context: prepared_credential.prepared_context,
            },
        )?;
        let claims = verify_credential_jws_with_canister_id(
            &get_credential_response.unwrap().vc_jws,
            &issuer_id,
            &root_pk_raw,
            time(&env) as u128,
        )
        .expect("credential verification failed");
        let vc_claims = claims
            .custom()
            .expect("missing custom claims")
            .as_object()
            .expect("malformed custom claims")
            .get("vc")
            .expect("missing vc claims")
            .as_object()
            .expect("malformed vc claims");
        validate_claims_match_spec(vc_claims, &credential_spec).expect("Clam validation failed");
    }

    Ok(())
}

#[test]
fn should_configure() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    api::configure(&env, issuer_id, &DUMMY_ISSUER_INIT).expect("API call failed");
}

#[test]
fn should_set_alternative_origins() {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let alternative_origins = r#"{"alternativeOrigins":["https://test.issuer"]}"#;
    let request = HttpRequest {
        method: "GET".to_string(),
        url: "/.well-known/ii-alternative-origins".to_string(),
        headers: vec![],
        body: ByteBuf::new(),
        certificate_version: Some(2),
    };

    let http_response = http_request(&env, issuer_id, &request).expect("HTTP request failed");
    assert_eq!(http_response.status_code, 404);

    api::set_alternative_origins(&env, issuer_id, alternative_origins).expect("API call failed");

    let http_response = http_request(&env, issuer_id, &request).expect("HTTP request failed");
    assert_eq!(http_response.status_code, 200);
    assert_eq!(&http_response.body, alternative_origins.as_bytes())
}

/// Verifies that the expected assets is delivered and certified.
#[test]
fn issuer_canister_serves_http_assets() -> Result<(), CallError> {
    fn verify_response_certification(
        env: &PocketIc,
        canister_id: CanisterId,
        request: HttpRequest,
        http_response: HttpResponse,
        min_certification_version: u16,
    ) -> VerificationInfo {
        verify_request_response_pair(
            ic_http_certification::HttpRequest {
                method: request.method,
                url: request.url,
                headers: request.headers,
                body: request.body.into_vec(),
            },
            ic_http_certification::HttpResponse {
                status_code: http_response.status_code,
                headers: http_response.headers,
                body: http_response.body.into_vec(),
                upgrade: None,
            },
            canister_id.as_slice(),
            time(env) as u128,
            Duration::from_secs(300).as_nanos(),
            &env.root_key().unwrap(),
            min_certification_version as u8,
        )
        .unwrap_or_else(|e| panic!("validation failed: {e}"))
    }

    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    // for each asset and certification version, fetch the asset, check the HTTP status code, headers and certificate.

    for certification_version in 1..=2 {
        let request = HttpRequest {
            method: "GET".to_string(),
            url: "/".to_string(),
            headers: vec![],
            body: ByteBuf::new(),
            certificate_version: Some(certification_version),
        };
        let http_response = http_request(&env, canister_id, &request)?;
        assert_eq!(http_response.status_code, 200);

        let result = verify_response_certification(
            &env,
            canister_id,
            request,
            http_response,
            certification_version,
        );
        assert_eq!(result.verification_version, certification_version);
    }

    Ok(())
}
