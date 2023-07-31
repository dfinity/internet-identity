//! Tests related to issue_credential canister call.

use canister_tests::api::internet_identity::vc_mvp as ii_api;
use canister_tests::flows;
use canister_tests::framework::{env, get_wasm_path, principal_1, II_WASM};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_cdk::export::Principal;
use ic_test_state_machine_client::call_candid_as;
use ic_test_state_machine_client::{query_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ConsentMessageRequest, ConsentPreferences, CredentialSpec, IssueCredentialRequest,
    IssueCredentialResponse, ManifestRequest, ManifestResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasResponse, IdAliasRequest, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use lazy_static::lazy_static;
use std::path::PathBuf;

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

    pub fn get_manifest(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        manifest_request: &ManifestRequest,
    ) -> Result<Option<ManifestResponse>, CallError> {
        query_candid_as(
            env,
            canister_id,
            sender,
            "get_manifest",
            (manifest_request,),
        )
        .map(|(x,)| x)
    }

    pub fn issue_credential(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
        credential_request: &IssueCredentialRequest,
    ) -> Result<IssueCredentialResponse, CallError> {
        call_candid_as(
            env,
            canister_id,
            sender,
            "issue_credential",
            (credential_request,),
        )
        .map(|(x,)| x)
    }
}

/// Verifies that the manifest can be requested.
#[test]
#[ignore]
fn should_issue_credential() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let manifest_request = ManifestRequest {
        consent_message_request: ConsentMessageRequest {
            preferences: ConsentPreferences {
                language: "en".to_string(),
            },
        },
    };

    let _manifest_response =
        api::get_manifest(&env, canister_id, principal_1(), &manifest_request)?
            .expect("Got 'None' from get_manifest");
    Ok(())
}

/// Verifies that a credential is being created including II interactions.
#[test]
#[ignore]
fn should_issue_credential_e2e() -> Result<(), CallError> {
    let env = env();
    let issuer_id = install_canister(&env, VC_ISSUER_WASM.clone());
    let ii_id = install_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, ii_id);

    let relying_party = FrontendHostname::from("https://some-dapp.com");
    let issuer = FrontendHostname::from("https://some-issuer.com");
    let id_alias_req = IdAliasRequest {
        identity_number,
        relying_party,
        issuer,
    };

    let prepare_response =
        ii_api::prepare_id_alias(&env, ii_id, principal_1(), id_alias_req.clone())?
            .expect("Got 'None' from prepare_id_alias");

    let _canister_sig_key = if let PrepareIdAliasResponse::Ok(key) = prepare_response {
        key
    } else {
        panic!("prepare id_alias failed")
    };

    let id_alias_credentials = match ii_api::get_id_alias(&env, ii_id, principal_1(), id_alias_req)?
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

    let _credential = api::issue_credential(
        &env,
        issuer_id,
        id_alias_credentials.issuer_id_alias_credential.id_dapp,
        &IssueCredentialRequest {
            credential_spec: CredentialSpec {
                info: "foo".to_string(),
            },
            signed_id_alias: id_alias_credentials.issuer_id_alias_credential,
        },
    )?;

    Ok(())
}
