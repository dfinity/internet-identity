//! Tests related to issue_credential canister call.

use canister_tests::framework::{env, get_wasm_path, principal_1};
use ic_cdk::api::management_canister::provisional::CanisterId;
use ic_cdk::export::Principal;
use ic_test_state_machine_client::{query_candid_as, CallError, StateMachine};
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
    use internet_identity_interface::internet_identity::types::vc_mvp::issuer::ManifestResponse;

    pub fn get_manifest(
        env: &StateMachine,
        canister_id: CanisterId,
        sender: Principal,
    ) -> Result<Option<ManifestResponse>, CallError> {
        query_candid_as(env, canister_id, sender, "get_manifest", ()).map(|(x,)| x)
    }
}

/// Verifies that a credential is being created.
#[test]
#[ignore]
fn should_issue_credential() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_canister(&env, VC_ISSUER_WASM.clone());

    let _manifest_response =
        api::get_manifest(&env, canister_id, principal_1())?.expect("Got 'None' from get_manifest");
    Ok(())
}
