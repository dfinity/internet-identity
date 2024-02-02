use candid::Principal;
use canister_sig_util::extract_raw_root_pk_from_der;
use std::collections::HashMap;
use vc_util::issuer_api::ArgumentValue;
use vc_util::{CredentialSpec, VcFlowSigners};
use wasm_bindgen::prelude::wasm_bindgen;

// TODO: clarify encoding of all those &[u8]
#[wasm_bindgen(js_name = validateVerifiedPresentation)]
pub fn validate_verified_presentation(
    _credential_type: String, /* TODO: USEME */
    vp_jwt: &[u8],
    vc_subject_principal: &[u8],
    ii_canister_id: &[u8],
    _issuer_origin: &[u8],
    issuer_canister_id: &[u8],
    root_pk_raw: &[u8],
    current_time_ns: u64,
) -> Result<(), String> {
    let credential_type = "VerifiedEmployee".to_string();
    // TODO; proper args
    // XXX: right now, somehow the employerName is nested in the issued jwt
    let arguments = None; // Some(HashMap::from([("employerName".to_string(), ArgumentValue::String("DFINITY Foundation".to_string()))]));
    let credential_spec = CredentialSpec {
        credential_type,
        arguments,
    };
    let vp_jwt = String::from_utf8(vp_jwt.to_vec()).expect("wrong vp_jwt");
    // TODO: panic does not work (unreachable)
    // TODO: why does Principal::from_slice not work? what is expected encoding?
    let effective_vc_subject =
        Principal::from_text(&String::from_utf8(vc_subject_principal.to_vec()).unwrap()).unwrap();
    let ii_canister_id =
        Principal::from_text(&String::from_utf8(ii_canister_id.to_vec()).unwrap()).unwrap();
    let issuer_canister_id =
        Principal::from_text(&String::from_utf8(issuer_canister_id.to_vec()).unwrap()).unwrap();
    let root_pk_raw = extract_raw_root_pk_from_der(root_pk_raw).unwrap();
    let vc_flow_signers: VcFlowSigners = VcFlowSigners {
        ii_canister_id,
        ii_origin: vc_util::II_ISSUER_URL.to_string(),
        issuer_canister_id,
        issuer_origin: "https://employment.info/".to_string(),
        // TODO: what about the actual origin?
        // issuer_origin: String::from_utf8(issuer_origin.to_vec()).expect("wrong issuer origin"),
    };
    let res = vc_util::custom::validate_verified_presentation(
        &credential_spec,
        &vp_jwt,
        effective_vc_subject,
        &vc_flow_signers,
        &root_pk_raw,
        current_time_ns as u128,
    )
    .map_err(|e| format!("{:?}", e));
    res
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
