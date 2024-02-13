use candid::Principal;
use std::collections::HashMap;
use vc_util::issuer_api::{ArgumentValue, CredentialSpec};
use vc_util::VcFlowSigners;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen(js_name = validateVerifiedAdultPresentation)]
pub fn validate_verified_adult_presentation(
    vp_jwt: &[u8],
    vc_subject_principal: &[u8],
    ii_canister_id: &[u8],
    issuer_origin: &[u8],
    issuer_canister_id: &[u8],
    root_pk_raw: &[u8],
    current_time_ns: u64,
) -> Result<(), String> {
    let vp_jwt = String::from_utf8(vp_jwt.to_vec()).expect("wrong vp_jwt");
    let effective_vc_subject = Principal::from_slice(vc_subject_principal);
    let vc_flow_signers: VcFlowSigners = VcFlowSigners {
        ii_canister_id: Principal::from_slice(ii_canister_id),
        ii_origin: vc_util::II_ISSUER_URL.to_string(),
        issuer_canister_id: Principal::from_slice(issuer_canister_id),
        issuer_origin: String::from_utf8(issuer_origin.to_vec()).expect("wrong issuer origin"),
    };
    let credential_spec = {
        let mut args = HashMap::new();
        args.insert("minAge".to_string(), ArgumentValue::Int(18));
        CredentialSpec {
            credential_type: "VerifiedAdult".to_string(),
            arguments: Some(args),
        }
    };
    vc_util::validate_ii_presentation_and_claims(
        &vp_jwt,
        effective_vc_subject,
        &vc_flow_signers,
        &credential_spec,
        root_pk_raw,
        current_time_ns as u128,
    )
    .map_err(|e| format!("{:?}", e))
}
