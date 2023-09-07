use candid::{candid_method, Principal};
use canister_sig_util::{
    der_encoded_canister_sig_pk, did_for_principal, hash_bytes, vc_jwt_to_jws, vc_signing_input,
    vc_signing_input_hash,
};
use ic_cdk_macros::{query, update};
use ic_certified_map::Hash;
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    GetCredentialRequest, GetCredentialResponse, Icrc21ConsentInfo, Icrc21ConsentMessageRequest,
    Icrc21ConsentMessageResponse, IssuedCredentialData, PrepareCredentialRequest,
    PrepareCredentialResponse, PreparedCredentialData,
};
use serde_json::json;
use std::fmt::Error;

#[update]
#[candid_method]
async fn prepare_credential(req: PrepareCredentialRequest) -> PrepareCredentialResponse {
    if let Err(err) = verify_prepare_credential_request(&req) {
        return PrepareCredentialResponse::Err(err.to_string());
    }
    let user = req.signed_id_alias.id_alias;
    let seed = calculate_seed(&user);
    let canister_id = ic_cdk::id();
    let canister_sig_pk_der = der_encoded_canister_sig_pk(canister_id, &seed);
    let credential = new_credential(&user);
    let credential_jwt = credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure");
    let signing_input = vc_signing_input(&credential_jwt, &canister_sig_pk_der, canister_id);
    let _msg_hash = vc_signing_input_hash(&signing_input);
    // TODO: add the (seed, msg_hash) to the canister sigs in certified data.
    PrepareCredentialResponse::Ok(PreparedCredentialData {
        vc_jwt: credential_jwt,
    })
}

#[query]
#[candid_method(query)]
fn get_credential(req: GetCredentialRequest) -> GetCredentialResponse {
    if let Err(err) = verify_get_credential_request(&req) {
        return GetCredentialResponse::Err(err.to_string());
    }
    // TODO: retrieve the actual signature and add it to the JWS.
    let user = req.signed_id_alias.id_dapp;
    let seed = calculate_seed(&user);
    let canister_id = ic_cdk::id();
    let canister_sig_pk_der = der_encoded_canister_sig_pk(canister_id, &seed);
    let sig = b"dummy sig";
    let vc_jws = vc_jwt_to_jws(&req.vc_jwt, &canister_sig_pk_der, sig, canister_id);
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[update]
#[candid_method]
async fn consent_message(req: Icrc21ConsentMessageRequest) -> Icrc21ConsentMessageResponse {
    Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
        consent_message: "Do you want to get a verifiable credential?".to_string(),
        language: req.preferences.language,
    })
}

fn main() {}

fn calculate_seed(principal: &Principal) -> Hash {
    let dummy_salt = [5u8; 32];

    let mut bytes: Vec<u8> = vec![];
    bytes.push(dummy_salt.len() as u8);
    bytes.extend_from_slice(&dummy_salt);

    let principal_bytes = principal.as_slice();
    bytes.push(principal_bytes.len() as u8);
    bytes.extend(principal_bytes);
    hash_bytes(bytes)
}

fn new_credential(principal: &Principal) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_for_principal(principal.clone()),
      "name": "Alice",
      "degree": {
        "type": "BachelorDegree",
        "name": "Bachelor of Science and Arts",
      },
      "GPA": "4.0",
    }))
    .unwrap();

    // Build credential using subject above and issuer.
    CredentialBuilder::default()
        .id(Url::parse("https://example.edu/credentials/3732").unwrap())
        .issuer(Url::parse("https://identity.ic0.app").unwrap())
        .type_("UniversityDegreeCredential")
        .subject(subject)
        .build()
        .unwrap()
}

fn verify_prepare_credential_request(_req: &PrepareCredentialRequest) -> Result<(), Error> {
    Ok(())
}

fn verify_get_credential_request(_req: &GetCredentialRequest) -> Result<(), Error> {
    Ok(())
}

// Order dependent: do not move above any function annotated with #[candid_method]!
candid::export_service!();

#[cfg(test)]
mod test {
    use crate::__export_service;

    #[test]
    fn print_candid_interface() {
        let canister_interface = __export_service();
        println!("{}", canister_interface);
    }
}
