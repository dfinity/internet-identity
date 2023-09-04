use candid::{candid_method, Principal};
use ic_cdk_macros::{query, update};
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ConsentInfo, ConsentMessageRequest, ConsentMessageResponse, GetCredentialRequest,
    GetCredentialResponse, IssuedCredentialData, PrepareCredentialRequest,
    PrepareCredentialResponse, PreparedCredentialData,
};
use serde_json::json;
use std::fmt::Error;
use std::ops::Add;

#[update]
#[candid_method]
async fn prepare_credential(req: PrepareCredentialRequest) -> PrepareCredentialResponse {
    if let Err(err) = verify_prepare_credential_request(&req) {
        return PrepareCredentialResponse::Err(err.to_string());
    }
    let vc = new_credential(&req.signed_id_alias.id_alias);
    // TODO: convert to JWT.
    let vc_jwt = serde_json::to_string(&vc).unwrap();
    PrepareCredentialResponse::Ok(PreparedCredentialData { vc_jwt })
}

#[query]
#[candid_method(query)]
fn get_credential(req: GetCredentialRequest) -> GetCredentialResponse {
    if let Err(err) = verify_get_credential_request(&req) {
        return GetCredentialResponse::Err(err.to_string());
    }
    // TODO: reuse the JWT from the request, and sign it.
    let vc = new_credential(&req.signed_id_alias.id_alias);
    let vc_jws = serde_json::to_string(&vc).unwrap();
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[query]
#[candid_method]
async fn consent_message(req: ConsentMessageRequest) -> ConsentMessageResponse {
    ConsentMessageResponse::Valid(ConsentInfo {
        consent_message: "Do you want to get a verifiable credential?".to_string(),
        language: req.preferences.language,
    })
}

fn main() {}

fn did_from_principal(principal: &Principal) -> String {
    let prefix = String::from("did:web:");
    prefix.add(&*principal.to_string())
}

fn new_credential(principal: &Principal) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_from_principal(principal),
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
