use candid::{candid_method, Principal};
use ic_cdk_macros::{query, update};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ConsentData, CredentialData, IssueCredentialRequest, IssueCredentialResponse, ManifestData,
    ManifestRequest, ManifestResponse,
};
use serde::{Deserialize, Serialize};
use std::fmt::Error;
use std::ops::Add;

// partial Credential, to be replaced by a ssi_vc::Credential
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Credential {
    #[serde(rename = "@context")]
    pub context: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(rename = "type")]
    pub type_: String,
    pub credential_subject: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub issuer: Option<String>,
}

#[update]
#[candid_method]
async fn issue_credential(req: IssueCredentialRequest) -> IssueCredentialResponse {
    if let Err(err) = verify_credential_request(&req) {
        return IssueCredentialResponse::Err(err.to_string());
    }
    let vc = new_credential(&req);
    let vc_jwt = serde_json::to_string(&vc).unwrap();
    IssueCredentialResponse::Ok(CredentialData { vc_jwt })
}

#[query]
#[candid_method]
async fn get_manifest(req: ManifestRequest) -> ManifestResponse {
    ManifestResponse::Ok(ManifestData {
        consent_info: ConsentData {
            consent_message: "Do you want to get a verifiable credential?".to_string(),
            language: req.consent_message_request.preferences.language,
        },
    })
}

fn main() {}

fn did_from_principal(principal: &Principal) -> String {
    let prefix = String::from("did:web:");
    prefix.add(&*principal.to_string())
}

fn new_credential(req: &IssueCredentialRequest) -> Credential {
    Credential {
        context: "test_context".to_string(),
        id: Some("did:example:76e12ec712ebc6f1c221ebfeb1f".to_string()),
        type_: "neuron owner".to_string(),
        credential_subject: did_from_principal(&req.signed_id_alias.id_alias),
        issuer: Some("Test IC issuer".to_string()),
    }
}

fn verify_credential_request(_req: &IssueCredentialRequest) -> Result<(), Error> {
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
