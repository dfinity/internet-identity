use candid::{candid_method, Principal};
use ic_cdk_macros::{query, update};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    CredentialData, IssueCredentialRequest, IssueCredentialResponse, ManifestRequest,
    ManifestResponse,
};
use ssi_dids::example::DIDExample;
use ssi_vc::{Contexts, Credential, LinkedDataProofOptions, OneOrMany, DEFAULT_CONTEXT, URI};
use std::ops::Add;
use std::str::FromStr;

#[update]
#[candid_method]
async fn issue_credential(req: IssueCredentialRequest) -> IssueCredentialResponse {
    let vc = new_credential(&req);
    let options = LinkedDataProofOptions {
        checks: None,
        created: None,
        ..Default::default()
    };
    match vc.generate_jwt(None, &options, &DIDExample).await {
        Ok(vc_jwt) => IssueCredentialResponse::Ok(CredentialData { vc_jwt }),
        Err(err) => IssueCredentialResponse::Err(err.to_string()),
    }
}

#[query]
#[candid_method]
async fn get_manifest(_req: ManifestRequest) -> ManifestResponse {
    ManifestResponse::Err("Not implemented yet".to_string())
}

fn main() {}

fn did_from_principal(principal: &Principal) -> String {
    let prefix = String::from("did:web:");
    prefix.add(&*principal.to_string())
}

fn new_credential(req: &IssueCredentialRequest) -> Credential {
    let context = Contexts::One(ssi_vc::Context::URI(
        DEFAULT_CONTEXT.parse().expect("bad URI"),
    ));
    let type_ = OneOrMany::One("some type".to_string());
    let credential_subject = OneOrMany::One(ssi_vc::CredentialSubject {
        id: Some(
            URI::from_str(&*did_from_principal(&req.signed_id_alias.id_alias)).expect("bad Id"),
        ),
        property_set: None,
    });
    Credential {
        context,
        id: None,
        type_,
        credential_subject,
        issuer: None,
        issuance_date: None,
        proof: None,
        expiration_date: None,
        credential_status: None,
        terms_of_use: None,
        evidence: None,
        credential_schema: None,
        refresh_service: None,
        property_set: None,
    }
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
