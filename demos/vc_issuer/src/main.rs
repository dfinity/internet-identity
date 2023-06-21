use candid::candid_method;
use ic_cdk_macros::update;
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    IssueCredentialRequest, IssueCredentialResponse,
};

#[update]
#[candid_method]
async fn credential_request(_req: IssueCredentialRequest) -> IssueCredentialResponse {
    todo!("Not implemented yet")
}

fn main() {}

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
