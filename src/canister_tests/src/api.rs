use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use pocket_ic::{query_candid, CallError, PocketIc};

pub mod archive;
pub mod internet_identity;

// api methods common to all canisters

pub fn http_request(
    env: &PocketIc,
    canister_id: CanisterId,
    http_request: &HttpRequest,
) -> Result<HttpResponse, CallError> {
    query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}
