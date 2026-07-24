use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use pocket_ic::{query_candid, update_candid, PocketIc, RejectResponse};

pub mod archive;
pub mod internet_identity;

// api methods common to all canisters

pub fn http_request(
    env: &PocketIc,
    canister_id: CanisterId,
    http_request: &HttpRequest,
) -> Result<HttpResponse, RejectResponse> {
    query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}

/// Call `http_request_update` directly, like the HTTP gateway does after
/// `http_request` responds with `upgrade = Some(true)`.
pub fn http_request_update(
    env: &PocketIc,
    canister_id: CanisterId,
    http_request: &HttpRequest,
) -> Result<HttpResponse, RejectResponse> {
    update_candid(env, canister_id, "http_request_update", (http_request,)).map(|(x,)| x)
}
