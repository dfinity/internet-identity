use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::{HttpRequest, HttpResponse};
use state_machine_client::{query_candid, CallError, StateMachine};

pub mod archive;
pub mod internet_identity;

// api methods common to all canisters

pub fn http_request(
    env: &StateMachine,
    canister_id: CanisterId,
    http_request: HttpRequest,
) -> Result<HttpResponse, CallError> {
    query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}
