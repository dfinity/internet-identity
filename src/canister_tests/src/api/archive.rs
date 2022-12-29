use candid::Principal;
use ic_cdk::api::management_canister::main::{CanisterId, CanisterStatusResponse};
use internet_identity_interface::archive::*;
use internet_identity_interface::*;
use state_machine_client::{call_candid, call_candid_as, query_candid, CallError, StateMachine};

pub fn add_entry(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    anchor: AnchorNumber,
    timestamp: Timestamp,
    entry: Vec<u8>,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "write_entry",
        (anchor, timestamp, entry),
    )
}

pub fn get_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    idx: Option<u64>,
    limit: Option<u16>,
) -> Result<Entries, CallError> {
    query_candid(env, canister_id, "get_entries", (idx, limit)).map(|(x,)| x)
}

pub fn get_anchor_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    cursor: Option<Cursor>,
    limit: Option<u16>,
) -> Result<AnchorEntries, CallError> {
    query_candid(
        env,
        canister_id,
        "get_anchor_entries",
        (anchor, cursor, limit),
    )
    .map(|(x,)| x)
}

pub fn status(
    env: &StateMachine,
    canister_id: CanisterId,
) -> Result<CanisterStatusResponse, CallError> {
    call_candid(env, canister_id, "status", ()).map(|(x,)| x)
}

pub fn http_request(
    env: &StateMachine,
    canister_id: CanisterId,
    http_request: HttpRequest,
) -> Result<HttpResponse, CallError> {
    query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}
