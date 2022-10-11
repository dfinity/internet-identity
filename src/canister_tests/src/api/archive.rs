use crate::framework;
use crate::framework::CallError;
use ic_state_machine_tests::{CanisterId, PrincipalId, StateMachine};
use internet_identity_interface as types;

pub fn add_entry(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    anchor: types::UserNumber,
    timestamp: types::Timestamp,
    entry: Vec<u8>,
) -> Result<(), CallError> {
    framework::call_candid_as(
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
) -> Result<types::Entries, CallError> {
    framework::query_candid(env, canister_id, "get_entries", (idx, limit)).map(|(x,)| x)
}

pub fn get_anchor_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    anchor: types::UserNumber,
    cursor: Option<types::Cursor>,
    limit: Option<u16>,
) -> Result<types::AnchorEntries, CallError> {
    framework::query_candid(
        env,
        canister_id,
        "get_anchor_entries",
        (anchor, cursor, limit),
    )
    .map(|(x,)| x)
}

pub fn http_request(
    env: &StateMachine,
    canister_id: CanisterId,
    http_request: types::HttpRequest,
) -> Result<types::HttpResponse, CallError> {
    framework::query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}
