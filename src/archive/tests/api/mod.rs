use canister_tests::framework;
use canister_tests::framework::CallError;
use ic_state_machine_tests::StateMachine;
use ic_types::{CanisterId, PrincipalId};
use internet_identity_interface as types;

pub fn add_entry(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    timestamp: types::Timestamp,
    entry: Vec<u8>,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "write_entry",
        (user_number, timestamp, entry),
    )
}

pub fn get_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    idx: Option<u64>,
    limit: Option<u16>,
) -> Result<types::Entries, CallError> {
    framework::call_candid_as(env, canister_id, sender, "get_entries", (idx, limit)).map(|(x,)| x)
}

pub fn get_user_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    cursor: Option<types::Cursor>,
    limit: Option<u16>,
) -> Result<types::UserEntries, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "get_user_entries",
        (user_number, cursor, limit),
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
