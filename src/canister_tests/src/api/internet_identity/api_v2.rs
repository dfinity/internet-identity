use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{call_candid_as, CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{IdentityInfoResponse, IdentityNumber};

pub fn get_identity_info(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
) -> Result<Option<IdentityInfoResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_info",
        (identity_number,),
    )
    .map(|(x,)| x)
}
