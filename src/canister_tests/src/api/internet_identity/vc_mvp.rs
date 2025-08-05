use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, GetIdAliasRequest, IdAliasCredentials, PrepareIdAliasError,
    PrepareIdAliasRequest, PreparedIdAlias,
};
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid_as, query_candid_as, PocketIc, RejectResponse};

pub fn prepare_id_alias(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    prepare_id_alias_req: PrepareIdAliasRequest,
) -> Result<Result<PreparedIdAlias, PrepareIdAliasError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_id_alias",
        (prepare_id_alias_req,),
    )
    .map(|(x,)| x)
}

pub fn get_id_alias(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    get_id_alias_req: GetIdAliasRequest,
) -> Result<Result<IdAliasCredentials, GetIdAliasError>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_id_alias",
        (get_id_alias_req,),
    )
    .map(|(x,)| x)
}
