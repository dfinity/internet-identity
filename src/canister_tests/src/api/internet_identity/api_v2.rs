use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodAddResponse, AuthnMethodData, AuthnMethodRemoveResponse, IdentityInfoResponse,
    IdentityMetadataReplaceResponse, IdentityNumber, MetadataEntry, PublicKey,
};
use pocket_ic::{call_candid_as, CallError, PocketIc};
use std::collections::HashMap;

pub fn identity_info(
    env: &PocketIc,
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

pub fn identity_metadata_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    metadata: &HashMap<String, MetadataEntry>,
) -> Result<Option<IdentityMetadataReplaceResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "identity_metadata_replace",
        (identity_number, metadata),
    )
    .map(|(x,)| x)
}

pub fn authn_method_add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    authn_method: &AuthnMethodData,
) -> Result<Option<AuthnMethodAddResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "authn_method_add",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
) -> Result<Option<AuthnMethodRemoveResponse>, CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "authn_method_remove",
        (identity_number, public_key),
    )
    .map(|(x,)| x)
}
