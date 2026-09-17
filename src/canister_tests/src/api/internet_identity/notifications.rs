//! The notification API, derived (manually) from Internet Identity's Candid file.
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, NotificationConsentedApp, NotificationError,
};
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid_as, query_candid_as, PocketIc, RejectResponse};

pub fn grant_consent(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<Result<(), NotificationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "notification_grant_consent",
        (anchor_number, origin),
    )
    .map(|(x,)| x)
}

pub fn revoke_consent(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<Result<(), NotificationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "notification_revoke_consent",
        (anchor_number, origin),
    )
    .map(|(x,)| x)
}

pub fn consent_status(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<bool, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "notification_consent_status",
        (anchor_number, origin),
    )
    .map(|(x,)| x)
}

pub fn consented_apps(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
) -> Result<Vec<NotificationConsentedApp>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "notification_consented_apps",
        (anchor_number,),
    )
    .map(|(x,)| x)
}

pub fn set_app_muted(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    muted: bool,
) -> Result<Result<(), NotificationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "notification_set_app_muted",
        (anchor_number, origin, muted),
    )
    .map(|(x,)| x)
}
