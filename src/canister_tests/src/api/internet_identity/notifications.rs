//! The notification API, derived (manually) from Internet Identity's Candid file.
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ConsentStatusRequest, FrontendHostname, NotificationError,
    NotificationGrantConsentRequest, NotificationRevokeConsentRequest,
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
        (NotificationGrantConsentRequest {
            anchor_number,
            origin,
        },),
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
        (NotificationRevokeConsentRequest {
            anchor_number,
            origin,
        },),
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
        (ConsentStatusRequest {
            anchor_number,
            origin,
        },),
    )
    .map(|(x,)| x)
}
