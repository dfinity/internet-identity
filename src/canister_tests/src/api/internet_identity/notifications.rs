//! The notification API, derived (manually) from Internet Identity's Candid file.
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, FrontendHostname, GetNextNotificationArg, GetNextNotificationError,
    GetNextNotificationResponse, GetNotificationDelegationRequest,
    GetNotificationDelegationResponse, GetWebPushSubscriptionStatusRequest,
    NotificationConsentGrantedRequest, NotificationDelegationError, NotificationGrantConsentError,
    NotificationGrantConsentRequest, NotificationRevokeConsentError,
    NotificationRevokeConsentRequest, PrepareNotificationDelegationRequest,
    PrepareNotificationDelegationResponse, RemoveNotificationArg, RemoveNotificationError,
    RemoveNotificationResponse, RemoveWebPushSubscriptionError, RemoveWebPushSubscriptionRequest,
    SetWebPushSubscriptionError, SetWebPushSubscriptionRequest, WebPushSubscriptionStatus,
};
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid_as, query_candid_as, PocketIc, RejectResponse};

pub fn grant_consent(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<Result<(), NotificationGrantConsentError>, RejectResponse> {
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
) -> Result<Result<(), NotificationRevokeConsentError>, RejectResponse> {
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

pub fn consent_granted(
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
        "notification_consent_granted",
        (NotificationConsentGrantedRequest {
            anchor_number,
            origin,
        },),
    )
    .map(|(x,)| x)
}

pub fn set_webpush_subscription(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: SetWebPushSubscriptionRequest,
) -> Result<Result<(), SetWebPushSubscriptionError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "set_webpush_subscription",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn remove_webpush_subscription(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
) -> Result<Result<(), RemoveWebPushSubscriptionError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "remove_webpush_subscription",
        (RemoveWebPushSubscriptionRequest {
            anchor_number,
            browser_id,
        },),
    )
    .map(|(x,)| x)
}

pub fn get_webpush_subscription_status(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
) -> Result<Option<WebPushSubscriptionStatus>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_webpush_subscription_status",
        (GetWebPushSubscriptionStatusRequest { anchor_number },),
    )
    .map(|(x,)| x)
}

pub fn prepare_notification_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: PrepareNotificationDelegationRequest,
) -> Result<
    Result<PrepareNotificationDelegationResponse, NotificationDelegationError>,
    RejectResponse,
> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_notification_delegation",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn get_notification_delegation(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: GetNotificationDelegationRequest,
) -> Result<Result<GetNotificationDelegationResponse, NotificationDelegationError>, RejectResponse>
{
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_notification_delegation",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn browser_get_next_notification(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    arg: GetNextNotificationArg,
) -> Result<Result<GetNextNotificationResponse, GetNextNotificationError>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "browser_get_next_notification",
        (arg,),
    )
    .map(|(x,)| x)
}

pub fn browser_remove_notification(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    arg: RemoveNotificationArg,
) -> Result<Result<RemoveNotificationResponse, RemoveNotificationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "browser_remove_notification",
        (arg,),
    )
    .map(|(x,)| x)
}
