//! The notification API, derived (manually) from Internet Identity's Candid file.
use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, AppNotificationConsentStatusError, AppNotificationConsentStatusResponse,
    AppRevokeNotificationConsentError, BrowserId, ClaimNotificationAppError,
    ClaimNotificationAppRequest, DisableNotificationAppError, DisableNotificationAppRequest,
    EnableNotificationAppError, EnableNotificationAppRequest, FrontendHostname,
    GetNotificationAppStatusRequest, GetNotificationDelegationRequest,
    GetNotificationDelegationResponse, GetNotificationError, GetNotificationRequest,
    GetNotificationResponse, GetWebPushSubscriptionStatusRequest, LinkNotificationAppError,
    LinkNotificationAppRequest, LinkNotificationAppResponse, NotificationAppAnchorsRequest,
    NotificationAppStatus, NotificationConsentGrantedRequest, NotificationDelegationError,
    NotificationGrantConsentError, NotificationGrantConsentRequest, NotificationId,
    NotificationRevokeConsentError, NotificationRevokeConsentRequest,
    PrepareNotificationAppLinkError, PrepareNotificationAppLinkRequest,
    PrepareNotificationDelegationRequest, PrepareNotificationDelegationResponse,
    PrepareWebPushSubscriptionResponse, PrepareWebPushSubscriptionResult,
    ProbeNotificationAppError, ProbeNotificationAppRequest, RemoveNotificationError,
    RemoveNotificationRequest, RemoveNotificationResponse, RemoveWebPushSubscriptionError,
    RemoveWebPushSubscriptionRequest, SetWebPushSubscriptionError, SetWebPushSubscriptionRequest,
    WebPushSubscriptionStatus,
};
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid_as, query_candid_as, PocketIc, RejectResponse};
use serde_bytes::ByteBuf;

pub fn prepare_notification_app_link(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: PrepareNotificationAppLinkRequest,
) -> Result<Result<(), PrepareNotificationAppLinkError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_notification_app_link",
        (request,),
    )
    .map(|(result,)| result)
}

pub fn link_notification_app(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: LinkNotificationAppRequest,
) -> Result<Result<LinkNotificationAppResponse, LinkNotificationAppError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "link_notification_app",
        (request,),
    )
    .map(|(x,)| x)
}

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

pub fn app_notification_consent_status(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
) -> Result<
    Result<AppNotificationConsentStatusResponse, AppNotificationConsentStatusError>,
    RejectResponse,
> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "app_notification_consent_status",
        (),
    )
    .map(|(x,)| x)
}

pub fn app_revoke_notification_consent(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
) -> Result<Result<(), AppRevokeNotificationConsentError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "app_revoke_notification_consent",
        (),
    )
    .map(|(x,)| x)
}

pub fn prepare_webpush_subscription(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
) -> Result<PrepareWebPushSubscriptionResult, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "prepare_webpush_subscription",
        (anchor_number,),
    )
    .map(|(x,)| x)
}

/// The subscription's `auth` secret in these tests. Any 16 bytes will do; what matters is
/// that the canister reads back what was sealed.
pub const TEST_AUTH_SECRET: [u8; 16] = [4u8; 16];

/// `auth` encrypted to `transport_public_key`, the way a browser does it: ECDH over P-256
/// against a pair the browser mints, HKDF binding both public keys, then AES-128-GCM.
///
/// Answers the browser's public key alongside the ciphertext, since the canister needs it
/// to run its half of the agreement.
pub fn seal_auth(transport_public_key: &[u8], auth: &[u8]) -> (ByteBuf, ByteBuf) {
    use aes_gcm::aead::{Aead, KeyInit};
    use aes_gcm::{Aes128Gcm, Nonce};
    use hkdf::Hkdf;
    use p256::ecdh::diffie_hellman;
    use p256::elliptic_curve::sec1::ToEncodedPoint;
    use sha2::Sha256;

    let browser = p256::SecretKey::from_bytes(&[13u8; 32].into()).expect("a scalar");
    let browser_public = browser
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec();
    let theirs = p256::PublicKey::from_sec1_bytes(transport_public_key).expect("a point");
    let shared = diffie_hellman(browser.to_nonzero_scalar(), theirs.as_affine());

    let mut info = Vec::new();
    info.extend_from_slice(b"II webpush transport\0");
    info.extend_from_slice(transport_public_key);
    info.extend_from_slice(&browser_public);

    let derived = Hkdf::<Sha256>::new(None, &shared.raw_secret_bytes()[..]);
    let mut key = [0u8; 16];
    let mut iv = [0u8; 12];
    derived
        .expand(&[info.as_slice(), b"key\0"].concat(), &mut key)
        .expect("the key");
    derived
        .expand(&[info.as_slice(), b"iv\0"].concat(), &mut iv)
        .expect("the nonce");
    let ciphertext = Aes128Gcm::new_from_slice(&key)
        .expect("a key")
        .encrypt(&Nonce::from(iv), auth)
        .expect("sealing");
    (ByteBuf::from(browser_public), ByteBuf::from(ciphertext))
}

/// The keys, asking again where the canister is still drawing the randomness it derives
/// them from.
///
/// A cold pool answers `TryAgain` and puts that draw in flight. The reply to it lands a
/// round or two later, so the asks before that are told to come back too. The browser
/// does the same.
/// Enough to outlast the rounds a `raw_rand` reply takes to come back.
const PREPARE_ATTEMPTS: usize = 5;

fn prepared_keys(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor_number: AnchorNumber,
) -> Option<PrepareWebPushSubscriptionResponse> {
    for _ in 0..PREPARE_ATTEMPTS {
        match prepare_webpush_subscription(env, canister_id, sender, anchor_number) {
            Ok(PrepareWebPushSubscriptionResult::Ok(keys)) => return Some(keys),
            Ok(PrepareWebPushSubscriptionResult::Pending) => continue,
            _ => return None,
        }
    }
    None
}

/// Subscribes the way a browser does: fetch the keys, encrypt `auth` to the transport key
/// the canister just minted for this caller, then send.
///
/// A key fetch that is refused leaves the request exactly as the caller built it, so a
/// test about `set_webpush_subscription` being refused still sees its own refusal rather
/// than one from the fetch.
pub fn set_webpush_subscription(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: SetWebPushSubscriptionRequest,
) -> Result<Result<(), SetWebPushSubscriptionError>, RejectResponse> {
    let mut request = request;
    if let Some(keys) = prepared_keys(env, canister_id, sender, request.anchor_number) {
        let (transport_key, sealed_auth) = seal_auth(&keys.transport_public_key, &TEST_AUTH_SECRET);
        request.transport_key = transport_key;
        request.sealed_auth = sealed_auth;
    }
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

pub fn browser_get_notification(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: GetNotificationRequest,
) -> Result<Result<GetNotificationResponse, GetNotificationError>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "browser_get_notification",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn browser_remove_notification(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: RemoveNotificationRequest,
) -> Result<Result<RemoveNotificationResponse, RemoveNotificationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "browser_remove_notification",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn notification_app_anchors(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: NotificationAppAnchorsRequest,
) -> Result<Vec<AnchorNumber>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "notification_app_anchors",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn claim_notification_app(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: ClaimNotificationAppRequest,
) -> Result<Result<(), ClaimNotificationAppError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "claim_notification_app",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn get_notification_app_status(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: GetNotificationAppStatusRequest,
) -> Result<NotificationAppStatus, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_notification_app_status",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn probe_notification_app(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: ProbeNotificationAppRequest,
) -> Result<Result<NotificationId, ProbeNotificationAppError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "probe_notification_app",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn enable_notification_app(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: EnableNotificationAppRequest,
) -> Result<Result<NotificationId, EnableNotificationAppError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "enable_notification_app",
        (request,),
    )
    .map(|(x,)| x)
}

pub fn disable_notification_app(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    request: DisableNotificationAppRequest,
) -> Result<Result<(), DisableNotificationAppError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "disable_notification_app",
        (request,),
    )
    .map(|(x,)| x)
}
