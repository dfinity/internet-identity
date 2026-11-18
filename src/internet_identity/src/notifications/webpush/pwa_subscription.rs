//! What an app's own installed web app registered for Web Push, held on the identity's
//! config for that app, one per browser entry, with the account it delivers for.
//!
//! A push endpoint names one installed app on one device, and only one identity and
//! account deliver through it at a time: the one whose session subscribed last. A
//! subscribe for an endpoint another browser entry holds takes it over only when the keys
//! it arrives with match that entry's, which only the device that subscribed knows:
//! knowing an endpoint is not enough to silence the device.

use super::subscription;
use super::validation::{deliverable_endpoint, PushEndpoint, SealedAuth, SubscriberKey};
use super::{seed_pool, transport};
use crate::authz_utils::AuthorizedSession;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{PendingTransport, ReadySubscription, WebPushSubscription};
use crate::storage::storable::anchor_application_config::PwaSubscription;
use crate::storage::PwaSubscriptionHolder;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    PrepareWebPushSubscriptionResponse, PwaWebPushSubscription, SetWebPushSubscriptionError,
    Timestamp, WebPushSubscriptionStatus,
};
use p256::elliptic_curve::subtle::ConstantTimeEq;
use serde_bytes::ByteBuf;

/// What an installed web app's subscribe arrives with: the subscription's endpoint and
/// `p256dh`, the transport key `sealed_auth` was encrypted to, and the sealed `auth`, each
/// checked for the shape its newtype asserts and the endpoint for being one II posts to.
pub struct ValidatedPwaWebPushSubscription {
    pub endpoint: PushEndpoint,
    pub subscriber_key: SubscriberKey,
    pub transport_key: SubscriberKey,
    pub sealed_auth: SealedAuth,
}

impl TryFrom<PwaWebPushSubscription> for ValidatedPwaWebPushSubscription {
    type Error = String;

    /// Every field at once rather than the first to fail, as `set_webpush_subscription`
    /// does, so a caller learns every problem from one answer.
    fn try_from(subscription: PwaWebPushSubscription) -> Result<Self, Self::Error> {
        let PwaWebPushSubscription {
            endpoint,
            p256dh,
            transport_key,
            sealed_auth,
        } = subscription;
        match (
            PushEndpoint::try_from(endpoint.as_str())
                .and_then(|endpoint| deliverable_endpoint(&endpoint).map(|()| endpoint)),
            SubscriberKey::try_from(p256dh.into_vec()),
            SubscriberKey::try_from(transport_key.into_vec()),
            SealedAuth::try_from(sealed_auth.into_vec()),
        ) {
            (Ok(endpoint), Ok(subscriber_key), Ok(transport_key), Ok(sealed_auth)) => Ok(Self {
                endpoint,
                subscriber_key,
                transport_key,
                sealed_auth,
            }),
            (endpoint, subscriber_key, transport_key, sealed_auth) => {
                let problems: Vec<String> = [
                    endpoint.err(),
                    subscriber_key.err(),
                    transport_key.err(),
                    sealed_auth.err(),
                ]
                .into_iter()
                .flatten()
                .collect();
                Err(problems.join("; "))
            }
        }
    }
}

/// Why the keys an installed web app subscribes with were not handed out.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PreparePwaSubscriptionError {
    /// The identity has not allowed this app to notify it.
    NotificationsNotAllowed,
    /// Nothing the caller can act on: a broken invariant or a storage failure, and why.
    Internal(String),
}

/// Why an installed web app's subscription was not set.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SetPwaSubscriptionError {
    /// The identity has not allowed this app to notify it.
    NotificationsNotAllowed,
    /// The transport key `sealed_auth` was encrypted to has expired, or a later prepare
    /// replaced it.
    TransportKeyExpired,
    /// Another subscription holds the endpoint with other keys or for another app.
    EndpointInUse,
    /// Nothing the caller can act on: a broken invariant or a storage failure, and why.
    Internal(String),
}

/// The keys an installed web app subscribes with, or `Pending` where the seed pool has
/// nothing to derive them from yet.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PreparedPwaSubscription {
    Keys(PrepareWebPushSubscriptionResponse),
    Pending,
}

/// What [`resolve_pwa_holder`] answers for a session whose app has no application number.
const NO_APPLICATION_NUMBER: &str = "the session's app has no application number";

/// Where `session` registers: its identity, its app and the browser entry it signed in
/// from. An error, logged, where the session's app has no application number, which an
/// app with a session always has.
pub fn resolve_pwa_holder(session: &AuthorizedSession) -> Result<PwaSubscriptionHolder, String> {
    let Some(application_number) =
        storage_borrow(|storage| storage.lookup_application_number_with_origin(&session.origin))
    else {
        ic_cdk::println!("ERROR: {NO_APPLICATION_NUMBER}");
        return Err(NO_APPLICATION_NUMBER.to_string());
    };
    Ok(PwaSubscriptionHolder {
        anchor_number: session.anchor_number,
        application_number,
        browser_id: session.browser_id,
    })
}

/// Records the key a fetch just minted for `holder`'s browser entry, for the account
/// `account_principal` names. This replaces what the entry held, so it delivers nothing
/// and holds no endpoint until the subscribe that spends the key lands.
fn start_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    secret_key: [u8; 32],
    sender_seed: [u8; 32],
    now_ns: Timestamp,
) -> Result<(), PreparePwaSubscriptionError> {
    if !is_allowed(holder) {
        return Err(PreparePwaSubscriptionError::NotificationsNotAllowed);
    }
    let pending_transport = PendingTransport {
        secret_key,
        sender_seed,
        minted_at_ns: now_ns,
    };
    let subscription = PwaSubscription {
        subscription: WebPushSubscription::Pending(pending_transport),
        account_principal,
    };
    write(holder, Some(subscription), None).map_err(PreparePwaSubscriptionError::Internal)
}

/// Registers `holder`'s browser entry for Web Push for the account `account_principal`
/// names, replacing what it delivered for, or with `None` empties it where it delivers for
/// that account.
///
/// Where another browser entry holds the endpoint, that entry is emptied first, and only
/// when it is the same app's and holds the same `p256dh` and `auth`: a device switching
/// identity in its installed app subscribes with the keys it already has.
pub fn set_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    request: Option<ValidatedPwaWebPushSubscription>,
    now_ns: Timestamp,
) -> Result<(), SetPwaSubscriptionError> {
    let Some(request) = request else {
        return remove_pwa_subscription(holder, account_principal);
    };
    if !is_allowed(holder) {
        return Err(SetPwaSubscriptionError::NotificationsNotAllowed);
    }
    let Some(pending_transport) = read(holder)
        .and_then(|held_subscription| held_subscription.subscription.pending().cloned())
    else {
        return Err(SetPwaSubscriptionError::TransportKeyExpired);
    };
    let ValidatedPwaWebPushSubscription {
        endpoint,
        subscriber_key,
        transport_key,
        sealed_auth,
    } = request;
    let ready_subscription = subscription::spend_pending(
        &pending_transport,
        &transport_key,
        &sealed_auth,
        subscriber_key,
        endpoint,
        now_ns,
    )
    .inspect_err(|err| {
        if matches!(err, SetWebPushSubscriptionError::InvalidBrowserKey) {
            ic_cdk::println!("ERROR: spending a transport key refused a browser key");
        }
    })
    .map_err(map_set_error)?;
    let displaced_holder = find_endpoint_holder(holder, &ready_subscription.endpoint);
    if let Some(endpoint_holder) = displaced_holder {
        check_takeover(holder, endpoint_holder, &ready_subscription)?;
    }

    let subscription = WebPushSubscription::Ready(ready_subscription);
    let subscription = PwaSubscription {
        subscription,
        account_principal,
    };
    write(holder, Some(subscription), displaced_holder).map_err(SetPwaSubscriptionError::Internal)
}

/// Empties `holder`'s browser entry where it delivers for `account_principal`'s account.
/// One that holds nothing, or delivers for another account, is left as it is.
fn remove_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
) -> Result<(), SetPwaSubscriptionError> {
    match read(holder) {
        Some(held_subscription) if held_subscription.account_principal != account_principal => {
            return Ok(())
        }
        Some(_) => {}
        None => return Ok(()),
    }
    write(holder, None, None).map_err(SetPwaSubscriptionError::Internal)
}

/// The other slot that holds `endpoint` now, which a subscription for `holder` at that
/// endpoint takes it over from. `None` where no other slot holds it: the lookup is derived
/// from the configs, so a slot it still names that no longer holds the endpoint holds
/// nothing to take over.
fn find_endpoint_holder(
    holder: PwaSubscriptionHolder,
    endpoint: &PushEndpoint,
) -> Option<PwaSubscriptionHolder> {
    let indexed_holder = storage_borrow(|storage| {
        storage.lookup_pwa_webpush_subscription_with_endpoint(endpoint.as_str())
    })?;
    if indexed_holder == holder {
        return None;
    }
    let held_subscription = read(indexed_holder)?;
    let ready_subscription = held_subscription.subscription.into_ready()?;
    if ready_subscription.endpoint != *endpoint {
        return None;
    }
    Some(indexed_holder)
}

/// Whether `holder` may take `ready_subscription`'s endpoint over from `endpoint_holder`:
/// only the same app may, with the same keys, which is the same app on the same device
/// subscribing again. Refused with `EndpointInUse` otherwise.
fn check_takeover(
    holder: PwaSubscriptionHolder,
    endpoint_holder: PwaSubscriptionHolder,
    ready_subscription: &ReadySubscription,
) -> Result<(), SetPwaSubscriptionError> {
    let Some(held_subscription) = read(endpoint_holder)
        .and_then(|held_subscription| held_subscription.subscription.into_ready())
    else {
        return Ok(());
    };
    let is_same_application = endpoint_holder.application_number == holder.application_number;
    let is_same_subscriber_key = bool::from(
        held_subscription
            .subscriber_key
            .as_slice()
            .ct_eq(ready_subscription.subscriber_key.as_slice()),
    );
    let is_same_auth = bool::from(
        held_subscription
            .auth
            .as_slice()
            .ct_eq(ready_subscription.auth.as_slice()),
    );
    if !(is_same_application && is_same_subscriber_key && is_same_auth) {
        return Err(SetPwaSubscriptionError::EndpointInUse);
    }
    Ok(())
}

/// The subscription a relay answered gone for, dropped where `holder`'s browser entry
/// still holds that endpoint.
pub(crate) fn clear_gone_pwa_subscription(
    holder: PwaSubscriptionHolder,
    endpoint: &str,
) -> Result<(), String> {
    let Some(held_subscription) = read(holder) else {
        return Ok(());
    };
    let Some(registered_subscription) = held_subscription.subscription.into_ready() else {
        return Ok(());
    };
    if registered_subscription.endpoint.as_str() != endpoint {
        return Ok(());
    }
    write(holder, None, None)
}

/// The endpoint `holder`'s browser entry is subscribed at, where it is ready and delivers
/// for `account_principal`'s account.
pub fn read_pwa_subscription_status(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    now_ns: Timestamp,
) -> Option<WebPushSubscriptionStatus> {
    let held_subscription = read(holder)?;
    if held_subscription.account_principal != account_principal {
        return None;
    }
    let ready_subscription = held_subscription.subscription.into_ready()?;
    Some(subscription::status_of(&ready_subscription, now_ns))
}

/// Whether `holder`'s browser entry holds a subscription that delivers for the account
/// `account_principal` names.
pub fn delivers_for(holder: PwaSubscriptionHolder, account_principal: Principal) -> bool {
    read(holder).is_some_and(|held| held.account_principal == account_principal)
}

/// What `holder`'s browser entry holds, and the account it delivers for.
pub(crate) fn read(holder: PwaSubscriptionHolder) -> Option<PwaSubscription> {
    storage_borrow(|storage| {
        storage.anchor_application_config(holder.anchor_number, holder.application_number)
    })?
    .pwa_webpush_subscription(holder.browser_id)
}

/// Puts `subscription` on `holder`'s browser entry, or with `None` empties it, after
/// emptying `displaced_holder`'s where one is given.
pub(crate) fn write(
    holder: PwaSubscriptionHolder,
    subscription: Option<PwaSubscription>,
    displaced_holder: Option<PwaSubscriptionHolder>,
) -> Result<(), String> {
    storage_borrow_mut(|storage| {
        storage.write_pwa_webpush_subscription(holder, subscription, displaced_holder)
    })
    .map_err(|err| format!("{err}"))
}

fn is_allowed(holder: PwaSubscriptionHolder) -> bool {
    storage_borrow(|storage| {
        storage.anchor_application_config(holder.anchor_number, holder.application_number)
    })
    .is_some_and(|config| config.notifications_allowed())
}

/// What spending the transport key answers, in this module's terms. Spending names no
/// browser key, so a refusal of one is a broken invariant.
fn map_set_error(err: SetWebPushSubscriptionError) -> SetPwaSubscriptionError {
    match err {
        SetWebPushSubscriptionError::TransportKeyExpired => {
            SetPwaSubscriptionError::TransportKeyExpired
        }
        SetWebPushSubscriptionError::InternalCanisterError(err) => {
            SetPwaSubscriptionError::Internal(err)
        }
        SetWebPushSubscriptionError::InvalidBrowserKey => {
            SetPwaSubscriptionError::Internal("a browser key was refused".to_string())
        }
    }
}

/// The keys an installed web app subscribes `session`'s browser entry with, given the
/// held `vapid_public_key`. `Pending` where the seed pool is cold.
pub fn prepare_pwa_subscription(
    session: &AuthorizedSession,
    vapid_public_key: Vec<u8>,
    now_ns: Timestamp,
) -> Result<PreparedPwaSubscription, PreparePwaSubscriptionError> {
    let holder = resolve_pwa_holder(session).map_err(PreparePwaSubscriptionError::Internal)?;
    // Before the pool is asked, so an app without consent is told so rather than to come
    // back.
    if !is_allowed(holder) {
        return Err(PreparePwaSubscriptionError::NotificationsNotAllowed);
    }
    let Some(seeds) = seed_pool::derive_seeds_for_pwa(holder, now_ns) else {
        return Ok(PreparedPwaSubscription::Pending);
    };
    let transport_public_key = transport::public_key_of(&seeds.transport_secret);
    start_pwa_subscription(
        holder,
        session.account_principal,
        seeds.transport_secret,
        seeds.sender_seed,
        now_ns,
    )?;
    let vapid_public_key = ByteBuf::from(vapid_public_key);
    let transport_public_key = ByteBuf::from(transport_public_key);
    Ok(PreparedPwaSubscription::Keys(
        PrepareWebPushSubscriptionResponse {
            vapid_public_key,
            transport_public_key,
        },
    ))
}
