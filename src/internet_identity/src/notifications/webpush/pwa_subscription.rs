//! What an app's own installed web app registered for Web Push, held on the identity's
//! config for that app, in the slot of the browser entry and account its session is at.
//!
//! A push endpoint names one installed app on one device, and only one identity and
//! account deliver through it at a time. A subscribe for an endpoint another slot holds
//! takes it over only when the keys it arrives with match that slot's, which only the
//! device that subscribed knows: knowing an endpoint is not enough to silence the device.

use super::subscription::spend_pending;
use super::validation::{PushEndpoint, SealedAuth, SubscriberKey};
use super::{seed_pool, transport};
use crate::authz_utils::AuthorizedSession;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{PendingTransport, ReadySubscription, WebPushSubscription};
use crate::storage::storable::anchor_application_config::PwaSubscriptionSlot;
use crate::storage::PwaSubscriptionHolder;
use internet_identity_interface::internet_identity::types::{
    AppPreparePwaWebPushSubscriptionError, AppPreparePwaWebPushSubscriptionResult,
    AppSetPwaWebPushSubscriptionError, PrepareWebPushSubscriptionResponse,
    SetWebPushSubscriptionError, Timestamp,
};
use p256::elliptic_curve::subtle::ConstantTimeEq;
use serde_bytes::ByteBuf;

/// The subscription a Web Push subscribe arrives with, its fields validated.
pub struct PwaSubscriptionRequest {
    pub endpoint: PushEndpoint,
    pub subscriber_key: SubscriberKey,
    pub transport_key: SubscriberKey,
    pub sealed_auth: SealedAuth,
}

/// The slot `session` registers in: its identity, its app, the browser entry it signed in
/// from and its account. `None` where the app has no application number, which an app
/// with a session always has.
pub fn holder_of(session: &AuthorizedSession) -> Option<PwaSubscriptionHolder> {
    let application_number =
        storage_borrow(|storage| storage.lookup_application_number_with_origin(&session.origin))?;
    Some(PwaSubscriptionHolder {
        anchor_number: session.anchor_number,
        application_number,
        slot: PwaSubscriptionSlot {
            browser_id: session.browser_id,
            account_number: session.account_number,
        },
    })
}

/// Records the key a fetch just minted in `holder`'s slot, replacing what the slot held
/// until the subscribe that spends it lands.
fn start_pwa_subscription(
    holder: PwaSubscriptionHolder,
    secret_key: [u8; 32],
    sender_seed: [u8; 32],
    now_ns: Timestamp,
) -> Result<(), AppPreparePwaWebPushSubscriptionError> {
    if !is_allowed(holder) {
        return Err(AppPreparePwaWebPushSubscriptionError::NotificationsNotAllowed);
    }
    let pending = WebPushSubscription::Pending(PendingTransport {
        secret_key,
        sender_seed,
        minted_at_ns: now_ns,
    });
    write(holder, Some(pending), None)
        .map_err(AppPreparePwaWebPushSubscriptionError::InternalCanisterError)
}

/// Registers `holder`'s slot for Web Push, or with `None` empties it.
///
/// Where another slot holds the endpoint, that slot is emptied first, and only when it is
/// the same app's and holds the same `p256dh` and `auth`: a device switching identity or
/// account in its installed app subscribes with the keys it already has.
pub fn set_pwa_subscription(
    holder: PwaSubscriptionHolder,
    request: Option<PwaSubscriptionRequest>,
    now_ns: Timestamp,
) -> Result<(), AppSetPwaWebPushSubscriptionError> {
    let Some(request) = request else {
        return remove_pwa_subscription(holder);
    };
    if !is_allowed(holder) {
        return Err(AppSetPwaWebPushSubscriptionError::NotificationsNotAllowed);
    }
    let Some(pending) = read(holder)
        .as_ref()
        .and_then(|registered| registered.pending().cloned())
    else {
        return Err(AppSetPwaWebPushSubscriptionError::TransportKeyExpired);
    };
    let PwaSubscriptionRequest {
        endpoint,
        subscriber_key,
        transport_key,
        sealed_auth,
    } = request;
    let ready = spend_pending(
        &pending,
        &transport_key,
        &sealed_auth,
        subscriber_key,
        endpoint,
        now_ns,
    )
    .map_err(set_error)?;
    let displaced = displaced_holder(holder, &ready)?;

    write(holder, Some(WebPushSubscription::Ready(ready)), displaced)
        .map_err(AppSetPwaWebPushSubscriptionError::InternalCanisterError)
}

/// Empties `holder`'s slot. A slot or config that holds nothing is already empty.
fn remove_pwa_subscription(
    holder: PwaSubscriptionHolder,
) -> Result<(), AppSetPwaWebPushSubscriptionError> {
    if read(holder).is_none() {
        return Ok(());
    }
    write(holder, None, None).map_err(AppSetPwaWebPushSubscriptionError::InternalCanisterError)
}

/// The other slot `ready`'s endpoint is taken over from, refusing where it is not the
/// same app's or holds other keys. `None` where no other slot holds it.
fn displaced_holder(
    holder: PwaSubscriptionHolder,
    ready: &ReadySubscription,
) -> Result<Option<PwaSubscriptionHolder>, AppSetPwaWebPushSubscriptionError> {
    let Some(indexed) = storage_borrow(|storage| {
        storage.lookup_pwa_webpush_subscription_with_endpoint(ready.endpoint.as_str())
    }) else {
        return Ok(None);
    };
    if indexed == holder {
        return Ok(None);
    }
    // The lookup is derived from the configs, so a slot that no longer holds the endpoint
    // holds nothing to protect.
    let Some(held) = read(indexed)
        .and_then(WebPushSubscription::into_ready)
        .filter(|held| held.endpoint == ready.endpoint)
    else {
        return Ok(None);
    };
    let is_same_application = indexed.application_number == holder.application_number;
    let is_same_subscriber_key = bool::from(
        held.subscriber_key
            .as_slice()
            .ct_eq(ready.subscriber_key.as_slice()),
    );
    let is_same_auth = bool::from(held.auth.as_slice().ct_eq(ready.auth.as_slice()));
    if !(is_same_application && is_same_subscriber_key && is_same_auth) {
        return Err(AppSetPwaWebPushSubscriptionError::EndpointInUse);
    }
    Ok(Some(indexed))
}

/// The subscription a relay answered gone for, dropped where `holder`'s slot still holds
/// that endpoint.
pub(crate) fn clear_gone_pwa_subscription(
    holder: PwaSubscriptionHolder,
    endpoint: &str,
) -> Result<(), String> {
    let is_registered = read(holder)
        .and_then(WebPushSubscription::into_ready)
        .is_some_and(|registered| registered.endpoint.as_str() == endpoint);
    if !is_registered {
        return Ok(());
    }
    write(holder, None, None)
}

/// What `holder`'s slot holds.
pub(crate) fn read(holder: PwaSubscriptionHolder) -> Option<WebPushSubscription> {
    storage_borrow(|storage| {
        storage.read_anchor_application_config_with_number(
            holder.anchor_number,
            holder.application_number,
        )
    })?
    .pwa_webpush_subscription(holder.slot)
}

/// Puts `subscription` in `holder`'s slot, or with `None` empties it, after emptying
/// `displaced`'s slot where one is given.
pub(crate) fn write(
    holder: PwaSubscriptionHolder,
    subscription: Option<WebPushSubscription>,
    displaced: Option<PwaSubscriptionHolder>,
) -> Result<(), String> {
    storage_borrow_mut(|storage| {
        storage.write_pwa_webpush_subscription(holder, subscription, displaced)
    })
    .map_err(|err| format!("{err}"))
}

fn is_allowed(holder: PwaSubscriptionHolder) -> bool {
    storage_borrow(|storage| {
        storage.read_anchor_application_config_with_number(
            holder.anchor_number,
            holder.application_number,
        )
    })
    .is_some_and(|config| config.notifications_allowed())
}

/// What spending the transport key answers, in the installed app's terms.
fn set_error(err: SetWebPushSubscriptionError) -> AppSetPwaWebPushSubscriptionError {
    match err {
        SetWebPushSubscriptionError::TransportKeyExpired => {
            AppSetPwaWebPushSubscriptionError::TransportKeyExpired
        }
        SetWebPushSubscriptionError::InternalCanisterError(err) => {
            AppSetPwaWebPushSubscriptionError::InternalCanisterError(err)
        }
        SetWebPushSubscriptionError::InvalidBrowserKey => {
            AppSetPwaWebPushSubscriptionError::NoSuchSession
        }
    }
}

/// The keys an installed web app subscribes `session`'s slot with, after the VAPID key
/// was fetched. `Pending` where the seed pool is cold.
pub fn prepare_pwa_subscription(
    session: &AuthorizedSession,
    vapid_public_key: Vec<u8>,
    now_ns: Timestamp,
) -> AppPreparePwaWebPushSubscriptionResult {
    let Some(holder) = holder_of(session) else {
        return AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::InternalCanisterError(
                "the session's app has no application number".to_string(),
            ),
        );
    };
    // Before the pool is asked, so an app without consent is told so rather than to come
    // back.
    if !is_allowed(holder) {
        return AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::NotificationsNotAllowed,
        );
    }
    let Some(seeds) = seed_pool::seeds_for_pwa(holder, now_ns) else {
        return AppPreparePwaWebPushSubscriptionResult::Pending;
    };
    let transport_public_key = transport::public_key_of(&seeds.transport_secret);
    if let Err(err) =
        start_pwa_subscription(holder, seeds.transport_secret, seeds.sender_seed, now_ns)
    {
        return AppPreparePwaWebPushSubscriptionResult::Err(err);
    }
    AppPreparePwaWebPushSubscriptionResult::Ok(PrepareWebPushSubscriptionResponse {
        vapid_public_key: ByteBuf::from(vapid_public_key),
        transport_public_key: ByteBuf::from(transport_public_key),
    })
}
