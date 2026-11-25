//! What an app's own installed web app registered for Web Push, held on the identity's
//! config for that app, one per browser entry and frontend, with the account it delivers
//! for.
//!
//! An app may be served from several origins, its frontends, which all sign in with the
//! principals of the app's own origin. Each frontend is its own installed web app with its
//! own service worker, so each holds a slot of its own: a notification for one frontend
//! wakes that one, which opens its link signed in. A frontend other than the app's own
//! origin is accepted only where the app lists it among its alternative origins.
//!
//! A push endpoint names one installed app on one device, and only one slot holds it. A
//! subscribe for an endpoint another slot holds is refused while that slot is kept, so
//! knowing an endpoint is not enough to silence the device.
//!
//! Every write of an app's slots also empties the stale ones on that config: a slot whose
//! account is no longer signed in on its browser entry, and a slot of a frontend the
//! app's cached alternative origins no longer name.

use super::subscription;
use super::validation::{deliverable_endpoint, PushEndpoint, SealedAuth, SubscriberKey};
use super::{seed_pool, transport};
use crate::alternative_origins::{self, AlternativeOrigin};
use crate::authz_utils::AuthorizedSession;
use crate::notifications::dispatch;
use crate::origin::ApplicationOrigin;
use crate::origin::FetchableApplicationOrigin;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::account::Session;
use crate::storage::anchor::{PendingTransport, WebPushSubscription};
use crate::storage::storable::anchor_application_config::PwaSubscription;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::PwaSubscriptionHolder;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    FrontendHostname, PrepareWebPushSubscriptionResponse, PwaWebPushSubscription,
    SetWebPushSubscriptionError, Timestamp, WebPushSubscriptionStatus,
};
use serde_bytes::ByteBuf;
use std::collections::{BTreeMap, BTreeSet};

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
    /// The app lists no such frontend among its alternative origins.
    NoSuchFrontend,
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
    /// Another slot holds the endpoint and is not stale.
    EndpointInUse,
    /// The slot written is stale: the app's cached alternative origins no longer name its
    /// frontend.
    FrontendNotListed,
    /// Nothing the caller can act on: a broken invariant or a storage failure, and why.
    Internal(String),
}

/// The keys an installed web app subscribes with, or `Pending` where the seed pool has
/// nothing to derive them from yet, or the app's alternative origins are not read yet.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PreparedPwaSubscription {
    Keys(PrepareWebPushSubscriptionResponse),
    Pending,
}

/// The frontend an app method names, as the origin it is keyed by. `None` where the method
/// names none, which is the app's own origin; an error where it names no bare origin.
pub fn validate_frontend_origin(
    frontend_origin: Option<FrontendHostname>,
) -> Result<Option<ApplicationOrigin>, String> {
    frontend_origin
        .map(|frontend_origin| ApplicationOrigin::try_from(frontend_origin.as_str()))
        .transpose()
}

/// What [`resolve_pwa_holder`] answers for a session whose app has no application number.
const NO_APPLICATION_NUMBER: &str = "the session's app has no application number";

/// Where `session`'s installed web app at `frontend_origin` registers: the session's
/// identity, its app, the browser entry it signed in from, and the frontend, which is the
/// app's own origin where `frontend_origin` is `None`. An error, logged, where the
/// session's app has no application number, which an app with a session always has.
///
/// Whether the app accepts the frontend is not checked here: a slot exists only for a
/// frontend [`prepare_pwa_subscription`] accepted.
pub fn resolve_pwa_holder(
    session: &AuthorizedSession,
    frontend_origin: Option<&ApplicationOrigin>,
) -> Result<PwaSubscriptionHolder, String> {
    let Some(application_number) =
        storage_borrow(|storage| storage.lookup_application_number_with_origin(&session.origin))
    else {
        ic_cdk::println!("ERROR: {NO_APPLICATION_NUMBER}");
        return Err(NO_APPLICATION_NUMBER.to_string());
    };
    let frontend_origin_hash = StorableOriginSha256::from_origin(
        &frontend_origin.map_or_else(|| session.origin.clone(), |origin| origin.to_string()),
    );
    Ok(PwaSubscriptionHolder {
        anchor_number: session.anchor_number,
        application_number,
        browser_id: session.browser_id,
        frontend_origin_hash,
    })
}

/// Records the key a fetch just minted for `holder`'s slot, for the account
/// `account_principal` names. This replaces what the entry held, so it delivers nothing
/// and holds no endpoint until the subscribe that spends the key lands. `now_ns` is the
/// call's time.
fn start_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    secret_key: [u8; 32],
    sender_seed: [u8; 32],
    now_ns: Timestamp,
) -> Result<(), PreparePwaSubscriptionError> {
    if !is_notifications_allowed(holder) {
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
    write_pwa_subscription(holder, Some(subscription), None, now_ns).map_err(|err| match err {
        SetPwaSubscriptionError::NotificationsNotAllowed => {
            PreparePwaSubscriptionError::NotificationsNotAllowed
        }
        SetPwaSubscriptionError::FrontendNotListed => PreparePwaSubscriptionError::NoSuchFrontend,
        SetPwaSubscriptionError::TransportKeyExpired => {
            PreparePwaSubscriptionError::Internal("a transport key expired".to_string())
        }
        SetPwaSubscriptionError::EndpointInUse => {
            PreparePwaSubscriptionError::Internal("an endpoint is in use".to_string())
        }
        SetPwaSubscriptionError::Internal(reason) => PreparePwaSubscriptionError::Internal(reason),
    })
}

/// Registers `holder`'s slot for Web Push for the account `account_principal`
/// names, replacing what it delivered for, or with `None` empties it where it delivers for
/// that account.
///
/// Where another slot holds the endpoint, it is refused with `EndpointInUse` while that
/// slot is kept at `now_ns` (see [`find_stale_pwa_slots`]); a slot no longer kept is
/// emptied in the same write.
pub fn set_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    request: Option<ValidatedPwaWebPushSubscription>,
    now_ns: Timestamp,
) -> Result<(), SetPwaSubscriptionError> {
    let Some(request) = request else {
        return remove_pwa_subscription(holder, account_principal, now_ns);
    };
    if !is_notifications_allowed(holder) {
        return Err(SetPwaSubscriptionError::NotificationsNotAllowed);
    }
    let Some(pending_transport) = read_pwa_subscription(holder)
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
    let ready_subscription = subscription::spend_pending_transport(
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
        if !find_stale_pwa_slots(endpoint_holder, now_ns).contains(&endpoint_holder) {
            return Err(SetPwaSubscriptionError::EndpointInUse);
        }
    }

    let subscription = WebPushSubscription::Ready(ready_subscription);
    let subscription = PwaSubscription {
        subscription,
        account_principal,
    };
    write_pwa_subscription(holder, Some(subscription), displaced_holder, now_ns)
}

/// Empties `holder`'s slot where it delivers for `account_principal`'s account.
/// One that holds nothing, or delivers for another account, is left as it is. `now_ns`
/// is the call's time.
fn remove_pwa_subscription(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    now_ns: Timestamp,
) -> Result<(), SetPwaSubscriptionError> {
    match read_pwa_subscription(holder) {
        Some(held_subscription) if held_subscription.account_principal != account_principal => {
            return Ok(())
        }
        Some(_) => {}
        None => return Ok(()),
    }
    write_pwa_subscription(holder, None, None, now_ns)
}

/// The other slot that holds `endpoint` now. `None` where no other slot holds it: the
/// lookup is derived from the configs, so a slot it still names that no longer holds the
/// endpoint does not count.
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
    let held_subscription = read_pwa_subscription(indexed_holder)?;
    let ready_subscription = held_subscription.subscription.into_ready()?;
    if ready_subscription.endpoint != *endpoint {
        return None;
    }
    Some(indexed_holder)
}

/// The subscription a relay answered gone for, dropped where `holder`'s slot
/// still holds that endpoint. `now_ns` is the call's time.
pub(crate) fn clear_gone_pwa_subscription(
    holder: PwaSubscriptionHolder,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), String> {
    let Some(held_subscription) = read_pwa_subscription(holder) else {
        return Ok(());
    };
    let Some(registered_subscription) = held_subscription.subscription.into_ready() else {
        return Ok(());
    };
    if registered_subscription.endpoint.as_str() != endpoint {
        return Ok(());
    }
    write_pwa_subscription(holder, None, None, now_ns).map_err(|err| format!("{err:?}"))
}

/// The endpoint `holder`'s slot is subscribed at, where it is ready and delivers
/// for `account_principal`'s account.
pub fn read_pwa_subscription_status(
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    now_ns: Timestamp,
) -> Option<WebPushSubscriptionStatus> {
    let held_subscription = read_pwa_subscription(holder)?;
    if held_subscription.account_principal != account_principal {
        return None;
    }
    let ready_subscription = held_subscription.subscription.into_ready()?;
    Some(subscription::status_of(&ready_subscription, now_ns))
}

/// Whether `holder`'s slot holds a subscription that delivers for the account
/// `account_principal` names.
pub fn is_delivering_for(holder: PwaSubscriptionHolder, account_principal: Principal) -> bool {
    read_pwa_subscription(holder)
        .is_some_and(|held_subscription| held_subscription.account_principal == account_principal)
}

/// What `holder`'s slot holds, and the account it delivers for.
pub(crate) fn read_pwa_subscription(holder: PwaSubscriptionHolder) -> Option<PwaSubscription> {
    let config = storage_borrow(|storage| {
        storage.anchor_application_config(holder.anchor_number, holder.application_number)
    })?;
    config.pwa_webpush_subscription(holder.browser_id, holder.frontend_origin_hash)
}

/// Puts `subscription` on `holder`'s slot, or with `None` empties it, after emptying
/// `displaced_holder`'s where one is given. Every prepare, set and removal of an app's
/// slots writes through here, so the same write also empties every slot of `holder`'s
/// config that [`find_stale_pwa_slots`] finds stale at `now_ns`. Putting a subscription on
/// a slot that is itself stale is refused before anything is written, so a stale slot is
/// never recreated.
pub(crate) fn write_pwa_subscription(
    holder: PwaSubscriptionHolder,
    subscription: Option<PwaSubscription>,
    displaced_holder: Option<PwaSubscriptionHolder>,
    now_ns: Timestamp,
) -> Result<(), SetPwaSubscriptionError> {
    let stale_holders = find_stale_pwa_slots(holder, now_ns);
    if subscription.is_some() && stale_holders.contains(&holder) {
        return Err(SetPwaSubscriptionError::FrontendNotListed);
    }
    storage_borrow_mut(|storage| {
        storage.write_pwa_webpush_subscription(
            holder,
            subscription,
            displaced_holder,
            &stale_holders,
        )
    })
    .map_err(|err| SetPwaSubscriptionError::Internal(format!("{err}")))
}

/// The slots of the identity's config for the app `holder` is on, on every browser
/// entry, that are stale at `now_ns`: a slot whose account has no session on the slot's
/// browser entry that is neither expired nor idle, and a slot of a frontend other than
/// the app's own origin that the app's cached alternative origins no longer name. Where
/// no list is cached, frontend slots are kept: this reads the cache only and starts no
/// fetch.
fn find_stale_pwa_slots(
    holder: PwaSubscriptionHolder,
    now_ns: Timestamp,
) -> BTreeSet<PwaSubscriptionHolder> {
    let Some(application_origin) = storage_borrow(|storage| {
        storage.lookup_application_origin_with_number(holder.application_number)
    }) else {
        return BTreeSet::new();
    };
    let application_origin_hash = StorableOriginSha256::from_origin(&application_origin);
    let listed_frontend_origin_hashes: Option<BTreeSet<StorableOriginSha256>> =
        ApplicationOrigin::try_from(application_origin.as_str())
            .ok()
            .and_then(|application_origin| {
                alternative_origins::peek_alternative_origins(&application_origin)
            })
            .map(|alternative_origin_hashes| {
                alternative_origin_hashes
                    .into_iter()
                    .map(StorableOriginSha256::from)
                    .collect()
            });
    storage_borrow(|storage| {
        let Some(config) =
            storage.anchor_application_config(holder.anchor_number, holder.application_number)
        else {
            return BTreeSet::new();
        };
        let mut stale_holders = BTreeSet::new();
        let mut sessions_by_account: BTreeMap<Principal, Vec<Session>> = BTreeMap::new();
        for (browser_id, frontend_origin_hash, held_subscription) in
            config.pwa_webpush_subscription_slots()
        {
            let slot_holder = PwaSubscriptionHolder {
                browser_id,
                frontend_origin_hash,
                ..holder
            };
            let is_listed = frontend_origin_hash == application_origin_hash
                || listed_frontend_origin_hashes
                    .as_ref()
                    .is_none_or(|listed_hashes| listed_hashes.contains(&frontend_origin_hash));
            if !is_listed {
                stale_holders.insert(slot_holder);
                continue;
            }
            let account_principal = held_subscription.account_principal;
            let sessions = sessions_by_account
                .entry(account_principal)
                .or_insert_with(|| {
                    dispatch::read_account_sessions(
                        storage,
                        holder.anchor_number,
                        holder.application_number,
                        account_principal,
                    )
                });
            let has_live_session = sessions.iter().any(|session| {
                session.browser_id == browser_id && !session.is_expired_or_idle(now_ns)
            });
            if !has_live_session {
                stale_holders.insert(slot_holder);
            }
        }
        stale_holders
    })
}

fn is_notifications_allowed(holder: PwaSubscriptionHolder) -> bool {
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

/// What the app's alternative origins say about the frontend a prepare names.
enum FrontendAuthorization {
    /// The app accepts the frontend.
    Accepted,
    /// The app's alternative origins are not read yet.
    Pending,
}

/// Whether the app of `session` accepts `frontend_origin` as its own frontend, which it
/// does without a list where `frontend_origin` is `None` or the app's own origin.
/// Refused where its alternative origins do not list it, or where II will not fetch that
/// list from the app's origin. Starts the fetch of that list where it is not cached.
/// `now_ns` is the call's time.
fn authorize_frontend(
    session: &AuthorizedSession,
    frontend_origin: Option<&ApplicationOrigin>,
    now_ns: Timestamp,
) -> Result<FrontendAuthorization, PreparePwaSubscriptionError> {
    let Some(frontend_origin) = frontend_origin else {
        return Ok(FrontendAuthorization::Accepted);
    };
    let application_origin = ApplicationOrigin::try_from(session.origin.as_str())
        .map_err(PreparePwaSubscriptionError::Internal)?;
    if *frontend_origin == application_origin {
        return Ok(FrontendAuthorization::Accepted);
    }
    FetchableApplicationOrigin::try_from(&application_origin)
        .map_err(PreparePwaSubscriptionError::Internal)?;
    match alternative_origins::authorize_alternative_origin(
        &application_origin,
        frontend_origin,
        now_ns,
    ) {
        AlternativeOrigin::Listed => Ok(FrontendAuthorization::Accepted),
        AlternativeOrigin::NotListed => Err(PreparePwaSubscriptionError::NoSuchFrontend),
        AlternativeOrigin::Pending { .. } => Ok(FrontendAuthorization::Pending),
    }
}

/// The keys `session`'s installed web app at `frontend_origin` (the app's own origin
/// where `None`) subscribes its browser entry with, given the held `vapid_public_key`.
/// `Pending` where the seed pool is cold or the app's alternative origins are not read
/// yet; refused where the app does not list `frontend_origin` among them.
pub fn prepare_pwa_subscription(
    session: &AuthorizedSession,
    frontend_origin: Option<&ApplicationOrigin>,
    vapid_public_key: Vec<u8>,
    now_ns: Timestamp,
) -> Result<PreparedPwaSubscription, PreparePwaSubscriptionError> {
    let holder = resolve_pwa_holder(session, frontend_origin)
        .map_err(PreparePwaSubscriptionError::Internal)?;
    // Before the pool is asked, so an app without consent is told so rather than to come
    // back.
    if !is_notifications_allowed(holder) {
        return Err(PreparePwaSubscriptionError::NotificationsNotAllowed);
    }
    match authorize_frontend(session, frontend_origin, now_ns)? {
        FrontendAuthorization::Accepted => {}
        FrontendAuthorization::Pending => return Ok(PreparedPwaSubscription::Pending),
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
