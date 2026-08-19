//! The timer-driven dispatcher that turns each buffered ping into per-device Web
//! Push POSTs. II is fire-and-forget: one non-replicated outcall per device, no
//! retry (the dApp owns resends). A 410 from the relay means the device is gone,
//! so its subscription and seals are pruned. The payload is the blob sealed at
//! consent; nothing here reads or produces notification content.

use super::send::{take_next, BufferedNotification, NotificationUrgency};
use super::webpush::vapid_jwt;
use crate::state::storage_borrow;
use crate::storage::storable::application::StorableOriginSha256;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

#[cfg(not(test))]
use super::feature_enabled;
#[cfg(not(test))]
use super::webpush::subscription::prune_device;
#[cfg(not(test))]
use ic_cdk::api::management_canister::http_request::{
    HttpHeader, HttpMethod, HttpResponse, TransformContext,
};

/// The management canister's `http_request` argument for a non-replicated
/// outcall. Hand-rolled because this `ic_cdk` version's
/// `CanisterHttpRequestArgument` has no `is_replicated` field; `Some(false)`
/// selects the single-node call instead of one per replica.
#[cfg(not(test))]
#[derive(candid::CandidType)]
struct NonReplicatedHttpRequest {
    url: String,
    max_response_bytes: Option<u64>,
    method: HttpMethod,
    headers: Vec<HttpHeader>,
    body: Option<Vec<u8>>,
    transform: Option<TransformContext>,
    is_replicated: Option<bool>,
}

/// TTL for a notification the sender left open-ended: how long the relay holds
/// it for an offline device.
const DEFAULT_TTL_SECONDS: u64 = 24 * 60 * 60;

/// How often the dispatcher fires.
#[cfg(not(test))]
const DISPATCH_INTERVAL_SECONDS: u64 = 2;
/// Soft cap on deliveries pulled per tick; a single anchor's device fan-out may
/// carry it slightly over, staying well under the ~500 in-flight ceiling.
#[cfg(not(test))]
const MAX_DELIVERIES_PER_DISPATCH: usize = 250;
/// Generous budget for one non-replicated POST; the unused remainder refunds.
#[cfg(not(test))]
const RELAY_CYCLES: u128 = 20_000_000_000;
/// We only read the status line, never the body.
#[cfg(not(test))]
const MAX_RESPONSE_BYTES: u64 = 1024;

/// One ready-to-POST Web Push: the sealed blob plus the headers that carry it.
struct Delivery {
    anchor: AnchorNumber,
    endpoint: String,
    vapid_public_key: Vec<u8>,
    blob: Vec<u8>,
    jwt: String,
    urgency: Option<NotificationUrgency>,
    ttl_seconds: u64,
}

/// Builds this tick's batch: pulls buffered pings and fans each out to its
/// user's sealed devices, stopping once `max` deliveries are queued. An expired
/// ping, or one with no reachable sealed device, drops.
fn take_pending_deliveries(max: usize, now_ns: Timestamp) -> Vec<Delivery> {
    let mut deliveries = Vec::new();
    while deliveries.len() < max {
        let Some(notification) = take_next() else {
            break;
        };
        if notification
            .expires_at_ns
            .is_some_and(|expiry| expiry <= now_ns)
        {
            continue;
        }
        queue_deliveries_for_origin(&notification, now_ns, &mut deliveries);
    }
    deliveries
}

/// For one buffered ping, queues a push (a `Delivery`) to each of the anchor's
/// devices that has a sealed payload for that origin. A device with no seal for
/// the origin has nothing to send, so it gets nothing.
fn queue_deliveries_for_origin(
    notification: &BufferedNotification,
    now_ns: Timestamp,
    out: &mut Vec<Delivery>,
) {
    let anchor = notification.anchor_number;
    let origin_hash = StorableOriginSha256::from_origin(&notification.origin);
    let ttl_seconds = ttl_seconds(notification.expires_at_ns, now_ns);

    let rows = storage_borrow(|storage| storage.devices_for_delivery(anchor, &origin_hash));
    for (endpoint, vapid_public_key, blob, pool) in rows {
        let Some(relay_origin) = relay_origin_of(&endpoint) else {
            continue;
        };
        let Some(jwt) = vapid_jwt::assemble(&pool, &relay_origin, now_ns) else {
            continue;
        };
        out.push(Delivery {
            anchor,
            endpoint,
            vapid_public_key,
            blob,
            jwt,
            urgency: notification.urgency,
            ttl_seconds,
        });
    }
}

/// The `scheme://host[:port]` a relay endpoint lives at — the VAPID `aud`.
fn relay_origin_of(endpoint: &str) -> Option<String> {
    let scheme_end = endpoint.find("://")? + 3;
    let host_len = endpoint[scheme_end..]
        .find('/')
        .unwrap_or(endpoint.len() - scheme_end);
    let origin = &endpoint[..scheme_end + host_len];
    (host_len > 0).then(|| origin.to_string())
}

fn ttl_seconds(expires_at_ns: Option<Timestamp>, now_ns: Timestamp) -> u64 {
    match expires_at_ns {
        Some(expiry) => expiry.saturating_sub(now_ns) / 1_000_000_000,
        None => DEFAULT_TTL_SECONDS,
    }
}

fn urgency_header(urgency: Option<NotificationUrgency>) -> &'static str {
    match urgency {
        Some(NotificationUrgency::Low) => "low",
        Some(NotificationUrgency::High) => "high",
        Some(NotificationUrgency::Normal) | None => "normal",
    }
}

/// RFC 8030/8291/8292 headers: retention, encrypted-payload encoding, urgency,
/// and the VAPID authorization pairing the device's JWT with its public key.
fn build_push_headers(delivery: &Delivery) -> Vec<(String, String)> {
    let key = BASE64_URL_SAFE_NO_PAD.encode(&delivery.vapid_public_key);
    vec![
        ("TTL".to_string(), delivery.ttl_seconds.to_string()),
        ("Content-Encoding".to_string(), "aes128gcm".to_string()),
        (
            "Urgency".to_string(),
            urgency_header(delivery.urgency).to_string(),
        ),
        (
            "Authorization".to_string(),
            format!("vapid t={}, k={}", delivery.jwt, key),
        ),
    ]
}

#[cfg(not(test))]
thread_local! {
    static DISPATCHING: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Starts the recurring dispatcher. Gated on the feature flag, which only changes on
/// upgrade — the same path that re-runs this — so re-checking here is enough.
#[cfg(not(test))]
pub fn schedule_dispatch() {
    if !feature_enabled() {
        return;
    }
    ic_cdk_timers::set_timer_interval(
        std::time::Duration::from_secs(DISPATCH_INTERVAL_SECONDS),
        || ic_cdk::spawn(dispatch()),
    );
}

#[cfg(test)]
pub fn schedule_dispatch() {}

/// One dispatch pass. The re-entrancy guard keeps a slow pass (outcalls in flight)
/// from overlapping with the next tick, bounding total in-flight outcalls.
#[cfg(not(test))]
async fn dispatch() {
    if DISPATCHING.with(|dispatching| dispatching.replace(true)) {
        return;
    }
    let deliveries = take_pending_deliveries(MAX_DELIVERIES_PER_DISPATCH, ic_cdk::api::time());
    for (anchor, endpoint) in run_deliveries(deliveries).await {
        prune_device(anchor, &endpoint);
    }
    DISPATCHING.with(|dispatching| dispatching.set(false));
}

/// Fires every delivery concurrently (the batch is already capped) and returns
/// the devices the relay reported gone.
#[cfg(not(test))]
async fn run_deliveries(deliveries: Vec<Delivery>) -> Vec<(AnchorNumber, String)> {
    let outcomes = futures::future::join_all(deliveries.iter().map(deliver_one)).await;
    deliveries
        .into_iter()
        .zip(outcomes)
        .filter(|(_, gone)| *gone)
        .map(|(delivery, _)| (delivery.anchor, delivery.endpoint))
        .collect()
}

/// POSTs one sealed payload to its relay via a non-replicated outcall — one node
/// makes the call, so we send once instead of once per replica. Returns whether
/// the relay reported the device gone (410). Any other outcome is dropped.
#[cfg(not(test))]
async fn deliver_one(delivery: &Delivery) -> bool {
    use candid::Principal;

    let request = NonReplicatedHttpRequest {
        url: delivery.endpoint.clone(),
        max_response_bytes: Some(MAX_RESPONSE_BYTES),
        method: HttpMethod::POST,
        headers: build_push_headers(delivery)
            .into_iter()
            .map(|(name, value)| HttpHeader { name, value })
            .collect(),
        body: Some(delivery.blob.clone()),
        transform: None,
        is_replicated: Some(false),
    };

    let result: ic_cdk::api::call::CallResult<(HttpResponse,)> =
        ic_cdk::api::call::call_with_payment128(
            Principal::management_canister(),
            "http_request",
            (request,),
            RELAY_CYCLES,
        )
        .await;
    matches!(result, Ok((response,)) if response.status == 410u32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::send::enqueue;
    use crate::notifications::test_setup as setup;
    use crate::notifications::webpush::fixtures::subscribe;
    use crate::state::storage_borrow_mut;
    use crate::storage::storable::notifications::webpush::endpoint_hash::StorableEndpointSha256;
    use crate::storage::storable::notifications::webpush::seal::StorableWebPushSeal;

    const ANCHOR: AnchorNumber = 1;
    const ENDPOINT: &str = "https://relay.example/wpush/abc";
    const ORIGIN: &str = "https://app.example";

    fn buffered(expires_at_ns: Option<Timestamp>) -> BufferedNotification {
        BufferedNotification {
            anchor_number: ANCHOR,
            origin: ORIGIN.to_string(),
            expires_at_ns,
            urgency: Some(NotificationUrgency::High),
        }
    }

    fn seed_seal(blob: Vec<u8>) {
        let endpoint_hash = StorableEndpointSha256::from_endpoint(ENDPOINT);
        let origin_hash = StorableOriginSha256::from_origin(&ORIGIN.to_string());
        storage_borrow_mut(|storage| {
            storage.webpush_seal_memory.insert(
                (ANCHOR, endpoint_hash, origin_hash),
                StorableWebPushSeal {
                    blob,
                    created_at_ns: 0,
                },
            );
        });
    }

    #[test]
    fn one_delivery_per_sealed_device() {
        setup();
        subscribe(ANCHOR, ENDPOINT, 1_000).unwrap();
        seed_seal(vec![0xEE; 100]);
        enqueue(buffered(None));

        let deliveries = take_pending_deliveries(10, 2_000);
        assert_eq!(deliveries.len(), 1);
        let delivery = &deliveries[0];
        assert_eq!(delivery.anchor, ANCHOR);
        assert_eq!(delivery.endpoint, ENDPOINT);
        assert_eq!(delivery.blob, vec![0xEE; 100]);
        assert_eq!(delivery.vapid_public_key.len(), 65);
        assert!(delivery
            .jwt
            .starts_with("eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9."));
    }

    #[test]
    fn drops_expired_before_fanning_out() {
        setup();
        subscribe(ANCHOR, ENDPOINT, 1_000).unwrap();
        seed_seal(vec![0xEE; 100]);
        enqueue(buffered(Some(1_500)));
        assert!(take_pending_deliveries(10, 2_000).is_empty());
    }

    #[test]
    fn no_delivery_without_a_seal_for_the_origin() {
        setup();
        subscribe(ANCHOR, ENDPOINT, 1_000).unwrap();
        enqueue(buffered(None));
        assert!(take_pending_deliveries(10, 2_000).is_empty());
    }

    #[test]
    fn no_delivery_without_a_subscription() {
        setup();
        seed_seal(vec![0xEE; 100]);
        enqueue(buffered(None));
        assert!(take_pending_deliveries(10, 2_000).is_empty());
    }

    #[test]
    fn relay_origin_strips_the_path() {
        assert_eq!(
            relay_origin_of("https://fcm.googleapis.com/fcm/send/abc").as_deref(),
            Some("https://fcm.googleapis.com")
        );
        assert_eq!(
            relay_origin_of("https://host:8443/x").as_deref(),
            Some("https://host:8443")
        );
        assert_eq!(
            relay_origin_of("https://bare.example").as_deref(),
            Some("https://bare.example")
        );
        assert_eq!(relay_origin_of("not-a-url"), None);
    }

    #[test]
    fn ttl_from_expiry_else_default() {
        assert_eq!(ttl_seconds(Some(5_000_000_000), 2_000_000_000), 3);
        assert_eq!(ttl_seconds(None, 999), DEFAULT_TTL_SECONDS);
    }

    #[test]
    fn urgency_maps_to_rfc_values() {
        assert_eq!(urgency_header(None), "normal");
        assert_eq!(urgency_header(Some(NotificationUrgency::Low)), "low");
        assert_eq!(urgency_header(Some(NotificationUrgency::Normal)), "normal");
        assert_eq!(urgency_header(Some(NotificationUrgency::High)), "high");
    }

    #[test]
    fn headers_carry_ttl_encoding_urgency_and_vapid_auth() {
        let delivery = Delivery {
            anchor: ANCHOR,
            endpoint: ENDPOINT.to_string(),
            vapid_public_key: vec![4u8; 65],
            blob: vec![1, 2, 3],
            jwt: "JWT".to_string(),
            urgency: Some(NotificationUrgency::High),
            ttl_seconds: 42,
        };
        let headers = build_push_headers(&delivery);
        let value = |name: &str| {
            headers
                .iter()
                .find(|(header, _)| header == name)
                .map(|(_, value)| value.clone())
        };
        assert_eq!(value("TTL").as_deref(), Some("42"));
        assert_eq!(value("Content-Encoding").as_deref(), Some("aes128gcm"));
        assert_eq!(value("Urgency").as_deref(), Some("high"));
        assert!(value("Authorization")
            .unwrap()
            .starts_with("vapid t=JWT, k="));
    }
}
