//! Business logic for the five push-notification methods exposed on the
//! canister's Candid surface. Each function is called from a thin
//! `#[update]` wrapper in `main.rs` that first runs
//! [`check_authz_and_record_activity`] and then delegates here.
//!
//! Storage lives on the shared [`Storage`] struct (see [`crate::storage`]);
//! access goes through the state getters so all three push maps stay
//! consistent under concurrent update calls.
//!
//! `notify_user` is called with pre-fetched entropy (32 bytes from
//! `raw_rand`) — the async part lives on the `#[update]` wrapper. This
//! keeps the storage/encrypt/outcall pipeline synchronous once entropy
//! is in hand, matching the "no suspension after auth" property design.

use crate::delegation;
use crate::push::rfc8291::{self, AUTH_SECRET_LEN, P256_UNCOMPRESSED_LEN};
use crate::push::vapid;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::push_consent::StorablePushConsent;
use crate::storage::storable::push_sender_info::StorablePushSenderInfo;
use crate::storage::storable::push_subscription::StorablePushSubscription;
use candid::Principal;
use ic_cdk::api::management_canister::http_request::{
    http_request, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
};
use ic_cdk::api::time;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};
use rand_chacha::rand_core::SeedableRng;
use rand_chacha::ChaCha20Rng;

/// The alert record `notify_user` accepts. Mirrors the `PushAlert`
/// Candid record in `main.rs` — kept here so `api.rs` has no cross-
/// module dependency for the payload shape.
#[derive(Clone, Debug)]
pub struct PushAlert {
    pub hostname: String,
    pub title: String,
    pub body: String,
    pub url: Option<String>,
}

/// Endpoint URLs from Apple/Google/Mozilla are ~200-300 bytes in practice;
/// we cap at 1 KiB to bound the per-row footprint. Anything longer is
/// almost certainly malformed and rejected up front.
const MAX_ENDPOINT_LEN: usize = 1024;

/// Register a browser subscription for the caller's `(anchor, origin)`
/// pair, recovered from `push_principal_index_memory`.
///
/// The dApp doesn't pass `anchor` or `origin` — both are recovered from
/// the caller's per-origin principal, which was inserted into
/// `PRINCIPAL_INDEX` when the user granted consent at sign-in. Requiring
/// a `PRINCIPAL_INDEX` entry is the authorization gate: only principals
/// the user has consented to can register subscriptions on their behalf.
///
/// Idempotent by `(anchor, origin_hash)` — a re-subscribe from the same
/// browser overwrites the existing row.
pub fn subscribe_device(
    caller: Principal,
    endpoint: String,
    p256dh: Vec<u8>,
    auth: Vec<u8>,
) -> Result<(), String> {
    if endpoint.is_empty() || endpoint.len() > MAX_ENDPOINT_LEN {
        return Err(format!(
            "endpoint length {} out of range (1..{MAX_ENDPOINT_LEN})",
            endpoint.len()
        ));
    }
    // The relay guarantees HTTPS; anything else is a malformed subscription
    // (or an attacker's forgery). We validate scheme up front so bad rows
    // never reach the outcall builder.
    if !endpoint.starts_with("https://") {
        return Err("endpoint must be an https:// URL".to_string());
    }
    if p256dh.len() != P256_UNCOMPRESSED_LEN {
        return Err(format!(
            "p256dh must be {P256_UNCOMPRESSED_LEN} bytes (uncompressed SEC1), got {}",
            p256dh.len()
        ));
    }
    if p256dh[0] != 0x04 {
        return Err("p256dh must start with 0x04 (uncompressed marker)".to_string());
    }
    if auth.len() != AUTH_SECRET_LEN {
        return Err(format!(
            "auth must be {AUTH_SECRET_LEN} bytes, got {}",
            auth.len()
        ));
    }

    let sender = storage_borrow(|storage| storage.push_principal_index_memory.get(&caller))
        .ok_or_else(|| {
            "no consent recorded for this principal — grant consent via II sign-in first"
                .to_string()
        })?;
    let anchor = sender.anchor;
    let origin_hash =
        StorableOriginSha256::from_bytes(std::borrow::Cow::Owned(sender.origin_hash.to_vec()));

    let subscription = StorablePushSubscription {
        anchor,
        endpoint,
        p256dh,
        auth,
        created_at_ns: time(),
    };

    storage_borrow_mut(|storage| {
        storage
            .push_subscriptions_memory
            .insert((anchor, origin_hash), subscription);
    });
    Ok(())
}

/// Remove the caller's browser subscription. Both anchor and origin are
/// recovered from `PRINCIPAL_INDEX` — same shape as [`subscribe_device`],
/// so a dApp can unsubscribe by calling this method with no arguments.
pub fn unsubscribe_device(caller: Principal) -> Result<(), String> {
    let sender = storage_borrow(|storage| storage.push_principal_index_memory.get(&caller))
        .ok_or_else(|| "no consent recorded for this principal".to_string())?;
    let anchor = sender.anchor;
    let origin_hash =
        StorableOriginSha256::from_bytes(std::borrow::Cow::Owned(sender.origin_hash.to_vec()));

    storage_borrow_mut(|storage| {
        storage
            .push_subscriptions_memory
            .remove(&(anchor, origin_hash));
    });
    Ok(())
}

/// Record that the user has granted `origin` permission to send push
/// notifications for this anchor. Also writes the reverse index
/// `in_app_principal -> anchor` so `notify_user` can find the anchor from
/// the dApp's per-origin principal.
///
/// The per-origin principal is deterministic ([`delegation::get_principal`]),
/// so an existing consent row with a stale principal (e.g. after a salt
/// change — unrelated to this feature) would be replaced on the next
/// grant.
pub fn grant_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> Result<(), String> {
    let origin_hash_raw = crate::utils::sha256sum(origin.as_bytes());
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    let in_app_principal = delegation::get_principal(anchor_number, origin.clone());

    storage_borrow_mut(|storage| {
        storage.push_consent_memory.insert(
            (anchor_number, origin_hash),
            StorablePushConsent {
                granted_at_ns: time(),
                origin,
            },
        );
        storage.push_principal_index_memory.insert(
            in_app_principal,
            StorablePushSenderInfo {
                anchor: anchor_number,
                origin_hash: origin_hash_raw,
            },
        );
    });
    Ok(())
}

/// Reverse of [`grant_consent`]. Removes both the consent row and the
/// principal-index row. Subscriptions for the origin are left in place
/// intentionally — the user may re-grant later, and re-storing the
/// browser's keys would require a fresh subscribe round-trip.
pub fn revoke_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> Result<(), String> {
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    let in_app_principal = delegation::get_principal(anchor_number, origin);

    storage_borrow_mut(|storage| {
        storage
            .push_consent_memory
            .remove(&(anchor_number, origin_hash));
        storage
            .push_principal_index_memory
            .remove(&in_app_principal);
    });
    Ok(())
}

/// List every origin `anchor_number` has granted push-notification consent
/// to. Backs the Settings UI's "manage notification permissions" screen.
///
/// The map key only holds the origin's hash (one-way), so this reads the
/// plaintext `origin` off each matching [`StorablePushConsent`] value
/// instead. A full scan is acceptable here: consent rows per anchor are
/// bounded by the number of dApps a user has granted, a small PoC-scale
/// count.
pub fn list_consented_origins(anchor_number: AnchorNumber) -> Vec<FrontendHostname> {
    storage_borrow(|storage| {
        storage
            .push_consent_memory
            .iter()
            .filter(|((anchor, _), _)| *anchor == anchor_number)
            .map(|(_, consent)| consent.origin)
            .collect()
    })
}

/// Cycles budgeted per HTTPS outcall. FCM/APNs/Mozilla all return short
/// bodies (< 1 KiB), and the request itself is ~2 KiB — the actual
/// consumption on a 13-node subnet is around 1B cycles per call. We
/// budget 3x to leave headroom for slow relays.
const PUSH_OUTCALL_CYCLES: u128 = 3_000_000_000;

/// Encrypt `alert` under the browser's keys and detach one HTTPS outcall
/// per subscription for the target `(anchor, origin)`.
///
/// `entropy_seed` is 32 bytes pre-fetched from the management canister's
/// `raw_rand` — we use it to seed a per-call `ChaCha20Rng` so the
/// synchronous part (encryption + outcall build) never suspends.
///
/// The outcalls are spawned with `ic_cdk::spawn` and their success is
/// observed only by the browser Service Worker — the caller is
/// acknowledged in milliseconds, before any relay round-trip. This is
/// deliberate: the design says the dApp shouldn't block on relay latency
/// (docs/push-notifications-poc.md §15).
pub fn notify_user(
    in_app_principal: Principal,
    alert: PushAlert,
    entropy_seed: [u8; 32],
) -> Result<(), String> {
    // 1. Reverse-lookup the calling in-app principal.
    let sender =
        storage_borrow(|storage| storage.push_principal_index_memory.get(&in_app_principal))
            .ok_or_else(|| "no consent recorded for that principal".to_string())?;

    let anchor = sender.anchor;
    let origin_hash =
        StorableOriginSha256::from_bytes(std::borrow::Cow::Owned(sender.origin_hash.to_vec()));

    // 2. Defensive: consent should still be present given the index was
    //    written by grant_consent and revoke_consent clears both. If it's
    //    missing, treat as revoked rather than trusting the index.
    let consent_exists = storage_borrow(|storage| {
        storage
            .push_consent_memory
            .contains_key(&(anchor, origin_hash.clone()))
    });
    if !consent_exists {
        return Err("consent has been revoked".to_string());
    }

    // 3. Look up the subscription for this (anchor, origin). One row max
    //    in the current schema (see docs/push-notifications-poc.md §13).
    let subscription = storage_borrow(|storage| {
        storage
            .push_subscriptions_memory
            .get(&(anchor, origin_hash))
    })
    .ok_or_else(|| "no push subscription for that (anchor, origin)".to_string())?;

    // 4. Encrypt. Auth secret length is enforced at subscribe time, so a
    //    length mismatch here means the row was corrupted post-write —
    //    surface it rather than silently drop the notification.
    let auth: [u8; AUTH_SECRET_LEN] = subscription
        .auth
        .as_slice()
        .try_into()
        .map_err(|_| "corrupted auth secret in storage".to_string())?;
    if subscription.p256dh.len() != P256_UNCOMPRESSED_LEN {
        return Err("corrupted p256dh in storage".to_string());
    }

    let plaintext = alert_to_json(&alert);
    let mut rng = ChaCha20Rng::from_seed(entropy_seed);
    let ciphertext = rfc8291::encrypt(&plaintext, &subscription.p256dh, &auth, &mut rng)
        .map_err(|e| format!("encryption failed: {e:?}"))?;

    // 5. Build the VAPID JWT. `exp` is now + 12h (RFC 8292 §2 cap).
    let audience = vapid::origin_of_endpoint(&subscription.endpoint)?;
    let now_secs = time() / 1_000_000_000;
    let exp = now_secs + vapid::VAPID_JWT_MAX_LIFETIME_SECS;
    let jwt = vapid::sign_jwt(&audience, exp);
    let pubkey_b64 = vapid::public_key_base64url();

    // 6. Compose the outcall — one HTTP POST to the relay's endpoint URL.
    //    Headers per RFC 8291 §4 + RFC 8292 §3.
    let request = CanisterHttpRequestArgument {
        url: subscription.endpoint.clone(),
        method: HttpMethod::POST,
        body: Some(ciphertext),
        max_response_bytes: Some(1024),
        transform: None,
        headers: vec![
            HttpHeader {
                name: "Authorization".into(),
                value: format!("vapid t={jwt}, k={pubkey_b64}"),
            },
            HttpHeader {
                name: "Content-Encoding".into(),
                value: "aes128gcm".into(),
            },
            HttpHeader {
                // TTL is the relay's "hold for this long if the device is
                // offline" hint (seconds). 60 s is Web Push's typical
                // default for time-sensitive alerts.
                name: "TTL".into(),
                value: "60".into(),
            },
            HttpHeader {
                name: "Content-Type".into(),
                value: "application/octet-stream".into(),
            },
        ],
    };

    // 7. Detach the outcall. The browser SW observes success; the dApp
    //    caller returns in ms. Failures are logged so a real deployment
    //    can wire them into stats.
    ic_cdk::spawn(async move {
        match http_request(request, PUSH_OUTCALL_CYCLES).await {
            Ok((response,)) => {
                let status = response.status.0.to_string();
                // Web Push relays return 201 Created on success. 4xx/5xx
                // are worth surfacing during PoC bring-up.
                if !status.starts_with('2') {
                    ic_cdk::println!("push outcall non-2xx status: {status}");
                }
            }
            Err((_code, msg)) => {
                ic_cdk::println!("push outcall failed: {msg}");
            }
        }
    });

    Ok(())
}

/// Serialize a PushAlert as JSON for the encrypted body. The Service
/// Worker parses this in its `onpush` handler.
fn alert_to_json(alert: &PushAlert) -> Vec<u8> {
    // Manual formatting rather than serde_json — the shape is fixed and
    // avoiding another serialization crate keeps the wasm smaller. We
    // escape `"` and `\` in the string fields per JSON grammar.
    let mut buf = String::with_capacity(256);
    buf.push('{');
    push_json_field(&mut buf, "hostname", &alert.hostname);
    buf.push(',');
    push_json_field(&mut buf, "title", &alert.title);
    buf.push(',');
    push_json_field(&mut buf, "body", &alert.body);
    if let Some(url) = &alert.url {
        buf.push(',');
        push_json_field(&mut buf, "url", url);
    }
    buf.push('}');
    buf.into_bytes()
}

fn push_json_field(buf: &mut String, key: &str, value: &str) {
    buf.push('"');
    buf.push_str(key);
    buf.push_str("\":\"");
    for c in value.chars() {
        match c {
            '"' => buf.push_str("\\\""),
            '\\' => buf.push_str("\\\\"),
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                use std::fmt::Write;
                let _ = write!(buf, "\\u{:04x}", c as u32);
            }
            c => buf.push(c),
        }
    }
    buf.push('"');
}
