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
use crate::storage::storable::push_endpoint_hash::StorableEndpointSha256;
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

/// Register a browser subscription for `anchor_number` on this device.
///
/// Called from **II's own frontend** (`/manage → Settings → Enable
/// notifications on this device`), so the caller is authenticated as
/// the anchor's raw principal via `check_authz_and_record_activity` in
/// the update wrapper — same gate as any other anchor-scoped setting.
///
/// Keyed by `(anchor, sha256(endpoint))` — each browser's relay
/// endpoint gets its own row, so a user can enable notifications on
/// phone and laptop and both receive pushes. Re-subscribing the same
/// browser (same endpoint) is idempotent.
pub fn subscribe_device(
    anchor_number: AnchorNumber,
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

    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    let subscription = StorablePushSubscription {
        anchor: anchor_number,
        endpoint,
        p256dh,
        auth,
        created_at_ns: time(),
    };

    storage_borrow_mut(|storage| {
        storage
            .push_subscriptions_memory
            .insert((anchor_number, endpoint_hash), subscription);
    });
    Ok(())
}

/// Remove the browser subscription identified by `endpoint` under
/// `anchor_number`. Callers pass the endpoint URL because it's the only
/// stable identifier the browser has for its own subscription (there
/// can be several under one anchor).
pub fn unsubscribe_device(anchor_number: AnchorNumber, endpoint: String) -> Result<(), String> {
    let endpoint_hash = StorableEndpointSha256::from_endpoint(&endpoint);
    storage_borrow_mut(|storage| {
        storage
            .push_subscriptions_memory
            .remove(&(anchor_number, endpoint_hash));
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

/// Encrypt `alert` under each of the anchor's registered devices' keys
/// and detach one HTTPS outcall per subscription.
///
/// Called by dApps — the caller authenticates as an in-app principal
/// which we reverse-lookup via `PRINCIPAL_INDEX` to recover the target
/// anchor. Subscriptions live on II's SW (Option A), so all of them —
/// phone, laptop, whatever — get pinged. The SW is responsible for
/// showing the notification attributed to the caller's origin, which we
/// override into `alert.hostname` here so a malicious dApp can't lie
/// about who sent it.
///
/// `entropy_seed` is 32 bytes pre-fetched from `raw_rand`; we HKDF-
/// expand it per-device so each RFC 8291 encryption gets a fresh
/// ephemeral scalar + salt without another async round-trip.
pub fn notify_user(
    in_app_principal: Principal,
    mut alert: PushAlert,
    entropy_seed: [u8; 32],
) -> Result<(), String> {
    // 1. Reverse-lookup the calling in-app principal.
    let sender =
        storage_borrow(|storage| storage.push_principal_index_memory.get(&in_app_principal))
            .ok_or_else(|| "no consent recorded for that principal".to_string())?;

    let anchor = sender.anchor;
    let origin_hash =
        StorableOriginSha256::from_bytes(std::borrow::Cow::Owned(sender.origin_hash.to_vec()));

    // 2. Defensive consent check — grant/revoke keep both maps in sync,
    //    but if the consent row is missing we treat this as revoked
    //    rather than trusting a stale PRINCIPAL_INDEX row.
    let consent = storage_borrow(|storage| {
        storage
            .push_consent_memory
            .get(&(anchor, origin_hash.clone()))
    })
    .ok_or_else(|| "consent has been revoked".to_string())?;

    // 3. Force the alert's `hostname` field to the origin the user
    //    actually consented to — a dApp that owns `foo.app` can't send
    //    a notification labelled as `bar.app`. The SW displays this
    //    string verbatim.
    alert.hostname = consent.origin;

    // 4. Collect every subscription for the anchor. Bounded per-anchor
    //    by the number of devices the user has enabled II push on — a
    //    handful in practice.
    let subscriptions: Vec<StorablePushSubscription> = storage_borrow(|storage| {
        storage
            .push_subscriptions_memory
            .iter()
            .filter(|((a, _), _)| *a == anchor)
            .map(|(_, sub)| sub)
            .collect()
    });
    if subscriptions.is_empty() {
        return Err(
            "no devices are enabled for push notifications on this identity yet".to_string(),
        );
    }

    // 5. Derive a per-device RNG from the single `raw_rand` seed via a
    //    counter-mode ChaCha20 stream so each encryption gets distinct
    //    ephemeral P-256 key material without another async round-trip.
    let plaintext = alert_to_json(&alert);
    let mut root_rng = ChaCha20Rng::from_seed(entropy_seed);
    let now_secs = time() / 1_000_000_000;
    let exp = now_secs + vapid::VAPID_JWT_MAX_LIFETIME_SECS;
    let pubkey_b64 = vapid::public_key_base64url();

    // JWT audience varies by relay; sign once per unique audience so a
    // mass push doesn't redo the ECDSA op for every device on the same
    // relay (e.g. 3 Chrome devices → 1 FCM JWT).
    let mut audience_to_jwt: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    for subscription in subscriptions {
        // Derive a fresh 32-byte seed for this subscription's encryption.
        let mut per_device_seed = [0u8; 32];
        rand_chacha::rand_core::RngCore::fill_bytes(&mut root_rng, &mut per_device_seed);

        let auth_arr: [u8; AUTH_SECRET_LEN] = match subscription.auth.as_slice().try_into() {
            Ok(a) => a,
            Err(_) => {
                // Corrupted row — skip this device, keep going for others.
                ic_cdk::println!(
                    "push: skipping subscription with malformed auth ({} bytes)",
                    subscription.auth.len()
                );
                continue;
            }
        };
        if subscription.p256dh.len() != P256_UNCOMPRESSED_LEN {
            ic_cdk::println!(
                "push: skipping subscription with malformed p256dh ({} bytes)",
                subscription.p256dh.len()
            );
            continue;
        }

        let mut rng = ChaCha20Rng::from_seed(per_device_seed);
        let ciphertext =
            match rfc8291::encrypt(&plaintext, &subscription.p256dh, &auth_arr, &mut rng) {
                Ok(c) => c,
                Err(e) => {
                    ic_cdk::println!("push: encrypt failed: {e:?}");
                    continue;
                }
            };

        let audience = match vapid::origin_of_endpoint(&subscription.endpoint) {
            Ok(a) => a,
            Err(e) => {
                ic_cdk::println!("push: bad endpoint origin: {e}");
                continue;
            }
        };
        let jwt = audience_to_jwt
            .entry(audience.clone())
            .or_insert_with(|| vapid::sign_jwt(&audience, exp))
            .clone();

        let request = CanisterHttpRequestArgument {
            url: subscription.endpoint.clone(),
            method: HttpMethod::POST,
            body: Some(ciphertext),
            max_response_bytes: Some(1024),
            transform: None,
            headers: vec![
                HttpHeader {
                    name: "Authorization".into(),
                    value: format!("vapid t={jwt}, k={}", pubkey_b64.clone()),
                },
                HttpHeader {
                    name: "Content-Encoding".into(),
                    value: "aes128gcm".into(),
                },
                HttpHeader {
                    name: "TTL".into(),
                    value: "60".into(),
                },
                HttpHeader {
                    name: "Content-Type".into(),
                    value: "application/octet-stream".into(),
                },
            ],
        };

        // Detach the outcall. The SW observes success; the dApp caller
        // returns in ms. Failures are logged so a real deployment can
        // wire them into stats.
        ic_cdk::spawn(async move {
            match http_request(request, PUSH_OUTCALL_CYCLES).await {
                Ok((response,)) => {
                    let status = response.status.0.to_string();
                    if !status.starts_with('2') {
                        ic_cdk::println!("push outcall non-2xx status: {status}");
                    }
                }
                Err((_code, msg)) => {
                    ic_cdk::println!("push outcall failed: {msg}");
                }
            }
        });
    }

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
