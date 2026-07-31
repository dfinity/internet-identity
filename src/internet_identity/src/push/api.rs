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
use crate::storage::storable::push_sender_registration::StorablePushSenderRegistration;
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

/// Matches `delegation::check_frontend_length`, which is what derives the
/// in-app principal. Checked rather than trapped on: this is reachable from a
/// caller-supplied string, and canister code must not trap on user input.
const MAX_ORIGIN_LEN: usize = 255;

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
                origin: Some(origin),
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
            // Legacy rows written before `origin` was added carry `None`
            // here (see StorablePushConsent's schema-evolution note) —
            // skip them since we can't recover the plaintext origin.
            .filter_map(|(_, consent)| consent.origin)
            .collect()
    })
}

/// Return the push-relay endpoint URLs registered for `anchor_number` —
/// one per device that has been enabled via II's Settings. Handy for
/// debugging: if the phone's Enable button appeared to succeed but the
/// phone doesn't receive pushes, checking whether its endpoint is here
/// tells us whether the subscribe round-trip completed.
///
/// The endpoint URL is not secret — it's just a per-browser handle the
/// relay published — so surfacing it verbatim is safe. Encryption keys
/// are not returned.
pub fn debug_list_devices(anchor_number: AnchorNumber) -> Vec<String> {
    storage_borrow(|storage| {
        storage
            .push_subscriptions_memory
            .iter()
            .filter(|((anchor, _), _)| *anchor == anchor_number)
            .map(|(_, sub)| sub.endpoint)
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
/// Register `sender` as the canister allowed to send as `origin`, or clear the
/// registration when `sender` is `None`. Authorization is the caller's
/// responsibility (see `push_register_sender`).
pub fn register_sender(origin: FrontendHostname, sender: Option<Principal>) -> Result<(), String> {
    if origin.len() > MAX_ORIGIN_LEN {
        return Err(format!(
            "origin must be at most {MAX_ORIGIN_LEN} bytes, got {}",
            origin.len()
        ));
    }
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow_mut(|storage| match sender {
        Some(principal) => {
            storage.push_sender_memory.insert(
                origin_hash,
                StorablePushSenderRegistration::new(principal, time()),
            );
        }
        None => {
            storage.push_sender_memory.remove(&origin_hash);
        }
    });
    Ok(())
}

/// The canister registered to send as `origin`, if any.
pub fn registered_sender(origin: FrontendHostname) -> Option<Principal> {
    if origin.len() > MAX_ORIGIN_LEN {
        return None;
    }
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow(|storage| storage.push_sender_memory.get(&origin_hash))
        .and_then(|registration| registration.sender_principal())
}

/// Whether `caller()` may send notifications attributed to `origin_hash`.
///
/// Accepts the registered sender for the origin, or the recipient itself
/// (a user asking to be notified needs no further authorization).
fn authorize_sender(
    origin_hash: &StorableOriginSha256,
    in_app_principal: Principal,
) -> Result<(), String> {
    let caller = ic_cdk::caller();
    if caller == in_app_principal {
        return Ok(());
    }
    let registered = storage_borrow(|storage| storage.push_sender_memory.get(origin_hash))
        .and_then(|registration| registration.sender_principal());
    match registered {
        Some(sender) if sender == caller => Ok(()),
        // Deliberately does not say whether a sender is registered for this
        // origin — the caller learns only that it isn't the one.
        _ => Err("caller is not a registered sender for that origin".to_string()),
    }
}

/// ephemeral scalar + salt without another async round-trip.
pub async fn notify_user(
    in_app_principal: Principal,
    mut alert: PushAlert,
    entropy_seed: [u8; 32],
) -> Result<(), String> {
    let vapid_key = vapid::get_or_init_signing_key()
        .await
        .map_err(|e| format!("VAPID key unavailable: {e}"))?;
    // 1. Reverse-lookup the calling in-app principal.
    let sender =
        storage_borrow(|storage| storage.push_principal_index_memory.get(&in_app_principal))
            .ok_or_else(|| "no consent recorded for that principal".to_string())?;

    let anchor = sender.anchor;
    let origin_hash =
        StorableOriginSha256::from_bytes(std::borrow::Cow::Owned(sender.origin_hash.to_vec()));

    // 1b. Authorize the caller as a sender for that origin.
    //
    //     Not "prove you are the recipient": `in_app_principal` is a
    //     canister-signature principal derived from II's seed, so only the
    //     user's own browser can ever present it as `caller()` — an
    //     inter-canister call always arrives as the calling canister's
    //     principal. Requiring caller == recipient therefore rejects every
    //     real sender and admits only self-sends, which is the opposite of
    //     what this endpoint is for.
    //
    //     A self-send is still allowed: the recipient asking to be notified
    //     is trivially authorized, and it keeps the browser-driven demo path
    //     working.
    authorize_sender(&origin_hash, in_app_principal)?;

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
    //    string verbatim. Legacy consent rows without an origin
    //    plaintext (see StorablePushConsent's schema-evolution note)
    //    fail closed — the user has to re-grant.
    alert.hostname = consent
        .origin
        .ok_or_else(|| "consent row is missing its origin; re-grant to fix".to_string())?;

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
    // One id per admitted message, shared by every device in this fan-out. It
    // rides inside the encrypted payload, so no relay or gateway can read or
    // forge it. Derived from the same `raw_rand` seed rather than a counter,
    // which keeps it unguessable and needs no stored state.
    let msg_id = hex_prefix(&entropy_seed, 16);
    let plaintext = alert_to_json(&alert, &msg_id);
    let mut root_rng = ChaCha20Rng::from_seed(entropy_seed);
    let now_secs = time() / 1_000_000_000;
    let exp = now_secs + vapid::VAPID_JWT_MAX_LIFETIME_SECS;
    let pubkey_b64 = vapid::public_key_base64url(&vapid_key);

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
            .or_insert_with(|| vapid::sign_jwt(&vapid_key, &audience, exp))
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
        let dead_row = (
            anchor,
            StorableEndpointSha256::from_endpoint(&subscription.endpoint),
        );
        ic_cdk::spawn(async move {
            match http_request(request, PUSH_OUTCALL_CYCLES).await {
                Ok((response,)) => {
                    let status = response.status.0.to_string();
                    // 404/410 mean the relay has no such subscription: the
                    // browser dropped or rotated it. That is the only signal
                    // that a row is dead, so drop it here — otherwise rows
                    // accumulate forever and every future send pays for a
                    // target that can never receive.
                    if status == "404" || status == "410" {
                        storage_borrow_mut(|storage| {
                            storage.push_subscriptions_memory.remove(&dead_row);
                        });
                        ic_cdk::println!("push: dropped a subscription the relay reports gone");
                    } else if !status.starts_with('2') {
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
/// Lowercase hex of the first `n` bytes. Used for `msg_id`; avoids pulling in a
/// hex crate for one 16-byte value.
fn hex_prefix(bytes: &[u8], n: usize) -> String {
    let mut out = String::with_capacity(n * 2);
    for byte in bytes.iter().take(n) {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

/// Serializes the payload the service worker receives, including `msg_id`.
///
/// `msg_id` is deliberately not a field on `PushAlert`: no dApp supplies it, and
/// it must be identical for every device in one fan-out. That is what lets the
/// service worker collapse duplicates — two subscription rows pointing at the
/// same browser (an endpoint rotation that left a stale row, a second
/// registration) carry the same id, so the second banner is suppressed.
fn alert_to_json(alert: &PushAlert, msg_id: &str) -> Vec<u8> {
    // Manual formatting rather than serde_json — the shape is fixed and
    // avoiding another serialization crate keeps the wasm smaller. We
    // escape `"` and `\` in the string fields per JSON grammar.
    let mut buf = String::with_capacity(256);
    buf.push('{');
    push_json_field(&mut buf, "msg_id", msg_id);
    buf.push(',');
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn msg_id_is_stable_for_one_seed_and_differs_across_seeds() {
        // Every device in one fan-out must get the SAME id — that is what lets
        // the service worker collapse two rows pointing at one browser. Two
        // different sends must not collide, or a genuine notification would be
        // suppressed as a duplicate.
        let seed_a = [7u8; 32];
        let seed_b = [8u8; 32];

        assert_eq!(hex_prefix(&seed_a, 16), hex_prefix(&seed_a, 16));
        assert_ne!(hex_prefix(&seed_a, 16), hex_prefix(&seed_b, 16));
        assert_eq!(hex_prefix(&seed_a, 16).len(), 32);
    }

    #[test]
    fn payload_carries_the_msg_id_the_worker_dedups_on() {
        let alert = PushAlert {
            hostname: "https://app.example".to_string(),
            title: "t".to_string(),
            body: "b".to_string(),
            url: None,
        };

        let json = String::from_utf8(alert_to_json(&alert, "abc123")).unwrap();

        assert!(json.contains(r#""msg_id":"abc123""#), "got {json}");
    }

    #[test]
    fn payload_escapes_string_fields() {
        let alert = PushAlert {
            hostname: "https://app.example".to_string(),
            title: r#"a"b\c"#.to_string(),
            body: "b".to_string(),
            url: None,
        };

        let json = String::from_utf8(alert_to_json(&alert, "id")).unwrap();

        assert!(json.contains(r#"a\"b\\c"#), "got {json}");
    }
}
