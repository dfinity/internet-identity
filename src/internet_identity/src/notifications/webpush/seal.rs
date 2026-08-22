//! RFC 8291 payload sealing, computed eagerly at consent/subscribe and reused
//! per send. One sealed blob per (device, origin): a device holds one push
//! subscription but can be consented to several apps, so each app's ping needs
//! its own sealed payload telling the SW which origin to pull for.

use super::rfc8291;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::webpush::endpoint_hash::StorableEndpointSha256;
use crate::storage::storable::notifications::webpush::seal::StorableWebPushSeal;
use crate::storage::Storage;
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, Timestamp,
};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

/// The push payload: the origin of the app that has a notification waiting, and
/// nothing else. The content itself never travels through the push relay — the
/// service worker wakes on this ping and fetches it from the app directly.
fn notification_payload(origin: &str) -> Vec<u8> {
    serde_json::to_vec(&serde_json::json!({ "o": origin }))
        .expect("serializing a notification payload cannot fail")
}

fn seal(rng: &mut ChaCha20Rng, p256dh: &[u8], auth: &[u8], origin: &str) -> Vec<u8> {
    let auth: &[u8; rfc8291::AUTH_SECRET_LEN] = auth
        .try_into()
        .expect("auth is validated to AUTH_SECRET_LEN at subscribe");
    rfc8291::encrypt(&notification_payload(origin), p256dh, auth, rng)
}

/// Seal every subscribed device of `anchor` for `origin`. Called when consent
/// is granted, so the send path finds a ready blob for each device.
pub async fn seal_devices_for_origin(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    now_ns: Timestamp,
) {
    let devices = storage_borrow(|storage| storage.webpush_subscribed_devices(anchor_number));
    if devices.is_empty() {
        return;
    }
    let mut rng = ChaCha20Rng::from_seed(crate::random_salt().await);
    storage_borrow_mut(|storage| {
        seal_devices_into(storage, &mut rng, anchor_number, origin, &devices, now_ns)
    });
}

/// Seals each device's `p256dh`/`auth` for `origin` and stores the blobs. Split
/// from the async wrapper so it's testable without `raw_rand`.
fn seal_devices_into(
    storage: &mut Storage<DefaultMemoryImpl>,
    rng: &mut ChaCha20Rng,
    anchor_number: AnchorNumber,
    origin: &str,
    devices: &[(StorableEndpointSha256, Vec<u8>, Vec<u8>)],
    now_ns: Timestamp,
) {
    let origin_hash = StorableOriginSha256::from_origin(&origin.to_string());
    for (endpoint_hash, p256dh, auth) in devices {
        let blob = seal(rng, p256dh, auth, origin);
        storage.add_webpush_seal(
            (anchor_number, endpoint_hash.clone(), origin_hash.clone()),
            StorableWebPushSeal {
                blob,
                created_at_ns: now_ns,
            },
        );
    }
}

/// Reseal one device for every origin `anchor` has consented to. Called on
/// (re-)subscribe: the device's keys may have changed, so its old seals are
/// dropped first, then rebuilt.
pub async fn reseal_device(
    anchor_number: AnchorNumber,
    endpoint_hash: StorableEndpointSha256,
    p256dh: &[u8],
    auth: &[u8],
    now_ns: Timestamp,
) {
    let origins = storage_borrow(|storage| storage.notifications_consented_origins(anchor_number));
    if origins.is_empty() {
        drop_device_seals(anchor_number, &endpoint_hash);
        return;
    }
    let mut rng = ChaCha20Rng::from_seed(crate::random_salt().await);
    storage_borrow_mut(|storage| {
        reseal_device_into(
            storage,
            &mut rng,
            anchor_number,
            &endpoint_hash,
            p256dh,
            auth,
            &origins,
            now_ns,
        )
    });
}

/// Drops the device's old seals then reseals it for each consented origin, in
/// one borrow. Split from the async wrapper so it's testable without `raw_rand`.
#[allow(clippy::too_many_arguments)]
fn reseal_device_into(
    storage: &mut Storage<DefaultMemoryImpl>,
    rng: &mut ChaCha20Rng,
    anchor_number: AnchorNumber,
    endpoint_hash: &StorableEndpointSha256,
    p256dh: &[u8],
    auth: &[u8],
    origins: &[FrontendHostname],
    now_ns: Timestamp,
) {
    storage.remove_webpush_seals_for_device(anchor_number, endpoint_hash);
    for origin in origins {
        let blob = seal(rng, p256dh, auth, origin);
        let origin_hash = StorableOriginSha256::from_origin(origin);
        storage.add_webpush_seal(
            (anchor_number, endpoint_hash.clone(), origin_hash),
            StorableWebPushSeal {
                blob,
                created_at_ns: now_ns,
            },
        );
    }
}

/// Drop every seal for one device.
pub fn drop_device_seals(anchor_number: AnchorNumber, endpoint_hash: &StorableEndpointSha256) {
    storage_borrow_mut(|storage| {
        storage.remove_webpush_seals_for_device(anchor_number, endpoint_hash)
    });
}

/// Drop every device's seal for one origin.
pub fn drop_origin_seals(anchor_number: AnchorNumber, origin: &FrontendHostname) {
    let origin_hash = StorableOriginSha256::from_origin(origin);
    storage_borrow_mut(|storage| {
        storage.remove_webpush_seals_for_origin(anchor_number, &origin_hash)
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::test_setup as setup;

    fn insert_seal(anchor: AnchorNumber, endpoint: &str, origin: &str) {
        storage_borrow_mut(|storage| {
            storage.webpush_seal_memory.insert(
                (
                    anchor,
                    StorableEndpointSha256::from_endpoint(endpoint),
                    StorableOriginSha256::from_origin(&origin.to_string()),
                ),
                StorableWebPushSeal {
                    blob: vec![1u8; 10],
                    created_at_ns: 0,
                },
            );
        });
    }

    fn seal_count(anchor: AnchorNumber) -> usize {
        storage_borrow(|storage| {
            let start = (
                anchor,
                StorableEndpointSha256::MIN,
                StorableOriginSha256::MIN,
            );
            let end = (
                anchor,
                StorableEndpointSha256::MAX,
                StorableOriginSha256::MAX,
            );
            storage.webpush_seal_memory.range(start..=end).count()
        })
    }

    fn test_rng() -> ChaCha20Rng {
        ChaCha20Rng::from_seed([7u8; 32])
    }

    fn seal_present(anchor: AnchorNumber, endpoint: &str, origin: &str) -> bool {
        storage_borrow(|storage| {
            storage.webpush_seal_memory.contains_key(&(
                anchor,
                StorableEndpointSha256::from_endpoint(endpoint),
                StorableOriginSha256::from_origin(&origin.to_string()),
            ))
        })
    }

    #[test]
    fn seal_devices_for_origin_seals_every_subscribed_device() {
        setup();
        let anchor = 1;
        super::super::fixtures::subscribe(anchor, "https://relay.example/x", 0).unwrap();
        super::super::fixtures::subscribe(anchor, "https://relay.example/y", 0).unwrap();

        let devices = storage_borrow(|storage| storage.webpush_subscribed_devices(anchor));
        let mut rng = test_rng();
        storage_borrow_mut(|storage| {
            seal_devices_into(
                storage,
                &mut rng,
                anchor,
                "https://app.example",
                &devices,
                123,
            )
        });

        assert_eq!(seal_count(anchor), 2, "both devices get a seal");
        assert!(seal_present(
            anchor,
            "https://relay.example/x",
            "https://app.example"
        ));
        assert!(seal_present(
            anchor,
            "https://relay.example/y",
            "https://app.example"
        ));
    }

    #[test]
    fn reseal_device_replaces_that_device_across_all_origins() {
        setup();
        let anchor = 1;
        let endpoint = "https://relay.example/x";
        let endpoint_hash = StorableEndpointSha256::from_endpoint(endpoint);
        // A stale seal from before the key rotated, plus another device untouched.
        insert_seal(anchor, endpoint, "https://app-a.example");
        insert_seal(anchor, "https://relay.example/y", "https://app-a.example");

        let origins = vec![
            "https://app-a.example".to_string(),
            "https://app-b.example".to_string(),
        ];
        let (p256dh, auth) = (
            super::super::fixtures::valid_p256dh(),
            super::super::fixtures::valid_auth(),
        );
        let mut rng = test_rng();
        storage_borrow_mut(|storage| {
            reseal_device_into(
                storage,
                &mut rng,
                anchor,
                &endpoint_hash,
                &p256dh,
                &auth,
                &origins,
                7,
            )
        });

        // The device now has exactly its two consented origins, the other device
        // is untouched.
        assert!(seal_present(anchor, endpoint, "https://app-a.example"));
        assert!(seal_present(anchor, endpoint, "https://app-b.example"));
        assert!(seal_present(
            anchor,
            "https://relay.example/y",
            "https://app-a.example"
        ));
        assert_eq!(seal_count(anchor), 3);
    }

    #[test]
    fn notification_payload_is_origin_only() {
        assert_eq!(
            notification_payload("https://app.example"),
            br#"{"o":"https://app.example"}"#.to_vec(),
            "payload must carry only the origin"
        );
    }

    #[test]
    fn drop_device_seals_removes_only_that_device() {
        setup();
        let anchor = 1;
        insert_seal(anchor, "https://relay.example/x", "https://app-a.example");
        insert_seal(anchor, "https://relay.example/x", "https://app-b.example");
        insert_seal(anchor, "https://relay.example/y", "https://app-a.example");

        drop_device_seals(
            anchor,
            &StorableEndpointSha256::from_endpoint("https://relay.example/x"),
        );

        assert_eq!(
            seal_count(anchor),
            1,
            "only the other device's seal remains"
        );
    }

    #[test]
    fn drop_origin_seals_removes_only_that_origin() {
        setup();
        let anchor = 1;
        insert_seal(anchor, "https://relay.example/x", "https://app-a.example");
        insert_seal(anchor, "https://relay.example/y", "https://app-a.example");
        insert_seal(anchor, "https://relay.example/x", "https://app-b.example");

        drop_origin_seals(anchor, &"https://app-a.example".to_string());

        assert_eq!(
            seal_count(anchor),
            1,
            "only the other origin's seal remains"
        );
    }
}
