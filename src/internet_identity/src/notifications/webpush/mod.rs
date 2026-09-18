//! The Web Push channel: one subscription per device plus its device-signed VAPID JWT
//! pool. Shared validators and bounds live here.
//!
//! Nothing here encrypts, because the push carries no body: the browser's `p256dh` and
//! `auth` keys are neither asked for nor stored. What remains is RFC 8292 VAPID, which
//! authenticates II to the relay either way.
use std::ops::RangeInclusive;
use url::Url;

pub mod subscription;

pub use subscription::{subscribe_device, unsubscribe_device};

/// Server-side gate for the channel. Per deployment rather than per app: a subscription
/// belongs to a browser, not to one of the apps that may notify it.
pub fn check_enabled() -> Result<(), String> {
    if crate::notifications::notifications_enabled() {
        Ok(())
    } else {
        Err("notifications are not enabled".to_string())
    }
}

/// Relay endpoints run ~200-300 bytes; capped at 1 KiB.
pub const MAX_ENDPOINT_LEN: usize = 1024;
/// VAPID application server key: uncompressed SEC1 P-256, 65 bytes.
const VAPID_PUBKEY_LEN: usize = 65;
/// Raw ECDSA P-256 signature: r‖s, 32 bytes each.
const JWT_SIG_LEN: usize = 64;
/// One JWT per validity window; 30 covers ~30 days. Bounded to cap the row.
pub const MAX_JWT_POOL_LEN: usize = 30;

fn validate_param_len(
    observed_len_bytes: usize,
    allowed_range_bytes: RangeInclusive<usize>,
    dbg_name: &str,
) -> Result<(), String> {
    if !allowed_range_bytes.contains(&observed_len_bytes) {
        let bounds = if allowed_range_bytes.start() == allowed_range_bytes.end() {
            format!("expected {}", allowed_range_bytes.start())
        } else {
            format!(
                "{}..={}",
                allowed_range_bytes.start(),
                allowed_range_bytes.end()
            )
        };
        return Err(format!(
            "{dbg_name} length {observed_len_bytes} out of range ({bounds})"
        ));
    }
    Ok(())
}

/// Length plus an absolute `https://` URL. The canister POSTs to this to wake the
/// browser, so a value that is not one is a row nothing can ever be delivered to.
fn validate_endpoint(endpoint: &str) -> Result<(), String> {
    validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint")?;
    let Ok(url) = Url::parse(endpoint) else {
        return Err("endpoint is not a URL".to_string());
    };
    if url.scheme() != "https" {
        return Err("endpoint must be an https:// URL".to_string());
    }
    if url.host().is_none() {
        return Err("endpoint has no host".to_string());
    }
    Ok(())
}

/// Length plus curve validity. The key is echoed to the relay as VAPID `k=`, and a relay
/// rejects a push whose `k` it cannot parse.
fn validate_vapid_public_key(vapid_public_key: &[u8]) -> Result<(), String> {
    validate_param_len(
        vapid_public_key.len(),
        VAPID_PUBKEY_LEN..=VAPID_PUBKEY_LEN,
        "vapid_public_key",
    )?;
    if p256::PublicKey::from_sec1_bytes(vapid_public_key).is_err() {
        return Err("vapid_public_key is not a valid SEC1 P-256 point".to_string());
    }
    Ok(())
}

/// Not verified: garbage only breaks the uploader's own delivery, and 30
/// ECDSA verifications per subscribe isn't worth it.
fn validate_jwt_pool(signatures: &[Vec<u8>]) -> Result<(), String> {
    validate_param_len(signatures.len(), 1..=MAX_JWT_POOL_LEN, "jwt_pool")?;
    if let Some(bad) = signatures.iter().find(|sig| sig.len() != JWT_SIG_LEN) {
        return Err(format!(
            "each jwt signature must be {JWT_SIG_LEN} bytes, got {}",
            bad.len()
        ));
    }
    Ok(())
}

#[cfg(test)]
pub(crate) mod fixtures {
    use super::subscription::{
        add_subscription, browser_of_key, check_browser_proof, SubscribeDeviceRequest, Subscription,
    };
    use super::*;
    use crate::state::{storage_borrow, storage_borrow_mut};
    use crate::storage::anchor::Browser;
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, BrowserBrand, BrowserDescription, BrowserId, FormFactor, OperatingSystem,
        PublicKey, SubscribeDeviceError, Timestamp,
    };
    use p256::ecdsa::signature::Signer;
    use p256::ecdsa::{Signature, SigningKey};
    use serde_bytes::ByteBuf;

    /// A browser that can prove itself: a real P-256 key, on a real anchor.
    pub(crate) struct TestBrowser {
        pub anchor: AnchorNumber,
        pub id: BrowserId,
        pub key: PublicKey,
        signing: SigningKey,
    }

    impl TestBrowser {
        /// The signature the canister verifies, spelled out rather than shared with the
        /// verifier so a wire change has to be made on both sides.
        pub(crate) fn sign(
            &self,
            endpoint: &str,
            jwt_issued_at_ns: Timestamp,
            vapid_public_key: &[u8],
            jwt_signatures: &[Vec<u8>],
        ) -> ByteBuf {
            use sha2::{Digest, Sha256};
            let mut pool = Sha256::new();
            for signature in jwt_signatures {
                pool.update(signature);
            }
            let mut message = b"ii-webpush-subscription".to_vec();
            message.extend_from_slice(&self.anchor.to_be_bytes());
            message.extend_from_slice(&jwt_issued_at_ns.to_be_bytes());
            message.extend_from_slice(&Sha256::digest(vapid_public_key));
            message.extend_from_slice(&pool.finalize());
            message.extend_from_slice(endpoint.as_bytes());
            let signature: Signature = self.signing.sign(&message);
            ByteBuf::from(signature.to_bytes().to_vec())
        }
    }

    /// The SPKI header for an uncompressed P-256 key, the same 26 bytes for every such
    /// key. Written out because the crate is built with pkcs8 decoding only.
    const P256_SPKI_PREFIX: [u8; 26] = [
        0x30, 0x59, 0x30, 0x13, 0x06, 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01, 0x06, 0x08,
        0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07, 0x03, 0x42, 0x00,
    ];

    fn key_pair(seed: u8) -> (SigningKey, PublicKey) {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        let signing = SigningKey::from_slice(&[seed.max(1); 32]).expect("fixed scalar is valid");
        let point = p256::PublicKey::from(signing.verifying_key()).to_encoded_point(false);
        let mut der = P256_SPKI_PREFIX.to_vec();
        der.extend_from_slice(point.as_bytes());
        (signing, ByteBuf::from(der))
    }

    fn entry(id: BrowserId, key: &PublicKey) -> Browser {
        Browser {
            id,
            // A key no test presents, standing in for the one the sign-in was reached
            // by, which the browser has already discarded.
            current_browser_key: ByteBuf::from(vec![0xAAu8; 91]),
            next_browser_key: key.clone(),
            description: BrowserDescription {
                brand: BrowserBrand::Chrome,
                os: OperatingSystem::Macos,
                form_factor: FormFactor::Desktop,
                model: None,
            },
            created_at: 0,
            last_used: 0,
            session_count: 0,
        }
    }

    /// One browser on a fresh anchor.
    pub(crate) fn registered_browser(seed: u8) -> TestBrowser {
        registered_browsers(&[seed])
            .pop()
            .expect("one seed, one browser")
    }

    /// Two browsers of the *same* identity, which a per-identity row would collapse.
    pub(crate) fn two_registered_browsers() -> (TestBrowser, TestBrowser) {
        let mut browsers = registered_browsers(&[1, 2]);
        let other = browsers.pop().expect("two seeds");
        let one = browsers.pop().expect("two seeds");
        (one, other)
    }

    fn registered_browsers(seeds: &[u8]) -> Vec<TestBrowser> {
        storage_borrow_mut(|storage| {
            let mut anchor = storage
                .allocate_anchor(0)
                .expect("the test anchor range has room");
            let anchor_number = anchor.anchor_number();
            let browsers = seeds
                .iter()
                .enumerate()
                .map(|(index, seed)| {
                    let id = index as BrowserId + 1;
                    let (signing, key) = key_pair(*seed);
                    anchor.browsers.push(entry(id, &key));
                    TestBrowser {
                        anchor: anchor_number,
                        id,
                        key,
                        signing,
                    }
                })
                .collect();
            storage.write(anchor).expect("writing a fresh anchor");
            browsers
        })
    }

    /// A key on no anchor, for the refusal path.
    pub(crate) fn unregistered_browser(seed: u8) -> TestBrowser {
        let (signing, key) = key_pair(seed);
        TestBrowser {
            anchor: 0,
            id: 0,
            key,
            signing,
        }
    }

    pub(crate) fn valid_vapid_key() -> Vec<u8> {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        // A real, fixed SEC1 point: a length-only fixture can't pass the curve
        // check `add_subscription` runs.
        let secret = p256::SecretKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid");
        secret
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec()
    }

    /// A second real point, for a test that swaps the key the proof was made over.
    pub(crate) fn other_valid_vapid_key() -> Vec<u8> {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        let secret = p256::SecretKey::from_slice(&[2u8; 32]).expect("fixed scalar is valid");
        secret
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec()
    }

    pub(crate) fn valid_pool() -> Vec<Vec<u8>> {
        vec![vec![3u8; JWT_SIG_LEN]; 3]
    }

    pub(super) fn subscription_of(browser: &TestBrowser, endpoint: &str) -> Subscription {
        Subscription {
            anchor_number: browser.anchor,
            browser_id: browser.id,
            endpoint: endpoint.to_string(),
            vapid_public_key: valid_vapid_key(),
            jwt_signatures: valid_pool(),
            jwt_issued_at_ns: 0,
        }
    }

    pub(crate) fn request_from(
        browser: &TestBrowser,
        anchor: AnchorNumber,
        endpoint: &str,
        jwt_issued_at_ns: Timestamp,
    ) -> SubscribeDeviceRequest {
        let vapid_public_key = valid_vapid_key();
        let jwt_signatures = valid_pool();
        SubscribeDeviceRequest {
            anchor_number: anchor,
            endpoint: endpoint.to_string(),
            browser_key_signature: browser.sign(
                endpoint,
                jwt_issued_at_ns,
                &vapid_public_key,
                &jwt_signatures,
            ),
            vapid_public_key: ByteBuf::from(vapid_public_key),
            jwt_signatures: jwt_signatures.into_iter().map(ByteBuf::from).collect(),
            jwt_issued_at_ns,
            browser_key: browser.key.clone(),
        }
    }

    /// `subscribe_device` without the canister-only feature gate and clock.
    pub(crate) fn subscribe_via_proof(
        request: SubscribeDeviceRequest,
    ) -> Result<(), SubscribeDeviceError> {
        let browser_id = browser_of_key(request.anchor_number, &request.browser_key)?;
        let jwt_signatures: Vec<Vec<u8>> = request
            .jwt_signatures
            .iter()
            .map(|signature| signature.to_vec())
            .collect();
        check_browser_proof(
            &request.browser_key,
            &request.browser_key_signature,
            request.anchor_number,
            &request.endpoint,
            request.jwt_issued_at_ns,
            &request.vapid_public_key,
            &jwt_signatures,
        )?;
        let mut subscription = subscription_of(
            &TestBrowser {
                anchor: request.anchor_number,
                id: browser_id,
                key: request.browser_key.clone(),
                signing: SigningKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid"),
            },
            &request.endpoint,
        );
        subscription.jwt_issued_at_ns = request.jwt_issued_at_ns;
        add_subscription(subscription, 0)
            .map_err(|problems| SubscribeDeviceError::InternalCanisterError(problems.join("; ")))
    }

    /// Well-formed defaults so a signature change doesn't touch every test.
    pub(crate) fn subscribe(
        browser: &TestBrowser,
        endpoint: &str,
        now_ns: Timestamp,
    ) -> Result<(), Vec<String>> {
        let mut subscription = subscription_of(browser, endpoint);
        subscription.jwt_issued_at_ns = now_ns;
        add_subscription(subscription, now_ns)
    }

    pub(crate) fn subscription_count(anchor: AnchorNumber) -> u64 {
        storage_borrow(|storage| {
            storage
                .webpush_subscriptions_memory
                .range((anchor, BrowserId::MIN)..=(anchor, BrowserId::MAX))
                .count() as u64
        })
    }

    pub(crate) fn stored_endpoint(anchor: AnchorNumber, browser_id: BrowserId) -> Option<String> {
        storage_borrow(|storage| {
            storage
                .webpush_subscriptions_memory
                .get(&(anchor, browser_id))
                .map(|row| row.endpoint)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vapid_key_must_be_a_real_point() {
        assert!(validate_vapid_public_key(&fixtures::valid_vapid_key()).is_ok());
        // Right length, not on the curve, so only the curve check rejects it.
        assert!(validate_vapid_public_key(&[4u8; VAPID_PUBKEY_LEN]).is_err());
        assert!(validate_vapid_public_key(&[4u8; 33]).is_err());
    }
}
