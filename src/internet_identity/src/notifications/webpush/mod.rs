//! The Web Push channel: one subscription per device plus its device-signed
//! VAPID JWT pool. Shared validators and bounds live here.
use std::ops::RangeInclusive;

pub mod jwt_pool;
pub mod rfc8291;
pub mod seal;
pub mod subscription;
pub mod vapid_jwt;

/// Relay endpoints run ~200-300 bytes; capped at 1 KiB.
pub const MAX_ENDPOINT_LEN: usize = 1024;
/// Device public key: uncompressed SEC1 P-256, 65 bytes.
const P256DH_LEN: usize = 65;
/// Auth secret from the push subscription (RFC 8291), 16 bytes.
const AUTH_LEN: usize = 16;
/// VAPID application server key: uncompressed SEC1 P-256, 65 bytes.
const VAPID_PUBKEY_LEN: usize = 65;
/// Raw ECDSA P-256 signature: r‖s, 32 bytes each.
const JWT_SIG_LEN: usize = 64;
/// One JWT per validity window; 30 covers ~30 days. Bounded to cap the row.
pub const MAX_JWT_POOL_LEN: usize = 30;
/// Overflow evicts the oldest rather than rejecting: subscriptions are
/// ephemeral, so eviction keeps the current device working.
pub const MAX_SUBSCRIPTIONS_PER_ANCHOR: u64 = 20;

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

/// Length plus curve validity: an unparseable `p256dh` could never be sealed,
/// so reject it here with an actionable error instead of silently storing a
/// subscription that never becomes deliverable.
fn validate_p256dh(p256dh: &[u8]) -> Result<(), String> {
    validate_param_len(p256dh.len(), P256DH_LEN..=P256DH_LEN, "p256dh")?;
    if !rfc8291::validate_device_public_key(p256dh) {
        return Err("p256dh is not a valid SEC1 P-256 point".to_string());
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
    use super::*;
    use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

    pub(crate) fn valid_p256dh() -> Vec<u8> {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        // A real, fixed SEC1 point: the length-only fixture can't pass the curve
        // check `add_subscription` now runs.
        let secret = p256::SecretKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid");
        secret
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec()
    }

    pub(crate) fn valid_auth() -> Vec<u8> {
        vec![9u8; AUTH_LEN]
    }

    pub(crate) fn valid_vapid_key() -> Vec<u8> {
        vec![4u8; VAPID_PUBKEY_LEN]
    }

    pub(crate) fn valid_pool() -> Vec<Vec<u8>> {
        vec![vec![3u8; JWT_SIG_LEN]; 3]
    }

    /// Well-formed defaults so a signature change doesn't touch every test.
    pub(crate) fn subscribe(
        anchor: AnchorNumber,
        endpoint: &str,
        now_ns: Timestamp,
    ) -> Result<(), Vec<String>> {
        super::subscription::add_subscription(
            anchor,
            endpoint.to_string(),
            valid_p256dh(),
            valid_auth(),
            valid_vapid_key(),
            valid_pool(),
            now_ns,
            now_ns,
        )
    }
}
