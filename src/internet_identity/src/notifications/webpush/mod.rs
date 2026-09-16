//! The Web Push channel: one subscription per device plus its device-signed
//! VAPID JWT pool. Shared validators and bounds live here.
//!
//! The push itself carries no body, so nothing here encrypts: the browser's
//! `p256dh` and `auth` keys exist only for RFC 8291 payloads and are neither
//! asked for nor stored. What remains is RFC 8292 VAPID, which authenticates II
//! to the relay whether or not there is a body.
use std::ops::RangeInclusive;

pub mod jwt_pool;
pub mod subscription;

/// Relay endpoints run ~200-300 bytes; capped at 1 KiB.
pub const MAX_ENDPOINT_LEN: usize = 1024;
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

/// Length plus curve validity. This key is echoed to the relay as the VAPID
/// `k=` parameter, and a relay rejects a push whose `k` it cannot parse, so an
/// unparseable key stored here would surface only as a device that silently
/// never receives anything.
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
    use super::*;
    use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

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
            super::subscription::Subscription {
                anchor_number: anchor,
                endpoint: endpoint.to_string(),
                vapid_public_key: valid_vapid_key(),
                jwt_signatures: valid_pool(),
                jwt_issued_at_ns: now_ns,
            },
            now_ns,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vapid_key_must_be_a_real_point() {
        assert!(validate_vapid_public_key(&fixtures::valid_vapid_key()).is_ok());
        // Right length, not on the curve: the length-only check that used to
        // stand here would have stored it.
        assert!(validate_vapid_public_key(&[4u8; VAPID_PUBKEY_LEN]).is_err());
        assert!(validate_vapid_public_key(&[4u8; 33]).is_err());
    }
}
