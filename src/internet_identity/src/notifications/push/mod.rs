//! The Web Push channel: one subscription per device plus its device-signed
//! VAPID JWT pool. Shared validators and bounds live here.
use std::ops::RangeInclusive;

pub mod jwt_pool;
pub mod subscription;

/// Relay endpoints run ~200-300 bytes; capped at 1 KiB.
pub const MAX_ENDPOINT_LEN: usize = 1024;
const P256DH_LEN: usize = 65;
const AUTH_LEN: usize = 16;
const VAPID_PUBKEY_LEN: usize = 65;
/// Raw ECDSA P-256 signature: r‖s, 32 bytes each.
const JWT_SIG_LEN: usize = 64;
/// One JWT per validity window; 30 covers ~30 days. Bounded to cap the row.
pub const MAX_JWT_POOL_LEN: usize = 30;
/// Overflow evicts the oldest rather than rejecting: subscriptions are
/// ephemeral, so eviction keeps the current device working.
pub const MAX_SUBSCRIPTIONS_PER_ANCHOR: u64 = 20;

fn validate_param_len(name: &str, len: usize, range: RangeInclusive<usize>) -> Result<(), String> {
    if !range.contains(&len) {
        let bounds = if range.start() == range.end() {
            format!("expected {}", range.start())
        } else {
            format!("{}..={}", range.start(), range.end())
        };
        return Err(format!("{name} length {len} out of range ({bounds})"));
    }
    Ok(())
}

/// Not verified: garbage only breaks the uploader's own delivery, and 30
/// ECDSA verifications per subscribe isn't worth it.
fn validate_jwt_pool(signatures: &[Vec<u8>]) -> Result<(), String> {
    validate_param_len("jwt_pool", signatures.len(), 1..=MAX_JWT_POOL_LEN)?;
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
        vec![4u8; P256DH_LEN]
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
    ) -> Result<(), String> {
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
