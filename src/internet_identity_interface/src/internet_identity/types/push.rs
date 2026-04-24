//! Types for Web Push notifications.

use candid::{CandidType, Deserialize, Principal};
use serde_bytes::ByteBuf;

// --- Bounds ---

/// Push subscription endpoints can be up to ~2048 bytes for RFC 8030 compliance.
pub const MAX_PUSH_ENDPOINT_BYTES: usize = 2_048;
/// Uncompressed P-256 public key: 65 bytes (0x04 || X || Y).
pub const PUSH_P256DH_KEY_BYTES: usize = 65;
/// Client auth secret per RFC 8291: 16 bytes.
pub const PUSH_AUTH_SECRET_BYTES: usize = 16;
/// Maximum number of push subscriptions stored per identity (anchor).
pub const MAX_PUSH_SUBSCRIPTIONS_PER_USER: usize = 10;

// --- API types (Candid) ---

/// A Web Push subscription as delivered by the browser's `PushManager.subscribe()`.
///
/// `endpoint` is the push service URL the canister POSTs to. `p256dh` and `auth`
/// are reserved for a future payload-encrypted implementation (RFC 8291). In V1
/// the canister sends empty-body pushes so these fields are accepted but unused.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct PushSubscription {
    pub endpoint: String,
    pub p256dh: ByteBuf,
    pub auth: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum PushSubscribeError {
    Unauthorized(Principal),
    InvalidSubscription(String),
    TooManySubscriptions,
    InternalCanisterError(String),
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum PushUnsubscribeError {
    Unauthorized(Principal),
    InternalCanisterError(String),
}
