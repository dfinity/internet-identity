//! Push notifications PoC — RFC 8291 (Web Push encrypted payload).
//!
//! High-level design lives in `docs/push-notifications-poc.md`. In one
//! line: dApp calls `notify_user(in_app_principal, PushAlert)` on II, II
//! encrypts the payload per device under RFC 8291, then `ic_cdk::spawn`s
//! one outcall per device to the push relay. The Service Worker on the
//! device decrypts and renders — no round-trip back to the canister.
//!
//! Submodules:
//!
//! - [`rfc8291`] — pure-CPU payload encryption. Validated against the
//!   RFC 8291 §5 test vectors in its own `#[cfg(test)]` block. This is
//!   the only piece that requires new crypto crates (`hkdf`, `aes-gcm`,
//!   `p256`'s `ecdh` feature).
//!
//! The rest of the PoC (storage, VAPID signing, `notify_user` handler,
//! outcall, `inspect_message` gate, rate limiter, PocketIC test) is not
//! wired up yet — this commit only lands the crypto core.

pub mod api;
pub mod rfc8291;
pub mod vapid;
