//! Verified emails — parallel to `email_recovery`. Reuses the
//! inbound-DKIM pipeline via `email_inbound`; differs only in the
//! storage destination, the `II-Verify-` nonce prefix, and the
//! per-anchor cap (`MAX_VERIFIED_EMAILS_PER_ANCHOR`).

pub mod prepare;
pub mod remove;

pub use prepare::prepare_add;
pub use remove::{remove, RemoveError};
