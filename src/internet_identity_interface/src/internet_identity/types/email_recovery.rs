//! Types specific to the email-based identity recovery flow.
//!
//! See `docs/ongoing/email-recovery.md` (PR #3836) for the full design.
//! At a high level:
//!
//! - A user binds an email address to an anchor via the *setup* flow
//!   (`email_recovery_credential_prepare_add` + a verified email to
//!   `register@id.ai`).
//! - When they later lose their other authn methods, they trigger the
//!   *recovery* flow (`email_recovery_prepare_delegation` + a verified
//!   email to `recover@id.ai`) and end up with a delegation bound to a
//!   fresh session keypair.
//!
//! The shared inbound-DKIM challenge primitive that powers the
//! ownership-proof step of both flows lives in
//! [`super::email_challenge`] — challenge / status / diagnostics /
//! error / submit-leaf / DoH-resolve types are flow-neutral and live
//! there. This module owns the recovery-specific types only:
//! [`EmailRecoveryCredential`] (what gets bound to the anchor) and
//! [`EmailRecoveryGetDelegationArgs`] (the recovery-as-login
//! delegation-retrieval argument shape).

use crate::internet_identity::types::Timestamp;
use candid::{CandidType, Deserialize};
use serde::Serialize;
use serde_bytes::ByteBuf;

/// One bound recovery email, owned by a single anchor.
///
/// The credential lives directly on the anchor struct rather than in
/// a separate stable map: anchor storage uses `minicbor-derive`, which
/// is forward-compatible across optional-field additions. Old anchors
/// deserialize with `email_recovery: None`; no migration needed.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct EmailRecoveryCredential {
    /// Lowercased canonical form: `lowercase(local-part) + "@" +
    /// lowercase(domain)`. Stored verbatim (not hashed) so the user
    /// can see it in the management UI — exactly what they typed at
    /// registration.
    pub address: String,

    /// Nanoseconds since the Unix epoch (matches the rest of II's
    /// `Timestamp` field encoding).
    pub created_at: Timestamp,
    /// Same encoding as `created_at`; `None` until the credential
    /// is actually used to authorise a recovery.
    pub last_used: Option<Timestamp>,
}

/// Argument shape for `email_recovery_get_delegation` — mirrors the
/// existing `openid_get_delegation` query.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailRecoveryGetDelegationArgs {
    pub nonce: String,
    pub session_key: ByteBuf,
    pub expiration: Timestamp,
}
