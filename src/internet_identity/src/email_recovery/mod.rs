//! Email-based identity recovery: setup + recovery flows.
//!
//! See `docs/ongoing/email-recovery.md` (PR #3836) for the full design.
//! Both halves of the canister-side flow live in this module:
//!
//! - **Setup** (binding): an authenticated caller binds a recovery
//!   email address to an anchor.
//! - **Recovery** (delegation): an anonymous caller proves control
//!   of a previously-bound address and ends with a `SignedDelegation`
//!   rooted in the anchor that address resolves to.
//!
//! Both flows are built on top of the shared inbound-DKIM-challenge
//! primitive at [`crate::email_inbound`], which owns the SMTP gateway
//! handler, the pending-challenge map, the DKIM verifier, the DMARC
//! alignment check, and the `(address → AnchorNumber)` reverse index
//! used at recovery time. What this module adds is the
//! recovery-specific surface: binding to `Anchor.email_recovery`, the
//! recovery-as-login delegation flow, and removal.
//!
//! Setup flow at a glance:
//!
//! 1. The user, while authenticated, calls
//!    `email_recovery_credential_prepare_add(anchor, dns_input)`.
//! 2. The canister validates the input, issues a random nonce, and
//!    parks a `PendingChallenge` keyed by that nonce.
//! 3. The user sends a DKIM-signed email with that nonce in the
//!    `Subject:` header to `register@id.ai`.
//! 4. The SMTP gateway forwards the email via `smtp_request`. The
//!    canister verifies DKIM + DMARC, matches the From: against the
//!    claimed address, and binds the credential to the anchor.
//!
//! Recovery follows the same two-phase DNSSEC shape but starts
//! anonymous, sends to `recover@id.ai`, and finishes by stamping a
//! delegation seed bound to the FE's session_pk (see §8.5).

mod prepare;
mod remove;

pub use prepare::{prepare_add, prepare_delegation};
pub use remove::{remove_credential, RemoveError};

/// Look up the cached `RecoveryOutcome.seed` for a recovery-flow
/// pending challenge. Used by `email_recovery_get_delegation` to
/// find the canister-signature without having to recompute the
/// seed (which requires the address + anchor — both already
/// resolved at submit-leaf time).
///
/// Returns `None` if the nonce is unknown, expired, isn't a
/// recovery-kind entry, or hasn't yet completed (`recovery_outcome`
/// is `None`).
pub fn recovery_seed_for_nonce(nonce: &str, now_secs: u64) -> Option<ic_certification::Hash> {
    crate::email_inbound::pending::with_mut(nonce, now_secs, |c| {
        c.recovery_outcome.as_ref().map(|o| o.seed)
    })
    .flatten()
}

// ---------------------------------------------------------------------------
// Backwards-compatible re-exports of the shared inbound primitive.
//
// External callers (`main.rs`, `session_delegation`, `authz_utils`,
// rustdoc references in `dkim/`, `dmarc/`, `storage/`) imported these
// from `crate::email_recovery::*` when the primitive lived here. The
// re-exports keep their call sites valid through this PR; a follow-up
// will migrate them to `crate::email_inbound::*` directly.
// ---------------------------------------------------------------------------

#[allow(unused_imports)]
pub use crate::email_inbound::{
    dnssec_email_recovery_enabled, handle_smtp_request, handle_smtp_request_validate,
    mailbox_domains, pending_diagnostics, pending_status, resolve_via_doh, submit_dkim_leaf,
    PendingChallenge, PendingKind, PendingStatus, CHALLENGE_TTL_SECS, MAX_ADDRESS,
    MAX_DKIM_TXT_BYTES, MAX_DMARC_TXT_BYTES, MAX_DOMAIN, MAX_LOCAL_PART, MAX_PENDING_CHALLENGES,
    MAX_SESSION_KEY_BYTES, MAX_VERIFIED_EMAILS_PER_ANCHOR, NONCE_PREFIX, NONCE_SUFFIX_BYTES,
    RECOVERY_RECIPIENT_USER, SETUP_RECIPIENT_USER, VERIFIED_EMAIL_NONCE_PREFIX,
};
