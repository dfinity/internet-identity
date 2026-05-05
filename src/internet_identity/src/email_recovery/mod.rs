//! Email-based identity recovery: setup (binding) flow.
//!
//! See `docs/ongoing/email-recovery.md` (PR #3836) for the full design.
//! This module currently implements the **setup** half:
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
//! The recovery half (`email_recovery_prepare_delegation` +
//! `recover@id.ai` dispatch) lives in a follow-up PR — it needs a
//! reverse `address → AnchorNumber` stable index for the verified
//! `From:` lookup, which is best landed as its own focused change.
//!
//! ## Why heap state, not stable
//!
//! The pending-challenge map has a 30-minute TTL and is a strict
//! optimisation — losing it on canister upgrade just costs every
//! in-flight registration its retry. We do **not** want stable-memory
//! churn for entries that expire in half an hour. The heap-side
//! `HashMap` is naturally bounded by the policy in §8.8 of the design
//! doc (size cap + oldest-first eviction).

#![allow(dead_code)] // Exposed as the module fills in.

mod pending;
mod prepare;
mod remove;
mod rng;
mod smtp;

pub use prepare::prepare_add;
pub use remove::{remove_credential, RemoveError};
pub use smtp::handle_smtp_request;

/// Wrapper around `pending::status_of` so the canister method in
/// `main.rs` doesn't need to know which submodule the heap state
/// lives in.
pub fn pending_status(
    nonce: &str,
    now_secs: u64,
) -> internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryStatus {
    pending::status_of(nonce, now_secs)
}

#[allow(unused_imports)]
pub use pending::{PendingChallenge, PendingKind, PendingStatus};

/// Verbose human-readable token prefix. Every legitimate token the
/// canister hands out begins with this exact byte sequence; the FE
/// surfaces this prefix prominently in the wizard so a phisher who
/// hands the user a different-looking string (no `II-Recovery-`,
/// truncated, etc.) is immediately obvious.
pub const NONCE_PREFIX: &str = "II-Recovery-";

/// Byte length of the random suffix appended after [`NONCE_PREFIX`].
/// 8 bytes → 16 hex chars → ~64 bits of entropy. The challenge map
/// has a 30-minute TTL and is bounded in size, so 64 bits is well
/// past any plausible online-guessing budget.
pub const NONCE_SUFFIX_BYTES: usize = 8;

/// How long a pending challenge lives. The user has this long to
/// compose and send the magic email; after this the canister evicts
/// the entry and the FE shows `Expired` on its next status poll.
pub const CHALLENGE_TTL_SECS: u64 = 30 * 60;

/// Cap on the in-flight pending-challenge map. Sized generously
/// (10 000 entries) so legitimate fill rates never get close, and
/// the design-doc §8.8 eviction analysis applies cleanly above it.
pub const MAX_PENDING_CHALLENGES: usize = 10_000;

/// Mailbox the user emails for the **setup** flow. The canister
/// dispatches by recipient: `register@id.ai` ↔ binding flow,
/// `recover@id.ai` ↔ recovery flow (the latter is deferred).
pub const SETUP_MAILBOX: &str = "register@id.ai";

/// Mailbox the user emails for the **recovery** flow. Reserved for
/// the follow-up PR; declared here so the constant lives next to its
/// peer and the surface is committed.
pub const RECOVERY_MAILBOX: &str = "recover@id.ai";
