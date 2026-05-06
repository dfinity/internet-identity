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
//! Both share the SMTP gateway protocol (`smtp_request`), the DKIM
//! verifier, the DMARC alignment check, and the `(address →
//! AnchorNumber)` reverse index in stable memory used at recovery
//! time to resolve the verified `From:` to an anchor.
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
pub(crate) mod smtp;
mod submit_leaf;

pub use prepare::{prepare_add, prepare_delegation};
pub use remove::{remove_credential, RemoveError};
pub use smtp::{handle_smtp_request, handle_smtp_request_validate};
pub use submit_leaf::submit_dkim_leaf;

/// Wrapper around `pending::status_of` so the canister method in
/// `main.rs` doesn't need to know which submodule the heap state
/// lives in.
pub fn pending_status(
    nonce: &str,
    now_secs: u64,
) -> internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryStatus {
    pending::status_of(nonce, now_secs)
}

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
    pending::with_mut(nonce, now_secs, |c| {
        c.recovery_outcome.as_ref().map(|o| o.seed)
    })
    .flatten()
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

/// Recipient user-part for the **setup** flow. Together with any of
/// the configured mailbox domains (see [`mailbox_domains`]) this
/// forms a full mailbox the canister accepts at SMTP time.
pub const SETUP_RECIPIENT_USER: &str = "register";

/// Recipient user-part for the **recovery** flow.
pub const RECOVERY_RECIPIENT_USER: &str = "recover";

/// All mailbox domains the canister accepts as `register@…` /
/// `recover@…` aliases. Derived entirely from `related_origins`
/// (already a per-deploy arg, set by the deploy scripts): for every
/// entry we take the host part, lowercased.
///
/// On prod that's typically `["id.ai", "identity.ic0.app",
/// "identity.internetcomputer.org"]`; on beta it's `["beta.id.ai"]`.
/// All entries are equal aliases — recipient dispatch and the
/// `smtp_request_validate` query accept the user-part `register` or
/// `recover` paired with any of these domains, so the same WASM
/// works against whichever zone the gateway routes mail for and a
/// multi-domain prod deploy doesn't have to single one out as
/// canonical. The FE renders the user-facing mailbox label by
/// pairing `register` / `recover` with `window.location.hostname`,
/// so each tab automatically shows the user the alias matching the
/// origin they're already on; the canister never needs to pick a
/// "primary" domain.
///
/// Returns an empty vec when `related_origins` is unset or empty —
/// the canister then drops every inbound `smtp_request` recipient.
/// Deploys are expected to configure `related_origins`; running
/// the email-recovery flow without it is a misconfig.
pub fn mailbox_domains() -> Vec<String> {
    crate::state::persistent_state(|p| {
        p.related_origins
            .as_ref()
            .map(|v| {
                v.iter()
                    .filter_map(|s| origin_to_host(s))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    })
}

/// Strip the scheme and any trailing path/port from `origin`,
/// returning the lowercased host. Returns `None` for inputs that
/// don't have a recognisable host part — a malformed
/// `related_origins` entry shouldn't trap the canister; we just
/// silently drop it from the accepted-domains list.
fn origin_to_host(origin: &str) -> Option<String> {
    let after_scheme = origin
        .strip_prefix("https://")
        .or_else(|| origin.strip_prefix("http://"))
        .unwrap_or(origin);
    let host_only = after_scheme
        .split(['/', ':'])
        .next()
        .filter(|s| !s.is_empty())?;
    Some(host_only.to_ascii_lowercase())
}

/// RFC 5321 §4.5.3.1 limits, applied at every address-handling
/// boundary (prepare-time validation, verified-`From:` extraction):
///
/// - `MAX_LOCAL_PART` — local-part SMTP limit (§4.5.3.1.1).
/// - `MAX_DOMAIN` — domain SMTP limit (§4.5.3.1.2).
/// - `MAX_ADDRESS` — addr-spec limit derived from the path limit
///   (§4.5.3.1.3) minus the `<` and `>` framing.
///
/// Without these caps a caller could submit a multi-KB string and
/// inflate the heap-resident pending-challenge map (and, post-PR-7,
/// the stable address→anchor reverse index).
pub const MAX_LOCAL_PART: usize = 64;
pub const MAX_DOMAIN: usize = 255;
pub const MAX_ADDRESS: usize = 254;

/// Caps on the cached TXT-record bytes the DNSSEC path stores on
/// each pending challenge. A real DKIM RSA-2048 record is ~430 bytes
/// (`p=` is base64 of a ~270-byte SPKI); RSA-4096 is ~770 bytes;
/// realistic published DMARC records are well under 500 bytes.
/// These caps are an order of magnitude above realistic values
/// without being so generous that an attacker who controls a signed
/// zone could pump multi-KB into every pending entry.
///
/// RFC 6376 / 7489 don't specify a maximum length for the records
/// themselves; these are pragmatic limits for the email-recovery
/// flow, not a protocol-level claim.
pub const MAX_DKIM_TXT_BYTES: usize = 4096;
pub const MAX_DMARC_TXT_BYTES: usize = 1024;

/// Cap on the FE-supplied `session_pk` length carried on a recovery
/// pending entry. Real session keys are well under 1 KB regardless
/// of algorithm (Ed25519 ~44 bytes, ECDSA P-256 ~91, RSA-2048 ~294);
/// this leaves headroom without giving an anonymous caller room to
/// inflate the challenge map.
pub const MAX_SESSION_KEY_BYTES: usize = 1024;
