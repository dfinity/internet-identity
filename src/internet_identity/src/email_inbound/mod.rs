//! Inbound DKIM-challenge primitive — shared infrastructure powering
//! both [`crate::email_recovery`] (recovery-as-login) and
//! [`crate::verified_emails`] (verified-email-as-attribute-source).
//!
//! Owns the pieces that don't care which flow they're serving:
//!
//! - The pending-challenge map in [`pending`] (TTL-bounded, in-heap;
//!   see `docs/ongoing/email-recovery.md` §8.8 for the size + eviction
//!   policy).
//! - The nonce PRNG and formatter in [`rng`].
//! - The inbound SMTP gateway dispatcher and `(Subject) → PendingKind →
//!   anchor-write` path in [`smtp`]. Walks both nonce prefixes and
//!   dispatches by `PendingKind`.
//! - The DKIM-leaf submission path in [`submit_leaf`].
//! - The shared `prepare_common` core in [`prepare`] — input
//!   validation, path picking (DNSSEC skeleton vs. DoH allowlist), and
//!   nonce issuing. Both flow-specific `prepare_add` wrappers (recovery
//!   and verified-email) call this directly.
//!
//! The two flow modules ([`crate::email_recovery`],
//! [`crate::verified_emails`]) sit on top: each owns its own
//! flow-specific entry points, its own anchor field, and the prefix
//! its nonces carry. The `PendingKind` variant attached to each
//! pending entry is what tells this module which flow a given inbound
//! message belongs to.
//!
//! # Why heap state, not stable
//!
//! The pending-challenge map has a 30-minute TTL and is a strict
//! optimisation — losing it on canister upgrade just costs every
//! in-flight registration its retry. We do **not** want stable-memory
//! churn for entries that expire in half an hour. The heap-side
//! `HashMap` is naturally bounded by the policy in §8.8 of the design
//! doc (size cap + oldest-first eviction).

#![allow(dead_code)] // Exposed as the module fills in.

pub(crate) mod pending;
pub(crate) mod prepare;
pub(crate) mod rng;
pub(crate) mod smtp;
mod submit_leaf;

pub use submit_leaf::{resolve_via_doh, submit_dkim_leaf};

#[allow(unused_imports)]
pub use pending::{PendingChallenge, PendingKind, PendingStatus};

pub use smtp::{handle_smtp_request, handle_smtp_request_validate};

/// Wrapper around `pending::status_of` so the canister method in
/// `main.rs` doesn't need to know which submodule the heap state
/// lives in.
pub fn pending_status(
    nonce: &str,
    now_secs: u64,
) -> internet_identity_interface::internet_identity::types::email_recovery::EmailChallengeStatus {
    pending::status_of(nonce, now_secs)
}

/// Wrapper around `pending::diagnostics_of` (see
/// `EmailChallengeDiagnostics`) so the canister method in `main.rs`
/// doesn't reach into the submodule. Returns `None` for an
/// unknown/expired nonce; the record carries strictly-public fields
/// only.
pub fn pending_diagnostics(
    nonce: &str,
    now_secs: u64,
) -> Option<
    internet_identity_interface::internet_identity::types::email_recovery::EmailChallengeDiagnostics,
>{
    pending::diagnostics_of(nonce, now_secs)
}

/// Whether the legacy DNSSEC email-recovery path is enabled. Off by
/// default — the canister runs DoH-only and `prepare_*` ignores any
/// client-supplied `dns_proof`.
pub fn dnssec_email_recovery_enabled() -> bool {
    crate::state::persistent_state(|p| p.enable_dnssec_email_recovery.unwrap_or(false))
}

/// Verbose human-readable token prefix for the **recovery** flow.
/// Every legitimate recovery token the canister hands out begins with
/// this exact byte sequence; the FE surfaces this prefix prominently
/// in the wizard so a phisher who hands the user a different-looking
/// string (no `II-Recovery-`, truncated, etc.) is immediately obvious.
pub const NONCE_PREFIX: &str = "II-Recovery-";

/// Like [`NONCE_PREFIX`] but for the **verified-email** wizard.
/// Differs from `NONCE_PREFIX` by a single token so an inbound
/// challenge email can never be cross-applied between the recovery
/// flow and the verified-email flow: `find_nonce_in` walks both
/// prefixes when scanning a Subject and the pending entry's
/// `PendingKind` ensures the match resolves to the right flow.
pub const VERIFIED_EMAIL_NONCE_PREFIX: &str = "II-Verify-";

/// Hard cap on the number of verified emails one anchor can hold.
/// Surfaced to the FE wizard so the panel can show a "limit
/// reached" notice instead of round-tripping. Picked tight (5)
/// because verified emails are attribute sources for dapps — most
/// users will have at most one or two; 5 leaves clear headroom for
/// work/personal/alias combinations without inviting credential
/// stuffing.
pub const MAX_VERIFIED_EMAILS_PER_ANCHOR: usize = 5;

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
///
/// **Doubles as the per-canister rate-limit on `prepare_add` /
/// `prepare_delegation`.** There is no per-anchor or per-caller
/// counter: at sustained call rate `R`, an attacker can hold at
/// most `min(R × CHALLENGE_TTL_SECS, MAX_PENDING_CHALLENGES)`
/// entries in flight. The cap + TTL together bound heap residency
/// to ≈ 10 k × 1 KB ≈ 10 MB — a small fraction of the canister's
/// heap budget. Legitimate users coexist because oldest-first
/// eviction targets stale attacker entries first whenever the map
/// fills (see `evict_oldest` in `pending.rs`).
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
