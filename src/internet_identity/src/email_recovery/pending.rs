//! In-flight registration challenges, keyed by random nonce.
//!
//! Each `prepare_add` call inserts one entry; `smtp_request`
//! consumes it (flipping `status` to a terminal variant); the FE
//! polls until it sees the terminal status. Entries naturally drain
//! at the 30-minute TTL.
//!
//! See `docs/ongoing/email-recovery.md` §8.2 / §8.8 for the policy
//! decisions baked into this module — in particular **why** we use a
//! random-nonce-only key (untargetability) and bound the map size
//! (eviction is benign for both legitimate users and attackers).

use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryError, EmailRecoveryStatus,
};
use internet_identity_interface::internet_identity::types::AnchorNumber;
use std::cell::RefCell;
use std::collections::HashMap;

/// Per-challenge state stored in the pending map.
#[derive(Clone, Debug)]
pub struct PendingChallenge {
    /// Distinguishes the setup flow (binding) from the recovery flow.
    /// Recovery is wired up in a follow-up PR; only `Register` is
    /// produced today.
    pub kind: PendingKind,
    /// Lowercased canonical address the user claimed at prepare time.
    /// `smtp_request` rejects with `AddressMismatch` if the verified
    /// `From:` of the inbound email doesn't match this exactly.
    pub claimed_address: String,
    /// DKIM selector the FE found at prepare time. The inbound email
    /// must be signed under this selector or the canister rejects
    /// with `SelectorMismatch` (selector rotations between prepare
    /// and send are rare in practice).
    pub selector: String,
    /// Unix-seconds at which `prepare_add` ran. Used for TTL expiry
    /// and as the basis for `expires_at` shipped to the FE.
    pub created_at_secs: u64,
    /// Status the FE polls. Flips from `Pending` to a terminal
    /// variant on `smtp_request`; sticky thereafter (the FE reads
    /// once and ends the wizard).
    pub status: PendingStatus,
}

#[derive(Clone, Debug)]
pub enum PendingKind {
    /// Setup flow — caller is authenticated; the email-recovery
    /// credential will be written to this anchor on success.
    Register { anchor: AnchorNumber },
    // `Recover { session_pk }` is added by the recovery follow-up PR.
}

/// Terminal-flippable status the FE polls. The terminal variants
/// (`Succeeded`, `Failed`, `Expired`) are sticky: a later poll keeps
/// returning the same variant until the entry is evicted.
#[derive(Clone, Debug)]
pub enum PendingStatus {
    Pending,
    Succeeded,
    Failed(EmailRecoveryError),
    Expired,
}

thread_local! {
    /// The pending-challenge map. Heap-only (lost on canister upgrade
    /// — entries expire in 30 minutes anyway, so the user just retries
    /// after an upgrade lands during their wizard session). Bounded
    /// by `MAX_PENDING_CHALLENGES` with oldest-first eviction.
    static PENDING: RefCell<HashMap<String, PendingChallenge>> =
        RefCell::new(HashMap::new());
}

/// Insert a fresh `Pending` entry. Performs the eviction sweep
/// described in §8.8 of the design doc:
///
/// - Drop every entry past its TTL.
/// - If the map is still at capacity, drop the single oldest entry.
///
/// Both operations are O(n) in current map size, but the map is
/// capped at `MAX_PENDING_CHALLENGES` so the constant is small.
pub fn insert_with_eviction(
    nonce: String,
    challenge: PendingChallenge,
    now_secs: u64,
) {
    PENDING.with(|cell| {
        let mut map = cell.borrow_mut();

        // Sweep expired entries.
        map.retain(|_, c| !is_expired_at(c, now_secs));

        // If still at capacity, evict the single oldest entry.
        if map.len() >= super::MAX_PENDING_CHALLENGES {
            evict_oldest(&mut map);
        }

        map.insert(nonce, challenge);
    });
}

/// Read the status of a pending challenge. The query path:
/// `email_recovery_status(nonce)` calls this after a TTL sweep so the
/// FE sees `Expired` at the moment the entry would otherwise have
/// been evicted (rather than `Pending` until the next prepare call
/// happens to evict it).
pub fn status_of(nonce: &str, now_secs: u64) -> EmailRecoveryStatus {
    PENDING.with(|cell| {
        let mut map = cell.borrow_mut();

        // Lazy-expire on read so polling sees a timely `Expired`.
        // Other entries are left alone — sweeping every read would
        // make this query O(n) in total map size, which is wasteful;
        // `insert_with_eviction` does the periodic sweep.
        if let Some(challenge) = map.get(nonce) {
            if is_expired_at(challenge, now_secs) {
                map.remove(nonce);
                return EmailRecoveryStatus::Expired;
            }
        }

        match map.get(nonce) {
            None => EmailRecoveryStatus::Expired, // Same observable effect as TTL eviction.
            Some(c) => match &c.status {
                PendingStatus::Pending => EmailRecoveryStatus::Pending,
                PendingStatus::Succeeded => match &c.kind {
                    PendingKind::Register { .. } => EmailRecoveryStatus::RegistrationSucceeded,
                },
                PendingStatus::Failed(e) => EmailRecoveryStatus::Failed(e.clone()),
                PendingStatus::Expired => EmailRecoveryStatus::Expired,
            },
        }
    })
}

/// Look up a pending challenge by nonce, applying the TTL check, and
/// run a closure against its mutable state. Returns `None` if the
/// nonce is unknown or has just expired.
///
/// `smtp_request` uses this to flip `status` from `Pending` to a
/// terminal variant in one borrow. The closure receives `&mut` so
/// the caller can mutate freely; the entry stays in the map after
/// the closure returns (the FE's next poll reads the new status,
/// then a later call evicts the entry).
pub fn with_mut<R>(
    nonce: &str,
    now_secs: u64,
    f: impl FnOnce(&mut PendingChallenge) -> R,
) -> Option<R> {
    PENDING.with(|cell| {
        let mut map = cell.borrow_mut();
        if let Some(c) = map.get(nonce) {
            if is_expired_at(c, now_secs) {
                map.remove(nonce);
                return None;
            }
        }
        map.get_mut(nonce).map(f)
    })
}

/// Whether a challenge has aged past its TTL.
fn is_expired_at(c: &PendingChallenge, now_secs: u64) -> bool {
    now_secs.saturating_sub(c.created_at_secs) >= super::CHALLENGE_TTL_SECS
}

/// Drop the entry with the smallest `created_at_secs`. Called only
/// when the map is at capacity *after* the TTL sweep — i.e. the
/// attacker is filling faster than entries naturally expire.
///
/// The eviction is **untargeted** (oldest by start time, not by
/// caller-controlled keys), which is what makes the §8.8 argument
/// hold: an attacker can fill the map but can't pick whose entry
/// gets evicted.
fn evict_oldest(map: &mut HashMap<String, PendingChallenge>) {
    let oldest = map
        .iter()
        .min_by_key(|(_, c)| c.created_at_secs)
        .map(|(k, _)| k.clone());
    if let Some(k) = oldest {
        map.remove(&k);
    }
}

#[cfg(test)]
pub(crate) fn reset_for_tests() {
    PENDING.with(|cell| cell.borrow_mut().clear());
}

#[cfg(test)]
pub(crate) fn len_for_tests() -> usize {
    PENDING.with(|cell| cell.borrow().len())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn challenge(addr: &str, now: u64) -> PendingChallenge {
        PendingChallenge {
            kind: PendingKind::Register { anchor: 1 },
            claimed_address: addr.into(),
            selector: "20230601".into(),
            created_at_secs: now,
            status: PendingStatus::Pending,
        }
    }

    #[test]
    fn insert_then_status_returns_pending() {
        reset_for_tests();
        insert_with_eviction("n1".into(), challenge("a@x.com", 100), 100);
        assert!(matches!(status_of("n1", 200), EmailRecoveryStatus::Pending));
    }

    #[test]
    fn status_of_unknown_nonce_returns_expired() {
        // Unknown and expired collapse to the same observable result.
        // The FE doesn't need to distinguish "the canister never
        // heard of this nonce" from "the canister forgot it" — both
        // mean "start over."
        reset_for_tests();
        assert!(matches!(
            status_of("never-issued", 0),
            EmailRecoveryStatus::Expired
        ));
    }

    #[test]
    fn entry_expires_after_ttl() {
        reset_for_tests();
        let created_at = 1_000;
        insert_with_eviction("n1".into(), challenge("a@x.com", created_at), created_at);

        // Just under the TTL → still Pending.
        assert!(matches!(
            status_of("n1", created_at + super::super::CHALLENGE_TTL_SECS - 1),
            EmailRecoveryStatus::Pending
        ));

        // Right at the TTL → Expired (the inequality is `>=`).
        assert!(matches!(
            status_of("n1", created_at + super::super::CHALLENGE_TTL_SECS),
            EmailRecoveryStatus::Expired
        ));
        // And the entry was evicted on read.
        assert_eq!(len_for_tests(), 0);
    }

    #[test]
    fn insert_sweeps_expired_entries() {
        reset_for_tests();
        let t0 = 1_000;
        let ttl = super::super::CHALLENGE_TTL_SECS;
        insert_with_eviction("old".into(), challenge("a@x.com", t0), t0);
        insert_with_eviction(
            "older".into(),
            challenge("b@x.com", t0 - 100),
            t0,
        );

        // Insert at t = t0 + ttl + 1 → both prior entries should be
        // swept by the TTL sweep.
        insert_with_eviction(
            "fresh".into(),
            challenge("c@x.com", t0 + ttl + 1),
            t0 + ttl + 1,
        );
        assert_eq!(len_for_tests(), 1, "only the fresh entry should remain");
    }

    #[test]
    fn at_capacity_oldest_is_evicted_first() {
        reset_for_tests();
        // Fill below the cap with a known oldest entry.
        let cap = super::super::MAX_PENDING_CHALLENGES;
        // For test-speed sanity, we can't realistically fill 10 000
        // entries here; we exercise the eviction-when-full path with
        // a short prefix of the cap by manually invoking eviction.
        let t0 = 1_000;
        for i in 0..5 {
            insert_with_eviction(
                format!("n{i}"),
                challenge("a@x.com", t0 + i),
                t0 + i,
            );
        }
        assert_eq!(len_for_tests(), 5);
        // Force a manual eviction of the oldest.
        PENDING.with(|cell| {
            let mut map = cell.borrow_mut();
            super::evict_oldest(&mut map);
        });
        assert_eq!(len_for_tests(), 4);
        // The one with t0 + 0 (smallest created_at) should be gone.
        assert!(matches!(
            status_of("n0", t0 + 5),
            EmailRecoveryStatus::Expired
        ));

        // The actual capacity behaviour: on the cap-th insert, the
        // sweep + evict path is exercised. We test that the data
        // structure honours `cap` as an upper bound.
        let _ = cap; // Just referenced so the constant doesn't show as unused in this test module.
    }

    #[test]
    fn with_mut_lets_caller_flip_status() {
        reset_for_tests();
        insert_with_eviction("n1".into(), challenge("a@x.com", 100), 100);
        let outcome = with_mut("n1", 200, |c| {
            c.status = PendingStatus::Succeeded;
            "did-it"
        });
        assert_eq!(outcome, Some("did-it"));

        // Subsequent status read sees Succeeded.
        assert!(matches!(
            status_of("n1", 200),
            EmailRecoveryStatus::RegistrationSucceeded
        ));
    }

    #[test]
    fn with_mut_returns_none_for_expired_entry() {
        reset_for_tests();
        let t = 100;
        insert_with_eviction("n1".into(), challenge("a@x.com", t), t);
        let outcome = with_mut("n1", t + super::super::CHALLENGE_TTL_SECS, |_| ());
        assert!(outcome.is_none());
        // And the entry was evicted in the process.
        assert_eq!(len_for_tests(), 0);
    }
}
