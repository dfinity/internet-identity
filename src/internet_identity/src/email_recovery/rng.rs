//! Heap-resident PRNG seeded once per canister lifetime.
//!
//! `raw_rand` is an inter-canister call to the management canister:
//! it costs cycles and an async round trip. Doing one per nonce on
//! the hot prepare path is wasteful when ChaCha20Rng's output is
//! cryptographically strong. So we seed once, lazily, on first use,
//! and draw from that PRNG for every subsequent nonce.
//!
//! The PRNG state lives in a `thread_local!` (the canister is
//! single-threaded), held inside an `Option` so the very first call
//! triggers the async raw_rand fetch. All later calls are cheap heap
//! reads. Re-seeded automatically on canister upgrade — heap state
//! is wiped, so the next prepare call after upgrade hits the lazy-
//! init path and pulls a fresh seed.

use crate::random_salt;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::cell::RefCell;

thread_local! {
    /// Lazy-initialised PRNG. `None` until the first `prepare_*` call
    /// after canister start triggers `ensure_seeded()`.
    static EMAIL_RECOVERY_RNG: RefCell<Option<ChaCha20Rng>> =
        const { RefCell::new(None) };
}

/// Make sure the PRNG is seeded. Must be called from an async context
/// — `raw_rand` is an inter-canister call. Cheap once seeded: the
/// first `prepare_*` call per canister-lifetime pays the round trip,
/// every subsequent call is a single bool check.
pub async fn ensure_seeded() {
    let already = EMAIL_RECOVERY_RNG.with(|cell| cell.borrow().is_some());
    if already {
        return;
    }
    // Two simultaneous prepare calls hitting an unseeded PRNG would
    // both call raw_rand and the second's seed would overwrite the
    // first. That's harmless: the RNG output is still random either
    // way, and the cost is a wasted raw_rand call (rare — only on the
    // very first race after an upgrade).
    let seed = random_salt().await;
    let rng = ChaCha20Rng::from_seed(seed);
    EMAIL_RECOVERY_RNG.with(|cell| {
        *cell.borrow_mut() = Some(rng);
    });
}

/// Draw `NONCE_SUFFIX_BYTES` bytes of randomness from the PRNG. Must
/// be called only after [`ensure_seeded`].
///
/// Returns a fresh `[u8; NONCE_SUFFIX_BYTES]` — the caller hex-encodes
/// it and prepends the prefix to form the user-visible nonce.
pub fn draw_nonce_bytes() -> [u8; super::NONCE_SUFFIX_BYTES] {
    let mut bytes = [0u8; super::NONCE_SUFFIX_BYTES];
    EMAIL_RECOVERY_RNG.with(|cell| {
        let mut cell = cell.borrow_mut();
        let rng = cell
            .as_mut()
            .expect("RNG not seeded — call ensure_seeded() first");
        rng.fill_bytes(&mut bytes);
    });
    bytes
}

/// Compose the user-visible nonce from random bytes.
///
/// Format: `II-Recovery-{hex}` where `{hex}` is the random suffix
/// hex-encoded (lowercase, no separator). 16 hex chars total — fits
/// on a single line and copy-pastes cleanly into a mail-client
/// `Subject:` field.
pub fn format_nonce(suffix: &[u8; super::NONCE_SUFFIX_BYTES]) -> String {
    let mut out = String::with_capacity(super::NONCE_PREFIX.len() + suffix.len() * 2);
    out.push_str(super::NONCE_PREFIX);
    for b in suffix {
        out.push_str(&format!("{b:02x}"));
    }
    out
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;

    /// Bypass `ensure_seeded()` for unit tests — those run on the
    /// host, which has no `raw_rand`. Seeds the PRNG with a fixed
    /// value so test output is deterministic.
    pub(in crate::email_recovery) fn seed_for_tests(seed: [u8; 32]) {
        EMAIL_RECOVERY_RNG.with(|cell| {
            *cell.borrow_mut() = Some(ChaCha20Rng::from_seed(seed));
        });
    }

    #[test]
    fn nonce_format_matches_design_doc() {
        seed_for_tests([0u8; 32]);
        let bytes = draw_nonce_bytes();
        let nonce = format_nonce(&bytes);
        assert!(nonce.starts_with(super::super::NONCE_PREFIX));
        assert_eq!(
            nonce.len(),
            super::super::NONCE_PREFIX.len() + super::super::NONCE_SUFFIX_BYTES * 2
        );
        // Suffix should be lowercase hex.
        for c in nonce[super::super::NONCE_PREFIX.len()..].chars() {
            assert!(
                c.is_ascii_digit() || ('a'..='f').contains(&c),
                "non-hex char {c:?} in suffix",
            );
        }
    }

    #[test]
    fn distinct_seeds_produce_distinct_nonces() {
        seed_for_tests([1u8; 32]);
        let n1 = format_nonce(&draw_nonce_bytes());
        seed_for_tests([2u8; 32]);
        let n2 = format_nonce(&draw_nonce_bytes());
        assert_ne!(n1, n2, "different seeds must produce different nonces");
    }

    #[test]
    fn same_seed_distinct_draws_produce_distinct_nonces() {
        // The whole point of a PRNG is that successive draws differ.
        seed_for_tests([7u8; 32]);
        let n1 = format_nonce(&draw_nonce_bytes());
        let n2 = format_nonce(&draw_nonce_bytes());
        assert_ne!(n1, n2);
    }
}
