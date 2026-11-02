//! Where a subscribe's two private keys come from.
//!
//! Both are secrets, and `raw_rand` is the only place a canister can get one. Drawing per
//! request would put a management call between a user and the browser's permission
//! prompt, so one drawn value is pooled and every subscribe derives its own keys from it.
//!
//! The pool is never handed out. A call's keys are derived from it, from who is calling
//! and from the time, so two calls never share a key even though they share the pool.
//! Those mixed inputs are public and need only be distinct per call; all of the secrecy
//! is the pool's, and the pool stays here.

use candid::Principal;
use ic_cdk::api::call::call;
use ic_cdk::trap;
use sha2::{Digest, Sha256};
use std::cell::RefCell;

use crate::authz_utils::NotificationKey;
use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

/// How long a drawn value is served before a redraw is due.
const FRESH_FOR_SECONDS: u64 = 5 * 60;

/// How long it is still served past that while the redraw runs behind it, so a caller
/// arriving at a stale pool is answered rather than made to wait.
const STALE_FOR_SECONDS: u64 = 5 * 60;

/// A redraw that fails leaves the old value serving out the rest of that window.
const STALE_IF_ERROR_FOR_SECONDS: u64 = 5 * 60;

/// `raw_rand` answers within a round, so a claim older than this belongs to a task that
/// trapped rather than one still working.
const ABANDON_FILL_AFTER_SECONDS: u64 = 30;

const RETRY_BASE_SECONDS: u64 = 1;
const RETRY_MULTIPLIER: u64 = 2;

/// Domain separators, so the two keys a call derives cannot be each other.
const TRANSPORT_INFO: &[u8] = b"II webpush transport key\0";
const SENDER_INFO: &[u8] = b"II webpush sender seed\0";

/// One pooled value, so one key.
///
/// The cache is what makes this single-flight, and it is the same one the OpenID and DoH
/// caches use: concurrent callers share the draw already in flight rather than each
/// starting one, a draw whose task traps is taken over instead of wedging every later
/// caller, and a failing `raw_rand` backs off instead of being re-issued per request.
type PoolCache = SingleFlightCache<(), [u8; 32], String>;

thread_local! {
    static POOL: RefCell<PoolCache> = RefCell::new(new_pool_cache());
}

fn new_pool_cache() -> PoolCache {
    SingleFlightCache::new(
        |()| async {
            match raw_rand().await {
                Ok(bytes) => FillOutcome::Ready(bytes),
                Err(err) => FillOutcome::Failed(err),
            }
        },
        CacheConfig {
            fresh_for: FRESH_FOR_SECONDS,
            stale_for: STALE_FOR_SECONDS,
            stale_if_error_for: STALE_IF_ERROR_FOR_SECONDS,
            max_entries: 1,
            backoff: RetryBackoff::new(RETRY_BASE_SECONDS, RETRY_MULTIPLIER),
            abandon_fill_after: ABANDON_FILL_AFTER_SECONDS,
        },
    )
}

/// The two private keys one subscribe needs: the one the browser encrypts `auth` to, and
/// the seed that becomes this subscription's first sender key.
pub(crate) struct SubscribeSeeds {
    pub transport_scalar: [u8; 32],
    pub sender_seed: [u8; 32],
}

/// This caller's own seeds, or `None` where the pool has nothing servable yet.
///
/// `None` only on a cold pool, which means nobody has subscribed for the length of both
/// windows above. The draw is already in flight by the time this answers, so a caller
/// told to come back finds it warm.
pub(crate) fn seeds_for(
    anchor_number: AnchorNumber,
    key: NotificationKey,
    now_ns: Timestamp,
) -> Option<SubscribeSeeds> {
    match single_flight_cache::get(&POOL, ()) {
        Cached::Ready(pool) => Some(derive(&pool, &caller_bytes(anchor_number, key), now_ns)),
        Cached::Pending => None,
    }
}

/// 32 fresh bytes, straight from the subnet.
///
/// A dispatch pass draws its own this way rather than through the pool: its salts are
/// those bytes with each wake-up's place in the pass, and that place starts again at zero
/// every pass, so two passes sharing bytes would seal two payloads under one salt.
pub(crate) async fn raw_rand() -> Result<[u8; 32], String> {
    let drawn: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((drawn,)) => drawn,
        Err((_, err)) => return Err(format!("raw_rand: {err}")),
    };
    drawn
        .as_slice()
        .try_into()
        .map_err(|_| format!("raw_rand answered {} bytes, not 32", drawn.len()))
}

/// Who is subscribing, as bytes the derivation can mix in. Tagged and carrying the
/// identity, because a browser id is handed out per identity, so two of them reach 1.
fn caller_bytes(anchor_number: AnchorNumber, key: NotificationKey) -> Vec<u8> {
    let (tag, id) = match key {
        NotificationKey::Browser(id) => (0u8, u64::from(id)),
        NotificationKey::App(id) => (1u8, id),
    };
    [
        &[tag][..],
        &anchor_number.to_be_bytes()[..],
        &id.to_be_bytes()[..],
    ]
    .concat()
}

/// Two tagged hashes rather than one expansion: what this needs is distinctness per call,
/// which a hash gives for a fraction of an HKDF.
fn derive(pool: &[u8; 32], caller: &[u8], now_ns: Timestamp) -> SubscribeSeeds {
    let half = |info: &[u8]| -> [u8; 32] {
        Sha256::new()
            .chain_update(info)
            .chain_update(pool)
            .chain_update(caller)
            .chain_update(now_ns.to_be_bytes())
            .finalize()
            .into()
    };
    SubscribeSeeds {
        transport_scalar: half(TRANSPORT_INFO),
        sender_seed: half(SENDER_INFO),
    }
}

/// The P-256 key a seed stands for, which all but one seed in 2^128 already is.
///
/// Here because every caller is turning seed material from this module into a key: the
/// transport key a subscribe opens `auth` with, the sender key it derives against, and
/// the one a dispatch pass mints from its own draw.
pub(crate) fn secret_key_from(seed: &[u8; 32]) -> p256::SecretKey {
    p256::SecretKey::from_slice(seed).unwrap_or_else(|_| {
        // The alternative is refusing a subscribe over a digest that happened to land
        // outside the field order, which no caller could act on and nobody will see.
        trap("a derived seed is not a P-256 key")
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const BROWSER: NotificationKey = NotificationKey::Browser(1);
    const ANCHOR: AnchorNumber = 10_000;

    fn seeds(
        pool: u8,
        anchor_number: AnchorNumber,
        key: NotificationKey,
        now_ns: Timestamp,
    ) -> SubscribeSeeds {
        derive(&[pool; 32], &caller_bytes(anchor_number, key), now_ns)
    }

    #[test]
    fn two_callers_sharing_a_pool_do_not_share_a_key() {
        let first = seeds(1, ANCHOR, BROWSER, 10);
        let second = seeds(1, ANCHOR, NotificationKey::Browser(2), 10);
        assert_ne!(first.transport_scalar, second.transport_scalar);
        assert_ne!(first.sender_seed, second.sender_seed);
    }

    /// Browser ids are handed out per identity, so without the identity in the mix two
    /// identities' first browsers would derive the same transport key.
    #[test]
    fn the_same_browser_number_under_two_identities_derives_differently() {
        assert_ne!(
            seeds(1, 10_000, BROWSER, 10).transport_scalar,
            seeds(1, 10_001, BROWSER, 10).transport_scalar
        );
    }

    /// A browser and a Home Screen app can hold the same number and are not one caller.
    #[test]
    fn a_browser_and_an_app_at_one_number_derive_differently() {
        assert_ne!(
            seeds(1, ANCHOR, NotificationKey::Browser(1), 10).transport_scalar,
            seeds(1, ANCHOR, NotificationKey::App(1), 10).transport_scalar
        );
    }

    #[test]
    fn one_caller_at_two_times_does_not_repeat_a_key() {
        assert_ne!(
            seeds(1, ANCHOR, BROWSER, 10).sender_seed,
            seeds(1, ANCHOR, BROWSER, 11).sender_seed
        );
    }

    /// Both come off one pooled value, so a derivation that did not separate them would
    /// hand the same secret to both uses.
    #[test]
    fn the_transport_key_and_the_sender_seed_are_not_the_same_bytes() {
        let derived = seeds(1, ANCHOR, BROWSER, 10);
        assert_ne!(derived.transport_scalar, derived.sender_seed);
    }

    #[test]
    fn a_redrawn_pool_derives_differently_for_the_same_call() {
        assert_ne!(
            seeds(1, ANCHOR, BROWSER, 10).transport_scalar,
            seeds(2, ANCHOR, BROWSER, 10).transport_scalar
        );
    }

    #[test]
    fn the_same_inputs_derive_the_same_seeds() {
        assert_eq!(
            seeds(3, ANCHOR, BROWSER, 7).sender_seed,
            seeds(3, ANCHOR, BROWSER, 7).sender_seed
        );
    }
}
