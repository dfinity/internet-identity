//! The randomness a subscribe draws on, held as one pool rather than one draw per call.
//!
//! Both keys a subscribe needs are secrets, and `raw_rand` is the only place a canister
//! can get one. Calling it per request would make the key fetch an awaiting call, with
//! state to re-check on the far side of the await and a management call standing between
//! a user and the browser permission prompt. One pooled value removes both: a call that
//! finds the pool warm derives what it needs and returns as a single message.
//!
//! The pool is never handed out. What a call uses is derived from it, the browser key and
//! the time, so two calls never share a key even though they share the pool. Those mixed
//! inputs are public, and need only be distinct per call; all of the secrecy is the pool's,
//! and the pool stays here.

use candid::Principal;
use ic_cdk::api::call::call;
use ic_cdk::trap;
use sha2::{Digest, Sha256};
use std::cell::{Cell, RefCell};

use internet_identity_interface::internet_identity::types::Timestamp;

const SECOND_NS: u64 = 1_000_000_000;

/// Past this a value is served while a fresh one is drawn behind it, which is what keeps
/// the refresh off the path of the call that noticed.
const REFRESH_AFTER_NS: u64 = 5 * 60 * SECOND_NS;

/// Past this nothing is served. A caller arriving here waits for a draw, which only
/// happens when nobody has subscribed for twice the refresh window.
const MAX_AGE_NS: u64 = 10 * 60 * SECOND_NS;

/// Domain separators, so the two halves a call derives cannot be each other.
const TRANSPORT_INFO: &[u8] = b"II webpush transport key\0";
const SENDER_INFO: &[u8] = b"II webpush sender seed\0";

#[derive(Clone, Copy)]
struct Pool {
    bytes: [u8; 32],
    drawn_at_ns: Timestamp,
}

thread_local! {
    static POOL: RefCell<Option<Pool>> = const { RefCell::new(None) };
    /// One refresh at a time. Without it every caller served a stale value would start
    /// its own `raw_rand` behind it. A cold pool is not covered, because a caller there
    /// has nothing to be served and has to wait for a draw either way.
    static DRAWING: Cell<bool> = const { Cell::new(false) };
}

/// What one subscribe derives from the pool: the keypair the browser encrypts `auth` to,
/// and the seed that becomes this subscription's first sender key.
pub(crate) struct CallBytes {
    pub transport_scalar: [u8; 32],
    pub sender_seed: [u8; 32],
}

/// Answers this call's own bytes, drawing a pool value first only where none can be
/// served.
pub(crate) async fn call_bytes(browser_key: &[u8], now_ns: Timestamp) -> Result<CallBytes, String> {
    let held = POOL.with_borrow(|pool| *pool);
    let usable = held.filter(|pool| now_ns.saturating_sub(pool.drawn_at_ns) <= MAX_AGE_NS);

    let pool = match usable {
        Some(pool) => {
            if now_ns.saturating_sub(pool.drawn_at_ns) > REFRESH_AFTER_NS {
                spawn_draw();
            }
            pool
        }
        None => draw(now_ns).await?,
    };

    Ok(derive(&pool.bytes, browser_key, now_ns))
}

/// Replaces the pool behind a caller that was served a stale value.
///
/// A failure clears the flag and leaves the old value in place: the next caller tries
/// again, and one that arrives past [`MAX_AGE_NS`] draws for itself. A flag left set by a
/// failed draw would wedge every later refresh.
fn spawn_draw() {
    if DRAWING.get() {
        return;
    }
    DRAWING.set(true);
    ic_cdk::spawn(async {
        let drawn = raw_rand().await;
        DRAWING.set(false);
        match drawn {
            Ok(bytes) => {
                store(bytes, ic_cdk::api::time());
            }
            Err(err) => ic_cdk::println!("Could not refresh the notification randomness: {err}"),
        }
    });
}

async fn draw(now_ns: Timestamp) -> Result<Pool, String> {
    // A concurrent refresh may have landed while this caller was deciding to draw.
    if DRAWING.get() {
        if let Some(pool) = POOL
            .with_borrow(|pool| *pool)
            .filter(|pool| now_ns.saturating_sub(pool.drawn_at_ns) <= MAX_AGE_NS)
        {
            return Ok(pool);
        }
    }
    let bytes = raw_rand().await?;
    Ok(store(bytes, now_ns))
}

fn store(bytes: [u8; 32], now_ns: Timestamp) -> Pool {
    let pool = Pool {
        bytes,
        drawn_at_ns: now_ns,
    };
    POOL.with_borrow_mut(|held| *held = Some(pool));
    pool
}

/// 32 fresh bytes, straight from the subnet.
///
/// A dispatch pass calls this directly rather than through the pool: its salts are these
/// bytes with each wake-up's place in the pass, and that place starts again at zero every
/// pass, so two passes sharing bytes would seal two different payloads under one salt.
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

/// Two tagged hashes rather than one expansion: what this needs is distinctness per call,
/// which a hash gives for a fraction of an HKDF.
fn derive(pool: &[u8; 32], browser_key: &[u8], now_ns: Timestamp) -> CallBytes {
    let half = |info: &[u8]| -> [u8; 32] {
        Sha256::new()
            .chain_update(info)
            .chain_update(pool)
            .chain_update(browser_key)
            .chain_update(now_ns.to_be_bytes())
            .finalize()
            .into()
    };
    CallBytes {
        transport_scalar: half(TRANSPORT_INFO),
        sender_seed: half(SENDER_INFO),
    }
}

/// A scalar a P-256 key can be built from, which all but one in 2^128 digests already is.
pub(crate) fn secret_key_from(scalar: &[u8; 32]) -> p256::SecretKey {
    p256::SecretKey::from_slice(scalar).unwrap_or_else(|_| {
        // The alternative is refusing a subscribe over a digest that happened to land
        // outside the field order, which no caller could act on and nobody will see.
        trap("a derived scalar is not a P-256 key")
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn pool_of(byte: u8) -> [u8; 32] {
        [byte; 32]
    }

    #[test]
    fn two_browsers_sharing_a_pool_do_not_share_a_key() {
        let pool = pool_of(1);
        let first = derive(&pool, b"browser-a", 10);
        let second = derive(&pool, b"browser-b", 10);
        assert_ne!(first.transport_scalar, second.transport_scalar);
        assert_ne!(first.sender_seed, second.sender_seed);
    }

    #[test]
    fn one_browser_at_two_times_does_not_repeat_a_key() {
        let pool = pool_of(1);
        assert_ne!(
            derive(&pool, b"browser-a", 10).sender_seed,
            derive(&pool, b"browser-a", 11).sender_seed
        );
    }

    /// The two halves come from one pool value, so a derivation that did not separate
    /// them would hand the same secret to both uses.
    #[test]
    fn the_transport_key_and_the_sender_seed_are_not_the_same_bytes() {
        let derived = derive(&pool_of(1), b"browser-a", 10);
        assert_ne!(derived.transport_scalar, derived.sender_seed);
    }

    #[test]
    fn a_refilled_pool_derives_differently_for_the_same_call() {
        assert_ne!(
            derive(&pool_of(1), b"browser-a", 10).transport_scalar,
            derive(&pool_of(2), b"browser-a", 10).transport_scalar
        );
    }

    #[test]
    fn the_same_inputs_derive_the_same_bytes() {
        assert_eq!(
            derive(&pool_of(3), b"browser-a", 7).sender_seed,
            derive(&pool_of(3), b"browser-a", 7).sender_seed
        );
    }
}
