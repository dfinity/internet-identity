//! DoH's in-memory cache with concurrent-fetch deduplication.
//!
//! A specialisation of the IC single-flight cache
//! ([`SingleFlightCache`](crate::single_flight_cache::SingleFlightCache)):
//! the key is the FQDN (e.g. `selector1._domainkey.gmail.com`) and the
//! value is a [`DohRecord`] — either the fetched TXT bytes or a definitive
//! `NoAnswer`. It deduplicates concurrent fetches for the same domain so a
//! flurry of mail from one sender triggers a single five-provider fan-out,
//! and is heap-only — losing it on upgrade is fine, we just refetch.
//!
//! `super::fetch_txt` reads it through
//! [`get`](crate::single_flight_cache::get), which dedups the fan-out and
//! answers immediately — [`Ready`](crate::single_flight_cache::Cached::Ready)
//! with the record, or [`Pending`](crate::single_flight_cache::Cached::Pending)
//! while a detached fetch is in flight (see that module for the IC
//! reply-routing reason it's poll, not blocking).
//!
//! ## What counts as a cacheable answer vs. a failure
//!
//! A quorum result is one of three things, and they are handled
//! differently on purpose:
//!
//! - **TXT bytes** — cached as [`DohRecord::Txt`].
//! - **`NoAnswer`** — a *definitive* "no such record" verdict (3-of-5
//!   providers agree on NXDOMAIN/empty answer). It is cached as
//!   [`DohRecord::NoAnswer`], **not** treated as a failure: the DMARC path
//!   turns it into the strict-alignment fallback (design §6.3), so it must
//!   be a stable, shareable answer. Debouncing it (negative caching with a
//!   cooldown) would make a concurrent or follow-up verification see a
//!   transient error instead of the fallback.
//! - **Transient failure** (`AllProvidersFailed` / `QuorumFailed` / …) —
//!   surfaced as an `Err` from the fill, so the cache debounces it (below).
//!
//! ## Knobs for DKIM email recovery
//!
//! - **`fresh_for`** is the cache's freshness window, from the deploy-arg
//!   `max_cache_age_secs` (default 1 h when unset; the operator value is used
//!   verbatim), set on the cache in `new_doh_cache`. It is a canister-side
//!   lifetime, **not** the record's DNS TTL: the `transform` strips the TTL
//!   (it decrements per second, so replicas would disagree and break outcall
//!   consensus), and a DKIM/DMARC record carries no expiry of its own, so
//!   there is no record-derived window to honour.
//! - **Stale window ([`DOH_STALE_SECS`], 30 min).** A cached answer keeps
//!   being served past its TTL while a refresh is retried, so a transient
//!   DoH/quorum blip doesn't fail an in-progress recovery (stale-while-
//!   revalidate / stale-if-error). Sized to the 30-min challenge TTL so a
//!   recovery in flight can ride out an outage for its whole life; the
//!   security cost — trusting a possibly-rotated DKIM key up to 30 min
//!   longer — is negligible against DKIM's weeks-long rotation cadence.
//! - **Retry backoff ([`DOH_RETRY_BASE_SECS`] × [`DOH_RETRY_MULTIPLIER`]).**
//!   A *transient* failed fetch debounces the five-provider fan-out (1 min,
//!   then 2, 4, …, reset on success) instead of re-firing on every message.
//!   Only `Err` outcomes debounce — a definitive `NoAnswer` is cached as an
//!   answer (above), never debounced. For a domain already in cache,
//!   stale-serving covers the gap so the debounce never delays recovery;
//!   it only throttles the background re-fetch.
//! - **LRU cap ([`DOH_MAX_ENTRIES`]).** The key space is already
//!   allowlist-bounded, so this is generous headroom plus a hard memory
//!   bound that favours recently-seen domains.

use std::future::Future;

use super::types::{DohError, DEFAULT_CACHE_AGE_SECS};
use crate::single_flight_cache::{CacheConfig, RetryBackoff, SingleFlightCache};

/// A cached DoH answer: the TXT-record bytes, or a definitive "no such
/// record" verdict. Both are stable, shareable answers — see the module
/// docs for why `NoAnswer` is cached rather than treated as a failure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DohRecord {
    Txt(Vec<u8>),
    NoAnswer,
}

/// DoH cache: FQDN → [`DohRecord`], with concurrent-fetch dedup. Fills may
/// fail transiently with [`DohError`]; a definitive `NoAnswer` is a cached
/// answer, not a failure (see the module docs).
pub type DohCache = SingleFlightCache<String, DohRecord, DohError>;

/// Serve a cached answer up to 30 min past its TTL while refreshing
/// (stale-while-revalidate / stale-if-error). Matches the 30-min challenge
/// TTL, so a recovery in flight can ride out a DoH outage for its whole life
/// — see the module docs.
pub const DOH_STALE_SECS: u64 = 1800;

/// First transient failed fetch waits this long before the next attempt.
pub const DOH_RETRY_BASE_SECS: u64 = 60;

/// Each further consecutive transient failure multiplies the wait by this.
pub const DOH_RETRY_MULTIPLIER: u64 = 2;

/// Hard cap on cached FQDNs; over it, the least-recently-used is evicted.
pub const DOH_MAX_ENTRIES: usize = 256;

/// Take over an in-flight fetch whose owner trapped after the outcall once
/// it's this old. Far longer than a healthy 5-provider fan-out (a few
/// seconds), so a live fetch is never abandoned; short enough that a wedged
/// FQDN — which would otherwise block every verification for that domain
/// until the entry dies — recovers within a retry or two.
pub const DOH_ABANDON_FILL_AFTER_SECS: u64 = 120;

/// Construct the DoH cache around its shared `fill` (the five-provider
/// fan-out for one FQDN, defined in the parent module where the outcall
/// plumbing lives). The freshness window (`fresh_for`) is the deploy arg
/// `max_cache_age_secs` (default 1 h when unset; the operator-chosen value is
/// used verbatim), read here so the cache owns all its policy. Config only
/// changes on upgrade, which wipes this heap cache, so the value is stable for
/// the cache's lifetime.
pub fn new_doh_cache<Fut>(fill: impl Fn(String) -> Fut + 'static) -> DohCache
where
    Fut: Future<Output = Result<DohRecord, DohError>> + 'static,
{
    let fresh_for = crate::state::persistent_state(|p| {
        p.doh_config.as_ref().and_then(|c| c.max_cache_age_secs)
    })
    .unwrap_or(DEFAULT_CACHE_AGE_SECS);
    SingleFlightCache::new(
        fill,
        CacheConfig {
            fresh_for,
            stale_for: DOH_STALE_SECS,
            max_entries: DOH_MAX_ENTRIES,
            backoff: RetryBackoff::new(DOH_RETRY_BASE_SECS, DOH_RETRY_MULTIPLIER),
            abandon_fill_after: DOH_ABANDON_FILL_AFTER_SECS,
        },
    )
}
