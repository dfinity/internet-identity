//! DoH's in-memory cache with concurrent-fetch deduplication.
//!
//! A specialisation of the IC single-flight cache
//! ([`SingleFlightCache`](crate::single_flight_cache::SingleFlightCache)):
//! the key is the FQDN (e.g. `selector1._domainkey.gmail.com`) and the
//! value is the fetched TXT-record bytes. It deduplicates concurrent
//! fetches for the same domain so a flurry of mail from one sender
//! triggers a single five-provider fan-out, and is heap-only — losing it
//! on upgrade is fine, we just refetch the next time mail arrives.
//!
//! `super::fetch_txt` drives it through
//! [`get_or_fill`](crate::single_flight_cache::get_or_fill), which owns the
//! lookup/wait/publish dance (including the IC-specific reason waiters poll
//! from their own call context rather than being woken — see that module).
//!
//! ## Knobs for DKIM email recovery
//!
//! - **`ttl`** is per-fetch, from the deploy-arg `max_cache_age_secs`
//!   (default 1 h, capped 24 h) — passed by `fetch_txt`, not fixed here.
//!   It is a canister-side lifetime, **not** the record's DNS TTL: the
//!   `transform` strips the TTL (it decrements per second, so replicas
//!   would disagree and break outcall consensus), and a DKIM/DMARC record
//!   carries no expiry of its own, so there is no record-derived window to
//!   honour.
//! - **Stale window ([`DOH_STALE_SECS`], 10 min).** A fetched DKIM/DMARC
//!   record keeps being served for a short window past its TTL while a
//!   refresh is retried, so a transient DoH/quorum blip doesn't fail an
//!   in-progress email recovery. The window is deliberately short: serving
//!   a *stale* DKIM key trusts a possibly-rotated key a little longer, and
//!   10 min is negligible against DNS propagation / selector-overlap while
//!   still riding out a brief outage (a forgery would still need the
//!   domain's old private key).
//! - **Retry backoff ([`DOH_RETRY_BASE_SECS`] × [`DOH_RETRY_MULTIPLIER`]).**
//!   A failed refetch debounces the five-provider fan-out instead of
//!   re-firing it on every arriving message.
//! - **LRU cap ([`DOH_MAX_ENTRIES`]).** The key space is already
//!   allowlist-bounded, so this is generous headroom plus a hard memory
//!   bound that favours recently-seen domains.

use crate::single_flight_cache::{RetryBackoff, SingleFlightCache};

/// DoH cache: FQDN → TXT-record bytes, with concurrent-fetch dedup.
pub type DohCache = SingleFlightCache<String, Vec<u8>>;

/// Serve a fetched record up to 10 min past its TTL while refreshing
/// (stale-while-revalidate). Short on purpose — see the module docs.
pub const DOH_STALE_SECS: u64 = 600;

/// First failed refetch waits this long before the next attempt.
pub const DOH_RETRY_BASE_SECS: u64 = 60;

/// Each further consecutive failed refetch multiplies the wait by this.
pub const DOH_RETRY_MULTIPLIER: u64 = 2;

/// Hard cap on cached FQDNs; over it, the least-recently-used is evicted.
pub const DOH_MAX_ENTRIES: usize = 256;

/// Construct the DoH cache with the knobs above.
pub fn new_doh_cache() -> DohCache {
    SingleFlightCache::new()
        .with_stale_secs(DOH_STALE_SECS)
        .with_max_entries(DOH_MAX_ENTRIES)
        .with_retry_backoff(RetryBackoff::new(DOH_RETRY_BASE_SECS, DOH_RETRY_MULTIPLIER))
}
