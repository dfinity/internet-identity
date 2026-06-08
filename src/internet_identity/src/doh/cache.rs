//! DoH's in-memory cache with concurrent-fetch deduplication.
//!
//! This is a thin specialisation of the generic
//! [`SingleFlightCache`](crate::single_flight_cache::SingleFlightCache):
//! the key is the FQDN (e.g. `selector1._domainkey.gmail.com`) and the
//! value is the fetched TXT-record bytes. It gives the DoH layer:
//!
//! 1. **Caching** — a fetched TXT record stays around until the
//!    deploy-arg `max_cache_age_secs` (default 1 h, capped at 24 h)
//!    elapses, so a flurry of mail from the same domain doesn't re-fetch
//!    on every message. The cache is heap-only — losing it on upgrade is
//!    fine, we just refetch the next time mail arrives.
//!
//! 2. **Dedup** — when several concurrent `smtp_request` calls arrive for
//!    the same domain and the cache is cold, only the first fans out to
//!    the providers. The rest observe an in-flight marker
//!    ([`CacheState::Pending`]) and re-check the cache once it publishes,
//!    rather than each firing their own five-provider fan-out.
//!
//! The waiters do **not** block on a shared future woken by the fetcher.
//! The canister runs a single wasm thread, and waking another call's task
//! runs that task to completion *inside the fetcher's call context* — so
//! the woken waiter's reply is routed to the wrong call
//! (`ic0.msg_reply ... already replied`) and the real owner is left
//! unreplied. Instead each waiter polls this cache from its OWN call
//! context, yielding a round via a cheap downstream call between checks
//! (see `super::fetch_txt`). The generic cache holds no `Waker`s and no
//! shared futures precisely so that nothing drives one call's task from
//! another's.
//!
//! The default staleness window
//! ([`DEFAULT_PENDING_STALE_AFTER_SECS`](crate::single_flight_cache::DEFAULT_PENDING_STALE_AFTER_SECS),
//! used via [`DohCache::default`]) fits DoH well: the IC commits state
//! changes made *before* an `.await`, so if a fetcher's post-outcall
//! continuation traps its in-flight marker survives, and later arrivals
//! would otherwise wait on it forever. 120 s is comfortably longer than
//! any plausible HTTP-outcall round trip yet quick enough that genuinely
//! stuck entries clear within a couple of email retries.

/// What a DoH cache lookup returned. Re-exported so call sites read in
/// DoH terms.
pub use crate::single_flight_cache::CacheState;

use crate::single_flight_cache::SingleFlightCache;

/// DoH cache: FQDN → TXT-record bytes, with concurrent-fetch dedup.
pub type DohCache = SingleFlightCache<String, Vec<u8>>;
