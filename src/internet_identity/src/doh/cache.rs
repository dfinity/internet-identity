//! DoH's in-memory cache with concurrent-fetch deduplication.
//!
//! A thin specialisation of the IC single-flight cache
//! ([`SingleFlightCache`](crate::single_flight_cache::SingleFlightCache)):
//! the key is the FQDN (e.g. `selector1._domainkey.gmail.com`) and the
//! value is the fetched TXT-record bytes. The cache keeps a record around
//! until the deploy-arg `max_cache_age_secs` (default 1 h, capped at 24 h)
//! elapses, and deduplicates concurrent fetches for the same domain so a
//! flurry of mail from one sender triggers a single five-provider fan-out.
//!
//! `super::fetch_txt` drives it through
//! [`get_or_fill`](crate::single_flight_cache::get_or_fill), which owns
//! the lookup/wait/publish dance — including the IC-specific reason
//! waiters poll from their own call context rather than being woken (see
//! that module's docs). The cache is heap-only: losing it on upgrade is
//! fine, we just refetch the next time mail arrives.

use crate::single_flight_cache::SingleFlightCache;

/// DoH cache: FQDN → TXT-record bytes, with concurrent-fetch dedup.
pub type DohCache = SingleFlightCache<String, Vec<u8>>;
