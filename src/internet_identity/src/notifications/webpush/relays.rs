//! Which relay an origin belongs to, and what Internet Identity will sign for it.
//!
//! A subscription names the relay it is delivered through, and the browser chooses that,
//! not us: whatever origin the endpoint carries is where a wake-up has to go. Signing for
//! an origin costs a threshold signature, so an origin nobody can reach is a cost with no
//! delivery at the end of it.
//!
//! Origins fall into three populations, and the point of separating them is that one
//! population's traffic cannot take what another needs. Each has its own caches and its
//! own share of the budgets:
//!
//! - The relays we name outright. Their reachability is asserted rather than tested, and
//!   their signatures are held in a cache sized to the list, so nothing evicts them.
//! - Hosts under a relay operator's own domain. The operator alone controls what resolves
//!   there, so reaching one proves it is theirs, which is what makes the test worth
//!   making here.
//! - Everything else. The same test only filters dead and mistyped endpoints, because
//!   whoever registered the endpoint may well own the domain and answer from it.
//!
//! Reachability and signing are separate caches rather than one with a flag, so that an
//! origin which has never answered holds no signature and takes no signature slot. The
//! reachability cache's own value-less failure marker is what records "did not answer",
//! and [`crate::single_flight_cache`] parks that marker under backoff, so a name that
//! does not resolve is not re-probed on every pass.

use std::cell::RefCell;
use std::thread::LocalKey;

use super::vapid_key::{self, JwtCache, SignedJwt};
use crate::concurrency::{ConcurrencyLimiter, Limiter, LimiterConfig};
use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};

/// The relays a browser subscribes through in practice, each the whole origin a wake-up
/// is posted to. These are not probed: their operators publish them, and a probe would
/// only report what the wake-up itself reports a moment later.
const KNOWN_RELAY_ORIGINS: [&str; 3] = [
    "https://fcm.googleapis.com",
    "https://updates.push.services.mozilla.com",
    "https://web.push.apple.com",
];

/// Domains where the relay operator alone decides what resolves. Apple tells senders to
/// allow any subdomain of `push.apple.com` and states that it does not publish which
/// names exist, because they change — so the hosts can be recognised but not listed.
///
/// Microsoft is absent on purpose. WNS publishes no host pattern that can be checked, and
/// a guessed suffix here would hand whoever guessed it the protected tier, so a Windows
/// subscription is treated as [`Tier::Unknown`] until a real endpoint supplies the name.
const KNOWN_RELAY_SUFFIXES: [&str; 1] = [".push.apple.com"];

/// Which population an origin belongs to.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Tier {
    Known,
    KnownSubdomain,
    Unknown,
}

/// The host an origin names, for a `https` origin. [`vapid_key::relay_origin_of`] builds
/// these from a validated endpoint, so the scheme is the one a push may use at all.
fn host_of(relay_origin: &str) -> Option<&str> {
    let authority = relay_origin.strip_prefix("https://")?;
    Some(
        authority
            .split_once(':')
            .map_or(authority, |(host, _)| host),
    )
}

/// The tier `relay_origin` falls into.
///
/// A suffix is matched against the host and carries its leading dot, so it matches a
/// subdomain and nothing else: `evilpush.apple.com` does not end in `.push.apple.com`,
/// and neither does `web.push.apple.com.example`, whose host ends in `.example`.
fn tier_of(relay_origin: &str) -> Tier {
    if KNOWN_RELAY_ORIGINS.contains(&relay_origin) {
        return Tier::Known;
    }
    match host_of(relay_origin) {
        Some(host) if KNOWN_RELAY_SUFFIXES.iter().any(|end| host.ends_with(end)) => {
            Tier::KnownSubdomain
        }
        _ => Tier::Unknown,
    }
}

/// What a probe learns: the status the relay answered with. Any status at all is the
/// whole of it — a relay answering `404` for a path it has no subscription at has still
/// proved it is there, which is the only question being asked.
type ReachabilityCache = SingleFlightCache<String, u16, String>;

/// Origins held per reachability cache. Sized for the hosts a relay operator actually
/// runs, not against a flood: a new name defeats any size, so what bounds a flood is the
/// probe budget, and the cache is only here to stop re-probing what has already answered.
const REACHABILITY_MAX_ORIGINS: usize = 256;

/// How long an answer stands before it is asked for again. Long, because what it tests
/// changes on the scale of a relay operator retiring a host, and a wake-up to a host that
/// has since gone simply fails, which costs the one notification and nothing else.
const REACHABILITY_FRESH_FOR_SECS: u64 = 24 * 60 * 60;

/// How long the answer in hand goes on being served while the next probe runs.
const REACHABILITY_STALE_FOR_SECS: u64 = 24 * 60 * 60;

/// For a known relay's subdomains this window is the one that costs an attacker: it is
/// how long a failure marker and its backoff survive before the backoff resets, so an
/// invented name stays parked for a day rather than being retried.
const SUBDOMAIN_REACHABILITY_STALE_IF_ERROR_FOR_SECS: u64 = 24 * 60 * 60;

/// For everything else the same window is kept short. A probe there filters nothing an
/// attacker cannot pass anyway, so a long marker would only hold out a small relay that
/// had an outage.
const UNKNOWN_REACHABILITY_STALE_IF_ERROR_FOR_SECS: u64 = 60 * 60;

const REACHABILITY_RETRY_BASE_SECS: u64 = 60;
const REACHABILITY_RETRY_MULTIPLIER: u64 = 2;

/// Comfortably past an outcall's own timeout, so a probe is taken over only once the task
/// carrying it is gone.
const REACHABILITY_ABANDON_FILL_AFTER_SECS: u64 = 120;

/// Signatures held for a known relay: the list is the bound, so eviction never runs.
const KNOWN_JWT_MAX_RELAYS: usize = KNOWN_RELAY_ORIGINS.len();

/// Signatures held per probed tier. Only origins that have answered reach these, and a
/// tier of its own means one population's churn cannot evict another's token and make it
/// pay for a second signature.
const SUBDOMAIN_JWT_MAX_RELAYS: usize = 32;
const UNKNOWN_JWT_MAX_RELAYS: usize = 32;

/// Probes in flight per tier. Small, because a probe is what a *new* origin costs and new
/// origins arrive when a relay operator rotates a name, not when a notification is sent.
const PROBE_MAX_CONCURRENT: usize = 8;

/// Signatures in flight per probed tier.
const SIGNING_MAX_CONCURRENT: usize = 8;

/// Known relays sign for a fixed list, so their budget is that list.
const KNOWN_SIGNING_MAX_CONCURRENT: usize = KNOWN_RELAY_ORIGINS.len();

/// Above the round trip of what these guard, so a live call is never reclaimed early.
const PERMIT_MAX_AGE_SECS: u64 = 90;

macro_rules! limiter {
    ($name:ident, $max_concurrent:expr) => {
        thread_local! {
            static $name: RefCell<ConcurrencyLimiter> =
                RefCell::new(ConcurrencyLimiter::new(LimiterConfig {
                    max_concurrent: $max_concurrent,
                    max_age_secs: PERMIT_MAX_AGE_SECS,
                }));
        }
    };
}

limiter!(SUBDOMAIN_PROBE_LIMIT, PROBE_MAX_CONCURRENT);
limiter!(UNKNOWN_PROBE_LIMIT, PROBE_MAX_CONCURRENT);
limiter!(KNOWN_SIGNING_LIMIT, KNOWN_SIGNING_MAX_CONCURRENT);
limiter!(SUBDOMAIN_SIGNING_LIMIT, SIGNING_MAX_CONCURRENT);
limiter!(UNKNOWN_SIGNING_LIMIT, SIGNING_MAX_CONCURRENT);

thread_local! {
    static KNOWN_JWTS: RefCell<JwtCache> = RefCell::new(
        vapid_key::new_jwt_cache(KNOWN_JWT_MAX_RELAYS, &KNOWN_SIGNING_LIMIT),
    );

    static SUBDOMAIN_REACHABILITY: RefCell<ReachabilityCache> = RefCell::new(
        new_reachability_cache(
            SUBDOMAIN_REACHABILITY_STALE_IF_ERROR_FOR_SECS,
            &SUBDOMAIN_PROBE_LIMIT,
        ),
    );
    static SUBDOMAIN_JWTS: RefCell<JwtCache> = RefCell::new(
        vapid_key::new_jwt_cache(SUBDOMAIN_JWT_MAX_RELAYS, &SUBDOMAIN_SIGNING_LIMIT),
    );

    static UNKNOWN_REACHABILITY: RefCell<ReachabilityCache> = RefCell::new(
        new_reachability_cache(
            UNKNOWN_REACHABILITY_STALE_IF_ERROR_FOR_SECS,
            &UNKNOWN_PROBE_LIMIT,
        ),
    );
    static UNKNOWN_JWTS: RefCell<JwtCache> = RefCell::new(
        vapid_key::new_jwt_cache(UNKNOWN_JWT_MAX_RELAYS, &UNKNOWN_SIGNING_LIMIT),
    );
}

fn new_reachability_cache(stale_if_error_for: u64, limiter: Limiter) -> ReachabilityCache {
    SingleFlightCache::new(
        move |relay_origin: String| async move {
            // No slot, no probe: abandoning records nothing, so the origin keeps whatever
            // standing it had and is tried again once the budget frees, rather than
            // taking a backoff for a crowd it was never part of.
            let Some(_permit) = crate::concurrency::acquire(limiter) else {
                return FillOutcome::Abandoned;
            };
            match probe(&relay_origin).await {
                Ok(status) => FillOutcome::Ready(status),
                Err(err) => {
                    ic_cdk::println!("INFO: no relay answered at {relay_origin}: {err}");
                    FillOutcome::Failed(err)
                }
            }
        },
        CacheConfig {
            fresh_for: REACHABILITY_FRESH_FOR_SECS,
            stale_for: REACHABILITY_STALE_FOR_SECS,
            stale_if_error_for,
            max_entries: REACHABILITY_MAX_ORIGINS,
            backoff: RetryBackoff::new(REACHABILITY_RETRY_BASE_SECS, REACHABILITY_RETRY_MULTIPLIER),
            abandon_fill_after: REACHABILITY_ABANDON_FILL_AFTER_SECS,
        },
    )
}

/// The JWT to wake `relay_origin` with, where one is in hand.
///
/// `Pending` covers both halves of the answer: an origin still being probed and one still
/// being signed for say the same thing to a caller, which is to leave the notification in
/// the backlog and ask again on the next pass.
pub(crate) fn signed_jwt(relay_origin: String) -> Cached<SignedJwt> {
    // Off-wasm nothing drives a detached fill, so a test drives it here and reads the
    // answer in the pass that asked. A probe and a signature are two fills in sequence,
    // and neither starts until the lookup before it has landed.
    #[cfg(test)]
    for _ in 0..2 {
        if let answer @ Cached::Ready(_) = look_up(relay_origin.clone()) {
            return answer;
        }
        single_flight_cache::run_detached();
    }
    look_up(relay_origin)
}

fn look_up(relay_origin: String) -> Cached<SignedJwt> {
    match tier_of(&relay_origin) {
        Tier::Known => single_flight_cache::get(&KNOWN_JWTS, relay_origin),
        Tier::KnownSubdomain => {
            signed_once_reachable(&SUBDOMAIN_REACHABILITY, &SUBDOMAIN_JWTS, relay_origin)
        }
        Tier::Unknown => signed_once_reachable(&UNKNOWN_REACHABILITY, &UNKNOWN_JWTS, relay_origin),
    }
}

/// Signs only for an origin that has answered, so a name that resolves to nothing never
/// reaches the signing budget and never occupies a signature slot.
fn signed_once_reachable(
    reachability: &'static LocalKey<RefCell<ReachabilityCache>>,
    jwts: &'static LocalKey<RefCell<JwtCache>>,
    relay_origin: String,
) -> Cached<SignedJwt> {
    match single_flight_cache::get(reachability, relay_origin.clone()) {
        Cached::Ready(_) => single_flight_cache::get(jwts, relay_origin),
        Cached::Pending => Cached::Pending,
    }
}

/// Asks whether anything answers at `relay_origin`, without delivering anything.
///
/// `HEAD` on the origin itself rather than on a subscription's own endpoint: a `POST`
/// there is a wake-up, and a relay that took one would deliver it. What comes back is
/// read for the fact that it came back, so a `404` or a `405` answers the question as
/// well as a `200` does.
#[cfg(not(test))]
async fn probe(relay_origin: &str) -> Result<u16, String> {
    use candid::Principal;
    use ic_cdk::api::management_canister::http_request::{
        HttpHeader, HttpMethod, HttpResponse, TransformContext,
    };

    /// Unused cycles come back, so this is a ceiling rather than a price.
    const PROBE_CYCLES: u128 = 20_000_000_000;
    /// Only the status is read; this is headroom for the headers carrying it.
    const MAX_RESPONSE_BYTES: u64 = 2 * 1024;

    /// This SDK lacks `is_replicated`; set it explicitly to avoid one probe per replica.
    #[derive(candid::CandidType)]
    struct NonReplicatedHttpRequest {
        url: String,
        max_response_bytes: Option<u64>,
        method: HttpMethod,
        headers: Vec<HttpHeader>,
        body: Option<Vec<u8>>,
        transform: Option<TransformContext>,
        is_replicated: Option<bool>,
    }

    let request = NonReplicatedHttpRequest {
        url: format!("{relay_origin}/"),
        max_response_bytes: Some(MAX_RESPONSE_BYTES),
        method: HttpMethod::HEAD,
        headers: Vec::new(),
        body: None,
        transform: None,
        is_replicated: Some(false),
    };

    let result: ic_cdk::api::call::CallResult<(HttpResponse,)> =
        ic_cdk::api::call::call_with_payment128(
            Principal::management_canister(),
            "http_request",
            (request,),
            PROBE_CYCLES,
        )
        .await;

    match result {
        Ok((response,)) => u16::try_from(&response.status.0)
            .map_err(|_| format!("an unreadable status: {}", response.status)),
        Err((code, message)) => Err(format!("{code:?} {message}")),
    }
}

/// Off-wasm there is no management canister to make an outcall with, and what a test
/// asserts is which cache an origin was taken through rather than what the network said.
#[cfg(test)]
async fn probe(_relay_origin: &str) -> Result<u16, String> {
    Ok(200)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn names_the_relays_we_hold_outright() {
        for origin in KNOWN_RELAY_ORIGINS {
            assert_eq!(tier_of(origin), Tier::Known);
        }
    }

    #[test]
    fn takes_a_subdomain_of_a_known_relay_on_its_own_tier() {
        assert_eq!(tier_of("https://web2.push.apple.com"), Tier::KnownSubdomain);
    }

    #[test]
    fn refuses_a_host_that_only_reads_like_a_known_relay() {
        for origin in [
            "https://evilpush.apple.com",
            "https://web.push.apple.com.example",
            "https://push.apple.com.example",
            "https://apple.com",
        ] {
            assert_eq!(tier_of(origin), Tier::Unknown, "{origin}");
        }
    }

    #[test]
    fn a_port_does_not_make_an_origin_one_we_name() {
        assert_eq!(
            tier_of("https://web.push.apple.com:8443"),
            Tier::KnownSubdomain
        );
        assert_eq!(tier_of("https://fcm.googleapis.com:8443"), Tier::Unknown);
    }
}
