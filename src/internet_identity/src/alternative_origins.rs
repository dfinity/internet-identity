//! Which frontends an application accepts as its own: the origins it lists at
//! `/.well-known/ii-alternative-origins`, fetched on demand and cached in memory.
//!
//! ```json
//! { "alternativeOrigins": ["https://app.example", "https://other.example"] }
//! ```
//!
//! The browser reads the same file to let a frontend sign in with the principals of the
//! application at its derivation origin. The canister reads it where a call names a
//! frontend on behalf of such an application. What bounds the outcalls is the cache,
//! [`ALTERNATIVE_ORIGINS_OUTCALL_LIMIT`] and the failure backoff.

use crate::concurrency::{self, ConcurrencyLimiter, LimiterConfig};
use crate::origin::{self, ApplicationOrigin};
use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};
use ic_cdk::api::management_canister::http_request::{
    http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod, HttpResponse,
};
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::future::Future;

const WELL_KNOWN_PATH: &str = "/.well-known/ii-alternative-origins";

/// The most entries a list may hold, as the browser accepts
/// (`MAX_ALTERNATIVE_ORIGINS` in `src/frontend/src/lib/utils/urlUtils.ts`). A longer list
/// is unusable as a whole, never truncated: a truncated list would unlist a frontend.
const MAX_ALTERNATIVE_ORIGINS: usize = 100;

/// [`MAX_ALTERNATIVE_ORIGINS`] entries at the 255-byte origin limit are about 26 KiB of
/// JSON, and the response certificate a gateway adds to a certified asset takes a few
/// kilobytes of headers, which count against this too.
const ALTERNATIVE_ORIGINS_MAX_RESPONSE_BYTES: u64 = 40 * 1024;

/// Covers the outcall at [`ALTERNATIVE_ORIGINS_MAX_RESPONSE_BYTES`] on the largest
/// subnet, with room to spare; what is not used is refunded.
const ALTERNATIVE_ORIGINS_CALL_CYCLES: u128 = 30_000_000_000;

/// A frontend dropped from the list keeps passing for `FRESH_FOR_SECONDS` plus
/// `STALE_FOR_SECONDS`: the refresh is served the stale list, and a failed refresh keeps
/// serving it.
const FRESH_FOR_SECONDS: u64 = 60 * 60;
const STALE_FOR_SECONDS: u64 = 60 * 60;

/// Eviction headroom: a list holds up to about 26 KiB, so at the cap the cache holds at
/// most about 52 MB. An origin evicted by junk is fetched again on its next call.
const CACHE_MAX_ENTRIES: usize = 2_000;
const RETRY_BASE_SECONDS: u64 = 60;
const RETRY_MULTIPLIER: u64 = 2;
/// Above the outcall timeout of about a minute, so a live fill is never taken over.
const ABANDON_FILL_AFTER_SECONDS: u64 = 120;

/// Concurrent fetches. Lists change rarely and stay cached for an hour, so a small
/// budget serves every application while leaving the subnet's outcall capacity to the
/// paths that need it more.
const MAX_CONCURRENT_OUTCALLS: usize = 20;
/// Above the outcall timeout of about a minute, so a live call is never reclaimed early.
const OUTCALL_PERMIT_MAX_AGE_SECONDS: u64 = 90;

/// A little over one outcall round trip.
const PENDING_RETRY_AFTER_NS: u64 = 3 * 1_000_000_000;

const HTTP_STATUS_OK: u16 = 200;
const HTTP_STATUS_NOT_FOUND: u16 = 404;

/// The folded origins an application lists, keyed by the application's folded origin.
type AlternativeOriginsCache =
    SingleFlightCache<FrontendHostname, BTreeSet<FrontendHostname>, String>;

thread_local! {
    static ALTERNATIVE_ORIGINS_CACHE: RefCell<AlternativeOriginsCache> =
        RefCell::new(new_alternative_origins_cache(fill_alternative_origins));

    static ALTERNATIVE_ORIGINS_OUTCALL_LIMIT: RefCell<ConcurrencyLimiter> =
        RefCell::new(ConcurrencyLimiter::new(LimiterConfig {
            max_concurrent: MAX_CONCURRENT_OUTCALLS,
            max_age_secs: OUTCALL_PERMIT_MAX_AGE_SECONDS,
        }));
}

/// A cache whose misses are filled by `fill`, which reads the list of the application at
/// the folded origin it is given.
fn new_alternative_origins_cache<Fut>(
    fill: impl Fn(FrontendHostname) -> Fut + 'static,
) -> AlternativeOriginsCache
where
    Fut: Future<Output = FillOutcome<BTreeSet<FrontendHostname>, String>> + 'static,
{
    SingleFlightCache::new(
        fill,
        CacheConfig {
            fresh_for: FRESH_FOR_SECONDS,
            stale_for: STALE_FOR_SECONDS,
            stale_if_error_for: STALE_FOR_SECONDS,
            max_entries: CACHE_MAX_ENTRIES,
            backoff: RetryBackoff::new(RETRY_BASE_SECONDS, RETRY_MULTIPLIER),
            abandon_fill_after: ABANDON_FILL_AFTER_SECONDS,
        },
    )
}

/// Whether an application accepts a frontend as its own.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AlternativeOrigin {
    /// The frontend is the application's own origin, or its list names it.
    Listed,
    /// The application lists no such frontend: no file, an unusable one, or one that does
    /// not name it.
    NotListed,
    /// No list yet; come back at `retry_after`.
    Pending { retry_after: Timestamp },
}

/// Whether the application at `derivation_origin` accepts `frontend_origin` as its own
/// frontend, starting the fetch of its list where the list is not cached. Only an update
/// may call this: a fill spawns an outcall. `now_ns` is the call's time.
pub fn authorize_alternative_origin(
    derivation_origin: &ApplicationOrigin,
    frontend_origin: &ApplicationOrigin,
    now_ns: Timestamp,
) -> AlternativeOrigin {
    if derivation_origin == frontend_origin {
        return AlternativeOrigin::Listed;
    }
    let derivation_origin = derivation_origin.as_str().to_string();
    match single_flight_cache::get(&ALTERNATIVE_ORIGINS_CACHE, derivation_origin.clone()) {
        Cached::Ready(alternative_origins) => {
            if alternative_origins.contains(frontend_origin.as_str()) {
                AlternativeOrigin::Listed
            } else {
                AlternativeOrigin::NotListed
            }
        }
        Cached::Pending => AlternativeOrigin::Pending {
            retry_after: retry_after(&derivation_origin, now_ns),
        },
    }
}

/// A parked origin is not retried before the cache's own deadline, so sending the caller
/// back sooner only buys deferrals.
fn retry_after(derivation_origin: &FrontendHostname, now_ns: Timestamp) -> Timestamp {
    single_flight_cache::retry_at(&ALTERNATIVE_ORIGINS_CACHE, derivation_origin).map_or(
        now_ns.saturating_add(PENDING_RETRY_AFTER_NS),
        |retry_at_seconds| retry_at_seconds.saturating_mul(1_000_000_000),
    )
}

/// The list as an application publishes it, and what the transform re-emits.
#[derive(serde::Deserialize, serde::Serialize)]
struct AlternativeOriginList {
    #[serde(rename = "alternativeOrigins")]
    alternative_origins: Vec<String>,
}

/// What a response means for an application's frontends. An application that serves no
/// list, or one II cannot use, lists none: that is its own configuration, so it fills
/// successfully with an empty set. Only II's own trouble, a status it did not ask for, is
/// a failed fill, which the cache retries with backoff.
fn alternative_origins_from_response(
    status: u16,
    body: &[u8],
) -> Result<BTreeSet<FrontendHostname>, String> {
    if status == HTTP_STATUS_NOT_FOUND {
        return Ok(BTreeSet::new());
    }
    if status != HTTP_STATUS_OK {
        return Err(format!("ii-alternative-origins answered {status}"));
    }
    Ok(parse_alternative_origins(body).unwrap_or_default())
}

/// The folded origins a list names, or `None` where the body is not a list II can use:
/// unparsable, or over [`MAX_ALTERNATIVE_ORIGINS`]. An entry that is not a bare origin
/// is skipped, as it can never match a frontend.
fn parse_alternative_origins(body: &[u8]) -> Option<BTreeSet<FrontendHostname>> {
    let list = serde_json::from_slice::<AlternativeOriginList>(body).ok()?;
    if list.alternative_origins.len() > MAX_ALTERNATIVE_ORIGINS {
        return None;
    }
    let mut alternative_origins = BTreeSet::new();
    for entry in &list.alternative_origins {
        if let Ok(alternative_origin) = ApplicationOrigin::try_from(entry.as_str()) {
            alternative_origins.insert(alternative_origin.as_str().to_string());
        }
    }
    Some(alternative_origins)
}

/// Reads the list of the application at `derivation_origin`. No outcall slot means
/// abandon, which records nothing and retries once capacity frees.
async fn fill_alternative_origins(
    derivation_origin: FrontendHostname,
) -> FillOutcome<BTreeSet<FrontendHostname>, String> {
    let Some(_permit) = concurrency::acquire(&ALTERNATIVE_ORIGINS_OUTCALL_LIMIT) else {
        return FillOutcome::Abandoned;
    };
    match fetch_alternative_origins(&derivation_origin).await {
        Ok(alternative_origins) => FillOutcome::Ready(alternative_origins),
        Err(err) => FillOutcome::Failed(err),
    }
}

async fn fetch_alternative_origins(
    derivation_origin: &str,
) -> Result<BTreeSet<FrontendHostname>, String> {
    let request = CanisterHttpRequestArgument {
        url: format!(
            "{}{WELL_KNOWN_PATH}",
            origin::fetch_origin(derivation_origin)
        ),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(ALTERNATIVE_ORIGINS_MAX_RESPONSE_BYTES),
        transform: None,
        headers: vec![
            HttpHeader {
                name: "Accept".into(),
                value: "application/json".into(),
            },
            HttpHeader {
                name: "User-Agent".into(),
                value: "internet_identity_canister".into(),
            },
        ],
    };
    let (response,) = http_request_with_closure(
        request,
        ALTERNATIVE_ORIGINS_CALL_CYCLES,
        transform_alternative_origins,
    )
    .await
    .map_err(|(_, err)| err)?;
    let status = u16::try_from(response.status.0.clone()).unwrap_or_default();
    alternative_origins_from_response(status, &response.body)
}

/// Narrows a response to the list and nothing else, so what crosses consensus is what
/// the check reads.
fn transform_alternative_origins(response: HttpResponse) -> HttpResponse {
    let HttpResponse { status, body, .. } = response;
    if status != HTTP_STATUS_OK {
        return HttpResponse {
            status,
            headers: vec![],
            body: b"invalid ii-alternative-origins response status".to_vec(),
        };
    }
    let invalid_response = HttpResponse {
        status: status.clone(),
        headers: vec![],
        body: b"invalid ii-alternative-origins JSON".to_vec(),
    };
    let Ok(list) = serde_json::from_slice::<AlternativeOriginList>(&body) else {
        return invalid_response;
    };
    let Ok(body) = serde_json::to_vec(&list) else {
        return invalid_response;
    };
    HttpResponse {
        status,
        headers: vec![],
        body,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    thread_local! {
        /// What each application publishes, by folded origin; an absent one fails to fetch.
        static PUBLISHED_LISTS: RefCell<BTreeMap<FrontendHostname, BTreeSet<FrontendHostname>>> =
            const { RefCell::new(BTreeMap::new()) };
    }

    const SUITE: &str = "https://suite.example";
    const FRONTEND: &str = "https://mail.suite.example";
    const NOW_NS: Timestamp = 1_000_000_000_000;

    async fn fill_from_published_lists(
        derivation_origin: FrontendHostname,
    ) -> FillOutcome<BTreeSet<FrontendHostname>, String> {
        match PUBLISHED_LISTS.with_borrow(|lists| lists.get(&derivation_origin).cloned()) {
            Some(alternative_origins) => FillOutcome::Ready(alternative_origins),
            None => FillOutcome::Failed(format!("no test list for {derivation_origin}")),
        }
    }

    fn reset() {
        PUBLISHED_LISTS.with_borrow_mut(BTreeMap::clear);
        ALTERNATIVE_ORIGINS_CACHE.with_borrow_mut(|cache| {
            *cache = new_alternative_origins_cache(fill_from_published_lists)
        });
    }

    fn publish(derivation_origin: &str, alternative_origins: &[&str]) {
        PUBLISHED_LISTS.with_borrow_mut(|lists| {
            lists.insert(
                derivation_origin.to_string(),
                alternative_origins
                    .iter()
                    .map(|origin| origin.to_string())
                    .collect(),
            )
        });
    }

    fn origin(origin: &str) -> ApplicationOrigin {
        ApplicationOrigin::try_from(origin).expect("a bare origin")
    }

    fn is_pending(answer: AlternativeOrigin) -> bool {
        matches!(answer, AlternativeOrigin::Pending { .. })
    }

    #[test]
    fn an_application_accepts_its_own_origin_without_a_fetch() {
        reset();
        assert_eq!(
            authorize_alternative_origin(&origin(SUITE), &origin(SUITE), NOW_NS),
            AlternativeOrigin::Listed
        );
        assert_eq!(
            single_flight_cache::stats(&ALTERNATIVE_ORIGINS_CACHE).entries,
            0
        );
    }

    #[test]
    fn a_listed_frontend_is_pending_then_listed() {
        reset();
        publish(SUITE, &[FRONTEND]);

        assert!(is_pending(authorize_alternative_origin(
            &origin(SUITE),
            &origin(FRONTEND),
            NOW_NS
        )));
        single_flight_cache::run_detached();
        assert_eq!(
            authorize_alternative_origin(&origin(SUITE), &origin(FRONTEND), NOW_NS),
            AlternativeOrigin::Listed
        );
    }

    #[test]
    fn a_frontend_the_list_omits_is_not_listed() {
        reset();
        publish(SUITE, &[FRONTEND]);

        assert!(is_pending(authorize_alternative_origin(
            &origin(SUITE),
            &origin("https://evil-suite.example"),
            NOW_NS
        )));
        single_flight_cache::run_detached();
        assert_eq!(
            authorize_alternative_origin(
                &origin(SUITE),
                &origin("https://evil-suite.example"),
                NOW_NS
            ),
            AlternativeOrigin::NotListed
        );
    }

    #[test]
    fn a_failed_fetch_defers_until_the_cache_tries_again() {
        reset();
        single_flight_cache::set_test_now(1_000);

        assert!(is_pending(authorize_alternative_origin(
            &origin(SUITE),
            &origin(FRONTEND),
            NOW_NS
        )));
        single_flight_cache::run_detached();
        assert_eq!(
            authorize_alternative_origin(&origin(SUITE), &origin(FRONTEND), NOW_NS),
            AlternativeOrigin::Pending {
                retry_after: 1_060 * 1_000_000_000
            }
        );
    }

    #[test]
    fn parses_a_published_list_folding_every_gateway_spelling() {
        let body = br#"{"alternativeOrigins":["https://app.example","https://aaaaa-aa.icp0.io"]}"#;
        assert_eq!(
            alternative_origins_from_response(200, body),
            Ok(BTreeSet::from([
                "https://aaaaa-aa.ic0.app".to_string(),
                "https://app.example".to_string(),
            ]))
        );
    }

    #[test]
    fn skips_an_entry_that_is_not_a_bare_origin() {
        let body = br#"{"alternativeOrigins":["https://app.example/path","https://app.example"]}"#;
        assert_eq!(
            alternative_origins_from_response(200, body),
            Ok(BTreeSet::from(["https://app.example".to_string()]))
        );
    }

    #[test]
    fn a_missing_list_names_no_frontends() {
        assert_eq!(
            alternative_origins_from_response(404, b""),
            Ok(BTreeSet::new())
        );
    }

    #[test]
    fn a_list_of_one_hundred_entries_is_read_and_one_more_names_none() {
        let entries = |count: usize| -> Vec<String> {
            (0..count)
                .map(|index| format!("https://frontend-{index}.example"))
                .collect()
        };
        let body = |count: usize| {
            serde_json::to_vec(&AlternativeOriginList {
                alternative_origins: entries(count),
            })
            .expect("a list")
        };
        assert_eq!(
            alternative_origins_from_response(200, &body(100)).map(|origins| origins.len()),
            Ok(100)
        );
        assert_eq!(
            alternative_origins_from_response(200, &body(101)),
            Ok(BTreeSet::new())
        );
    }

    #[test]
    fn a_body_of_the_wrong_shape_names_no_frontends() {
        for body in [
            &br#"["https://app.example"]"#[..],
            &br#"{"alternativeOrigins":"https://app.example"}"#[..],
            &br#"{"alternative_origins":["https://app.example"]}"#[..],
            &br#"not json"#[..],
        ] {
            assert_eq!(
                alternative_origins_from_response(200, body),
                Ok(BTreeSet::new())
            );
        }
    }

    #[test]
    fn a_status_ii_did_not_ask_for_is_a_failure() {
        for status in [500, 503, 429, 301] {
            assert!(
                alternative_origins_from_response(status, b"").is_err(),
                "{status} must not be an answer about the application's frontends"
            );
        }
    }
}
