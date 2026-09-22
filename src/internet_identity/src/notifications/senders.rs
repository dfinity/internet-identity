//! Who may notify for an origin: the canisters it names in the list it
//! publishes, fetched on demand and cached in memory, never persisted.
//!
//! An origin authorizes its senders by serving
//! `/.well-known/ii-notification-senders`, the same way it authorizes
//! alternative origins by serving `/.well-known/ii-alternative-origins`:
//!
//! ```json
//! { "senders": ["ryjl3-tyaaa-aaaaa-aaaba-cai"] }
//! ```
//!
//! The list is read through a single-flight cache, so the first call for an
//! origin starts the fetch and answers [`Senders::Pending`] — the caller is
//! told to come back rather than told it is unauthorized, because II does not
//! yet know either way. Only an origin this deployment notifies for is ever
//! fetched (`main.rs` validates the origin first), so no caller can aim an
//! outcall at a host of its choosing.

use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};
use candid::Principal;
use internet_identity_interface::internet_identity::types::FrontendHostname;
use std::cell::RefCell;

#[cfg(not(test))]
const WELL_KNOWN_PATH: &str = "/.well-known/ii-notification-senders";

/// Max senders accepted from one origin's list. An over-cap list is rejected
/// rather than truncated: truncating would silently turn a listed sender into
/// an unlisted one. Ten matches the cap on `ii-alternative-origins`, and an
/// app needing more backends than that can front them with one.
#[cfg(not(test))]
const MAX_SENDERS: usize = 10;

/// Response-size cap. Ten textual principals with JSON around them is well
/// under a kilobyte; 4 KiB leaves room for formatting without inviting a large
/// body to be parsed.
#[cfg(not(test))]
const SENDERS_MAX_RESPONSE_BYTES: u64 = 4 * 1024;

#[cfg(not(test))]
const SENDERS_CALL_CYCLES: u128 = 30_000_000_000;

/// An hour, matching SSO discovery. It is also the revocation lag: an origin
/// that drops a sender stays notifiable by it until the entry goes stale.
const FRESH_FOR_SECONDS: u64 = 60 * 60;
/// Serve the last-good list through a transient fetch failure for this long
/// past freshness, so an origin's brief outage doesn't stop its notifications.
const STALE_FOR_SECONDS: u64 = 60 * 60;
/// One entry per notifying origin, and only origins this deployment enables
/// are ever fetched, so the key space is bounded by configuration rather than
/// by callers. Sized well above any plausible list of enabled origins.
const CACHE_MAX_ENTRIES: usize = 1_000;
const RETRY_BASE_SECONDS: u64 = 60;
const RETRY_MULTIPLIER: u64 = 2;
const ABANDON_FILL_AFTER_SECONDS: u64 = 120;

type SendersCache = SingleFlightCache<FrontendHostname, Vec<Principal>, String>;

thread_local! {
    static SENDERS_CACHE: RefCell<SendersCache> = RefCell::new(new_senders_cache());
}

fn new_senders_cache() -> SendersCache {
    SingleFlightCache::new(
        senders_fill,
        CacheConfig {
            fresh_for: FRESH_FOR_SECONDS,
            stale_for: STALE_FOR_SECONDS,
            max_entries: CACHE_MAX_ENTRIES,
            backoff: RetryBackoff::new(RETRY_BASE_SECONDS, RETRY_MULTIPLIER),
            abandon_fill_after: ABANDON_FILL_AFTER_SECONDS,
        },
    )
}

/// What the origin's list says about a caller.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Senders {
    /// The list was read and names the caller.
    Listed,
    /// The list was read and does not name the caller.
    NotListed,
    /// II has no list yet. The fetch is now running; the caller retries.
    Pending,
}

/// Whether `caller` may send for `origin`, starting the fetch if the list is
/// not cached. Only an update may call this: a fill spawns an outcall.
pub fn authorize(origin: &FrontendHostname, caller: Principal) -> Senders {
    match single_flight_cache::get(&SENDERS_CACHE, origin.clone()) {
        Cached::Ready(senders) => {
            if senders.contains(&caller) {
                Senders::Listed
            } else {
                Senders::NotListed
            }
        }
        Cached::Pending => Senders::Pending,
    }
}

/// The list as published by an origin.
#[derive(serde::Deserialize)]
struct SenderList {
    senders: Vec<String>,
}

/// Parse and bound an origin's list. Separate from the fetch so the rules are
/// testable without an outcall.
fn parse_senders(body: &[u8], max_senders: usize) -> Result<Vec<Principal>, String> {
    let list = serde_json::from_slice::<SenderList>(body)
        .map_err(|_| "invalid ii-notification-senders JSON".to_string())?;
    if list.senders.len() > max_senders {
        return Err(format!(
            "ii-notification-senders exceeds the {max_senders}-entry cap ({} entries)",
            list.senders.len()
        ));
    }
    list.senders
        .iter()
        .map(|sender| {
            Principal::from_text(sender).map_err(|_| format!("{sender} is not a principal"))
        })
        .collect()
}

#[cfg(not(test))]
thread_local! {
    /// Caps how many sender-list fetches run at once, so a wave of first-time
    /// sends doesn't fan out all at the same moment. The 90s reclaim age is
    /// kept above the ~60s outcall timeout so a still-live call is never
    /// reclaimed early. See [`crate::concurrency`].
    static SENDERS_OUTCALL_LIMIT: RefCell<crate::concurrency::ConcurrencyLimiter> =
        RefCell::new(crate::concurrency::ConcurrencyLimiter::new(
            crate::concurrency::LimiterConfig {
                max_concurrent: 80,
                max_age_secs: 90,
            },
        ));
}

/// The cache fill: one outcall slot, then fetch and parse. No slot → abandon,
/// which records nothing (no backoff) and retries once capacity frees.
#[cfg(not(test))]
async fn senders_fill(origin: FrontendHostname) -> FillOutcome<Vec<Principal>, String> {
    let Some(_permit) = crate::concurrency::acquire(&SENDERS_OUTCALL_LIMIT) else {
        return FillOutcome::Abandoned;
    };
    match fetch_senders(origin).await {
        Ok(senders) => FillOutcome::Ready(senders),
        Err(err) => FillOutcome::Failed(err),
    }
}

#[cfg(not(test))]
async fn fetch_senders(origin: FrontendHostname) -> Result<Vec<Principal>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
    };

    let request = CanisterHttpRequestArgument {
        url: format!("{origin}{WELL_KNOWN_PATH}"),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(SENDERS_MAX_RESPONSE_BYTES),
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

    let (response,) = http_request_with_closure(request, SENDERS_CALL_CYCLES, transform_senders)
        .await
        .map_err(|(_, err)| err)?;
    parse_senders(&response.body, MAX_SENDERS)
}

/// Re-serializes the parsed JSON so subnet nodes reach consensus on a body
/// that may differ in whitespace or header order between them.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_senders(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use candid::Nat;
    use ic_cdk::api::management_canister::http_request::HttpResponse;

    const HTTP_STATUS_OK: u8 = 200;
    if response.status != HTTP_STATUS_OK {
        return HttpResponse {
            status: response.status,
            headers: vec![],
            body: b"invalid ii-notification-senders response status".to_vec(),
        };
    }
    let Ok(list) = serde_json::from_slice::<serde_json::Value>(response.body.as_slice()) else {
        return HttpResponse {
            status: Nat::from(HTTP_STATUS_OK),
            headers: vec![],
            body: b"invalid ii-notification-senders JSON".to_vec(),
        };
    };
    let Ok(body) = serde_json::to_vec(&list) else {
        return HttpResponse {
            status: Nat::from(HTTP_STATUS_OK),
            headers: vec![],
            body: b"invalid ii-notification-senders JSON".to_vec(),
        };
    };
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

// In test builds the fill reads from injected state instead of doing outcalls.
#[cfg(test)]
async fn senders_fill(origin: FrontendHostname) -> FillOutcome<Vec<Principal>, String> {
    match tests::TEST_SENDERS.with_borrow(|m| m.get(&origin).cloned()) {
        Some(senders) => FillOutcome::Ready(senders),
        None => FillOutcome::Failed(format!("no test sender list for {origin}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    thread_local! {
        pub(super) static TEST_SENDERS: RefCell<HashMap<FrontendHostname, Vec<Principal>>> =
            RefCell::new(HashMap::new());
    }

    const APP: &str = "https://app.example";

    fn principal(text: &str) -> Principal {
        Principal::from_text(text).expect("a principal")
    }

    fn publish(origin: &str, senders: &[Principal]) {
        TEST_SENDERS.with_borrow_mut(|m| m.insert(origin.to_string(), senders.to_vec()));
    }

    fn reset() {
        TEST_SENDERS.with_borrow_mut(HashMap::clear);
        SENDERS_CACHE.with_borrow_mut(|cache| *cache = new_senders_cache());
    }

    /// The first call has no list yet, so it starts the fetch and answers
    /// Pending rather than claiming the caller is unauthorized.
    #[test]
    fn first_call_is_pending_then_resolves() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        publish(APP, &[sender]);

        // The first call spawns the fill and has nothing to judge with yet.
        assert_eq!(authorize(&APP.to_string(), sender), Senders::Pending);
        single_flight_cache::run_detached();
        assert_eq!(authorize(&APP.to_string(), sender), Senders::Listed);
    }

    #[test]
    fn a_caller_the_list_omits_is_not_listed() {
        reset();
        let listed = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        let other = principal("rrkah-fqaaa-aaaaa-aaaaq-cai");
        publish(APP, &[listed]);

        assert_eq!(authorize(&APP.to_string(), listed), Senders::Pending);
        single_flight_cache::run_detached();
        assert_eq!(authorize(&APP.to_string(), other), Senders::NotListed);
    }

    /// An origin serving nothing usable never resolves to Listed, and the
    /// failed fill backs off rather than answering NotListed on the spot.
    #[test]
    fn an_origin_with_no_list_stays_pending() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");

        assert_eq!(authorize(&APP.to_string(), sender), Senders::Pending);
        single_flight_cache::run_detached();
        assert_eq!(authorize(&APP.to_string(), sender), Senders::Pending);
    }

    /// Every canister of one origin is authorized by the same list.
    #[test]
    fn several_canisters_may_send_for_one_origin() {
        reset();
        let first = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        let second = principal("rrkah-fqaaa-aaaaa-aaaaq-cai");
        publish(APP, &[first, second]);

        assert_eq!(authorize(&APP.to_string(), first), Senders::Pending);
        single_flight_cache::run_detached();
        assert_eq!(authorize(&APP.to_string(), first), Senders::Listed);
        assert_eq!(authorize(&APP.to_string(), second), Senders::Listed);
    }

    #[test]
    fn parses_a_published_list() {
        let body = br#"{"senders":["ryjl3-tyaaa-aaaaa-aaaba-cai","rrkah-fqaaa-aaaaa-aaaaq-cai"]}"#;
        assert_eq!(
            parse_senders(body, 10).unwrap(),
            vec![
                principal("ryjl3-tyaaa-aaaaa-aaaba-cai"),
                principal("rrkah-fqaaa-aaaaa-aaaaq-cai"),
            ]
        );
    }

    #[test]
    fn accepts_an_empty_list() {
        assert_eq!(parse_senders(br#"{"senders":[]}"#, 10).unwrap(), vec![]);
    }

    /// Rejected, not truncated: a truncated list silently unlists a sender.
    #[test]
    fn rejects_a_list_over_the_cap() {
        let body = br#"{"senders":["ryjl3-tyaaa-aaaaa-aaaba-cai","rrkah-fqaaa-aaaaa-aaaaq-cai"]}"#;
        assert!(parse_senders(body, 1).is_err());
    }

    #[test]
    fn rejects_an_entry_that_is_not_a_principal() {
        assert!(parse_senders(br#"{"senders":["not-a-principal"]}"#, 10).is_err());
    }

    #[test]
    fn rejects_a_body_that_is_not_the_expected_shape() {
        for body in [
            &br#"["ryjl3-tyaaa-aaaaa-aaaba-cai"]"#[..],
            &br#"{"senders":"ryjl3-tyaaa-aaaaa-aaaba-cai"}"#[..],
            &br#"{}"#[..],
            &br#"not json"#[..],
        ] {
            assert!(parse_senders(body, 10).is_err(), "must not be accepted");
        }
    }
}
