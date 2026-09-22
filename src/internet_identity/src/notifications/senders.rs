//! Who may notify for an origin: the canisters it names at
//! `/.well-known/ii-notification-senders`, fetched on demand and cached in
//! memory.
//!
//! ```json
//! { "senders": ["ryjl3-tyaaa-aaaaa-aaaba-cai"] }
//! ```
//!
//! `notifications_enabled_origins` gates which origins are fetchable today but
//! is a rollout gate, not the bound on outcalls: once it goes, any origin a
//! caller names is fetchable, as with SSO discovery. What bounds them is the
//! cache, [`SENDERS_OUTCALL_LIMIT`] and the failure backoff.

use crate::notifications::ValidatedSendNotificationArg;
use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use std::cell::RefCell;

#[cfg(not(test))]
const WELL_KNOWN_PATH: &str = "/.well-known/ii-notification-senders";

/// Rejected over this, never truncated: a truncated list unlists a sender.
#[cfg(not(test))]
const MAX_SENDERS: usize = 10;

#[cfg(not(test))]
const SENDERS_MAX_RESPONSE_BYTES: u64 = 4 * 1024;

#[cfg(not(test))]
const SENDERS_CALL_CYCLES: u128 = 30_000_000_000;

// A dropped sender stays authorized for FRESH_FOR + STALE_FOR, not FRESH_FOR:
// the refresh is served the stale list (stale-while-revalidate) and a failed
// refresh keeps serving it (stale-if-error).
const FRESH_FOR_SECONDS: u64 = 60 * 60;
const STALE_FOR_SECONDS: u64 = 60 * 60;

/// Eviction headroom rather than capacity: junk origins are free to generate,
/// and at 50k an attacker must sustain ~167 calls/s to evict an app that sends
/// every five minutes. ~50 MB at under a kilobyte an entry.
const CACHE_MAX_ENTRIES: usize = 50_000;
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

/// A little over one outcall round trip.
const PENDING_RETRY_AFTER_NS: u64 = 3 * 1_000_000_000;

/// What the origin's list says about a caller.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Senders {
    /// The list was read and names the caller.
    Listed,
    /// The list was read and does not name the caller.
    NotListed,
    /// No list yet; come back at `retry_after`.
    Pending { retry_after: Timestamp },
}

/// Whether `caller` may send for the request's origin, starting the fetch if
/// the list is not cached. Only an update may call this: a fill spawns an
/// outcall.
pub fn authorize(
    ValidatedSendNotificationArg { origin, .. }: &ValidatedSendNotificationArg,
    caller: Principal,
    now_ns: Timestamp,
) -> Senders {
    match single_flight_cache::get(&SENDERS_CACHE, origin.clone()) {
        Cached::Ready(senders) => {
            if senders.contains(&caller) {
                Senders::Listed
            } else {
                Senders::NotListed
            }
        }
        Cached::Pending => Senders::Pending {
            retry_after: retry_after(origin, now_ns),
        },
    }
}

/// A parked origin is not retried before the cache's own deadline, so sending
/// the caller back sooner only buys deferrals.
fn retry_after(origin: &FrontendHostname, now_ns: Timestamp) -> Timestamp {
    single_flight_cache::retry_at(&SENDERS_CACHE, origin)
        .map_or(now_ns + PENDING_RETRY_AFTER_NS, |secs| secs * 1_000_000_000)
}

/// The list as published by an origin, and what the transform re-emits.
#[derive(serde::Deserialize, serde::Serialize)]
struct SenderList {
    senders: Vec<String>,
}

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
    /// The 90s reclaim age is above the ~60s outcall timeout, so a live call
    /// is never reclaimed early.
    static SENDERS_OUTCALL_LIMIT: RefCell<crate::concurrency::ConcurrencyLimiter> =
        RefCell::new(crate::concurrency::ConcurrencyLimiter::new(
            crate::concurrency::LimiterConfig {
                max_concurrent: 80,
                max_age_secs: 90,
            },
        ));
}

/// No outcall slot → abandon, which records nothing and retries once capacity
/// frees.
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

/// Narrows a response to the senders and nothing else, so what crosses
/// consensus is what the authorization reads.
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
    let invalid = || HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body: b"invalid ii-notification-senders JSON".to_vec(),
    };
    let Ok(list) = serde_json::from_slice::<SenderList>(response.body.as_slice()) else {
        return invalid();
    };
    let Ok(body) = serde_json::to_vec(&list) else {
        return invalid();
    };
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

// In test builds the fill reads injected state instead of doing outcalls.
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
    const NOW_NS: Timestamp = 1_000_000_000_000;

    fn pending(senders: Senders) -> bool {
        matches!(senders, Senders::Pending { .. })
    }

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

    fn request(origin: &str) -> ValidatedSendNotificationArg {
        crate::state::persistent_state_mut(|s| {
            s.notifications_enabled_origins = Some(vec![origin.to_string()]);
        });
        internet_identity_interface::internet_identity::types::SendNotificationArg {
            origin: origin.to_string(),
            notifications: vec![],
        }
        .try_into()
        .expect("a notifiable origin")
    }

    #[test]
    fn first_call_is_pending_then_resolves() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        publish(APP, &[sender]);

        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
        single_flight_cache::run_detached();
        assert_eq!(authorize(&request(APP), sender, NOW_NS), Senders::Listed);
    }

    #[test]
    fn a_caller_the_list_omits_is_not_listed() {
        reset();
        let listed = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        let other = principal("rrkah-fqaaa-aaaaa-aaaaq-cai");
        publish(APP, &[listed]);

        assert!(pending(authorize(&request(APP), listed, NOW_NS)));
        single_flight_cache::run_detached();
        assert_eq!(authorize(&request(APP), other, NOW_NS), Senders::NotListed);
    }

    #[test]
    fn an_origin_with_no_list_stays_pending() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");

        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
        single_flight_cache::run_detached();
        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
    }

    /// The reported time is the cache's backoff deadline, not the estimate.
    #[test]
    fn a_failed_fetch_defers_until_the_cache_tries_again() {
        reset();
        single_flight_cache::set_test_now(1_000);
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");

        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
        single_flight_cache::run_detached();

        let Senders::Pending { retry_after } = authorize(&request(APP), sender, NOW_NS) else {
            panic!("a parked origin is still pending");
        };
        assert_eq!(retry_after, (1_000 + RETRY_BASE_SECONDS) * 1_000_000_000);
        assert!(
            retry_after > NOW_NS + PENDING_RETRY_AFTER_NS,
            "the parked deadline is what is reported, not the fill estimate"
        );
    }

    #[test]
    fn an_unfinished_fetch_defers_by_one_fill() {
        reset();
        single_flight_cache::set_test_now(1_000);
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        publish(APP, &[sender]);

        assert_eq!(
            authorize(&request(APP), sender, NOW_NS),
            Senders::Pending {
                retry_after: NOW_NS + PENDING_RETRY_AFTER_NS
            }
        );
    }

    #[test]
    fn several_canisters_may_send_for_one_origin() {
        reset();
        let first = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        let second = principal("rrkah-fqaaa-aaaaa-aaaaq-cai");
        publish(APP, &[first, second]);

        assert!(pending(authorize(&request(APP), first, NOW_NS)));
        single_flight_cache::run_detached();
        assert_eq!(authorize(&request(APP), first, NOW_NS), Senders::Listed);
        assert_eq!(authorize(&request(APP), second, NOW_NS), Senders::Listed);
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

    #[test]
    fn rejects_a_list_over_the_cap() {
        let body = br#"{"senders":["ryjl3-tyaaa-aaaaa-aaaba-cai","rrkah-fqaaa-aaaaa-aaaaq-cai"]}"#;
        assert!(parse_senders(body, 1).is_err());
    }

    #[test]
    fn a_padded_body_narrows_to_the_senders_it_carries() {
        let padded = br#"{"note":"ignored","senders":["ryjl3-tyaaa-aaaaa-aaaba-cai"]}"#;
        let list = serde_json::from_slice::<SenderList>(padded).expect("the accepted shape");
        let narrowed = serde_json::to_vec(&list).expect("re-serializing");

        assert_eq!(narrowed, br#"{"senders":["ryjl3-tyaaa-aaaaa-aaaba-cai"]}"#);
        assert_eq!(
            parse_senders(&narrowed, 10).unwrap(),
            vec![principal("ryjl3-tyaaa-aaaaa-aaaba-cai")]
        );
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
