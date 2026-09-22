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
//! yet know either way.
//!
//! What keeps a caller from pointing II's outcalls wherever it likes is the
//! fetch path itself, not the origin allowlist: `notifications_enabled_origins`
//! is a rollout gate and goes away once the feature is general, after which any
//! origin a caller names is fetchable — the position SSO discovery is already
//! in, with no domain allowlist at all. The durable bounds are the ones it
//! relies on: the single-flight cache fetches an origin at most once per fresh
//! window, [`SENDERS_OUTCALL_LIMIT`] caps how many fetches are in flight, a
//! failing origin is parked by exponential backoff, and outcalls cost no cycles
//! on a system subnet. Keep those in place when the gate is removed.

use crate::notifications::ValidatedSendNotificationArg;
use crate::single_flight_cache::{
    self, CacheConfig, Cached, FillOutcome, RetryBackoff, SingleFlightCache,
};
use candid::Principal;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
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

/// An hour, matching SSO discovery: how long a fetched list is authoritative.
const FRESH_FOR_SECONDS: u64 = 60 * 60;
/// Serve the last-good list through a transient fetch failure for this long
/// past freshness, so an origin's brief outage doesn't stop its notifications.
const STALE_FOR_SECONDS: u64 = 60 * 60;

// Revocation lag is `FRESH_FOR_SECONDS + STALE_FOR_SECONDS`, not the freshness
// window alone. Past `fresh_for` the cache serves the stale list to the call
// that triggers the refresh (stale-while-revalidate), so the new list is only
// seen by a later call; and if that refresh fails, the stale list keeps being
// served through `stale_for` (stale-if-error). An origin that drops a sender
// therefore stays notifiable by it for up to two hours from the last
// successful fetch. That is the accepted trade: cutting `stale_for` to zero
// would let a brief outage at the origin stop its notifications outright, and
// a sender listed by mistake is the origin's own error to make.

/// One entry per origin fetched. This is not a capacity figure — a few hundred
/// apps notifying within a fresh window would be a lot — it is eviction
/// headroom, and the cap is what an attacker has to out-run. Origins are
/// strings and a failed fill parks an entry too, so junk origins are free to
/// generate; an app sending every five minutes is only evicted if this many
/// distinct origins arrive between two of its sends, which at 50k means
/// sustaining ~167 calls a second rather than the ~3 a 1k cap would ask for.
/// An evicted app is not broken — its next send is deferred once and refetches
/// — so this buys degradation resistance, not denial resistance. At under a
/// kilobyte an entry (an origin up to [`crate::delegation::FRONTEND_HOSTNAME_LIMIT`] and at most
/// [`MAX_SENDERS`] principals) the ceiling is ~50 MB against a ~3 GB heap. Once the origin
/// allowlist goes, this is the only bound on a caller-driven key space, so it
/// is sized for that now.
const CACHE_MAX_ENTRIES: usize = 50_000;
const RETRY_BASE_SECONDS: u64 = 60;
const RETRY_MULTIPLIER: u64 = 2;
const ABANDON_FILL_AFTER_SECONDS: u64 = 120;

/// Keyed by the origin itself rather than a hash of it, which would bound an
/// entry's key at 32 bytes instead of [`crate::delegation::FRONTEND_HOSTNAME_LIMIT`]: a fill is
/// handed only its key (`SingleFlightCache::new` takes `Fn(K) -> Fut`) and has
/// to rebuild the URL from it, so a hashed key cannot fetch. Keeping the
/// string is why a caller picks its own entries' key cost by how long an
/// origin it names, which the sizing below accounts for at the worst case.
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

/// How long a caller waits when the list is still being fetched. One outcall
/// round trip takes a couple of seconds, so this is a little longer than that:
/// coming back early is one more cheap call that defers again, coming back
/// late is a notification sitting undelivered.
const PENDING_RETRY_AFTER_NS: u64 = 3 * 1_000_000_000;

/// What the origin's list says about a caller.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Senders {
    /// The list was read and names the caller.
    Listed,
    /// The list was read and does not name the caller.
    NotListed,
    /// II has no list yet, so the caller comes back at `retry_after`: the
    /// cache's own next attempt when a failed fetch has parked this origin,
    /// otherwise one fill's worth of time from now.
    Pending { retry_after: Timestamp },
}

/// Whether `caller` may send for the request's origin, starting the fetch if
/// the list is not cached. Only an update may call this: a fill spawns an
/// outcall.
///
/// Takes the validated request rather than a bare origin, so an origin that
/// has not been through `TryFrom` — one this deployment does not notify for,
/// or one whose list would not be fetched over https — cannot reach an
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

/// When a caller whose list is not ready should come back. A fetch that failed
/// parks the origin in backoff, and the cache will not try again before its
/// own deadline, so sending them back sooner only buys deferred calls.
fn retry_after(origin: &FrontendHostname, now_ns: Timestamp) -> Timestamp {
    single_flight_cache::retry_at(&SENDERS_CACHE, origin)
        .map_or(now_ns + PENDING_RETRY_AFTER_NS, |secs| secs * 1_000_000_000)
}

/// The list as published by an origin. Serialized as well as deserialized: the
/// transform re-emits this shape, so what crosses consensus is only the field
/// the canister reads.
#[derive(serde::Deserialize, serde::Serialize)]
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

/// Narrows a response to the shape the canister accepts: the senders and
/// nothing else, re-serialized without the origin's whitespace or any field
/// [`parse_senders`] would ignore. What crosses consensus is then exactly what
/// the authorization reads, and an origin cannot pad the body with data the
/// canister carries around for it.
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
    /// Any fixed wall clock; the cache keeps its own, set through
    /// `single_flight_cache::set_test_now`.
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

    /// Built through `TryFrom`, the only way to make one, so these go through
    /// the same validation a caller does.
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

    /// The first call has no list yet, so it starts the fetch and answers
    /// Pending rather than claiming the caller is unauthorized.
    #[test]
    fn first_call_is_pending_then_resolves() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");
        publish(APP, &[sender]);

        // The first call spawns the fill and has nothing to judge with yet.
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

    /// An origin serving nothing usable never resolves to Listed, and the
    /// failed fill backs off rather than answering NotListed on the spot.
    #[test]
    fn an_origin_with_no_list_stays_pending() {
        reset();
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");

        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
        single_flight_cache::run_detached();
        assert!(pending(authorize(&request(APP), sender, NOW_NS)));
    }

    /// A fetch that failed parks the origin in backoff, and the cache will not
    /// try again before its own deadline — so the caller is sent back then,
    /// not a fill's worth of time from now, which would only buy deferrals.
    #[test]
    fn a_failed_fetch_defers_until_the_cache_tries_again() {
        reset();
        single_flight_cache::set_test_now(1_000);
        let sender = principal("ryjl3-tyaaa-aaaaa-aaaba-cai");

        // Nothing published for APP, so the fill fails and parks the origin.
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

    /// While the first fetch is in flight there is nothing parked, so the
    /// caller gets the short estimate instead.
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

    /// Every canister of one origin is authorized by the same list.
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

    /// Rejected, not truncated: a truncated list silently unlists a sender.
    #[test]
    fn rejects_a_list_over_the_cap() {
        let body = br#"{"senders":["ryjl3-tyaaa-aaaaa-aaaba-cai","rrkah-fqaaa-aaaaa-aaaaq-cai"]}"#;
        assert!(parse_senders(body, 1).is_err());
    }

    /// What the transform re-emits is what the parser accepts, so a body
    /// carrying extra fields narrows to the senders and still parses.
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
