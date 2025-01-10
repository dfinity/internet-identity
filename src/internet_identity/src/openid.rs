use ic_cdk::spawn;
use ic_cdk_timers::{set_timer, set_timer_interval};
use identity_jose::jwk::Jwk;
use std::cell::RefCell;
use std::time::Duration;

pub(crate) mod google;

pub const FETCH_GOOGLE_CERTS_INTERVAL: Duration = Duration::from_secs(60 * 60);

thread_local! {
    static GOOGLE_CERTS: RefCell<Vec<Jwk>> = const { RefCell::new(vec![]) };
}

pub fn setup_timers() {
    // Fetch the Google certs directly after canister initialization and then every hour,
    // the responses are always valid for at least 5 hours so that should be enough margin.
    set_timer(Duration::from_secs(0), || spawn(fetch_google_certs(None)));
    set_timer_interval(FETCH_GOOGLE_CERTS_INTERVAL, || {
        spawn(fetch_google_certs(None));
    });
}

async fn fetch_google_certs(previous_delay: Option<u64>) {
    if let Ok(google_certs) = google::get_certs().await {
        GOOGLE_CERTS.replace(google_certs);
    } else {
        // Try again with backoff if fetch failed, the HTTP outcall responses aren't the same
        // across nodes when we fetch the certificates just at the moment of key rotation.
        let backoff = Option::map(previous_delay, |value| value * 2).unwrap_or(60);
        if backoff < FETCH_GOOGLE_CERTS_INTERVAL.as_secs() {
            set_timer(Duration::from_secs(backoff), move || {
                spawn(fetch_google_certs(Some(backoff)));
            });
        }
    }
}
