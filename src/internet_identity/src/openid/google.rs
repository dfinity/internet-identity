use candid::{Deserialize, Nat};
use ic_cdk::api::management_canister::http_request::{
    http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod, HttpResponse,
};
use ic_cdk::{spawn, trap};
use ic_cdk_timers::set_timer;
use identity_jose::jwk::Jwk;
use serde::Serialize;
use std::cell::RefCell;
use std::cmp::min;
use std::convert::Into;
use std::time::Duration;

const CERTS_URL: &str = "https://www.googleapis.com/oauth2/v3/certs";

// The amount of cycles needed to make the HTTP outcall with a large enough margin
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

const HTTP_STATUS_OK: u8 = 200;

// Fetch the Google certs every hour, the responses are always
// valid for at least 5 hours so that should be enough margin.
const FETCH_CERTS_INTERVAL: u64 = 60 * 60;

#[derive(Serialize, Deserialize)]
struct GoogleCerts {
    keys: Vec<Jwk>,
}

thread_local! {
    static GOOGLE_CERTS: RefCell<Vec<Jwk>> = const { RefCell::new(vec![]) };
}

pub fn setup_timers() {
    // Fetch the certs directly after canister initialization.
    schedule_fetch_certs(None);
}

fn schedule_fetch_certs(delay: Option<u64>) {
    set_timer(Duration::from_secs(delay.unwrap_or(0)), move || {
        spawn(async move {
            let new_delay = match fetch_certs().await {
                Ok(google_certs) => {
                    GOOGLE_CERTS.replace(google_certs);
                    FETCH_CERTS_INTERVAL
                }
                // Try again earlier with backoff if fetch failed, the HTTP outcall responses
                // aren't the same across nodes when we fetch at the moment of key rotation.
                Err(_) => min(FETCH_CERTS_INTERVAL, delay.unwrap_or(60) * 2),
            };
            schedule_fetch_certs(Some(new_delay));
        });
    });
}

async fn fetch_certs() -> Result<Vec<Jwk>, String> {
    let request = CanisterHttpRequestArgument {
        url: CERTS_URL.into(),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: None,
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

    let (response,) = http_request_with_closure(request, CERTS_CALL_CYCLES, transform_certs)
        .await
        .map_err(|(_, err)| err)?;

    serde_json::from_slice::<GoogleCerts>(response.body.as_slice())
        .map_err(|_| "Invalid JSON".into())
        .map(|res| res.keys)
}

// The Google API occasionally returns a response with keys and their properties in random order,
// so we deserialize, sort the keys and serialize to make the response the same across all nodes.
//
// This function traps since HTTP outcall transforms can't return or log errors anyway.
#[allow(clippy::needless_pass_by_value)]
fn transform_certs(response: HttpResponse) -> HttpResponse {
    if response.status != HTTP_STATUS_OK {
        trap("Invalid response status")
    };

    let certs: GoogleCerts =
        serde_json::from_slice(response.body.as_slice()).unwrap_or_else(|_| trap("Invalid JSON"));

    let mut sorted_keys = certs.keys.clone();
    sorted_keys.sort_by_key(|key| key.kid().unwrap().to_owned());

    let body = serde_json::to_vec(&GoogleCerts { keys: sorted_keys })
        .unwrap_or_else(|_| trap("Invalid JSON"));

    // All headers are ignored including the Cache-Control header, instead we fetch the certs
    // hourly since responses are always valid for at least 5 hours based on analysis of the
    // Cache-Control header over a timespan of multiple days, so hourly is a large enough margin.
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

#[test]
fn should_transform_certs_to_same() {
    let input = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![HttpHeader {
            name: "Cache-Control".into(),
            value: "public, max-age=18544, must-revalidate, no-transform".into()
        }],
        body: Vec::from(br#"{"keys":[{"e":"AQAB","alg":"RS256","kty":"RSA","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","use":"sig"},{"use":"sig","alg":"RS256","kty":"RSA","e":"AQAB","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","kid":"89ce3598c473af1bda4bff95e6c8736450206fba"}]}"#)
    };
    let expected = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body: Vec::from(br#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"89ce3598c473af1bda4bff95e6c8736450206fba","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","e":"AQAB"}]}"#)
    };

    assert_eq!(transform_certs(input), expected);
}
