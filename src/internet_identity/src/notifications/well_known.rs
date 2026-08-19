//! Fetches a dApp's `/.well-known/ii-notification-senders` to learn which
//! canisters may send notifications for its origin, and caches `sender ->
//! origin`. Runs best-effort at consent, so a sender is authorized by the time
//! it sends. A canister principal doesn't encode its origin, so serving this
//! file is what proves origin ownership — there is no register endpoint.

#[cfg(not(test))]
use super::sender::cache_sender;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};

#[cfg(not(test))]
#[derive(serde::Serialize, serde::Deserialize)]
struct SendersDoc {
    senders: Vec<String>,
}

/// Fetch the origin's well-known senders and cache each `canister -> origin`.
/// Best-effort: any failure leaves the cache unchanged, and a send from an
/// uncached canister is rejected until a later consent refetches.
#[cfg(not(test))]
pub async fn fetch_and_cache(origin: FrontendHostname, now_ns: Timestamp) {
    let Ok(senders) = fetch_senders(&origin).await else {
        return;
    };
    for text in senders {
        if let Ok(sender) = candid::Principal::from_text(&text) {
            cache_sender(sender, origin.clone(), now_ns);
        }
    }
}

#[cfg(test)]
pub async fn fetch_and_cache(_origin: FrontendHostname, _now_ns: Timestamp) {}

#[cfg(not(test))]
async fn fetch_senders(origin: &str) -> Result<Vec<String>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
    };

    const WELL_KNOWN_PATH: &str = "/.well-known/ii-notification-senders";
    const MAX_RESPONSE_BYTES: u64 = 4 * 1024;
    const MAX_SENDERS: usize = 20;
    const FETCH_CYCLES: u128 = 30_000_000_000;

    let request = CanisterHttpRequestArgument {
        url: format!("{origin}{WELL_KNOWN_PATH}"),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(MAX_RESPONSE_BYTES),
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

    let (response,) = http_request_with_closure(request, FETCH_CYCLES, transform_senders)
        .await
        .map_err(|(_, err)| err)?;
    let mut doc: SendersDoc =
        serde_json::from_slice(response.body.as_slice()).map_err(|_| "invalid JSON".to_string())?;
    doc.senders.truncate(MAX_SENDERS);
    Ok(doc.senders)
}

// Nodes can receive the sender list in any order, so sort it for a deterministic
// response every node agrees on. Traps on a bad status/body — a transform can't
// return an error.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_senders(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use ic_cdk::api::management_canister::http_request::HttpResponse;
    use ic_cdk::trap;

    const HTTP_STATUS_OK: u8 = 200;
    if response.status != HTTP_STATUS_OK {
        trap("well-known senders: non-200 status");
    }
    let mut doc: SendersDoc = serde_json::from_slice(response.body.as_slice())
        .unwrap_or_else(|_| trap("well-known senders: invalid JSON"));
    doc.senders.sort();
    let body =
        serde_json::to_vec(&doc).unwrap_or_else(|_| trap("well-known senders: invalid JSON"));
    HttpResponse { body, ..response }
}
