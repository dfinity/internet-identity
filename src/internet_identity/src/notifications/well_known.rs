//! Fetches a dApp's `/.well-known/ii-notification-senders` to learn which
//! canisters may send notifications for its origin, and records a verified
//! `(canister, origin)` binding for each. Runs best-effort at consent, so a
//! sender is bound by the time it sends. Serving this file is what proves origin
//! ownership; the sender must also declare the origin at send time (see
//! `sender`), so the trust is two-way and there is no register endpoint.

#[cfg(not(test))]
use super::sender::bind_sender;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};

#[cfg(not(test))]
#[derive(serde::Serialize, serde::Deserialize)]
struct SendersDoc {
    senders: Vec<String>,
}

/// Fetch the origin's well-known senders and record a `(canister, origin)`
/// binding for each. Errors if the list can't be fetched or parsed, or names no
/// valid sender, so a consent grant that depends on it fails rather than
/// recording a consent no sender can deliver against.
#[cfg(not(test))]
pub async fn fetch_and_cache(origin: FrontendHostname, now_ns: Timestamp) -> Result<(), String> {
    let senders = fetch_senders(&origin).await?;
    let mut bound = 0;
    for text in senders {
        if let Ok(sender) = candid::Principal::from_text(&text) {
            bind_sender(sender, origin.clone(), now_ns);
            bound += 1;
        }
    }
    if bound == 0 {
        return Err("origin lists no valid notification senders".to_string());
    }
    Ok(())
}

#[cfg(test)]
pub async fn fetch_and_cache(_origin: FrontendHostname, _now_ns: Timestamp) -> Result<(), String> {
    Ok(())
}

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
