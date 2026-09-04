//! Fetches a dApp's `/.well-known/ii-notification-senders` to learn which
//! canisters may send notifications for its origin, and records a verified
//! `(canister, origin)` binding for each. Runs at consent and must succeed, so a
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
    // The origin is canonicalized to the legacy gateway for principal
    // stability, which does not serve every canister, so the well-known is
    // looked for on each gateway before the grant is refused.
    let mut errors = Vec::new();
    let mut senders = None;
    for candidate in super::fetch_origins(&origin) {
        match fetch_senders(&candidate).await {
            Ok(fetched) => {
                senders = Some(fetched);
                break;
            }
            Err(err) => errors.push(format!("{candidate}: {err}")),
        }
    }
    let Some(senders) = senders else {
        return Err(format!(
            "well-known senders unreachable ({})",
            errors.join("; ")
        ));
    };

    let mut bound = 0;
    for text in senders {
        if let Ok(sender) = candid::Principal::from_text(&text) {
            // Bind under the canonical origin: that is what the send path and
            // the recorded consent key against.
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
    // Counts headers, not just the body. A certified asset carries an
    // `IC-Certificate` header holding the certificate and hash tree — a few KB
    // on its own, and larger the more assets the canister serves — so a limit
    // sized for the tiny JSON body rejects the response before it arrives.
    const MAX_RESPONSE_BYTES: u64 = 64 * 1024;
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
    if response.status != 200u32 {
        return Err(format!("status {}", response.status));
    }
    let mut doc: SendersDoc =
        serde_json::from_slice(response.body.as_slice()).map_err(|_| "invalid JSON".to_string())?;
    doc.senders.truncate(MAX_SENDERS);
    Ok(doc.senders)
}

// The response must come back byte-identical on every node or the replicated
// outcall never reaches consensus, so this rebuilds it from scratch — no headers
// (a gateway's `Date` and request ids differ per node) and the sender list
// sorted, since nodes can receive it in any order.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_senders(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use ic_cdk::api::management_canister::http_request::HttpResponse;

    const HTTP_STATUS_OK: u8 = 200;
    // Headers are dropped and the body rewritten so every replica sees the same
    // response: per-node headers (`Date`) and a gateway's own error page would
    // otherwise leave the outcall without consensus. A non-200 keeps its status
    // with an empty body and `fetch_senders` turns it into an error — trapping
    // here would abort the whole message, leaving no room to try the other
    // gateway.
    if response.status != HTTP_STATUS_OK {
        return HttpResponse {
            status: response.status,
            headers: vec![],
            body: vec![],
        };
    }
    let body = match serde_json::from_slice::<SendersDoc>(response.body.as_slice()) {
        Ok(mut doc) => {
            doc.senders.sort();
            serde_json::to_vec(&doc).unwrap_or_default()
        }
        // An empty body fails the caller's parse, without trapping.
        Err(_) => vec![],
    };
    HttpResponse {
        status: response.status,
        headers: vec![],
        body,
    }
}
