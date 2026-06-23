//! The JWK seam: where a provider's JWKs come from.
//!
//! [`super::verify`] runs identically for configured and SSO providers; the one
//! input that differs is the JWK set, sourced per [`JwkSource`]:
//!
//! - [`JwkSource::Configured`] — stable storage (memory id 24), seeded at
//!   install and refreshed on a timer. Always synchronously `Ready`.
//! - [`JwkSource::Sso`] — the on-demand single-flight JWKS cache, keyed by
//!   `jwks_uri`. `Pending` until the cache is warm.
//!
//! This module also owns the JWKS fetch + deterministic transform used by both
//! the configured refresh timer and the SSO cache fill.

use super::{configured, sso};
use crate::single_flight_cache::Cached;
use identity_jose::jwk::Jwk;

/// Which JWK source backs a given provider, matched in exactly one place
/// ([`read_jwks`]).
pub(super) enum JwkSource {
    /// Configured provider: JWKs in stable storage under this issuer.
    Configured(String),
    /// SSO provider: JWKs in the on-demand cache under this `jwks_uri`.
    Sso(String),
}

/// Read the JWKs for a source. Peek-only, so it's safe from a query; the SSO
/// arm reads the cache without spawning a fill (an update drives the fill via
/// [`sso::prefetch`]), and reports `Pending` until the cache is warm. The
/// configured arm is a stable-storage read, always `Ready`.
pub(super) fn read_jwks(source: &JwkSource) -> Cached<Vec<Jwk>> {
    match source {
        JwkSource::Configured(issuer) => Cached::Ready(configured::read_stable_jwks(issuer)),
        JwkSource::Sso(jwks_uri) => sso::read_jwks(jwks_uri),
    }
}

// ---------------------------------------------------------------------------
// JWKS fetch + transform, shared by the configured refresh timer and the SSO
// cache fill.
// ---------------------------------------------------------------------------

#[cfg(not(test))]
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

/// Response-size cap for a JWKS fetch. Real OIDC key sets run to several KB and
/// often embed `x5c` certificate chains — Microsoft's is ~14.5 KB — so 32 KiB
/// leaves headroom (including key-rotation overlap) while still bounding the
/// fill's transient buffer and, together with the per-entry LRU, the SSO JWKS
/// cache's memory. Deliberately broad: this fetch is shared with the configured
/// Google/Microsoft refresh, which must not be rejected.
#[cfg(not(test))]
const JWKS_MAX_RESPONSE_BYTES: u64 = 32 * 1024;

/// Cap on the number of keys kept from a JWKS. Verification matches by `kid` and
/// real providers publish well under this (Microsoft ~8); keys beyond the cap
/// are dropped. Bounds both the stored key set and the verify-time scan, so a
/// provider serving an absurd number of keys only degrades its own SSO entry.
#[cfg(not(test))]
const JWKS_MAX_KEYS: usize = 20;

#[cfg(not(test))]
#[derive(serde::Serialize, serde::Deserialize)]
pub(super) struct Certs {
    pub keys: Vec<Jwk>,
}

/// Fetch and parse a JWKS document from `jwks_uri`.
#[cfg(not(test))]
pub(super) async fn fetch_jwks(jwks_uri: String) -> Result<Vec<Jwk>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
    };

    let request = CanisterHttpRequestArgument {
        url: jwks_uri,
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(JWKS_MAX_RESPONSE_BYTES),
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

    serde_json::from_slice::<Certs>(response.body.as_slice())
        .map_err(|_| "Invalid JSON".into())
        .map(|res| {
            // Bound the stored key set; verification matches by `kid` and real
            // providers publish far fewer than this.
            let mut keys = res.keys;
            keys.truncate(JWKS_MAX_KEYS);
            keys
        })
}

// OpenID APIs occasionally return responses with keys and their properties in random order,
// so we deserialize, sort the keys and serialize to make the response the same across all nodes.
//
// This function traps since HTTP outcall transforms can't return or log errors anyway.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_certs(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use candid::Nat;
    use ic_cdk::api::management_canister::http_request::HttpResponse;
    use ic_cdk::trap;

    const HTTP_STATUS_OK: u8 = 200;
    if response.status != HTTP_STATUS_OK {
        trap("Invalid response status")
    }

    let certs: Certs =
        serde_json::from_slice(response.body.as_slice()).unwrap_or_else(|_| trap("Invalid JSON"));

    let mut sorted_keys = certs.keys.clone();
    sorted_keys.sort_by_key(|key| key.kid().unwrap_or_else(|| trap("Invalid JSON")).to_owned());

    let body =
        serde_json::to_vec(&Certs { keys: sorted_keys }).unwrap_or_else(|_| trap("Invalid JSON"));

    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}
