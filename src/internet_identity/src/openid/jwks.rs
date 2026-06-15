//! The JWK seam: the single place the two JWK sources of truth diverge.
//!
//! Everything in [`super::verify`] is identical for configured and SSO
//! providers. The *only* difference is where the JWKs come from, and that
//! difference lives here as [`JwkSource`]:
//!
//! - [`JwkSource::Configured`] — stable storage (memory id 24), seeded at
//!   install and timer-refreshed. Always synchronously `Ready`.
//! - [`JwkSource::Sso`] — the on-demand single-flight JWKS cache, keyed by
//!   `jwks_uri`. May be `Pending` on a cold cache.
//!
//! This module also owns the shared JWKS fetch + deterministic transform, used
//! by both sources (the configured timer and the SSO cache fill).

use super::{configured, sso, VerifyMode};
use crate::single_flight_cache::Cached;
use identity_jose::jwk::Jwk;

/// Which of the two JWK sources of truth backs a given provider. No trait
/// object — a plain two-arm enum, matched in exactly one place ([`read_jwks`]).
pub(super) enum JwkSource {
    /// Configured provider: JWKs persisted in stable storage under this issuer.
    Configured(String),
    /// SSO provider: JWKs fetched on demand into the single-flight cache under
    /// this `jwks_uri`.
    Sso(String),
}

/// Read the JWKs for a source.
///
/// In [`VerifyMode::Update`] a cold SSO cache spawns the outcall fill and reads
/// `Pending`; in [`VerifyMode::Query`] it only peeks (a query cannot spawn an
/// outcall), so `Pending` means "retry through the update path". The configured
/// arm is a stable-storage read, always `Ready`, regardless of mode.
pub(super) fn read_jwks(source: &JwkSource, mode: VerifyMode) -> Cached<Vec<Jwk>> {
    match source {
        JwkSource::Configured(issuer) => Cached::Ready(configured::read_stable_jwks(issuer)),
        JwkSource::Sso(jwks_uri) => match mode {
            VerifyMode::Update => sso::get_jwks(jwks_uri),
            VerifyMode::Query => sso::peek_jwks(jwks_uri),
        },
    }
}

// ---------------------------------------------------------------------------
// Shared JWKS fetch + transform (configured timer and SSO cache fill alike).
// ---------------------------------------------------------------------------

#[cfg(not(test))]
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

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

    serde_json::from_slice::<Certs>(response.body.as_slice())
        .map_err(|_| "Invalid JSON".into())
        .map(|res| res.keys)
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
