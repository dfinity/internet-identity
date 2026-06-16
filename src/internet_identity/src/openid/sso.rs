//! SSO (discoverable) OpenID providers — the second JWK source of truth.
//!
//! Unlike configured providers (timers + stable storage), SSO discovery and
//! JWKS are fetched on demand and cached in memory, never persisted. A sign-in
//! supplies the discovery domain; the canister resolves it through two
//! single-flight caches and verification reads the result. No background
//! timers, no per-domain fan-out — the two caches *are* the lookup path:
//!
//! ```text
//! domain   ─[discovery cache]─▶ { issuer, client_id, jwks_uri, auth_endpoint, scopes, name }
//! jwks_uri ─[JWKS cache]──────▶ keys
//! ```
//!
//! The discovery domain is gated by the canary allowlist.

use super::verify::{Descriptor, Stamp};
use super::OpenIDJWTVerificationError;
use crate::single_flight_cache::{self, CacheConfig, Cached, RetryBackoff, SingleFlightCache};
use identity_jose::jwk::Jwk;
use std::cell::RefCell;

#[cfg(not(test))]
use crate::state;

/// Entry lifetime for both caches: discovery metadata and JWKS change
/// infrequently, so an hour balances freshness against outcall volume.
const FRESH_FOR_SECONDS: u64 = 60 * 60;
/// Stale-if-error window: serve the last-good value through a transient fill
/// failure for up to this long past freshness before failing the caller.
const STALE_FOR_SECONDS: u64 = 60 * 60;
/// LRU cap per cache.
const MAX_ENTRIES: usize = 100;
const RETRY_BASE_SECONDS: u64 = 60;
const RETRY_MULTIPLIER: u64 = 2;
const ABANDON_FILL_AFTER_SECONDS: u64 = 120;

/// Default scopes requested when a provider's discovery document doesn't
/// advertise `scopes_supported`.
#[cfg(not(test))]
const DEFAULT_SCOPES: [&str; 3] = ["openid", "profile", "email"];

/// The result of resolving a discovery domain (hop 1 + hop 2). Carries
/// everything the redemption path needs (`issuer`, `jwks_uri`) and everything
/// the sign-in initiation path needs (`authorization_endpoint`, `scopes`,
/// `client_id`, `name`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct DiscoveredConfig {
    pub issuer: String,
    pub client_id: String,
    pub jwks_uri: String,
    pub authorization_endpoint: String,
    pub scopes: Vec<String>,
    pub name: Option<String>,
}

type DiscoveryCache = SingleFlightCache<String, DiscoveredConfig, String>;
type JwksCache = SingleFlightCache<String, Vec<Jwk>, String>;

thread_local! {
    static DISCOVERY_CACHE: RefCell<DiscoveryCache> = RefCell::new(new_discovery_cache());
    static JWKS_CACHE: RefCell<JwksCache> = RefCell::new(new_jwks_cache());
}

fn cache_config() -> CacheConfig {
    CacheConfig {
        fresh_for: FRESH_FOR_SECONDS,
        stale_for: STALE_FOR_SECONDS,
        max_entries: MAX_ENTRIES,
        backoff: RetryBackoff::new(RETRY_BASE_SECONDS, RETRY_MULTIPLIER),
        abandon_fill_after: ABANDON_FILL_AFTER_SECONDS,
    }
}

fn new_discovery_cache() -> DiscoveryCache {
    SingleFlightCache::new(discovery_fill, cache_config())
}

fn new_jwks_cache() -> JwksCache {
    SingleFlightCache::new(jwks_fill, cache_config())
}

// ---------------------------------------------------------------------------
// Lookup paths.
// ---------------------------------------------------------------------------

/// Drive the on-demand fetches for `domain` forward: ensure the discovery fill
/// is running, and once discovery has resolved, ensure the JWKS fill for its
/// `jwks_uri` is running too. Only an update may call this — it can spawn the
/// outcalls the fills make. Reads ([`peek_discovery`], [`read_jwks`]) stay
/// peek-only, so a query never needs this and never spawns. A no-op for a
/// disallowed domain.
pub(super) fn prefetch(domain: &str) {
    let domain = domain.to_ascii_lowercase();
    if !is_allowed_discovery_domain(&domain) {
        return;
    }
    if let Cached::Ready(cfg) = single_flight_cache::get(&DISCOVERY_CACHE, domain) {
        single_flight_cache::get(&JWKS_CACHE, cfg.jwks_uri);
    }
}

/// Drive the discovery fetch for `domain` (the discovery cache only — JWKS is a
/// verify-time concern). For the sign-in initiation poll, where the frontend
/// hits this from a query that read no value yet. A no-op for a disallowed
/// domain.
pub(super) fn drive_discovery(domain: &str) {
    let domain = domain.to_ascii_lowercase();
    if is_allowed_discovery_domain(&domain) {
        single_flight_cache::get(&DISCOVERY_CACHE, domain);
    }
}

/// Read the cached discovery result for `domain`, peek-only (never spawns) so
/// it's safe from a query. `Pending` means no cached value yet; the caller
/// gates on [`is_allowed_discovery_domain`] separately where it needs to tell a
/// disallowed domain apart from a cold one.
pub(super) fn peek_discovery(domain: &str) -> Cached<DiscoveredConfig> {
    single_flight_cache::peek(&DISCOVERY_CACHE, &domain.to_ascii_lowercase())
}

/// Resolve an SSO domain into a verify descriptor + `jwks_uri` from the cached
/// discovery result, cross-checking the JWT's issuer against the discovered
/// issuer. Peek-only.
pub(super) fn resolve(
    domain: &str,
    jwt_iss: &str,
) -> Result<Cached<(Descriptor, String)>, OpenIDJWTVerificationError> {
    if !is_allowed_discovery_domain(domain) {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "SSO discovery domain not allowed: {domain}"
        )));
    }
    let cfg = match peek_discovery(domain) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(cfg) => cfg,
    };
    if cfg.issuer != jwt_iss {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "JWT issuer '{jwt_iss}' does not match discovered issuer '{}' for domain '{domain}'",
            cfg.issuer
        )));
    }
    let descriptor = Descriptor {
        issuer: cfg.issuer,
        client_id: cfg.client_id,
        stamp: Stamp::Sso {
            domain: domain.to_string(),
            name: cfg.name,
        },
    };
    Ok(Cached::Ready((descriptor, cfg.jwks_uri)))
}

/// Read the cached SSO JWKs for `jwks_uri`. Peek-only.
pub(super) fn read_jwks(jwks_uri: &str) -> Cached<Vec<Jwk>> {
    single_flight_cache::peek(&JWKS_CACHE, &jwks_uri.to_string())
}

// ---------------------------------------------------------------------------
// Allowlist (canary gate).
// ---------------------------------------------------------------------------

pub fn allowed_discovery_domains() -> Vec<String> {
    #[cfg(not(test))]
    {
        let configured = state::persistent_state(|ps| ps.sso_discoverable_domains.clone());
        if let Some(domains) = configured {
            return domains;
        }
        let is_production = state::persistent_state(|ps| ps.is_production);
        match is_production {
            Some(true) => vec!["dfinity.org".to_string()],
            _ => vec!["beta.dfinity.org".to_string()],
        }
    }
    #[cfg(test)]
    {
        tests::TEST_ALLOWED.with_borrow(|d| d.clone())
    }
}

pub fn is_allowed_discovery_domain(domain: &str) -> bool {
    allowed_discovery_domains()
        .iter()
        .any(|allowed| allowed.eq_ignore_ascii_case(domain))
}

/// True if `host` (the `host:port` portion of a URL) matches an allowlist
/// entry. Used as the gate for relaxing the `https://` requirement: any domain
/// explicitly blessed by an II admin MAY publish its discovery endpoints over
/// plain HTTP, which is what makes e2e tests against `http://localhost:11107`
/// work without weakening prod's strict-HTTPS posture for unblessed hosts.
#[cfg(not(test))]
fn is_allowlisted_host(host: &str) -> bool {
    is_allowed_discovery_domain(host)
}

// ---------------------------------------------------------------------------
// Two-hop discovery fill.
// ---------------------------------------------------------------------------

/// II-specific SSO indirection document served at
/// `{domain}/.well-known/ii-openid-configuration`.
#[cfg(not(test))]
#[derive(serde::Deserialize)]
struct IIOpenIdConfiguration {
    client_id: String,
    openid_configuration: String,
    #[serde(default)]
    name: Option<String>,
}

/// OIDC discovery document — only the fields the canister needs.
#[cfg(not(test))]
#[derive(serde::Deserialize)]
struct DiscoveryDocument {
    issuer: String,
    jwks_uri: String,
    authorization_endpoint: String,
    #[serde(default)]
    scopes_supported: Option<Vec<String>>,
}

/// The discovery cache fill: hop 1 (`ii-openid-configuration`) then hop 2 (the
/// standard OIDC discovery document), with host self-assertion checks between
/// them. Errors are surfaced as `Err` (the cache backs off and serves
/// stale-if-error).
#[cfg(not(test))]
async fn discovery_fill(domain: String) -> Result<DiscoveredConfig, String> {
    // Hop 1: fetch /.well-known/ii-openid-configuration. Default to https; an
    // allowlisted loopback host (the e2e provider, which can't serve TLS) may
    // use http. The allowlist is the trust gate.
    let hop1_scheme = scheme_for_allowlisted_host(&domain);
    let hop1_url = format!("{hop1_scheme}://{domain}/.well-known/ii-openid-configuration");
    let ii_config = fetch_ii_openid_configuration(hop1_url).await?;
    validate_discovery_url(&ii_config.openid_configuration)?;

    // Hop 2: fetch the standard OIDC discovery document.
    let doc = fetch_discovery(ii_config.openid_configuration.clone()).await?;

    // The discovered issuer's host must match the openid_configuration host
    // (standard OIDC self-assertion; defends against a compromised hop-1 that
    // points jwks_uri at an unrelated issuer). The discovery domain itself is
    // allowed to differ — it hosts a custom SSO indirection.
    validate_issuer_host(&ii_config.openid_configuration, &doc.issuer)?;
    validate_discovery_url(&doc.jwks_uri)?;
    // The authorization_endpoint must live on the same host as the issuer, so a
    // tampered discovery doc can't bounce the user's auth off-host after we've
    // committed to a provider.
    validate_same_host(&doc.issuer, &doc.authorization_endpoint)?;
    validate_discovery_url(&doc.authorization_endpoint)?;

    let scopes = doc
        .scopes_supported
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| DEFAULT_SCOPES.iter().map(|s| (*s).to_string()).collect());

    Ok(DiscoveredConfig {
        issuer: doc.issuer,
        client_id: ii_config.client_id,
        jwks_uri: doc.jwks_uri,
        authorization_endpoint: doc.authorization_endpoint,
        scopes,
        name: ii_config.name,
    })
}

/// The JWKS cache fill: fetch and parse the keys at `jwks_uri`.
#[cfg(not(test))]
async fn jwks_fill(jwks_uri: String) -> Result<Vec<Jwk>, String> {
    super::jwks::fetch_jwks(jwks_uri).await
}

// In test builds the fills read from injected state instead of doing outcalls.
#[cfg(test)]
async fn discovery_fill(domain: String) -> Result<DiscoveredConfig, String> {
    tests::TEST_DISCOVERY
        .with_borrow(|m| m.get(&domain).cloned())
        .ok_or_else(|| format!("no test discovery for {domain}"))
}

#[cfg(test)]
async fn jwks_fill(jwks_uri: String) -> Result<Vec<Jwk>, String> {
    tests::TEST_JWKS
        .with_borrow(|m| m.get(&jwks_uri).cloned())
        .ok_or_else(|| format!("no test jwks for {jwks_uri}"))
}

// ---------------------------------------------------------------------------
// HTTP fetches + URL validation (non-test).
// ---------------------------------------------------------------------------

#[cfg(not(test))]
const DISCOVERY_CALL_CYCLES: u128 = 30_000_000_000;

#[cfg(not(test))]
async fn fetch_ii_openid_configuration(url: String) -> Result<IIOpenIdConfiguration, String> {
    let body = http_get_json(url).await?;
    serde_json::from_slice::<IIOpenIdConfiguration>(&body)
        .map_err(|_| "Invalid ii-openid-configuration JSON".into())
}

#[cfg(not(test))]
async fn fetch_discovery(discovery_url: String) -> Result<DiscoveryDocument, String> {
    let body = http_get_json(discovery_url).await?;
    serde_json::from_slice::<DiscoveryDocument>(&body).map_err(|_| "Invalid discovery JSON".into())
}

#[cfg(not(test))]
async fn http_get_json(url: String) -> Result<Vec<u8>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
    };

    let request = CanisterHttpRequestArgument {
        url,
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

    let (response,) =
        http_request_with_closure(request, DISCOVERY_CALL_CYCLES, transform_discovery)
            .await
            .map_err(|(_, err)| err)?;
    Ok(response.body)
}

/// Transform for JSON discovery responses (both hops). Re-serializes
/// deterministically so subnet nodes reach consensus.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_discovery(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use candid::Nat;
    use ic_cdk::api::management_canister::http_request::HttpResponse;

    const HTTP_STATUS_OK: u8 = 200;
    if response.status != HTTP_STATUS_OK {
        return HttpResponse {
            status: response.status,
            headers: vec![],
            body: b"Invalid discovery response status".to_vec(),
        };
    }
    let Ok(doc) = serde_json::from_slice::<serde_json::Value>(response.body.as_slice()) else {
        return HttpResponse {
            status: Nat::from(HTTP_STATUS_OK),
            headers: vec![],
            body: b"Invalid discovery JSON".to_vec(),
        };
    };
    let Ok(body) = serde_json::to_vec(&doc) else {
        return HttpResponse {
            status: Nat::from(HTTP_STATUS_OK),
            headers: vec![],
            body: b"Failed to re-serialize discovery JSON".to_vec(),
        };
    };
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

/// Ensure a URL is syntactically valid and uses an acceptable scheme. `https`
/// is always accepted; `http` only when the host is on the SSO allowlist.
#[cfg(not(test))]
fn validate_discovery_url(url: &str) -> Result<(), String> {
    let parsed = url::Url::parse(url).map_err(|e| format!("parse error: {e}"))?;
    match parsed.scheme() {
        "https" => Ok(()),
        "http" => {
            let host = host_with_port(&parsed).ok_or_else(|| format!("URL has no host: {url}"))?;
            if is_allowlisted_host(&host) {
                Ok(())
            } else {
                Err(format!(
                    "http scheme not allowed for non-allowlisted host '{host}'"
                ))
            }
        }
        other => Err(format!("expected http(s) scheme, got '{other}'")),
    }
}

/// "host" or "host:port" portion of a URL, lowercased.
#[cfg(not(test))]
fn host_with_port(url: &url::Url) -> Option<String> {
    let host = url.host_str()?.to_lowercase();
    Some(match url.port() {
        Some(p) => format!("{host}:{p}"),
        None => host,
    })
}

/// Scheme for the hop-1 URL of an allowlisted domain: loopback (the e2e test
/// provider) gets `http`, everything else `https`.
#[cfg(not(test))]
fn scheme_for_allowlisted_host(host: &str) -> &'static str {
    let bare = host.split(':').next().unwrap_or(host).to_ascii_lowercase();
    if matches!(bare.as_str(), "localhost" | "127.0.0.1") {
        "http"
    } else {
        "https"
    }
}

/// Validate that `issuer` and the OpenID configuration URL share a host.
#[cfg(not(test))]
fn validate_issuer_host(openid_config_url: &str, issuer: &str) -> Result<(), String> {
    validate_same_host(openid_config_url, issuer)
}

/// Validate that two URLs resolve to the same host (case-insensitive).
#[cfg(not(test))]
fn validate_same_host(reference_url: &str, other_url: &str) -> Result<(), String> {
    let reference_host = url::Url::parse(reference_url)
        .map_err(|e| format!("Invalid URL: {e}"))?
        .host_str()
        .ok_or_else(|| format!("No host in URL: {reference_url}"))?
        .to_lowercase();
    let other_host = url::Url::parse(other_url)
        .map_err(|e| format!("Invalid URL: {e}"))?
        .host_str()
        .ok_or_else(|| format!("No host in URL: {other_url}"))?
        .to_lowercase();
    if reference_host != other_host {
        return Err(format!(
            "Host '{other_host}' does not match expected host '{reference_host}'"
        ));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests.
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::single_flight_cache::{run_detached, set_test_now};
    use std::collections::HashMap;

    thread_local! {
        pub(super) static TEST_ALLOWED: RefCell<Vec<String>> = const { RefCell::new(vec![]) };
        pub(super) static TEST_DISCOVERY: RefCell<HashMap<String, DiscoveredConfig>> = RefCell::new(HashMap::new());
        pub(super) static TEST_JWKS: RefCell<HashMap<String, Vec<Jwk>>> = RefCell::new(HashMap::new());
    }

    fn reset() {
        set_test_now(1_700_000_000);
        TEST_ALLOWED.with_borrow_mut(|d| *d = vec!["example.org".to_string()]);
        TEST_DISCOVERY.with_borrow_mut(|m| m.clear());
        TEST_JWKS.with_borrow_mut(|m| m.clear());
        DISCOVERY_CACHE.with(|c| *c.borrow_mut() = new_discovery_cache());
        JWKS_CACHE.with(|c| *c.borrow_mut() = new_jwks_cache());
    }

    fn sample_config() -> DiscoveredConfig {
        DiscoveredConfig {
            issuer: "https://idp.example.org".to_string(),
            client_id: "client-123".to_string(),
            jwks_uri: "https://idp.example.org/jwks".to_string(),
            authorization_endpoint: "https://idp.example.org/authorize".to_string(),
            scopes: vec!["openid".to_string()],
            name: Some("Example".to_string()),
        }
    }

    #[test]
    fn resolve_rejects_disallowed_domain() {
        reset();
        assert!(!is_allowed_discovery_domain("not-allowed.com"));
        assert!(is_allowed_discovery_domain("example.org"));
        // The verify path rejects a disallowed domain.
        assert!(resolve("not-allowed.com", "https://idp.example.org").is_err());
    }

    #[test]
    fn prefetch_then_peek_resolves() {
        reset();
        TEST_DISCOVERY.with_borrow_mut(|m| {
            m.insert("example.org".to_string(), sample_config());
        });
        // Reading without prefetching stays Pending — no fill was spawned.
        assert_eq!(peek_discovery("example.org"), Cached::Pending);
        // Prefetch spawns the discovery fill; the read is still Pending until
        // the detached fill runs.
        prefetch("example.org");
        assert_eq!(peek_discovery("example.org"), Cached::Pending);
        run_detached();
        match peek_discovery("example.org") {
            Cached::Ready(cfg) => assert_eq!(cfg.issuer, "https://idp.example.org"),
            other => panic!("expected Ready, got {other:?}"),
        }
    }

    #[test]
    fn reads_never_spawn() {
        reset();
        TEST_DISCOVERY.with_borrow_mut(|m| {
            m.insert("example.org".to_string(), sample_config());
        });
        // peek_discovery/resolve are peek-only: without a prefetch they stay
        // Pending even after draining detached tasks, because none was spawned.
        assert_eq!(peek_discovery("example.org"), Cached::Pending);
        assert!(matches!(
            resolve("example.org", "https://idp.example.org"),
            Ok(Cached::Pending)
        ));
        run_detached();
        assert_eq!(peek_discovery("example.org"), Cached::Pending);
    }

    #[test]
    fn resolve_cross_checks_issuer() {
        reset();
        TEST_DISCOVERY.with_borrow_mut(|m| {
            m.insert("example.org".to_string(), sample_config());
        });
        prefetch("example.org");
        run_detached();
        // Wrong issuer is rejected.
        assert!(resolve("example.org", "https://evil.example.org").is_err());
        // Matching issuer resolves to a descriptor + jwks_uri.
        match resolve("example.org", "https://idp.example.org") {
            Ok(Cached::Ready((descriptor, jwks_uri))) => {
                assert_eq!(jwks_uri, "https://idp.example.org/jwks");
                assert!(matches!(descriptor.stamp, Stamp::Sso { .. }));
            }
            _ => panic!("expected Ready"),
        }
    }
}
