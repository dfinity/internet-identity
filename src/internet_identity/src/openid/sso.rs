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
//! A caller-supplied discovery domain is validated as a bare authority (see
//! `validate_discovery_domain`); there is no domain allowlist.

use super::verify::{Descriptor, SsoProvider};
use super::OpenIDJWTVerificationError;
use crate::openid::AudClaim;
use crate::single_flight_cache::{self, CacheConfig, Cached, RetryBackoff, SingleFlightCache};
use identity_jose::jwk::Jwk;
use sha2::{Digest, Sha256};
use std::cell::RefCell;

/// Default for the well-known's optional `stable_identifier_claim`. `sub` is the
/// OIDC subject claim; when an org's `sub` is the same across its OIDC clients,
/// II keys identity on it directly (no bridge). An org with a pairwise `sub`
/// (a different value per client, e.g. Entra) overrides this with a claim that
/// is stable across clients (e.g. `oid`).
pub(super) const DEFAULT_STABLE_IDENTIFIER_CLAIM: &str = "sub";

/// The non-default stable-identifier claim, or `None` for a `sub` org (which
/// keys identity on the token `sub` directly and needs no bridge).
pub(super) fn non_default_stable_claim(claim: &str) -> Option<String> {
    (claim != DEFAULT_STABLE_IDENTIFIER_CLAIM).then(|| claim.to_string())
}

/// Max `app_clients` entries accepted from an org's well-known. The parsed map
/// is retained in the in-memory SSO discovery cache (one per cached domain), so
/// this bounds its heap footprint there — a generous per-org ceiling. An
/// over-cap map is rejected, never truncated (see `validate_app_clients`).
pub(super) const MAX_APP_CLIENTS: usize = 100;

/// Maximum length of an app_clients client_id.
const MAX_APP_CLIENT_ID_LENGTH: usize = 255;

#[cfg(not(test))]
use crate::state;

// Cache sizing. II runs on a system subnet (HTTP outcalls cost no cycles) and
// ingress flooding is handled by the boundary nodes, so these caches don't need
// to rate-limit outcalls or account for cycles. Their one DDoS-relevant job is
// to keep canister state *bounded and fairly evicted* — the discovery domain
// and `jwks_uri` are caller-controlled and unbounded.
//
// Discovery and JWKS are a single coupled flow (`domain → discovery cache →
// jwks_uri → JWKS cache`): a verification needs *both* the domain's discovery
// entry and its JWKS entry warm. So the two caches share one budget rather than
// sizing independently — different caps would just leave one cache holding
// entries the other can't back. The shared cap is bounded by the larger (JWKS)
// entry (≤ `jwks::JWKS_MAX_RESPONSE_BYTES`, 32 KiB); every fill is also bounded
// in bytes (see `DISCOVERY_MAX_RESPONSE_BYTES` and `jwks::JWKS_MAX_RESPONSE_BYTES`).

/// Entry lifetime for both caches: discovery metadata and JWKS change
/// infrequently, so an hour balances freshness against outcall volume.
const FRESH_FOR_SECONDS: u64 = 60 * 60;
/// Stale-if-error window: serve the last-good value through a transient fill
/// failure for up to this long past freshness before failing the caller.
const STALE_FOR_SECONDS: u64 = 60 * 60;
/// Shared LRU cap for both SSO caches — they back one coupled flow, so one
/// budget keeps them coherent (this many domains cached end-to-end,
/// discovery → keys). Worst-case memory is dominated by the JWKS cache at
/// `jwks::JWKS_MAX_RESPONSE_BYTES` (32 KiB) per entry: 5k × 32 KiB ≈ 160 MB,
/// plus ~10 MB of (much smaller) discovery entries ≈ ~170 MB — about 5-6 % of
/// the ~3 GB Wasm heap, leaving the bulk for core II operations while keeping
/// wide headroom so a flood of distinct domains can't evict the providers real
/// users rely on.
const SSO_CACHE_MAX_ENTRIES: usize = 5_000;
const RETRY_BASE_SECONDS: u64 = 60;
const RETRY_MULTIPLIER: u64 = 2;
const ABANDON_FILL_AFTER_SECONDS: u64 = 120;

/// Response-size cap for the two discovery hops (`ii-openid-configuration` and
/// the OIDC discovery document). Sized to fit up to `MAX_APP_CLIENTS`
/// `origin -> client_id` entries (each key up to ~140 bytes when hashed) with
/// headroom.
#[cfg(not(test))]
const DISCOVERY_MAX_RESPONSE_BYTES: u64 = 64 * 1024;
/// Cap on the number of `scopes_supported` stored per discovery entry. `scopes`
/// is the only unbounded field in `DiscoveredConfig`; capping it keeps a
/// discovery entry ~1-2 KB so its share of the shared budget stays small. II
/// only needs `openid`/`email`/`profile`, so extra advertised scopes are
/// irrelevant.
#[cfg(not(test))]
const DISCOVERY_MAX_SCOPES: usize = 32;

/// Default scopes requested when a provider's discovery document doesn't
/// advertise `scopes_supported`.
#[cfg(not(test))]
const DEFAULT_SCOPES: [&str; 3] = ["openid", "profile", "email"];

/// Maximum length of the client_id from the well-known.
#[cfg(not(test))]
const MAX_CLIENT_ID_LENGTH: usize = 255;
/// Maximum length of the issuer from the OIDC configuration.
#[cfg(not(test))]
const MAX_ISSUER_LENGTH: usize = 255;
/// Maximum length of the jwks_uri from the OIDC configuration.
#[cfg(not(test))]
const MAX_JWKS_URI_LENGTH: usize = 255;
/// Maximum length of the display name from the well-known.
#[cfg(not(test))]
const MAX_SSO_NAME_LENGTH: usize = 255;
/// Maximum length of an SSO discovery domain.
const MAX_DOMAIN_LENGTH: usize = 255;

/// The result of resolving a discovery domain (hop 1 + hop 2). Carries
/// everything the redemption path needs (`issuer`, `jwks_uri`) and everything
/// the sign-in initiation path needs (`authorization_endpoint`, `scopes`,
/// `client_id`, `name`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct DiscoveredConfig {
    pub issuer: String,
    /// The org's primary OIDC client; identity is keyed on it.
    pub client_id: String,
    pub jwks_uri: String,
    pub authorization_endpoint: String,
    pub scopes: Vec<String>,
    pub name: Option<String>,
    pub app_clients: Vec<AppClient>,
    /// When true, an origin not in `app_clients` is denied.
    pub gate_all_apps: bool,
    /// Claim holding the cross-client-stable identifier (default `sub`).
    pub stable_identifier_claim: String,
}

/// One `origin -> client_id` entry from `app_clients`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct AppClient {
    pub key: AppClientKey,
    pub client_id: String,
}

/// An `app_clients` key: a cleartext origin or a salted hash
/// (`sha256(origin || salt)` hex + hex `salt`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum AppClientKey {
    Cleartext(String),
    Hashed { hash: String, salt: String },
}

impl AppClientKey {
    /// Parse a raw `app_clients` key: hashed iff it has the exact `<hex>:<hex>`
    /// shape, else cleartext.
    fn parse(raw: &str) -> Self {
        if let Some((hash, salt)) = raw.split_once(':') {
            let is_hex = |s: &str| !s.is_empty() && s.bytes().all(|b| b.is_ascii_hexdigit());
            if is_hex(hash) && is_hex(salt) {
                return AppClientKey::Hashed {
                    hash: hash.to_ascii_lowercase(),
                    salt: salt.to_string(),
                };
            }
        }
        AppClientKey::Cleartext(raw.to_string())
    }

    /// Does this key designate `origin`?
    fn matches(&self, origin: &str) -> bool {
        match self {
            AppClientKey::Cleartext(o) => o == origin,
            AppClientKey::Hashed { hash, salt } => {
                let mut hasher = Sha256::new();
                hasher.update(origin.as_bytes());
                hasher.update(salt.as_bytes());
                let computed = hex::encode(hasher.finalize());
                computed.eq_ignore_ascii_case(hash)
            }
        }
    }
}

/// An origin denied under `gate_all_apps` (not listed in `app_clients`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Forbidden;

/// Parse the raw `app_clients` map, rejecting (never truncating) a map over
/// `MAX_APP_CLIENTS` — truncation could silently drop a gated origin into the
/// open fallback.
pub(super) fn validate_app_clients(
    entries: &std::collections::HashMap<String, String>,
) -> Result<Vec<AppClient>, String> {
    if entries.len() > MAX_APP_CLIENTS {
        return Err(format!(
            "app_clients exceeds the {MAX_APP_CLIENTS}-entry cap ({} entries)",
            entries.len()
        ));
    }
    entries
        .iter()
        .map(|(key, client_id)| {
            if client_id.len() > MAX_APP_CLIENT_ID_LENGTH {
                return Err(format!(
                    "app_clients client_id exceeds the {MAX_APP_CLIENT_ID_LENGTH}-byte cap"
                ));
            }
            Ok(AppClient {
                key: AppClientKey::parse(key),
                client_id: client_id.clone(),
            })
        })
        .collect()
}

impl DiscoveredConfig {
    /// The client id `origin` signs in against: its per-app client if listed in
    /// `app_clients`, else the org's primary client — unless `gate_all_apps`
    /// denies an unlisted origin (`Forbidden`).
    pub(super) fn resolve_client_for_origin(&self, origin: &str) -> Result<String, Forbidden> {
        for app in &self.app_clients {
            if app.key.matches(origin) {
                return Ok(app.client_id.clone());
            }
        }
        if self.gate_all_apps {
            Err(Forbidden)
        } else {
            Ok(self.client_id.clone())
        }
    }
}

type DiscoveryCache = SingleFlightCache<String, DiscoveredConfig, String>;
type JwksCache = SingleFlightCache<String, Vec<Jwk>, String>;

thread_local! {
    static DISCOVERY_CACHE: RefCell<DiscoveryCache> = RefCell::new(new_discovery_cache());
    static JWKS_CACHE: RefCell<JwksCache> = RefCell::new(new_jwks_cache());
}

fn cache_config(max_entries: usize) -> CacheConfig {
    CacheConfig {
        fresh_for: FRESH_FOR_SECONDS,
        stale_for: STALE_FOR_SECONDS,
        max_entries,
        backoff: RetryBackoff::new(RETRY_BASE_SECONDS, RETRY_MULTIPLIER),
        abandon_fill_after: ABANDON_FILL_AFTER_SECONDS,
    }
}

fn new_discovery_cache() -> DiscoveryCache {
    SingleFlightCache::new(discovery_fill, cache_config(SSO_CACHE_MAX_ENTRIES))
}

fn new_jwks_cache() -> JwksCache {
    SingleFlightCache::new(jwks_fill, cache_config(SSO_CACHE_MAX_ENTRIES))
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
    if validate_discovery_domain(&domain).is_err() {
        return;
    }
    if let Cached::Ready(discovery_config) = single_flight_cache::get(&DISCOVERY_CACHE, domain) {
        single_flight_cache::get(&JWKS_CACHE, discovery_config.jwks_uri);
    }
}

/// Drive the discovery fetch for `domain` (the discovery cache only — JWKS is a
/// verify-time concern). For the sign-in initiation poll, where the frontend
/// hits this from a query that read no value yet. A no-op for a disallowed
/// domain.
pub(super) fn drive_discovery(domain: &str) {
    let domain = domain.to_ascii_lowercase();
    if validate_discovery_domain(&domain).is_ok() {
        single_flight_cache::get(&DISCOVERY_CACHE, domain);
    }
}

/// Read the cached discovery result for `domain`, peek-only (never spawns) so
/// it's safe from a query. `Pending` means no cached value yet; the caller
/// gates on [`validate_discovery_domain`] separately where it needs to
/// tell a disallowed domain apart from a cold one.
pub(super) fn peek_discovery(domain: &str) -> Cached<DiscoveredConfig> {
    single_flight_cache::peek(&DISCOVERY_CACHE, &domain.to_ascii_lowercase())
}

/// Resolve an SSO domain into a verify descriptor + `jwks_uri` from the cached
/// discovery result, cross-checking the JWT's issuer against the discovered
/// issuer. Peek-only.
pub(super) fn resolve(
    domain: &str,
    jwt_iss: &str,
    jwt_aud: &AudClaim,
) -> Result<Cached<(Descriptor, String)>, OpenIDJWTVerificationError> {
    validate_discovery_domain(domain).map_err(OpenIDJWTVerificationError::GenericError)?;
    let discovery_config = match peek_discovery(domain) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(discovery_config) => discovery_config,
    };
    if !jwt_aud.matches(&discovery_config.client_id) {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "JWT audience '{jwt_aud}' does not match discovered client_id '{}' for domain '{domain}'",
            discovery_config.client_id
        )));
    }

    if discovery_config.issuer != jwt_iss {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "JWT issuer '{jwt_iss}' does not match discovered issuer '{}' for domain '{domain}'",
            discovery_config.issuer
        )));
    }
    let descriptor = Descriptor {
        issuer: discovery_config.issuer,
        client_id: discovery_config.client_id,
        sso: Some(SsoProvider {
            domain: domain.to_string(),
            name: discovery_config.name,
            stable_identifier_claim: non_default_stable_claim(
                &discovery_config.stable_identifier_claim,
            ),
        }),
    };
    Ok(Cached::Ready((descriptor, discovery_config.jwks_uri)))
}

/// Read the cached SSO JWKs for `jwks_uri`. Peek-only.
pub(super) fn read_jwks(jwks_uri: &str) -> Cached<Vec<Jwk>> {
    single_flight_cache::peek(&JWKS_CACHE, &jwks_uri.to_string())
}

// ---------------------------------------------------------------------------
// Discovery-domain validation + scheme selection.
// ---------------------------------------------------------------------------

/// Deploy flag: when set, discovery outcalls to loopback hosts may use plain
/// `http` (so e2e can point at local mock IdPs). See
/// `InternetIdentityInit::sso_allow_insecure_discovery`. Non-loopback hosts
/// always require `https` regardless.
fn sso_allow_insecure_discovery() -> bool {
    #[cfg(not(test))]
    {
        state::persistent_state(|ps| ps.sso_allow_insecure_discovery).unwrap_or(false)
    }
    #[cfg(test)]
    {
        tests::TEST_ALLOW_INSECURE.with_borrow(|b| *b)
    }
}

/// Validate a caller-supplied SSO discovery domain: it must be within the length
/// cap and a bare URL authority (a host, optionally `host:port`, and nothing
/// else). The bare-authority check is the security boundary — `domain` is
/// interpolated into a discovery URL (`{scheme}://{domain}/.well-known/...`), so
/// inputs carrying userinfo (`evil.com@127.0.0.1`), a path, a query, or a
/// fragment could otherwise change the effective request target.
pub fn validate_discovery_domain(domain: &str) -> Result<(), String> {
    if domain.len() > MAX_DOMAIN_LENGTH {
        return Err(format!(
            "SSO discovery domain exceeds {MAX_DOMAIN_LENGTH} bytes"
        ));
    }
    if !is_bare_authority(domain) {
        return Err(format!(
            "SSO discovery domain is not a bare authority: {domain}"
        ));
    }
    Ok(())
}

/// True if `domain` is a bare URL authority — a host, optionally `host:port`,
/// and nothing else: no scheme, userinfo, path, query, or fragment. It is
/// parsed the same way it is later used (as the authority of an `https`
/// discovery URL) and required to round-trip exactly, so anything the URL
/// parser would reinterpret — embedded userinfo/path/query/fragment, stripped
/// control characters, an injected scheme — is rejected. (A redundant
/// `:443`/`:80` default port is normalized away by the parser and thus rejected
/// too; real discovery domains don't carry one.)
fn is_bare_authority(domain: &str) -> bool {
    let Ok(url) = url::Url::parse(&format!("https://{domain}")) else {
        return false;
    };
    if !url.username().is_empty()
        || url.password().is_some()
        || url.query().is_some()
        || url.fragment().is_some()
        || !matches!(url.path(), "" | "/")
    {
        return false;
    }
    let Some(host) = url.host_str() else {
        return false;
    };
    let authority = match url.port() {
        Some(port) => format!("{host}:{port}"),
        None => host.to_string(),
    };
    authority == domain.to_ascii_lowercase()
}

/// True if `host` (host or `host:port`) is loopback.
fn is_loopback_host(host: &str) -> bool {
    let bare = host.split(':').next().unwrap_or(host).to_ascii_lowercase();
    matches!(bare.as_str(), "localhost" | "127.0.0.1")
}

/// `http` discovery is permitted only for a loopback host, and only when the
/// `sso_allow_insecure_discovery` deploy flag is set (e2e mock IdPs). Every
/// other host — and everything in production, where the flag is off — requires
/// `https`, so an un-flagged caller can never trigger an `http` outcall.
fn allow_insecure_scheme(host: &str) -> bool {
    sso_allow_insecure_discovery() && is_loopback_host(host)
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
    /// Per-app clients: `origin -> client_id`.
    #[serde(default)]
    app_clients: std::collections::HashMap<String, String>,
    /// Default-deny an origin not in `app_clients`.
    #[serde(default)]
    gate_all_apps: bool,
    /// Cross-client-stable identifier claim (default `sub`).
    #[serde(default)]
    stable_identifier_claim: Option<String>,
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

/// Reject over-length values from the untrusted well-known configuration.
#[cfg(not(test))]
fn validate_ii_config(config: &IIOpenIdConfiguration) -> Result<(), String> {
    if config.client_id.len() > MAX_CLIENT_ID_LENGTH {
        return Err(format!(
            "SSO client_id exceeds {MAX_CLIENT_ID_LENGTH} bytes"
        ));
    }
    if config
        .name
        .as_ref()
        .is_some_and(|name| name.len() > MAX_SSO_NAME_LENGTH)
    {
        return Err(format!("SSO name exceeds {MAX_SSO_NAME_LENGTH} bytes"));
    }
    Ok(())
}

/// Reject over-length values from the untrusted OIDC discovery document.
#[cfg(not(test))]
fn validate_discovery_document(doc: &DiscoveryDocument) -> Result<(), String> {
    if doc.issuer.len() > MAX_ISSUER_LENGTH {
        return Err(format!("SSO issuer exceeds {MAX_ISSUER_LENGTH} bytes"));
    }
    if doc.jwks_uri.len() > MAX_JWKS_URI_LENGTH {
        return Err(format!("SSO jwks_uri exceeds {MAX_JWKS_URI_LENGTH} bytes"));
    }
    Ok(())
}

/// The discovery cache fill: hop 1 (`ii-openid-configuration`) then hop 2 (the
/// standard OIDC discovery document), with host self-assertion checks between
/// them. Errors are surfaced as `Err` (the cache backs off and serves
/// stale-if-error).
#[cfg(not(test))]
async fn discovery_fill(domain: String) -> Result<DiscoveredConfig, String> {
    // Hop 1: fetch /.well-known/ii-openid-configuration. `https` by default; a
    // loopback host may use `http` under the `sso_allow_insecure_discovery` flag
    // (the e2e mock provider, which can't serve TLS).
    let hop1_scheme = scheme_for_host(&domain);
    let hop1_url = format!("{hop1_scheme}://{domain}/.well-known/ii-openid-configuration");
    let ii_config = fetch_ii_openid_configuration(hop1_url).await?;
    validate_discovery_url(&ii_config.openid_configuration)?;

    let app_clients = validate_app_clients(&ii_config.app_clients)?;
    let stable_identifier_claim = ii_config
        .stable_identifier_claim
        .clone()
        .filter(|c| !c.is_empty())
        .unwrap_or_else(|| DEFAULT_STABLE_IDENTIFIER_CLAIM.to_string());
    let gate_all_apps = ii_config.gate_all_apps;

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

    validate_ii_config(&ii_config)?;
    validate_discovery_document(&doc)?;

    let mut scopes = doc
        .scopes_supported
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| DEFAULT_SCOPES.iter().map(|s| (*s).to_string()).collect());
    // Bound the only unbounded field stored per discovery entry.
    scopes.truncate(DISCOVERY_MAX_SCOPES);

    let DiscoveryDocument {
        issuer,
        jwks_uri,
        authorization_endpoint,
        ..
    } = doc;
    let IIOpenIdConfiguration {
        client_id, name, ..
    } = ii_config;

    Ok(DiscoveredConfig {
        issuer,
        client_id,
        jwks_uri,
        authorization_endpoint,
        scopes,
        name,
        app_clients,
        gate_all_apps,
        stable_identifier_claim,
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
        max_response_bytes: Some(DISCOVERY_MAX_RESPONSE_BYTES),
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
            if allow_insecure_scheme(&host) {
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

/// Scheme for the hop-1 URL: `http` only for a loopback host under the
/// `sso_allow_insecure_discovery` flag (the e2e mock provider, which can't serve
/// TLS); every other host — and everything in production — gets `https`.
fn scheme_for_host(host: &str) -> &'static str {
    if allow_insecure_scheme(host) {
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

/// Test-only: reset the SSO caches and warm `domain`'s discovery + JWKS from an
/// injected `DiscoveredConfig`.
#[cfg(test)]
pub(super) fn test_setup_discovery(
    domain: &str,
    discovery_config: DiscoveredConfig,
    jwks: Vec<Jwk>,
) {
    use crate::single_flight_cache::{run_detached, set_test_now};
    set_test_now(1_700_000_000);
    DISCOVERY_CACHE.with(|c| *c.borrow_mut() = new_discovery_cache());
    JWKS_CACHE.with(|c| *c.borrow_mut() = new_jwks_cache());
    tests::TEST_DISCOVERY.with_borrow_mut(|m| {
        m.clear();
        m.insert(domain.to_string(), discovery_config.clone());
    });
    tests::TEST_JWKS.with_borrow_mut(|m| {
        m.clear();
        m.insert(discovery_config.jwks_uri.clone(), jwks);
    });
    // First pass warms discovery; the second warms JWKS now that discovery is Ready.
    prefetch(domain);
    run_detached();
    prefetch(domain);
    run_detached();
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
        pub(super) static TEST_ALLOW_INSECURE: RefCell<bool> = const { RefCell::new(false) };
        pub(super) static TEST_DISCOVERY: RefCell<HashMap<String, DiscoveredConfig>> = RefCell::new(HashMap::new());
        pub(super) static TEST_JWKS: RefCell<HashMap<String, Vec<Jwk>>> = RefCell::new(HashMap::new());
    }

    fn reset() {
        set_test_now(1_700_000_000);
        TEST_ALLOW_INSECURE.with_borrow_mut(|b| *b = false);
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
            app_clients: vec![],
            gate_all_apps: false,
            stable_identifier_claim: DEFAULT_STABLE_IDENTIFIER_CLAIM.to_string(),
        }
    }

    fn test_aud_claim() -> AudClaim {
        AudClaim::Single(sample_config().client_id)
    }

    fn allowed(domain: &str) -> bool {
        validate_discovery_domain(domain).is_ok()
    }

    #[test]
    fn resolve_rejects_non_authority_domain() {
        reset();
        // No allowlist: any bare authority is accepted; a non-authority is not.
        assert!(allowed("not-allowed.com"));
        assert!(!allowed("evil.com@127.0.0.1"));
        // The verify path rejects a non-authority domain.
        assert!(resolve(
            "evil.com@127.0.0.1",
            "https://idp.example.org",
            &test_aud_claim()
        )
        .is_err());
    }

    #[test]
    fn any_bare_authority_is_allowed() {
        reset();
        // No allowlist: any bare-authority domain passes the discovery gate.
        assert!(allowed("not-allowed.com"));
        assert!(allowed("example.org"));
        assert!(allowed("sub.example.com:8443"));
    }

    #[test]
    fn http_scheme_requires_insecure_flag_and_loopback() {
        reset();
        // Default (flag off): https for every host, loopback included.
        assert_eq!(scheme_for_host("localhost:11107"), "https");
        assert_eq!(scheme_for_host("127.0.0.1:8080"), "https");
        assert_eq!(scheme_for_host("example.com"), "https");

        // Flag on: loopback hosts may use plain http (the e2e mock provider,
        // which can't serve TLS)...
        TEST_ALLOW_INSECURE.with_borrow_mut(|b| *b = true);
        assert_eq!(scheme_for_host("localhost:11107"), "http");
        assert_eq!(scheme_for_host("127.0.0.1:8080"), "http");
        // ...but non-loopback hosts always require https.
        assert_eq!(scheme_for_host("evil.example.com"), "https");
    }

    #[test]
    fn non_authority_domain_is_rejected() {
        reset();

        // Bare authorities pass: host, sub-host, and explicit (non-default)
        // port, case-insensitively.
        assert!(allowed("example.com"));
        assert!(allowed("sub.example.com"));
        assert!(allowed("example.com:8443"));
        assert!(allowed("Example.COM"));

        // Anything that isn't a bare host[:port] is rejected even with the flag
        // on, so the caller-controlled value can't reshape the interpolated
        // discovery URL (`{scheme}://{domain}/.well-known/...`).
        for bad in [
            "evil.com@127.0.0.1",    // userinfo — real host is 127.0.0.1
            "user:pass@example.com", // userinfo
            "example.com/..",        // path traversal
            "example.com/foo",       // path
            "example.com?x=1",       // query
            "example.com#frag",      // fragment
            "https://example.com",   // injected scheme
            "example.com:443",       // redundant default port (normalized away)
            "exa mple.com",          // whitespace in host
            "",                      // empty
        ] {
            assert!(!allowed(bad), "expected `{bad}` to be rejected");
        }
    }

    #[test]
    fn over_long_domain_is_rejected() {
        reset();
        let at_cap = format!("{}.com", "a".repeat(MAX_DOMAIN_LENGTH - 4));
        assert_eq!(at_cap.len(), MAX_DOMAIN_LENGTH);
        assert!(allowed(&at_cap));
        let over_cap = format!("{}.com", "a".repeat(MAX_DOMAIN_LENGTH - 3));
        assert_eq!(over_cap.len(), MAX_DOMAIN_LENGTH + 1);
        assert!(!allowed(&over_cap));
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
            Cached::Ready(discovery_config) => {
                assert_eq!(discovery_config.issuer, "https://idp.example.org")
            }
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
            resolve("example.org", "https://idp.example.org", &test_aud_claim()),
            Ok(Cached::Pending),
        ));
        run_detached();
        assert_eq!(peek_discovery("example.org"), Cached::Pending);
    }

    #[test]
    fn app_client_cleartext_resolution() {
        let discovery_config = DiscoveredConfig {
            app_clients: vec![
                AppClient {
                    key: AppClientKey::Cleartext("https://payroll.com".to_string()),
                    client_id: "0oaPAYROLL".to_string(),
                },
                AppClient {
                    key: AppClientKey::Cleartext("https://admin.internal.app".to_string()),
                    client_id: "0oaADMIN".to_string(),
                },
            ],
            ..sample_config()
        };
        assert_eq!(
            discovery_config.resolve_client_for_origin("https://payroll.com"),
            Ok("0oaPAYROLL".to_string())
        );
        assert_eq!(
            discovery_config.resolve_client_for_origin("https://public.app"),
            Ok("client-123".to_string())
        );
    }

    #[test]
    fn app_client_hashed_key_resolution() {
        let origin = "https://oc.app";
        let salt = "9f3a7c2e";
        let mut hasher = Sha256::new();
        hasher.update(origin.as_bytes());
        hasher.update(salt.as_bytes());
        let hash = hex::encode(hasher.finalize());
        let discovery_config = DiscoveredConfig {
            app_clients: vec![AppClient {
                key: AppClientKey::Hashed {
                    hash: hash.clone(),
                    salt: salt.to_string(),
                },
                client_id: "0oaCHAT".to_string(),
            }],
            ..sample_config()
        };
        assert_eq!(
            discovery_config.resolve_client_for_origin(origin),
            Ok("0oaCHAT".to_string())
        );
        assert_eq!(
            discovery_config.resolve_client_for_origin("https://evil.app"),
            Ok("client-123".to_string())
        );
    }

    #[test]
    fn gate_all_apps_default_deny() {
        let discovery_config = DiscoveredConfig {
            app_clients: vec![AppClient {
                key: AppClientKey::Cleartext("https://payroll.com".to_string()),
                client_id: "0oaPAYROLL".to_string(),
            }],
            gate_all_apps: true,
            ..sample_config()
        };
        assert_eq!(
            discovery_config.resolve_client_for_origin("https://payroll.com"),
            Ok("0oaPAYROLL".to_string())
        );
        assert_eq!(
            discovery_config.resolve_client_for_origin("https://public.app"),
            Err(Forbidden)
        );
    }

    #[test]
    fn app_clients_over_cap_rejected() {
        let mut entries = std::collections::HashMap::new();
        for i in 0..=MAX_APP_CLIENTS {
            entries.insert(format!("https://app{i}.com"), format!("client{i}"));
        }
        assert!(entries.len() > MAX_APP_CLIENTS);
        assert!(validate_app_clients(&entries).is_err());

        entries.clear();
        for i in 0..MAX_APP_CLIENTS {
            entries.insert(format!("https://app{i}.com"), format!("client{i}"));
        }
        assert_eq!(entries.len(), MAX_APP_CLIENTS);
        assert!(validate_app_clients(&entries).is_ok());
    }

    #[test]
    fn app_clients_over_long_client_id_rejected() {
        let mut entries = std::collections::HashMap::new();
        entries.insert(
            "https://app.com".to_string(),
            "a".repeat(MAX_APP_CLIENT_ID_LENGTH + 1),
        );
        assert!(validate_app_clients(&entries).is_err());

        entries.clear();
        entries.insert(
            "https://app.com".to_string(),
            "a".repeat(MAX_APP_CLIENT_ID_LENGTH),
        );
        assert!(validate_app_clients(&entries).is_ok());
    }

    #[test]
    fn app_client_key_parse_distinguishes_cleartext_from_hashed() {
        assert_eq!(
            AppClientKey::parse("https://payroll.com"),
            AppClientKey::Cleartext("https://payroll.com".to_string())
        );
        assert_eq!(
            AppClientKey::parse("b5d4045c:9f3a7c2e"),
            AppClientKey::Hashed {
                hash: "b5d4045c".to_string(),
                salt: "9f3a7c2e".to_string()
            }
        );
    }

    #[test]
    fn resolve_cross_checks_issuer() {
        reset();
        TEST_DISCOVERY.with_borrow_mut(|m| {
            m.insert("example.org".to_string(), sample_config());
        });
        prefetch("example.org");
        run_detached();
        // Wrong issuer is rejected (the audience matches, so it's the issuer
        // cross-check that fails).
        assert!(resolve("example.org", "https://evil.example.org", &test_aud_claim()).is_err());
        // Matching issuer resolves to a descriptor + jwks_uri.
        match resolve("example.org", "https://idp.example.org", &test_aud_claim()) {
            Ok(Cached::Ready((descriptor, jwks_uri))) => {
                assert_eq!(jwks_uri, "https://idp.example.org/jwks");
                assert!(descriptor.sso.is_some());
            }
            _ => panic!("expected Ready"),
        }
    }
}
