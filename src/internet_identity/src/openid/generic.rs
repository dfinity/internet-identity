use super::{
    get_all_claims, get_issuer_placeholders, replace_issuer_placeholders, AudClaim,
    OpenIDJWTVerificationError,
};
use crate::openid::OpenIdCredential;
use crate::openid::OpenIdProvider;
use crate::secs_to_nanos;
use crate::state;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use candid::{Deserialize, Nat};
use ic_cdk::api::management_canister::http_request::{HttpHeader, HttpResponse};
use ic_cdk::trap;
use ic_stable_structures::Storable;
use identity_jose::jwk::{Jwk, JwkParamsRsa};
use identity_jose::jws::JwsAlgorithm::RS256;
use identity_jose::jws::{
    Decoder, JwsVerifierFn, SignatureVerificationError, SignatureVerificationErrorKind,
    VerificationInput,
};
use internet_identity_interface::internet_identity::types::{
    DiscoverableOidcConfig, MetadataEntryV2, OpenIdConfig, OpenIdEmailVerificationScheme,
};
use rsa::{Pkcs1v15Sign, RsaPublicKey};
use serde::Serialize;
use sha2::{Digest, Sha256};
#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Into;
use std::rc::Rc;

// The amount of cycles needed to make the HTTP outcall with a large enough margin
#[cfg(not(test))]
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

#[cfg(not(test))]
const DISCOVERY_CALL_CYCLES: u128 = 30_000_000_000;

const HTTP_STATUS_OK: u8 = 200;

// Fetch the certs every fifteen minutes, the responses are always
// valid for a couple of hours so that should be enough margin.
const FETCH_CERTS_INTERVAL_SECONDS: u64 = 60 * 15; // 15 minutes in seconds

/// Re-fetch OIDC discovery documents every hour. Discovery metadata (issuer, jwks_uri)
/// changes infrequently (typically only on provider-side reconfigurations), but hourly
/// refresh keeps II responsive to such changes within a bounded window.
#[cfg(not(test))]
const FETCH_DISCOVERY_INTERVAL_SECONDS: u64 = 60 * 60; // 1 hour

// A JWT is only valid for a very small window, even if the JWT itself says it's valid for longer,
// we only need it right after it's being issued to create a JWT delegation with its own expiry.
// As the JWT is also used for registration, which may include longer user interaction,
// we are using 10 minutes to account for potential clock offsets as well as users.
const MAX_VALIDITY_WINDOW_SECONDS: u64 = 10 * 60; // Same as ingress expiry

// Maximum length of the email claim in the JWT, in practice we expect the identity provider to
// already validate it on their end for a sane maximum length. This is an additional sanity check.
const MAX_EMAIL_LENGTH: usize = 256;

// Maximum length of the name claim in the JWT, in practice we expect the identity provider to
// already validate it on their end for a sane maximum length. This is an additional sanity check.
const MAX_NAME_LENGTH: usize = 128;

#[derive(Serialize, Deserialize)]
struct Certs {
    keys: Vec<Jwk>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum EmailVerifiedClaim {
    Bool(bool),
    String(String),
}

// Depending on the OpenID provider, this claim can either be a boolean or string,
// so we need to support both formats when parsing the claims, but we will convert
// both to a string for storing it in the OpenIdCredential metadata.
impl EmailVerifiedClaim {
    fn into_string(self) -> String {
        match self {
            EmailVerifiedClaim::Bool(value) => {
                if value {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            EmailVerifiedClaim::String(value) => value,
        }
    }
}

#[derive(Deserialize)]
struct Claims {
    iss: String,
    sub: String,
    aud: AudClaim,
    nonce: String,
    exp: u64,
    iat: u64,
    // Optional metadata claims
    email: Option<String>,
    name: Option<String>,
    email_verified: Option<EmailVerifiedClaim>,
}

pub struct Provider {
    client_id: String,
    issuer: String,
    certs: Rc<RefCell<Vec<Jwk>>>,
    email_verification: Option<OpenIdEmailVerificationScheme>,
}

impl OpenIdProvider for Provider {
    fn issuer(&self) -> Option<String> {
        Some(self.issuer.clone())
    }

    fn client_id(&self) -> Option<String> {
        Some(self.client_id.clone())
    }

    fn email_verification_scheme(&self) -> Option<OpenIdEmailVerificationScheme> {
        self.email_verification
    }

    fn verify(
        &self,
        jwt: &str,
        salt: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        // Decode JWT and decode claims
        let validation_item = Decoder::new()
            .decode_compact_serialization(jwt.as_bytes(), None)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Unable to decode JWT".to_string())
            })?;
        let claims: Claims = serde_json::from_slice(validation_item.claims()).map_err(|_| {
            OpenIDJWTVerificationError::GenericError(
                "Unable to decode claims or expected claims are missing".to_string(),
            )
        })?;

        // Calculate effective issuer and use it to verify the JWT claims
        let issuer_placeholders = get_issuer_placeholders(&self.issuer);
        let issuer_claims = get_all_claims(validation_item.claims(), issuer_placeholders);
        let effective_issuer = replace_issuer_placeholders(&self.issuer, &issuer_claims);
        verify_claims(&effective_issuer, &self.client_id, &claims, salt)?;

        // Verify JWT signature
        let kid = validation_item
            .kid()
            .ok_or(OpenIDJWTVerificationError::GenericError(
                "JWT is missing kid".to_string(),
            ))?;
        let certs = self.certs.borrow();
        let cert = certs
            .iter()
            .find(|cert| cert.kid().is_some_and(|v| v == kid))
            .ok_or(OpenIDJWTVerificationError::GenericError(format!(
                "Certificate not found for {kid}"
            )))?;
        validation_item
            .verify(&JwsVerifierFn::from(verify_signature), cert)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Invalid signature".to_string())
            })?;

        // Return credential with metadata
        let mut metadata: HashMap<String, MetadataEntryV2> = HashMap::new();
        if let Some(email) = claims.email {
            metadata.insert("email".into(), MetadataEntryV2::String(email));
        }
        if let Some(email_verified) = claims.email_verified {
            metadata.insert(
                "email_verified".into(),
                MetadataEntryV2::String(email_verified.into_string()),
            );
        }
        if let Some(name) = claims.name {
            metadata.insert("name".into(), MetadataEntryV2::String(name));
        }
        // Store issuer specific claims in the metadata e.g. `tid`
        for (key, value) in issuer_claims {
            metadata.insert(key, MetadataEntryV2::String(value));
        }
        Ok(OpenIdCredential {
            // Do NOT use self.issuer() here since every iss and sub pair should uniquely identify
            // an account, the config issuer in the case of Microsoft is not the actual issuer
            // but a string with placeholders that doesn't identify in which issuer a sub belongs.
            //
            // Example issuer config:
            // https://login.microsoftonline.com/{tenantid}/v2.0
            //
            // Example iss claims:
            // https://login.microsoftonline.com/164d0422-a01d-41d5-945a-37456ea80dbb/v2.0
            // https://login.microsoftonline.com/599249e6-791a-48a7-84d0-b3e858773ac2/v2.0
            iss: claims.iss,
            sub: claims.sub,
            // `aud` is verified against `client_id` above; store the canonical value.
            aud: self.client_id.clone(),
            last_usage_timestamp: None,
            metadata,
        })
    }
}

impl Provider {
    pub fn create(config: OpenIdConfig) -> Provider {
        #[cfg(test)]
        let certs = Rc::new(RefCell::new(TEST_CERTS.take()));

        #[cfg(not(test))]
        let certs: Rc<RefCell<Vec<Jwk>>> = Rc::new(RefCell::new(vec![]));

        #[cfg(not(test))]
        schedule_fetch_certs(config.jwks_uri, Rc::clone(&certs), Some(0));

        Provider {
            client_id: config.client_id,
            issuer: config.issuer,
            certs,
            email_verification: config.email_verification,
        }
    }
}

/// CANARY: SSO via two-hop discovery is a proof-of-concept. Exactly one
/// domain is currently accepted as a `discovery_domain`, and which one
/// depends on whether this deployment self-identifies as production via
/// the `is_production` init arg:
///
/// * `is_production == Some(true)` (i.e. `id.ai`) → `dfinity.org`
/// * otherwise (beta, staging, local dev, tests) → `beta.dfinity.org`
///
/// Keeping the prod/beta allowlists disjoint means a DNS takeover of the
/// beta test domain can't backdoor the production canister, and lets us
/// stage registration changes (e.g. a new IdP) on `beta.dfinity.org`
/// without risking the production issuer. Widen this (or move it to
/// canister config) once the feature exits canary.
pub fn allowed_discovery_domains() -> &'static [&'static str] {
    let is_production = state::persistent_state(|ps| ps.is_production);
    match is_production {
        Some(true) => &["dfinity.org"],
        _ => &["beta.dfinity.org"],
    }
}

pub fn is_allowed_discovery_domain(domain: &str) -> bool {
    allowed_discovery_domains()
        .iter()
        .any(|allowed| allowed.eq_ignore_ascii_case(domain))
}

/// SSO provider that resolves its configuration via two-hop discovery.
/// Used with the `DiscoverableOidcConfig` type.
///
/// The `discovery_domain` and human-readable `name` aren't held on the
/// provider itself — they live on the matching `DiscoveryState` in
/// `DISCOVERY_TASKS` (written by the periodic discovery timer, read by
/// `sso_fields_for` when shaping API responses).
pub struct DiscoverableProvider {
    discovered_client_id: Rc<RefCell<Option<String>>>,
    discovered_issuer: Rc<RefCell<Option<String>>>,
    certs: Rc<RefCell<Vec<Jwk>>>,
}

impl OpenIdProvider for DiscoverableProvider {
    fn issuer(&self) -> Option<String> {
        self.discovered_issuer.borrow().clone()
    }

    fn client_id(&self) -> Option<String> {
        self.discovered_client_id.borrow().clone()
    }

    fn email_verification_scheme(&self) -> Option<OpenIdEmailVerificationScheme> {
        // SSO providers don't opt into II's hardcoded provider-specific email
        // verification heuristics (Google `email_verified`, Microsoft `tid`).
        // Verification is a trust decision made when the SSO domain is configured.
        None
    }

    fn verify(
        &self,
        jwt: &str,
        salt: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        let issuer = self.discovered_issuer.borrow().clone().ok_or_else(|| {
            OpenIDJWTVerificationError::GenericError(
                "Provider not yet initialized (discovery pending)".to_string(),
            )
        })?;
        let client_id = self.discovered_client_id.borrow().clone().ok_or_else(|| {
            OpenIDJWTVerificationError::GenericError(
                "Provider client_id not yet available (discovery pending)".to_string(),
            )
        })?;

        // Decode JWT and decode claims
        let validation_item = Decoder::new()
            .decode_compact_serialization(jwt.as_bytes(), None)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Unable to decode JWT".to_string())
            })?;
        let claims: Claims = serde_json::from_slice(validation_item.claims()).map_err(|_| {
            OpenIDJWTVerificationError::GenericError(
                "Unable to decode claims or expected claims are missing".to_string(),
            )
        })?;

        // Verify claims against discovered issuer
        verify_claims(&issuer, &client_id, &claims, salt)?;

        // Verify JWT signature
        let kid = validation_item
            .kid()
            .ok_or(OpenIDJWTVerificationError::GenericError(
                "JWT is missing kid".to_string(),
            ))?;
        let certs = self.certs.borrow();
        if certs.is_empty() {
            // Distinguish a still-pending JWKS fetch from a genuine kid mismatch so
            // the frontend can offer a retry instead of surfacing a fatal error.
            return Err(OpenIDJWTVerificationError::GenericError(
                "OIDC provider JWKS not yet fetched, please try again shortly".to_string(),
            ));
        }
        let cert = certs
            .iter()
            .find(|cert| cert.kid().is_some_and(|v| v == kid))
            .ok_or(OpenIDJWTVerificationError::GenericError(format!(
                "Certificate not found for {kid}"
            )))?;
        validation_item
            .verify(&JwsVerifierFn::from(verify_signature), cert)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Invalid signature".to_string())
            })?;

        // Return credential with metadata. SSO-specific labels
        // (`sso_domain`, `sso_name`) are NOT stored here; they're looked
        // up on-demand from current `DISCOVERY_TASKS` state when a
        // credential is returned via the API (see
        // `openid::generic::sso_fields_for`). Computing them at query
        // time means the FE always sees the current SSO `name` if the
        // domain's `/.well-known/ii-openid-configuration` is updated.
        let mut metadata: HashMap<String, MetadataEntryV2> = HashMap::new();
        if let Some(email) = claims.email {
            metadata.insert("email".into(), MetadataEntryV2::String(email));
        }
        if let Some(email_verified) = claims.email_verified {
            metadata.insert(
                "email_verified".into(),
                MetadataEntryV2::String(email_verified.into_string()),
            );
        }
        if let Some(name) = claims.name {
            metadata.insert("name".into(), MetadataEntryV2::String(name));
        }
        Ok(OpenIdCredential {
            iss: claims.iss,
            sub: claims.sub,
            // `aud` is verified against `client_id` above; store the canonical value.
            aud: client_id,
            last_usage_timestamp: None,
            metadata,
        })
    }
}

/// Shared state for a single discovery task, exchanged between the periodic
/// timer and the owning `DiscoverableProvider`.
#[derive(Clone)]
pub struct DiscoveryState {
    pub discovery_domain: String,
    pub client_id_ref: Rc<RefCell<Option<String>>>,
    pub openid_configuration_ref: Rc<RefCell<Option<String>>>,
    pub issuer_ref: Rc<RefCell<Option<String>>>,
    /// Human-readable SSO name served at
    /// `{discovery_domain}/.well-known/ii-openid-configuration`.
    /// Populated by hop-1 once per refresh; `None` if absent in the
    /// hop-1 body. Read by `sso_fields_for` when shaping credential
    /// responses.
    pub name_ref: Rc<RefCell<Option<String>>>,
    /// Only read by the periodic discovery timer (non-test builds).
    #[allow(dead_code)]
    pub certs_ref: Rc<RefCell<Vec<Jwk>>>,
    /// Tracks the last seen `jwks_uri` so cert fetching restarts when it changes.
    /// Only read by the periodic discovery timer (non-test builds).
    #[allow(dead_code)]
    pub last_jwks_uri: Rc<RefCell<Option<String>>>,
}

// TODO: `DISCOVERY_TASKS` is unbounded — a long `allowed_discovery_domains()` list
// or many admin-configured SSO providers would fan out into many periodic HTTP
// outcalls. Revisit once the canary allowlist is lifted. See reviewer comment on
// https://github.com/dfinity/internet-identity/pull/3778#discussion_r3099150266.
thread_local! {
    static DISCOVERY_TASKS: RefCell<Vec<DiscoveryState>> = const { RefCell::new(vec![]) };
}

impl DiscoverableProvider {
    pub fn create(config: DiscoverableOidcConfig) -> DiscoverableProvider {
        let certs: Rc<RefCell<Vec<Jwk>>> = Rc::new(RefCell::new(vec![]));
        let discovered_issuer: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
        let discovered_client_id: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
        let openid_configuration: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
        let discovered_name: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));

        DISCOVERY_TASKS.with_borrow_mut(|tasks| {
            tasks.push(DiscoveryState {
                discovery_domain: config.discovery_domain,
                client_id_ref: Rc::clone(&discovered_client_id),
                openid_configuration_ref: Rc::clone(&openid_configuration),
                issuer_ref: Rc::clone(&discovered_issuer),
                name_ref: discovered_name,
                certs_ref: Rc::clone(&certs),
                last_jwks_uri: Rc::new(RefCell::new(None)),
            });
        });

        DiscoverableProvider {
            discovered_client_id,
            discovered_issuer,
            certs,
        }
    }
}

/// Initializes timers for OIDC discovery. Called once from `initialize()`.
/// Uses `set_timer_interval` for periodic refresh and an immediate first fetch.
#[cfg(not(test))]
pub fn init_discovery_timers() {
    use ic_cdk::spawn;
    use ic_cdk_timers::{set_timer, set_timer_interval};
    use std::time::Duration;

    // Immediate first fetch
    set_timer(Duration::ZERO, || spawn(run_discovery_tasks()));

    // Periodic refresh
    set_timer_interval(
        Duration::from_secs(FETCH_DISCOVERY_INTERVAL_SECONDS),
        || spawn(run_discovery_tasks()),
    );
}

#[cfg(not(test))]
async fn run_discovery_tasks() {
    let tasks: Vec<DiscoveryState> = DISCOVERY_TASKS.with_borrow(|tasks| tasks.clone());

    for task in tasks {
        // Hop 1: fetch https://{domain}/.well-known/ii-openid-configuration,
        // which is expected to return { client_id, openid_configuration }.
        let hop1_url = format!(
            "https://{}/.well-known/ii-openid-configuration",
            task.discovery_domain
        );
        let ii_config = match fetch_ii_openid_configuration(hop1_url.clone()).await {
            Ok(c) => c,
            Err(err) => {
                ic_cdk::println!("II OpenID configuration fetch failed for {hop1_url}: {err}");
                continue;
            }
        };
        if !ii_config.openid_configuration.starts_with("https://") {
            ic_cdk::println!(
                "II OpenID configuration from {hop1_url} has non-https openid_configuration URL"
            );
            continue;
        }

        // Hop 2: fetch the standard OIDC discovery document at openid_configuration.
        let doc = match fetch_discovery(ii_config.openid_configuration.clone()).await {
            Ok(d) => d,
            Err(err) => {
                ic_cdk::println!(
                    "OIDC discovery fetch failed for {}: {err}",
                    ii_config.openid_configuration
                );
                continue;
            }
        };

        // Verify that the discovered issuer's host matches the openid_configuration URL host.
        // This is the standard OIDC-discovery self-assertion check and defends against a
        // compromised hop-1 response that declares an unrelated issuer for its jwks_uri.
        if let Err(err) = validate_issuer_host(&ii_config.openid_configuration, &doc.issuer) {
            ic_cdk::println!("Discovery issuer validation failed: {err}");
            continue;
        }

        // Reject malformed / non-HTTPS jwks_uri before scheduling repeated outcalls.
        if let Err(err) = validate_https_url(&doc.jwks_uri) {
            ic_cdk::println!(
                "Invalid jwks_uri from {}: {err}",
                ii_config.openid_configuration
            );
            continue;
        }

        task.client_id_ref.replace(Some(ii_config.client_id));
        task.openid_configuration_ref
            .replace(Some(ii_config.openid_configuration));
        task.issuer_ref.replace(Some(doc.issuer));
        // Pass `name` through unchanged — `None` means the domain didn't
        // publish one in `/.well-known/ii-openid-configuration`. The FE
        // decides how to render: "DFINITY" if `sso_name` is present,
        // "dfinity.org" if only `sso_domain` is available.
        task.name_ref.replace(ii_config.name);

        // Start or restart cert fetching when jwks_uri changes
        let jwks_changed = {
            let last = task.last_jwks_uri.borrow();
            last.as_deref() != Some(&doc.jwks_uri)
        };
        if jwks_changed {
            task.last_jwks_uri.replace(Some(doc.jwks_uri.clone()));
            schedule_fetch_certs(doc.jwks_uri, Rc::clone(&task.certs_ref), Some(0));
        }
    }
}

/// Ensure a URL is syntactically valid and uses `https://`.
#[cfg(not(test))]
fn validate_https_url(url: &str) -> Result<(), String> {
    let parsed = url::Url::parse(url).map_err(|e| format!("parse error: {e}"))?;
    if parsed.scheme() != "https" {
        return Err(format!("expected https scheme, got '{}'", parsed.scheme()));
    }
    Ok(())
}

/// Validates that the discovered `issuer` belongs to the same host as the
/// OpenID configuration URL (hop 2). The `discovery_domain` (hop 1) is
/// intentionally allowed to differ — it hosts a custom SSO indirection that
/// legitimately points at an external OIDC provider.
#[cfg(not(test))]
fn validate_issuer_host(openid_config_url: &str, issuer: &str) -> Result<(), String> {
    let openid_config_host = url::Url::parse(openid_config_url)
        .map_err(|e| format!("Invalid openid_configuration URL: {e}"))?
        .host_str()
        .ok_or_else(|| format!("No host in openid_configuration URL: {openid_config_url}"))?
        .to_lowercase();
    let issuer_host = url::Url::parse(issuer)
        .map_err(|e| format!("Invalid issuer URL: {e}"))?
        .host_str()
        .ok_or_else(|| format!("No host in issuer URL: {issuer}"))?
        .to_lowercase();

    if openid_config_host != issuer_host {
        return Err(format!(
            "Issuer host '{issuer_host}' does not match openid_configuration host '{openid_config_host}'"
        ));
    }
    Ok(())
}

#[cfg(not(test))]
async fn fetch_ii_openid_configuration(url: String) -> Result<IIOpenIdConfiguration, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpMethod,
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

    serde_json::from_slice::<IIOpenIdConfiguration>(response.body.as_slice())
        .map_err(|_| "Invalid ii-openid-configuration JSON".into())
}

#[cfg(not(test))]
async fn fetch_discovery(discovery_url: String) -> Result<DiscoveryDocument, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpMethod,
    };

    let request = CanisterHttpRequestArgument {
        url: discovery_url,
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

    serde_json::from_slice::<DiscoveryDocument>(response.body.as_slice())
        .map_err(|_| "Invalid discovery JSON".into())
}

/// Transform function for JSON discovery responses (both hops).
/// Re-serializes deterministically for consensus across subnet nodes.
#[cfg(not(test))]
#[allow(clippy::needless_pass_by_value)]
fn transform_discovery(response: HttpResponse) -> HttpResponse {
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

/// II-specific SSO indirection document served at `{domain}/.well-known/ii-openid-configuration`.
#[cfg(not(test))]
#[derive(serde::Deserialize, Clone)]
struct IIOpenIdConfiguration {
    client_id: String,
    openid_configuration: String,
    /// Human-readable name for the SSO (e.g. `"DFINITY"`). Optional; we
    /// pass it through to the API as-is, letting the FE pick between
    /// this and the bare domain for rendering. `#[serde(default)]` so
    /// older deployments that don't publish the field continue to parse.
    #[serde(default)]
    name: Option<String>,
}

/// OIDC discovery document — only the fields needed by the backend.
#[cfg(not(test))]
#[derive(serde::Deserialize, Clone)]
struct DiscoveryDocument {
    issuer: String,
    jwks_uri: String,
}

/// Returns the discovered state for a given `discovery_domain`.
/// Returns `(client_id, openid_configuration, issuer)` — any entry may be `None`
/// if discovery for that domain has not yet completed.
pub fn discovered_state_for(
    discovery_domain: &str,
) -> (Option<String>, Option<String>, Option<String>) {
    DISCOVERY_TASKS.with_borrow(|tasks| {
        tasks
            .iter()
            .find(|t| t.discovery_domain == discovery_domain)
            .map(|t| {
                (
                    t.client_id_ref.borrow().clone(),
                    t.openid_configuration_ref.borrow().clone(),
                    t.issuer_ref.borrow().clone(),
                )
            })
            .unwrap_or((None, None, None))
    })
}

/// Looks up the SSO domain + optional SSO name for an OpenID credential
/// by its `(iss, aud)` pair. Computed on-demand from current
/// `DISCOVERY_TASKS` state — that way `get_anchor_info` always reflects
/// live SSO metadata (the domain's current `name` field, or absence
/// thereof) instead of whatever got stamped at verification time.
///
/// Returns `(None, None)` for:
///   - Direct-provider credentials (Google / Apple / Microsoft — the
///     matching provider isn't a `DiscoverableProvider`, so no task
///     matches on `(iss, aud)`).
///   - SSO credentials whose domain has since been removed from the
///     canister's allowlist (no task registered at all).
///   - SSO credentials whose hop-1 / hop-2 discovery hasn't completed
///     yet (task exists but `client_id_ref` / `issuer_ref` are `None`).
///
/// The caller renders a human-readable label by falling back
/// `sso_name → sso_domain → direct-provider `findConfig` result` on the
/// frontend — the backend intentionally does not collapse the two: we
/// want the FE to be able to tell "no name published" apart from "has a
/// name" for future divergent rendering.
pub fn sso_fields_for(iss: &str, aud: &str) -> (Option<String>, Option<String>) {
    DISCOVERY_TASKS.with_borrow(|tasks| {
        tasks
            .iter()
            .find(|t| {
                t.issuer_ref.borrow().as_deref() == Some(iss)
                    && t.client_id_ref.borrow().as_deref() == Some(aud)
            })
            .map(|t| {
                (
                    Some(t.discovery_domain.clone()),
                    t.name_ref.borrow().clone(),
                )
            })
            .unwrap_or((None, None))
    })
}

fn compute_next_certs_fetch_delay<T, E>(
    result: &Result<T, E>,
    current_delay: Option<u64>,
) -> Option<u64> {
    const MIN_DELAY_SECONDS: u64 = 60;
    const MAX_DELAY_SECONDS: u64 = FETCH_CERTS_INTERVAL_SECONDS;
    const BACKOFF_MULTIPLIER: u64 = 2;

    match result {
        // Reset delay to None so default (`FETCH_CERTS_INTERVAL`) delay is used.
        Ok(_) => None,
        // Try again earlier with backoff if fetch failed, the HTTP outcall responses
        // aren't the same across nodes when we fetch at the moment of key rotation.
        //
        // The delay should be at most `MAX_DELAY` and at minimum `MIN_DELAY`.
        Err(_) => Some(
            current_delay
                .map_or(MIN_DELAY_SECONDS, |d| d * BACKOFF_MULTIPLIER)
                .clamp(MIN_DELAY_SECONDS, MAX_DELAY_SECONDS),
        ),
    }
}

#[cfg(not(test))]
fn schedule_fetch_certs(
    jwks_uri: String,
    certs_reference: Rc<RefCell<Vec<Jwk>>>,
    delay: Option<u64>,
) {
    use ic_cdk::spawn;
    use ic_cdk_timers::set_timer;
    use std::time::Duration;

    set_timer(
        Duration::from_secs(delay.unwrap_or(FETCH_CERTS_INTERVAL_SECONDS)),
        move || {
            spawn(async move {
                let result = fetch_certs(jwks_uri.clone()).await;
                let next_delay = compute_next_certs_fetch_delay(&result, delay);
                if let Ok(certs) = result {
                    certs_reference.replace(certs);
                }
                schedule_fetch_certs(jwks_uri, certs_reference, next_delay);
            });
        },
    );
}

#[cfg(not(test))]
async fn fetch_certs(jwks_uri: String) -> Result<Vec<Jwk>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpMethod,
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
#[allow(clippy::needless_pass_by_value)]
fn transform_certs(response: HttpResponse) -> HttpResponse {
    if response.status != HTTP_STATUS_OK {
        trap("Invalid response status")
    }

    let certs: Certs =
        serde_json::from_slice(response.body.as_slice()).unwrap_or_else(|_| trap("Invalid JSON"));

    let mut sorted_keys = certs.keys.clone();
    sorted_keys.sort_by_key(|key| key.kid().unwrap_or_else(|| trap("Invalid JSON")).to_owned());

    let body =
        serde_json::to_vec(&Certs { keys: sorted_keys }).unwrap_or_else(|_| trap("Invalid JSON"));

    // All headers are ignored including the Cache-Control header, instead we fetch the certs
    // hourly since responses are always valid for at least 5 hours based on analysis of the
    // Cache-Control header over a timespan of multiple days, so hourly is a large enough margin.
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

fn create_rsa_public_key(jwk: &Jwk) -> Result<RsaPublicKey, String> {
    // Extract the RSA parameters (modulus 'n' and exponent 'e') from the JWK.
    let JwkParamsRsa { n, e, .. } = jwk
        .try_rsa_params()
        .map_err(|_| "Unable to extract modulus and exponent")?;

    // Decode the base64-url encoded modulus 'n' of the RSA public key.
    let n = BASE64_URL_SAFE_NO_PAD
        .decode(n)
        .map_err(|_| "Unable to decode modulus")?;

    // Decode the base64-url encoded public exponent 'e' of the RSA public key.
    let e = BASE64_URL_SAFE_NO_PAD
        .decode(e)
        .map_err(|_| "Unable to decode exponent")?;

    // Construct the RSA public key using the decoded modulus and exponent.
    RsaPublicKey::new(
        rsa::BigUint::from_bytes_be(&n),
        rsa::BigUint::from_bytes_be(&e),
    )
    .map_err(|_| "Unable to construct RSA public key".into())
}

/// Verifier implementation for `identity_jose` that verifies the signature of a JWT.
///
/// - `input`: A `VerificationInput` struct containing the JWT's algorithm (`alg`),
///   the signing input (payload to be hashed and verified), and the decoded signature.
/// - `jwk`: A reference to a `Jwk` (JSON Web Key) that contains the RSA public key
///   parameters (`n` and `e`) used to verify the JWT signature.
#[allow(clippy::needless_pass_by_value)]
fn verify_signature(input: VerificationInput, jwk: &Jwk) -> Result<(), SignatureVerificationError> {
    // Ensure the algorithm specified in the JWT header matches the expected algorithm (RS256).
    // If the algorithm does not match, return an UnsupportedAlg error.
    // Additional algorithms can be implemented here if needed in the future.
    if input.alg != RS256 {
        return Err(SignatureVerificationErrorKind::UnsupportedAlg.into());
    }

    // Compute the SHA-256 hash of the JWT payload (the signing input).
    // This hashed value will be used for signature verification.
    let hashed_input = Sha256::digest(input.signing_input);

    // Define the signature scheme to be used for verification (RSA PKCS#1 v1.5 with SHA-256).
    let scheme = Pkcs1v15Sign::new::<Sha256>();

    // Create RSA public key from JWK
    let public_key = create_rsa_public_key(jwk).map_err(|_| {
        SignatureVerificationError::new(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;

    // Verify the JWT signature using the RSA public key and the defined signature scheme.
    // If the signature is invalid, return an InvalidSignature error.
    public_key
        .verify(scheme, &hashed_input, input.decoded_signature.as_ref())
        .map_err(|_| SignatureVerificationErrorKind::InvalidSignature.into())
}

fn verify_claims(
    issuer: &str,
    client_id: &str,
    claims: &Claims,
    salt: &[u8; 32],
) -> Result<(), OpenIDJWTVerificationError> {
    let now = time();
    let mut hasher = Sha256::new();
    hasher.update(salt);
    hasher.update(caller().to_bytes());
    let hash: [u8; 32] = hasher.finalize().into();
    let expected_nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

    if claims.iss != issuer {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid issuer: {}",
            claims.iss
        )));
    }
    if !claims.aud.matches(client_id) {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Invalid audience".to_string(),
        ));
    }
    if claims.nonce != expected_nonce {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid nonce: {}",
            claims.nonce
        )));
    }
    if now > secs_to_nanos(claims.exp) {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    }
    if now > secs_to_nanos(claims.iat + MAX_VALIDITY_WINDOW_SECONDS) {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    }
    if now < secs_to_nanos(claims.iat) {
        return Err(OpenIDJWTVerificationError::GenericError(
            "JWT is not valid yet".into(),
        ));
    }
    if claims
        .email
        .as_ref()
        .is_some_and(|val| val.len() > MAX_EMAIL_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Email too long".into(),
        ));
    }
    if claims
        .name
        .as_ref()
        .is_some_and(|val| val.len() > MAX_NAME_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Name too long".into(),
        ));
    }
    if let Some(EmailVerifiedClaim::String(ref s)) = claims.email_verified {
        if s != "true" && s != "false" {
            return Err(OpenIDJWTVerificationError::GenericError(format!(
                "email_verified must be 'true' or 'false', got: {}",
                s
            )));
        }
    }

    Ok(())
}

#[cfg(test)]
thread_local! {
    static TEST_CALLER: Cell<Principal> = Cell::new(Principal::from_text("x4gp4-hxabd-5jt4d-wc6uw-qk4qo-5am4u-mncv3-wz3rt-usgjp-od3c2-oae").unwrap());
    static TEST_TIME: Cell<u64> = const { Cell::new(secs_to_nanos(1_736_794_102)) };
    static TEST_CERTS: Cell<Vec<Jwk>> = Cell::new(serde_json::from_str::<Certs>(r#"{"keys":[{"n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use": "sig","kty": "RSA","alg": "RS256","kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847","e": "AQAB"}]}"#).unwrap().keys);
}

#[cfg(not(test))]
fn caller() -> Principal {
    ic_cdk::caller()
}

#[cfg(test)]
fn caller() -> Principal {
    TEST_CALLER.get()
}

#[cfg(not(test))]
fn time() -> u64 {
    ic_cdk::api::time()
}

#[cfg(test)]
fn time() -> u64 {
    TEST_TIME.get()
}

#[test]
fn should_transform_certs_to_same() {
    let input = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![HttpHeader {
            name: "Cache-Control".into(),
            value: "public, max-age=18544, must-revalidate, no-transform".into()
        }],
        body: Vec::from(br#"{"keys":[{"e":"AQAB","alg":"RS256","kty":"RSA","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","use":"sig"},{"use":"sig","alg":"RS256","kty":"RSA","e":"AQAB","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","kid":"89ce3598c473af1bda4bff95e6c8736450206fba"}]}"#),
    };
    let expected = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body: Vec::from(br#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"89ce3598c473af1bda4bff95e6c8736450206fba","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","e":"AQAB"}]}"#),
    };

    assert_eq!(transform_certs(input), expected);
}

#[cfg(test)]
const TEST_AUD: &str = "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com";

#[cfg(test)]
fn test_data() -> (String, [u8; 32], OpenIdConfig, Claims) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let salt: [u8; 32] = [
        143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81,
        139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();
    let config: OpenIdConfig = OpenIdConfig {
        name: "Google".to_string(),
        logo: String::new(),
        issuer: claims.iss.clone(),
        client_id: TEST_AUD.into(),
        jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".to_string(),
        auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".to_string(),
        auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
        fedcm_uri: Some("https://accounts.google.com/gsi/fedcm.json".into()),
        email_verification: None,
    };

    (jwt.into(), salt, config, claims)
}

#[test]
fn should_return_credential() {
    let (jwt, salt, config, claims) = test_data();
    let provider = Provider::create(config);
    let credential = OpenIdCredential {
        iss: claims.iss,
        sub: claims.sub,
        aud: TEST_AUD.into(),
        last_usage_timestamp: None,
        metadata: HashMap::from([
            (
                "email".into(),
                MetadataEntryV2::String(claims.email.unwrap()),
            ),
            (
                "email_verified".into(),
                MetadataEntryV2::String(claims.email_verified.unwrap().into_string()),
            ),
            ("name".into(), MetadataEntryV2::String(claims.name.unwrap())),
        ]),
    };

    assert_eq!(provider.verify(&jwt, &salt), Ok(credential));
}

#[test]
fn should_return_error_when_encoding_invalid() {
    let (_, salt, config, _) = test_data();
    let provider = Provider::create(config);
    let invalid_jwt = "invalid-jwt";

    assert_eq!(
        provider.verify(invalid_jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Unable to decode JWT".into()
        ))
    );
}

#[test]
fn should_return_error_when_cert_missing() {
    TEST_CERTS.replace(vec![]);
    let (jwt, salt, config, _) = test_data();
    let provider = Provider::create(config);

    assert_eq!(
        provider.verify(&jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Certificate not found for dd125d5f462fbc6014aedab81ddf3bcedab70847".into()
        ))
    );
}

#[test]
fn should_return_error_when_signature_invalid() {
    let (jwt, salt, config, _) = test_data();
    let provider = Provider::create(config);
    let chunks: Vec<&str> = jwt.split('.').collect();
    let header = chunks[0];
    let payload = chunks[1];
    let invalid_signature = "f47b0sNskm-85sT5XtoRzORnfobK2nzVFF8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww5Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let invalid_jwt = [header, payload, invalid_signature].join(".");

    assert_eq!(
        provider.verify(&invalid_jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid signature".into()
        ))
    );
}

#[test]
fn should_return_error_when_invalid_issuer() {
    let (_, salt, config, claims) = test_data();
    let mut invalid_claims = claims;
    invalid_claims.iss = "invalid-issuer".into();

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid issuer: {}",
            invalid_claims.iss
        )))
    );
}

#[test]
fn should_return_error_when_invalid_audience() {
    let (_, salt, config, claims) = test_data();
    let mut invalid_claims = claims;
    invalid_claims.aud = AudClaim::Single("invalid-audience".into());

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid audience".into()
        ))
    );
}

#[test]
fn should_accept_client_id_in_aud_array() {
    let (_, salt, config, claims) = test_data();
    let mut multi_aud = claims;
    multi_aud.aud = AudClaim::Multiple(vec!["some-other-aud".into(), TEST_AUD.into()]);

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &multi_aud, &salt),
        Ok(())
    );
}

#[test]
fn should_reject_aud_array_without_client_id() {
    let (_, salt, config, claims) = test_data();
    let mut multi_aud = claims;
    multi_aud.aud = AudClaim::Multiple(vec!["a".into(), "b".into()]);

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &multi_aud, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid audience".into()
        ))
    );
}

#[test]
fn should_return_error_when_invalid_salt() {
    let (_, _, config, claims) = test_data();
    let invalid_salt: [u8; 32] = [
        143, 79, 58, 224, 18, 15, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81, 139,
        161, 218, 224, 243, 4, 120, 26, 123, 229, 242, 200, 189,
    ];

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &claims, &invalid_salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into()
        ))
    );
}

#[test]
fn should_return_error_when_invalid_caller() {
    TEST_CALLER.replace(
        Principal::from_text("necp6-24oof-6e2i2-xg7fk-pawxw-nlol2-by5bb-mltvt-sazk6-nqrzz-zae")
            .unwrap(),
    );
    let (_, salt, config, claims) = test_data();

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into()
        ))
    );
}

#[test]
fn should_return_error_when_no_longer_valid() {
    TEST_TIME.replace(time() + secs_to_nanos(MAX_VALIDITY_WINDOW_SECONDS) + 1);
    let (_, salt, config, claims) = test_data();

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::JWTExpired)
    );
}

#[test]
fn should_return_error_when_not_valid_yet() {
    TEST_TIME.replace(time() - 1);
    let (_, salt, config, claims) = test_data();

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "JWT is not valid yet".into()
        ))
    );
}

#[test]
fn should_return_error_when_email_too_long() {
    let (_, salt, config, claims) = test_data();
    let mut invalid_claims = claims;
    invalid_claims.email = Some("thisisanemailaddresswhichistoolongaccordingtothemaxlengththatisallowedbythestandardemailprotocolsandshouldnotbeconsideredasvalidbutitisusefultotestvalidationmechanismsintheapplicationwhichmayexceedstandardlimitationsforemailaddressesandshouldbetested@gmail.com".into());

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Email too long".into()
        ))
    );
}

#[test]
fn should_return_error_when_name_too_long() {
    let (_, salt, config, claims) = test_data();
    let mut invalid_claims = claims;
    invalid_claims.name = Some("Jonathan Maximilian Theodore Alexander Montgomery Fitzgerald Jameson Davidson Hawthorne Winchester Baldwin the Fifth of Lancaster".into());

    assert_eq!(
        verify_claims(&config.issuer, &config.client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Name too long".into()
        ))
    );
}
#[test]
fn should_compute_next_certs_fetch_delay() {
    const MIN_DELAY_SECONDS: u64 = 60;
    const MAX_DELAY_SECONDS: u64 = FETCH_CERTS_INTERVAL_SECONDS;

    let success: Result<(), ()> = Ok(());
    let error: Result<(), ()> = Err(());

    for (current_delay, expected_next_delay_on_error) in [
        // Should be at least `MIN_DELAY` (1 minute)
        (None, Some(MIN_DELAY_SECONDS)),
        (Some(0), Some(MIN_DELAY_SECONDS)),
        (Some(1), Some(MIN_DELAY_SECONDS)),
        (Some(MIN_DELAY_SECONDS / 2 - 1), Some(MIN_DELAY_SECONDS)),
        // Should be multiplied by two
        (Some(MIN_DELAY_SECONDS / 2), Some(MIN_DELAY_SECONDS)),
        (Some(MIN_DELAY_SECONDS / 2 + 1), Some(MIN_DELAY_SECONDS + 2)),
        (Some(120), Some(240)),
        (Some(120), Some(240)),
        (Some(240), Some(480)),
        // Should be at most `MAX_DELAY` (15 minutes)
        (Some(480), Some(MAX_DELAY_SECONDS)),
        (Some(MAX_DELAY_SECONDS / 2 + 1), Some(MAX_DELAY_SECONDS)),
        (Some(MAX_DELAY_SECONDS * 2), Some(MAX_DELAY_SECONDS)),
    ] {
        // Should return `None` on success so default (`FETCH_CERTS_INTERVAL`) delay is used.
        assert_eq!(
            compute_next_certs_fetch_delay(&success, current_delay),
            None
        );
        // Should return `expected_next_delay_on_error` on error as specified above.
        assert_eq!(
            compute_next_certs_fetch_delay(&error, current_delay),
            expected_next_delay_on_error
        );
    }
}
