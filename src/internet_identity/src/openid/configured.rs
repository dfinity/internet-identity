//! Configured (hardcoded) OpenID providers: Google, Microsoft, Apple.
//!
//! A fixed registry (`CONFIG_REGISTRY`) of provider data installed from the
//! canister's init args. Their JWKs are persisted in stable storage (memory id
//! 24, keyed by issuer), seeded at install time and refreshed by a periodic
//! timer, so they're always synchronously readable and the configured path
//! never returns `Pending`. SSO providers source their JWKs differently (see
//! [`super::sso`]); the JWK read ([`super::jwks`]) is the only place the two
//! diverge.

use super::verify::Descriptor;
use super::OpenIdCredential;
use super::{get_all_claims, get_issuer_placeholders, replace_issuer_placeholders, AudClaim};
use identity_jose::jwk::Jwk;
use internet_identity_interface::internet_identity::types::{
    OpenIdConfig, OpenIdEmailVerificationScheme,
};
use std::cell::RefCell;

#[cfg(not(test))]
use crate::state;

// Fetch the certs every fifteen minutes, the responses are always
// valid for a couple of hours so that should be enough margin.
#[cfg(not(test))]
const FETCH_CERTS_INTERVAL_SECONDS: u64 = 60 * 15; // 15 minutes in seconds

/// A configured provider's static data. The JWKs live separately in stable
/// storage keyed by `issuer` — this holds only what's needed to match a JWT
/// and shape the credential.
struct ConfiguredProvider {
    /// Issuer, possibly a placeholder template (Microsoft's
    /// `https://login.microsoftonline.com/{tid}/v2.0`). Also the stable-storage
    /// key under which this provider's JWKs are persisted.
    issuer: String,
    client_id: String,
    email_verification: Option<OpenIdEmailVerificationScheme>,
}

thread_local! {
    static CONFIG_REGISTRY: RefCell<Vec<ConfiguredProvider>> = const { RefCell::new(vec![]) };
}

/// Install the configured providers from init args. Seeds + persists each
/// provider's JWK cache and starts its periodic refresh timer (non-test).
pub fn setup(configs: Vec<OpenIdConfig>) {
    for config in configs {
        #[cfg(not(test))]
        {
            let keys = initial_certs(&config);
            state::storage_borrow_mut(|s| s.write_openid_jwks(&config.issuer, keys));
            schedule_fetch_certs(config.issuer.clone(), config.jwks_uri.clone(), Some(0));
        }
        CONFIG_REGISTRY.with_borrow_mut(|registry| {
            registry.push(ConfiguredProvider {
                issuer: config.issuer,
                client_id: config.client_id,
                email_verification: config.email_verification,
            });
        });
    }
}

/// Resolve the configured provider for an incoming JWT, returning its verify
/// descriptor and the issuer key under which its JWKs are stored. The issuer
/// template is expanded with the JWT's claims (e.g. Microsoft's `{tid}`) before
/// comparing to the JWT's `iss`. `None` if no configured provider matches.
pub(super) fn resolve(
    iss: &str,
    aud: &AudClaim,
    claims_bytes: &[u8],
) -> Option<(Descriptor, String)> {
    CONFIG_REGISTRY.with_borrow(|registry| {
        registry.iter().find_map(|provider| {
            let placeholders = get_issuer_placeholders(&provider.issuer);
            let issuer_claims = get_all_claims(claims_bytes, placeholders);
            let effective_issuer = replace_issuer_placeholders(&provider.issuer, &issuer_claims);
            if effective_issuer == iss && aud.matches(&provider.client_id) {
                Some((
                    Descriptor {
                        issuer: provider.issuer.clone(),
                        client_id: provider.client_id.clone(),
                        sso: None,
                    },
                    provider.issuer.clone(),
                ))
            } else {
                None
            }
        })
    })
}

/// The (template) issuer of the configured provider matching a stored
/// credential, expanding placeholders from the credential's stored metadata
/// (e.g. `tid`). `None` if no configured provider matches — including for SSO
/// credentials, which callers should short-circuit on `sso_domain` before
/// reaching here. Mirrors the `resolve` match against stored claims instead of
/// fresh ones.
pub(super) fn config_issuer_for(credential: &OpenIdCredential) -> Option<String> {
    CONFIG_REGISTRY.with_borrow(|registry| {
        registry
            .iter()
            .find(|provider| matches_credential(provider, credential))
            .map(|provider| provider.issuer.clone())
    })
}

/// The email-verification scheme of the configured provider matching a stored
/// credential, if any. `None` for SSO credentials and unmatched credentials.
pub(super) fn email_scheme_for(
    credential: &OpenIdCredential,
) -> Option<OpenIdEmailVerificationScheme> {
    CONFIG_REGISTRY.with_borrow(|registry| {
        registry
            .iter()
            .find(|provider| matches_credential(provider, credential))
            .and_then(|provider| provider.email_verification)
    })
}

/// Whether a configured provider matches a stored credential: its issuer
/// template, expanded from the credential's metadata-stored issuer claims,
/// equals the credential's `iss`, and its `client_id` equals the `aud`.
fn matches_credential(provider: &ConfiguredProvider, credential: &OpenIdCredential) -> bool {
    let placeholders = get_issuer_placeholders(&provider.issuer);
    let mut issuer_claims: Vec<(String, String)> = vec![];
    for key in placeholders {
        let Some(value) = credential.read_metadata_string(&key) else {
            return false;
        };
        issuer_claims.push((key, value));
    }
    let effective_issuer = replace_issuer_placeholders(&provider.issuer, &issuer_claims);
    effective_issuer == credential.iss && provider.client_id == credential.aud
}

/// Read this provider's persisted JWKs from stable storage. The configured
/// source is seeded at install and timer-refreshed, so it is always
/// synchronously available (an empty vec on a cold/unseeded canister surfaces
/// later as "certificate not found", never as `Pending`).
#[cfg(not(test))]
pub(super) fn read_stable_jwks(issuer: &str) -> Vec<Jwk> {
    state::storage_borrow(|s| s.read_openid_jwks(issuer)).unwrap_or_default()
}

#[cfg(test)]
pub(super) fn read_stable_jwks(_issuer: &str) -> Vec<Jwk> {
    TEST_JWKS.with_borrow(|keys| keys.clone())
}

#[cfg(test)]
thread_local! {
    static TEST_JWKS: RefCell<Vec<Jwk>> = const { RefCell::new(vec![]) };
}

/// Test-only: register a configured provider and seed its JWKs without running
/// the install-time outcall/timer machinery.
#[cfg(test)]
pub(crate) fn setup_for_test(config: OpenIdConfig, keys: Vec<Jwk>) {
    TEST_JWKS.with_borrow_mut(|k| *k = keys);
    CONFIG_REGISTRY.with_borrow_mut(|registry| {
        registry.push(ConfiguredProvider {
            issuer: config.issuer,
            client_id: config.client_id,
            email_verification: config.email_verification,
        });
    });
}

#[cfg(test)]
pub(crate) fn clear_for_test() {
    CONFIG_REGISTRY.with_borrow_mut(|r| r.clear());
    TEST_JWKS.with_borrow_mut(|k| k.clear());
}

// ---------------------------------------------------------------------------
// Seed + persistence + refresh timer (configured providers only).
// ---------------------------------------------------------------------------

/// Build a single [`Jwk`] from one entry of [`OpenIdConfig::seed_jwks`] — the
/// `(field, value)` pairs of one JWK's JSON object (e.g. `("kty","RSA")`,
/// `("kid","...")`, `("n","...")`, `("e","AQAB")`). The pairs are assembled into
/// a JSON object and parsed with the same `serde` path used for fetched certs,
/// so a seeded key is byte-for-byte identical to a fetched one.
#[cfg(not(test))]
fn build_seed_jwk(pairs: &[(String, String)]) -> Result<Jwk, String> {
    let mut map = serde_json::Map::new();
    for (k, v) in pairs {
        map.insert(k.clone(), serde_json::Value::String(v.clone()));
    }
    let value = serde_json::Value::Object(map);
    serde_json::from_value::<Jwk>(value).map_err(|e| format!("invalid seed JWK: {e}"))
}

/// Build every JWK supplied via [`OpenIdConfig::seed_jwks`]: one [`Jwk`] per
/// inner entry. Invalid entries are logged and skipped.
#[cfg(not(test))]
fn build_seed_jwks(entries: &[Vec<(String, String)>]) -> (Vec<Jwk>, Vec<String>) {
    let mut keys = vec![];
    let mut errors = vec![];
    for entry in entries {
        match build_seed_jwk(entry) {
            Ok(jwk) => keys.push(jwk),
            Err(err) => errors.push(err),
        }
    }
    (keys, errors)
}

/// Compute the initial contents of a provider's JWK cache.
///
/// - If `config.seed_jwks` supplies any valid JWKs, they are authoritative.
/// - Otherwise, fall back to the JWKs already persisted for this `issuer` (from
///   an earlier seed or fetch), so verification keeps working across upgrades
///   before the first post-upgrade fetch.
#[cfg(not(test))]
fn initial_certs(config: &OpenIdConfig) -> Vec<Jwk> {
    if let Some(entries) = config.seed_jwks.as_ref() {
        let (keys, errors) = build_seed_jwks(entries);
        for err in errors {
            ic_cdk::println!("Skipping invalid seed JWK for {}: {err}", config.issuer);
        }
        if !keys.is_empty() {
            return keys;
        }
    }
    state::storage_borrow(|s| s.read_openid_jwks(&config.issuer)).unwrap_or_default()
}

#[cfg(not(test))]
fn compute_next_certs_fetch_delay<T, E>(
    result: &Result<T, E>,
    current_delay: Option<u64>,
) -> Option<u64> {
    const MIN_DELAY_SECONDS: u64 = 60;
    const MAX_DELAY_SECONDS: u64 = FETCH_CERTS_INTERVAL_SECONDS;
    const BACKOFF_MULTIPLIER: u64 = 2;

    match result {
        Ok(_) => None,
        Err(_) => Some(
            current_delay
                .map_or(MIN_DELAY_SECONDS, |d| d * BACKOFF_MULTIPLIER)
                .clamp(MIN_DELAY_SECONDS, MAX_DELAY_SECONDS),
        ),
    }
}

/// Schedule a (re-)fetch of a configured provider's JWKs. Successfully fetched
/// keys are written through to stable storage under `issuer`, so the latest
/// keys (not just the seed) survive canister upgrades.
#[cfg(not(test))]
fn schedule_fetch_certs(issuer: String, jwks_uri: String, delay: Option<u64>) {
    use ic_cdk::spawn;
    use ic_cdk_timers::set_timer;
    use std::time::Duration;

    set_timer(
        Duration::from_secs(delay.unwrap_or(FETCH_CERTS_INTERVAL_SECONDS)),
        move || {
            spawn(async move {
                let result = super::jwks::fetch_jwks(jwks_uri.clone()).await;
                let next_delay = compute_next_certs_fetch_delay(&result, delay);
                if let Ok(certs) = result {
                    state::storage_borrow_mut(|s| s.write_openid_jwks(&issuer, certs));
                }
                schedule_fetch_certs(issuer, jwks_uri, next_delay);
            });
        },
    );
}
