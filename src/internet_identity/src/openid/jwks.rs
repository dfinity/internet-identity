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
use identity_jose::jwk::{Jwk, JwkParamsRsa, JwkUse};
use identity_jose::jws::JwsAlgorithm::RS256;

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
/// fill's transient buffer; what is kept is bounded by [`verifiable_keys`].
/// Deliberately broad: this fetch is shared with the configured
/// Google/Microsoft refresh, which must not be rejected.
#[cfg(not(test))]
const JWKS_MAX_RESPONSE_BYTES: u64 = 32 * 1024;

/// Cap on the number of keys kept from a JWKS. Verification matches by `kid` and
/// real providers publish well under this (Microsoft ~8); keys beyond the cap
/// are dropped. Bounds both the stored key set and the verify-time scan, so a
/// provider serving an absurd number of keys only degrades its own SSO entry.
const JWKS_MAX_KEYS: usize = 20;

/// Maximum length of a kept key's `kid`.
const MAX_KID_LENGTH: usize = 255;

/// Maximum base64url length of a kept key's modulus `n`: a 4096-bit modulus,
/// the largest `rsa::RsaPublicKey` accepts.
const MAX_RSA_MODULUS_LENGTH: usize = 683;

/// Maximum base64url length of a kept key's public exponent `e`: five bytes,
/// covering the largest exponent `rsa::RsaPublicKey` accepts (`2^33 - 1`).
const MAX_RSA_EXPONENT_LENGTH: usize = 7;

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
        .map(|res| verifiable_keys(res.keys))
}

/// The keys of a JWKS that can verify an RS256 JWT, each reduced to the fields
/// verification reads (`kid`, `alg`, `n`, `e`), deduplicated by `kid` and capped
/// at [`JWKS_MAX_KEYS`]. Everything else a provider publishes (`x5c` chains,
/// other key types, encryption keys) is dropped.
pub(super) fn verifiable_keys(keys: Vec<Jwk>) -> Vec<Jwk> {
    let mut kept: Vec<Jwk> = Vec::with_capacity(keys.len().min(JWKS_MAX_KEYS));
    for key in &keys {
        if kept.len() == JWKS_MAX_KEYS {
            break;
        }
        let Some(kid) = key.kid() else {
            continue;
        };
        let Ok(JwkParamsRsa { n, e, .. }) = key.try_rsa_params() else {
            continue;
        };
        let alg = key.alg();
        if kid.len() > MAX_KID_LENGTH
            || n.len() > MAX_RSA_MODULUS_LENGTH
            || e.len() > MAX_RSA_EXPONENT_LENGTH
            || alg.is_some_and(|alg| alg != RS256.name())
            || key.use_().is_some_and(|use_| use_ != JwkUse::Signature)
            || kept.iter().any(|k| k.kid() == Some(kid))
        {
            continue;
        }
        let mut params = JwkParamsRsa::new();
        params.n.clone_from(n);
        params.e.clone_from(e);
        let mut stripped = Jwk::from_params(params);
        stripped.set_kid(kid);
        if let Some(alg) = alg {
            stripped.set_alg(alg);
        }
        kept.push(stripped);
    }
    kept
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

    let body = serde_json::to_vec(&Certs {
        keys: keys_sorted_by_kid(certs.keys),
    })
    .unwrap_or_else(|_| trap("Invalid JSON"));

    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

/// The keys that carry a `kid`, sorted by it so every replica's transform
/// produces the same response. A key without a `kid` can't be matched to a JWT,
/// so it is dropped rather than failing the whole key set.
fn keys_sorted_by_kid(keys: Vec<Jwk>) -> Vec<Jwk> {
    let mut keys: Vec<Jwk> = keys.into_iter().filter(|key| key.kid().is_some()).collect();
    keys.sort_by(|a, b| a.kid().cmp(&b.kid()));
    keys
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn jwk(value: serde_json::Value) -> Jwk {
        serde_json::from_value(value).unwrap()
    }

    fn rsa(kid: &str) -> serde_json::Value {
        json!({ "kty": "RSA", "kid": kid, "n": "n".repeat(342), "e": "AQAB" })
    }

    fn kids(keys: &[Jwk]) -> Vec<&str> {
        keys.iter().map(|k| k.kid().unwrap()).collect()
    }

    #[test]
    fn keeps_only_the_fields_verification_reads() {
        let mut microsoft = rsa("ms");
        microsoft["use"] = json!("sig");
        microsoft["x5t"] = json!("thumbprint");
        microsoft["x5c"] = json!(["certificate"]);
        let mut google = rsa("google");
        google["alg"] = json!("RS256");

        let kept = verifiable_keys(vec![jwk(microsoft), jwk(google)]);

        assert_eq!(
            serde_json::to_value(&kept).unwrap(),
            json!([
                { "kty": "RSA", "kid": "ms", "n": "n".repeat(342), "e": "AQAB" },
                { "kty": "RSA", "kid": "google", "alg": "RS256", "n": "n".repeat(342), "e": "AQAB" },
            ])
        );
    }

    #[test]
    fn drops_keys_that_cannot_verify_rs256() {
        let mut other_alg = rsa("rs512");
        other_alg["alg"] = json!("RS512");
        let mut encryption = rsa("enc");
        encryption["use"] = json!("enc");
        let mut oversized_modulus = rsa("big-n");
        oversized_modulus["n"] = json!("n".repeat(MAX_RSA_MODULUS_LENGTH + 1));
        let mut oversized_exponent = rsa("big-e");
        oversized_exponent["e"] = json!("e".repeat(MAX_RSA_EXPONENT_LENGTH + 1));
        let ec = json!({ "kty": "EC", "kid": "ec", "crv": "P-256", "x": "x", "y": "y" });
        let no_kid = json!({ "kty": "RSA", "n": "n", "e": "AQAB" });

        let kept = verifiable_keys(
            [
                rsa(&"k".repeat(MAX_KID_LENGTH + 1)),
                other_alg,
                encryption,
                oversized_modulus,
                oversized_exponent,
                ec,
                no_kid,
                rsa("ok"),
            ]
            .into_iter()
            .map(jwk)
            .collect(),
        );

        assert_eq!(kids(&kept), vec!["ok"]);
    }

    #[test]
    fn keeps_the_largest_accepted_rsa_parameters() {
        let mut largest = rsa(&"k".repeat(MAX_KID_LENGTH));
        largest["n"] = json!("n".repeat(MAX_RSA_MODULUS_LENGTH));
        largest["e"] = json!("e".repeat(MAX_RSA_EXPONENT_LENGTH));

        assert_eq!(verifiable_keys(vec![jwk(largest)]).len(), 1);
    }

    #[test]
    fn keeps_the_first_key_per_kid() {
        let mut second = rsa("a");
        second["alg"] = json!("RS256");

        let kept = verifiable_keys(vec![jwk(rsa("a")), jwk(second), jwk(rsa("b"))]);

        assert_eq!(kids(&kept), vec!["a", "b"]);
        assert_eq!(kept[0].alg(), None);
    }

    #[test]
    fn transform_drops_keys_without_kid_and_sorts_by_kid() {
        let no_kid = json!({ "kty": "RSA", "n": "n", "e": "AQAB" });

        let sorted = keys_sorted_by_kid(vec![jwk(rsa("b")), jwk(no_kid), jwk(rsa("a"))]);

        assert_eq!(kids(&sorted), vec!["a", "b"]);
    }

    #[test]
    fn caps_the_key_count_and_its_allocation() {
        let keys = (0..1_000).map(|i| jwk(rsa(&i.to_string()))).collect();

        let kept = verifiable_keys(keys);

        assert_eq!(kept.len(), JWKS_MAX_KEYS);
        assert_eq!(kept.capacity(), JWKS_MAX_KEYS);
    }
}
