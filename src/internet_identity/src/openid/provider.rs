//! Resolve an incoming JWT to a provider descriptor + JWK source — the dispatch
//! that picks between the two kinds. No trait objects: a configured provider is
//! a `(iss, aud)` match against the data-only `CONFIG_REGISTRY`; an SSO provider
//! is a discovery-cache lookup by the FE-supplied domain.

use super::jwks::JwkSource;
use super::verify::Descriptor;
use super::{configured, sso, AudClaim, OpenIDJWTVerificationError, VerifyMode};
use crate::single_flight_cache::Cached;

/// A resolved provider: the descriptor the shared verify pipeline needs, plus
/// the JWK source backing it.
pub(super) struct Resolved {
    pub descriptor: Descriptor,
    pub jwk_source: JwkSource,
}

/// Resolve the provider for a JWT.
///
/// - `discovery_domain == None` → configured provider, matched by `(iss, aud)`
///   against `CONFIG_REGISTRY`; synchronous (`Ready` or an "unsupported issuer"
///   error).
/// - `discovery_domain == Some(domain)` → SSO provider, resolved via the
///   discovery cache; may read `Cached::Pending` on a cold cache.
pub(super) fn resolve(
    iss: &str,
    aud: &AudClaim,
    claims_bytes: &[u8],
    discovery_domain: Option<&str>,
    mode: VerifyMode,
) -> Result<Cached<Resolved>, OpenIDJWTVerificationError> {
    match discovery_domain {
        Some(domain) => {
            let (descriptor, jwks_uri) = match sso::resolve(domain, iss, mode)? {
                Cached::Pending => return Ok(Cached::Pending),
                Cached::Ready(pair) => pair,
            };
            Ok(Cached::Ready(Resolved {
                descriptor,
                jwk_source: JwkSource::Sso(jwks_uri),
            }))
        }
        None => {
            let (descriptor, issuer_key) =
                configured::resolve(iss, aud, claims_bytes).ok_or_else(|| {
                    OpenIDJWTVerificationError::GenericError(format!("Unsupported issuer: {iss}"))
                })?;
            Ok(Cached::Ready(Resolved {
                descriptor,
                jwk_source: JwkSource::Configured(issuer_key),
            }))
        }
    }
}
