//! Dispatch an incoming JWT to a provider descriptor + JWK source, picking
//! between the two provider kinds. A configured provider is a `(iss, aud)` match
//! against the data-only `CONFIG_REGISTRY`; an SSO provider is a discovery-cache
//! lookup by the frontend-supplied domain.

use super::jwks::JwkSource;
use super::verify::Descriptor;
use super::{configured, sso, AudClaim, OpenIDJWTVerificationError};
use crate::single_flight_cache::Cached;

/// A resolved provider: the descriptor the shared verify pipeline needs, plus
/// the JWK source backing it.
pub(super) struct Resolved {
    pub descriptor: Descriptor,
    pub jwk_source: JwkSource,
}

/// Resolve the provider for a JWT. `Some(domain)` resolves an SSO provider from
/// the discovery cache (peek-only — `Cached::Pending` until the cache is warm);
/// `None` resolves a configured provider by `(iss, aud)` against
/// `CONFIG_REGISTRY` (`Ready`, or an "unsupported issuer" error).
pub(super) fn resolve(
    iss: &str,
    aud: &AudClaim,
    claims_bytes: &[u8],
    discovery_domain: Option<&str>,
) -> Result<Cached<Resolved>, OpenIDJWTVerificationError> {
    match discovery_domain {
        Some(domain) => Ok(match sso::resolve(domain, iss)? {
            Cached::Pending => Cached::Pending,
            Cached::Ready((descriptor, jwks_uri)) => Cached::Ready(Resolved {
                descriptor,
                jwk_source: JwkSource::Sso(jwks_uri),
            }),
        }),
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
