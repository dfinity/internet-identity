//! Internal types for the DoH fallback.

/// One DoH provider in the quorum.
///
/// All three providers below run free public DoH endpoints as a public
/// service (not a commercial product), so we don't expect a-rate-
/// limit-killed-our-canister failure mode. Each is operated in a
/// different jurisdiction; we deliberately don't lean on a single
/// political/legal regime for the answer to "what does Gmail's DKIM
/// key look like right now".
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DohProvider {
    /// Display name for diagnostics / logging.
    pub name: &'static str,
    /// Country/jurisdiction tag, for diagnostics.
    pub jurisdiction: &'static str,
    /// Wire-format DoH endpoint (`application/dns-message` POST).
    pub url: &'static str,
}

/// The three providers we query in parallel for every cache miss.
///
/// Picked for jurisdictional diversity (CH / CA / US), public-service
/// rather than commercial operation, and continuous uptime under their
/// current foundations / non-profits as of 2026-05.
pub const PROVIDERS: &[DohProvider] = &[
    DohProvider {
        name: "Quad9",
        jurisdiction: "Switzerland",
        url: "https://dns.quad9.net/dns-query",
    },
    DohProvider {
        name: "CIRA Canadian Shield",
        jurisdiction: "Canada",
        url: "https://private.canadianshield.cira.ca/dns-query",
    },
    DohProvider {
        name: "Cloudflare",
        jurisdiction: "United States",
        url: "https://cloudflare-dns.com/dns-query",
    },
];

/// Quorum threshold: we accept the response iff at least this many of
/// `PROVIDERS.len()` returned identical TXT bytes.
pub const QUORUM_THRESHOLD: usize = 2;

/// Default cache TTL when the deploy arg doesn't override it. One hour
/// is short enough that a key rotation is picked up the next time mail
/// flows after the rotation, long enough to cover ~all of a recovery
/// flow's expected duration.
pub const DEFAULT_CACHE_AGE_SECS: u64 = 3600;

/// Hard cap on `DohConfig.max_cache_age_secs`. Stale keys can break
/// recovery for users mid-flow when a provider rotates; the cap keeps
/// "stuck cache" from being the failure mode.
pub const MAX_CACHE_AGE_SECS: u64 = 24 * 3600;

/// All the ways the DoH path can fail. The verifier collapses these
/// into a single fail-reason variant up the call stack; the granular
/// shape is for diagnostics and tests.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DohError {
    /// The queried registered domain isn't on `allowed_domains`. The
    /// verifier will not go to the internet for arbitrary domains.
    DomainNotAllowed,
    /// All three outcalls failed (network error / non-200 / etc).
    AllProvidersFailed,
    /// Outcalls succeeded but the responses didn't reach the quorum
    /// threshold of identical TXT bytes.
    QuorumFailed { agreeing: usize, total: usize },
    /// A single response was received but failed to parse as a DNS
    /// message with a valid TXT record. (Reported during quorum
    /// counting; not a top-level failure unless every response is
    /// malformed.)
    ResponseMalformed(String),
    /// The deploy/upgrade arg never set a `DohConfig`, or the config
    /// was cleared via `Some(None)`. The DoH fallback is disabled.
    NotConfigured,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn three_providers_three_jurisdictions() {
        assert_eq!(PROVIDERS.len(), 3);
        let mut seen: Vec<&str> = PROVIDERS.iter().map(|p| p.jurisdiction).collect();
        seen.sort();
        seen.dedup();
        assert_eq!(seen.len(), 3, "providers must span 3 jurisdictions");
    }

    #[test]
    fn quorum_strictly_above_minority() {
        // 2-of-3 is a Byzantine-tolerant majority: any one provider
        // failing or lying still leaves a majority of two honest
        // providers agreeing. If we accidentally relax to 1-of-3, a
        // single dishonest provider could drive verification.
        assert!(QUORUM_THRESHOLD * 2 > PROVIDERS.len());
    }

    #[test]
    fn cache_age_cap_is_one_day() {
        assert_eq!(MAX_CACHE_AGE_SECS, 86_400);
    }

    #[test]
    fn provider_urls_use_dns_query_path() {
        for p in PROVIDERS {
            assert!(
                p.url.ends_with("/dns-query"),
                "{} url should end in /dns-query",
                p.name
            );
            assert!(p.url.starts_with("https://"), "{} must be https", p.name);
        }
    }
}
