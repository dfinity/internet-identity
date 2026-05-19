//! Internal types for the DoH fallback.

/// One DoH provider in the quorum.
///
/// All five providers run free public DoH endpoints as a public service
/// rather than a commercial product, so we don't expect a "rate limit
/// killed our canister" failure mode. Each is operated in a different
/// jurisdiction (with the deliberate exception of two US providers,
/// see [`PROVIDERS`]); we don't lean on a single political/legal regime
/// for the answer to "what does Gmail's DKIM key look like right now".
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DohProvider {
    /// Display name for diagnostics / logging.
    pub name: &'static str,
    /// Country/jurisdiction tag, for diagnostics.
    pub jurisdiction: &'static str,
    /// Wire-format DoH endpoint (`application/dns-message` POST).
    pub url: &'static str,
}

/// The five providers we query in parallel for every cache miss.
///
/// Picked for jurisdictional diversity (US ×2, CH, CA, JP), a mix of
/// operator types (commercial telcos, non-profit foundation, ISP), and
/// continuous uptime under their current operators as of 2026-05. Two
/// US providers are tolerable here only because the quorum requires
/// 3-of-5 — a single jurisdiction can't reach the threshold on its own.
pub const PROVIDERS: &[DohProvider] = &[
    DohProvider {
        name: "Cloudflare",
        jurisdiction: "United States",
        url: "https://cloudflare-dns.com/dns-query",
    },
    DohProvider {
        name: "Google",
        jurisdiction: "United States",
        url: "https://dns.google/dns-query",
    },
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
        name: "IIJ",
        jurisdiction: "Japan",
        url: "https://public.dns.iij.jp/dns-query",
    },
];

/// Quorum threshold: we accept the response iff at least this many of
/// `PROVIDERS.len()` returned identical TXT bytes.
///
/// 3-of-5 is a strict majority (>n/2) and tolerates up to 2 providers
/// being down, slow, or returning different bytes — including both US
/// providers being subpoenaed to lie in the same direction.
pub const QUORUM_THRESHOLD: usize = 3;

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
    /// Every provider's outcall failed (network error / non-200 / etc).
    /// "Every" means all of `PROVIDERS.len()` — currently five.
    AllProvidersFailed,
    /// Outcalls succeeded but the responses didn't reach the quorum
    /// threshold of identical TXT bytes.
    QuorumFailed { agreeing: usize, total: usize },
    /// Quorum of providers authoritatively reported no record at this
    /// name (DNS `RCODE=NXDOMAIN` or an empty answer section). Distinct
    /// from `QuorumFailed` / `AllProvidersFailed` so the caller can
    /// distinguish "this record doesn't exist" — a valid DNS state — from
    /// "we couldn't reach a verdict on what this record is". DMARC, for
    /// example, treats a missing `_dmarc` TXT as "no policy published"
    /// and falls back to strict alignment (design §6.3); a transient
    /// outage should NOT take the same fallback — it should fail.
    NoAnswer,
    /// A single response was received but failed to parse as a DNS
    /// message with a valid TXT record. (Reported during quorum
    /// counting; not a top-level failure unless every response is
    /// malformed.)
    ResponseMalformed(String),
    /// The deploy/upgrade arg never set a `DohConfig`, or the config
    /// was cleared via `Some(None)`. The DoH fallback is disabled.
    NotConfigured,
    /// The FQDN we were asked to look up isn't a valid wire-format
    /// DNS name (label > 63 octets, name > 255 octets, or empty
    /// label). We refuse to silently truncate, so this is a
    /// caller-visible failure rather than a quietly-different lookup.
    InvalidName(String),
    /// The FQDN doesn't sit inside the registered domain that the
    /// caller passed for the allowlist check. Usually a caller bug;
    /// we fail closed rather than make an outcall for an unrelated
    /// name.
    NameOutsideRegisteredDomain {
        name: String,
        registered_domain: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn five_providers_four_jurisdictions() {
        assert_eq!(PROVIDERS.len(), 5);
        // We allow two US providers (Cloudflare + Google) but want at
        // least 4 distinct jurisdictions so no single jurisdiction has
        // anywhere near the agreeing threshold of 3.
        let mut seen: Vec<&str> = PROVIDERS.iter().map(|p| p.jurisdiction).collect();
        seen.sort();
        seen.dedup();
        assert!(
            seen.len() >= 4,
            "providers must span at least 4 jurisdictions"
        );
    }

    #[test]
    fn no_jurisdiction_alone_can_reach_quorum() {
        // The whole point of 3-of-5 with mixed jurisdictions is that no
        // single legal/political regime — including a 2-provider one
        // like the US — can on its own force a "quorum". If this
        // assertion ever fires, we've over-concentrated.
        let mut counts: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
        for p in PROVIDERS {
            *counts.entry(p.jurisdiction).or_default() += 1;
        }
        let max_per_jurisdiction = counts.values().copied().max().unwrap_or(0);
        assert!(
            max_per_jurisdiction < QUORUM_THRESHOLD,
            "no single jurisdiction may reach quorum on its own"
        );
    }

    #[test]
    fn quorum_strictly_above_minority() {
        // 3-of-5 is a strict majority. If we accidentally relax to
        // 2-of-5, two split-bucket factions could each claim "quorum".
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
