//! DoH (DNS-over-HTTPS) fallback configuration.
//!
//! Configures the fallback path the canister uses when a domain is not
//! DNSSEC-signed (the design doc §7.6 case — covers Gmail, Outlook,
//! iCloud, Yahoo, and most other consumer mailbox providers). Lets the
//! canister fetch DKIM and DMARC TXT records over HTTPS via a fixed
//! quorum of independent DoH providers, instead of consuming a
//! caller-supplied DNSSEC chain.
//!
//! Set on every `init` / `post_upgrade` via the canister-wide
//! [`InternetIdentityInit`] arg, kept in `PersistentState` so it
//! survives upgrades when the arg omits the field.

use candid::{CandidType, Deserialize};

/// Allowlist + tuning knobs for the DoH fallback path.
///
/// We deliberately don't poll the allowlist proactively: the cache is
/// populated on demand, the first time `smtp_request` arrives for a
/// listed domain. Adding a domain costs nothing until mail actually
/// flows; removing a domain is enforced at the *next* call (the cache
/// entry expires and isn't refilled).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct DohConfig {
    /// Domains for which DoH outcalls are permitted. The canister will
    /// only fetch DKIM / DMARC TXT records for an FQDN whose
    /// registered domain is in this list. Anything else is rejected
    /// without an outcall — DNSSEC is the only path for unlisted
    /// domains.
    ///
    /// Each entry is the *registered* domain (e.g. `gmail.com`,
    /// `outlook.com`) lowercased. The canister extracts the registered
    /// domain from the queried FQDN by dropping leading subdomain
    /// labels until it finds an exact match, with a label-anchored
    /// dot check (so `evilexample.com` cannot match `example.com`).
    pub allowed_domains: Vec<String>,

    /// Maximum age, in seconds, that the in-memory cache keeps a
    /// fetched TXT record before re-fetching. Defaults to 1 hour
    /// (3600) when omitted; capped at 24 hours by the verifier so a
    /// stuck cache can't shadow a real key rotation indefinitely.
    pub max_cache_age_secs: Option<u64>,
}
