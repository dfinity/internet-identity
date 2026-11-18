//! DoH (DNS-over-HTTPS) configuration.
//!
//! Configures how the canister fetches DKIM and DMARC TXT records over
//! HTTPS via a fixed quorum of independent DoH providers, the path every
//! email domain takes when no DNSSEC chain is supplied.
//!
//! Set on every `init` / `post_upgrade` via the canister-wide
//! [`InternetIdentityInit`] arg, kept in `PersistentState` so it
//! survives upgrades when the arg omits the field.

use candid::{CandidType, Deserialize};

/// Configuration for the DoH path. While a `DohConfig` is set, the canister
/// resolves DKIM / DMARC records over DoH for any email domain; while none is
/// set, the DoH path is disabled.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct DohConfig {
    /// Registered domains (e.g. `gmail.com`, `outlook.com`) whose lookups
    /// draw from an outcall budget of their own, so lookups for other
    /// domains can never use up the capacity these domains rely on.
    /// Matched case-insensitively against the registered domain of the
    /// email address. `None` reserves no domain.
    pub reserved_domains: Option<Vec<String>>,

    /// Maximum age, in seconds, that the in-memory cache keeps a
    /// fetched TXT record fresh before re-fetching. Defaults to 1 hour
    /// (3600) when omitted; otherwise the operator-chosen value is used
    /// as-is.
    pub max_cache_age_secs: Option<u64>,
}
