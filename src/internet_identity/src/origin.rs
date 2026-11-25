//! The origin an application is identified by, in the one spelling it is keyed under.

use crate::delegation::frontend_length_within_limit;
use crate::state;
use crate::utils::is_loopback_host;
use internet_identity_interface::internet_identity::types::attributes::remap_to_legacy_domain;
use std::fmt;
use url::Url;

/// An origin identifying an application: a bare, length-bounded `scheme://host[:port]`,
/// folded onto the gateway spelling principals are derived against.
///
/// The folding is what makes one application one row. A canister reached at
/// `<id>.icp0.io` and at `<id>.ic0.app` is one site, and a value that kept them apart
/// would give it two of everything it is keyed by.
///
/// Built only through its `TryFrom`, so a function taking one cannot be handed a
/// spelling nobody folded.
#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct ApplicationOrigin(String);

impl ApplicationOrigin {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ApplicationOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl TryFrom<&str> for ApplicationOrigin {
    type Error = String;

    /// Accepts a length-bounded, bare `scheme://host[:port]`. Anything else is rejected
    /// rather than trimmed, so one application cannot hold two rows.
    fn try_from(origin: &str) -> Result<Self, Self::Error> {
        if origin.is_empty() {
            return Err("origin is empty".to_string());
        }
        // Before parsing, so an enormous string is refused without being parsed.
        frontend_length_within_limit(&origin.to_string())?;

        let Ok(url) = Url::parse(origin) else {
            return Err("origin is not a URL".to_string());
        };
        // One comparison against the browser's own serialization, so credentials, path,
        // query, fragment, trailing slash, an explicit default port and uppercase host
        // all fail here. The scheme is not restricted: an origin this canister already
        // signs in at is one it can be asked about.
        if url.origin().ascii_serialization() != origin {
            return Err("origin must be a bare scheme://host[:port]".to_string());
        }
        Ok(Self(remap_to_legacy_domain(origin)))
    }
}

/// The canister gateway domains an origin may name.
const GATEWAY_DOMAINS: &[&str] = &["ic0.app", "icp0.io", "icp.net"];

/// The gateway every canister is served through, so one request settles the question.
const FETCH_DOMAIN: &str = "icp.net";

/// The label a gateway origin carries between the canister id and the gateway domain
/// when it names the uncertified endpoint, as in `<id>.raw.icp0.io`.
const RAW_LABEL: &str = ".raw";

/// Where a file the application at an [`ApplicationOrigin`] publishes under
/// `/.well-known` is read from.
///
/// An [`ApplicationOrigin`] is folded to `ic0.app`, a spelling the canister need not be
/// served on: a boundary node answers `400 client_domain_canister_mismatch` where it is
/// not, and the file would never be read. The gateway domains resolve to the same
/// canister and serve the same file, so a gateway origin `https://<id>.<gateway>` or
/// `https://<id>.raw.<gateway>` is read as `https://<id>.icp.net`: through
/// [`FETCH_DOMAIN`], and never through the raw endpoint, whose responses carry no
/// certification. Anything else, a custom domain or localhost, is read as it stands.
///
/// Built only from an [`ApplicationOrigin`] II may fetch from, so it is always a bare
/// origin whose file cannot be replaced in flight: `https`, or `http` on a loopback host
/// while the deployment allows insecure sender lists. Build one per fetch: the flag is
/// persistent state, so a value kept across a configuration change would no longer
/// hold.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FetchableApplicationOrigin(String);

impl FetchableApplicationOrigin {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<&ApplicationOrigin> for FetchableApplicationOrigin {
    type Error = String;

    fn try_from(origin: &ApplicationOrigin) -> Result<Self, Self::Error> {
        let origin = origin.as_str();
        let Ok(url) = Url::parse(origin) else {
            return Err("origin is not a URL".to_string());
        };
        let is_insecure_fetch_allowed = url.scheme() == "http"
            && is_insecure_sender_list_allowed()
            && is_loopback_host(url.host_str().unwrap_or_default());
        if url.scheme() != "https" && !is_insecure_fetch_allowed {
            return Err("origin must be https".to_string());
        }
        let canister_id = origin
            .strip_prefix("https://")
            .and_then(|rest| {
                GATEWAY_DOMAINS
                    .iter()
                    .find_map(|domain| rest.strip_suffix(&format!(".{domain}")))
            })
            .map(|subdomain| subdomain.strip_suffix(RAW_LABEL).unwrap_or(subdomain))
            .filter(|canister_id| {
                !canister_id.is_empty()
                    && canister_id
                        .chars()
                        .all(|character| character.is_ascii_alphanumeric() || character == '-')
            });
        match canister_id {
            Some(canister_id) => Ok(Self(format!("https://{canister_id}.{FETCH_DOMAIN}"))),
            None => Ok(Self(origin.to_string())),
        }
    }
}

fn is_insecure_sender_list_allowed() -> bool {
    state::persistent_state(|s| s.notifications_allow_insecure_sender_list).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::FRONTEND_HOSTNAME_LIMIT;

    fn parsed(origin: &str) -> Result<String, String> {
        ApplicationOrigin::try_from(origin).map(|origin| origin.as_str().to_string())
    }

    #[test]
    fn a_bare_origin_is_taken_as_written() {
        assert_eq!(
            parsed("https://example.com"),
            Ok("https://example.com".into())
        );
        assert_eq!(
            parsed("https://example.com:8443"),
            Ok("https://example.com:8443".into())
        );
        assert_eq!(
            parsed("http://localhost:8000"),
            Ok("http://localhost:8000".into())
        );
    }

    /// One comparison against the browser's own serialization covers all of these, so
    /// they are one rule rather than six.
    #[test]
    fn anything_that_is_not_a_bare_origin_is_refused() {
        for spelling in [
            "",
            "example.com",
            "https://example.com/",
            "https://example.com/path",
            "https://example.com?q=1",
            "https://example.com#f",
            "https://user:pw@example.com",
            "https://EXAMPLE.com",
            "https://example.com:443",
        ] {
            assert!(parsed(spelling).is_err(), "{spelling} should be refused");
        }
    }

    /// The gateways are three spellings of one site, and the application they name has
    /// to be one row however the user arrived at it.
    #[test]
    fn every_gateway_spelling_folds_onto_one() {
        let folded = "https://aaaaa-aa.ic0.app".to_string();
        assert_eq!(parsed("https://aaaaa-aa.icp0.io"), Ok(folded.clone()));
        assert_eq!(parsed("https://aaaaa-aa.icp.net"), Ok(folded.clone()));
        assert_eq!(parsed("https://aaaaa-aa.ic0.app"), Ok(folded));
        assert_eq!(
            parsed("https://aaaaa-aa.raw.icp0.io"),
            Ok("https://aaaaa-aa.raw.ic0.app".into())
        );
    }

    /// Folding is idempotent, so what comes out goes back in unchanged.
    #[test]
    fn what_it_answers_parses_back_to_itself() {
        let once =
            ApplicationOrigin::try_from("https://aaaaa-aa.icp0.io").expect("a gateway origin");
        let twice = ApplicationOrigin::try_from(once.as_str()).expect("its own answer");
        assert_eq!(once, twice);
    }

    /// A browser never serializes userinfo into an origin, and each of these parses far
    /// enough to look like an authority without being one a browser can hand an app.
    #[test]
    fn rejects_an_authority_no_browser_can_serialize() {
        for spelling in [
            "https://user:pass@app.example",
            "https://@app.example",
            // Out of range for a port, which is a u16.
            "https://app.example:99999",
            // Not a host: a space cannot appear in one.
            "https://not a host",
            "https://",
            "https://:443",
            "https://app.example:",
            "https://app.example:https",
            // The default port for the scheme, which a browser drops.
            "http://app.example:80",
        ] {
            assert!(parsed(spelling).is_err(), "{spelling} should be refused");
        }
    }

    #[test]
    fn rejects_an_over_long_origin() {
        let too_long = format!("https://{}", "a".repeat(FRONTEND_HOSTNAME_LIMIT));
        assert!(parsed(&too_long).is_err());
    }

    /// The scheme is the application's, not the channel anything travels on: this
    /// canister signs in at these.
    #[test]
    fn accepts_an_origin_that_is_not_https() {
        assert_eq!(
            parsed("http://127.0.0.1:8080"),
            Ok("http://127.0.0.1:8080".into())
        );
    }

    /// A site that is not on a canister gateway is left exactly as it was.
    #[test]
    fn an_origin_off_the_gateways_is_left_alone() {
        assert_eq!(parsed("https://ic0.app"), Ok("https://ic0.app".into()));
        assert_eq!(
            parsed("https://not-a-gateway.com"),
            Ok("https://not-a-gateway.com".into())
        );
    }

    fn allow_insecure(allow: bool) {
        state::persistent_state_mut(|s| {
            s.notifications_allow_insecure_sender_list = Some(allow);
        });
    }

    fn fetchable(origin: &str) -> Result<String, String> {
        let origin = ApplicationOrigin::try_from(origin).expect("a bare origin");
        FetchableApplicationOrigin::try_from(&origin).map(|origin| origin.as_str().to_string())
    }

    fn fetched(origin: &str) -> String {
        fetchable(origin).expect("a fetchable origin")
    }

    #[test]
    fn a_gateway_origin_is_read_through_the_gateway_that_serves_every_canister() {
        for origin in [
            "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net",
        ] {
            assert_eq!(
                fetched(origin),
                "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net",
                "{origin}"
            );
        }
    }

    /// The raw endpoint serves responses without certification, so it is never read.
    #[test]
    fn a_raw_gateway_origin_is_read_through_the_certified_gateway() {
        for origin in [
            "https://vt36r-2qaaa-aaaad-aad5a-cai.raw.ic0.app",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.raw.icp0.io",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.raw.icp.net",
        ] {
            assert_eq!(
                fetched(origin),
                "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net",
                "{origin}"
            );
        }
    }

    #[test]
    fn anything_that_is_no_gateway_origin_is_read_as_it_stands() {
        allow_insecure(true);
        for origin in [
            "https://app.example",
            "https://nice-name.com",
            "https://raw.example",
            "http://127.0.0.1:4943",
            // A gateway domain with nothing before it names no canister.
            "https://icp.net",
            // Plain http is no gateway, whatever the host.
            "http://vt36r-2qaaa-aaaad-aad5a-cai.localhost:4943",
        ] {
            assert_eq!(fetched(origin), origin);
        }
    }

    /// The scheme decides where II sends an outcall, so a file that would be
    /// fetched over plain http is refused however the origin is spelled.
    #[test]
    fn refuses_an_origin_whose_file_would_not_be_fetched_over_https() {
        allow_insecure(false);
        for origin in [
            "http://app.example",
            "http://localhost:5173",
            "http://127.0.0.1:8080",
        ] {
            assert_eq!(
                fetchable(origin),
                Err("origin must be https".to_string()),
                "{origin} must not be fetched"
            );
        }
        assert_eq!(
            fetchable("https://app.example"),
            Ok("https://app.example".to_string())
        );
    }

    /// The flag reaches loopback only: a public host over http stays refused,
    /// so a flagged deployment still cannot be pointed at a plaintext app.
    #[test]
    fn the_insecure_flag_reaches_loopback_only() {
        allow_insecure(true);
        for origin in [
            "http://localhost:5173",
            "http://127.0.0.1:8080",
            "http://localhost",
        ] {
            assert_eq!(fetchable(origin), Ok(origin.to_string()));
        }
        assert!(fetchable("http://app.example").is_err());
    }

    /// And it reaches `http` only. A scheme II cannot fetch would fail at the
    /// outcall anyway, but the rule is what the type states, not where the
    /// call happens to break.
    #[test]
    fn the_insecure_flag_reaches_http_only() {
        allow_insecure(true);
        for origin in ["ftp://localhost", "ws://localhost:5173", "wss://localhost"] {
            assert!(fetchable(origin).is_err(), "{origin} must not be fetched");
        }
    }
}
