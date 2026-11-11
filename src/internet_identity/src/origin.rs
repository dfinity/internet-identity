//! The origin an application is identified by, in the one spelling it is keyed under.

use crate::delegation::frontend_length_within_limit;
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
}
