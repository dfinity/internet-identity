use crate::utils::is_loopback_host;
use candid::{CandidType, Deserialize};
use std::fmt;

/// The most bytes a domain may have before it is parsed.
pub const MAX_DOMAIN_LENGTH: usize = 255;

/// The most characters in a DNS name, and in one of its labels.
const MAX_DNS_NAME_LENGTH: usize = 253;
const MAX_DNS_LABEL_LENGTH: usize = 63;

/// The most characters of an input an error message quotes.
const MAX_QUOTED_LENGTH: usize = 100;

/// An organization's domain: a DNS name, or a loopback host with an optional
/// port for a local mock provider, lowercased.
///
/// Built with [`TryFrom<&str>`], which trims and lowercases the value and
/// names every problem with it. It is interpolated into the discovery URL
/// (`{scheme}://{domain}/.well-known/...`), so the checks are the security
/// boundary: userinfo (`evil.com@127.0.0.1`), a path, a query, or a fragment
/// would otherwise change the request target.
///
/// Serializes as its text. Deserializing does not check it again: only state
/// this canister wrote, from a checked value, is read back.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, CandidType, Deserialize)]
#[serde(transparent)]
pub struct Domain(String);

impl Domain {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// A domain read back from storage, which was built with
    /// [`TryFrom<&str>`] when it was written. It is not checked again, so a
    /// value stored under earlier rules still loads.
    pub fn from_stored(domain: String) -> Self {
        Self(domain)
    }
}

impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<Domain> for String {
    fn from(domain: Domain) -> Self {
        domain.0
    }
}

impl TryFrom<&str> for Domain {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, String> {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            return Err("the domain is empty".to_string());
        }
        if trimmed.len() > MAX_DOMAIN_LENGTH {
            return Err(format!(
                "domain {} exceeds {MAX_DOMAIN_LENGTH} bytes",
                quote(trimmed)
            ));
        }
        let lowered = trimmed.to_ascii_lowercase();
        let (scheme, rest) = split_scheme(&lowered);
        let Ok(url) = url::Url::parse(&format!("https://{rest}")) else {
            return Err(format!("domain {} is not a domain", quote(trimmed)));
        };
        let (username, password, path, query, fragment, host, port) = (
            url.username(),
            url.password(),
            url.path(),
            url.query(),
            url.fragment(),
            url.host_str(),
            url.port(),
        );
        // The parser drops an empty or default port such as `:443`, so the typed
        // one is read from the input itself.
        let typed_authority = rest.split(['/', '?', '#']).next().unwrap_or_default();
        let typed_host_port = typed_authority
            .rsplit_once('@')
            .map_or(typed_authority, |(_, host_port)| host_port);
        let typed_port = typed_host_port
            .rsplit_once(':')
            .map(|(_, port)| port)
            .filter(|port| port.bytes().all(|b| b.is_ascii_digit()));

        let mut problems = vec![];
        if scheme {
            problems.push("a scheme".to_string());
        }
        if !username.is_empty() || password.is_some() {
            problems.push("userinfo".to_string());
        }
        if !matches!(path, "" | "/") {
            problems.push("a path".to_string());
        }
        if query.is_some() {
            problems.push("a query".to_string());
        }
        if fragment.is_some() {
            problems.push("a fragment".to_string());
        }
        let Some(host) = host else {
            problems.push("no host".to_string());
            return Err(format!(
                "domain {} has {}",
                quote(trimmed),
                listed(&problems)
            ));
        };
        let loopback = is_loopback_host(host);
        match typed_port {
            Some(_) if !loopback => {
                problems.push("a port, which only loopback hosts may carry".to_string())
            }
            Some(typed) if port.map(|p| p.to_string()).as_deref() != Some(typed) => {
                problems.push("an empty or default port".to_string())
            }
            _ => {}
        }
        if !loopback {
            problems.extend(dns_name_problems(host));
        }
        let authority = match port {
            Some(port) => format!("{host}:{port}"),
            None => host.to_string(),
        };
        // Anything the parser rewrote on the way (stripped tabs or newlines,
        // percent-decoding, IDNA mapping) would make the value fetched differ
        // from the value checked.
        if problems.is_empty() && authority != rest {
            problems.push("characters a URL parser rewrites".to_string());
        }
        if !problems.is_empty() {
            return Err(format!(
                "domain {} has {}",
                quote(trimmed),
                listed(&problems)
            ));
        }
        Ok(Self(authority))
    }
}

/// Whether `value` starts with `<scheme>://`, and what follows it.
fn split_scheme(value: &str) -> (bool, &str) {
    match value.split_once("://") {
        Some((scheme, rest))
            if scheme.starts_with(|c: char| c.is_ascii_alphabetic())
                && scheme
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || matches!(c, '+' | '.' | '-')) =>
        {
            (true, rest)
        }
        _ => (false, value),
    }
}

/// What keeps `host` from being a DNS name by IDNA's strict rules (letters,
/// digits and hyphens, no hyphen at either end of a label, `--` in the third
/// and fourth places only for punycode, DNS lengths), plus at least two labels.
fn dns_name_problems(host: &str) -> Vec<String> {
    let labels: Vec<&str> = host.split('.').collect();
    let mut problems = vec![];
    if labels.len() < 2 {
        problems.push("a single label".to_string());
    }
    if labels.iter().any(|label| label.is_empty()) {
        problems.push("an empty label".to_string());
    }
    if labels
        .iter()
        .any(|label| label.len() > MAX_DNS_LABEL_LENGTH)
    {
        problems.push(format!(
            "a label longer than {MAX_DNS_LABEL_LENGTH} characters"
        ));
    }
    if labels.iter().any(|label| {
        !label
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'-')
    }) {
        problems.push("a character other than a letter, digit, or hyphen".to_string());
    }
    if labels
        .iter()
        .any(|label| label.starts_with('-') || label.ends_with('-'))
    {
        problems.push("a label starting or ending with a hyphen".to_string());
    }
    if labels
        .iter()
        .any(|label| label.get(2..4) == Some("--") && !label.starts_with("xn--"))
    {
        problems.push("a label with \"--\" in its third and fourth places".to_string());
    }
    if host.len() > MAX_DNS_NAME_LENGTH {
        problems.push(format!(
            "a name longer than {MAX_DNS_NAME_LENGTH} characters"
        ));
    }
    if problems.is_empty() && idna::domain_to_ascii_strict(host).map_or(true, |ascii| ascii != host)
    {
        problems.push("a name that is not a valid DNS name".to_string());
    }
    problems
}

/// `value` for an error message: cut short, and quoted so control characters
/// show escaped.
fn quote(value: &str) -> String {
    match value.char_indices().nth(MAX_QUOTED_LENGTH) {
        Some((cut, _)) => format!("{:?}", format!("{}…", &value[..cut])),
        None => format!("{value:?}"),
    }
}

/// `["a", "b", "c"]` as `a, b and c`.
fn listed(items: &[String]) -> String {
    match items.split_last() {
        Some((last, rest)) if !rest.is_empty() => format!("{} and {last}", rest.join(", ")),
        Some((last, _)) => last.clone(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn domain(value: &str) -> Result<String, String> {
        Domain::try_from(value).map(String::from)
    }

    #[test]
    fn a_domain_is_trimmed_and_lowercased() {
        assert_eq!(domain("  Example.COM  "), Ok("example.com".to_string()));
        assert_eq!(domain("sub.example.com"), Ok("sub.example.com".to_string()));
        assert_eq!(
            domain("sso-2.example.com"),
            Ok("sso-2.example.com".to_string())
        );
        assert_eq!(
            domain("xn--zrich-kva.example"),
            Ok("xn--zrich-kva.example".to_string())
        );
    }

    #[test]
    fn a_loopback_host_may_carry_a_port() {
        for value in [
            "localhost:11107",
            "127.0.0.1:8080",
            "app.localhost:8000",
            "localhost",
        ] {
            assert_eq!(domain(value), Ok(value.to_string()), "{value}");
        }
    }

    #[test]
    fn anything_but_a_bare_domain_is_rejected() {
        // The caller-controlled value must not reshape the interpolated
        // discovery URL (`{scheme}://{domain}/.well-known/...`).
        for bad in [
            "evil.com@127.0.0.1",    // userinfo: the real host is 127.0.0.1
            "user:pass@example.com", // userinfo
            "example.com/..",        // path traversal
            "example.com/foo",       // path
            "example.com?x=1",       // query
            "example.com#frag",      // fragment
            "https://example.com",   // injected scheme
            "example.com:443",       // a default port (normalized away)
            "example.com:8443",      // a port on a non-loopback host
            "localhost:443",         // a default port on loopback
            "localhost:",            // an empty port
            "exa mple.com",          // whitespace in host
            "exa\tmple.com",         // a tab the parser strips
            "exa%6dple.com",         // percent-encoding the parser decodes
            "example",               // a single label
            ".com",                  // empty label
            "example..com",          // empty label
            "-example.com",          // leading hyphen
            "example-.com",          // trailing hyphen
            "exa_mple.com",          // underscore
            "ab--cd.com",            // hyphens in the third and fourth places
            "",                      // empty
        ] {
            assert!(domain(bad).is_err(), "expected {bad:?} to be rejected");
        }
    }

    #[test]
    fn every_problem_is_named() {
        assert_eq!(
            domain("https://user@acme.com:8443/sso?x=1#y"),
            Err("domain \"https://user@acme.com:8443/sso?x=1#y\" has a scheme, userinfo, a path, a query, a fragment and a port, which only loopback hosts may carry".to_string())
        );
        assert_eq!(
            domain("-a--cd..org"),
            Err("domain \"-a--cd..org\" has an empty label, a label starting or ending with a hyphen and a label with \"--\" in its third and fourth places".to_string())
        );
    }

    #[test]
    fn the_error_quotes_at_most_100_characters() {
        let long = format!("{}/x", "a".repeat(150));
        assert_eq!(
            domain(&long),
            Err(format!(
                "domain \"{}…\" has a path, a single label and a label longer than 63 characters",
                "a".repeat(100)
            ))
        );
    }

    #[test]
    fn over_long_domains_are_rejected() {
        // DNS allows at most 253 characters in a name.
        let labels = vec!["a".repeat(62); 3].join(".");
        let longest = format!("{labels}.{}.com", "a".repeat(60));
        assert_eq!(longest.len(), 253);
        assert_eq!(domain(&longest), Ok(longest.clone()));
        assert!(domain(&format!("b{longest}")).is_err());

        let over_cap = format!("b{}.com", vec!["a".repeat(62); 4].join("."));
        assert_eq!(over_cap.len(), MAX_DOMAIN_LENGTH + 1);
        assert!(domain(&over_cap).is_err());
    }
}
