//! One origin per canister, whichever gateway domain reached it.
//!
//! Internet Identity folds a canister's gateway origin onto `ic0.app` before it
//! derives anything from it, so that a principal does not depend on the domain
//! the user arrived through. That is the spelling it signs into the caller info
//! a content pull carries, so an app configured with the domain it is served on
//! has to read the two the same way.

/// The gateway domains that reach the same canister.
const GATEWAY_DOMAINS: &[&str] = &["ic0.app", "icp0.io", "icp.net"];

/// The one Internet Identity keys an origin by.
const CANONICAL_DOMAIN: &str = "ic0.app";

/// A canister's gateway origin as Internet Identity spells it. Anything else —
/// a custom domain, localhost — is its own origin and stands as it is.
pub fn fold(origin: &str) -> String {
    origin
        .strip_prefix("https://")
        .and_then(|rest| {
            GATEWAY_DOMAINS
                .iter()
                .find_map(|domain| rest.strip_suffix(&format!(".{domain}")))
        })
        .filter(|subdomain| {
            !subdomain.is_empty()
                && subdomain
                    .chars()
                    .all(|character| character.is_ascii_alphanumeric() || character == '-')
        })
        .map_or_else(
            || origin.to_string(),
            |subdomain| format!("https://{subdomain}.{CANONICAL_DOMAIN}"),
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_canister_is_one_origin_on_every_gateway_domain() {
        let folded = "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app";
        for origin in [
            "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
            "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net",
        ] {
            assert_eq!(fold(origin), folded, "{origin}");
        }
    }

    #[test]
    fn anything_that_is_no_gateway_origin_stands_as_it_is() {
        for origin in [
            "https://chat.example.com",
            "https://example.icp0.io.evil.com",
            "http://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
            "https://.ic0.app",
            "https://a.b.icp0.io",
            "http://127.0.0.1:4943",
        ] {
            assert_eq!(fold(origin), origin, "{origin}");
        }
    }
}
