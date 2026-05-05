//! Domain alignment check for DMARC.
//!
//! Per design doc §6.3 we accept two cases:
//!
//! - **Strict** (`adkim=s`): the DKIM `d=` must equal the From-header
//!   domain byte-for-byte after ASCII-lowercasing.
//! - **Relaxed** (`adkim=r`): the two domains must equal *or* one must
//!   be a strict subdomain of the other (label-aligned suffix).
//!
//! This is *stricter* than RFC-7489-compliant relaxed alignment, which
//! uses the Public Suffix List to compute the "organizational domain"
//! and accepts e.g. `gmail.com` ↔ `googlemail.com` if both are listed
//! as the same org. We deliberately don't consult the PSL — design
//! doc §6.4 documents the trust + asymmetric-failure-mode reasoning.
//! In exchange we fail closed on multi-domain orgs (we reject
//! `googlemail.com` + `From: gmail.com` mail), which is the safe
//! direction for an account-recovery surface.
//!
//! Both inputs are expected to already be ASCII-lowercased.

use super::types::AlignmentMode;

/// Returns true iff the DKIM `d=` aligns with the From-header domain
/// under `mode`.
pub fn aligns(dkim_domain: &str, from_domain: &str, mode: AlignmentMode) -> bool {
    if dkim_domain == from_domain {
        return true;
    }
    if matches!(mode, AlignmentMode::Strict) {
        return false;
    }
    is_subdomain_of(dkim_domain, from_domain) || is_subdomain_of(from_domain, dkim_domain)
}

/// Returns true iff `child` is a strict subdomain of `parent` — i.e.
/// `child == "<labels>." + parent`. We anchor on the dot so a suffix
/// match like `evilexample.com` ↔ `example.com` does **not** align.
fn is_subdomain_of(child: &str, parent: &str) -> bool {
    if child.len() <= parent.len() {
        return false;
    }
    if !child.ends_with(parent) {
        return false;
    }
    let dot_pos = child.len() - parent.len() - 1;
    child.as_bytes()[dot_pos] == b'.'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strict_exact_match() {
        assert!(aligns("example.com", "example.com", AlignmentMode::Strict));
    }

    #[test]
    fn strict_subdomain_does_not_align() {
        assert!(!aligns(
            "mail.example.com",
            "example.com",
            AlignmentMode::Strict
        ));
        assert!(!aligns(
            "example.com",
            "mail.example.com",
            AlignmentMode::Strict
        ));
    }

    #[test]
    fn relaxed_exact_match() {
        assert!(aligns("example.com", "example.com", AlignmentMode::Relaxed));
    }

    #[test]
    fn relaxed_dkim_subdomain_of_from() {
        assert!(aligns(
            "mail.example.com",
            "example.com",
            AlignmentMode::Relaxed
        ));
    }

    #[test]
    fn relaxed_from_subdomain_of_dkim() {
        // The opposite direction (sending service signs as the
        // registered domain on behalf of a sub-domain of itself) is
        // valid per spec. We accept both directions in relaxed mode.
        assert!(aligns(
            "example.com",
            "mail.example.com",
            AlignmentMode::Relaxed
        ));
    }

    #[test]
    fn relaxed_evil_suffix_does_not_align() {
        // The whole point of the dot anchor: a suffix match without a
        // label boundary must NOT align.
        assert!(!aligns(
            "evilexample.com",
            "example.com",
            AlignmentMode::Relaxed
        ));
        assert!(!aligns(
            "example.com",
            "evilexample.com",
            AlignmentMode::Relaxed
        ));
    }

    #[test]
    fn relaxed_unrelated_does_not_align() {
        assert!(!aligns(
            "gmail.com",
            "googlemail.com",
            AlignmentMode::Relaxed
        ));
        assert!(!aligns(
            "example.com",
            "evil.com",
            AlignmentMode::Relaxed
        ));
    }

    #[test]
    fn relaxed_deep_subdomain_aligns() {
        assert!(aligns(
            "deep.nested.example.com",
            "example.com",
            AlignmentMode::Relaxed
        ));
    }
}
