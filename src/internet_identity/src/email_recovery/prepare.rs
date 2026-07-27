//! `email_recovery_credential_prepare_add` — start a new binding flow.
//!
//! Authenticated entry point that validates the FE's input, picks
//! the verification path (DNSSEC if a skeleton bundle is supplied,
//! DoH allowlist otherwise), draws a fresh nonce from the heap PRNG,
//! and parks a `PendingChallenge` keyed by that nonce. Returns the
//! user-visible challenge (nonce + expiry) so the FE can render
//! the "send a magic email" screen.
//!
//! See `docs/ongoing/email-recovery.md` §8.4. Two-phase path picker:
//!
//! - **DNSSEC**: when `dns_proof` is supplied, validate the
//!   *skeleton chain* (root DNSKEY + delegations) synchronously
//!   against the canister's trust anchors and cache the deepest-zone
//!   DNSKEY RRset on the pending challenge. The bundle may also
//!   carry an optional DMARC leaf at `_dmarc.<domain>`; if present
//!   we validate it and cache the TXT bytes. The DKIM leaf is *not*
//!   in this bundle — it lands later via
//!   `email_challenge_submit_dkim_leaf`.
//! - **DoH allowlist**: when `dns_proof` is absent, the registered
//!   domain must be in `DohConfig.allowed_domains`. The DKIM TXT is
//!   resolved via `crate::doh::fetch_txt` at email-arrival time and
//!   verification finishes inside `smtp_request` (no submit-leaf
//!   follow-up).

use crate::email_inbound::prepare::prepare_common;
use crate::email_inbound::{PendingKind, MAX_SESSION_KEY_BYTES, NONCE_PREFIX};
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, SessionKey};

/// Body of the canister method
/// `email_recovery_credential_prepare_add(anchor, dns_input)`.
///
/// `caller_authorized` is the result of the same authentication +
/// activity-tracking check used elsewhere in the canister — only the
/// owner of `anchor` may bind a recovery email to it. The caller has
/// already been validated by `main.rs`; this function only sees the
/// happy-path arguments.
///
/// `now_secs` is `ic_cdk::api::time() / 1_000_000_000`, hoisted out
/// for testability.
pub async fn prepare_add(
    anchor: AnchorNumber,
    dns_input: EmailChallengeDnsInput,
    now_secs: u64,
) -> Result<EmailChallenge, EmailChallengeError> {
    prepare_common(
        dns_input,
        now_secs,
        PendingKind::Register { anchor },
        NONCE_PREFIX,
    )
    .await
}

/// Body of `email_recovery_prepare_delegation(dns_input, session_pk)`.
///
/// Anonymous: anyone can call this. The `session_pk` is a fresh
/// public key the FE just generated; the eventual delegation will be
/// bound to it. The pending challenge holds it until
/// `submit_dkim_leaf` succeeds, at which point the canister stamps a
/// delegation seed and adds the matching canister-signature.
pub async fn prepare_delegation(
    dns_input: EmailChallengeDnsInput,
    session_pk: SessionKey,
    now_secs: u64,
) -> Result<EmailChallenge, EmailChallengeError> {
    // Cap the FE-supplied `session_pk` length. The pending entry
    // holds this for up to 30 minutes; without a bound an open
    // caller could inflate every challenge they prepare. Real
    // session keys are well under 1 KB regardless of algorithm
    // (Ed25519 ~44 bytes, ECDSA P-256 ~91, RSA-2048 ~294).
    if session_pk.len() > MAX_SESSION_KEY_BYTES {
        return Err(EmailChallengeError::InternalCanisterError(format!(
            "session_pk is {} bytes, exceeds the {}-byte limit",
            session_pk.len(),
            MAX_SESSION_KEY_BYTES,
        )));
    }
    prepare_common(
        dns_input,
        now_secs,
        PendingKind::Recover { session_pk },
        NONCE_PREFIX,
    )
    .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::doh::DohConfig;

    fn install_doh_allowlist(domains: &[&str]) {
        crate::state::persistent_state_mut(|p| {
            p.doh_config = Some(DohConfig {
                allowed_domains: domains.iter().map(|d| (*d).to_string()).collect(),
                max_cache_age_secs: None,
            });
        });
    }

    fn doh_input(addr: &str) -> EmailChallengeDnsInput {
        EmailChallengeDnsInput {
            address: addr.to_string(),
            dns_proof: None,
        }
    }

    /// An input carrying a structurally-present but content-bogus DNSSEC
    /// bundle. With the flag off the bundle is dropped before any
    /// validation runs, so its contents don't matter; with the flag on it
    /// reaches `verify_dnssec_skeleton` and fails there.
    fn dnssec_input(addr: &str) -> EmailChallengeDnsInput {
        use internet_identity_interface::internet_identity::types::dnssec::{
            DnsProofBundle, Rrsig, SignedRRset,
        };
        use serde_bytes::ByteBuf;
        let empty_rrset = SignedRRset {
            name: ByteBuf::new(),
            rtype: 0,
            rdata: vec![],
            ttl: 0,
            rrsig: Rrsig {
                type_covered: 0,
                algorithm: 0,
                labels: 0,
                original_ttl: 0,
                expiration: 0,
                inception: 0,
                key_tag: 0,
                signer_name: ByteBuf::new(),
                signature: ByteBuf::new(),
            },
        };
        EmailChallengeDnsInput {
            address: addr.to_string(),
            dns_proof: Some(DnsProofBundle {
                root_dnskey: empty_rrset,
                chains: vec![],
                hops: vec![],
            }),
        }
    }

    fn set_dnssec_flag(enabled: bool) {
        crate::state::persistent_state_mut(|p| p.enable_dnssec_email_recovery = Some(enabled));
    }

    fn block_on<F: std::future::Future>(mut f: F) -> F::Output {
        use std::future::Future;
        use std::pin::Pin;
        use std::sync::Arc;
        use std::task::{Context, Poll, Wake, Waker};
        struct Noop;
        impl Wake for Noop {
            fn wake(self: Arc<Self>) {}
        }
        let mut f = unsafe { Pin::new_unchecked(&mut f) };
        let waker = Waker::from(Arc::new(Noop));
        let mut cx = Context::from_waker(&waker);
        match Future::poll(f.as_mut(), &mut cx) {
            Poll::Ready(out) => out,
            Poll::Pending => panic!("test future returned Pending"),
        }
    }

    #[test]
    fn rejects_non_allowlisted_domain() {
        crate::email_inbound::pending::reset_for_tests();
        install_doh_allowlist(&["gmail.com"]);
        let result = block_on(prepare_add(1, doh_input("alice@example.com"), 100));
        match result {
            Err(EmailChallengeError::DomainNotAllowlisted(d)) => assert_eq!(d, "example.com"),
            other => panic!("expected DomainNotAllowlisted, got {other:?}"),
        }
    }

    #[test]
    fn rejects_when_doh_config_unset() {
        crate::email_inbound::pending::reset_for_tests();
        crate::state::persistent_state_mut(|p| p.doh_config = None);
        let result = block_on(prepare_add(1, doh_input("alice@gmail.com"), 100));
        assert!(matches!(
            result,
            Err(EmailChallengeError::DomainNotAllowlisted(_))
        ));
    }

    #[test]
    fn flag_off_drops_proof_and_takes_doh_path() {
        // Flag off (the deploy default): a present `dns_proof` is ignored
        // and the allowlist gate decides. A non-allowlisted domain is
        // rejected with DomainNotAllowlisted — never reaching the DNSSEC
        // verifier (which, on this bogus bundle, would fail differently).
        crate::email_inbound::pending::reset_for_tests();
        set_dnssec_flag(false);
        install_doh_allowlist(&["gmail.com"]);
        let result = block_on(prepare_add(1, dnssec_input("alice@example.com"), 100));
        match result {
            Err(EmailChallengeError::DomainNotAllowlisted(d)) => assert_eq!(d, "example.com"),
            other => panic!("expected DomainNotAllowlisted, got {other:?}"),
        }
    }

    #[test]
    fn flag_off_with_proof_succeeds_via_doh_when_allowlisted() {
        // Strongest form: a proof is present *and* the domain is
        // allowlisted. The flag being off means we take the DoH path and
        // succeed, proving the bogus proof was never validated.
        crate::email_inbound::pending::reset_for_tests();
        crate::email_inbound::rng::tests::seed_for_tests([9u8; 32]);
        set_dnssec_flag(false);
        install_doh_allowlist(&["gmail.com"]);
        let result = block_on(prepare_add(1, dnssec_input("alice@gmail.com"), 100));
        assert!(result.is_ok(), "expected DoH success, got {result:?}");
    }

    #[test]
    fn flag_on_routes_proof_to_dnssec_path() {
        // Flag on re-enables the DNSSEC path: a present proof reaches
        // `verify_dnssec_skeleton`. With no trust anchors configured it
        // fails there with the dedicated message — distinct from the DoH
        // allowlist rejection, proving the DNSSEC branch was taken.
        crate::email_inbound::pending::reset_for_tests();
        set_dnssec_flag(true);
        crate::state::persistent_state_mut(|p| p.dnssec_config = None);
        let result = block_on(prepare_add(1, dnssec_input("alice@example.com"), 100));
        match result {
            Err(EmailChallengeError::DomainNotSupported(msg)) => {
                assert!(msg.contains("trust anchors"), "unexpected message: {msg}")
            }
            other => panic!("expected DomainNotSupported (DNSSEC path), got {other:?}"),
        }
    }

    #[test]
    fn rejects_obviously_malformed_address() {
        crate::email_inbound::pending::reset_for_tests();
        install_doh_allowlist(&["gmail.com"]);
        for bad in &[
            "",
            " ",
            "no-at-sign",
            "@nolocal.com",
            "nolocal@",
            "a b@c.com",
        ] {
            let result = block_on(prepare_add(1, doh_input(bad), 100));
            assert!(
                matches!(result, Err(EmailChallengeError::InvalidEmailAddress(_))),
                "expected InvalidEmailAddress for {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn rejects_oversized_address() {
        // Addresses longer than RFC 5321's effective addr-spec cap
        // (254 bytes) or with local/domain parts past their own
        // sub-caps (64 / 255) are rejected at prepare time so the
        // pending-challenge map can't be inflated.
        crate::email_inbound::pending::reset_for_tests();
        install_doh_allowlist(&["example.com"]);

        let oversized_total = format!("{}@example.com", "a".repeat(250));
        let oversized_local = format!("{}@example.com", "a".repeat(65));
        let oversized_domain = format!("alice@{}.com", "b".repeat(252));

        for bad in &[
            oversized_total.as_str(),
            oversized_local.as_str(),
            oversized_domain.as_str(),
        ] {
            let result = block_on(prepare_add(1, doh_input(bad), 100));
            assert!(
                matches!(result, Err(EmailChallengeError::InvalidEmailAddress(_))),
                "expected InvalidEmailAddress for oversized {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn happy_path_returns_a_well_formed_challenge() {
        crate::email_inbound::pending::reset_for_tests();
        crate::email_inbound::rng::tests::seed_for_tests([7u8; 32]);
        install_doh_allowlist(&["gmail.com"]);

        let result = block_on(prepare_add(42, doh_input("Alice@Gmail.COM"), 1_000));
        let challenge = result.expect("expected Ok");

        assert!(challenge.nonce.starts_with(super::NONCE_PREFIX));
        // expires_at is in nanoseconds since epoch; we passed
        // now_secs = 1_000.
        assert_eq!(
            challenge.expires_at,
            (1_000 + crate::email_inbound::CHALLENGE_TTL_SECS) * 1_000_000_000
        );

        // The pending entry stores the lowercased address (input was
        // mixed case) and the registered domain.
        crate::email_inbound::pending::with_mut(&challenge.nonce, 1_001, |c| {
            assert_eq!(c.claimed_address, "alice@gmail.com");
            assert_eq!(c.registered_domain, "gmail.com");
            matches!(
                c.kind,
                crate::email_inbound::pending::PendingKind::Register { anchor: 42 }
            );
        })
        .expect("entry should exist");
    }

    #[test]
    fn allowlist_match_is_case_insensitive() {
        crate::email_inbound::pending::reset_for_tests();
        crate::email_inbound::rng::tests::seed_for_tests([3u8; 32]);
        // Operator stored the domain in caps; user typed it lowercase.
        // We accept either side mixed-case.
        install_doh_allowlist(&["GMAIL.COM"]);
        let result = block_on(prepare_add(1, doh_input("alice@gmail.com"), 1_000));
        assert!(result.is_ok(), "expected Ok, got {result:?}");
    }
}
