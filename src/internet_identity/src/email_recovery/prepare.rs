//! `email_recovery_credential_prepare_add` — start a new binding flow.
//!
//! Authenticated entry point that validates the FE's input, makes
//! sure the registered domain is on the DoH allowlist, draws a fresh
//! nonce from the heap PRNG, and parks a `PendingChallenge` keyed by
//! that nonce. Returns the user-visible challenge (nonce + mailbox +
//! expiry) so the FE can render the "send a magic email" screen.
//!
//! See `docs/ongoing/email-recovery.md` §8.4. The DNSSEC path lands
//! in a follow-up PR (it adds an optional `dns_proof` field on
//! `EmailRecoveryDnsInput` that the canister validates synchronously
//! when present, falling through to the DoH allowlist check below
//! when absent).

use super::pending::{insert_with_eviction, PendingChallenge, PendingKind, PendingStatus};
use super::rng::{draw_nonce_bytes, ensure_seeded, format_nonce};
use crate::state;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryChallenge, EmailRecoveryDnsInput, EmailRecoveryError,
};
use internet_identity_interface::internet_identity::types::AnchorNumber;

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
    dns_input: EmailRecoveryDnsInput,
    now_secs: u64,
) -> Result<EmailRecoveryChallenge, EmailRecoveryError> {
    let EmailRecoveryDnsInput { address, selector } = dns_input;

    // Sanity check the address shape. Detailed RFC 5321/5322 validation
    // is the verifier's job; here we just want to fail fast on the
    // obviously-malformed inputs (no `@`, leading/trailing whitespace,
    // empty parts) so the FE doesn't get back an opaque rejection
    // half a flow later.
    let address = normalize_address(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;
    let registered_domain = registered_domain_of(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;

    // Path picker. In this PR only the DoH-allowlist path is wired
    // up, so we check the allowlist directly. The DNSSEC follow-up
    // PR adds an optional `dns_proof` field on `EmailRecoveryDnsInput`
    // — when supplied, this branch shifts to "validate chain and
    // use it"; the allowlist check below moves to the `else` arm.
    //
    // The FE doesn't need to know which path was picked; it just
    // gets a clean nonce or a clean error.
    let allowlisted = state::persistent_state(|p| {
        p.doh_config
            .as_ref()
            .map(|c| {
                c.allowed_domains
                    .iter()
                    .any(|d| d.eq_ignore_ascii_case(&registered_domain))
            })
            .unwrap_or(false)
    });
    if !allowlisted {
        // Use the dedicated variant so the FE can distinguish "this
        // domain isn't on the operator's DoH allowlist" (an
        // operator-config issue) from `DomainNotSupported` (which
        // is reserved for genuinely unsupported domain shapes).
        // Once the DNSSEC path lands, the choice between
        // `DomainNotAllowlisted` and `DomainNotSupported` will key
        // on whether DNSSEC was attempted-but-failed vs not
        // attempted at all.
        return Err(EmailRecoveryError::DomainNotAllowlisted(registered_domain));
    }

    // Selector sanity. DKIM selectors are themselves DNS labels (RFC
    // 6376 §3.6.2.1), so they share the label-validity rules. We
    // intentionally don't probe DoH here — the selector is unverified
    // until the inbound email lands in `smtp_request`, and the
    // `SelectorMismatch` error covers the "FE guessed wrong" case.
    if selector.is_empty()
        || selector.len() > 63
        || !selector
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        return Err(EmailRecoveryError::DomainNotSupported(format!(
            "invalid DKIM selector: {selector:?}"
        )));
    }

    // Now we can mutate state. The async raw_rand fetch happens at
    // most once per canister lifetime (see `rng::ensure_seeded`).
    ensure_seeded().await;

    // Loop guards against the (vanishingly improbable) event of a
    // nonce collision against an existing pending entry. With 64
    // bits of entropy and at most 10 000 in-flight entries, the
    // collision probability per draw is < 2^-50 — but the cost of
    // looping is one extra PRNG draw, so we do it anyway.
    let mut attempts = 0;
    let nonce = loop {
        attempts += 1;
        let candidate = format_nonce(&draw_nonce_bytes());
        let collision = super::pending::with_mut(&candidate, now_secs, |_| ()).is_some();
        if !collision {
            break candidate;
        }
        if attempts > 8 {
            // Should never trigger in practice. If it does, something
            // is very wrong with the PRNG (or the caller is doing
            // something pathological); abort rather than spin.
            return Err(EmailRecoveryError::InternalCanisterError(
                "exhausted nonce collision retries".into(),
            ));
        }
    };

    let challenge = PendingChallenge {
        kind: PendingKind::Register { anchor },
        claimed_address: address,
        selector,
        created_at_secs: now_secs,
        status: PendingStatus::Pending,
    };
    insert_with_eviction(nonce.clone(), challenge, now_secs);

    Ok(EmailRecoveryChallenge {
        nonce,
        mailbox: super::SETUP_MAILBOX.into(),
        // `Timestamp` is nanoseconds since epoch in this crate (see
        // `internet_identity_interface::types`). We work in seconds
        // internally for the TTL math and convert at the wire boundary.
        expires_at: now_secs
            .saturating_add(super::CHALLENGE_TTL_SECS)
            .saturating_mul(1_000_000_000),
    })
}

/// Normalise an email address to the canonical `lowercase(local) +
/// "@" + lowercase(domain)` form. Returns `None` if the address is
/// obviously malformed (no `@`, empty local-part, empty domain,
/// embedded whitespace).
///
/// Stricter normalisation (IDN, RFC 5322 quoted local-parts) is the
/// mail-auth verifier's job — here we just want to reject the
/// "definitely not an email" cases before issuing a nonce.
fn normalize_address(input: &str) -> Option<String> {
    let trimmed = input.trim();
    if trimmed.is_empty() || trimmed.contains(char::is_whitespace) {
        return None;
    }
    let (local, domain) = trimmed.split_once('@')?;
    if local.is_empty() || domain.is_empty() {
        return None;
    }
    Some(format!(
        "{}@{}",
        local.to_ascii_lowercase(),
        domain.to_ascii_lowercase()
    ))
}

/// Pull the registered domain out of an already-normalised address.
/// "Registered domain" here is the part after the rightmost `@`.
/// We deliberately don't try to compute the eTLD+1 (no PSL — see
/// design doc §6.4); the DoH allowlist is checked against this exact
/// value, which is what the operator configures.
fn registered_domain_of(address: &str) -> Option<String> {
    address.rsplit_once('@').map(|(_, d)| d.to_string())
}

/// Convenience wrapper used by the canister method that fetches
/// `now_secs` from the IC clock. Tests pin `now_secs` and call
/// `prepare_add` directly.
#[cfg(not(test))]
pub async fn prepare_add_now(
    anchor: AnchorNumber,
    dns_input: EmailRecoveryDnsInput,
) -> Result<EmailRecoveryChallenge, EmailRecoveryError> {
    let now_secs = ic_cdk::api::time() / 1_000_000_000;
    prepare_add(anchor, dns_input, now_secs).await
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

    fn doh_input(addr: &str, selector: &str) -> EmailRecoveryDnsInput {
        EmailRecoveryDnsInput {
            address: addr.to_string(),
            selector: selector.to_string(),
        }
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
        super::super::pending::reset_for_tests();
        install_doh_allowlist(&["gmail.com"]);
        let result = block_on(prepare_add(
            1,
            doh_input("alice@example.com", "default"),
            100,
        ));
        match result {
            Err(EmailRecoveryError::DomainNotAllowlisted(d)) => assert_eq!(d, "example.com"),
            other => panic!("expected DomainNotAllowlisted, got {other:?}"),
        }
    }

    #[test]
    fn rejects_when_doh_config_unset() {
        super::super::pending::reset_for_tests();
        crate::state::persistent_state_mut(|p| p.doh_config = None);
        let result = block_on(prepare_add(
            1,
            doh_input("alice@gmail.com", "20230601"),
            100,
        ));
        assert!(matches!(
            result,
            Err(EmailRecoveryError::DomainNotAllowlisted(_))
        ));
    }

    #[test]
    fn rejects_obviously_malformed_address() {
        super::super::pending::reset_for_tests();
        install_doh_allowlist(&["gmail.com"]);
        for bad in &[
            "",
            " ",
            "no-at-sign",
            "@nolocal.com",
            "nolocal@",
            "a b@c.com",
        ] {
            let result = block_on(prepare_add(1, doh_input(bad, "default"), 100));
            assert!(
                matches!(result, Err(EmailRecoveryError::DomainNotSupported(_))),
                "expected DomainNotSupported for {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn rejects_invalid_selector() {
        super::super::pending::reset_for_tests();
        install_doh_allowlist(&["gmail.com"]);
        let too_long = "a".repeat(64);
        for bad in &["", "has space", "has/slash", too_long.as_str()] {
            let result = block_on(prepare_add(1, doh_input("alice@gmail.com", bad), 100));
            assert!(
                matches!(result, Err(EmailRecoveryError::DomainNotSupported(_))),
                "expected rejection for selector {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn happy_path_returns_a_well_formed_challenge() {
        super::super::pending::reset_for_tests();
        super::super::rng::tests::seed_for_tests([7u8; 32]);
        install_doh_allowlist(&["gmail.com"]);

        let result = block_on(prepare_add(
            42,
            doh_input("Alice@Gmail.COM", "20230601"),
            1_000,
        ));
        let challenge = result.expect("expected Ok");

        assert!(challenge.nonce.starts_with(super::super::NONCE_PREFIX));
        assert_eq!(challenge.mailbox, super::super::SETUP_MAILBOX);
        // expires_at is in nanoseconds since epoch; we passed
        // now_secs = 1_000.
        assert_eq!(
            challenge.expires_at,
            (1_000 + super::super::CHALLENGE_TTL_SECS) * 1_000_000_000
        );

        // The pending entry stores the lowercased address (input was
        // mixed case).
        super::super::pending::with_mut(&challenge.nonce, 1_001, |c| {
            assert_eq!(c.claimed_address, "alice@gmail.com");
            assert_eq!(c.selector, "20230601");
            matches!(
                c.kind,
                super::super::pending::PendingKind::Register { anchor: 42 }
            );
        })
        .expect("entry should exist");
    }

    #[test]
    fn allowlist_match_is_case_insensitive() {
        super::super::pending::reset_for_tests();
        super::super::rng::tests::seed_for_tests([3u8; 32]);
        // Operator stored the domain in caps; user typed it lowercase.
        // We accept either side mixed-case.
        install_doh_allowlist(&["GMAIL.COM"]);
        let result = block_on(prepare_add(
            1,
            doh_input("alice@gmail.com", "20230601"),
            1_000,
        ));
        assert!(result.is_ok(), "expected Ok, got {result:?}");
    }
}
