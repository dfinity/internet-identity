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
//!   `email_recovery_submit_dkim_leaf`.
//! - **DoH allowlist**: when `dns_proof` is absent, the registered
//!   domain must be in `DohConfig.allowed_domains`. The DKIM TXT is
//!   resolved via `crate::doh::fetch_txt` at email-arrival time and
//!   verification finishes inside `smtp_request` (no submit-leaf
//!   follow-up).

use super::pending::{insert_with_eviction, PendingChallenge, PendingKind, PendingStatus};
use super::rng::{draw_nonce_bytes, ensure_seeded, format_nonce};
use crate::state;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryChallenge, EmailRecoveryDnsInput, EmailRecoveryError,
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
    dns_input: EmailRecoveryDnsInput,
    now_secs: u64,
) -> Result<EmailRecoveryChallenge, EmailRecoveryError> {
    prepare_common(dns_input, now_secs, PendingKind::Register { anchor }).await
}

/// Body of `email_recovery_prepare_delegation(dns_input, session_pk)`.
///
/// Anonymous: anyone can call this. The `session_pk` is a fresh
/// public key the FE just generated; the eventual delegation will be
/// bound to it. The pending challenge holds it until
/// `submit_dkim_leaf` succeeds, at which point the canister stamps a
/// delegation seed and adds the matching canister-signature.
pub async fn prepare_delegation(
    dns_input: EmailRecoveryDnsInput,
    session_pk: SessionKey,
    now_secs: u64,
) -> Result<EmailRecoveryChallenge, EmailRecoveryError> {
    // Cap the FE-supplied `session_pk` length. The pending entry
    // holds this for up to 30 minutes; without a bound an open
    // caller could inflate every challenge they prepare. Real
    // session keys are well under 1 KB regardless of algorithm
    // (Ed25519 ~44 bytes, ECDSA P-256 ~91, RSA-2048 ~294).
    if session_pk.len() > super::MAX_SESSION_KEY_BYTES {
        return Err(EmailRecoveryError::InternalCanisterError(format!(
            "session_pk is {} bytes, exceeds the {}-byte limit",
            session_pk.len(),
            super::MAX_SESSION_KEY_BYTES,
        )));
    }
    prepare_common(dns_input, now_secs, PendingKind::Recover { session_pk }).await
}

/// Shared input-validation + nonce-issuing core. `kind`
/// parametrises over which flow we're starting. The challenge no
/// longer carries a `mailbox` field — the FE pairs the user-part
/// (`register` / `recover`) with `window.location.hostname` to
/// render the user-facing label, and the canister accepts mail at
/// any of the configured `related_origins` aliases (see
/// [`super::mailbox_domains`]).
async fn prepare_common(
    dns_input: EmailRecoveryDnsInput,
    now_secs: u64,
    kind: PendingKind,
) -> Result<EmailRecoveryChallenge, EmailRecoveryError> {
    let EmailRecoveryDnsInput { address, dns_proof } = dns_input;
    crate::er_dbg!(
        "prepare.start address={} kind={} dns_proof={}",
        address,
        match &kind {
            PendingKind::Register { anchor } => format!("Register({anchor})"),
            PendingKind::Recover { .. } => "Recover".into(),
        },
        if dns_proof.is_some() { "DNSSEC" } else { "DoH" }
    );

    // Sanity check the address shape. Detailed RFC 5321/5322 validation
    // is the verifier's job; here we just want to fail fast on the
    // obviously-malformed inputs (no `@`, leading/trailing whitespace,
    // empty parts) so the FE doesn't get back an opaque rejection
    // half a flow later.
    let address = normalize_address(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;
    let registered_domain = registered_domain_of(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;

    // Path picker. The FE never decides the path — it sends whatever
    // it could gather, and we choose:
    //
    // - DNSSEC path: if `dns_proof` is supplied, validate the
    //   *skeleton chain* synchronously and cache the deepest-zone
    //   DNSKEY for later leaf admission. If the bundle also carries a
    //   DMARC leaf, validate it and cache the TXT bytes.
    // - DoH path: fall through to the allowlist check; `smtp_request`
    //   resolves the DKIM TXT via `crate::doh::fetch_txt` at email
    //   arrival.
    // - Neither: reject — the FE will surface a "we can't accept
    //   email from this domain" error.
    let (cached_root_dnskey, cached_zones, cached_dmarc_txt) = if let Some(proof) = dns_proof {
        let extracted = verify_dnssec_skeleton(proof, &registered_domain, now_secs)?;
        (
            Some(extracted.root_dnskey),
            extracted.zones,
            extracted.dmarc,
        )
    } else {
        // No DNSSEC bundle → DoH allowlist gate.
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
            crate::er_dbg!(
                "prepare DomainNotAllowlisted reg={}",
                registered_domain
            );
            return Err(EmailRecoveryError::DomainNotAllowlisted(registered_domain));
        }
        crate::er_dbg!("prepare doh_path_ok reg={}", registered_domain);
        (None, crate::dnssec::ZoneKeysMap::new(), None)
    };

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
        kind,
        claimed_address: address,
        registered_domain,
        created_at_secs: now_secs,
        cached_root_dnskey,
        cached_zones,
        cached_dmarc_txt,
        partial_verification: None,
        status: PendingStatus::Pending,
        recovery_outcome: None,
    };
    insert_with_eviction(nonce.clone(), challenge, now_secs);
    crate::er_dbg!("prepare.ok nonce={}", nonce);

    Ok(EmailRecoveryChallenge {
        nonce,
        // `Timestamp` is nanoseconds since epoch in this crate (see
        // `internet_identity_interface::types`). We work in seconds
        // internally for the TTL math and convert at the wire boundary.
        expires_at: now_secs
            .saturating_add(super::CHALLENGE_TTL_SECS)
            .saturating_mul(1_000_000_000),
    })
}

/// Output of a successful DNSSEC skeleton-bundle verification — the
/// validated root DNSKEY RRset (cached so the canister can admit
/// further chains under it at submit time when the DKIM CNAME chain
/// crosses into a new zone), the validated `(zone → DNSKEY)` map for
/// every zone the prepare bundle covered, and the optional DMARC
/// TXT bytes (set when the FE included a DMARC leaf in the skeleton
/// bundle).
struct DnssecExtracted {
    root_dnskey: crate::dnssec::SignedRRset,
    zones: crate::dnssec::ZoneKeysMap,
    dmarc: Option<Vec<u8>>,
}

/// Validate a caller-supplied DNSSEC *skeleton* bundle against the
/// canister's configured trust anchors and extract the validated
/// zone-keys map and optional DMARC TXT bytes.
///
/// The bundle's `hops` is allowed to carry at most one entry, and
/// that entry must be the DMARC TXT at `_dmarc.<registered_domain>`.
/// Any other hop — including a DKIM TXT — is rejected: the DKIM
/// resolution belongs in the post-email submit-leaf call, not the
/// prepare bundle, and an attacker who got a different TXT
/// validated under the same chain shouldn't be able to smuggle it
/// through here.
fn verify_dnssec_skeleton(
    proof: internet_identity_interface::internet_identity::types::DnsProofBundle,
    registered_domain: &str,
    now_secs: u64,
) -> Result<DnssecExtracted, EmailRecoveryError> {
    let trust_anchors = state::persistent_state(|p| {
        p.dnssec_config
            .as_ref()
            .map(|c| c.root_anchors.clone())
            .unwrap_or_default()
    });
    if trust_anchors.is_empty() {
        return Err(EmailRecoveryError::DomainNotSupported(
            "DNSSEC trust anchors not configured".into(),
        ));
    }

    // Convert the Candid-friendly bundle to the verifier's internal
    // representation. (See `crate::dnssec::types::interface_conversions`
    // for the From<> impls.)
    let bundle: crate::dnssec::DnsProofBundle = proof.into();

    // The skeleton bundle must carry at most one hop (the optional
    // DMARC TXT at `_dmarc.<registered_domain>`). Reject up front if
    // the FE tried to smuggle a DKIM resolution — that belongs in
    // submit_dkim_leaf, not here.
    if bundle.hops.len() > 1 {
        return Err(EmailRecoveryError::EmailVerificationFailed(format!(
            "skeleton bundle has {} hops; expected 0 (no DMARC) or 1 (DMARC TXT)",
            bundle.hops.len()
        )));
    }

    // Step 1: validate the root DNSKEY against trust anchors +
    // RRSIG freshness.
    crate::dnssec::verify_root_dnskey_with_clock(&bundle.root_dnskey, &trust_anchors, now_secs)
        .map_err(|e| EmailRecoveryError::EmailVerificationFailed(format!("DNSSEC: {e:?}")))?;

    // Step 2: walk every delegation chain into a (zone → DNSKEY)
    // map. The map starts with one zone for Gmail-style direct-TXT
    // domains and may carry several zones when the FE pre-walked a
    // CNAME chain across signed zones at prepare time.
    let mut zones = crate::dnssec::ZoneKeysMap::new();
    crate::dnssec::verify_extra_chains_with_clock(
        &bundle.chains,
        &bundle.root_dnskey,
        &mut zones,
        now_secs,
    )
    .map_err(|e| EmailRecoveryError::EmailVerificationFailed(format!("DNSSEC chain: {e:?}")))?;
    if zones.is_empty() {
        return Err(EmailRecoveryError::EmailVerificationFailed(
            "skeleton bundle supplied no delegation chains".into(),
        ));
    }

    // Optional DMARC hop: if present, validate it as a single TXT
    // hop at `_dmarc.<registered_domain>`.
    let dmarc_fqdn = format!("_dmarc.{registered_domain}.");
    let dmarc = if bundle.hops.is_empty() {
        None
    } else {
        let requested_name = bundle.hops[0].name.clone();
        let verified = crate::dnssec::verify_hops_with_clock(
            &bundle.hops,
            &zones,
            &requested_name,
            crate::dnssec::TYPE_TXT,
            now_secs,
        )
        .map_err(|e| EmailRecoveryError::EmailVerificationFailed(format!("DNSSEC leaf: {e:?}")))?;
        let leaf_name = super::dns::decode_dns_name_lowercase(&verified.name.0);
        if !leaf_name.eq_ignore_ascii_case(&dmarc_fqdn) {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "skeleton bundle leaf name {leaf_name:?} is not the expected \
                 DMARC name {dmarc_fqdn:?} — DKIM leaves belong in submit_dkim_leaf"
            )));
        }
        let txt = super::dns::parse_txt_rdata(&verified.rdata)?;
        if txt.len() > super::MAX_DMARC_TXT_BYTES {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "DMARC TXT record at {leaf_name:?} is {} bytes; \
                 refusing to cache more than {} bytes per pending entry",
                txt.len(),
                super::MAX_DMARC_TXT_BYTES,
            )));
        }
        Some(txt)
    };

    Ok(DnssecExtracted {
        root_dnskey: bundle.root_dnskey,
        zones,
        dmarc,
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
    // Reject before splitting: the addr-spec must fit RFC 5321
    // §4.5.3.1.3's path limit minus the `<>` framing. This caps
    // pending-entry heap use against a caller passing in a multi-KB
    // string. The local/domain caps below are stricter than this for
    // valid addresses but we check the total first so we don't waste
    // a `split_once` on obviously-oversized input.
    if trimmed.len() > super::MAX_ADDRESS {
        return None;
    }
    let (local, domain) = trimmed.split_once('@')?;
    if local.is_empty() || domain.is_empty() {
        return None;
    }
    if local.len() > super::MAX_LOCAL_PART || domain.len() > super::MAX_DOMAIN {
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

    fn doh_input(addr: &str) -> EmailRecoveryDnsInput {
        EmailRecoveryDnsInput {
            address: addr.to_string(),
            dns_proof: None,
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
        let result = block_on(prepare_add(1, doh_input("alice@example.com"), 100));
        match result {
            Err(EmailRecoveryError::DomainNotAllowlisted(d)) => assert_eq!(d, "example.com"),
            other => panic!("expected DomainNotAllowlisted, got {other:?}"),
        }
    }

    #[test]
    fn rejects_when_doh_config_unset() {
        super::super::pending::reset_for_tests();
        crate::state::persistent_state_mut(|p| p.doh_config = None);
        let result = block_on(prepare_add(1, doh_input("alice@gmail.com"), 100));
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
            let result = block_on(prepare_add(1, doh_input(bad), 100));
            assert!(
                matches!(result, Err(EmailRecoveryError::DomainNotSupported(_))),
                "expected DomainNotSupported for {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn rejects_oversized_address() {
        // Addresses longer than RFC 5321's effective addr-spec cap
        // (254 bytes) or with local/domain parts past their own
        // sub-caps (64 / 255) are rejected at prepare time so the
        // pending-challenge map can't be inflated.
        super::super::pending::reset_for_tests();
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
                matches!(result, Err(EmailRecoveryError::DomainNotSupported(_))),
                "expected DomainNotSupported for oversized {bad:?}, got {result:?}"
            );
        }
    }

    #[test]
    fn happy_path_returns_a_well_formed_challenge() {
        super::super::pending::reset_for_tests();
        super::super::rng::tests::seed_for_tests([7u8; 32]);
        install_doh_allowlist(&["gmail.com"]);

        let result = block_on(prepare_add(42, doh_input("Alice@Gmail.COM"), 1_000));
        let challenge = result.expect("expected Ok");

        assert!(challenge.nonce.starts_with(super::super::NONCE_PREFIX));
        // expires_at is in nanoseconds since epoch; we passed
        // now_secs = 1_000.
        assert_eq!(
            challenge.expires_at,
            (1_000 + super::super::CHALLENGE_TTL_SECS) * 1_000_000_000
        );

        // The pending entry stores the lowercased address (input was
        // mixed case) and the registered domain.
        super::super::pending::with_mut(&challenge.nonce, 1_001, |c| {
            assert_eq!(c.claimed_address, "alice@gmail.com");
            assert_eq!(c.registered_domain, "gmail.com");
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
        let result = block_on(prepare_add(1, doh_input("alice@gmail.com"), 1_000));
        assert!(result.is_ok(), "expected Ok, got {result:?}");
    }
}
