//! `email_recovery_credential_prepare_add` — start a new binding flow.
//!
//! Authenticated entry point that validates the FE's input, picks
//! the verification path (DNSSEC if a proof bundle is supplied, DoH
//! allowlist otherwise), draws a fresh nonce from the heap PRNG,
//! and parks a `PendingChallenge` keyed by that nonce. Returns the
//! user-visible challenge (nonce + mailbox + expiry) so the FE can
//! render the "send a magic email" screen.
//!
//! See `docs/ongoing/email-recovery.md` §8.4. Path picker:
//!
//! - **DNSSEC**: when `dns_proof` is supplied, validate the chain
//!   synchronously against the canister's configured trust anchors,
//!   verify the bundle's leaf is at `<selector>._domainkey.<domain>`,
//!   and cache the verified TXT bytes on the pending challenge so
//!   `smtp_request` can skip the DoH outcall fan-out entirely.
//! - **DoH allowlist**: when `dns_proof` is absent, the registered
//!   domain must be in `DohConfig.allowed_domains`. The DKIM key is
//!   resolved via `crate::doh::fetch_txt` at email-arrival time.

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
    let EmailRecoveryDnsInput {
        address,
        selector,
        dns_proof,
    } = dns_input;

    // Sanity check the address shape. Detailed RFC 5321/5322 validation
    // is the verifier's job; here we just want to fail fast on the
    // obviously-malformed inputs (no `@`, leading/trailing whitespace,
    // empty parts) so the FE doesn't get back an opaque rejection
    // half a flow later.
    let address = normalize_address(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;
    let registered_domain = registered_domain_of(&address)
        .ok_or_else(|| EmailRecoveryError::DomainNotSupported(address.clone()))?;

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

    // Path picker. The FE never decides the path — it sends whatever
    // it could gather, and we choose:
    //
    // - DNSSEC path: if `dns_proof` is supplied, validate the chain
    //   synchronously and cache the verified DKIM TXT for
    //   `smtp_request` to consume. No outcall fan-out at email time.
    // - DoH path: fall through to the allowlist check; `smtp_request`
    //   resolves the TXT via `crate::doh::fetch_txt`.
    // - Neither: reject — the FE will surface a "we can't accept
    //   email from this domain" error.
    let (cached_dkim_txt, cached_dmarc_txt) = if let Some(proof) = dns_proof {
        let extracted = verify_dnssec_chain(proof, &selector, &registered_domain, now_secs)?;
        (Some(extracted.dkim), extracted.dmarc)
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
            return Err(EmailRecoveryError::DomainNotAllowlisted(registered_domain));
        }
        (None, None)
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
        kind: PendingKind::Register { anchor },
        claimed_address: address,
        selector,
        created_at_secs: now_secs,
        cached_dkim_txt,
        cached_dmarc_txt,
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

/// Output of a successful DNSSEC bundle verification — the TXT bytes
/// for DKIM (required) plus DMARC (optional, included when the FE
/// supplied a DMARC leaf in the bundle). When `dmarc` is `None`, the
/// canister treats the domain as "no DMARC published" and uses the
/// strict `d=` alignment fallback at email-arrival time.
struct DnssecExtractedTxt {
    dkim: Vec<u8>,
    dmarc: Option<Vec<u8>>,
}

/// Validate a caller-supplied DNSSEC proof bundle against the
/// canister's configured trust anchors and extract the DKIM (required)
/// and DMARC (optional) TXT bytes from the verified leaves.
///
/// The bundle is allowed to carry one or two leaves:
/// - exactly one TXT RRset at `<selector>._domainkey.<registered_domain>`
///   (DKIM) — required.
/// - at most one TXT RRset at `_dmarc.<registered_domain>` (DMARC) —
///   optional. If the FE includes it we cache the bytes so
///   `smtp_request` can skip the DoH outcall.
///
/// Other leaves are an error: an attacker who got a chain validated
/// for an arbitrary TXT record at the same zone shouldn't be able to
/// smuggle it into a recovery flow.
fn verify_dnssec_chain(
    proof: internet_identity_interface::internet_identity::types::DnsProofBundle,
    selector: &str,
    registered_domain: &str,
    now_secs: u64,
) -> Result<DnssecExtractedTxt, EmailRecoveryError> {
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

    let verified = crate::dnssec::verify_with_clock(&bundle, &trust_anchors, now_secs)
        .map_err(|e| EmailRecoveryError::EmailVerificationFailed(format!("DNSSEC: {e:?}")))?;

    let dkim_fqdn = format!("{selector}._domainkey.{registered_domain}.");
    let dmarc_fqdn = format!("_dmarc.{registered_domain}.");

    let mut dkim: Option<Vec<u8>> = None;
    let mut dmarc: Option<Vec<u8>> = None;

    for rec in &verified {
        if rec.rtype != crate::dnssec::types::TYPE_TXT {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "DNSSEC bundle leaf is not TXT (got rtype {})",
                rec.rtype
            )));
        }
        let leaf_name = decode_dns_name_lowercase(&rec.name.0);
        let txt = parse_txt_rdata(&rec.rdata)?;
        if leaf_name.eq_ignore_ascii_case(&dkim_fqdn) {
            if dkim.is_some() {
                return Err(EmailRecoveryError::EmailVerificationFailed(
                    "DNSSEC bundle has duplicate DKIM leaf".into(),
                ));
            }
            dkim = Some(txt);
        } else if leaf_name.eq_ignore_ascii_case(&dmarc_fqdn) {
            if dmarc.is_some() {
                return Err(EmailRecoveryError::EmailVerificationFailed(
                    "DNSSEC bundle has duplicate DMARC leaf".into(),
                ));
            }
            dmarc = Some(txt);
        } else {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "DNSSEC bundle leaf name {leaf_name:?} matches neither DKIM \
                 ({dkim_fqdn:?}) nor DMARC ({dmarc_fqdn:?})"
            )));
        }
    }

    let dkim = dkim.ok_or_else(|| {
        EmailRecoveryError::EmailVerificationFailed(format!(
            "DNSSEC bundle missing required DKIM leaf at {dkim_fqdn:?}"
        ))
    })?;

    Ok(DnssecExtractedTxt { dkim, dmarc })
}

/// Concatenate one or more TXT character-strings (each prefixed by a
/// length octet) into the bytes the DKIM/DMARC verifier expects.
/// `rdata` is `Vec<Vec<u8>>` because TXT RRsets can carry multiple
/// records — but for DKIM/DMARC there's exactly one record made of
/// multiple chunks.
fn parse_txt_rdata(rdata: &[Vec<u8>]) -> Result<Vec<u8>, EmailRecoveryError> {
    let mut txt_bytes = Vec::new();
    for rec in rdata {
        let mut i = 0;
        while i < rec.len() {
            let len = rec[i] as usize;
            i += 1;
            if i + len > rec.len() {
                return Err(EmailRecoveryError::EmailVerificationFailed(
                    "DNSSEC TXT RDATA truncated".into(),
                ));
            }
            txt_bytes.extend_from_slice(&rec[i..i + len]);
            i += len;
        }
    }
    Ok(txt_bytes)
}

/// Decode a wire-format DNS name (length-prefixed labels) into a
/// dotted ASCII-lowercased string with a trailing dot.
fn decode_dns_name_lowercase(wire: &[u8]) -> String {
    let mut out = String::new();
    let mut i = 0;
    while i < wire.len() {
        let len = wire[i] as usize;
        i += 1;
        if len == 0 {
            out.push('.');
            break;
        }
        if i + len > wire.len() {
            return out;
        }
        for &b in &wire[i..i + len] {
            out.push(b.to_ascii_lowercase() as char);
        }
        out.push('.');
        i += len;
    }
    out
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
