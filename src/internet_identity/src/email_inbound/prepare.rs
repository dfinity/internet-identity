//! Shared `prepare_common` core used by both flow-specific entry
//! points ([`crate::email_recovery::prepare::prepare_add`] /
//! `prepare_delegation` and
//! [`crate::verified_emails::prepare::prepare_add`]).
//!
//! Validates the FE's input (address shape, registered-domain
//! extraction), picks the verification path (DNSSEC skeleton if a
//! bundle is supplied and the deploy flag is on, DoH allowlist
//! otherwise), draws a fresh nonce from the heap PRNG, and parks a
//! `PendingChallenge` keyed by that nonce. Returns the user-visible
//! challenge (nonce + expiry) so the FE can render the "send a magic
//! email" screen.
//!
//! Path picker (see `docs/ongoing/email-recovery.md` §8.4):
//!
//! - **DNSSEC**: when `dns_proof` is supplied and the flag is on,
//!   validate the *skeleton chain* (root DNSKEY + delegations)
//!   synchronously against the canister's trust anchors and cache the
//!   deepest-zone DNSKEY RRset on the pending challenge. The bundle
//!   may also carry an optional DMARC leaf at `_dmarc.<domain>`; if
//!   present we validate it and cache the TXT bytes. The DKIM leaf is
//!   *not* in this bundle — it lands later via
//!   `email_challenge_submit_dkim_leaf`.
//! - **DoH allowlist**: when `dns_proof` is absent (or the flag is
//!   off), the registered domain must be in `DohConfig.allowed_domains`.
//!   The DKIM TXT is resolved via `crate::doh::fetch_txt` at
//!   email-arrival time and verification finishes inside
//!   `smtp_request` (no submit-leaf follow-up).

use super::pending::{insert_with_eviction, PendingChallenge, PendingKind, PendingStatus};
use super::rng::{draw_nonce_bytes, ensure_seeded, format_nonce_with_prefix};
use crate::state;
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
};

/// Shared input-validation + nonce-issuing core. `kind`
/// parametrises over which flow we're starting. The challenge no
/// longer carries a `mailbox` field — the FE pairs the user-part
/// (`register` / `recover`) with `window.location.hostname` to
/// render the user-facing label, and the canister accepts mail at
/// any of the configured `related_origins` aliases (see
/// [`super::mailbox_domains`]).
pub(crate) async fn prepare_common(
    dns_input: EmailChallengeDnsInput,
    now_secs: u64,
    kind: PendingKind,
    nonce_prefix: &str,
) -> Result<EmailChallenge, EmailChallengeError> {
    let EmailChallengeDnsInput { address, dns_proof } = dns_input;

    // Sanity check the address shape. Detailed RFC 5321/5322 validation
    // is the verifier's job; here we just want to fail fast on the
    // obviously-malformed inputs (no `@`, leading/trailing whitespace,
    // empty parts, oversized local-part) so the FE doesn't get back an
    // opaque rejection half a flow later. Reported as
    // `InvalidEmailAddress` so the wizard can show an inline form error
    // instead of routing to the "domain not supported" view, which is
    // for valid addresses whose domain we can't verify.
    let address = normalize_address(&address)
        .ok_or_else(|| EmailChallengeError::InvalidEmailAddress(address.clone()))?;
    let registered_domain = registered_domain_of(&address)
        .ok_or_else(|| EmailChallengeError::DomainNotSupported(address.clone()))?;

    // Path picker. The FE never decides the path — it sends whatever
    // it could gather, and we choose:
    //
    // - DNSSEC path: taken only when the deploy flag enables it *and* a
    //   `dns_proof` is supplied. Validates the *skeleton chain*
    //   synchronously and caches the deepest-zone DNSKEY for later leaf
    //   admission; a bundled DMARC leaf is validated and its TXT cached.
    // - DoH path: the default. With the flag off any supplied proof is
    //   ignored and we fall through to the allowlist check; `smtp_request`
    //   resolves the DKIM TXT via `crate::doh::fetch_txt` at email arrival.
    // - Non-allowlisted domain on the DoH path → reject; the FE surfaces a
    //   "we can't accept email from this domain" error.
    let (cached_root_dnskey, cached_zones, cached_dmarc_txt) = match dns_proof {
        Some(proof) if super::dnssec_email_recovery_enabled() => {
            let extracted = verify_dnssec_skeleton(proof, &registered_domain, now_secs)?;
            (
                Some(extracted.root_dnskey),
                extracted.zones,
                extracted.dmarc,
            )
        }
        _ => {
            // DoH allowlist gate.
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
                return Err(EmailChallengeError::DomainNotAllowlisted(registered_domain));
            }
            (None, crate::dnssec::ZoneKeysMap::new(), None)
        }
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
        let candidate = format_nonce_with_prefix(nonce_prefix, &draw_nonce_bytes());
        let collision = super::pending::with_mut(&candidate, now_secs, |_| ()).is_some();
        if !collision {
            break candidate;
        }
        if attempts > 8 {
            // Should never trigger in practice. If it does, something
            // is very wrong with the PRNG (or the caller is doing
            // something pathological); abort rather than spin.
            return Err(EmailChallengeError::InternalCanisterError(
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
        // Populated later, in `handle_smtp_request`, once an email
        // bearing this nonce arrives carrying a gateway correlation id.
        message_id: None,
    };
    insert_with_eviction(nonce.clone(), challenge, now_secs);

    Ok(EmailChallenge {
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
) -> Result<DnssecExtracted, EmailChallengeError> {
    let trust_anchors = state::persistent_state(|p| {
        p.dnssec_config
            .as_ref()
            .map(|c| c.root_anchors.clone())
            .unwrap_or_default()
    });
    if trust_anchors.is_empty() {
        return Err(EmailChallengeError::DomainNotSupported(
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
        return Err(EmailChallengeError::EmailVerificationFailed(format!(
            "skeleton bundle has {} hops; expected 0 (no DMARC) or 1 (DMARC TXT)",
            bundle.hops.len()
        )));
    }

    // Step 1: validate the root DNSKEY against trust anchors +
    // RRSIG freshness.
    crate::dnssec::verify_root_dnskey_with_clock(&bundle.root_dnskey, &trust_anchors, now_secs)
        .map_err(|e| EmailChallengeError::EmailVerificationFailed(format!("DNSSEC: {e:?}")))?;

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
    .map_err(|e| EmailChallengeError::EmailVerificationFailed(format!("DNSSEC chain: {e:?}")))?;
    if zones.is_empty() {
        return Err(EmailChallengeError::EmailVerificationFailed(
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
        .map_err(|e| EmailChallengeError::EmailVerificationFailed(format!("DNSSEC leaf: {e:?}")))?;
        let leaf_name = crate::dnssec::wire::decode_dns_name_lowercase(&verified.name.0);
        if !leaf_name.eq_ignore_ascii_case(&dmarc_fqdn) {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
                "skeleton bundle leaf name {leaf_name:?} is not the expected \
                 DMARC name {dmarc_fqdn:?} — DKIM leaves belong in submit_dkim_leaf"
            )));
        }
        let txt = crate::dnssec::wire::parse_txt_rdata(&verified.rdata).map_err(|_| {
            EmailChallengeError::EmailVerificationFailed("DNSSEC TXT RDATA truncated".into())
        })?;
        if txt.len() > super::MAX_DMARC_TXT_BYTES {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
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
