//! Second-phase DNSSEC leaf submission, exposed as two canister
//! methods that share a finalize tail:
//!
//! - `email_recovery_submit_dkim_leaf(arg)` — the FE walked a
//!   fully-signed DNSSEC resolution and submits the
//!   `(hops, extra_chains)` bundle ([`submit_dkim_leaf`]).
//! - `email_recovery_submit_dkim_leaf_via_doh(nonce)` — the FE could
//!   not walk DNSSEC because the DKIM record CNAMEs into an unsigned
//!   zone, so the canister resolves the key over its own DoH path
//!   ([`submit_dkim_leaf_via_doh`]).
//!
//! On the DNSSEC path, `smtp_request` parsed the DKIM-Signature
//! header, validated the body hash, and stashed a small
//! `PartialVerification` record on the pending challenge. It then
//! flipped the polled status to `NeedDkimLeaf { selector }` so the
//! FE could resolve that selector and call one of the two methods.
//!
//! `submit_dkim_leaf` (DNSSEC):
//!
//! 1. Looks up the pending challenge by nonce; rejects with
//!    `NoDkimLeafExpected` if the entry isn't in `NeedDkimLeaf`.
//! 2. Re-validates the cached root DNSKEY against the configured
//!    trust anchors (it may have aged out of its inception/expiration
//!    window since prepare), then walks each `extra_chain` under it,
//!    extending the cached `(zone → DNSKEY)` map for any new signed
//!    zone the DKIM CNAME chain crosses into (Proton/Tutanota-style;
//!    see design doc §7.2).
//! 3. Verifies each FE-supplied hop under the zone its
//!    `RRSIG.signer_name` names, then walks the hop sequence as a
//!    coherent CNAME → … → TXT resolution starting at
//!    `<expected_selector>._domainkey.<registered_domain>.`. Both
//!    the selector and the registered domain come from canister-
//!    pinned state, never the FE.
//! 4. Parses the final TXT, runs the cryptographic signature check
//!    using the cached headers digest + signature blob, and
//!    optionally verifies DMARC alignment against the cached DMARC
//!    bytes (or strict `d=` alignment when no DMARC was cached).
//! 5. Binds the credential and flips status to `Succeeded`.
//!
//! `submit_dkim_leaf_via_doh` (DoH fallback) skips steps 2–3 and
//! resolves the DKIM/DMARC TXT records over the canister's
//! allowlist-gated DoH path, then runs the same steps 4–5. See
//! [`run_doh_fallback`].
//!
//! See design doc §8.4 / §8.5.

use super::pending::{PendingKind, PendingStatus};
use crate::email_recovery::pending;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryError, EmailRecoveryStatus, EmailRecoverySubmitDkimLeafArg,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DelegationChain, SessionKey, SignedRRset,
};

/// Body of `email_recovery_submit_dkim_leaf(arg)` — the DNSSEC path.
/// The FE walked a fully-signed DNSSEC resolution for the leaf and
/// submits the `(hops, extra_chains)` bundle here. Returns the
/// post-call polling status — `RegistrationSucceeded` for the setup
/// flow, `RecoveryReady{...}` for the recovery flow, or
/// `Failed(reason)`. The FE can also poll
/// `email_recovery_status(nonce)` for the same answer; returning it
/// here saves the FE one round-trip on the happy path.
///
/// When the leaf's DKIM record CNAMEs into an *unsigned* zone the FE
/// can't walk DNSSEC at all (the `outlook.com` ->
/// `outbound.protection.outlook.com` / `live.com` case); it then calls
/// the sibling `submit_dkim_leaf_via_doh` instead of this method.
pub async fn submit_dkim_leaf(
    arg: EmailRecoverySubmitDkimLeafArg,
    now_secs: u64,
) -> Result<EmailRecoveryStatus, EmailRecoveryError> {
    let EmailRecoverySubmitDkimLeafArg {
        nonce,
        hops,
        extra_chains,
    } = arg;

    // Snapshot everything we need under one borrow so the rest of
    // the function works against owned data. Includes early
    // rejection of "not a DNSSEC pending entry" / "not in
    // NeedDkimLeaf state" / etc.
    let snapshot = pending::with_mut(&nonce, now_secs, |c| Snapshot::take(c))
        .ok_or(EmailRecoveryError::NonceUnknown)??;

    let verification = run_submit(&hops, &extra_chains, &snapshot, now_secs);
    finalize(&nonce, &snapshot, verification, now_secs).await
}

/// Body of `email_recovery_submit_dkim_leaf_via_doh(nonce)` — the DoH
/// fallback for the DNSSEC path. The FE calls this (instead of
/// `submit_dkim_leaf`) when it could not walk a fully-signed DNSSEC
/// resolution for the leaf because the DKIM record CNAMEs into an
/// unsigned zone (`outlook.com` -> `outbound.protection.outlook.com`,
/// `live.com`, …). It carries no leaf data: the canister resolves the
/// DKIM key over its own allowlist-gated DoH path, reusing the cached
/// `partial_verification` crypto material. A domain the operator hasn't
/// enabled comes back as `DomainNotAllowlisted` rather than leaving the
/// pending entry stuck in `NeedDkimLeaf` until it expires. Returns the
/// same post-call polling status as `submit_dkim_leaf`.
pub fn submit_dkim_leaf_via_doh(
    nonce: String,
    now_secs: u64,
) -> Result<EmailRecoveryStatus, EmailRecoveryError> {
    let snapshot = pending::with_mut(&nonce, now_secs, |c| Snapshot::take(c))
        .ok_or(EmailRecoveryError::NonceUnknown)??;
    // Resolve the DKIM key over DoH and verify in the background; the call
    // returns `Verifying` immediately and the FE polls until it flips
    // terminal (rather than blocking on the outcall). `run_doh_fallback`
    // detaches the fetches and finalizes via callback.
    pending::with_mut(&nonce, now_secs, |c| {
        c.status = PendingStatus::Verifying;
    });
    run_doh_fallback(snapshot, nonce, now_secs);
    Ok(EmailRecoveryStatus::Verifying)
}

/// Shared tail of both submit methods: on a failed `verification`,
/// stamp the pending entry `Failed` and surface the error; on success,
/// finalize per kind — setup binds the credential, recovery stamps a
/// delegation seed.
async fn finalize(
    nonce: &str,
    snapshot: &Snapshot,
    verification: Result<(), EmailRecoveryError>,
    now_secs: u64,
) -> Result<EmailRecoveryStatus, EmailRecoveryError> {
    if let Err(e) = verification {
        let cloned = e.clone();
        pending::with_mut(nonce, now_secs, |c| {
            c.status = PendingStatus::Failed(cloned);
            c.partial_verification = None;
        });
        return Err(e);
    }

    match &snapshot.kind {
        SnapshotKind::Register { anchor } => {
            match super::smtp::bind_credential(*anchor, &snapshot.claimed_address, now_secs) {
                Ok(()) => {
                    pending::with_mut(nonce, now_secs, |c| {
                        c.status = PendingStatus::Succeeded;
                        c.partial_verification = None;
                    });
                    Ok(EmailRecoveryStatus::RegistrationSucceeded)
                }
                Err(e) => {
                    let cloned = e.clone();
                    pending::with_mut(nonce, now_secs, |c| {
                        c.status = PendingStatus::Failed(cloned);
                        c.partial_verification = None;
                    });
                    Err(e)
                }
            }
        }
        SnapshotKind::Recovery { session_pk } => {
            // Build a transient `PendingSnapshot` shape that
            // `stamp_recovery_delegation` already accepts (the DoH
            // path uses the same helper). The stamper looks up the
            // anchor via the reverse-address index and adds the
            // canister signature.
            let smtp_snapshot = super::smtp::recovery_snapshot(
                snapshot.claimed_address.clone(),
                snapshot.registered_domain.clone(),
                session_pk.clone(),
            );
            match super::smtp::stamp_recovery_delegation(&smtp_snapshot, session_pk).await {
                Ok(outcome) => {
                    let user_key = serde_bytes::ByteBuf::from(outcome.user_key.clone());
                    let expiration = outcome.expiration;
                    let anchor_number = outcome.anchor_number;
                    pending::with_mut(nonce, now_secs, |c| {
                        c.recovery_outcome = Some(outcome);
                        c.status = PendingStatus::Succeeded;
                        c.partial_verification = None;
                    });
                    Ok(EmailRecoveryStatus::RecoveryReady {
                        user_key,
                        expiration,
                        anchor_number,
                    })
                }
                Err(e) => {
                    let cloned = e.clone();
                    pending::with_mut(nonce, now_secs, |c| {
                        c.status = PendingStatus::Failed(cloned);
                        c.partial_verification = None;
                    });
                    Err(e)
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Snapshot {
    kind: SnapshotKind,
    claimed_address: String,
    registered_domain: String,
    expected_selector: String,
    cached_root_dnskey: crate::dnssec::SignedRRset,
    cached_zones: crate::dnssec::ZoneKeysMap,
    cached_dmarc_txt: Option<Vec<u8>>,
    headers_digest: [u8; 32],
    signature: Vec<u8>,
    signing_domain: String,
    /// Parsed `i=` (AUID) from the email's DKIM-Signature header.
    /// Needed at submit time to check `i=` alignment with `d=` once
    /// the DKIM record's `t=s` flag is known. See design §5.4.
    signing_auid: String,
    algorithm: crate::dkim::Algorithm,
    from_address_lc: String,
}

#[derive(Clone, Debug)]
enum SnapshotKind {
    Register { anchor: AnchorNumber },
    Recovery { session_pk: SessionKey },
}

impl Snapshot {
    /// Read the pending challenge under the brief borrow held by
    /// `pending::with_mut`; reject anything that's not "ready for a
    /// DKIM leaf". The closure returns
    /// `Result<Snapshot, EmailRecoveryError>` so the caller surfaces
    /// "wrong state" as a typed error instead of a silent no-op.
    fn take(c: &super::pending::PendingChallenge) -> Result<Snapshot, EmailRecoveryError> {
        // Must be in the NeedDkimLeaf state — anything else means
        // either the email hasn't arrived yet, the entry is on the
        // DoH path, or it's already terminal.
        let expected_selector = match &c.status {
            PendingStatus::NeedDkimLeaf { selector } => selector.clone(),
            _ => return Err(EmailRecoveryError::NoDkimLeafExpected),
        };
        let cached_root_dnskey = c
            .cached_root_dnskey
            .as_ref()
            .ok_or(EmailRecoveryError::NoDkimLeafExpected)?
            .clone();
        let partial = c
            .partial_verification
            .as_ref()
            .ok_or(EmailRecoveryError::NoDkimLeafExpected)?;
        let kind = match &c.kind {
            PendingKind::Register { anchor } => SnapshotKind::Register { anchor: *anchor },
            PendingKind::Recover { session_pk } => SnapshotKind::Recovery {
                session_pk: session_pk.clone(),
            },
        };
        Ok(Snapshot {
            kind,
            claimed_address: c.claimed_address.clone(),
            registered_domain: c.registered_domain.clone(),
            expected_selector,
            cached_root_dnskey,
            cached_zones: c.cached_zones.clone(),
            cached_dmarc_txt: c.cached_dmarc_txt.clone(),
            headers_digest: partial.headers_digest,
            signature: partial.signature.clone(),
            signing_domain: partial.signing_domain.clone(),
            signing_auid: partial.signing_auid.clone(),
            algorithm: partial.algorithm,
            from_address_lc: partial.from_address_lc.clone(),
        })
    }
}

/// The leaf-validation + signature-verification + alignment-check
/// pipeline. Returns `Ok(())` on full success.
fn run_submit(
    hops: &[SignedRRset],
    extra_chains: &[DelegationChain],
    snapshot: &Snapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    // A genuine DNSSEC submission always carries at least the final
    // TXT hop. An empty `hops` set is malformed input on this path —
    // the FE that can't walk DNSSEC must call
    // `submit_dkim_leaf_via_doh` instead. Reject it with the dedicated
    // `EmptyDkimLeafHops` so the malformed-request case is unambiguous
    // (distinct from a non-empty chain that failed to validate, which
    // is `DkimLeafMismatch`), rather than relying on the hop walk below.
    if hops.is_empty() {
        return Err(EmailRecoveryError::EmptyDkimLeafHops);
    }

    // Step 1: re-validate the cached root DNSKEY against the trust
    // anchors. The pending challenge has a 30-min TTL but the RRSIG
    // window is what really decides freshness — re-checking is
    // cheap (one signature verify) and removes any chance of
    // admitting a chain under a now-stale root DNSKEY.
    let trust_anchors = crate::state::persistent_state(|p| {
        p.dnssec_config
            .as_ref()
            .map(|c| c.root_anchors.clone())
            .unwrap_or_default()
    });
    crate::dnssec::verify_root_dnskey_with_clock(
        &snapshot.cached_root_dnskey,
        &trust_anchors,
        now_secs,
    )
    .map_err(|_| EmailRecoveryError::DkimLeafMismatch)?;

    // Step 2: extend the cached zone-keys map with any *new*
    // delegation chains the FE supplied. Empty for the Gmail-style
    // same-zone case; one new chain for the Proton/Tutanota-style
    // cross-zone CNAME case.
    let mut zones = snapshot.cached_zones.clone();
    let extra_chains_internal: Vec<crate::dnssec::DelegationChain> =
        extra_chains.iter().cloned().map(Into::into).collect();
    crate::dnssec::verify_extra_chains_with_clock(
        &extra_chains_internal,
        &snapshot.cached_root_dnskey,
        &mut zones,
        now_secs,
    )
    .map_err(|_| EmailRecoveryError::DkimLeafMismatch)?;

    // Step 3: walk the hops as a CNAME → … → TXT resolution
    // anchored at the canister-pinned `<selector>._domainkey.<d>.`
    // FQDN. Any incoherence (wrong first hop, intermediate that
    // isn't a CNAME, bad target chaining, oversized chain, loop) is
    // surfaced as DkimLeafMismatch.
    let hops_internal: Vec<crate::dnssec::SignedRRset> =
        hops.iter().cloned().map(Into::into).collect();
    let expected_fqdn = format!(
        "{}._domainkey.{}.",
        snapshot.expected_selector, snapshot.registered_domain
    );
    let expected_wire = crate::dnssec::wire::encode_dns_name_lowercase(&expected_fqdn)
        .map_err(|_| EmailRecoveryError::DkimLeafMismatch)?;
    let verified = crate::dnssec::verify_hops_with_clock(
        &hops_internal,
        &zones,
        &crate::dnssec::DnsName(expected_wire),
        crate::dnssec::TYPE_TXT,
        now_secs,
    )
    .map_err(|_| EmailRecoveryError::DkimLeafMismatch)?;

    let leaf_name = crate::dnssec::wire::decode_dns_name_lowercase(&verified.name.0);

    // Steps 4–6 (parse TXT → tag contract → signature → DMARC
    // alignment → From-match) are shared with the DoH fallback. The
    // DNSSEC path feeds the TXT it just authenticated through the hop
    // walk and the DMARC record cached at prepare time.
    let txt = crate::dnssec::wire::parse_txt_rdata(&verified.rdata).map_err(|_| {
        EmailRecoveryError::EmailVerificationFailed("DNSSEC TXT RDATA truncated".into())
    })?;
    verify_dkim_key_against_partial(
        &txt,
        snapshot.cached_dmarc_txt.as_deref(),
        snapshot,
        &leaf_name,
    )
}

/// DoH fallback for the DNSSEC path: invoked from
/// `submit_dkim_leaf_via_doh` when the FE could not walk a fully-signed
/// DNSSEC resolution for the leaf (the DKIM record CNAMEs into an
/// unsigned zone — `outlook.com` -> `outbound.protection.outlook.com`,
/// `live.com`, …).
///
/// Rather than leave the pending entry stuck in `NeedDkimLeaf` until it
/// expires, we resolve the DKIM public key over the canister's own DoH
/// path and verify the signature the same way the synchronous DoH path
/// (`smtp::verify_setup_email_doh`) does — except the email body is long
/// gone (dropped after `bh=` validated at `smtp_request` time), so we
/// reuse the cached `partial_verification` crypto material instead of
/// re-parsing the message.
///
/// All DNS names handed to `fetch_txt` come from canister-pinned state
/// (`expected_selector` from the email's `s=` tag, `signing_domain` from
/// its `d=`, `registered_domain` from the claimed address), never the FE.
/// The fetch is allowlist-gated on `registered_domain`: a domain the
/// operator hasn't enabled returns `DomainNotAllowed`/`NotConfigured`,
/// which [`super::smtp::map_doh_error`] folds into
/// `DomainNotAllowlisted` — the FE turns that into the unsupported-domain
/// view.
fn run_doh_fallback(snapshot: Snapshot, nonce: String, now_secs: u64) {
    // DKIM key lives at `<selector>._domainkey.<d>`; the allowlist gate
    // and the in-domain check both key off the registered domain. The
    // DKIM + optional-DMARC fetch is deduped through the cache and
    // delivered to the callback (see `doh::fetch_txt_pair`); a missing
    // DMARC record drops us to strict `d=` alignment, any other DMARC
    // failure propagates — same handling as `smtp::verify_setup_email_doh`.
    let dkim_fqdn = format!(
        "{}._domainkey.{}",
        snapshot.expected_selector, snapshot.signing_domain
    );
    let dmarc_fqdn = format!("_dmarc.{}", snapshot.registered_domain);
    let registered_domain = snapshot.registered_domain.clone();

    crate::doh::fetch_txt_pair(
        dkim_fqdn.clone(),
        dmarc_fqdn,
        registered_domain.clone(),
        move |pair| {
            let verification = match pair {
                Ok((dkim_bytes, dmarc_bytes)) => verify_dkim_key_against_partial(
                    &dkim_bytes,
                    dmarc_bytes.as_deref(),
                    &snapshot,
                    &dkim_fqdn,
                ),
                Err(e) => Err(super::smtp::map_doh_error(e, &registered_domain)),
            };
            finalize_via_doh(nonce, snapshot, verification, now_secs);
        },
    );
}

/// Callback-style tail of the DoH fallback: apply the verdict to the pending
/// entry the FE polls. Mirrors [`finalize`], but sets status instead of
/// returning it (the FE polls), and the recovery leg — whose delegation
/// stamping is async — is detached so this completion execution returns.
fn finalize_via_doh(
    nonce: String,
    snapshot: Snapshot,
    verification: Result<(), EmailRecoveryError>,
    now_secs: u64,
) {
    match verification {
        Err(e) => {
            pending::with_mut(&nonce, now_secs, |c| {
                c.status = PendingStatus::Failed(e);
                c.partial_verification = None;
            });
        }
        Ok(()) => match &snapshot.kind {
            SnapshotKind::Register { anchor } => {
                let result =
                    super::smtp::bind_credential(*anchor, &snapshot.claimed_address, now_secs);
                pending::with_mut(&nonce, now_secs, |c| {
                    c.partial_verification = None;
                    c.status = match result {
                        Ok(()) => PendingStatus::Succeeded,
                        Err(e) => PendingStatus::Failed(e),
                    };
                });
            }
            SnapshotKind::Recovery { session_pk } => {
                let smtp_snapshot = super::smtp::recovery_snapshot(
                    snapshot.claimed_address.clone(),
                    snapshot.registered_domain.clone(),
                    session_pk.clone(),
                );
                let session_pk = session_pk.clone();
                ic_cdk::spawn(async move {
                    match super::smtp::stamp_recovery_delegation(&smtp_snapshot, &session_pk).await
                    {
                        Ok(outcome) => pending::with_mut(&nonce, now_secs, |c| {
                            c.recovery_outcome = Some(outcome);
                            c.status = PendingStatus::Succeeded;
                            c.partial_verification = None;
                        }),
                        Err(e) => pending::with_mut(&nonce, now_secs, |c| {
                            c.status = PendingStatus::Failed(e);
                            c.partial_verification = None;
                        }),
                    };
                });
            }
        },
    }
}

/// Steps 4–6 of leaf verification, shared by the DNSSEC hop-walk path
/// and the DoH fallback: parse the DKIM TXT, enforce the DNS-record tag
/// contract, run the cached-digest signature check, then DMARC
/// alignment and the final From-match. `dkim_txt` is the decoded TXT
/// (record value) bytes; `dmarc_txt` is the decoded DMARC TXT or `None`
/// when no record is published (strict `d=`/From equality applies).
/// `context` names the leaf for diagnostics only.
fn verify_dkim_key_against_partial(
    dkim_txt: &[u8],
    dmarc_txt: Option<&[u8]>,
    snapshot: &Snapshot,
    context: &str,
) -> Result<(), EmailRecoveryError> {
    // Step 4: parse the DKIM TXT, get the public key + key type.
    if dkim_txt.len() > super::MAX_DKIM_TXT_BYTES {
        return Err(EmailRecoveryError::EmailVerificationFailed(format!(
            "DKIM TXT record at {context:?} is {} bytes; refusing to admit",
            dkim_txt.len()
        )));
    }
    let txt_str = std::str::from_utf8(dkim_txt).map_err(|_| {
        EmailRecoveryError::EmailVerificationFailed("DKIM TXT is not valid UTF-8".into())
    })?;
    let dns_record = crate::dkim::parse_dkim_txt(txt_str).map_err(|e| {
        EmailRecoveryError::EmailVerificationFailed(format!("DKIM DNS record: {e}"))
    })?;

    // DNS-record-dependent DKIM tag contract (design §5.4). Routes
    // through the same shared umbrella the DoH path calls so the
    // `t=y` and AUID-alignment policies stay in lock-step across
    // pipelines. The trail the umbrella builds is the DoH-side
    // diagnostic; we discard it here because this path collapses
    // every failure into a single `EmailRecoveryError`.
    if let Err((reason, _trail)) = crate::dkim::enforce_dns_record_tag_contract(
        &snapshot.signing_auid,
        &snapshot.signing_domain,
        &dns_record,
    ) {
        return Err(EmailRecoveryError::EmailVerificationFailed(format!(
            "{reason:?}"
        )));
    }

    // Step 5: complete the cryptographic signature check.
    //
    // `verify_signature` takes the raw signed_data and hashes it
    // internally. Since we cached only the SHA-256 digest, feed
    // the digest in as the "signed_data" — for RSA-SHA256 the
    // PKCS#1 v1.5 verify would normally hash again, but we side-
    // step that by using the prehash variant of the underlying
    // primitive directly. For Ed25519-SHA256 the standard verify
    // is over `SHA256(signed_data)` (RFC 8463), so the cached
    // digest IS the correct input — we can call it directly.
    use crate::dkim::VerifyOutcome;
    let outcome = crate::dkim::verify_signature_prehashed(
        snapshot.algorithm,
        dns_record.key_type,
        &dns_record.public_key,
        &snapshot.headers_digest,
        &snapshot.signature,
    );
    match outcome {
        VerifyOutcome::Valid => {}
        VerifyOutcome::BadSignature => {
            return Err(EmailRecoveryError::EmailVerificationFailed(
                "signature did not validate against public key".into(),
            ));
        }
        VerifyOutcome::MalformedKey(e) => {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "malformed DKIM key: {e}"
            )));
        }
        VerifyOutcome::MalformedSignature(e) => {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "malformed DKIM signature: {e}"
            )));
        }
        VerifyOutcome::AlgorithmMismatch => {
            return Err(EmailRecoveryError::EmailVerificationFailed(
                "DKIM key type does not match signature algorithm".into(),
            ));
        }
        VerifyOutcome::RsaKeyTooSmall(bits) => {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "RSA DKIM key only {bits} bits"
            )));
        }
    }

    // Step 6: DMARC alignment. The smtp.rs path enforced that the
    // signing domain (`d=`) is within the registered zone, so any
    // strict-or-relaxed alignment under DMARC is trivially
    // satisfied. We re-check explicitly here against the DMARC
    // record (cached at prepare time on the DNSSEC path, or
    // freshly DoH-fetched on the fallback path).
    let from_domain = snapshot
        .from_address_lc
        .rsplit_once('@')
        .map(|(_, d)| d.to_string())
        .ok_or(EmailRecoveryError::AddressMismatch)?;
    if let Some(dmarc_bytes) = dmarc_txt {
        let dmarc_str = std::str::from_utf8(dmarc_bytes).map_err(|_| {
            EmailRecoveryError::EmailVerificationFailed("DMARC TXT is not valid UTF-8".into())
        })?;
        let dmarc = crate::dmarc::parse_dmarc_txt(dmarc_str).map_err(|e| {
            EmailRecoveryError::EmailVerificationFailed(format!("DMARC parse: {e}"))
        })?;
        if !crate::dmarc::aligns(&snapshot.signing_domain, &from_domain, dmarc.adkim) {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "DKIM d={} does not align with From={} under adkim={:?}",
                snapshot.signing_domain, from_domain, dmarc.adkim
            )));
        }
    } else {
        // No DMARC published — strict equality.
        if !snapshot.signing_domain.eq_ignore_ascii_case(&from_domain) {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "no DMARC published; DKIM d={} must equal From={}",
                snapshot.signing_domain, from_domain
            )));
        }
    }

    // From: matches the claimed address. smtp.rs already confirmed
    // this when caching `from_address_lc` on the partial verification
    // record, but pin again as defense-in-depth.
    if !snapshot
        .from_address_lc
        .eq_ignore_ascii_case(&snapshot.claimed_address)
    {
        return Err(EmailRecoveryError::AddressMismatch);
    }

    Ok(())
}

// Tests for the DNS-record tag contract live alongside the umbrella
// in `crate::dkim::tag_checks::tests`. The DNSSEC submit-side mapping
// of `(VerificationFailReason, _) → EmailRecoveryError::
// EmailVerificationFailed` is a trivial one-liner; the DNSSEC-prepare
// half of the same pattern (the `?` mapping inside
// `prepare_partial_verification`) is exercised end-to-end by
// `email_recovery::smtp::tests::dnssec_prepare_rejects_*`.
