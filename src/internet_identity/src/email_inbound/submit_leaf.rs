//! Second-phase DNSSEC leaf submission, exposed as two canister
//! methods that each accept the leaf and stamp the verdict on the
//! pending challenge the FE polls (neither returns the verdict):
//!
//! - `email_challenge_submit_dkim_leaf(arg)` — the FE walked a
//!   fully-signed DNSSEC resolution and submits the
//!   `(hops, extra_chains)` bundle ([`submit_dkim_leaf`]).
//! - `email_challenge_resolve_via_doh(nonce)` — the canister resolves
//!   the DKIM key over its own DoH path. Used for the pure-DoH (Gmail)
//!   case and as the fallback when the FE can't walk DNSSEC because the
//!   DKIM record CNAMEs into an unsigned zone ([`resolve_via_doh`]). The
//!   FE calls it repeatedly while the status is `ResolvingDoh`: each call
//!   reads the DoH cache and either finishes (cache `Ready`) or leaves the
//!   status `ResolvingDoh` to poll again (cache `Pending`).
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
//! `resolve_via_doh` (the DoH path) skips steps 2–3 and reads the
//! DKIM/DMARC TXT records from the canister's allowlist-gated DoH cache,
//! then runs the same steps 4–5 once they're [`Ready`](crate::single_flight_cache::Cached).
//! See [`resolve_dkim_and_dmarc`].
//!
//! See design doc §8.4 / §8.5.

use super::pending::{self, PendingKind, PendingStatus};
use crate::doh::{DohError, DohRecord};
use crate::single_flight_cache::Cached;
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallengeError, EmailChallengeSubmitDkimLeafArg,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DelegationChain, SessionKey, SignedRRset,
};

/// A resolved DKIM TXT (required) plus DMARC TXT (optional — `None` is the
/// "no policy published" strict-alignment fallback).
type DkimAndDmarc = (Vec<u8>, Option<Vec<u8>>);

/// Body of `email_challenge_submit_dkim_leaf(arg)` — the DNSSEC path.
/// The FE walked a fully-signed DNSSEC resolution for the leaf and
/// submits the `(hops, extra_chains)` bundle here. **Accept-only:**
/// returns `Ok(())` once the leaf is processed and the verdict written
/// to the pending status, or `Err` for a call-level rejection (unknown
/// nonce / not in `NeedDkimLeaf`). The verification verdict
/// (`RegistrationSucceeded` / `RecoveryReady` / `Failed`) is read by
/// polling `email_challenge_status(nonce)` — the single source of truth,
/// shared with the asynchronous DoH paths.
///
/// When the leaf's DKIM record CNAMEs into an *unsigned* zone the FE
/// can't walk DNSSEC at all (the `outlook.com` ->
/// `outbound.protection.outlook.com` / `live.com` case); it then calls
/// the sibling `resolve_via_doh` instead of this method.
pub async fn submit_dkim_leaf(
    arg: EmailChallengeSubmitDkimLeafArg,
    now_secs: u64,
) -> Result<(), EmailChallengeError> {
    let EmailChallengeSubmitDkimLeafArg {
        nonce,
        hops,
        extra_chains,
    } = arg;

    // Snapshot everything we need under one borrow so the rest of
    // the function works against owned data. Includes early
    // rejection of "not a DNSSEC pending entry" / "not in
    // NeedDkimLeaf state" / etc. — these are call-level errors,
    // returned to the FE; the verification verdict goes on the polled
    // status instead (single source of truth).
    let snapshot = pending::with_mut(&nonce, now_secs, |c| Snapshot::take(c))
        .ok_or(EmailChallengeError::NonceUnknown)??;

    let verification = run_submit(&hops, &extra_chains, &snapshot, now_secs);
    finalize(&nonce, &snapshot.material, verification, now_secs).await;
    Ok(())
}

/// Body of `email_challenge_resolve_via_doh(nonce)` — the DoH path. Used
/// for the pure-DoH (Gmail) case and as the fallback when the FE could not
/// walk a fully-signed DNSSEC resolution because the DKIM record CNAMEs
/// into an unsigned zone (`outlook.com` -> `outbound.protection.outlook.com`,
/// `live.com`, …). It carries no leaf data: the canister resolves the DKIM
/// key over its own allowlist-gated DoH cache, reusing the
/// `partial_verification` crypto material `smtp_request` stashed.
///
/// **Polled, idempotent, accept-only.** The FE calls this repeatedly while
/// the status is `ResolvingDoh`. Each call reads the DoH cache:
///
/// - cache [`Pending`](Cached::Pending) — a fetch is in flight; the status
///   stays `ResolvingDoh` and the call returns `Ok(())` for the FE to poll
///   again.
/// - cache [`Ready`](Cached::Ready) — the key is in hand; verify against the
///   stashed partial and stamp the terminal verdict.
///
/// A domain the operator hasn't enabled lands as
/// `Failed(DomainNotAllowlisted)`. Returns `Ok(())` and the verdict is read
/// by polling `email_challenge_status`; `Err` is a call-level rejection
/// (unknown nonce).
pub fn resolve_via_doh(nonce: String, now_secs: u64) -> Result<(), EmailChallengeError> {
    let Some(mat) = pending::with_mut(&nonce, now_secs, |c| VerifyMaterial::take_for_doh(c))
        .ok_or(EmailChallengeError::NonceUnknown)??
    else {
        // Nothing to do — the entry is already terminal (a concurrent poll
        // finished it) or carries no partial. The FE reads the verdict from
        // the status; this poll is a silent no-op.
        return Ok(());
    };

    // Make sure the status reflects "resolving over DoH" — this flips a
    // first call that arrived in `NeedDkimLeaf` (the DNSSEC fallback) so the
    // FE switches from walking DNSSEC to driving this method.
    pending::with_mut(&nonce, now_secs, |c| {
        c.status = PendingStatus::ResolvingDoh;
    });

    let dkim_fqdn = format!("{}._domainkey.{}", mat.selector, mat.signing_domain);
    let dmarc_fqdn = format!("_dmarc.{}", mat.registered_domain);
    match resolve_dkim_and_dmarc(&dkim_fqdn, &dmarc_fqdn, &mat.registered_domain) {
        // A fetch is still in flight — keep polling.
        Ok(Cached::Pending) => Ok(()),
        Ok(Cached::Ready((dkim_bytes, dmarc_bytes))) => {
            let verification = verify_dkim_key_against_partial(
                &dkim_bytes,
                dmarc_bytes.as_deref(),
                &mat,
                &dkim_fqdn,
            );
            finalize_via_doh(nonce, mat, verification, now_secs);
            Ok(())
        }
        Err(e) => {
            let reason = super::smtp::map_doh_error(e, &mat.registered_domain);
            finalize_via_doh(nonce, mat, Err(reason), now_secs);
            Ok(())
        }
    }
}

/// Read the DKIM key (required) and DMARC policy (optional) for `mat` from
/// the DoH cache, keeping the DKIM-required / DMARC-optional + `NoAnswer`
/// rule in one place:
///
/// - either fetch [`Pending`](Cached::Pending) → the whole resolution is
///   `Pending` (poll again).
/// - DKIM `NoAnswer` (no key published) → a verification failure
///   (`DohError::NoAnswer`).
/// - DMARC `NoAnswer` → `None`: the valid "no policy published" state (RFC
///   7489) that drops the verifier to strict `d=` alignment (design §6.3).
///   Any *other* DMARC failure propagates rather than quietly tightening the
///   check.
///
/// All DNS names come from canister-pinned state (`selector` from the
/// email's `s=`, `signing_domain` from its `d=`, `registered_domain` from
/// the claimed address), never the FE, and the fetch is allowlist-gated on
/// `registered_domain`.
fn resolve_dkim_and_dmarc(
    dkim_fqdn: &str,
    dmarc_fqdn: &str,
    registered_domain: &str,
) -> Result<Cached<DkimAndDmarc>, DohError> {
    let dkim = match crate::doh::fetch_txt(dkim_fqdn, registered_domain)? {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(DohRecord::Txt(bytes)) => bytes,
        Cached::Ready(DohRecord::NoAnswer) => return Err(DohError::NoAnswer),
    };
    let dmarc = match crate::doh::fetch_txt(dmarc_fqdn, registered_domain)? {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(DohRecord::Txt(bytes)) => Some(bytes),
        Cached::Ready(DohRecord::NoAnswer) => None,
    };
    Ok(Cached::Ready((dkim, dmarc)))
}

/// Tail of the DNSSEC submit path: stamp the verdict onto the pending entry
/// the FE polls (it returns nothing — the status is the source of truth). On
/// a failed `verification` → `Failed`; on success, per kind — setup binds the
/// credential, recovery stamps a delegation seed. (The DoH paths use
/// [`finalize_via_doh`], which detaches the recovery leg.)
async fn finalize(
    nonce: &str,
    mat: &VerifyMaterial,
    verification: Result<(), EmailChallengeError>,
    now_secs: u64,
) {
    if let Err(e) = verification {
        pending::with_mut(nonce, now_secs, |c| {
            c.status = PendingStatus::Failed(e);
            c.partial_verification = None;
        });
        return;
    }

    match &mat.kind {
        SnapshotKind::Register { anchor } => {
            let result = super::smtp::bind_credential(*anchor, &mat.claimed_address, now_secs);
            pending::with_mut(nonce, now_secs, |c| {
                c.partial_verification = None;
                c.status = match result {
                    Ok(()) => PendingStatus::Succeeded,
                    Err(e) => PendingStatus::Failed(e),
                };
            });
        }
        SnapshotKind::VerifyEmail { anchor } => {
            let result = super::smtp::append_or_refresh_verified_email(
                *anchor,
                &mat.claimed_address,
                now_secs,
            );
            pending::with_mut(nonce, now_secs, |c| {
                c.partial_verification = None;
                c.status = match result {
                    Ok(()) => PendingStatus::Succeeded,
                    Err(e) => PendingStatus::Failed(e),
                };
            });
        }
        SnapshotKind::Recovery { session_pk } => {
            // Build a transient `PendingSnapshot` shape that
            // `stamp_recovery_delegation` already accepts (the DoH
            // path uses the same helper). The stamper looks up the
            // anchor via the reverse-address index and adds the
            // canister signature.
            let smtp_snapshot = super::smtp::recovery_snapshot(
                mat.claimed_address.clone(),
                mat.registered_domain.clone(),
                session_pk.clone(),
            );
            match super::smtp::stamp_recovery_delegation(&smtp_snapshot, session_pk).await {
                Ok(outcome) => pending::with_mut(nonce, now_secs, |c| {
                    c.recovery_outcome = Some(outcome);
                    c.status = PendingStatus::Succeeded;
                    c.partial_verification = None;
                }),
                Err(e) => pending::with_mut(nonce, now_secs, |c| {
                    c.status = PendingStatus::Failed(e);
                    c.partial_verification = None;
                }),
            };
        }
    }
}

/// Everything the signature verification + finalize need, derived from the
/// `partial_verification` record and the challenge identity — independent of
/// how the DKIM key is obtained (DNSSEC hop walk or DoH cache). The DNSSEC
/// path wraps this in [`Snapshot`] alongside its chain material; the DoH path
/// uses it directly.
#[derive(Clone, Debug)]
struct VerifyMaterial {
    kind: SnapshotKind,
    claimed_address: String,
    registered_domain: String,
    /// Parsed `s=` (selector) from the email's DKIM-Signature header. The
    /// `<selector>._domainkey.<d>` owner-name to resolve.
    selector: String,
    signing_domain: String,
    /// Parsed `i=` (AUID) from the email's DKIM-Signature header.
    /// Needed at submit time to check `i=` alignment with `d=` once
    /// the DKIM record's `t=s` flag is known. See design §5.4.
    signing_auid: String,
    algorithm: crate::dkim::Algorithm,
    headers_digest: [u8; 32],
    signature: Vec<u8>,
    from_address_lc: String,
}

/// DNSSEC-path snapshot: the partial-derived [`VerifyMaterial`] plus the
/// cached DNSSEC chain material the hop walk re-validates against.
#[derive(Clone, Debug)]
struct Snapshot {
    material: VerifyMaterial,
    cached_root_dnskey: crate::dnssec::SignedRRset,
    cached_zones: crate::dnssec::ZoneKeysMap,
    cached_dmarc_txt: Option<Vec<u8>>,
}

#[derive(Clone, Debug)]
enum SnapshotKind {
    Register {
        anchor: AnchorNumber,
    },
    Recovery {
        session_pk: SessionKey,
    },
    /// Verified-email flow — on success the verified `From:` is
    /// appended to `Anchor.verified_emails` instead of being bound
    /// as a recovery credential.
    VerifyEmail {
        anchor: AnchorNumber,
    },
}

impl VerifyMaterial {
    fn kind_from(c: &super::pending::PendingChallenge) -> SnapshotKind {
        match &c.kind {
            PendingKind::Register { anchor } => SnapshotKind::Register { anchor: *anchor },
            PendingKind::Recover { session_pk } => SnapshotKind::Recovery {
                session_pk: session_pk.clone(),
            },
            PendingKind::VerifyEmail { anchor } => SnapshotKind::VerifyEmail { anchor: *anchor },
        }
    }

    /// Build the verify material for the DoH path. Accepts a challenge whose
    /// email has arrived (`NeedDkimLeaf` — the DNSSEC fallback first call —
    /// or `ResolvingDoh`) and carries a stashed partial. Returns:
    ///
    /// - `Ok(Some(mat))` — proceed.
    /// - `Ok(None)` — nothing to do: the entry is already terminal or carries
    ///   no partial (a concurrent poll finished it). The caller no-ops.
    /// - `Err(_)` — call-level rejection.
    fn take_for_doh(
        c: &super::pending::PendingChallenge,
    ) -> Result<Option<VerifyMaterial>, EmailChallengeError> {
        match &c.status {
            PendingStatus::NeedDkimLeaf { .. } | PendingStatus::ResolvingDoh => {}
            // Email hasn't arrived yet, or it's already terminal.
            _ => return Ok(None),
        }
        let Some(partial) = c.partial_verification.as_ref() else {
            return Ok(None);
        };
        Ok(Some(VerifyMaterial {
            kind: Self::kind_from(c),
            claimed_address: c.claimed_address.clone(),
            registered_domain: c.registered_domain.clone(),
            selector: partial.selector.clone(),
            signing_domain: partial.signing_domain.clone(),
            signing_auid: partial.signing_auid.clone(),
            algorithm: partial.algorithm,
            headers_digest: partial.headers_digest,
            signature: partial.signature.clone(),
            from_address_lc: partial.from_address_lc.clone(),
        }))
    }
}

impl Snapshot {
    /// Read the pending challenge under the brief borrow held by
    /// `pending::with_mut`; reject anything that's not "ready for a
    /// DKIM leaf". The closure returns
    /// `Result<Snapshot, EmailChallengeError>` so the caller surfaces
    /// "wrong state" as a typed error instead of a silent no-op.
    fn take(c: &super::pending::PendingChallenge) -> Result<Snapshot, EmailChallengeError> {
        // Must be in the NeedDkimLeaf state — anything else means
        // either the email hasn't arrived yet, the entry is on the
        // DoH path, or it's already terminal.
        if !matches!(c.status, PendingStatus::NeedDkimLeaf { .. }) {
            return Err(EmailChallengeError::NoDkimLeafExpected);
        }
        let cached_root_dnskey = c
            .cached_root_dnskey
            .as_ref()
            .ok_or(EmailChallengeError::NoDkimLeafExpected)?
            .clone();
        let partial = c
            .partial_verification
            .as_ref()
            .ok_or(EmailChallengeError::NoDkimLeafExpected)?;
        Ok(Snapshot {
            material: VerifyMaterial {
                kind: VerifyMaterial::kind_from(c),
                claimed_address: c.claimed_address.clone(),
                registered_domain: c.registered_domain.clone(),
                selector: partial.selector.clone(),
                signing_domain: partial.signing_domain.clone(),
                signing_auid: partial.signing_auid.clone(),
                algorithm: partial.algorithm,
                headers_digest: partial.headers_digest,
                signature: partial.signature.clone(),
                from_address_lc: partial.from_address_lc.clone(),
            },
            cached_root_dnskey,
            cached_zones: c.cached_zones.clone(),
            cached_dmarc_txt: c.cached_dmarc_txt.clone(),
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
) -> Result<(), EmailChallengeError> {
    // A genuine DNSSEC submission always carries at least the final
    // TXT hop. An empty `hops` set is malformed input on this path —
    // the FE that can't walk DNSSEC must call
    // `resolve_via_doh` instead. Reject it with the dedicated
    // `EmptyDkimLeafHops` so the malformed-request case is unambiguous
    // (distinct from a non-empty chain that failed to validate, which
    // is `DkimLeafMismatch`), rather than relying on the hop walk below.
    if hops.is_empty() {
        return Err(EmailChallengeError::EmptyDkimLeafHops);
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
    .map_err(|_| EmailChallengeError::DkimLeafMismatch)?;

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
    .map_err(|_| EmailChallengeError::DkimLeafMismatch)?;

    // Step 3: walk the hops as a CNAME → … → TXT resolution
    // anchored at the canister-pinned `<selector>._domainkey.<d>.`
    // FQDN. Any incoherence (wrong first hop, intermediate that
    // isn't a CNAME, bad target chaining, oversized chain, loop) is
    // surfaced as DkimLeafMismatch.
    let hops_internal: Vec<crate::dnssec::SignedRRset> =
        hops.iter().cloned().map(Into::into).collect();
    let expected_fqdn = format!(
        "{}._domainkey.{}.",
        snapshot.material.selector, snapshot.material.registered_domain
    );
    let expected_wire = crate::dnssec::wire::encode_dns_name_lowercase(&expected_fqdn)
        .map_err(|_| EmailChallengeError::DkimLeafMismatch)?;
    let verified = crate::dnssec::verify_hops_with_clock(
        &hops_internal,
        &zones,
        &crate::dnssec::DnsName(expected_wire),
        crate::dnssec::TYPE_TXT,
        now_secs,
    )
    .map_err(|_| EmailChallengeError::DkimLeafMismatch)?;

    let leaf_name = crate::dnssec::wire::decode_dns_name_lowercase(&verified.name.0);

    // Steps 4–6 (parse TXT → tag contract → signature → DMARC
    // alignment → From-match) are shared with the DoH fallback. The
    // DNSSEC path feeds the TXT it just authenticated through the hop
    // walk and the DMARC record cached at prepare time.
    let txt = crate::dnssec::wire::parse_txt_rdata(&verified.rdata).map_err(|_| {
        EmailChallengeError::EmailVerificationFailed("DNSSEC TXT RDATA truncated".into())
    })?;
    verify_dkim_key_against_partial(
        &txt,
        snapshot.cached_dmarc_txt.as_deref(),
        &snapshot.material,
        &leaf_name,
    )
}

/// Apply a DoH-path verdict to the pending entry the FE polls. Mirrors
/// [`finalize`], but sets the status instead of returning it (the FE polls),
/// and the recovery leg — whose delegation stamping is async — is detached so
/// the polling call returns immediately.
///
/// **Idempotency.** `resolve_via_doh` is polled, so two calls can both observe
/// a `Ready` cache. We clear `partial_verification` *before* the success path
/// does any `.await` (the recovery stamp is detached): a concurrent or
/// follow-up poll then sees no partial in [`VerifyMaterial::take_for_doh`] and
/// no-ops, so the credential is bound / delegation stamped exactly once.
fn finalize_via_doh(
    nonce: String,
    mat: VerifyMaterial,
    verification: Result<(), EmailChallengeError>,
    now_secs: u64,
) {
    match verification {
        Err(e) => {
            pending::with_mut(&nonce, now_secs, |c| {
                c.status = PendingStatus::Failed(e);
                c.partial_verification = None;
            });
        }
        Ok(()) => match &mat.kind {
            SnapshotKind::Register { anchor } => {
                let result = super::smtp::bind_credential(*anchor, &mat.claimed_address, now_secs);
                pending::with_mut(&nonce, now_secs, |c| {
                    c.partial_verification = None;
                    c.status = match result {
                        Ok(()) => PendingStatus::Succeeded,
                        Err(e) => PendingStatus::Failed(e),
                    };
                });
            }
            SnapshotKind::VerifyEmail { anchor } => {
                let result = super::smtp::append_or_refresh_verified_email(
                    *anchor,
                    &mat.claimed_address,
                    now_secs,
                );
                pending::with_mut(&nonce, now_secs, |c| {
                    c.partial_verification = None;
                    c.status = match result {
                        Ok(()) => PendingStatus::Succeeded,
                        Err(e) => PendingStatus::Failed(e),
                    };
                });
            }
            SnapshotKind::Recovery { session_pk } => {
                // Clear the partial up front so a concurrent/follow-up poll
                // can't also reach the stamp leg while this `.await` is in
                // flight (the success is set inside the spawned task).
                pending::with_mut(&nonce, now_secs, |c| c.partial_verification = None);
                let smtp_snapshot = super::smtp::recovery_snapshot(
                    mat.claimed_address.clone(),
                    mat.registered_domain.clone(),
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
    mat: &VerifyMaterial,
    context: &str,
) -> Result<(), EmailChallengeError> {
    // Step 4: parse the DKIM TXT, get the public key + key type.
    if dkim_txt.len() > super::MAX_DKIM_TXT_BYTES {
        return Err(EmailChallengeError::EmailVerificationFailed(format!(
            "DKIM TXT record at {context:?} is {} bytes; refusing to admit",
            dkim_txt.len()
        )));
    }
    let txt_str = std::str::from_utf8(dkim_txt).map_err(|_| {
        EmailChallengeError::EmailVerificationFailed("DKIM TXT is not valid UTF-8".into())
    })?;
    let dns_record = crate::dkim::parse_dkim_txt(txt_str).map_err(|e| {
        EmailChallengeError::EmailVerificationFailed(format!("DKIM DNS record: {e}"))
    })?;

    // DNS-record-dependent DKIM tag contract (design §5.4). Routes
    // through the same shared umbrella the DoH path calls so the
    // `t=y` and AUID-alignment policies stay in lock-step across
    // pipelines. The trail the umbrella builds is the DoH-side
    // diagnostic; we discard it here because this path collapses
    // every failure into a single `EmailChallengeError`.
    if let Err((reason, _trail)) = crate::dkim::enforce_dns_record_tag_contract(
        &mat.signing_auid,
        &mat.signing_domain,
        &dns_record,
    ) {
        return Err(EmailChallengeError::EmailVerificationFailed(format!(
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
        mat.algorithm,
        dns_record.key_type,
        &dns_record.public_key,
        &mat.headers_digest,
        &mat.signature,
    );
    match outcome {
        VerifyOutcome::Valid => {}
        VerifyOutcome::BadSignature => {
            return Err(EmailChallengeError::EmailVerificationFailed(
                "signature did not validate against public key".into(),
            ));
        }
        VerifyOutcome::MalformedKey(e) => {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
                "malformed DKIM key: {e}"
            )));
        }
        VerifyOutcome::MalformedSignature(e) => {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
                "malformed DKIM signature: {e}"
            )));
        }
        VerifyOutcome::AlgorithmMismatch => {
            return Err(EmailChallengeError::EmailVerificationFailed(
                "DKIM key type does not match signature algorithm".into(),
            ));
        }
        VerifyOutcome::RsaKeyTooSmall(bits) => {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
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
    let from_domain = mat
        .from_address_lc
        .rsplit_once('@')
        .map(|(_, d)| d.to_string())
        .ok_or(EmailChallengeError::AddressMismatch)?;
    if let Some(dmarc_bytes) = dmarc_txt {
        let dmarc_str = std::str::from_utf8(dmarc_bytes).map_err(|_| {
            EmailChallengeError::EmailVerificationFailed("DMARC TXT is not valid UTF-8".into())
        })?;
        let dmarc = crate::dmarc::parse_dmarc_txt(dmarc_str).map_err(|e| {
            EmailChallengeError::EmailVerificationFailed(format!("DMARC parse: {e}"))
        })?;
        if !crate::dmarc::aligns(&mat.signing_domain, &from_domain, dmarc.adkim) {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
                "DKIM d={} does not align with From={} under adkim={:?}",
                mat.signing_domain, from_domain, dmarc.adkim
            )));
        }
    } else {
        // No DMARC published — strict equality.
        if !mat.signing_domain.eq_ignore_ascii_case(&from_domain) {
            return Err(EmailChallengeError::EmailVerificationFailed(format!(
                "no DMARC published; DKIM d={} must equal From={}",
                mat.signing_domain, from_domain
            )));
        }
    }

    // From: matches the claimed address. smtp.rs already confirmed
    // this when caching `from_address_lc` on the partial verification
    // record, but pin again as defense-in-depth.
    if !mat
        .from_address_lc
        .eq_ignore_ascii_case(&mat.claimed_address)
    {
        return Err(EmailChallengeError::AddressMismatch);
    }

    Ok(())
}

// Tests for the DNS-record tag contract live alongside the umbrella
// in `crate::dkim::tag_checks::tests`. The DNSSEC submit-side mapping
// of `(VerificationFailReason, _) → EmailChallengeError::
// EmailVerificationFailed` is a trivial one-liner; the DNSSEC-prepare
// half of the same pattern (the `?` mapping inside
// `prepare_partial_verification`) is exercised end-to-end by
// `email_recovery::smtp::tests::dnssec_prepare_rejects_*`.
