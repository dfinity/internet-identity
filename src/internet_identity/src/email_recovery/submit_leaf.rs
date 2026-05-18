//! `email_recovery_submit_dkim_leaf` — second-phase DNSSEC leaf
//! submission.
//!
//! On the DNSSEC path, `smtp_request` parsed the DKIM-Signature
//! header, validated the body hash, and stashed a small
//! `PartialVerification` record on the pending challenge. It then
//! flipped the polled status to `NeedDkimLeaf { selector }` so the
//! FE could walk DNSSEC for that selector and submit the resulting
//! `(hops, extra_chains)` bundle back here.
//!
//! This call:
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
//! See design doc §8.4 / §8.5.

use super::pending::{PendingKind, PendingStatus};
use crate::email_recovery::pending;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryError, EmailRecoveryStatus, EmailRecoverySubmitDkimLeafArg,
};
use internet_identity_interface::internet_identity::types::SessionKey;

/// Body of `email_recovery_submit_dkim_leaf(arg)`. Returns the
/// post-call polling status — `RegistrationSucceeded` for the setup
/// flow, `RecoveryReady{...}` for the recovery flow, or
/// `Failed(reason)`. The FE can also poll
/// `email_recovery_status(nonce)` for the same answer; returning it
/// here saves the FE one round-trip on the happy path.
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

    if let Err(e) = run_submit(&hops, &extra_chains, &snapshot, now_secs) {
        let cloned = e.clone();
        pending::with_mut(&nonce, now_secs, |c| {
            c.status = PendingStatus::Failed(cloned);
            c.partial_verification = None;
        });
        return Err(e);
    }

    // Verification passed — finalize per kind. Setup binds the
    // credential, recovery stamps a delegation seed.
    match &snapshot.kind {
        SnapshotKind::Register { anchor } => {
            match super::smtp::bind_credential(*anchor, &snapshot.claimed_address, now_secs) {
                Ok(()) => {
                    pending::with_mut(&nonce, now_secs, |c| {
                        c.status = PendingStatus::Succeeded;
                        c.partial_verification = None;
                    });
                    Ok(EmailRecoveryStatus::RegistrationSucceeded)
                }
                Err(e) => {
                    let cloned = e.clone();
                    pending::with_mut(&nonce, now_secs, |c| {
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
                    pending::with_mut(&nonce, now_secs, |c| {
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
                    pending::with_mut(&nonce, now_secs, |c| {
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
    algorithm: crate::dkim::Algorithm,
    from_address_lc: String,
}

#[derive(Clone, Debug)]
enum SnapshotKind {
    Register {
        anchor: internet_identity_interface::internet_identity::types::AnchorNumber,
    },
    Recovery {
        session_pk: SessionKey,
    },
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
            algorithm: partial.algorithm,
            from_address_lc: partial.from_address_lc.clone(),
        })
    }
}

/// The leaf-validation + signature-verification + alignment-check
/// pipeline. Returns `Ok(())` on full success.
fn run_submit(
    hops: &[internet_identity_interface::internet_identity::types::SignedRRset],
    extra_chains: &[internet_identity_interface::internet_identity::types::DelegationChain],
    snapshot: &Snapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
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

    // Step 4: parse the DKIM TXT, get the public key + key type.
    let txt = crate::dnssec::wire::parse_txt_rdata(&verified.rdata).map_err(|_| {
        EmailRecoveryError::EmailVerificationFailed("DNSSEC TXT RDATA truncated".into())
    })?;
    if txt.len() > super::MAX_DKIM_TXT_BYTES {
        return Err(EmailRecoveryError::EmailVerificationFailed(format!(
            "DKIM TXT record at {leaf_name:?} is {} bytes; refusing to admit",
            txt.len()
        )));
    }
    let txt_str = std::str::from_utf8(&txt).map_err(|_| {
        EmailRecoveryError::EmailVerificationFailed("DKIM TXT is not valid UTF-8".into())
    })?;
    let dns_record = crate::dkim::parse_dkim_txt(txt_str).map_err(|e| {
        EmailRecoveryError::EmailVerificationFailed(format!("DKIM DNS record: {e}"))
    })?;

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
    let outcome = verify_with_prehash(
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
    // satisfied. We re-check explicitly here against the cached
    // DMARC record (when one was supplied at prepare time).
    let from_domain = snapshot
        .from_address_lc
        .rsplit_once('@')
        .map(|(_, d)| d.to_string())
        .ok_or(EmailRecoveryError::AddressMismatch)?;
    if let Some(dmarc_bytes) = &snapshot.cached_dmarc_txt {
        let dmarc_str = std::str::from_utf8(dmarc_bytes).map_err(|_| {
            EmailRecoveryError::EmailVerificationFailed("cached DMARC is not valid UTF-8".into())
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

    // Step 6: From: matches the claimed address. smtp.rs already
    // confirmed this when caching `from_address_lc` on the partial
    // verification record, but pin again as defense-in-depth.
    if !snapshot
        .from_address_lc
        .eq_ignore_ascii_case(&snapshot.claimed_address)
    {
        return Err(EmailRecoveryError::AddressMismatch);
    }

    Ok(())
}

/// Verify a DKIM signature against a SHA-256-prehashed signed-data
/// digest. This is the second-phase verification entry point we
/// need on the recovery surface — `crate::dkim::verify_signature`
/// takes the raw `signed_data` and hashes it internally, so it
/// can't consume the cached 32-byte digest directly.
fn verify_with_prehash(
    algorithm: crate::dkim::Algorithm,
    key_type: crate::dkim::KeyType,
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> crate::dkim::VerifyOutcome {
    use crate::dkim::{Algorithm, VerifyOutcome};
    if !key_type.matches_signature_alg(algorithm) {
        return VerifyOutcome::AlgorithmMismatch;
    }
    match algorithm {
        Algorithm::RsaSha256 => verify_rsa_sha256_prehashed(key_bytes, digest, signature),
        Algorithm::Ed25519Sha256 => verify_ed25519_prehashed(key_bytes, digest, signature),
    }
}

fn verify_rsa_sha256_prehashed(
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> crate::dkim::VerifyOutcome {
    use crate::dkim::VerifyOutcome;
    use rsa::pkcs1v15::{Signature as RsaSignature, VerifyingKey as RsaVerifyingKey};
    use rsa::pkcs8::DecodePublicKey;
    use rsa::traits::PublicKeyParts;
    use rsa::RsaPublicKey;
    use sha2::Sha256;
    /// Minimum RSA key size in bits (matches `crate::dkim::signature::RSA_MIN_KEY_BITS`).
    const RSA_MIN_KEY_BITS: usize = 1024;

    let key = match RsaPublicKey::from_public_key_der(key_bytes) {
        Ok(k) => k,
        Err(e) => return VerifyOutcome::MalformedKey(format!("RSA SPKI decode: {e}")),
    };
    let bits = key.n().bits();
    if bits < RSA_MIN_KEY_BITS {
        return VerifyOutcome::RsaKeyTooSmall(bits);
    }
    if signature.len() != key.size() {
        return VerifyOutcome::MalformedSignature(format!(
            "RSA signature length {} != modulus length {}",
            signature.len(),
            key.size()
        ));
    }
    let verifying_key = RsaVerifyingKey::<Sha256>::new(key);
    let sig = match RsaSignature::try_from(signature) {
        Ok(s) => s,
        Err(e) => return VerifyOutcome::MalformedSignature(format!("RSA sig decode: {e}")),
    };
    use rsa::signature::hazmat::PrehashVerifier;
    match verifying_key.verify_prehash(digest, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

fn verify_ed25519_prehashed(
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> crate::dkim::VerifyOutcome {
    use crate::dkim::VerifyOutcome;
    use ed25519_dalek::{Signature as Ed25519Signature, Verifier as _, VerifyingKey};
    let key_array: [u8; 32] = match key_bytes.try_into() {
        Ok(arr) => arr,
        Err(_) => {
            return VerifyOutcome::MalformedKey(format!(
                "Ed25519 key length {} != 32",
                key_bytes.len()
            ));
        }
    };
    let sig_array: [u8; 64] = match signature.try_into() {
        Ok(arr) => arr,
        Err(_) => {
            return VerifyOutcome::MalformedSignature(format!(
                "Ed25519 signature length {} != 64",
                signature.len()
            ));
        }
    };
    let verifying_key = match VerifyingKey::from_bytes(&key_array) {
        Ok(k) => k,
        Err(e) => return VerifyOutcome::MalformedKey(format!("Ed25519 key: {e}")),
    };
    let sig = Ed25519Signature::from_bytes(&sig_array);
    // RFC 8463: Ed25519 over `SHA256(signed_data)`. The cached
    // digest IS the correct verify input.
    match verifying_key.verify(digest, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

