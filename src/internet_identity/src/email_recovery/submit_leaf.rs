//! `email_recovery_submit_dkim_leaf` — second-phase DNSSEC leaf
//! submission.
//!
//! On the DNSSEC path, `smtp_request` parsed the DKIM-Signature
//! header, validated the body hash, and stashed a small
//! `PartialVerification` record on the pending challenge. It then
//! flipped the polled status to `NeedDkimLeaf { selector }` so the
//! FE could walk DNSSEC for that one TXT and submit it back here.
//!
//! This call:
//!
//! 1. Looks up the pending challenge by nonce; rejects with
//!    `NoDkimLeafExpected` if the entry isn't in `NeedDkimLeaf`.
//! 2. Validates the FE-supplied `SignedRRset` against the cached
//!    deepest-zone DNSKEY (the chain itself was already validated
//!    at prepare time — see `verify_chain_with_clock`).
//! 3. Confirms the leaf's owner name is exactly
//!    `<expected_selector>._domainkey.<registered_domain>.`. Both
//!    sides come from canister-pinned state, never the FE.
//! 4. Parses the DKIM TXT, runs the cryptographic signature check
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

/// Body of `email_recovery_submit_dkim_leaf(arg)`. Returns the
/// post-call polling status — typically `RegistrationSucceeded` or
/// `Failed(reason)`. The FE can also poll
/// `email_recovery_status(nonce)` for the same answer; returning it
/// here saves the FE one round-trip on the happy path.
pub fn submit_dkim_leaf(
    arg: EmailRecoverySubmitDkimLeafArg,
    now_secs: u64,
) -> Result<EmailRecoveryStatus, EmailRecoveryError> {
    let EmailRecoverySubmitDkimLeafArg { nonce, dkim_leaf } = arg;

    // Snapshot everything we need under one borrow so the rest of
    // the function works against owned data. Includes early
    // rejection of "not a DNSSEC pending entry" / "not in
    // NeedDkimLeaf state" / etc.
    let snapshot = pending::with_mut(&nonce, now_secs, |c| Snapshot::take(c))
        .ok_or(EmailRecoveryError::NonceUnknown)??;

    let outcome = run_submit(&dkim_leaf, &snapshot, now_secs);
    match outcome {
        Ok(()) => {
            // Bind the credential and flip status. Re-borrow the
            // map; the entry still exists (we didn't await between
            // borrows, so no one else can mutate it).
            let bind_result = match &snapshot.kind {
                SnapshotKind::Register { anchor } => {
                    super::smtp::bind_credential(*anchor, &snapshot.claimed_address, now_secs)
                }
            };
            match bind_result {
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
                    });
                    Err(e)
                }
            }
        }
        Err(e) => {
            let cloned = e.clone();
            pending::with_mut(&nonce, now_secs, |c| {
                c.status = PendingStatus::Failed(cloned);
            });
            Err(e)
        }
    }
}

#[derive(Clone, Debug)]
struct Snapshot {
    kind: SnapshotKind,
    claimed_address: String,
    registered_domain: String,
    expected_selector: String,
    cached_zone_dnskey: crate::dnssec::SignedRRset,
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
        let cached_zone_dnskey = c
            .cached_zone_dnskey
            .as_ref()
            .ok_or(EmailRecoveryError::NoDkimLeafExpected)?
            .clone();
        let partial = c
            .partial_verification
            .as_ref()
            .ok_or(EmailRecoveryError::NoDkimLeafExpected)?;
        let kind = match &c.kind {
            PendingKind::Register { anchor } => SnapshotKind::Register { anchor: *anchor },
        };
        Ok(Snapshot {
            kind,
            claimed_address: c.claimed_address.clone(),
            registered_domain: c.registered_domain.clone(),
            expected_selector,
            cached_zone_dnskey,
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
    dkim_leaf: &internet_identity_interface::internet_identity::types::SignedRRset,
    snapshot: &Snapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    // Step 1: validate the FE-supplied leaf against the cached zone
    // DNSKEY. The cached DNSKEY was itself validated at prepare time
    // against the canister's trust anchors; the chain is implicit
    // here.
    let leaf_internal: crate::dnssec::SignedRRset = dkim_leaf.clone().into();
    let verified = crate::dnssec::verify_leaf_against_dnskey(
        &leaf_internal,
        &snapshot.cached_zone_dnskey,
        now_secs,
    )
    .map_err(|_| EmailRecoveryError::DkimLeafMismatch)?;

    // Step 2: confirm the leaf's owner name and rtype.
    if verified.rtype != crate::dnssec::types::TYPE_TXT {
        return Err(EmailRecoveryError::DkimLeafMismatch);
    }
    let leaf_name = decode_dns_name_lowercase(&verified.name.0);
    let expected_fqdn = format!(
        "{}._domainkey.{}.",
        snapshot.expected_selector, snapshot.registered_domain
    );
    if !leaf_name.eq_ignore_ascii_case(&expected_fqdn) {
        return Err(EmailRecoveryError::DkimLeafMismatch);
    }

    // Step 3: parse the DKIM TXT, get the public key + key type.
    let txt = parse_txt_rdata(&verified.rdata)?;
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

    // Step 4: complete the cryptographic signature check.
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

    // Step 5: DMARC alignment. The smtp.rs path enforced that the
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

/// Concatenate one or more TXT character-strings (each prefixed by
/// a length octet) into the bytes the DKIM verifier expects.
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
