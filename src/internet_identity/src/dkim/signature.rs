//! DKIM signature verification.
//!
//! Two algorithms per RFC 8624:
//! - `rsa-sha256` (RFC 5702 / 6376 §3.3.1): RSASSA-PKCS1-v1_5 with
//!   SHA-256, public key encoded as `SubjectPublicKeyInfo` (RFC 5280
//!   §4.1.2.7) wrapping the PKCS#1 RSAPublicKey.
//! - `ed25519-sha256` (RFC 8463): pure Ed25519 over the SHA-256 of the
//!   header hash input. The public key is 32 raw bytes — same format as
//!   the underlying ed25519-dalek `VerifyingKey`.
//!
//! Both rely on RustCrypto crates already in the workspace
//! (`rsa`, `sha2`, `ed25519-dalek`), so no new dependency surface.

use super::dns_record::KeyType;
use super::types::Algorithm;
use rsa::pkcs1v15::Signature as RsaSignature;
use rsa::pkcs1v15::VerifyingKey as RsaVerifyingKey;
use rsa::pkcs8::DecodePublicKey;
#[allow(unused_imports)]
use rsa::signature::Verifier as RsaVerifierTrait;
use rsa::traits::PublicKeyParts;
use rsa::RsaPublicKey;
use sha2::{Digest, Sha256};

/// Verification outcome for a single signature.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VerifyOutcome {
    /// Signature is cryptographically valid against the supplied key.
    Valid,
    /// Signature did not validate.
    BadSignature,
    /// The public key bytes are structurally malformed (bad SPKI,
    /// truncated Ed25519 point, etc).
    MalformedKey(String),
    /// The signature bytes are structurally malformed (wrong length,
    /// unparseable encoding, etc).
    MalformedSignature(String),
    /// The signature/key combination uses an algorithm/key-type pair we
    /// don't accept (e.g. signature `rsa-sha256` against `k=ed25519`).
    AlgorithmMismatch,
    /// The RSA key is below the configured minimum size (see
    /// [`RSA_MIN_KEY_BITS`]).
    RsaKeyTooSmall(usize),
}

/// Minimum RSA key size in bits, enforced at signature verification.
/// NIST SP 800-131A deprecated RSA-1024 in 2014 and every mainstream
/// sender we care about (Gmail, Outlook, iCloud, Yahoo, Proton,
/// Fastmail) publishes RSA-2048 or larger; M3AAWG recommends 2048 as
/// the production minimum. A sender stuck on 1024 surfaces as
/// `VerifyOutcome::RsaKeyTooSmall(1024)` and must rotate before
/// recovery binding will succeed.
pub const RSA_MIN_KEY_BITS: usize = 2048;

/// Verify `signature` against `signed_data` using the public key bytes
/// in `key_bytes`. The caller must already have parsed the algorithm
/// from the DKIM-Signature header and the key type from the DNS record;
/// we double-check the pair matches before spending the cryptographic
/// budget.
pub fn verify_signature(
    algorithm: Algorithm,
    key_type: KeyType,
    key_bytes: &[u8],
    signed_data: &[u8],
    signature: &[u8],
) -> VerifyOutcome {
    if !key_type.matches_signature_alg(algorithm) {
        return VerifyOutcome::AlgorithmMismatch;
    }
    match algorithm {
        Algorithm::RsaSha256 => verify_rsa_sha256(key_bytes, signed_data, signature),
        Algorithm::Ed25519Sha256 => verify_ed25519_sha256(key_bytes, signed_data, signature),
    }
}

/// Like [`verify_signature`] but takes a pre-computed SHA-256 digest
/// of the signed data instead of the raw bytes. Used by the DNSSEC
/// recovery path, which caches only the digest at prepare time
/// (design §8.4 drops the body once `bh=` validates).
///
/// RSA goes through PKCS#1 v1.5's `verify_prehash`; Ed25519 — per
/// RFC 8463 — already signs over `SHA256(signed_data)`, so the
/// cached digest *is* the correct verify input.
pub fn verify_signature_prehashed(
    algorithm: Algorithm,
    key_type: KeyType,
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> VerifyOutcome {
    if !key_type.matches_signature_alg(algorithm) {
        return VerifyOutcome::AlgorithmMismatch;
    }
    match algorithm {
        Algorithm::RsaSha256 => verify_rsa_sha256_prehashed(key_bytes, digest, signature),
        Algorithm::Ed25519Sha256 => verify_ed25519_prehashed(key_bytes, digest, signature),
    }
}

/// Decode the SPKI-DER public key, gate on the size floor, and
/// decode the signature bytes — the prefix every RSA-SHA256 verify
/// path runs before its final `verify`/`verify_prehash` call.
fn prepare_rsa_verifier(
    key_bytes: &[u8],
    signature: &[u8],
) -> Result<(RsaVerifyingKey<Sha256>, RsaSignature), VerifyOutcome> {
    // DKIM publishes RSA keys in SubjectPublicKeyInfo (DER) — the
    // X.509 wrapper around PKCS#1 RSAPublicKey. RustCrypto's
    // `DecodePublicKey::from_public_key_der` does the right thing.
    let key = RsaPublicKey::from_public_key_der(key_bytes)
        .map_err(|e| VerifyOutcome::MalformedKey(format!("RSA SPKI decode: {e}")))?;
    let bits = key.n().bits();
    if bits < RSA_MIN_KEY_BITS {
        return Err(VerifyOutcome::RsaKeyTooSmall(bits));
    }
    if signature.len() != key.size() {
        return Err(VerifyOutcome::MalformedSignature(format!(
            "RSA signature length {} != modulus length {}",
            signature.len(),
            key.size()
        )));
    }
    let sig = RsaSignature::try_from(signature)
        .map_err(|e| VerifyOutcome::MalformedSignature(format!("RSA sig decode: {e}")))?;
    let verifying_key = RsaVerifyingKey::<Sha256>::new(key);
    Ok((verifying_key, sig))
}

fn verify_rsa_sha256(key_bytes: &[u8], signed_data: &[u8], signature: &[u8]) -> VerifyOutcome {
    let (verifying_key, sig) = match prepare_rsa_verifier(key_bytes, signature) {
        Ok(v) => v,
        Err(outcome) => return outcome,
    };
    match verifying_key.verify(signed_data, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

fn verify_rsa_sha256_prehashed(
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> VerifyOutcome {
    use rsa::signature::hazmat::PrehashVerifier;
    let (verifying_key, sig) = match prepare_rsa_verifier(key_bytes, signature) {
        Ok(v) => v,
        Err(outcome) => return outcome,
    };
    match verifying_key.verify_prehash(digest, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

/// Decode the Ed25519 public key and signature bytes into the
/// ed25519-dalek types — the prefix shared between the raw and the
/// prehashed verify paths.
fn prepare_ed25519_verifier(
    key_bytes: &[u8],
    signature: &[u8],
) -> Result<(ed25519_dalek::VerifyingKey, ed25519_dalek::Signature), VerifyOutcome> {
    use ed25519_dalek::{Signature as Ed25519Signature, VerifyingKey};

    let key_array: [u8; 32] = key_bytes.try_into().map_err(|_| {
        VerifyOutcome::MalformedKey(format!("Ed25519 key length {} != 32", key_bytes.len()))
    })?;
    let sig_array: [u8; 64] = signature.try_into().map_err(|_| {
        VerifyOutcome::MalformedSignature(format!(
            "Ed25519 signature length {} != 64",
            signature.len()
        ))
    })?;
    let verifying_key = VerifyingKey::from_bytes(&key_array)
        .map_err(|e| VerifyOutcome::MalformedKey(format!("Ed25519 key: {e}")))?;
    let sig = Ed25519Signature::from_bytes(&sig_array);
    Ok((verifying_key, sig))
}

fn verify_ed25519_sha256(key_bytes: &[u8], signed_data: &[u8], signature: &[u8]) -> VerifyOutcome {
    use ed25519_dalek::Verifier as _;
    let (verifying_key, sig) = match prepare_ed25519_verifier(key_bytes, signature) {
        Ok(v) => v,
        Err(outcome) => return outcome,
    };
    // RFC 8463 signs over SHA256(signed_data); ed25519-dalek's
    // `verify` is *pure* Ed25519, so we wrap with SHA-256 ourselves.
    let mut h = Sha256::new();
    h.update(signed_data);
    let digest = h.finalize();
    match verifying_key.verify(&digest, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

fn verify_ed25519_prehashed(
    key_bytes: &[u8],
    digest: &[u8; 32],
    signature: &[u8],
) -> VerifyOutcome {
    use ed25519_dalek::Verifier as _;
    let (verifying_key, sig) = match prepare_ed25519_verifier(key_bytes, signature) {
        Ok(v) => v,
        Err(outcome) => return outcome,
    };
    // RFC 8463: Ed25519 signs over `SHA256(signed_data)`; the cached
    // digest IS the correct verify input.
    match verifying_key.verify(digest, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

/// Compute the SHA-256 of the canonical body, optionally truncated to
/// `l=` bytes per RFC 6376 §3.4.5. Returns the 32-byte digest.
pub fn body_hash_sha256(canonical_body: &[u8], l_bytes: Option<u64>) -> [u8; 32] {
    let truncated = match l_bytes {
        None => canonical_body,
        Some(l) => {
            let limit = l.min(canonical_body.len() as u64) as usize;
            &canonical_body[..limit]
        }
    };
    let mut h = Sha256::new();
    h.update(truncated);
    h.finalize().into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn algorithm_mismatch_short_circuits() {
        // RSA signature against ed25519 key
        let outcome = verify_signature(
            Algorithm::RsaSha256,
            KeyType::Ed25519,
            &[0u8; 32],
            b"x",
            &[0u8; 64],
        );
        assert_eq!(outcome, VerifyOutcome::AlgorithmMismatch);

        // Ed25519 signature against rsa key
        let outcome = verify_signature(
            Algorithm::Ed25519Sha256,
            KeyType::Rsa,
            &[0u8; 256],
            b"x",
            &[0u8; 64],
        );
        assert_eq!(outcome, VerifyOutcome::AlgorithmMismatch);
    }

    #[test]
    fn body_hash_with_no_l_hashes_full_body() {
        let body = b"hello\r\n";
        let h = body_hash_sha256(body, None);
        // Cross-checked via `printf 'hello\r\n' | sha256sum`.
        let expected: [u8; 32] =
            hex_literal("cd2eca3535741f27a8ae40c31b0c41d4057a7a7b912b33b9aed86485d1c84676");
        assert_eq!(h, expected);
    }

    #[test]
    fn body_hash_with_l_truncates() {
        let body = b"hello world\r\n";
        let h_truncated = body_hash_sha256(body, Some(5));
        let h_full = body_hash_sha256(b"hello", None);
        assert_eq!(h_truncated, h_full);
    }

    #[test]
    fn body_hash_l_larger_than_body_uses_full_body() {
        let body = b"x";
        let h_capped = body_hash_sha256(body, Some(1_000));
        let h_uncapped = body_hash_sha256(body, None);
        assert_eq!(h_capped, h_uncapped);
    }

    #[test]
    fn rsa_malformed_key_is_caught() {
        let outcome = verify_signature(
            Algorithm::RsaSha256,
            KeyType::Rsa,
            &[0u8; 16],
            b"x",
            &[0u8; 256],
        );
        assert!(matches!(outcome, VerifyOutcome::MalformedKey(_)));
    }

    #[test]
    fn ed25519_wrong_key_length_is_caught() {
        let outcome = verify_signature(
            Algorithm::Ed25519Sha256,
            KeyType::Ed25519,
            &[0u8; 16],
            b"x",
            &[0u8; 64],
        );
        assert!(matches!(outcome, VerifyOutcome::MalformedKey(_)));
    }

    #[test]
    fn ed25519_wrong_signature_length_is_caught() {
        let outcome = verify_signature(
            Algorithm::Ed25519Sha256,
            KeyType::Ed25519,
            &[0u8; 32],
            b"x",
            &[0u8; 16],
        );
        assert!(matches!(outcome, VerifyOutcome::MalformedSignature(_)));
    }

    fn hex_literal(s: &str) -> [u8; 32] {
        let bytes = hex::decode(s).expect("valid hex");
        bytes.try_into().expect("32 bytes")
    }

    // Boundary guards for `RSA_MIN_KEY_BITS`: a key below the floor
    // must be rejected with the exact bit count surfaced; a key at
    // the floor must pass the size gate (verification then falls
    // through to `BadSignature` on our bogus signature, which is
    // distinguishable from `RsaKeyTooSmall`). The at-floor happy
    // path with a real signature is covered end-to-end by
    // `email_recovery::full_setup_flow_binds_credential_to_anchor`.

    #[test]
    fn rsa_below_floor_is_rejected_with_actual_bits() {
        use rsa::pkcs8::EncodePublicKey;
        use rsa::{RsaPrivateKey, RsaPublicKey};

        let mut rng = rand::rngs::OsRng;
        let private_key = RsaPrivateKey::new(&mut rng, 1024).expect("RSA 1024-bit keygen");
        let public_key = RsaPublicKey::from(&private_key);
        // Read the modulus bit length back through the same
        // `PublicKeyParts::n()` call the verifier uses, so the
        // assertion matches what the verifier saw.
        let actual_bits = public_key.n().bits();
        let spki_der = public_key.to_public_key_der().expect("encode SPKI DER");

        let outcome = verify_signature(
            Algorithm::RsaSha256,
            KeyType::Rsa,
            spki_der.as_bytes(),
            b"any signed data",
            // 128-byte sig matches the 1024-bit modulus; needed only
            // to dodge `MalformedSignature` since the size gate runs
            // before the length check.
            &[0u8; 128],
        );

        assert_eq!(outcome, VerifyOutcome::RsaKeyTooSmall(actual_bits));
    }

    #[test]
    fn rsa_at_floor_passes_size_gate() {
        use rsa::pkcs8::EncodePublicKey;
        use rsa::{RsaPrivateKey, RsaPublicKey};

        let mut rng = rand::rngs::OsRng;
        let private_key = RsaPrivateKey::new(&mut rng, 2048).expect("RSA 2048-bit keygen");
        let public_key = RsaPublicKey::from(&private_key);
        let spki_der = public_key.to_public_key_der().expect("encode SPKI DER");

        let outcome = verify_signature(
            Algorithm::RsaSha256,
            KeyType::Rsa,
            spki_der.as_bytes(),
            b"any signed data",
            // 256-byte sig matches the 2048-bit modulus.
            &[0u8; 256],
        );

        // `BadSignature` (vs `RsaKeyTooSmall`) proves the size gate
        // accepted the at-floor key.
        assert_eq!(
            outcome,
            VerifyOutcome::BadSignature,
            "expected size gate to pass; got {outcome:?}"
        );
    }

    #[test]
    fn prehashed_path_enforces_the_same_rsa_floor() {
        use rsa::pkcs8::EncodePublicKey;
        use rsa::{RsaPrivateKey, RsaPublicKey};

        let mut rng = rand::rngs::OsRng;
        let private_key = RsaPrivateKey::new(&mut rng, 1024).expect("RSA 1024-bit keygen");
        let public_key = RsaPublicKey::from(&private_key);
        let actual_bits = public_key.n().bits();
        let spki_der = public_key.to_public_key_der().expect("encode SPKI DER");

        let outcome = verify_signature_prehashed(
            Algorithm::RsaSha256,
            KeyType::Rsa,
            spki_der.as_bytes(),
            &[0u8; 32],
            &[0u8; 128],
        );

        assert_eq!(outcome, VerifyOutcome::RsaKeyTooSmall(actual_bits));
    }
}
