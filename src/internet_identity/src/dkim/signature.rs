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
    /// The RSA key is below the configured minimum size (1024 bits per
    /// design §5.6).
    RsaKeyTooSmall(usize),
}

/// Minimum RSA key size in bits. RFC 8301 deprecated keys below 1024;
/// design doc §5.6 keeps 1024 as the floor for v1 (with a planned lift
/// to 2048 once telemetry confirms zero impact on real senders we
/// care about).
pub const RSA_MIN_KEY_BITS: usize = 1024;

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

fn verify_rsa_sha256(key_bytes: &[u8], signed_data: &[u8], signature: &[u8]) -> VerifyOutcome {
    // DKIM publishes RSA keys in SubjectPublicKeyInfo (DER) — that's the
    // X.509 wrapper around PKCS#1 RSAPublicKey. RustCrypto's
    // `DecodePublicKey::from_public_key_der` does the right thing.
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
    match verifying_key.verify(signed_data, &sig) {
        Ok(()) => VerifyOutcome::Valid,
        Err(_) => VerifyOutcome::BadSignature,
    }
}

fn verify_ed25519_sha256(key_bytes: &[u8], signed_data: &[u8], signature: &[u8]) -> VerifyOutcome {
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

    // Per RFC 8463, the Ed25519 signature is computed over the SHA-256
    // hash of the header hash input — i.e. the same `signed_data`
    // RSA-SHA256 receives, but pre-hashed. ed25519-dalek's `verify` is
    // *pure* Ed25519 over the input; we wrap with SHA-256 ourselves.
    let mut h = Sha256::new();
    h.update(signed_data);
    let digest = h.finalize();

    match verifying_key.verify(&digest, &sig) {
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
}
