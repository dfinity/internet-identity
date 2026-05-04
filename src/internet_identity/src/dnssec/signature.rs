//! Signature verification and DS digest matching.
//!
//! Each `verify_*` function takes the signed-data bytes and a DNSKEY
//! RDATA and returns Ok(()) iff the signature checks out under that key.
//! Algorithm dispatch by the RRSIG's `algorithm` field happens in
//! `verify_signature_for_alg`.
//!
//! Algorithm coverage (RFC 8624 MUST):
//! - 8 — RSA-SHA256 (RFC 5702): used by the DNS root, by `com.`, and by
//!   most legacy zones.
//! - 13 — ECDSA-P256-SHA256 (RFC 6605): used by most TLDs and modern
//!   zones (Cloudflare, Google's Cloud DNS).
//! - 15 — Ed25519 (RFC 8080): rare in production today but mandatory.

use super::canonical::ds_digest_input;
use super::types::DnssecError;
// The verify(...) calls on each verifying key resolve to inherent
// methods, but bringing the `signature::Verifier` trait into scope
// makes the dispatch unambiguous — without the trait imports, future
// upstream changes that rename the inherent method would silently
// break the build. Marked unused-imports-allow because the explicit
// `as _` aliasing doesn't silence the warning when the trait method
// is shadowed by an inherent of the same name.
#[allow(unused_imports)]
use ed25519_dalek::Verifier as Ed25519VerifierTrait;
#[allow(unused_imports)]
use p256::ecdsa::signature::Verifier as P256VerifierTrait;
use rsa::pkcs1v15::Signature as RsaSignature;
use rsa::pkcs1v15::VerifyingKey as RsaVerifyingKey;
#[allow(unused_imports)]
use rsa::signature::Verifier as RsaVerifierTrait;
use rsa::traits::PublicKeyParts;
use rsa::{BigUint, RsaPublicKey};
use sha2::{Digest, Sha256};

const ALG_RSA_SHA256: u8 = 8;
const ALG_ECDSA_P256_SHA256: u8 = 13;
const ALG_ED25519: u8 = 15;

const DIGEST_TYPE_SHA256: u8 = 2;

/// Dispatch an RRSIG signature check to the appropriate algorithm
/// implementation. `dnskey_rdata` is the entire DNSKEY RDATA (Flags |
/// Protocol | Algorithm | Public Key); the Public Key sub-field is
/// extracted internally and parsed per the algorithm's key format.
pub fn verify_signature_for_alg(
    algorithm: u8,
    signed_data: &[u8],
    signature: &[u8],
    dnskey_rdata: &[u8],
) -> Result<(), DnssecError> {
    let public_key = dnskey_public_key(dnskey_rdata)?;
    match algorithm {
        ALG_RSA_SHA256 => verify_rsa_sha256(signed_data, signature, public_key),
        ALG_ECDSA_P256_SHA256 => {
            verify_ecdsa_p256_sha256(signed_data, signature, public_key)
        }
        ALG_ED25519 => verify_ed25519(signed_data, signature, public_key),
        other => Err(DnssecError::UnsupportedAlgorithm(other)),
    }
}

/// Extract the Public Key sub-field of a DNSKEY RDATA (RFC 4034 §2.1):
/// Flags (2) | Protocol (1) | Algorithm (1) | Public Key (variable).
fn dnskey_public_key(dnskey_rdata: &[u8]) -> Result<&[u8], DnssecError> {
    if dnskey_rdata.len() < 4 {
        return Err(DnssecError::Malformed("DNSKEY RDATA shorter than 4 bytes"));
    }
    Ok(&dnskey_rdata[4..])
}

/// RFC 5702: signature is a fixed-length blob; public key is encoded in
/// RFC 3110 form: `exponent_length || exponent || modulus`. The exponent
/// length is one byte (1..=255) or, if zero, two bytes for an extended
/// length (covering the rare e > 255 bytes case).
fn verify_rsa_sha256(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    let (e, n) = parse_rfc3110_rsa_key(public_key)?;
    let pk = RsaPublicKey::new(BigUint::from_bytes_be(n), BigUint::from_bytes_be(e))
        .map_err(|_| DnssecError::Malformed("invalid RSA public key"))?;

    // pkcs1v15 size sanity: signature length must equal the modulus byte
    // length. Otherwise rsa::Signature::try_from returns an error which
    // we'd pass through, but checking here gives a clearer failure mode.
    let expected_sig_len = pk.size();
    if signature.len() != expected_sig_len {
        return Err(DnssecError::Malformed(
            "RSA signature length != modulus length",
        ));
    }

    let verifying_key = RsaVerifyingKey::<Sha256>::new(pk);
    let sig = RsaSignature::try_from(signature)
        .map_err(|_| DnssecError::Malformed("invalid RSA signature encoding"))?;
    verifying_key
        .verify(signed_data, &sig)
        .map_err(|_| DnssecError::BadSignature)
}

fn parse_rfc3110_rsa_key(key: &[u8]) -> Result<(&[u8], &[u8]), DnssecError> {
    if key.is_empty() {
        return Err(DnssecError::Malformed("empty RSA public key"));
    }
    let (exp_len, off): (usize, usize) = if key[0] != 0 {
        (key[0] as usize, 1)
    } else {
        if key.len() < 3 {
            return Err(DnssecError::Malformed("truncated RSA exp_len"));
        }
        (u16::from_be_bytes([key[1], key[2]]) as usize, 3)
    };
    if off + exp_len >= key.len() {
        return Err(DnssecError::Malformed("RSA key shorter than declared"));
    }
    let exponent = &key[off..off + exp_len];
    let modulus = &key[off + exp_len..];
    Ok((exponent, modulus))
}

/// RFC 6605: signature is `r || s` (32 + 32 = 64 bytes). The DNSKEY
/// public key is `X || Y` (32 + 32 = 64 bytes), the uncompressed P-256
/// point without the 0x04 prefix that RustCrypto expects on
/// `EncodedPoint`.
fn verify_ecdsa_p256_sha256(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    use p256::ecdsa::{Signature as P256Signature, VerifyingKey as P256VerifyingKey};
    use p256::EncodedPoint;

    if public_key.len() != 64 {
        return Err(DnssecError::Malformed(
            "ECDSA P-256 DNSKEY public key not 64 bytes",
        ));
    }
    if signature.len() != 64 {
        return Err(DnssecError::Malformed("ECDSA P-256 signature not 64 bytes"));
    }

    let mut uncompressed = [0u8; 65];
    uncompressed[0] = 0x04;
    uncompressed[1..].copy_from_slice(public_key);
    let point = EncodedPoint::from_bytes(uncompressed)
        .map_err(|_| DnssecError::Malformed("invalid ECDSA P-256 point"))?;
    let verifying_key = P256VerifyingKey::from_encoded_point(&point)
        .map_err(|_| DnssecError::Malformed("ECDSA P-256 point not on curve"))?;
    let sig = P256Signature::from_slice(signature)
        .map_err(|_| DnssecError::Malformed("invalid ECDSA P-256 signature"))?;
    verifying_key
        .verify(signed_data, &sig)
        .map_err(|_| DnssecError::BadSignature)
}

/// RFC 8080: Ed25519 public key is 32 bytes, signature is 64 bytes.
fn verify_ed25519(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    use ed25519_dalek::{Signature as Ed25519Signature, VerifyingKey as Ed25519VerifyingKey};

    let pk_array: [u8; 32] = public_key
        .try_into()
        .map_err(|_| DnssecError::Malformed("Ed25519 public key not 32 bytes"))?;
    let sig_array: [u8; 64] = signature
        .try_into()
        .map_err(|_| DnssecError::Malformed("Ed25519 signature not 64 bytes"))?;
    let verifying_key = Ed25519VerifyingKey::from_bytes(&pk_array)
        .map_err(|_| DnssecError::Malformed("invalid Ed25519 public key"))?;
    let sig = Ed25519Signature::from_bytes(&sig_array);
    verifying_key
        .verify(signed_data, &sig)
        .map_err(|_| DnssecError::BadSignature)
}

/// Compute the DS-style digest of a candidate child KSK and check it
/// against the parent's DS RDATA: per RFC 4034 §5.1, the DS RDATA is
/// `key_tag (2) | algorithm (1) | digest_type (1) | digest (variable)`.
/// Returns true iff:
/// - the digest_type matches one we support (currently only SHA-256);
/// - the digest of `(canonical_owner_name | dnskey_rdata)` matches the DS digest.
pub fn ds_matches_dnskey(
    child_zone_name: &[u8],
    dnskey_rdata: &[u8],
    ds_rdata: &[u8],
) -> bool {
    if ds_rdata.len() < 4 {
        return false;
    }
    let digest_type = ds_rdata[3];
    let claimed_digest = &ds_rdata[4..];
    match digest_type {
        DIGEST_TYPE_SHA256 => {
            let input = ds_digest_input(child_zone_name, dnskey_rdata);
            let mut h = Sha256::new();
            h.update(&input);
            let computed = h.finalize();
            &computed[..] == claimed_digest
        }
        _ => false, // SHA-1 (1) and other legacy digests rejected by construction.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_short_rsa_exponent() {
        // 3-byte exponent (0x010001) + small modulus marker
        let key = vec![0x03, 0x01, 0x00, 0x01, 0xab, 0xcd];
        let (e, n) = parse_rfc3110_rsa_key(&key).unwrap();
        assert_eq!(e, &[0x01, 0x00, 0x01]);
        assert_eq!(n, &[0xab, 0xcd]);
    }

    #[test]
    fn parses_extended_rsa_exponent() {
        // 0x00 marker + 2-byte length 0x0003 + exponent 0x010001 + modulus
        let key = vec![0x00, 0x00, 0x03, 0x01, 0x00, 0x01, 0xab, 0xcd];
        let (e, n) = parse_rfc3110_rsa_key(&key).unwrap();
        assert_eq!(e, &[0x01, 0x00, 0x01]);
        assert_eq!(n, &[0xab, 0xcd]);
    }

    #[test]
    fn unsupported_algorithm_returns_error() {
        let dnskey = vec![0u8; 4]; // header only, public key empty
        let signed = b"hello";
        let sig = vec![0u8; 32];
        let result = verify_signature_for_alg(5, signed, &sig, &dnskey);
        assert_eq!(result, Err(DnssecError::UnsupportedAlgorithm(5)));
    }
}
