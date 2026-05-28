//! Signature verification and DS digest matching.
//!
//! Each `verify_*` function takes the canonical signed-data bytes plus
//! the DNSKEY RDATA of the candidate signer and returns `Ok(())` iff
//! the signature checks out under that key. Algorithm dispatch by the
//! RRSIG's `algorithm` field happens in `verify_signature_for_alg`.
//!
//! Algorithm coverage matches the RFC 8624 §3.1 MUST set:
//!
//! - `ALG_RSA_SHA256` — RFC 5702. Used by the DNS root, `com.`, and
//!   most legacy zones.
//! - `ALG_ECDSA_P256_SHA256` — RFC 6605. Used by most TLDs and modern
//!   zones (Cloudflare, Google, …).
//! - `ALG_ED25519` — RFC 8080. Rare in production today but MUST per
//!   RFC 8624; we cover it for completeness and forward compatibility.
//!
//! Anything outside this set returns `UnsupportedAlgorithm`. In
//! particular RSA-SHA1 (`ALG_RSA_SHA1` in the IANA registry, numeric
//! value 5) is explicitly *not* supported (RFC 8624 §3.1 lists it as
//! MUST NOT for new deployments).

use super::canonical::ds_digest_input;
use super::types::DnssecError;
use super::wire::{DNSKEY_RDATA_HEADER_LEN, DS_DIGEST_TYPE_SHA256, DS_RDATA_HEADER_LEN};
// The `verify(...)` calls below resolve to inherent methods on the
// individual verifying-key types. Bringing the `Verifier` traits
// into scope makes the dispatch unambiguous — without these imports
// a future upstream change that renames the inherent method would
// silently break the build. The `as _` alias isn't enough to silence
// the unused-imports lint when an inherent method of the same name
// shadows the trait method, hence the explicit allow.
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

// IANA DNSSEC algorithm numbers (RFC 8624 §3.1, registry maintained
// by IANA at <https://www.iana.org/assignments/dns-sec-alg-numbers>).
// Kept at module scope because `verify_signature_for_alg`, every
// per-algorithm function, and the tests all reference them.

/// RFC 5702 — RSA with SHA-256. RFC 3110 public-key encoding,
/// PKCS#1 v1.5 signature.
const ALG_RSA_SHA256: u8 = 8;
/// RFC 6605 — ECDSA over the NIST P-256 curve with SHA-256.
const ALG_ECDSA_P256_SHA256: u8 = 13;
/// RFC 8080 — Ed25519 (Curve25519, EdDSA).
const ALG_ED25519: u8 = 15;

/// Dispatch an RRSIG signature check to the appropriate algorithm
/// implementation. `dnskey_rdata` is the entire DNSKEY RDATA
/// (`Flags | Protocol | Algorithm | Public Key`, RFC 4034 §2.1); the
/// public-key sub-field is extracted and then parsed per the
/// algorithm's RFC-defined encoding.
pub fn verify_signature_for_alg(
    algorithm: u8,
    signed_data: &[u8],
    signature: &[u8],
    dnskey_rdata: &[u8],
) -> Result<(), DnssecError> {
    if dnskey_rdata.len() < DNSKEY_RDATA_HEADER_LEN {
        return Err(DnssecError::Malformed(
            "DNSKEY RDATA shorter than 4-byte header",
        ));
    }
    let public_key = &dnskey_rdata[DNSKEY_RDATA_HEADER_LEN..];

    match algorithm {
        ALG_RSA_SHA256 => verify_rsa_sha256(signed_data, signature, public_key),
        ALG_ECDSA_P256_SHA256 => verify_ecdsa_p256_sha256(signed_data, signature, public_key),
        ALG_ED25519 => verify_ed25519(signed_data, signature, public_key),
        other => Err(DnssecError::UnsupportedAlgorithm(other)),
    }
}

/// RFC 5702: signature is a PKCS#1 v1.5 RSA signature whose byte
/// length equals the modulus byte length. The public key is encoded
/// in RFC 3110 form (parsed by `parse_rfc3110_rsa_key` below).
fn verify_rsa_sha256(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    let (e, n) = parse_rfc3110_rsa_key(public_key)?;
    let pk = RsaPublicKey::new(BigUint::from_bytes_be(n), BigUint::from_bytes_be(e))
        .map_err(|_| DnssecError::Malformed("invalid RSA public key"))?;

    // RFC 8017 §8.2.2: a PKCS#1 v1.5 signature must be exactly as
    // long as the modulus. `rsa::Signature::try_from` would catch
    // this too, but we surface a clearer error here.
    if signature.len() != pk.size() {
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

/// Parse an RSA public key in RFC 3110 §2 DNSKEY encoding:
///
/// ```text
/// +---------+----------+---------+
/// | exp_len | exponent | modulus |
/// +---------+----------+---------+
/// ```
///
/// `exp_len` is one byte when non-zero; if the first byte is zero,
/// the next two bytes carry a 16-bit extended length. In practice
/// every real RSA DNSKEY uses `e = 65537` and the short form, but the
/// extended form is part of the spec and we handle it.
fn parse_rfc3110_rsa_key(key: &[u8]) -> Result<(&[u8], &[u8]), DnssecError> {
    /// Header length in bytes when the first byte directly carries
    /// a non-zero exponent length: just the single byte.
    const HEADER_LEN_SHORT: usize = 1;
    /// Header length in bytes when the first byte is zero and the
    /// next two bytes carry a 16-bit extended exponent length.
    const HEADER_LEN_EXTENDED: usize = 3;

    if key.is_empty() {
        return Err(DnssecError::Malformed("empty RSA public key"));
    }
    let (exp_len, header_len): (usize, usize) = if key[0] != 0 {
        (key[0] as usize, HEADER_LEN_SHORT)
    } else {
        if key.len() < HEADER_LEN_EXTENDED {
            return Err(DnssecError::Malformed("truncated RSA extended exp_len"));
        }
        (
            u16::from_be_bytes([key[1], key[2]]) as usize,
            HEADER_LEN_EXTENDED,
        )
    };

    // `header_len + exp_len < key.len()` guarantees a non-empty
    // modulus (the exponent itself may technically be zero-length
    // only in pathological encodings, but `RsaPublicKey::new` rejects
    // that case downstream).
    if header_len + exp_len >= key.len() {
        return Err(DnssecError::Malformed("RSA key shorter than declared"));
    }
    let exponent = &key[header_len..header_len + exp_len];
    let modulus = &key[header_len + exp_len..];
    Ok((exponent, modulus))
}

/// RFC 6605 §4: ECDSA P-256 with SHA-256.
///
/// - Public key (in DNSKEY RDATA): `X || Y`, fixed-width.
/// - Signature (in RRSIG): `r || s`, fixed-width.
///
/// RustCrypto's `EncodedPoint::from_bytes` expects SEC1 form with
/// the 0x04 uncompressed-point tag (SEC1 §2.3.3). RFC 6605 strips
/// that tag, so we re-prepend it before parsing.
fn verify_ecdsa_p256_sha256(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    use p256::ecdsa::{Signature as P256Signature, VerifyingKey as P256VerifyingKey};
    use p256::EncodedPoint;

    /// Length of one P-256 scalar / coordinate (RFC 6605 §4).
    const SCALAR_LEN: usize = 32;
    /// DNSKEY public-key length for ECDSA P-256: `X || Y`.
    const PUBLIC_KEY_LEN: usize = 2 * SCALAR_LEN;
    /// RRSIG signature length for ECDSA P-256: `r || s`.
    const SIGNATURE_LEN: usize = 2 * SCALAR_LEN;
    /// SEC1 §2.3.3 tag byte indicating an uncompressed point.
    const SEC1_UNCOMPRESSED_TAG: u8 = 0x04;
    /// SEC1 uncompressed encoding total length: `0x04 || X || Y`.
    const SEC1_UNCOMPRESSED_POINT_LEN: usize = 1 + PUBLIC_KEY_LEN;

    if public_key.len() != PUBLIC_KEY_LEN {
        return Err(DnssecError::Malformed(
            "ECDSA P-256 DNSKEY public key not 64 bytes",
        ));
    }
    if signature.len() != SIGNATURE_LEN {
        return Err(DnssecError::Malformed(
            "ECDSA P-256 RRSIG signature not 64 bytes",
        ));
    }

    let mut sec1_uncompressed = [0u8; SEC1_UNCOMPRESSED_POINT_LEN];
    sec1_uncompressed[0] = SEC1_UNCOMPRESSED_TAG;
    sec1_uncompressed[1..].copy_from_slice(public_key);
    let point = EncodedPoint::from_bytes(sec1_uncompressed)
        .map_err(|_| DnssecError::Malformed("invalid ECDSA P-256 point encoding"))?;
    let verifying_key = P256VerifyingKey::from_encoded_point(&point)
        .map_err(|_| DnssecError::Malformed("ECDSA P-256 point not on curve"))?;
    let sig = P256Signature::from_slice(signature)
        .map_err(|_| DnssecError::Malformed("invalid ECDSA P-256 signature scalars"))?;
    verifying_key
        .verify(signed_data, &sig)
        .map_err(|_| DnssecError::BadSignature)
}

/// RFC 8080 §3: Ed25519.
///
/// - Public key: an Edwards-form point, encoded per RFC 8032 §5.1.5.
/// - Signature: `R || S`, encoded per RFC 8032 §5.1.6.
fn verify_ed25519(
    signed_data: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), DnssecError> {
    use ed25519_dalek::{Signature as Ed25519Signature, VerifyingKey as Ed25519VerifyingKey};

    /// Ed25519 public-key length (RFC 8032 §5.1.5 / RFC 8080 §3).
    const PUBLIC_KEY_LEN: usize = 32;
    /// Ed25519 signature length (RFC 8032 §5.1.6 / RFC 8080 §3).
    const SIGNATURE_LEN: usize = 64;

    let pk_array: [u8; PUBLIC_KEY_LEN] = public_key
        .try_into()
        .map_err(|_| DnssecError::Malformed("Ed25519 public key not 32 bytes"))?;
    let sig_array: [u8; SIGNATURE_LEN] = signature
        .try_into()
        .map_err(|_| DnssecError::Malformed("Ed25519 signature not 64 bytes"))?;
    let verifying_key = Ed25519VerifyingKey::from_bytes(&pk_array)
        .map_err(|_| DnssecError::Malformed("invalid Ed25519 public key"))?;
    let sig = Ed25519Signature::from_bytes(&sig_array);
    verifying_key
        .verify(signed_data, &sig)
        .map_err(|_| DnssecError::BadSignature)
}

/// Compute the DS-style digest of a candidate child KSK and check
/// whether it matches the parent's DS RDATA (RFC 4034 §5.1, §5.2).
///
/// DS RDATA layout (RFC 4034 §5.1):
/// `Key Tag (2) | Algorithm (1) | Digest Type (1) | Digest (variable)`.
///
/// Returns true iff:
/// - the digest type is one we support — currently only SHA-256
///   (Digest Type 2, RFC 4509); SHA-1 (Digest Type 1) is rejected by
///   construction per RFC 8624 §3.3,
/// - and `digest(canonical_owner_name || dnskey_rdata)` matches the
///   digest field of the DS RDATA (RFC 4034 §5.1.4).
pub fn ds_matches_dnskey(child_zone_name: &[u8], dnskey_rdata: &[u8], ds_rdata: &[u8]) -> bool {
    /// Offset of the Digest Type field in DS RDATA: 2-byte Key Tag
    /// + 1-byte Algorithm (RFC 4034 §5.1).
    const DIGEST_TYPE_OFFSET: usize = 3;

    if ds_rdata.len() < DS_RDATA_HEADER_LEN {
        return false;
    }
    let digest_type = ds_rdata[DIGEST_TYPE_OFFSET];
    let claimed_digest = &ds_rdata[DS_RDATA_HEADER_LEN..];
    match digest_type {
        DS_DIGEST_TYPE_SHA256 => {
            let input = ds_digest_input(child_zone_name, dnskey_rdata);
            let mut h = Sha256::new();
            h.update(&input);
            let computed = h.finalize();
            &computed[..] == claimed_digest
        }
        // SHA-1 (Digest Type 1) and any other legacy digest type:
        // rejected by construction per RFC 8624 §3.3.
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_short_rsa_exponent() {
        // 1-byte length prefix (0x03) + 3-byte exponent (0x010001) +
        // 2-byte modulus (0xabcd). Exercises the common short path.
        let key = vec![0x03, 0x01, 0x00, 0x01, 0xab, 0xcd];
        let (e, n) = parse_rfc3110_rsa_key(&key).unwrap();
        assert_eq!(e, &[0x01, 0x00, 0x01]);
        assert_eq!(n, &[0xab, 0xcd]);
    }

    #[test]
    fn parses_extended_rsa_exponent() {
        // 0x00 marker + 2-byte length 0x0003 + 3-byte exponent
        // 0x010001 + 2-byte modulus 0xabcd. Exercises the extended
        // (zero-marker) path.
        let key = vec![0x00, 0x00, 0x03, 0x01, 0x00, 0x01, 0xab, 0xcd];
        let (e, n) = parse_rfc3110_rsa_key(&key).unwrap();
        assert_eq!(e, &[0x01, 0x00, 0x01]);
        assert_eq!(n, &[0xab, 0xcd]);
    }

    #[test]
    fn unsupported_algorithm_returns_error() {
        /// RSA-SHA1 — IANA algorithm 5, explicitly rejected per
        /// RFC 8624 §3.1 ("MUST NOT" for new deployments).
        const ALG_RSA_SHA1: u8 = 5;
        let dnskey = vec![0u8; DNSKEY_RDATA_HEADER_LEN]; // header only
        let signed = b"hello";
        let sig = vec![0u8; 64];
        assert_eq!(
            verify_signature_for_alg(ALG_RSA_SHA1, signed, &sig, &dnskey),
            Err(DnssecError::UnsupportedAlgorithm(ALG_RSA_SHA1))
        );
    }
}
