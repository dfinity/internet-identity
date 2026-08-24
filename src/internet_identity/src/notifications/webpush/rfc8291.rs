//! RFC 8291 Web Push payload encryption.
//!
//! Encrypts a plaintext payload under a device's `p256dh` + `auth` keys so
//! it can travel through Apple/Google/Mozilla's push relays without them
//! seeing the content. The browser decrypts on delivery using the SW's
//! private half of the P-256 subscription key.
//!
//! The wire format is `aes128gcm` (RFC 8188), which is what browsers expect
//! for Web Push:
//!
//! ```text
//!   salt (16)  ||  rs (4, big-endian)  ||  idlen (1) = 65  ||
//!   epk_pub_uncompressed (65)  ||  aead_ciphertext
//! ```
//!
//! Where `aead_ciphertext = AES-128-GCM(CEK, NONCE, plaintext || 0x02 || pad)`
//! and the CEK/NONCE come from HKDF over ECDH(epk_priv, device.p256dh) mixed
//! with the device's `auth` secret per RFC 8291 §3.4.
//!
//! # Test vectors
//!
//! The `#[cfg(test)]` module round-trips through an in-test decryption
//! helper to prove the plaintext survives every stage. A follow-up slice
//! adds byte-exact interop verification against the RFC 8291 §5 vector.

// `encrypt` and its private helpers are dead code until the `notify_user`
// handler lands in the next slice — the module ships the crypto primitive
// first so it can be reviewed and tested in isolation.
#![allow(dead_code)]
// The `p256` + `aes-gcm` crates surface a few APIs that are deprecated
// as part of RustCrypto's transition to `generic-array` 1.x. The
// replacements aren't yet stable across the versions II pins, so we
// silence them for this module — cleanup rides on the ecosystem's
// generic-array 1.x bump.
#![allow(deprecated)]

use aes_gcm::{
    aead::{Aead, KeyInit, Payload},
    Aes128Gcm, Nonce,
};
use hkdf::Hkdf;
use p256::{
    ecdh::diffie_hellman, elliptic_curve::sec1::ToEncodedPoint, EncodedPoint,
    PublicKey as P256PublicKey, SecretKey as P256SecretKey,
};
use rand_core::{CryptoRng, RngCore};
use sha2::Sha256;

/// Length of an uncompressed SEC1-encoded P-256 public key.
pub(crate) const P256_UNCOMPRESSED_LEN: usize = 65;
/// Length of the device's `auth` secret per RFC 8291 §3.4.
pub(crate) const AUTH_SECRET_LEN: usize = 16;
/// Length of the `salt` field per RFC 8188.
const SALT_LEN: usize = 16;
/// AES-128-GCM key length.
const CEK_LEN: usize = 16;
/// AES-128-GCM nonce length.
const NONCE_LEN: usize = 12;

/// Encrypt `plaintext` for delivery to the device whose subscription yielded
/// `device_p256dh` (uncompressed SEC1 point) and `device_auth`, returning the
/// full RFC 8188 `aes128gcm` blob for the push request body.
///
/// Infallible: `device_p256dh` is validated once at subscribe (see
/// [`validate_device_public_key`]) and `plaintext` is a small, canister-built
/// payload, so every step below is a bounded, deterministic transform. The
/// remaining ways it could fail are pure invariants, asserted rather than
/// surfaced, so callers never carry a "seal that didn't encrypt".
///
/// `rng` supplies the ephemeral P-256 keypair and the 16-byte HKDF salt. In the
/// canister it is seeded from `raw_rand` via `rand_chacha::ChaCha20Rng`; in
/// tests a fixed seed reproduces the RFC 8291 §5 vectors.
pub fn encrypt(
    plaintext: &[u8],
    device_p256dh: &[u8],
    device_auth: &[u8; AUTH_SECRET_LEN],
    rng: &mut (impl RngCore + CryptoRng),
) -> Vec<u8> {
    // A single aes128gcm record holds rs (4096) minus the AEAD tag (16) and the
    // mandatory 0x02 padding delimiter (1). The only caller seals a tiny
    // canister-built payload, so this is an invariant, not a runtime path.
    const MAX_PLAINTEXT: usize = 4096 - 16 - 1;
    debug_assert!(
        plaintext.len() <= MAX_PLAINTEXT,
        "payload must fit one aes128gcm record"
    );

    let device_pk = decode_p256_point(device_p256dh)
        .expect("device_p256dh is validated at subscribe via validate_device_public_key");

    let ephemeral_sk = P256SecretKey::random(rng);
    let ephemeral_pk_bytes = ephemeral_sk
        .public_key()
        .to_encoded_point(false /* uncompressed */)
        .as_bytes()
        .to_vec();

    let mut salt = [0u8; SALT_LEN];
    rng.fill_bytes(&mut salt);

    let shared = diffie_hellman(ephemeral_sk.to_nonzero_scalar(), device_pk.as_affine());
    // shared_secret is the raw ECDH x-coordinate, always 32 bytes for P-256.
    let shared_secret: [u8; 32] = shared
        .raw_secret_bytes()
        .as_slice()
        .try_into()
        .expect("P-256 ECDH shared secret is 32 bytes");

    let (cek, nonce) = derive_cek_and_nonce(
        &shared_secret,
        device_auth,
        device_p256dh,
        &ephemeral_pk_bytes,
        &salt,
    );

    // AES-128-GCM sealed body: plaintext || 0x02 || <no padding>.
    // The 0x02 delimiter indicates "last record" per RFC 8188 §2.
    let mut sealed_input = Vec::with_capacity(plaintext.len() + 1);
    sealed_input.extend_from_slice(plaintext);
    sealed_input.push(0x02);

    let cipher = Aes128Gcm::new_from_slice(&cek).expect("CEK is exactly 16 bytes");
    let nonce = Nonce::from_slice(&nonce);
    let ciphertext = cipher
        .encrypt(
            nonce,
            Payload {
                msg: &sealed_input,
                aad: b"",
            },
        )
        .expect("payload fits one aes128gcm record, so encryption cannot fail");

    // Header: salt(16) || rs(4, be) || idlen(1) || key_id(idlen)
    let rs: u32 = 4096;
    let mut out =
        Vec::with_capacity(SALT_LEN + 4 + 1 + ephemeral_pk_bytes.len() + ciphertext.len());
    out.extend_from_slice(&salt);
    out.extend_from_slice(&rs.to_be_bytes());
    out.push(ephemeral_pk_bytes.len() as u8);
    out.extend_from_slice(&ephemeral_pk_bytes);
    out.extend_from_slice(&ciphertext);
    out
}

/// Validate that `bytes` is a well-formed uncompressed SEC1 P-256 point. A
/// subscription whose `p256dh` can't be parsed could never be sealed, so the
/// caller rejects it at subscribe time rather than silently storing nothing.
pub(crate) fn validate_device_public_key(bytes: &[u8]) -> bool {
    decode_p256_point(bytes).is_some()
}

/// Parse an uncompressed SEC1 P-256 point into a `PublicKey`, rejecting
/// invalid encodings.
fn decode_p256_point(bytes: &[u8]) -> Option<P256PublicKey> {
    if bytes.len() != P256_UNCOMPRESSED_LEN || bytes[0] != 0x04 {
        return None;
    }
    let encoded = EncodedPoint::from_bytes(bytes).ok()?;
    P256PublicKey::from_sec1_bytes(encoded.as_bytes()).ok()
}

/// Derive the AES-128-GCM key and nonce per RFC 8291 §3.4.
///
/// Two-stage HKDF-SHA256:
///
/// 1. `ikm = HKDF(salt=device_auth, ikm=ecdh_shared_secret,
///                info="WebPush: info\0" || device_pub || app_pub, 32)`
/// 2. `CEK  = HKDF(salt=random_salt,  ikm,
///                 info="Content-Encoding: aes128gcm\0", 16)`
/// 3. `NONCE = HKDF(salt=random_salt, ikm,
///                  info="Content-Encoding: nonce\0", 12)`
fn derive_cek_and_nonce(
    ecdh_shared: &[u8; 32],
    device_auth: &[u8; AUTH_SECRET_LEN],
    device_pub: &[u8],
    app_pub: &[u8],
    salt: &[u8; SALT_LEN],
) -> ([u8; CEK_LEN], [u8; NONCE_LEN]) {
    // Step 1: mix in the device's auth secret.
    let mut info1 = Vec::with_capacity(b"WebPush: info\0".len() + device_pub.len() + app_pub.len());
    info1.extend_from_slice(b"WebPush: info\0");
    info1.extend_from_slice(device_pub);
    info1.extend_from_slice(app_pub);

    let hk1 = Hkdf::<Sha256>::new(Some(device_auth), ecdh_shared);
    let mut ikm = [0u8; 32];
    hk1.expand(&info1, &mut ikm)
        .expect("32 bytes fits within HKDF-SHA256 output");

    // Step 2: derive CEK and NONCE from the same PRK, different info strings.
    let hk2 = Hkdf::<Sha256>::new(Some(salt), &ikm);
    let mut cek = [0u8; CEK_LEN];
    hk2.expand(b"Content-Encoding: aes128gcm\0", &mut cek)
        .expect("16 bytes fits within HKDF-SHA256 output");
    let mut nonce = [0u8; NONCE_LEN];
    hk2.expand(b"Content-Encoding: nonce\0", &mut nonce)
        .expect("12 bytes fits within HKDF-SHA256 output");

    (cek, nonce)
}

#[cfg(test)]
mod tests {
    //! Correctness tests for the encryption pipeline.
    //!
    //! The primary confidence signal is a **round-trip** test that reverses
    //! everything `encrypt()` does using the device's private key, and
    //! confirms the plaintext survives. If any part of the pipeline
    //! (HKDF info strings, AEAD nonce, framing bytes, ECDH direction)
    //! drifts, the round-trip fails immediately.
    //!
    //! The RFC's own §5 known-answer test (`rfc8291_section5_vector`) then
    //! decrypts the standard's canonical `aes128gcm` body, pinning our key
    //! derivation and AEAD to the spec. That is what proves a real browser
    //! interoperates; the round-trip only proves our two halves agree.

    use super::*;
    use base64::prelude::{Engine, BASE64_URL_SAFE_NO_PAD};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    /// Reverse of `encrypt()` — given a full `aes128gcm` body and the
    /// device's private half of the P-256 subscription keypair, recover
    /// the plaintext. This is what a browser (or a test mock receiver)
    /// does on the device side.
    ///
    /// Not exported outside this test module; the canister only encrypts.
    fn decrypt(
        body: &[u8],
        device_priv: &P256SecretKey,
        device_auth: &[u8; AUTH_SECRET_LEN],
    ) -> Vec<u8> {
        assert!(
            body.len() > SALT_LEN + 4 + 1 + P256_UNCOMPRESSED_LEN,
            "body too short"
        );
        let salt: [u8; SALT_LEN] = body[..SALT_LEN].try_into().expect("salt slice");
        let rs = u32::from_be_bytes(body[SALT_LEN..SALT_LEN + 4].try_into().expect("rs slice"));
        assert_eq!(rs, 4096, "rs framing byte");
        let idlen = body[SALT_LEN + 4] as usize;
        assert_eq!(idlen, P256_UNCOMPRESSED_LEN, "idlen framing byte");

        let key_id_start = SALT_LEN + 4 + 1;
        let key_id_end = key_id_start + idlen;
        let app_pub_bytes = &body[key_id_start..key_id_end];
        let ciphertext = &body[key_id_end..];

        let app_pub = decode_p256_point(app_pub_bytes).expect("valid app pub in body");
        let device_pub_bytes = device_priv
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec();

        // Note: ECDH is symmetric, so from the device's side we do
        //   ECDH(device_priv, app_pub) — but we still need to pass the
        //   original device_pub and app_pub to the HKDF info string in
        //   the same order the encryption side used.
        let shared = diffie_hellman(device_priv.to_nonzero_scalar(), app_pub.as_affine());
        let shared_secret: [u8; 32] = shared
            .raw_secret_bytes()
            .as_slice()
            .try_into()
            .expect("32 bytes");
        let (cek, nonce) = derive_cek_and_nonce(
            &shared_secret,
            device_auth,
            &device_pub_bytes,
            app_pub_bytes,
            &salt,
        );

        let cipher = Aes128Gcm::new_from_slice(&cek).expect("valid cek");
        let nonce = Nonce::from_slice(&nonce);
        let opened = cipher
            .decrypt(
                nonce,
                Payload {
                    msg: ciphertext,
                    aad: b"",
                },
            )
            .expect("AEAD decrypt");
        // Strip the mandatory 0x02 last-record marker per RFC 8188.
        let last = *opened.last().expect("non-empty");
        assert_eq!(last, 0x02, "expected 0x02 last-record marker");
        opened[..opened.len() - 1].to_vec()
    }

    /// RFC 8291 §5 known-answer test: decrypting the standard's canonical
    /// `aes128gcm` body with its receiver key recovers the example plaintext.
    /// Pins our key derivation and AEAD to the spec, so a browser decrypts what
    /// `encrypt()` produces.
    #[test]
    fn rfc8291_section5_vector() {
        let b64 = |s: &str| BASE64_URL_SAFE_NO_PAD.decode(s).expect("valid base64url");
        // Reproduced verbatim from RFC 8291 §5.
        let ua_private = b64("q1dXpw3UpT5VOmu_cf_v6ih07Aems3njxI-JWgLcM94");
        let auth: [u8; AUTH_SECRET_LEN] = b64("BTBZMqHH6r4Tts7J_aSIgg")
            .try_into()
            .expect("16-byte auth secret");
        let body = b64("DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPTpK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN");

        let device_priv = P256SecretKey::from_slice(&ua_private).expect("valid P-256 scalar");
        let recovered = decrypt(&body, &device_priv, &auth);

        assert_eq!(recovered, b"When I grow up, I want to be a watermelon");
    }

    #[test]
    fn round_trip_recovers_plaintext() {
        // Generate a fresh device keypair (this is what a real browser SW
        // does at subscription time), then encrypt + decrypt and check the
        // plaintext survives every stage.
        let mut rng = ChaCha20Rng::from_seed([0x42; 32]);
        let device_priv = P256SecretKey::random(&mut rng);
        let device_pub_bytes = device_priv
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec();
        let mut device_auth = [0u8; AUTH_SECRET_LEN];
        rng.fill_bytes(&mut device_auth);

        let plaintext = br#"{"hostname":"foo.app","title":"FooApp","body":"Bob says hi"}"#;
        let body = encrypt(plaintext, &device_pub_bytes, &device_auth, &mut rng);

        // Header framing sanity — cheaper than waiting for AEAD failure if
        // it goes wrong.
        assert_eq!(&body[SALT_LEN..SALT_LEN + 4], &4096u32.to_be_bytes());
        assert_eq!(body[SALT_LEN + 4] as usize, P256_UNCOMPRESSED_LEN);

        let recovered = decrypt(&body, &device_priv, &device_auth);
        assert_eq!(recovered.as_slice(), plaintext);
    }

    #[test]
    fn round_trip_survives_short_and_long_payloads() {
        // Two boundary cases: empty (1-byte padding delimiter alone) and
        // the largest single-record payload the encoder accepts.
        let mut rng = ChaCha20Rng::from_seed([0xa5; 32]);
        let device_priv = P256SecretKey::random(&mut rng);
        let device_pub_bytes = device_priv
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec();
        let mut device_auth = [0u8; AUTH_SECRET_LEN];
        rng.fill_bytes(&mut device_auth);

        for size in [0usize, 1, 32, 256, 4079] {
            let plaintext = vec![0x5au8; size];
            let body = encrypt(&plaintext, &device_pub_bytes, &device_auth, &mut rng);
            let recovered = decrypt(&body, &device_priv, &device_auth);
            assert_eq!(recovered, plaintext, "size={size}");
        }
    }

    #[test]
    fn validate_device_public_key_rejects_bad_points() {
        // The one input-dependent check, at the subscribe boundary. Past it,
        // `encrypt` treats the key as valid.
        assert!(!validate_device_public_key(&[0x04, 0x01, 0x02])); // wrong length
        let mut bad_tag = vec![0u8; P256_UNCOMPRESSED_LEN];
        bad_tag[0] = 0x03; // right length, compressed-point marker
        assert!(!validate_device_public_key(&bad_tag));

        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let good = P256SecretKey::random(&mut rng)
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec();
        assert!(validate_device_public_key(&good));
    }

    #[test]
    #[should_panic(expected = "one aes128gcm record")]
    fn oversized_plaintext_trips_the_invariant() {
        // The only caller seals a tiny canister-built payload; an oversized one
        // is a bug, caught by the debug_assert rather than silently truncated.
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let device_pub_bytes = P256SecretKey::random(&mut rng)
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec();
        let _ = encrypt(&vec![0u8; 4080], &device_pub_bytes, &[0u8; 16], &mut rng);
    }
}
