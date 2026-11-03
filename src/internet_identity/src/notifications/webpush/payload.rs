//! Sealing what a wake-up carries: RFC 8291 message encryption in RFC 8188's `aes128gcm`
//! framing.
//!
//! The costly half of RFC 8291 is the ECDH, and it does not run per push. It runs when a
//! subscription's keying material is first derived and when a pass refreshes it, so what
//! a push costs is one hash for the salt, two HKDF expansions and one AES-GCM over a few
//! dozen bytes.
//!
//! The browser decrypts before the service worker is woken, so a body that does not
//! authenticate is dropped by the browser rather than delivered: no push event, and
//! nothing the worker can log. What this module gets wrong is invisible from the client.

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes128Gcm, Nonce};
use hkdf::Hkdf;
use sha2::{Digest, Sha256};

use p256::ecdh::diffie_hellman;
use p256::elliptic_curve::sec1::ToEncodedPoint;

use super::validation::{AuthSecret, PayloadIkm, PayloadKeyId, SubscriberKey};

/// RFC 8291 §3.4: what the keying material's derivation is labelled with.
const IKM_INFO_PREFIX: &[u8] = b"WebPush: info\0";

/// RFC 8188 §2.1: the salt the record's key and nonce are derived under.
const SALT_LEN: usize = 16;

/// An uncompressed SEC1 point on P-256.
const SEC1_P256_LEN: usize = 65;

/// Record size, RFC 8188 §2.1. One record holds everything a wake-up names, so this only
/// has to be larger than the body it frames.
const RECORD_SIZE: u32 = 4096;

/// RFC 8188 §2.2 derivation labels, each terminated by a zero octet.
const CEK_INFO: &[u8] = b"Content-Encoding: aes128gcm\0";
const NONCE_INFO: &[u8] = b"Content-Encoding: nonce\0";

/// RFC 8188 §2: what follows the data of the last record.
const LAST_RECORD_DELIMITER: u8 = 0x02;

/// What a relay accepts as an encrypted body.
const MAX_BODY_LEN: usize = 4096;

/// The `Content-Encoding` a relay and a browser both require of a payload.
pub(crate) const CONTENT_ENCODING: &str = "aes128gcm";

/// RFC 8291 §3.4: the keying material every payload for one subscription is sealed under,
/// and the `keyid` a header carries so the browser can derive its own half.
///
/// This is the expensive half of the specification, and its output changes only when
/// `sender` does, which is why a pass keeps the result rather than running it per push.
pub(crate) fn derive_ikm(
    sender: &p256::SecretKey,
    subscriber_key: &SubscriberKey,
    auth: &AuthSecret,
) -> Result<(PayloadIkm, PayloadKeyId), String> {
    let theirs = p256::PublicKey::from_sec1_bytes(subscriber_key.as_slice())
        .map_err(|_| "p256dh is not a P-256 point".to_string())?;
    let shared = diffie_hellman(sender.to_nonzero_scalar(), theirs.as_affine());
    let sender_public = sender
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec();

    // The subscription's key first and the sender's second, which is the order the
    // browser reconstructs it in.
    let mut info = Vec::with_capacity(IKM_INFO_PREFIX.len() + 2 * SEC1_P256_LEN);
    info.extend_from_slice(IKM_INFO_PREFIX);
    info.extend_from_slice(subscriber_key.as_slice());
    info.extend_from_slice(&sender_public);

    let mut ikm = [0u8; 32];
    Hkdf::<Sha256>::new(Some(auth.as_slice()), &shared.raw_secret_bytes()[..])
        .expand(&info, &mut ikm)
        .map_err(|err| format!("deriving the keying material: {err}"))?;

    Ok((
        PayloadIkm::try_from(ikm.to_vec())?,
        PayloadKeyId::try_from(sender_public)?,
    ))
}

/// The salt one push is sealed under: this pass's own random bytes and the push's place
/// in that pass.
///
/// The key and the nonce both follow from the salt and the keying material, so two pushes
/// sharing one would share a keystream. Each half covers a direction the other cannot:
/// the index separates the pushes of one pass, and the bytes separate one pass from every
/// other, which the index cannot do because it starts again each time. A pass therefore
/// draws its own bytes, and nothing cached may stand in for them.
///
/// Hashed rather than packed, so the 16 bytes a header carries depend on all 36.
pub(crate) fn salt_at(pass_bytes: &[u8; 32], index: usize) -> u128 {
    let digest = Sha256::new()
        .chain_update(pass_bytes)
        .chain_update((index as u64).to_be_bytes())
        .finalize();
    u128::from_be_bytes(digest[..16].try_into().expect("a digest is 32 bytes"))
}

/// Seal `plaintext` for the browser that uploaded `payload_ikm`.
///
/// `salt` is what keeps one subscription's payloads apart: the key and the nonce are both
/// derived from it and the one IKM, so two payloads sealed under one salt would share
/// both. Callers pass [`salt_at`].
pub(crate) fn seal(
    payload_ikm: &PayloadIkm,
    payload_keyid: &PayloadKeyId,
    salt: u128,
    plaintext: &[u8],
) -> Result<Vec<u8>, String> {
    let salt_bytes = salt.to_be_bytes();

    let derived = Hkdf::<Sha256>::new(Some(&salt_bytes), payload_ikm.as_slice());
    let mut cek = [0u8; 16];
    derived
        .expand(CEK_INFO, &mut cek)
        .map_err(|err| format!("deriving the content encryption key: {err}"))?;
    let mut nonce = [0u8; 12];
    derived
        .expand(NONCE_INFO, &mut nonce)
        .map_err(|err| format!("deriving the nonce: {err}"))?;

    let mut record = Vec::with_capacity(plaintext.len() + 1);
    record.extend_from_slice(plaintext);
    record.push(LAST_RECORD_DELIMITER);

    let ciphertext = Aes128Gcm::new_from_slice(&cek)
        .map_err(|err| format!("the derived key is not a key: {err}"))?
        .encrypt(&Nonce::from(nonce), record.as_slice())
        .map_err(|_| "sealing the payload".to_string())?;

    let keyid = payload_keyid.as_slice();
    let keyid_len = u8::try_from(keyid.len())
        .map_err(|_| format!("a key id of {} bytes does not fit the header", keyid.len()))?;

    let mut body = Vec::with_capacity(SALT_LEN + 4 + 1 + keyid.len() + ciphertext.len());
    body.extend_from_slice(&salt_bytes);
    body.extend_from_slice(&RECORD_SIZE.to_be_bytes());
    body.push(keyid_len);
    body.extend_from_slice(keyid);
    body.extend_from_slice(&ciphertext);

    if body.len() > MAX_BODY_LEN {
        return Err(format!(
            "a sealed payload of {} bytes is past the {MAX_BODY_LEN} a relay accepts",
            body.len()
        ));
    }
    Ok(body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::webpush::fixtures::valid_sec1_point;
    use pretty_assertions::assert_eq;

    const HEADER_LEN: usize = SALT_LEN + 4 + 1 + 65;

    fn keys() -> (PayloadIkm, PayloadKeyId) {
        (
            PayloadIkm::try_from(vec![7u8; 32]).expect("32 bytes"),
            PayloadKeyId::try_from(valid_sec1_point()).expect("a SEC1 P-256 point"),
        )
    }

    /// Reads the record back the way a browser does: the header says which salt and key id
    /// it was sealed under, and the key and nonce follow from the salt and the IKM.
    fn open(payload_ikm: &PayloadIkm, body: &[u8]) -> Vec<u8> {
        let salt = &body[..SALT_LEN];
        let keyid_len = usize::from(body[SALT_LEN + 4]);
        let ciphertext = &body[SALT_LEN + 4 + 1 + keyid_len..];

        let derived = Hkdf::<Sha256>::new(Some(salt), payload_ikm.as_slice());
        let mut cek = [0u8; 16];
        derived.expand(CEK_INFO, &mut cek).expect("the key");
        let mut nonce = [0u8; 12];
        derived.expand(NONCE_INFO, &mut nonce).expect("the nonce");

        let mut opened = Aes128Gcm::new_from_slice(&cek)
            .expect("the derived key")
            .decrypt(&Nonce::from(nonce), ciphertext)
            .expect("the record authenticates");
        assert_eq!(opened.pop(), Some(LAST_RECORD_DELIMITER));
        opened
    }

    #[test]
    fn the_header_carries_the_salt_the_record_size_and_the_key_id() {
        let (payload_ikm, payload_keyid) = keys();
        let body = seal(&payload_ikm, &payload_keyid, 9, b"{}").expect("sealing");

        assert_eq!(&body[..SALT_LEN], &9u128.to_be_bytes());
        assert_eq!(&body[SALT_LEN..SALT_LEN + 4], &RECORD_SIZE.to_be_bytes());
        assert_eq!(body[SALT_LEN + 4], 65);
        assert_eq!(&body[SALT_LEN + 5..HEADER_LEN], payload_keyid.as_slice());
        // Two bytes of payload, the delimiter, and the GCM tag.
        assert_eq!(body.len(), HEADER_LEN + 3 + 16);
    }

    #[test]
    fn a_sealed_payload_reads_back_as_what_went_in() {
        let (payload_ikm, payload_keyid) = keys();
        let plaintext = br#"{"anchor_number":10772,"inbox_notification_id":3}"#;
        let body = seal(&payload_ikm, &payload_keyid, 1, plaintext).expect("sealing");
        assert_eq!(open(&payload_ikm, &body), plaintext.to_vec());
    }

    /// Within a pass the index is what separates two pushes.
    #[test]
    fn two_places_in_one_pass_do_not_share_a_salt() {
        let pass = [7u8; 32];
        assert_ne!(salt_at(&pass, 0), salt_at(&pass, 1));
    }

    /// Across passes the index is no help, because it starts again at zero. The bytes are
    /// the whole of what keeps two passes apart.
    #[test]
    fn the_same_place_in_two_passes_does_not_share_a_salt() {
        assert_ne!(salt_at(&[7u8; 32], 0), salt_at(&[8u8; 32], 0));
    }

    #[test]
    fn a_salt_is_the_same_for_the_same_pass_and_place() {
        assert_eq!(salt_at(&[7u8; 32], 3), salt_at(&[7u8; 32], 3));
    }

    #[test]
    fn one_salt_per_payload_keeps_two_payloads_apart() {
        let (payload_ikm, payload_keyid) = keys();
        let first = seal(&payload_ikm, &payload_keyid, 1, b"same").expect("sealing");
        let second = seal(&payload_ikm, &payload_keyid, 2, b"same").expect("sealing");
        assert_ne!(first[HEADER_LEN..], second[HEADER_LEN..]);
        assert_eq!(open(&payload_ikm, &first), open(&payload_ikm, &second));
    }

    /// RFC 8291 Appendix A, whose inputs and output are reproduced here in full. A
    /// derivation that round-trips against itself proves only that it is self-consistent;
    /// this is what says it agrees with the specification the browser implements. It has
    /// to, because a browser drops a payload it cannot open before waking the worker, so
    /// a disagreement is invisible from the client.
    #[test]
    fn the_rfc_8291_example_seals_to_the_body_the_rfc_prints() {
        use base64::engine::general_purpose::URL_SAFE_NO_PAD;
        use base64::Engine;

        let decode = |value: &str| URL_SAFE_NO_PAD.decode(value).expect("base64url");
        let payload_ikm =
            PayloadIkm::try_from(decode("S4lYMb_L0FxCeq0WhDx813KgSYqU26kOyzWUdsXYyrg"))
                .expect("the example's keying material");
        let payload_keyid = PayloadKeyId::try_from(decode(
            "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8",
        ))
        .expect("the example's ephemeral public key");
        let salt = u128::from_be_bytes(
            decode("DGv6ra1nlYgDCS1FRnbzlw")
                .try_into()
                .expect("16 octets"),
        );

        let body = seal(
            &payload_ikm,
            &payload_keyid,
            salt,
            b"When I grow up, I want to be a watermelon",
        )
        .expect("sealing the example");

        assert_eq!(
            body,
            decode(concat!(
                "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27ml",
                "mlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPT",
                "pK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN",
            ))
        );
    }

    /// RFC 8291 Appendix A again, this time the derivation that produces the keying
    /// material the test above seals with. Together they cover the whole schedule against
    /// the specification the browser implements.
    ///
    /// Worth pinning to the RFC rather than to a round trip: a browser drops a payload it
    /// cannot open before it wakes the service worker, so a derivation that disagrees
    /// with the specification fails silently and looks like no notification at all.
    #[test]
    fn the_rfc_8291_example_derives_the_keying_material_the_rfc_prints() {
        use base64::engine::general_purpose::URL_SAFE_NO_PAD;
        use base64::Engine;

        let decode = |value: &str| URL_SAFE_NO_PAD.decode(value).expect("base64url");
        let sender =
            p256::SecretKey::from_slice(&decode("yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"))
                .expect("the example's application server key");
        let subscriber_key = SubscriberKey::try_from(decode(
            "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4",
        ))
        .expect("the example's subscription key");
        let auth = AuthSecret::try_from(decode("BTBZMqHH6r4Tts7J_aSIgg"))
            .expect("the example's auth secret");

        let (payload_ikm, payload_keyid) =
            derive_ikm(&sender, &subscriber_key, &auth).expect("deriving");

        assert_eq!(
            payload_ikm.as_slice(),
            decode("S4lYMb_L0FxCeq0WhDx813KgSYqU26kOyzWUdsXYyrg")
        );
        assert_eq!(
            payload_keyid.as_slice(),
            decode("BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8")
        );
    }

    /// A refresh is a new sender key against the same subscription, so both halves have
    /// to move: keeping the old `keyid` would point the browser at a key that no longer
    /// derives this material.
    #[test]
    fn a_new_sender_key_derives_new_material_and_a_new_key_id() {
        let subscriber_key = SubscriberKey::try_from(valid_sec1_point()).expect("a point");
        let auth = AuthSecret::try_from(vec![4u8; 16]).expect("16 bytes");
        let first = derive_ikm(
            &p256::SecretKey::from_slice(&[3u8; 32]).expect("a scalar"),
            &subscriber_key,
            &auth,
        )
        .expect("deriving");
        let second = derive_ikm(
            &p256::SecretKey::from_slice(&[5u8; 32]).expect("a scalar"),
            &subscriber_key,
            &auth,
        )
        .expect("deriving");

        assert_ne!(first.0.as_slice(), second.0.as_slice());
        assert_ne!(first.1.as_slice(), second.1.as_slice());
    }

    #[test]
    fn a_payload_past_what_a_relay_accepts_is_refused() {
        let (payload_ikm, payload_keyid) = keys();
        let oversized = vec![b'x'; MAX_BODY_LEN];
        let refused = seal(&payload_ikm, &payload_keyid, 1, &oversized).expect_err("too large");
        assert!(refused.contains("past the 4096"), "{refused}");
    }
}
