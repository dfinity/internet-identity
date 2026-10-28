//! Sealing what a wake-up carries: RFC 8291 message encryption in RFC 8188's `aes128gcm`
//! framing.
//!
//! The costly half of RFC 8291 is the ECDH, and it does not run here. The browser runs it
//! against its own subscription keys and an ephemeral pair it mints for the purpose, and
//! uploads the resulting input keying material, so a payload costs two HKDF expansions and
//! one AES-GCM over a few dozen bytes.
//!
//! The browser decrypts before the service worker is woken, so a body that does not
//! authenticate is dropped by the browser rather than delivered: no push event, and
//! nothing the worker can log. What this module gets wrong is invisible from the client.

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes128Gcm, Nonce};
use hkdf::Hkdf;
use sha2::{Digest, Sha256};

use super::validation::{PayloadIkm, PayloadKeyId};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserId, InboxNotificationId,
};

/// RFC 8188 §2.1: the salt the record's key and nonce are derived under.
const SALT_LEN: usize = 16;

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

/// The salt a deposit's payload is sealed under: the identity, the browser whose inbox
/// holds it, and the name that inbox gave the entry, which together are what one deposit
/// is.
///
/// All three, because keying material does not always cover one inbox. A Home Screen
/// app's covers every identity pointing at it, and nothing stops two browsers of one
/// identity pointing at the same app: ids are handed out per inbox, so both would reach
/// the same number, and a salt naming only the identity and the id would repeat. The key
/// and the nonce both follow from the salt, so a repeat is a reused nonce.
///
/// Hashed rather than packed: the three are 160 bits between them. A wake-up re-posted
/// for the same entry seals the same bytes under the same salt, which reveals nothing it
/// has not already sent.
pub(crate) fn salt_for(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    inbox_notification_id: InboxNotificationId,
) -> u128 {
    let digest = Sha256::new()
        .chain_update(anchor_number.to_be_bytes())
        .chain_update(browser_id.to_be_bytes())
        .chain_update(inbox_notification_id.to_be_bytes())
        .finalize();
    u128::from_be_bytes(digest[..16].try_into().expect("a digest is 32 bytes"))
}

/// Seal `plaintext` for the browser that uploaded `payload_ikm`.
///
/// `salt` is what keeps one subscription's payloads apart: the key and the nonce are both
/// derived from it and the one IKM, so two payloads sealed under one salt would share
/// both. Callers pass [`salt_for`], which names the deposit rather than counting.
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

    #[test]
    fn a_salt_names_one_deposit_of_one_browser() {
        assert_eq!(salt_for(1, 0, 2), salt_for(1, 0, 2));
        assert_ne!(salt_for(1, 0, 2), salt_for(2, 0, 1));
        assert_ne!(salt_for(10772, 0, 0), salt_for(10772, 0, 1));
    }

    /// Two browsers of one identity can point at the same Home Screen app, and then seal
    /// with its one subscription. Their inboxes count from the same place, so without the
    /// browser in the salt both would seal a different payload under one key and nonce.
    #[test]
    fn two_inboxes_at_the_same_id_do_not_share_a_salt() {
        assert_ne!(salt_for(10772, 0, 1), salt_for(10772, 1, 1));
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

    #[test]
    fn a_payload_past_what_a_relay_accepts_is_refused() {
        let (payload_ikm, payload_keyid) = keys();
        let oversized = vec![b'x'; MAX_BODY_LEN];
        let refused = seal(&payload_ikm, &payload_keyid, 1, &oversized).expect_err("too large");
        assert!(refused.contains("past the 4096"), "{refused}");
    }
}
