//! Verifies that a sign-in request comes from a browser holding the key it names.

use internet_identity_interface::internet_identity::types::{PublicKey, SessionKey};
use p256::ecdsa::signature::Verifier;
use p256::ecdsa::{Signature, VerifyingKey};
use p256::pkcs8::DecodePublicKey;

/// Prefixed to the signed message so the browser key cannot be made to sign for another
/// purpose by presenting a message from one.
const DEVICE_KEY_SIGNATURE_DOMAIN: &[u8] = b"ii-session-device-key";

/// A different prefix for the successor's own signature, so neither signature can be
/// replayed in the other's role.
const SUCCESSOR_KEY_SIGNATURE_DOMAIN: &[u8] = b"ii-session-device-successor";

/// A browser key is P-256, and the signature the raw `r || s` pair WebCrypto produces.
const DEVICE_KEY_SIGNATURE_BYTES: usize = 64;

/// Both keys sign: the current one over the session key and its successor, and the successor
/// over the session key and the key it replaces.
///
/// The successor's own signature is what stops a key being announced by someone who does not
/// hold it — without it, keys read off the wire could be planted as another browser's
/// successor and claimed when that browser next presented one.
pub fn verify_device_keys(
    current_device_key: &PublicKey,
    current_device_key_signature: &[u8],
    next_device_key: &PublicKey,
    next_device_key_signature: &[u8],
    session_key: &SessionKey,
) -> bool {
    verify(
        current_device_key,
        current_device_key_signature,
        &signed_message(DEVICE_KEY_SIGNATURE_DOMAIN, session_key, next_device_key),
    ) && verify(
        next_device_key,
        next_device_key_signature,
        &signed_message(
            SUCCESSOR_KEY_SIGNATURE_DOMAIN,
            session_key,
            current_device_key,
        ),
    )
}

fn verify(key: &PublicKey, signature: &[u8], message: &[u8]) -> bool {
    if signature.len() != DEVICE_KEY_SIGNATURE_BYTES {
        return false;
    }
    let Ok(key) = VerifyingKey::from_public_key_der(key) else {
        return false;
    };
    let Ok(signature) = Signature::from_slice(signature) else {
        return false;
    };
    key.verify(message, &signature).is_ok()
}

/// Covers the other key as well as the session key: keys are visible on the wire, so a
/// signature that bound only the session key could be paired with one a caller chose.
fn signed_message(domain: &[u8], session_key: &SessionKey, other_key: &PublicKey) -> Vec<u8> {
    let mut message = Vec::with_capacity(domain.len() + session_key.len() + other_key.len());
    message.extend_from_slice(domain);
    message.extend_from_slice(session_key);
    message.extend_from_slice(other_key);
    message
}

#[cfg(test)]
mod tests {
    use super::*;
    use p256::ecdsa::signature::Signer;
    use p256::ecdsa::SigningKey;
    use serde_bytes::ByteBuf;

    /// The SPKI header WebCrypto emits for an `ECDSA` P-256 public key, ahead of the
    /// 65-byte uncompressed point.
    const P256_SPKI_HEADER: [u8; 26] = [
        0x30, 0x59, 0x30, 0x13, 0x06, 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01, 0x06, 0x08,
        0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07, 0x03, 0x42, 0x00,
    ];

    struct Key {
        signing: SigningKey,
        public: PublicKey,
    }

    fn key(seed: u8) -> Key {
        let signing = SigningKey::from_bytes(&[seed; 32].into()).unwrap();
        let point = VerifyingKey::from(&signing).to_encoded_point(false);
        let mut der = P256_SPKI_HEADER.to_vec();
        der.extend_from_slice(point.as_bytes());
        Key {
            signing,
            public: ByteBuf::from(der),
        }
    }

    impl Key {
        fn sign(&self, domain: &[u8], session_key: &SessionKey, other: &PublicKey) -> Vec<u8> {
            let signature: Signature =
                self.signing
                    .sign(&signed_message(domain, session_key, other));
            signature.to_bytes().to_vec()
        }

        fn current(&self, session_key: &SessionKey, next: &PublicKey) -> Vec<u8> {
            self.sign(DEVICE_KEY_SIGNATURE_DOMAIN, session_key, next)
        }

        fn successor(&self, session_key: &SessionKey, current: &PublicKey) -> Vec<u8> {
            self.sign(SUCCESSOR_KEY_SIGNATURE_DOMAIN, session_key, current)
        }
    }

    fn session_key(seed: u8) -> SessionKey {
        ByteBuf::from(vec![seed; 62])
    }

    /// A rotation as an honest browser performs it: it holds both keys and signs with both.
    fn rotation(current: &Key, next: &Key, session: &SessionKey) -> bool {
        verify_device_keys(
            &current.public,
            &current.current(session, &next.public),
            &next.public,
            &next.successor(session, &current.public),
            session,
        )
    }

    #[test]
    fn a_browser_holding_both_keys_is_accepted() {
        assert!(rotation(&key(1), &key(2), &session_key(7)));
    }

    #[test]
    fn a_successor_the_caller_does_not_hold_is_refused() {
        let current = key(1);
        let announced = key(2);
        let session = session_key(7);

        // Everything the wire carries, but signed only by the key the caller holds.
        assert!(!verify_device_keys(
            &current.public,
            &current.current(&session, &announced.public),
            &announced.public,
            &current.current(&session, &announced.public),
            &session
        ));
    }

    #[test]
    fn a_successor_signature_replayed_as_the_current_one_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(!verify_device_keys(
            &current.public,
            &current.successor(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn a_signature_over_another_session_key_is_refused() {
        let current = key(1);
        let next = key(2);

        assert!(!verify_device_keys(
            &current.public,
            &current.current(&session_key(7), &next.public),
            &next.public,
            &next.successor(&session_key(7), &current.public),
            &session_key(8)
        ));
    }

    #[test]
    fn a_signature_paired_with_another_successor_is_refused() {
        let current = key(1);
        let announced = key(2);
        let substituted = key(3);
        let session = session_key(7);

        assert!(!verify_device_keys(
            &current.public,
            &current.current(&session, &announced.public),
            &substituted.public,
            &substituted.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn another_browsers_signature_is_refused() {
        let current = key(1);
        let other = key(9);
        let next = key(2);
        let session = session_key(7);

        assert!(!verify_device_keys(
            &current.public,
            &other.current(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn a_signature_over_the_bare_session_key_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);
        let bare: Signature = current.signing.sign(&session);

        assert!(!verify_device_keys(
            &current.public,
            &bare.to_bytes(),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn a_key_that_is_not_a_p256_public_key_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(!verify_device_keys(
            &ByteBuf::from(vec![0u8; 91]),
            &current.current(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn a_signature_of_the_wrong_length_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);
        let mut signature = current.current(&session, &next.public);
        signature.push(0);

        assert!(!verify_device_keys(
            &current.public,
            &signature,
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
    }

    #[test]
    fn an_empty_signature_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(!verify_device_keys(
            &current.public,
            &[],
            &next.public,
            &next.successor(&session, &current.public),
            &session
        ));
        assert!(!verify_device_keys(
            &current.public,
            &current.current(&session, &next.public),
            &next.public,
            &[],
            &session
        ));
    }
}
