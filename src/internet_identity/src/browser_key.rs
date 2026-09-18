//! Verifies that a sign-in request comes from a browser holding the key it names.
//!
//! Knows nothing of sessions or storage, so both layers may depend on it: the endpoint
//! verifies, and storage requires the [`VerifiedBrowserKeys`] that verifying produces.

use internet_identity_interface::internet_identity::types::{PublicKey, SessionKey};
use p256::ecdsa::signature::Verifier;
use p256::ecdsa::{Signature, VerifyingKey};
use p256::pkcs8::DecodePublicKey;

/// Prefixed to the signed message so the browser key cannot be made to sign for another
/// purpose by presenting a message from one.
const BROWSER_KEY_SIGNATURE_DOMAIN: &[u8] = b"ii-session-browser-key";

/// A different prefix for the successor's own signature, so neither signature can be
/// replayed in the other's role.
const SUCCESSOR_KEY_SIGNATURE_DOMAIN: &[u8] = b"ii-session-browser-successor";

/// A browser key is P-256, and the signature the raw `r || s` pair WebCrypto produces.
const BROWSER_KEY_SIGNATURE_BYTES: usize = 64;

/// A pair of browser keys that have proved possession of each other, which is the only
/// way this type can be obtained: its fields are private and [`verify_browser_keys`] is
/// the only constructor, so holding one is the evidence rather than a reminder to check.
///
/// [`crate::storage::Storage::create_session`] registers a browser from these keys, so
/// what it takes is this rather than two byte strings a caller assembled.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedBrowserKeys {
    current: PublicKey,
    next: PublicKey,
}

impl VerifiedBrowserKeys {
    /// Keys that were never verified, for tests about what happens *after* verification.
    /// `#[cfg(test)]`, so no canister build can reach it and the type stays evidence.
    #[cfg(test)]
    pub fn unverified_for_test(current: PublicKey, next: PublicKey) -> Self {
        Self { current, next }
    }

    /// The key this sign-in was reached by.
    pub fn current(&self) -> &PublicKey {
        &self.current
    }

    /// The successor this sign-in announced, accepted at the next one.
    pub fn next(&self) -> &PublicKey {
        &self.next
    }
}

/// Both keys sign: the current one over the session key and its successor, and the successor
/// over the session key and the key it replaces.
///
/// The successor's own signature is what stops a key being announced by someone who does not
/// hold it — without it, keys read off the wire could be planted as another browser's
/// successor and claimed when that browser next presented one.
pub fn verify_browser_keys(
    current_browser_key: &PublicKey,
    current_browser_key_signature: &[u8],
    next_browser_key: &PublicKey,
    next_browser_key_signature: &[u8],
    session_key: &SessionKey,
) -> Option<VerifiedBrowserKeys> {
    let verified = verify(
        current_browser_key,
        current_browser_key_signature,
        &signed_message(BROWSER_KEY_SIGNATURE_DOMAIN, session_key, next_browser_key),
    ) && verify(
        next_browser_key,
        next_browser_key_signature,
        &signed_message(
            SUCCESSOR_KEY_SIGNATURE_DOMAIN,
            session_key,
            current_browser_key,
        ),
    );
    verified.then(|| VerifiedBrowserKeys {
        current: current_browser_key.clone(),
        next: next_browser_key.clone(),
    })
}

fn verify(key: &PublicKey, signature: &[u8], message: &[u8]) -> bool {
    if signature.len() != BROWSER_KEY_SIGNATURE_BYTES {
        return false;
    }
    let Some(key) = verifying_key(key) else {
        return false;
    };
    let Ok(signature) = Signature::from_slice(signature) else {
        return false;
    };
    key.verify(message, &signature).is_ok()
}

/// One encoding of a key, so one key is one browser.
///
/// `from_public_key_der` accepts the compressed SEC1 point as readily as the uncompressed
/// one, and both are valid DER — so a single private key has two spellings, and
/// [`crate::storage::anchor::Anchor::resolve_browser`], which compares the bytes it was
/// given, would see two browsers. Only the uncompressed form is accepted, which is the
/// only one WebCrypto can emit: `exportKey("spki")` on a P-256 key writes `0x04 || X || Y`
/// and the API offers no alternative, so nothing legitimate is turned away and nothing
/// stored today needs migrating.
fn verifying_key(key: &PublicKey) -> Option<VerifyingKey> {
    let verifying = VerifyingKey::from_public_key_der(key).ok()?;
    let uncompressed = verifying.to_encoded_point(false);
    let point = key.len().checked_sub(uncompressed.as_bytes().len())?;
    (&key[point..] == uncompressed.as_bytes()).then_some(verifying)
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

    /// The algorithm identifier RFC 5480 fixes for a `secp256r1` public key, which is
    /// what a WebCrypto `exportKey("spki")` writes ahead of the point.
    const P256_ALGORITHM: [u8; 21] = [
        0x30, 0x13, 0x06, 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01, 0x06, 0x08, 0x2a, 0x86,
        0x48, 0xce, 0x3d, 0x03, 0x01, 0x07,
    ];

    /// The SPKI a key travels in, built around whichever point encoding is handed in, so
    /// a test can offer the compressed one the same way a caller could.
    fn spki(point: &[u8]) -> PublicKey {
        let mut der = vec![0x30, (P256_ALGORITHM.len() + point.len() + 3) as u8];
        der.extend_from_slice(&P256_ALGORITHM);
        der.extend_from_slice(&[0x03, (point.len() + 1) as u8, 0x00]);
        der.extend_from_slice(point);
        ByteBuf::from(der)
    }

    struct Key {
        signing: SigningKey,
        public: PublicKey,
    }

    fn key(seed: u8) -> Key {
        let signing = SigningKey::from_bytes(&[seed; 32].into()).unwrap();
        let public = spki(
            VerifyingKey::from(&signing)
                .to_encoded_point(false)
                .as_bytes(),
        );
        Key { signing, public }
    }

    impl Key {
        fn sign(&self, domain: &[u8], session_key: &SessionKey, other: &PublicKey) -> Vec<u8> {
            let signature: Signature =
                self.signing
                    .sign(&signed_message(domain, session_key, other));
            signature.to_bytes().to_vec()
        }

        fn current(&self, session_key: &SessionKey, next: &PublicKey) -> Vec<u8> {
            self.sign(BROWSER_KEY_SIGNATURE_DOMAIN, session_key, next)
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
        verify_browser_keys(
            &current.public,
            &current.current(session, &next.public),
            &next.public,
            &next.successor(session, &current.public),
            session,
        )
        .is_some()
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
        assert!(verify_browser_keys(
            &current.public,
            &current.current(&session, &announced.public),
            &announced.public,
            &current.current(&session, &announced.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn a_successor_signature_replayed_as_the_current_one_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(verify_browser_keys(
            &current.public,
            &current.successor(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn a_signature_over_another_session_key_is_refused() {
        let current = key(1);
        let next = key(2);

        assert!(verify_browser_keys(
            &current.public,
            &current.current(&session_key(7), &next.public),
            &next.public,
            &next.successor(&session_key(7), &current.public),
            &session_key(8)
        )
        .is_none());
    }

    #[test]
    fn a_signature_paired_with_another_successor_is_refused() {
        let current = key(1);
        let announced = key(2);
        let substituted = key(3);
        let session = session_key(7);

        assert!(verify_browser_keys(
            &current.public,
            &current.current(&session, &announced.public),
            &substituted.public,
            &substituted.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn another_browsers_signature_is_refused() {
        let current = key(1);
        let other = key(9);
        let next = key(2);
        let session = session_key(7);

        assert!(verify_browser_keys(
            &current.public,
            &other.current(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn a_signature_over_the_bare_session_key_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);
        let bare: Signature = current.signing.sign(&session);

        assert!(verify_browser_keys(
            &current.public,
            &bare.to_bytes(),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn a_key_that_is_not_a_p256_public_key_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(verify_browser_keys(
            &ByteBuf::from(vec![0u8; 91]),
            &current.current(&session, &next.public),
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    /// One private key must not be two browsers. `resolve_browser` looks an entry up by
    /// the bytes it was handed, so accepting a second encoding of the same point would let
    /// one browser register twice and pass the rotation check as either.
    #[test]
    fn a_compressed_key_is_refused_although_it_is_valid_der() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);
        let compressed = spki(
            VerifyingKey::from(&current.signing)
                .to_encoded_point(true)
                .as_bytes(),
        );

        // Valid DER for that key, and both signatures made over exactly what is sent.
        assert!(VerifyingKey::from_public_key_der(&compressed).is_ok());
        assert!(verify_browser_keys(
            &compressed,
            &current.current(&session, &next.public),
            &next.public,
            &next.successor(&session, &compressed),
            &session
        )
        .is_none());
    }

    #[test]
    fn a_signature_of_the_wrong_length_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);
        let mut signature = current.current(&session, &next.public);
        signature.push(0);

        assert!(verify_browser_keys(
            &current.public,
            &signature,
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
    }

    #[test]
    fn an_empty_signature_is_refused() {
        let current = key(1);
        let next = key(2);
        let session = session_key(7);

        assert!(verify_browser_keys(
            &current.public,
            &[],
            &next.public,
            &next.successor(&session, &current.public),
            &session
        )
        .is_none());
        assert!(verify_browser_keys(
            &current.public,
            &current.current(&session, &next.public),
            &next.public,
            &[],
            &session
        )
        .is_none());
    }
}
