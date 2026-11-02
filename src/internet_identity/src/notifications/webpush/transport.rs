//! Getting the subscription's `auth` secret to the canister over a call anyone can read.
//!
//! RFC 8291 gives the application server `auth` over TLS. An ingress call has no such
//! channel: it passes a boundary node and reaches every replica. So the browser encrypts
//! `auth` to a key the canister minted for that one subscribe, and nothing in the call is
//! usable without the private half, which never leaves here and is dropped the moment the
//! subscribe lands.
//!
//! The construction is RFC 8291's own, with the browser as the sender: ECDH over P-256,
//! HKDF, AES-128-GCM. Not because it is the only way, but because WebCrypto offers one
//! asymmetric cipher and it is RSA-OAEP, whose keypair a canister cannot mint per attempt.
//! These three the browser has natively, and the delivery side already carries the curve.

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes128Gcm, Nonce};
use hkdf::Hkdf;
use p256::ecdh::diffie_hellman;
use p256::elliptic_curve::sec1::ToEncodedPoint;
use sha2::Sha256;

use super::validation::{AuthSecret, SealedAuth, SubscriberKey};

const SECOND_NS: u64 = 1_000_000_000;

/// How long a transport key waits for the subscribe it was minted for. The gap is a user
/// deciding on the browser's permission prompt, so it is minutes rather than seconds.
///
/// Nothing sweeps on this: a registration waiting past it is simply refused and replaced
/// by the next fetch, and the registration it is holding open goes on delivering.
pub(crate) const PENDING_TTL_NS: u64 = 5 * 60 * SECOND_NS;

/// RFC 8291 §3.4 binds the two public keys into the derivation, and so does this.
const INFO_PREFIX: &[u8] = b"II webpush transport\0";

/// The public half a browser encrypts to, uncompressed SEC1.
pub(crate) fn public_key_of(transport_scalar: &[u8; 32]) -> Vec<u8> {
    super::seed_pool::secret_key_from(transport_scalar)
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec()
}

/// Reads `auth` back out of what the browser sent.
///
/// `browser_key` is the public half of the pair the browser minted for this one
/// encryption, which is why it arrives alongside the ciphertext rather than being known
/// in advance.
pub(crate) fn open(
    transport_scalar: &[u8; 32],
    browser_key: &SubscriberKey,
    sealed: &SealedAuth,
) -> Result<AuthSecret, String> {
    let transport = super::seed_pool::secret_key_from(transport_scalar);
    let theirs = p256::PublicKey::from_sec1_bytes(browser_key.as_slice())
        .map_err(|_| "the browser's transport key is not a P-256 point".to_string())?;

    let shared = diffie_hellman(transport.to_nonzero_scalar(), theirs.as_affine());

    let mut info = Vec::with_capacity(INFO_PREFIX.len() + 65 + 65);
    info.extend_from_slice(INFO_PREFIX);
    info.extend_from_slice(transport.public_key().to_encoded_point(false).as_bytes());
    info.extend_from_slice(browser_key.as_slice());

    let derived = Hkdf::<Sha256>::new(None, &shared.raw_secret_bytes()[..]);
    let mut key = [0u8; 16];
    let mut iv = [0u8; 12];
    derived
        .expand(&[info.as_slice(), b"key\0"].concat(), &mut key)
        .map_err(|err| format!("deriving the transport key: {err}"))?;
    derived
        .expand(&[info.as_slice(), b"iv\0"].concat(), &mut iv)
        .map_err(|err| format!("deriving the transport nonce: {err}"))?;

    let opened = Aes128Gcm::new_from_slice(&key)
        .map_err(|err| format!("the derived key is not a key: {err}"))?
        .decrypt(&Nonce::from(iv), sealed.as_slice())
        .map_err(|_| "the encrypted auth secret does not authenticate".to_string())?;

    AuthSecret::try_from(opened)
}

/// The browser's half of the construction above, for tests that need a real sealed
/// `auth` rather than a stand-in.
#[cfg(test)]
pub(crate) fn seal_auth(
    transport_public: &[u8],
    browser_scalar: &[u8; 32],
    auth: &[u8],
) -> (SubscriberKey, SealedAuth) {
    let browser = super::seed_pool::secret_key_from(browser_scalar);
    let browser_public = browser
        .public_key()
        .to_encoded_point(false)
        .as_bytes()
        .to_vec();
    let theirs = p256::PublicKey::from_sec1_bytes(transport_public).expect("a point");
    let shared = diffie_hellman(browser.to_nonzero_scalar(), theirs.as_affine());

    let mut info = Vec::new();
    info.extend_from_slice(INFO_PREFIX);
    info.extend_from_slice(transport_public);
    info.extend_from_slice(&browser_public);

    let derived = Hkdf::<Sha256>::new(None, &shared.raw_secret_bytes()[..]);
    let mut key = [0u8; 16];
    let mut iv = [0u8; 12];
    derived
        .expand(&[info.as_slice(), b"key\0"].concat(), &mut key)
        .expect("the key");
    derived
        .expand(&[info.as_slice(), b"iv\0"].concat(), &mut iv)
        .expect("the nonce");
    let ciphertext = Aes128Gcm::new_from_slice(&key)
        .expect("a key")
        .encrypt(&Nonce::from(iv), auth)
        .expect("sealing");
    (
        SubscriberKey::try_from(browser_public).expect("a point"),
        SealedAuth::try_from(ciphertext).expect("a sealed auth secret"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    use seal_auth as sealed_by_browser;

    #[test]
    fn what_the_browser_sealed_is_what_the_canister_opens() {
        let transport_scalar = [9u8; 32];
        let transport_public = public_key_of(&transport_scalar);
        let auth = [4u8; 16];
        let (browser_key, sealed) = sealed_by_browser(&transport_public, &[5u8; 32], &auth);

        let opened = open(&transport_scalar, &browser_key, &sealed).expect("opening");
        assert_eq!(opened.as_slice(), &auth);
    }

    /// The whole point of the transport key: what travels over ingress is inert without
    /// the private half, so another key opens nothing.
    #[test]
    fn a_different_transport_key_opens_nothing() {
        let transport_public = public_key_of(&[9u8; 32]);
        let (browser_key, sealed) = sealed_by_browser(&transport_public, &[5u8; 32], &[4u8; 16]);

        assert!(open(&[8u8; 32], &browser_key, &sealed).is_err());
    }

    /// Both public keys enter the derivation, so a ciphertext cannot be lifted onto
    /// another subscribe's transport key.
    #[test]
    fn a_ciphertext_does_not_move_to_another_browser_key() {
        let transport_scalar = [9u8; 32];
        let transport_public = public_key_of(&transport_scalar);
        let (_, sealed) = sealed_by_browser(&transport_public, &[5u8; 32], &[4u8; 16]);
        let (other_browser_key, _) = sealed_by_browser(&transport_public, &[6u8; 32], &[4u8; 16]);

        assert!(open(&transport_scalar, &other_browser_key, &sealed).is_err());
    }
}
