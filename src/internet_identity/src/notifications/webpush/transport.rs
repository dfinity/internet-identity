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
use std::cell::RefCell;
use std::collections::HashMap;

use super::validation::{AuthSecret, SealedAuth, SubscriberKey};
use crate::authz_utils::NotificationKey;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

const SECOND_NS: u64 = 1_000_000_000;

/// How long a transport key waits for the subscribe it was minted for. The gap is a user
/// deciding on the browser's permission prompt, so it is minutes rather than seconds.
const PENDING_TTL_NS: u64 = 5 * 60 * SECOND_NS;

/// What the heap gives to subscribes in flight. Entries are ~70 bytes and live five
/// minutes; this caps what a flood of key fetches can hold at once, since expiring them
/// by age alone bounds nothing inside the window.
const MAX_PENDING: usize = 10_000;

/// RFC 8291 §3.4 binds the two public keys into the derivation, and so does this.
const INFO_PREFIX: &[u8] = b"II webpush transport\0";

/// What a key fetch left behind for the subscribe that follows it.
#[derive(Clone)]
pub(crate) struct Pending {
    /// The private half of the key the browser encrypted `auth` to.
    pub transport_scalar: [u8; 32],
    /// Expanded into this subscription's first sender key once a subscribe arrives, and
    /// held as a seed until then because a prompt the user denies never sends one.
    pub sender_seed: [u8; 32],
    pub minted_at_ns: Timestamp,
}

/// Who a pending record belongs to. The anchor is part of it because a browser id is
/// handed out per identity, so two identities both reach browser 1.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Caller {
    pub anchor_number: AnchorNumber,
    pub key: NotificationKey,
}

thread_local! {
    /// Not stable memory: losing these on an upgrade costs a subscribe a retry, which is
    /// cheaper than writing a five-minute secret to a durable store.
    static PENDING: RefCell<HashMap<Caller, Pending>> = RefCell::new(HashMap::new());
}

/// Holds what the subscribe will need, replacing whatever this browser left before.
///
/// One browser has one in flight: a second fetch means the browser is starting again, and
/// it will encrypt to the key that second fetch answered with.
pub(crate) fn remember(
    caller: Caller,
    transport_scalar: [u8; 32],
    sender_seed: [u8; 32],
    now_ns: Timestamp,
) {
    PENDING.with_borrow_mut(|pending| {
        pending.retain(|_, held| now_ns.saturating_sub(held.minted_at_ns) <= PENDING_TTL_NS);
        if pending.len() >= MAX_PENDING && !pending.contains_key(&caller) {
            let oldest = pending
                .iter()
                .min_by_key(|(_, held)| held.minted_at_ns)
                .map(|(caller, _)| *caller);
            if let Some(oldest) = oldest {
                pending.remove(&oldest);
            }
        }
        pending.insert(
            caller,
            Pending {
                transport_scalar,
                sender_seed,
                minted_at_ns: now_ns,
            },
        );
    });
}

/// Takes what this browser's key fetch left, which can only be spent once.
pub(crate) fn take(caller: Caller, now_ns: Timestamp) -> Option<Pending> {
    PENDING.with_borrow_mut(|pending| {
        let held = pending.remove(&caller)?;
        (now_ns.saturating_sub(held.minted_at_ns) <= PENDING_TTL_NS).then_some(held)
    })
}

/// The caller a pending record belongs to, as bytes the derivation can mix in. Tagged,
/// so a browser and an app that happen to share a number are still distinct.
pub(crate) fn caller_bytes(caller: Caller) -> Vec<u8> {
    let (tag, id) = match caller.key {
        NotificationKey::Browser(id) => (0u8, u64::from(id)),
        NotificationKey::App(id) => (1u8, id),
    };
    [
        &[tag][..],
        &caller.anchor_number.to_be_bytes()[..],
        &id.to_be_bytes()[..],
    ]
    .concat()
}

/// The public half a browser encrypts to, uncompressed SEC1.
pub(crate) fn public_key_of(transport_scalar: &[u8; 32]) -> Vec<u8> {
    super::randomness::secret_key_from(transport_scalar)
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
    let transport = super::randomness::secret_key_from(transport_scalar);
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
    let browser = super::randomness::secret_key_from(browser_scalar);
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

    const BROWSER: Caller = Caller {
        anchor_number: 10_000,
        key: NotificationKey::Browser(1),
    };
    const OTHER: Caller = Caller {
        anchor_number: 10_000,
        key: NotificationKey::Browser(2),
    };
    /// The same browser number under another identity, which is a different caller.
    const SAME_NUMBER_ELSEWHERE: Caller = Caller {
        anchor_number: 10_001,
        key: NotificationKey::Browser(1),
    };

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

    #[test]
    fn a_pending_record_is_spent_once() {
        remember(BROWSER, [1u8; 32], [2u8; 32], 0);
        assert!(take(BROWSER, 0).is_some());
        assert!(take(BROWSER, 0).is_none());
    }

    #[test]
    fn a_second_fetch_replaces_what_the_first_left() {
        remember(BROWSER, [1u8; 32], [2u8; 32], 0);
        remember(BROWSER, [3u8; 32], [4u8; 32], 0);
        let held = take(BROWSER, 0).expect("the second");
        assert_eq!(held.transport_scalar, [3u8; 32]);
    }

    #[test]
    fn a_pending_record_past_its_window_is_gone() {
        remember(BROWSER, [1u8; 32], [2u8; 32], 0);
        assert!(take(BROWSER, PENDING_TTL_NS + 1).is_none());
    }

    /// Browser ids are handed out per identity, so without the anchor one identity's
    /// subscribe would spend another's transport key.
    #[test]
    fn the_same_browser_number_under_another_identity_is_another_caller() {
        remember(BROWSER, [1u8; 32], [2u8; 32], 0);
        assert!(take(SAME_NUMBER_ELSEWHERE, 0).is_none());
        assert!(take(BROWSER, 0).is_some());
    }

    #[test]
    fn two_identities_do_not_derive_the_same_bytes() {
        assert_ne!(caller_bytes(BROWSER), caller_bytes(SAME_NUMBER_ELSEWHERE));
    }

    #[test]
    fn one_browser_does_not_take_another_s_key() {
        remember(BROWSER, [1u8; 32], [2u8; 32], 0);
        assert!(take(OTHER, 0).is_none());
        assert!(take(BROWSER, 0).is_some());
    }
}
