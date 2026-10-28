//! The VAPID identity Internet Identity signs its wake-ups with.
//!
//! RFC 8292 authorizes a push by an ES256 JWT over the relay's origin, so the signer must
//! hold a P-256 key. Threshold ECDSA over `secp256r1` is that key: one identity for the
//! whole canister, which is what a relay's `k=` names and what a browser binds its
//! subscription to.
//!
//! A JWT says only which relay it is for and when it expires, so one covers every
//! subscription on that relay until it lapses. They are cached per relay origin and
//! signed through [`crate::single_flight_cache`]: a refresh runs while the one in hand is
//! still served, which is what keeps signing off the path a notification takes.

use candid::{CandidType, Principal};
use serde::Deserialize;
use sha2::{Digest, Sha256};

use crate::single_flight_cache::{CacheConfig, FillOutcome, RetryBackoff, SingleFlightCache};

/// What the management canister calls the curve RFC 8292 requires.
#[derive(CandidType, Deserialize, Clone, Debug)]
enum EcdsaCurve {
    #[serde(rename = "secp256k1")]
    #[allow(dead_code)]
    Secp256k1,
    #[serde(rename = "secp256r1")]
    Secp256r1,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
struct EcdsaKeyId {
    curve: EcdsaCurve,
    name: String,
}

#[derive(CandidType, Deserialize)]
#[cfg_attr(test, allow(dead_code))]
struct EcdsaPublicKeyArgument {
    canister_id: Option<Principal>,
    derivation_path: Vec<Vec<u8>>,
    key_id: EcdsaKeyId,
}

#[derive(CandidType, Deserialize)]
#[cfg_attr(test, allow(dead_code))]
struct EcdsaPublicKeyResponse {
    public_key: Vec<u8>,
    #[allow(dead_code)]
    chain_code: Vec<u8>,
}

#[derive(CandidType, Deserialize)]
#[cfg_attr(test, allow(dead_code))]
struct SignWithEcdsaArgument {
    message_hash: Vec<u8>,
    derivation_path: Vec<Vec<u8>>,
    key_id: EcdsaKeyId,
}

#[derive(CandidType, Deserialize)]
#[cfg_attr(test, allow(dead_code))]
struct SignWithEcdsaResponse {
    signature: Vec<u8>,
}

/// What a signature costs. Attached rather than assumed: the call is refused outright
/// where too little rides with it, and anything unspent comes back.
#[cfg_attr(test, allow(dead_code))]
const SIGNATURE_CYCLES: u128 = 26_153_846_153;

/// One identity for the canister. A relay rejects a push whose `k=` is not the key its
/// subscription was made with, so this is fixed for as long as any subscription lives.
const DERIVATION_PATH: Vec<Vec<u8>> = Vec::new();

/// The production threshold key, which is also the name PocketIC holds one under, so a
/// test signs with the same identity the network does.
const KEY_NAME: &str = "key_1";

/// The `aud` a JWT is signed for: RFC 8292 names the origin of the push resource, which
/// is what the browser's own `new URL(endpoint).origin` answers.
pub(crate) fn relay_origin_of(endpoint: &str) -> Option<String> {
    let origin = url::Url::parse(endpoint).ok()?.origin();
    origin.is_tuple().then(|| origin.ascii_serialization())
}

fn key_id() -> EcdsaKeyId {
    EcdsaKeyId {
        curve: EcdsaCurve::Secp256r1,
        name: KEY_NAME.to_string(),
    }
}

/// Our VAPID public key, uncompressed SEC1, which is what a browser subscribes with and
/// what the relay is handed as `k=`.
///
/// The management canister answers in compressed form, and neither the browser nor the
/// relay takes that, so the point is expanded here.
pub(crate) async fn fetch_public_key() -> Result<Vec<u8>, String> {
    let (answer,): (EcdsaPublicKeyResponse,) = ic_cdk::api::call::call(
        Principal::management_canister(),
        "ecdsa_public_key",
        (EcdsaPublicKeyArgument {
            canister_id: None,
            derivation_path: DERIVATION_PATH,
            key_id: key_id(),
        },),
    )
    .await
    .map_err(|(code, message)| format!("ecdsa_public_key: {code:?} {message}"))?;

    use p256::elliptic_curve::sec1::ToEncodedPoint;
    let point = p256::PublicKey::from_sec1_bytes(&answer.public_key)
        .map_err(|err| format!("the signing key is not a P-256 point: {err}"))?;
    Ok(point.to_encoded_point(false).as_bytes().to_vec())
}

/// RFC 8292 §2: the header is fixed, so it is written once rather than built per JWT.
const HEADER_B64: &str = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9";

/// Operator contact for the RFC 8292 `sub` claim.
const VAPID_SUBJECT: &str = "https://id.ai";

/// Signs one JWT for `relay_origin`, expiring `lifetime_secs` from now.
async fn sign_jwt(relay_origin: &str, now_secs: u64, lifetime_secs: u64) -> Result<String, String> {
    use base64::engine::general_purpose::URL_SAFE_NO_PAD;
    use base64::Engine;

    let aud = serde_json::to_string(relay_origin).map_err(|err| format!("the aud: {err}"))?;
    let exp = now_secs.saturating_add(lifetime_secs);
    let payload = format!(r#"{{"aud":{aud},"exp":{exp},"sub":"{VAPID_SUBJECT}"}}"#);
    let signing_input = format!("{HEADER_B64}.{}", URL_SAFE_NO_PAD.encode(payload));

    let message_hash = Sha256::digest(signing_input.as_bytes()).to_vec();

    // Off-wasm there is no management canister to sign with, so the hash stands in for
    // the signature: everything a relay reads but the signature itself is still built.
    #[cfg(test)]
    return Ok(format!(
        "{signing_input}.{}",
        URL_SAFE_NO_PAD.encode(&message_hash)
    ));

    #[cfg(not(test))]
    let (answer,): (SignWithEcdsaResponse,) = ic_cdk::api::call::call_with_payment128(
        Principal::management_canister(),
        "sign_with_ecdsa",
        (SignWithEcdsaArgument {
            message_hash,
            derivation_path: DERIVATION_PATH,
            key_id: key_id(),
        },),
        SIGNATURE_CYCLES,
    )
    .await
    .map_err(|(code, message)| format!("sign_with_ecdsa: {code:?} {message}"))?;

    // ES256 is the raw pair, which is what the management canister answers with.
    #[cfg(not(test))]
    Ok(format!(
        "{signing_input}.{}",
        URL_SAFE_NO_PAD.encode(&answer.signature)
    ))
}

/// The second a JWT is signed in. Off-wasm the canister clock cannot be read, and what a
/// test asserts is the shape of what was signed rather than when.
fn now_secs() -> u64 {
    #[cfg(not(test))]
    {
        ic_cdk::api::time() / 1_000_000_000
    }
    #[cfg(test)]
    {
        1_700_000_000
    }
}

/// How long a signed JWT is accepted for. Short, because nothing waits on a refresh: the
/// one in hand is served while the next is signed.
const JWT_LIFETIME_SECS: u64 = 300;

/// When a refresh starts. Half the lifetime leaves the rest as the window a signature has
/// to land in, which is far longer than one takes.
const JWT_FRESH_FOR_SECS: u64 = JWT_LIFETIME_SECS / 2;

/// How long the one in hand goes on being served while its successor is signed. Ends
/// before the lifetime does, so nothing is ever posted that a relay would refuse.
const JWT_STALE_FOR_SECS: u64 = JWT_LIFETIME_SECS / 2 - 30;

/// Relays we hold a JWT for. Bounded because the set follows the endpoints registered.
const JWT_MAX_RELAYS: usize = 32;

const JWT_RETRY_BASE_SECS: u64 = 2;
const JWT_RETRY_MULTIPLIER: u64 = 2;

/// Comfortably past a signature's round trip, so a fill is taken over only once the task
/// carrying it is gone.
const JWT_ABANDON_FILL_AFTER_SECS: u64 = 60;

/// Our public key, where it has been asked for. A browser cannot subscribe before this
/// lands, and a push cannot name a key we do not hold.
pub(crate) fn public_key() -> Option<Vec<u8>> {
    crate::state::persistent_state(|p| p.vapid_public_key.clone())
}

thread_local! {
    /// One fetch at a time, for the same reason a signature has one: the answer is the
    /// same whoever asked, and the key is asked for again on the next pass.
    static FETCHING_PUBLIC_KEY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Asks for the key if we hold none and nobody is asking. Answers nothing either way:
/// whoever needed it reads [`public_key`] again once it has landed.
pub(crate) fn ensure_public_key() {
    if public_key().is_some() || FETCHING_PUBLIC_KEY.with(|one| one.replace(true)) {
        return;
    }
    ic_cdk::spawn(async {
        let fetched = fetch_public_key().await;
        FETCHING_PUBLIC_KEY.with(|one| one.set(false));
        match fetched {
            Ok(key) => crate::state::persistent_state_mut(|p| p.vapid_public_key = Some(key)),
            Err(err) => ic_cdk::println!("ERROR: could not read our VAPID public key: {err}"),
        }
    });
}

pub(crate) type JwtCache = SingleFlightCache<String, String, String>;

pub(crate) fn new_jwt_cache() -> JwtCache {
    SingleFlightCache::new(
        |relay_origin: String| async move {
            let now_secs = now_secs();
            match sign_jwt(&relay_origin, now_secs, JWT_LIFETIME_SECS).await {
                Ok(jwt) => FillOutcome::Ready(jwt),
                Err(err) => {
                    ic_cdk::println!("ERROR: could not sign a VAPID JWT for {relay_origin}: {err}");
                    FillOutcome::Failed(err)
                }
            }
        },
        CacheConfig {
            fresh_for: JWT_FRESH_FOR_SECS,
            stale_for: JWT_STALE_FOR_SECS,
            stale_if_error_for: JWT_STALE_FOR_SECS,
            max_entries: JWT_MAX_RELAYS,
            backoff: RetryBackoff::new(JWT_RETRY_BASE_SECS, JWT_RETRY_MULTIPLIER),
            abandon_fill_after: JWT_ABANDON_FILL_AFTER_SECS,
        },
    )
}
