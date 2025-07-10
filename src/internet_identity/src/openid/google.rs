use super::OpenIDJWTVerificationError;
use crate::openid::OpenIdCredential;
use crate::openid::OpenIdProvider;
use crate::openid::MINUTE_NS;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use candid::{Deserialize, Nat};
use ic_cdk::api::management_canister::http_request::{HttpHeader, HttpResponse};
use ic_cdk::trap;
use ic_stable_structures::Storable;
use identity_jose::jwk::{Jwk, JwkParamsRsa};
use identity_jose::jws::JwsAlgorithm::RS256;
use identity_jose::jws::{
    Decoder, JwsVerifierFn, SignatureVerificationError, SignatureVerificationErrorKind,
    VerificationInput,
};
use internet_identity_interface::internet_identity::types::{MetadataEntryV2, OpenIdConfig};
use rsa::{Pkcs1v15Sign, RsaPublicKey};
use serde::Serialize;
use sha2::{Digest, Sha256};
#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Into;
use std::rc::Rc;

const ISSUER: &str = "https://accounts.google.com";

#[cfg(not(test))]
const CERTS_URL: &str = "https://www.googleapis.com/oauth2/v3/certs";

// The amount of cycles needed to make the HTTP outcall with a large enough margin
#[cfg(not(test))]
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

const HTTP_STATUS_OK: u8 = 200;

// Fetch the Google certs every fifteen minutes, the responses are always
// valid for at least 5 hours so that should be enough margin.
// Update 2025-08-09: We found error when verifying JWTs quite often. Moved from 1h to 15 min.
#[cfg(not(test))]
const FETCH_CERTS_INTERVAL: u64 = 60 * 15; // 15 minutes in seconds

const NANOSECONDS_PER_SECOND: u64 = 1_000_000_000;

// A JWT is only valid for a very small window, even if the JWT itself says it's valid for longer,
// we only need it right after it's being issued to create a JWT delegation with its own expiry.
// As the JWT is also used for registration, which may include longer user interaction,
// we are using 10 minutes to account for potential clock offsets as well as users.
const MAX_VALIDITY_WINDOW: u64 = 10 * MINUTE_NS; // Same as ingress expiry

// Maximum length of the email claim in the Google JWT, in practice we expect Google to already
// validate the email on their end for a sane maximum length. This is an additional sanity check.
const MAX_EMAIL_LENGTH: usize = 256;

// Maximum length of the name claim in the Google JWT, in practice we expect Google to already
// validate the name on their end for a sane maximum length. This is an additional sanity check.
const MAX_NAME_LENGTH: usize = 128;

// Maximum length of the picture URL claim in the Google JWT, in practice we expect Google to not
// send us a picture URL that is longer than needed. This is an additional sanity check.
const MAX_PICTURE_URL_LENGTH: usize = 256;

#[derive(Serialize, Deserialize)]
struct Certs {
    keys: Vec<Jwk>,
}

#[derive(Deserialize)]
struct Claims {
    iss: String,
    sub: String,
    aud: String,
    nonce: String,
    iat: u64,
    // Optional Google specific claims
    email: Option<String>,
    name: Option<String>,
    picture: Option<String>,
}

pub struct Provider {
    client_id: String,
    certs: Rc<RefCell<Vec<Jwk>>>,
}

impl OpenIdProvider for Provider {
    fn issuer(&self) -> &'static str {
        ISSUER
    }

    fn verify(
        &self,
        jwt: &str,
        salt: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        // Decode JWT and verify claims
        let validation_item = Decoder::new()
            .decode_compact_serialization(jwt.as_bytes(), None)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Unable to decode JWT".to_string())
            })?;
        let claims: Claims = serde_json::from_slice(validation_item.claims()).map_err(|_| {
            OpenIDJWTVerificationError::GenericError(
                "Unable to decode claims or expected claims are missing".to_string(),
            )
        })?;
        verify_claims(&self.client_id, &claims, salt)?;

        // Verify JWT signature
        let kid = validation_item
            .kid()
            .ok_or(OpenIDJWTVerificationError::GenericError(
                "JWT is missing kid".to_string(),
            ))?;
        let certs = self.certs.borrow();
        let cert = certs
            .iter()
            .find(|cert| cert.kid().is_some_and(|v| v == kid))
            .ok_or(OpenIDJWTVerificationError::GenericError(format!(
                "Certificate not found for {kid}"
            )))?;
        validation_item
            .verify(&JwsVerifierFn::from(verify_signature), cert)
            .map_err(|_| {
                OpenIDJWTVerificationError::GenericError("Invalid signature".to_string())
            })?;

        // Return credential with Google specific metadata
        let mut metadata: HashMap<String, MetadataEntryV2> = HashMap::new();
        if let Some(email) = claims.email {
            metadata.insert("email".into(), MetadataEntryV2::String(email));
        }
        if let Some(name) = claims.name {
            metadata.insert("name".into(), MetadataEntryV2::String(name));
        }
        if let Some(picture) = claims.picture {
            metadata.insert("picture".into(), MetadataEntryV2::String(picture));
        }
        Ok(OpenIdCredential {
            iss: claims.iss,
            sub: claims.sub,
            aud: claims.aud,
            last_usage_timestamp: None,
            metadata,
        })
    }

    fn metadata_name(&self, metadata: HashMap<String, MetadataEntryV2>) -> Option<String> {
        metadata.get("name").and_then(|entry| match entry {
            MetadataEntryV2::String(value) => Some(value.clone()),
            _ => None,
        })
    }
}

impl Provider {
    pub fn create(config: OpenIdConfig) -> Provider {
        #[cfg(test)]
        let certs = Rc::new(RefCell::new(TEST_CERTS.take()));

        #[cfg(not(test))]
        let certs: Rc<RefCell<Vec<Jwk>>> = Rc::new(RefCell::new(vec![]));

        #[cfg(not(test))]
        schedule_fetch_certs(Rc::clone(&certs), None);

        Provider {
            client_id: config.client_id,
            certs,
        }
    }
}

#[cfg(not(test))]
fn schedule_fetch_certs(certs_reference: Rc<RefCell<Vec<Jwk>>>, delay: Option<u64>) {
    use ic_cdk::spawn;
    use ic_cdk_timers::set_timer;
    use std::cmp::min;
    use std::time::Duration;

    set_timer(Duration::from_secs(delay.unwrap_or(0)), move || {
        spawn(async move {
            let new_delay = match fetch_certs().await {
                Ok(google_certs) => {
                    certs_reference.replace(google_certs);
                    FETCH_CERTS_INTERVAL
                }
                // Try again earlier with backoff if fetch failed, the HTTP outcall responses
                // aren't the same across nodes when we fetch at the moment of key rotation.
                Err(_) => min(FETCH_CERTS_INTERVAL, delay.unwrap_or(60) * 2),
            };
            schedule_fetch_certs(certs_reference, Some(new_delay));
        });
    });
}

#[cfg(not(test))]
async fn fetch_certs() -> Result<Vec<Jwk>, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpMethod,
    };

    let request = CanisterHttpRequestArgument {
        url: CERTS_URL.into(),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: None,
        transform: None,
        headers: vec![
            HttpHeader {
                name: "Accept".into(),
                value: "application/json".into(),
            },
            HttpHeader {
                name: "User-Agent".into(),
                value: "internet_identity_canister".into(),
            },
        ],
    };

    let (response,) = http_request_with_closure(request, CERTS_CALL_CYCLES, transform_certs)
        .await
        .map_err(|(_, err)| err)?;

    serde_json::from_slice::<Certs>(response.body.as_slice())
        .map_err(|_| "Invalid JSON".into())
        .map(|res| res.keys)
}

// The Google API occasionally returns a response with keys and their properties in random order,
// so we deserialize, sort the keys and serialize to make the response the same across all nodes.
//
// This function traps since HTTP outcall transforms can't return or log errors anyway.
#[allow(clippy::needless_pass_by_value)]
fn transform_certs(response: HttpResponse) -> HttpResponse {
    if response.status != HTTP_STATUS_OK {
        trap("Invalid response status")
    };

    let certs: Certs =
        serde_json::from_slice(response.body.as_slice()).unwrap_or_else(|_| trap("Invalid JSON"));

    let mut sorted_keys = certs.keys.clone();
    sorted_keys.sort_by_key(|key| key.kid().unwrap_or_else(|| trap("Invalid JSON")).to_owned());

    let body =
        serde_json::to_vec(&Certs { keys: sorted_keys }).unwrap_or_else(|_| trap("Invalid JSON"));

    // All headers are ignored including the Cache-Control header, instead we fetch the certs
    // hourly since responses are always valid for at least 5 hours based on analysis of the
    // Cache-Control header over a timespan of multiple days, so hourly is a large enough margin.
    HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body,
    }
}

fn create_rsa_public_key(jwk: &Jwk) -> Result<RsaPublicKey, String> {
    // Extract the RSA parameters (modulus 'n' and exponent 'e') from the JWK.
    let JwkParamsRsa { n, e, .. } = jwk
        .try_rsa_params()
        .map_err(|_| "Unable to extract modulus and exponent")?;

    // Decode the base64-url encoded modulus 'n' of the RSA public key.
    let n = BASE64_URL_SAFE_NO_PAD
        .decode(n)
        .map_err(|_| "Unable to decode modulus")?;

    // Decode the base64-url encoded public exponent 'e' of the RSA public key.
    let e = BASE64_URL_SAFE_NO_PAD
        .decode(e)
        .map_err(|_| "Unable to decode exponent")?;

    // Construct the RSA public key using the decoded modulus and exponent.
    RsaPublicKey::new(
        rsa::BigUint::from_bytes_be(&n),
        rsa::BigUint::from_bytes_be(&e),
    )
    .map_err(|_| "Unable to construct RSA public key".into())
}

/// Verifier implementation for `identity_jose` that verifies the signature of a JWT.
///
/// - `input`: A `VerificationInput` struct containing the JWT's algorithm (`alg`),
///   the signing input (payload to be hashed and verified), and the decoded signature.
/// - `jwk`: A reference to a `Jwk` (JSON Web Key) that contains the RSA public key
///   parameters (`n` and `e`) used to verify the JWT signature.
#[allow(clippy::needless_pass_by_value)]
fn verify_signature(input: VerificationInput, jwk: &Jwk) -> Result<(), SignatureVerificationError> {
    // Ensure the algorithm specified in the JWT header matches the expected algorithm (RS256).
    // JSON Web Keys (JWK) returned from Google API (v3) always use RSA with SHA-256.
    // If the algorithm does not match, return an UnsupportedAlg error.
    if input.alg != RS256 {
        return Err(SignatureVerificationErrorKind::UnsupportedAlg.into());
    }

    // Compute the SHA-256 hash of the JWT payload (the signing input).
    // This hashed value will be used for signature verification.
    let hashed_input = Sha256::digest(input.signing_input);

    // Define the signature scheme to be used for verification (RSA PKCS#1 v1.5 with SHA-256).
    let scheme = Pkcs1v15Sign::new::<Sha256>();

    // Create RSA public key from JWK
    let public_key = create_rsa_public_key(jwk).map_err(|_| {
        SignatureVerificationError::new(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;

    // Verify the JWT signature using the RSA public key and the defined signature scheme.
    // If the signature is invalid, return an InvalidSignature error.
    public_key
        .verify(scheme, &hashed_input, input.decoded_signature.as_ref())
        .map_err(|_| SignatureVerificationErrorKind::InvalidSignature.into())
}

fn verify_claims(
    client_id: &String,
    claims: &Claims,
    salt: &[u8; 32],
) -> Result<(), OpenIDJWTVerificationError> {
    let now = time();
    let mut hasher = Sha256::new();
    hasher.update(salt);
    hasher.update(caller().to_bytes());
    let hash: [u8; 32] = hasher.finalize().into();
    let expected_nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

    if claims.iss != ISSUER {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid issuer: {}",
            claims.iss
        )));
    }
    if &claims.aud != client_id {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid audience: {}",
            claims.aud
        )));
    }
    if claims.nonce != expected_nonce {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid nonce: {}",
            claims.nonce
        )));
    }
    if now > claims.iat * NANOSECONDS_PER_SECOND + MAX_VALIDITY_WINDOW {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    }
    if now < claims.iat * NANOSECONDS_PER_SECOND {
        return Err(OpenIDJWTVerificationError::GenericError(
            "JWT is not valid yet".into(),
        ));
    }
    if claims
        .email
        .as_ref()
        .is_some_and(|val| val.len() > MAX_EMAIL_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Email too long".into(),
        ));
    }
    if claims
        .name
        .as_ref()
        .is_some_and(|val| val.len() > MAX_NAME_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Name too long".into(),
        ));
    }
    if claims
        .picture
        .as_ref()
        .is_some_and(|val| val.len() > MAX_PICTURE_URL_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Picture URL too long".into(),
        ));
    }

    Ok(())
}

#[cfg(test)]
thread_local! {
    static TEST_CALLER: Cell<Principal> = Cell::new(Principal::from_text("x4gp4-hxabd-5jt4d-wc6uw-qk4qo-5am4u-mncv3-wz3rt-usgjp-od3c2-oae").unwrap());
    static TEST_TIME: Cell<u64> = const { Cell::new(1_736_794_102 * NANOSECONDS_PER_SECOND) };
    static TEST_CERTS: Cell<Vec<Jwk>> = Cell::new(serde_json::from_str::<Certs>(r#"{"keys":[{"n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use": "sig","kty": "RSA","alg": "RS256","kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847","e": "AQAB"}]}"#).unwrap().keys);
}

#[cfg(not(test))]
fn caller() -> Principal {
    ic_cdk::caller()
}

#[cfg(test)]
fn caller() -> Principal {
    TEST_CALLER.get()
}

#[cfg(not(test))]
fn time() -> u64 {
    ic_cdk::api::time()
}

#[cfg(test)]
fn time() -> u64 {
    TEST_TIME.get()
}

#[test]
fn should_transform_certs_to_same() {
    let input = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![HttpHeader {
            name: "Cache-Control".into(),
            value: "public, max-age=18544, must-revalidate, no-transform".into()
        }],
        body: Vec::from(br#"{"keys":[{"e":"AQAB","alg":"RS256","kty":"RSA","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","use":"sig"},{"use":"sig","alg":"RS256","kty":"RSA","e":"AQAB","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","kid":"89ce3598c473af1bda4bff95e6c8736450206fba"}]}"#),
    };
    let expected = HttpResponse {
        status: Nat::from(HTTP_STATUS_OK),
        headers: vec![],
        body: Vec::from(br#"{"keys":[{"kty":"RSA","use":"sig","alg":"RS256","kid":"89ce3598c473af1bda4bff95e6c8736450206fba","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","e":"AQAB"},{"kty":"RSA","use":"sig","alg":"RS256","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","e":"AQAB"}]}"#),
    };

    assert_eq!(transform_certs(input), expected);
}

#[cfg(test)]
fn test_data() -> (String, [u8; 32], Claims) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let salt: [u8; 32] = [
        143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81,
        139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
    ];
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();

    (jwt.into(), salt, claims)
}

#[test]
fn should_return_credential() {
    let (jwt, salt, claims) = test_data();
    let provider = Provider::create(OpenIdConfig {
        client_id: claims.aud.clone(),
    });
    let credential = OpenIdCredential {
        iss: claims.iss,
        sub: claims.sub,
        aud: claims.aud,
        last_usage_timestamp: None,
        metadata: HashMap::from([
            (
                "email".into(),
                MetadataEntryV2::String(claims.email.unwrap()),
            ),
            ("name".into(), MetadataEntryV2::String(claims.name.unwrap())),
            (
                "picture".into(),
                MetadataEntryV2::String(claims.picture.unwrap()),
            ),
        ]),
    };

    assert_eq!(provider.verify(&jwt, &salt), Ok(credential));
}

#[test]
fn should_return_error_when_encoding_invalid() {
    let (_, salt, claims) = test_data();
    let provider = Provider::create(OpenIdConfig {
        client_id: claims.aud.clone(),
    });
    let invalid_jwt = "invalid-jwt";

    assert_eq!(
        provider.verify(invalid_jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Unable to decode JWT".into()
        ))
    );
}

#[test]
fn should_return_error_when_cert_missing() {
    TEST_CERTS.replace(vec![]);
    let (jwt, salt, claims) = test_data();
    let provider = Provider::create(OpenIdConfig {
        client_id: claims.aud.clone(),
    });

    assert_eq!(
        provider.verify(&jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Certificate not found for dd125d5f462fbc6014aedab81ddf3bcedab70847".into()
        ))
    );
}

#[test]
fn should_return_error_when_signature_invalid() {
    let (jwt, salt, claims) = test_data();
    let provider = Provider::create(OpenIdConfig {
        client_id: claims.aud.clone(),
    });
    let chunks: Vec<&str> = jwt.split('.').collect();
    let header = chunks[0];
    let payload = chunks[1];
    let invalid_signature = "f47b0sNskm-85sT5XtoRzORnfobK2nzVFF8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww5Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let invalid_jwt = [header, payload, invalid_signature].join(".");

    assert_eq!(
        provider.verify(&invalid_jwt, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid signature".into()
        ))
    );
}

#[test]
fn should_return_error_when_invalid_issuer() {
    let (_, salt, claims) = test_data();
    let client_id = claims.aud.clone();
    let mut invalid_claims = claims;
    invalid_claims.iss = "invalid-issuer".into();

    assert_eq!(
        verify_claims(&client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid issuer: {}",
            invalid_claims.iss
        )))
    );
}

#[test]
fn should_return_error_when_invalid_audience() {
    let (_, salt, claims) = test_data();
    let client_id = claims.aud.clone();
    let mut invalid_claims = claims;
    invalid_claims.aud = "invalid-audience".into();

    assert_eq!(
        verify_claims(&client_id, &invalid_claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid audience: {}",
            invalid_claims.aud
        )))
    );
}

#[test]
fn should_return_error_when_invalid_salt() {
    let (_, _, claims) = test_data();
    let client_id = &claims.aud;
    let invalid_salt: [u8; 32] = [
        143, 79, 58, 224, 18, 15, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81, 139,
        161, 218, 224, 243, 4, 120, 26, 123, 229, 242, 200, 189,
    ];

    assert_eq!(
        verify_claims(client_id, &claims, &invalid_salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into()
        ))
    );
}

#[test]
fn should_return_error_when_invalid_caller() {
    TEST_CALLER.replace(
        Principal::from_text("necp6-24oof-6e2i2-xg7fk-pawxw-nlol2-by5bb-mltvt-sazk6-nqrzz-zae")
            .unwrap(),
    );
    let (_, salt, claims) = test_data();
    let client_id = &claims.aud;

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into()
        ))
    );
}

#[test]
fn should_return_error_when_no_longer_valid() {
    TEST_TIME.replace(time() + MAX_VALIDITY_WINDOW + 1);
    let (_, salt, claims) = test_data();
    let client_id = &claims.aud;

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::JWTExpired)
    );
}

#[test]
fn should_return_error_when_not_valid_yet() {
    TEST_TIME.replace(time() - 1);
    let (_, salt, claims) = test_data();
    let client_id = &claims.aud;

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "JWT is not valid yet".into()
        ))
    );
}

#[test]
fn should_return_error_when_email_too_long() {
    let (_, salt, mut claims) = test_data();
    let client_id = &claims.aud;
    claims.email = Some("thisisanemailaddresswhichistoolongaccordingtothemaxlengththatisallowedbythestandardemailprotocolsandshouldnotbeconsideredasvalidbutitisusefultotestvalidationmechanismsintheapplicationwhichmayexceedstandardlimitationsforemailaddressesandshouldbetested@gmail.com".into());

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Email too long".into()
        ))
    );
}

#[test]
fn should_return_error_when_name_too_long() {
    let (_, salt, mut claims) = test_data();
    let client_id = &claims.aud;
    claims.name = Some("Jonathan Maximilian Theodore Alexander Montgomery Fitzgerald Jameson Davidson Hawthorne Winchester Baldwin the Fifth of Lancaster".into());

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Name too long".into()
        ))
    );
}

#[test]
fn should_return_error_when_picture_url_too_long() {
    let (_, salt, mut claims) = test_data();
    let client_id = &claims.aud;
    claims.picture = Some("https://lh3.googleusercontent.com/a/DFsf8fDFfldjfF8z_Hfdfsf8-lkdjFDF83f3f=s96-c&extraPadding=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".into());

    assert_eq!(
        verify_claims(client_id, &claims, &salt),
        Err(OpenIDJWTVerificationError::GenericError(
            "Picture URL too long".into()
        ))
    );
}
