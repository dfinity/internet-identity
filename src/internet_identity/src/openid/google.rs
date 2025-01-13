use crate::constants;
use crate::openid::OpenIdCredential;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Principal;
use candid::{Deserialize, Nat};
use ic_cdk::api::management_canister::http_request::{
    http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod, HttpResponse,
};
use ic_cdk::{spawn, trap};
use ic_cdk_timers::set_timer;
use ic_stable_structures::Storable;
use identity_jose::jwk::{Jwk, JwkParamsRsa};
use identity_jose::jws::JwsAlgorithm::RS256;
use identity_jose::jws::{
    Decoder, JwsVerifierFn, SignatureVerificationError, SignatureVerificationErrorKind,
    VerificationInput,
};
use internet_identity_interface::internet_identity::types::{MetadataEntryV2, Timestamp};
use rsa::{Pkcs1v15Sign, RsaPublicKey};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashMap;
use std::convert::Into;
use std::time::Duration;

pub const ISSUER: &str = "https://accounts.google.com";

const AUDIENCE: &str = constants::OPENID_GOOGLE_CLIENT_ID;

const CERTS_URL: &str = "https://www.googleapis.com/oauth2/v3/certs";

// The amount of cycles needed to make the HTTP outcall with a large enough margin
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

const HTTP_STATUS_OK: u8 = 200;

// Fetch the Google certs every hour, the responses are always
// valid for at least 5 hours so that should be enough margin.
const FETCH_CERTS_INTERVAL: u64 = 60 * 60;

// A JWT is only valid for a very small window, even if the JWT itself says it's valid for longer,
// we only need it right after it's being issued to create a JWT delegation with its own expiry.
const MAX_VALIDITY_WINDOW: u64 = 60_000_000_000; // 5 minutes in nanos, same as ingress expiry

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
    // Google specific claims
    email: String,
    name: String,
    picture: String,
}

thread_local! {
    static CERTS: RefCell<Vec<Jwk>> = const { RefCell::new(vec![]) };
}

pub fn setup_timers() {
    // Fetch the certs directly after canister initialization.
    schedule_fetch_certs(None);
}

fn schedule_fetch_certs(delay: Option<u64>) {
    set_timer(Duration::from_secs(delay.unwrap_or(0)), move || {
        spawn(async move {
            let new_delay = match fetch_certs().await {
                Ok(google_certs) => {
                    CERTS.replace(google_certs);
                    FETCH_CERTS_INTERVAL
                }
                // Try again earlier with backoff if fetch failed, the HTTP outcall responses
                // aren't the same across nodes when we fetch at the moment of key rotation.
                Err(_) => min(FETCH_CERTS_INTERVAL, delay.unwrap_or(60) * 2),
            };
            schedule_fetch_certs(Some(new_delay));
        });
    });
}

async fn fetch_certs() -> Result<Vec<Jwk>, String> {
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

pub fn verify(
    jwt: &str,
    session_principal: &Principal,
    session_salt: &[u8; 32],
    timestamp: Timestamp,
) -> Result<OpenIdCredential, String> {
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| "Unable to decode JWT")?;
    let kid = validation_item.kid().ok_or("JWT is missing kid")?;
    let certs = CERTS.with(|certs| certs.borrow().clone());
    let cert = certs
        .iter()
        .find(|cert| cert.kid().is_some_and(|v| v == kid))
        .ok_or(format!("Certificate not found for {kid}"))?;

    let claims: Claims = serde_json::from_slice(validation_item.claims())
        .map_err(|_| "Unable to decode claims or expected claims are missing")?;

    validation_item
        .verify(&JwsVerifierFn::from(verify_signature), cert)
        .map_err(|_| "Invalid signature")?;
    verify_claims(&claims, session_principal, session_salt, timestamp)?;

    Ok(OpenIdCredential {
        iss: claims.iss,
        sub: claims.sub,
        aud: claims.aud,
        principal: Principal::anonymous(),
        last_usage_timestamp: timestamp,
        metadata: HashMap::from([
            ("email".into(), MetadataEntryV2::String(claims.email)),
            ("name".into(), MetadataEntryV2::String(claims.name)),
            ("picture".into(), MetadataEntryV2::String(claims.picture)),
        ]),
    })
}

#[allow(clippy::needless_pass_by_value)]
fn verify_signature(input: VerificationInput, jwk: &Jwk) -> Result<(), SignatureVerificationError> {
    if input.alg != RS256 {
        return Err(SignatureVerificationErrorKind::UnsupportedAlg.into());
    }

    let hashed_input = Sha256::digest(input.signing_input);
    let scheme = Pkcs1v15Sign::new::<Sha256>();
    let JwkParamsRsa { n, e, .. } = jwk.try_rsa_params().map_err(|_| {
        SignatureVerificationError::from(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;
    let n = BASE64_URL_SAFE_NO_PAD.decode(n).map_err(|_| {
        SignatureVerificationError::from(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;
    let e = BASE64_URL_SAFE_NO_PAD.decode(e).map_err(|_| {
        SignatureVerificationError::from(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;
    let rsa_key = RsaPublicKey::new(
        rsa::BigUint::from_bytes_be(&n),
        rsa::BigUint::from_bytes_be(&e),
    )
    .map_err(|_| {
        SignatureVerificationError::from(SignatureVerificationErrorKind::KeyDecodingFailure)
    })?;

    rsa_key
        .verify(scheme, &hashed_input, input.decoded_signature.as_ref())
        .map_err(|_| SignatureVerificationErrorKind::InvalidSignature.into())
}

fn verify_claims(
    claims: &Claims,
    session_principal: &Principal,
    session_salt: &[u8; 32],
    timestamp: Timestamp,
) -> Result<(), String> {
    let mut hasher = Sha256::new();
    hasher.update(session_salt);
    hasher.update(session_principal.to_bytes());
    let hash: [u8; 32] = hasher.finalize().into();
    let expected_nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

    if claims.iss != ISSUER {
        return Err(format!("Invalid issuer: {}", claims.iss));
    }
    if claims.aud != AUDIENCE {
        return Err(format!("Invalid audience: {}", claims.aud));
    }
    if claims.nonce != expected_nonce {
        return Err(format!("Invalid nonce: {}", claims.nonce));
    }
    if timestamp > claims.iat * 1_000_000_000 + MAX_VALIDITY_WINDOW {
        return Err("JWT is no longer valid".into());
    }
    if timestamp < claims.iat * 1_000_000_000 {
        return Err("JWT is not valid yet".into());
    }

    Ok(())
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
fn valid_verification_test_data() -> (String, Certs, Principal, [u8; 32], Timestamp, Claims) {
    // This JWT is for testing purposes, it's already been expired before this commit has been made,
    // additionally the audience of this JWT is a test Google client registration, not production.
    let jwt = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let certs: Certs = serde_json::from_str(r#"{
          "keys": [
            {
              "n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q",
              "use": "sig",
              "kty": "RSA",
              "alg": "RS256",
              "kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847",
              "e": "AQAB"
            }
          ]
        }"#).unwrap();
    let session_principal =
        Principal::from_text("x4gp4-hxabd-5jt4d-wc6uw-qk4qo-5am4u-mncv3-wz3rt-usgjp-od3c2-oae")
            .unwrap();
    let session_salt: [u8; 32] = [
        143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81,
        139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
    ];
    let timestamp: u64 = 1_736_794_102_000_000_000;
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .unwrap();
    let claims: Claims = serde_json::from_slice(validation_item.claims()).unwrap();

    (
        jwt.into(),
        certs,
        session_principal,
        session_salt,
        timestamp,
        claims,
    )
}

#[test]
fn should_return_credential() {
    let (jwt, certs, session_principal, session_salt, timestamp, claims) =
        valid_verification_test_data();
    CERTS.replace(certs.keys);
    let credential = OpenIdCredential {
        iss: claims.iss,
        sub: claims.sub,
        aud: claims.aud,
        principal: Principal::anonymous(),
        last_usage_timestamp: timestamp,
        metadata: HashMap::from([
            ("email".into(), MetadataEntryV2::String(claims.email)),
            ("name".into(), MetadataEntryV2::String(claims.name)),
            ("picture".into(), MetadataEntryV2::String(claims.picture)),
        ]),
    };

    assert_eq!(
        verify(&jwt, &session_principal, &session_salt, timestamp),
        Ok(credential)
    );
}

#[test]
fn cert_should_be_missing() {
    let (jwt, _, session_principal, session_salt, timestamp, _) = valid_verification_test_data();
    CERTS.replace(vec![]);

    assert_eq!(
        verify(&jwt, &session_principal, &session_salt, timestamp),
        Err("Certificate not found for dd125d5f462fbc6014aedab81ddf3bcedab70847".into())
    );
}

#[test]
fn signature_should_be_invalid() {
    let (jwt, certs, session_principal, session_salt, timestamp, _) =
        valid_verification_test_data();
    CERTS.replace(certs.keys);
    let chunks: Vec<&str> = jwt.split('.').collect();
    let header = chunks[0];
    let payload = chunks[1];
    let invalid_signature = "f47b0sNskm-85sT5XtoRzORnfobK2nzVFF8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww5Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";
    let invalid_jwt = [header, payload, invalid_signature].join(".");

    assert_eq!(
        verify(&invalid_jwt, &session_principal, &session_salt, timestamp),
        Err("Invalid signature".into())
    );
}

#[test]
fn issuer_should_be_invalid() {
    let (_, _, session_principal, session_salt, timestamp, claims) = valid_verification_test_data();
    let mut invalid_claims = claims;
    invalid_claims.iss = "invalid-issuer".into();
    assert_eq!(
        verify_claims(
            &invalid_claims,
            &session_principal,
            &session_salt,
            timestamp
        ),
        Err(format!("Invalid issuer: {}", invalid_claims.iss))
    );
}

#[test]
fn audience_should_be_invalid() {
    let (_, _, session_principal, session_salt, timestamp, claims) = valid_verification_test_data();
    let mut invalid_claims = claims;
    invalid_claims.aud = "invalid-audience".into();
    assert_eq!(
        verify_claims(
            &invalid_claims,
            &session_principal,
            &session_salt,
            timestamp
        ),
        Err(format!("Invalid audience: {}", invalid_claims.aud))
    );
}

#[test]
fn nonce_should_be_invalid() {
    let (_, _, session_principal, session_salt, timestamp, claims) = valid_verification_test_data();
    let invalid_session_principal =
        Principal::from_text("necp6-24oof-6e2i2-xg7fk-pawxw-nlol2-by5bb-mltvt-sazk6-nqrzz-zae")
            .unwrap();
    let invalid_session_salt: [u8; 32] = [
        143, 79, 58, 224, 18, 15, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83, 81, 139,
        161, 218, 224, 243, 4, 120, 26, 123, 229, 242, 200, 189,
    ];

    assert_eq!(
        verify_claims(
            &claims,
            &invalid_session_principal,
            &session_salt,
            timestamp
        ),
        Err("Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into())
    );
    assert_eq!(
        verify_claims(
            &claims,
            &session_principal,
            &invalid_session_salt,
            timestamp
        ),
        Err("Invalid nonce: etiDaLGcRdm5-rcqe0ZQUeMgpfp4v9TOOYUPbhRx7nI".into())
    );
}

#[test]
fn should_be_no_longer_invalid() {
    let (_, _, session_principal, session_salt, timestamp, claims) = valid_verification_test_data();

    assert_eq!(
        verify_claims(
            &claims,
            &session_principal,
            &session_salt,
            timestamp + MAX_VALIDITY_WINDOW + 1
        ),
        Err("JWT is no longer valid".into())
    );
}
#[test]
fn should_be_not_valid_yet() {
    let (_, _, session_principal, session_salt, timestamp, claims) = valid_verification_test_data();

    assert_eq!(
        verify_claims(&claims, &session_principal, &session_salt, timestamp - 1),
        Err("JWT is not valid yet".into())
    );
}
