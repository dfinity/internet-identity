//! The single JWT verification pipeline, shared by every provider kind.
//!
//! Configured providers (Google / Microsoft / Apple) and SSO (discoverable)
//! providers differ in only two places: *where the JWKs come from* (see
//! [`super::jwks::JwkSource`]) and a handful of descriptor fields (an issuer
//! template vs. a concrete issuer, and the `sso_domain` / `sso_name` stamp).
//! Everything from decoding the JWT through verifying the signature and
//! building the [`OpenIdCredential`] lives here and runs identically for both
//! — this module is "the implementation up to the JWK read", and one step past
//! it.

use super::{
    get_all_claims, get_issuer_placeholders, replace_issuer_placeholders, AudClaim,
    OpenIDJWTVerificationError, OpenIdCredential,
};
use crate::secs_to_nanos;
use base64::prelude::BASE64_URL_SAFE_NO_PAD;
use base64::Engine;
use candid::Deserialize;
use ic_stable_structures::Storable;
use identity_jose::jwk::{Jwk, JwkParamsRsa};
use identity_jose::jws::JwsAlgorithm::RS256;
use identity_jose::jws::{
    Decoder, JwsVerifierFn, SignatureVerificationError, SignatureVerificationErrorKind,
    VerificationInput,
};
use internet_identity_interface::internet_identity::types::MetadataEntryV2;
use rsa::{Pkcs1v15Sign, RsaPublicKey};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

// A JWT is only valid for a very small window, even if the JWT itself says it's valid for longer,
// we only need it right after it's being issued to create a JWT delegation with its own expiry.
// As the JWT is also used for registration, which may include longer user interaction,
// we are using 10 minutes to account for potential clock offsets as well as users.
const MAX_VALIDITY_WINDOW_SECONDS: u64 = 10 * 60; // Same as ingress expiry

// Maximum length of the email claim in the JWT, in practice we expect the identity provider to
// already validate it on their end for a sane maximum length. This is an additional sanity check.
const MAX_EMAIL_LENGTH: usize = 256;

// Maximum length of the name claim in the JWT, in practice we expect the identity provider to
// already validate it on their end for a sane maximum length. This is an additional sanity check.
const MAX_NAME_LENGTH: usize = 128;

// Maximum length of the iss claim in the JWT; a sanity bound.
const MAX_ISS_LENGTH: usize = 255;

// Maximum length of the sub claim in the JWT; a sanity bound.
const MAX_SUB_LENGTH: usize = 255;

#[derive(Deserialize)]
#[serde(untagged)]
enum EmailVerifiedClaim {
    Bool(bool),
    String(String),
}

// Depending on the OpenID provider, this claim can either be a boolean or string,
// so we need to support both formats when parsing the claims, but we will convert
// both to a string for storing it in the OpenIdCredential metadata.
impl EmailVerifiedClaim {
    fn into_string(self) -> String {
        match self {
            EmailVerifiedClaim::Bool(value) => {
                if value {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            EmailVerifiedClaim::String(value) => value,
        }
    }
}

#[derive(Deserialize)]
struct Claims {
    iss: String,
    sub: String,
    aud: AudClaim,
    nonce: String,
    exp: u64,
    iat: u64,
    // Optional metadata claims
    email: Option<String>,
    name: Option<String>,
    email_verified: Option<EmailVerifiedClaim>,
}

/// What the resolver determined about a provider, independent of the JWK
/// source. Carries the only inputs that differ between configured and SSO
/// verification; the pipeline itself is identical for both.
pub(super) struct Descriptor {
    /// Issuer to verify the JWT's `iss` against. May contain `{tid}`-style
    /// placeholders (the Microsoft configured provider); concrete issuers
    /// (SSO, Google, Apple) carry none, so placeholder expansion is a no-op.
    pub issuer: String,
    /// Expected JWT `aud`; also the canonical value stored on the credential.
    pub client_id: String,
    /// The discoverable SSO provider this credential comes from, or `None` for a
    /// configured direct provider (Google / Microsoft / Apple).
    pub sso: Option<SsoProvider>,
}

/// A discoverable SSO provider: the discovery domain and optional display name
/// written onto the built credential, plus the claim (if any) carrying its
/// cross-client-stable identifier.
pub(super) struct SsoProvider {
    pub domain: String,
    pub name: Option<String>,
    /// Claim holding the cross-client-stable id (e.g. Entra `oid`). `None` for a
    /// `sub` org, which keys identity on the token `sub` and needs no bridge.
    pub stable_identifier_claim: Option<String>,
}

/// Maximum size, in bytes, of a configured stable-identifier claim value.
const MAX_STABLE_IDENTIFIER_BYTES: usize = 255;

/// Extract a string claim from raw JWT claim bytes; `None` if absent or non-string.
fn extract_string_claim(claims_bytes: &[u8], claim: &str) -> Option<String> {
    serde_json::from_slice::<serde_json::Value>(claims_bytes)
        .ok()?
        .get(claim)?
        .as_str()
        .map(str::to_string)
}

/// Verify `jwt` against `descriptor` using `keys`, returning the credential on
/// success. This is the shared pipeline: it never reads a cache or registry —
/// the caller resolves the descriptor and supplies the already-read JWKs, so a
/// cold/pending JWK source is handled (as `Cached::Pending`) before reaching
/// here. A `keys` slice that is non-empty but lacks the JWT's `kid` is a hard
/// "certificate not found" error, distinct from "not fetched yet".
pub(super) fn verify_and_build(
    jwt: &str,
    descriptor: &Descriptor,
    keys: &[Jwk],
    salt: &[u8; 32],
) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
    // Decode JWT and decode claims
    let jwt_under_validation = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| {
            OpenIDJWTVerificationError::GenericError("Unable to decode JWT".to_string())
        })?;
    let claims: Claims = serde_json::from_slice(jwt_under_validation.claims()).map_err(|_| {
        OpenIDJWTVerificationError::GenericError(
            "Unable to decode claims or expected claims are missing".to_string(),
        )
    })?;

    // Compute the effective issuer. For a placeholder issuer (Microsoft's
    // `https://login.microsoftonline.com/{tid}/v2.0`) the `{tid}` is filled
    // from the JWT claims; a concrete issuer has no placeholders and passes
    // through unchanged, so SSO and Google/Apple share this code path.
    let issuer_placeholders = get_issuer_placeholders(&descriptor.issuer);
    let issuer_claims = get_all_claims(jwt_under_validation.claims(), issuer_placeholders);
    let effective_issuer = replace_issuer_placeholders(&descriptor.issuer, &issuer_claims);

    verify_claims(&effective_issuer, &descriptor.client_id, &claims, salt)?;

    // Pull the SSO stable-id claim before `verify` consumes `jwt_under_validation`;
    // its value is only used below, once verification has succeeded.
    let sso_stable_id = match &descriptor.sso {
        Some(SsoProvider {
            stable_identifier_claim: Some(claim),
            ..
        }) => extract_string_claim(jwt_under_validation.claims(), claim),
        _ => None,
    };

    // Verify JWT signature against the matching key.
    let kid = jwt_under_validation
        .kid()
        .ok_or(OpenIDJWTVerificationError::GenericError(
            "JWT is missing kid".to_string(),
        ))?;
    let cert = keys
        .iter()
        .find(|cert| cert.kid().is_some_and(|v| v == kid))
        .ok_or(OpenIDJWTVerificationError::GenericError(format!(
            "Certificate not found for {kid}"
        )))?;
    jwt_under_validation
        .verify(&JwsVerifierFn::from(verify_signature), cert)
        .map_err(|_| OpenIDJWTVerificationError::GenericError("Invalid signature".to_string()))?;

    // Destructure the verified claims. `aud` is intentionally dropped: it was
    // only weakly checked (`AudClaim::matches` accepts a single value or any
    // member of an array), so the canonical `descriptor.client_id` is stored on
    // the credential instead. `nonce` / `exp` / `iat` were consumed by
    // `verify_claims` and aren't persisted.
    let Claims {
        iss,
        sub,
        email,
        name,
        email_verified,
        aud: _,
        nonce: _,
        exp: _,
        iat: _,
    } = claims;

    // Assemble metadata
    let mut metadata: HashMap<String, MetadataEntryV2> = HashMap::new();
    if let Some(email) = email {
        metadata.insert("email".into(), MetadataEntryV2::String(email));
    }
    if let Some(email_verified) = email_verified {
        metadata.insert(
            "email_verified".into(),
            MetadataEntryV2::String(email_verified.into_string()),
        );
    }
    if let Some(name) = name {
        metadata.insert("name".into(), MetadataEntryV2::String(name));
    }
    // Store issuer-specific claims (e.g. Microsoft's `tid`) in the metadata.
    for (key, value) in issuer_claims {
        metadata.insert(key, MetadataEntryV2::String(value));
    }

    let (sso_domain, sso_name, stable_id) = match &descriptor.sso {
        None => (None, None, None),
        Some(sso) => {
            // A non-`sub` org's stable id (e.g. Entra `oid`) is part of the
            // credential the moment it's built, so both the delegation and the
            // add/register paths get it — and its length cap — from here.
            if sso_stable_id
                .as_ref()
                .is_some_and(|v| v.len() > MAX_STABLE_IDENTIFIER_BYTES)
            {
                return Err(OpenIDJWTVerificationError::GenericError(
                    "stable identifier claim too long".to_string(),
                ));
            }
            (Some(sso.domain.clone()), sso.name.clone(), sso_stable_id)
        }
    };

    Ok(OpenIdCredential {
        // Do NOT use the configured issuer template here: every (iss, sub) pair
        // should uniquely identify an account, and the Microsoft config issuer
        // is a placeholder string that doesn't identify which tenant a `sub`
        // belongs to. Store the concrete `iss` claim instead.
        iss,
        sub,
        // `aud` is verified against `client_id` above; store the canonical value.
        aud: descriptor.client_id.clone(),
        last_usage_timestamp: None,
        metadata,
        sso_domain,
        sso_name,
        stable_id,
    })
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
    // If the algorithm does not match, return an UnsupportedAlg error.
    // Additional algorithms can be implemented here if needed in the future.
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
    issuer: &str,
    client_id: &str,
    claims: &Claims,
    salt: &[u8; 32],
) -> Result<(), OpenIDJWTVerificationError> {
    // JWT `iat` / `exp` are second-granular. Compare validity in whole seconds
    // rather than converting the claims to nanoseconds: `secs_to_nanos`
    // multiplies by 1e9, so a malicious far-future `iat` / `exp` would overflow
    // it, and canister code must stay panic-free.
    let now_secs = time() / secs_to_nanos(1);
    let mut hasher = Sha256::new();
    hasher.update(salt);
    hasher.update(caller().to_bytes());
    let hash: [u8; 32] = hasher.finalize().into();
    let expected_nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

    if claims.iss != issuer {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid issuer: {}",
            claims.iss
        )));
    }
    if !claims.aud.matches(client_id) {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Invalid audience".to_string(),
        ));
    }
    if claims.nonce != expected_nonce {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "Invalid nonce: {}",
            claims.nonce
        )));
    }
    if now_secs > claims.exp {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    }
    // `checked_add` instead of `iat + window`: a malicious far-future `iat`
    // would otherwise overflow and panic. An `iat` that large is past any
    // sane validity window, so treat the overflow as expired.
    let Some(max_valid_until_secs) = claims.iat.checked_add(MAX_VALIDITY_WINDOW_SECONDS) else {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    };
    if now_secs > max_valid_until_secs {
        return Err(OpenIDJWTVerificationError::JWTExpired);
    }
    if now_secs < claims.iat {
        return Err(OpenIDJWTVerificationError::GenericError(
            "JWT is not valid yet".into(),
        ));
    }
    if claims.iss.len() > MAX_ISS_LENGTH {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Issuer too long".into(),
        ));
    }
    if claims.sub.len() > MAX_SUB_LENGTH {
        return Err(OpenIDJWTVerificationError::GenericError(
            "Subject too long".into(),
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
    if let Some(EmailVerifiedClaim::String(ref s)) = claims.email_verified {
        if s != "true" && s != "false" {
            return Err(OpenIDJWTVerificationError::GenericError(format!(
                "email_verified must be 'true' or 'false', got: {}",
                s
            )));
        }
    }

    Ok(())
}

#[cfg(not(test))]
fn caller() -> candid::Principal {
    ic_cdk::caller()
}

#[cfg(test)]
fn caller() -> candid::Principal {
    tests::TEST_CALLER.get()
}

#[cfg(not(test))]
fn time() -> u64 {
    ic_cdk::api::time()
}

#[cfg(test)]
fn time() -> u64 {
    tests::TEST_TIME.get()
}

#[cfg(test)]
mod tests {
    use super::*;
    use candid::Principal;
    use identity_jose::jwk::Jwk;
    use std::cell::Cell;

    // `VALID_JWT` was issued at `TEST_IAT_SECONDS` and expires at
    // `TEST_EXP_SECONDS` (one hour later), with its nonce bound to
    // `DEFAULT_TEST_PRINCIPAL` + `test_salt()`. The default test clock is pinned
    // to `iat` so the token verifies.
    const DEFAULT_TEST_PRINCIPAL: &str =
        "x4gp4-hxabd-5jt4d-wc6uw-qk4qo-5am4u-mncv3-wz3rt-usgjp-od3c2-oae";
    const TEST_IAT_SECONDS: u64 = 1_736_794_102;
    const TEST_EXP_SECONDS: u64 = 1_736_797_702;

    thread_local! {
        pub(super) static TEST_CALLER: Cell<Principal> =
            Cell::new(Principal::from_text(DEFAULT_TEST_PRINCIPAL).unwrap());
        pub(super) static TEST_TIME: Cell<u64> = const { Cell::new(secs_to_nanos(TEST_IAT_SECONDS)) };
    }

    // The clock and caller are thread-locals that some tests mutate. Reset them
    // to the defaults at the start of every test so the test runner reusing a
    // thread can't leak a mutated value into the next test.
    fn reset_test_env() {
        TEST_CALLER.set(Principal::from_text(DEFAULT_TEST_PRINCIPAL).unwrap());
        TEST_TIME.set(secs_to_nanos(TEST_IAT_SECONDS));
    }

    const TEST_AUD: &str =
        "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com";

    fn test_certs() -> Vec<Jwk> {
        #[derive(serde::Deserialize)]
        struct Certs {
            keys: Vec<Jwk>,
        }
        serde_json::from_str::<Certs>(r#"{"keys":[{"n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use":"sig","kty":"RSA","alg":"RS256","kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847","e": "AQAB"}]}"#)
            .unwrap()
            .keys
    }

    // Real Google-signed JWT (matches `test_certs`'s key), expired in real time
    // but valid against `TEST_TIME`, with its nonce bound to `TEST_CALLER` +
    // `test_salt()`. Issuer `https://accounts.google.com`, aud `TEST_AUD`.
    const VALID_JWT: &str = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";

    fn test_salt() -> [u8; 32] {
        [
            143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83,
            81, 139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
        ]
    }

    fn descriptor() -> Descriptor {
        Descriptor {
            issuer: "https://accounts.google.com".to_string(),
            client_id: TEST_AUD.to_string(),
            sso: None,
        }
    }

    #[test]
    fn should_verify_and_build_credential() {
        reset_test_env();
        let credential = verify_and_build(VALID_JWT, &descriptor(), &test_certs(), &test_salt())
            .expect("expected verification to succeed");
        assert_eq!(credential.iss, "https://accounts.google.com");
        assert_eq!(credential.aud, TEST_AUD);
        assert_eq!(credential.sub, "115160716338813006902");
        assert_eq!(
            credential.get_email(),
            Some("thomas.gladdines@dfinity.org".to_string())
        );
        assert_eq!(credential.sso_domain, None);
        assert_eq!(credential.sso_name, None);
    }

    #[test]
    fn should_stamp_sso_fields() {
        reset_test_env();
        // Same shared pipeline, only the descriptor's stamp differs: an SSO
        // descriptor writes `sso_domain` / `sso_name` onto the credential.
        let descriptor = Descriptor {
            issuer: "https://accounts.google.com".to_string(),
            client_id: TEST_AUD.to_string(),
            sso: Some(SsoProvider {
                domain: "example.org".to_string(),
                name: Some("Example".to_string()),
                stable_identifier_claim: None,
            }),
        };
        let credential = verify_and_build(VALID_JWT, &descriptor, &test_certs(), &test_salt())
            .expect("expected verification to succeed");
        assert_eq!(credential.sso_domain, Some("example.org".to_string()));
        assert_eq!(credential.sso_name, Some("Example".to_string()));
    }

    #[test]
    fn should_reject_certificate_not_found() {
        reset_test_env();
        // A non-empty key set that lacks the JWT's `kid` is a hard error,
        // distinct from a pending (not-yet-fetched) JWK source.
        let mut wrong_kid = test_certs();
        wrong_kid[0] = serde_json::from_str(r#"{"n":"jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use":"sig","kty":"RSA","alg":"RS256","kid":"some-other-kid","e":"AQAB"}"#).unwrap();
        let err = verify_and_build(VALID_JWT, &descriptor(), &wrong_kid, &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("Certificate not found"))
        );
    }

    #[test]
    fn should_reject_invalid_encoding() {
        reset_test_env();
        assert_eq!(
            verify_and_build("invalid-jwt", &descriptor(), &test_certs(), &test_salt()),
            Err(OpenIDJWTVerificationError::GenericError(
                "Unable to decode JWT".to_string()
            ))
        );
    }

    #[test]
    fn should_reject_invalid_signature() {
        reset_test_env();
        // Flip the first character of the signature segment: the header and
        // claims still decode and pass every claim check, but the RSA signature
        // no longer verifies against the key.
        let (signed, signature) = VALID_JWT.rsplit_once('.').unwrap();
        let mut chars: Vec<char> = signature.chars().collect();
        chars[0] = if chars[0] == 'A' { 'B' } else { 'A' };
        let tampered = format!("{signed}.{}", chars.into_iter().collect::<String>());

        let err =
            verify_and_build(&tampered, &descriptor(), &test_certs(), &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("Invalid signature"))
        );
    }

    #[test]
    fn should_reject_invalid_issuer() {
        reset_test_env();
        let descriptor = Descriptor {
            issuer: "https://accounts.evil.example".to_string(),
            client_id: TEST_AUD.to_string(),
            sso: None,
        };
        let err =
            verify_and_build(VALID_JWT, &descriptor, &test_certs(), &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("Invalid issuer"))
        );
    }

    #[test]
    fn should_reject_invalid_audience() {
        reset_test_env();
        let descriptor = Descriptor {
            issuer: "https://accounts.google.com".to_string(),
            client_id: "wrong-client-id".to_string(),
            sso: None,
        };
        let err =
            verify_and_build(VALID_JWT, &descriptor, &test_certs(), &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("Invalid audience"))
        );
    }

    #[test]
    fn should_reject_invalid_nonce() {
        reset_test_env();
        // The nonce is a hash of the salt + caller. A different caller produces
        // a different expected nonce, so the JWT's nonce — bound to the default
        // caller — no longer matches.
        TEST_CALLER.set(Principal::anonymous());
        let err =
            verify_and_build(VALID_JWT, &descriptor(), &test_certs(), &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("Invalid nonce"))
        );
    }

    #[test]
    fn should_reject_expired_jwt() {
        reset_test_env();
        // One second past the token's `exp`.
        TEST_TIME.set(secs_to_nanos(TEST_EXP_SECONDS + 1));
        let err =
            verify_and_build(VALID_JWT, &descriptor(), &test_certs(), &test_salt()).unwrap_err();
        assert_eq!(err, OpenIDJWTVerificationError::JWTExpired);
    }

    #[test]
    fn should_reject_jwt_outside_validity_window() {
        reset_test_env();
        // Still before `exp` (iat 1736794102 + 601 < exp 1736797702), but past
        // `iat + MAX_VALIDITY_WINDOW_SECONDS`: a JWT the provider still considers
        // valid is rejected once it leaves our own (much shorter) window.
        TEST_TIME.set(secs_to_nanos(
            TEST_IAT_SECONDS + MAX_VALIDITY_WINDOW_SECONDS + 1,
        ));
        let err =
            verify_and_build(VALID_JWT, &descriptor(), &test_certs(), &test_salt()).unwrap_err();
        assert_eq!(err, OpenIDJWTVerificationError::JWTExpired);
    }

    #[test]
    fn should_reject_jwt_issued_in_the_future() {
        reset_test_env();
        // One second before the token's `iat`.
        TEST_TIME.set(secs_to_nanos(TEST_IAT_SECONDS - 1));
        let err =
            verify_and_build(VALID_JWT, &descriptor(), &test_certs(), &test_salt()).unwrap_err();
        assert!(
            matches!(err, OpenIDJWTVerificationError::GenericError(msg) if msg.contains("not valid yet"))
        );
    }

    #[test]
    fn should_not_overflow_on_far_future_iat() {
        reset_test_env();
        // A malicious `iat` near `u64::MAX` previously overflowed
        // `iat + MAX_VALIDITY_WINDOW_SECONDS` (and `secs_to_nanos`). Drive
        // `verify_claims` with a matching nonce so it gets past the earlier
        // checks and reaches the validity-window comparison, and assert it
        // rejects the JWT instead of panicking on the overflow.
        let mut hasher = Sha256::new();
        hasher.update(test_salt());
        hasher.update(caller().to_bytes());
        let hash: [u8; 32] = hasher.finalize().into();
        let nonce = BASE64_URL_SAFE_NO_PAD.encode(hash);

        let result = verify_claims(
            "https://accounts.google.com",
            TEST_AUD,
            &Claims {
                iss: "https://accounts.google.com".to_string(),
                sub: "sub".to_string(),
                aud: AudClaim::Single(TEST_AUD.to_string()),
                nonce,
                exp: u64::MAX,
                iat: u64::MAX,
                email: None,
                name: None,
                email_verified: None,
            },
            &test_salt(),
        );
        // `iat + window` overflows, which the verifier maps to `JWTExpired`
        // rather than panicking.
        assert_eq!(result, Err(OpenIDJWTVerificationError::JWTExpired));
    }

    // Nonce that matches `test_salt()` + `caller()`, so a crafted `Claims` gets
    // past the earlier checks and reaches the seed-field length caps.
    fn matching_nonce() -> String {
        let mut hasher = Sha256::new();
        hasher.update(test_salt());
        hasher.update(caller().to_bytes());
        let hash: [u8; 32] = hasher.finalize().into();
        BASE64_URL_SAFE_NO_PAD.encode(hash)
    }

    #[test]
    fn should_reject_over_long_sub() {
        reset_test_env();
        let result = verify_claims(
            "https://accounts.google.com",
            TEST_AUD,
            &Claims {
                iss: "https://accounts.google.com".to_string(),
                sub: "a".repeat(MAX_SUB_LENGTH + 1),
                aud: AudClaim::Single(TEST_AUD.to_string()),
                nonce: matching_nonce(),
                exp: TEST_EXP_SECONDS,
                iat: TEST_IAT_SECONDS,
                email: None,
                name: None,
                email_verified: None,
            },
            &test_salt(),
        );
        assert_eq!(
            result,
            Err(OpenIDJWTVerificationError::GenericError(
                "Subject too long".to_string()
            ))
        );
    }

    #[test]
    fn should_reject_over_long_iss() {
        reset_test_env();
        let long_iss = format!("https://{}", "a".repeat(MAX_ISS_LENGTH));
        let result = verify_claims(
            &long_iss,
            TEST_AUD,
            &Claims {
                iss: long_iss.clone(),
                sub: "sub".to_string(),
                aud: AudClaim::Single(TEST_AUD.to_string()),
                nonce: matching_nonce(),
                exp: TEST_EXP_SECONDS,
                iat: TEST_IAT_SECONDS,
                email: None,
                name: None,
                email_verified: None,
            },
            &test_salt(),
        );
        assert_eq!(
            result,
            Err(OpenIDJWTVerificationError::GenericError(
                "Issuer too long".to_string()
            ))
        );
    }
}
