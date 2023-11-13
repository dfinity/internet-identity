use candid::Principal;
use canister_sig_util::{extract_raw_canister_sig_pk_from_der, CanisterSigPublicKey};
use ic_certified_map::Hash;
use ic_crypto_standalone_sig_verifier::verify_canister_sig;
use ic_types::crypto::threshold_sig::IcRootOfTrust;
use identity_core::convert::FromJson;
use identity_credential::credential::Subject;
use identity_credential::error::Error as JwtVcError;
use identity_credential::validator::JwtValidationError;
use identity_jose::jwk::{Jwk, JwkParams, JwkParamsOct, JwkType};
use identity_jose::jws::{
    CompactJwsEncoder, Decoder, JwsAlgorithm, JwsHeader, SignatureVerificationError,
    SignatureVerificationErrorKind,
};
use identity_jose::jwt::JwtClaims;
use identity_jose::jwu::{decode_b64, encode_b64};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::ops::{Add, Deref, DerefMut};

pub const II_CREDENTIAL_URL_PREFIX: &str =
    "https://internetcomputer.org/credential/internet-identity/";
pub const II_ISSUER_URL: &str = "https://internetcomputer.org/issuers/internet-identity";
pub const VC_SIGNING_INPUT_DOMAIN: &[u8; 26] = b"iccs_verifiable_credential";

/// A pair of identities, that denote the same user.
/// Used in attribute sharing flow to maintain II's unlinkability of identities.
pub struct AliasTuple {
    /// A temporary identity, used in attribute sharing flow.
    pub id_alias: Principal,
    /// An identity under which a user is known to a dapp.
    pub id_dapp: Principal,
}

#[derive(Debug)]
pub enum CredentialVerificationError {
    InvalidJws(SignatureVerificationError),
    InvalidClaims(JwtValidationError),
}

/// Returns the effective bytes that will be signed when computing a canister signature for
/// the given JWT-credential, verifiable via the specified public key.
pub fn vc_signing_input(
    credential_jwt: &str,
    canister_sig_pk: &CanisterSigPublicKey,
) -> Result<Vec<u8>, String> {
    let encoder = jws_encoder(credential_jwt, canister_sig_pk)?;
    Ok(encoder.signing_input().to_vec())
}

/// Computes and returns SHA-256 hash of the given `signing_input` prefixed with
///      `length(VC_SIGNING_INPUT_DOMAIN) · VC_SIGNING_INPUT_DOMAIN`
/// (for domain separation), where `length(a)` is the length of byte-array `a`,
/// and `·` denotes concatenation of bytes.
pub fn vc_signing_input_hash(signing_input: &[u8]) -> Hash {
    let mut hasher = Sha256::new();
    let buf = [VC_SIGNING_INPUT_DOMAIN.len() as u8];
    hasher.update(buf);
    hasher.update(VC_SIGNING_INPUT_DOMAIN);
    hasher.update(signing_input);
    hasher.finalize().into()
}

/// Constructs and returns a JWS (a signed JWT) from the given components.
pub fn vc_jwt_to_jws(
    credential_jwt: &str,
    canister_sig_pk: &CanisterSigPublicKey,
    sig: &[u8],
) -> Result<String, String> {
    let encoder = jws_encoder(credential_jwt, canister_sig_pk)?;
    Ok(encoder.into_jws(sig))
}

/// Returns a DID for the given `principal`.
pub fn did_for_principal(principal: Principal) -> String {
    let prefix = String::from("did:icp:");
    prefix.add(&principal.to_string())
}

/// Verifies the given JWS-credential as an id_alias-VC for the specified alias tuple.
/// Performs both the cryptographic verification of the credential, and the semantic
/// validation of the id_alias-claims.
pub fn verify_id_alias_credential_jws(
    credential_jws: &str,
    alias_tuple: &AliasTuple,
    signing_canister_id: &Principal,
    root_pk_raw: &[u8],
) -> Result<(), CredentialVerificationError> {
    let claims =
        verify_credential_jws_with_canister_id(credential_jws, signing_canister_id, root_pk_raw)
            .map_err(CredentialVerificationError::InvalidJws)?;
    validate_id_alias_claims(claims, alias_tuple)
        .map_err(CredentialVerificationError::InvalidClaims)
}

/// Verifies the specified JWS credential cryptographically and checks that the signature was
/// created by the provided canister.
/// DOES NOT perform semantic validation of the claims in the credential.
pub fn verify_credential_jws_with_canister_id(
    credential_jws: &str,
    signing_canister_id: &Principal,
    root_pk_raw: &[u8],
) -> Result<JwtClaims<Value>, SignatureVerificationError> {
    ///// Decode JWS.
    let decoder: Decoder = Decoder::new();
    let jws = decoder
        .decode_compact_serialization(credential_jws.as_ref(), None)
        .map_err(|e| invalid_signature_err(&format!("credential JWS parsing error: {}", e)))?;
    let signature = jws.decoded_signature();
    let message = signing_input_with_prefix(jws.signing_input());
    let jws_header = jws
        .protected_header()
        .ok_or(invalid_signature_err("missing JWS header"))?;
    let canister_sig_pk_raw = get_canister_sig_pk_raw(jws_header)?;
    let canister_sig_pk = CanisterSigPublicKey::try_from_raw(canister_sig_pk_raw.as_slice())
        .map_err(|e| key_decoding_err(&format!("invalid canister sig public key: {}", e)))?;

    if signing_canister_id != &canister_sig_pk.canister_id {
        return Err(invalid_signature_err(&format!(
            "canister sig canister id does not match provided canister id: expected {}, got {}",
            signing_canister_id.to_text(),
            canister_sig_pk.canister_id.to_text()
        )));
    }

    let root_pk_bytes: [u8; 96] = root_pk_raw
        .try_into()
        .map_err(|e| key_decoding_err(&format!("invalid root public key: {}", e)))?;
    let root_pk = IcRootOfTrust::from(root_pk_bytes);
    verify_canister_sig(&message, signature, canister_sig_pk_raw.as_slice(), root_pk)
        .map_err(|e| invalid_signature_err(&format!("signature verification error: {}", e)))?;

    let claims: JwtClaims<Value> = serde_json::from_slice(jws.claims())
        .map_err(|e| invalid_signature_err(&format!("failed parsing JSON JWT claims: {}", e)))?;

    Ok(claims)
}

/// Returns the given `signing_input` prefixed with
///      length(VC_SIGNING_INPUT_DOMAIN) || VC_SIGNING_INPUT_DOMAIN
/// (for domain separation).
fn signing_input_with_prefix(signing_input: &[u8]) -> Vec<u8> {
    let mut result = Vec::from([VC_SIGNING_INPUT_DOMAIN.len() as u8]);
    result.extend_from_slice(VC_SIGNING_INPUT_DOMAIN);
    result.extend_from_slice(signing_input);
    result
}

/// Validates that the given claims are consistent with id_alias-credential
/// for the given alias tuple.
fn validate_id_alias_claims(
    claims: JwtClaims<Value>,
    alias_tuple: &AliasTuple,
) -> Result<(), JwtValidationError> {
    validate_claim("sub", did_for_principal(alias_tuple.id_dapp), claims.sub())?;
    validate_claim("iss", II_ISSUER_URL, claims.iss())?;
    let vc = claims
        .vc()
        .ok_or(inconsistent_jwt_claims("missing vc in id_alias JWT claims"))?;
    let subject_value = vc.get("credentialSubject").ok_or(inconsistent_jwt_claims(
        "missing credentialSubject in id_alias JWT vc",
    ))?;
    validate_credential_subject(subject_value, alias_tuple)?;
    Ok(())
}

fn validate_credential_subject(
    subject_value: &Value,
    alias_tuple: &AliasTuple,
) -> Result<(), JwtValidationError> {
    let subject = Subject::from_json_value(subject_value.clone())
        .map_err(|_| inconsistent_jwt_claims("missing credentialSubject in id_alias JWT vc"))?;
    if subject.properties["has_id_alias"] != did_for_principal(alias_tuple.id_alias) {
        return Err(inconsistent_jwt_claims("wrong id_alias"));
    }
    Ok(())
}

fn validate_claim<T: std::cmp::PartialEq<S> + std::fmt::Display, S: std::fmt::Display>(
    label: &str,
    expected: T,
    actual: Option<S>,
) -> Result<(), JwtValidationError> {
    if let Some(actual) = actual {
        if expected == actual {
            Ok(())
        } else {
            ic_cdk::println!(
                "inconsistent claim [{}] in id_alias VC::  expected: {}, actual: {}",
                label,
                expected,
                actual
            );
            Err(inconsistent_jwt_claims("inconsistent claim in id_alias VC"))
        }
    } else {
        ic_cdk::println!("missing claim [{}] in id_alias VC", label);
        Err(inconsistent_jwt_claims("missing claim in id_alias VC"))
    }
}

// Per https://datatracker.ietf.org/doc/html/rfc7518#section-6.4,
// JwkParamsOct are for symmetric keys or another key whose value is a single octet sequence.
fn canister_sig_pk_jwk(canister_sig_pk_der: &[u8]) -> Result<Jwk, String> {
    let mut cspk_jwk = Jwk::new(JwkType::Oct);
    cspk_jwk.set_alg("IcCs");
    cspk_jwk
        .set_params(JwkParams::Oct(JwkParamsOct {
            k: encode_b64(canister_sig_pk_der),
        }))
        .map_err(|e| format!("internal: failed creating JWK: {:?}", e))?;
    Ok(cspk_jwk)
}

fn jws_encoder<'a>(
    credential_jwt: &'a str,
    canister_sig_pk: &CanisterSigPublicKey,
) -> Result<CompactJwsEncoder<'a>, String> {
    let mut header: JwsHeader = JwsHeader::new();
    header.set_alg(JwsAlgorithm::IcCs);
    let kid = did_for_principal(canister_sig_pk.canister_id);
    let jwk = canister_sig_pk_jwk(&canister_sig_pk.to_der())?;
    header.set_kid(kid);
    header.deref_mut().set_jwk(jwk);

    let encoder: CompactJwsEncoder = CompactJwsEncoder::new(credential_jwt.as_ref(), &header)
        .map_err(|e| format!("internal: failed creating JWS encoder: {:?}", e))?;
    Ok(encoder)
}

fn unsupported_alg_err(custom_message: &str) -> SignatureVerificationError {
    let err: SignatureVerificationError = SignatureVerificationErrorKind::UnsupportedAlg.into();
    err.with_custom_message(custom_message.to_string())
}

fn key_decoding_err(custom_message: &str) -> SignatureVerificationError {
    let err: SignatureVerificationError = SignatureVerificationErrorKind::KeyDecodingFailure.into();
    err.with_custom_message(custom_message.to_string())
}

fn invalid_signature_err(custom_message: &str) -> SignatureVerificationError {
    let err: SignatureVerificationError = SignatureVerificationErrorKind::InvalidSignature.into();
    err.with_custom_message(custom_message.to_string())
}

fn inconsistent_jwt_claims(custom_message: &'static str) -> JwtValidationError {
    JwtValidationError::CredentialStructure(JwtVcError::InconsistentCredentialJwtClaims(
        custom_message,
    ))
}

// Extracts and returns raw canister sig public key (without DER-prefix) from the given header.
fn get_canister_sig_pk_raw(jws_header: &JwsHeader) -> Result<Vec<u8>, SignatureVerificationError> {
    let jwk = jws_header
        .deref()
        .jwk()
        .ok_or(key_decoding_err("missing JWK in JWS header"))?;
    if jwk.alg() != Some("IcCs") {
        return Err(unsupported_alg_err("expected IcCs"));
    }
    // Per https://datatracker.ietf.org/doc/html/rfc7518#section-6.4,
    // JwkParamsOct are for symmetric keys or another key whose value is a single octet sequence.
    if jwk.kty() != JwkType::Oct {
        return Err(unsupported_alg_err("expected JWK of type oct"));
    }
    let jwk_params = jwk
        .try_oct_params()
        .map_err(|_| key_decoding_err("missing JWK oct params"))?;
    let pk_der = decode_b64(jwk_params.k.as_bytes())
        .map_err(|_| key_decoding_err("invalid base64url encoding"))?;
    let pk_raw = extract_raw_canister_sig_pk_from_der(pk_der.as_slice())
        .map_err(|e| key_decoding_err(&e.to_string()))?;
    Ok(pk_raw)
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use canister_sig_util::{extract_raw_root_pk_from_der, IC_ROOT_PK_DER_PREFIX};

    const TEST_IC_ROOT_PK_B64URL: &str = "MIGCMB0GDSsGAQQBgtx8BQMBAgEGDCsGAQQBgtx8BQMCAQNhAK32VjilMFayIiyRuyRXsCdLypUZilrL2t_n_XIXjwab3qjZnpR52Ah6Job8gb88SxH-J1Vw1IHxaY951Giv4OV6zB4pj4tpeY2nqJG77Blwk-xfR1kJkj1Iv-1oQ9vtHw";
    const ALIAS_PRINCIPAL: &str = "s33qc-ctnp5-ubyz4-kubqo-p2tem-he4ls-6j23j-hwwba-37zbl-t2lv3-pae";
    const DAPP_PRINCIPAL: &str = "cpehq-54hef-odjjt-bockl-3ldtg-jqle4-ysi5r-6bfah-v6lsa-xprdv-pqe";
    const ID_ALIAS_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCamxUYzNvSzVRVU9SbUt0T3YyVXBhMnhlQW5vNEJ4RlFFYmY1VWRUSTZlYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbnRpdHkiLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaW50ZXJuZXRjb21wdXRlci5vcmcvY3JlZGVudGlhbC9pbnRlcm5ldC1pZGVudGl0eS8xNjIwMzI4NjMwMDAwMDAwMDAwIiwic3ViIjoiZGlkOmljcDpjcGVocS01NGhlZi1vZGpqdC1ib2NrbC0zbGR0Zy1qcWxlNC15c2k1ci02YmZhaC12NmxzYS14cHJkdi1wcWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIkludGVybmV0SWRlbnRpdHlJZEFsaWFzIl0sImNyZWRlbnRpYWxTdWJqZWN0Ijp7Imhhc19pZF9hbGlhcyI6ImRpZDppY3A6czMzcWMtY3RucDUtdWJ5ejQta3VicW8tcDJ0ZW0taGU0bHMtNmoyM2otaHd3YmEtMzd6YmwtdDJsdjMtcGFlIn19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIMlBo1U8rvfGFEAvZoEFZX0uFlOGZLwgNjaBiyazTjUsggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIBfmGXVF1WCWPapsKI5MoFLJ55x11hQqSb_sRnrp5hFVggRYIBNvlU5ah4f5OsbVrHPPSsAhJo91Mf_fFkAE813rttVaggRYIEfscybxhCV3H9WVS6yOWyrSfnoAvrk5mr3vaKZOwOZiggRYID_qjkmyX1ydMvjuG7MS1E4grrzurjpGjbvYR7-YHMXwgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwogHXOX8bnQxCHy8TOkAG2Xak_qb2Yx22j5c9R5WC-8ResKLfeGgkSWbadE92xqRRZHRyZWWDAYIEWCB1hhWALXUuzFwXrojqFkaSB6Yejid7LwgvbIj61MjIN4MCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBggRYIOxGloHVCCWojZGGGusUYkg0Q-v_podIYMUM-jvDBF62gwJYIP28vIabsWSa5kTsn1ypw6vHamcQnTcYFNsiRlZMNcbAggNA";
    const ID_ALIAS_CREDENTIAL_JWS_NO_JWK: &str = "eyJraWQiOiJkaWQ6aWM6aWktY2FuaXN0ZXIiLCJhbGciOiJJY0NzIn0.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbml0eSIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9pbnRlcm5ldGNvbXB1dGVyLm9yZy9jcmVkZW50aWFsL2ludGVybmV0LWlkZW5pdHkiLCJzdWIiOiJkaWQ6d2ViOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOndlYjpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIG3uU_jutBtXB-of0uEA3RkCrcunK6D8QFPtX-gDSwDeggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIMULjwe1N6XomH10SEyc2r_uc7mGf1aSadeDaid9cUrkggRYIDw__VW2PgWMFp6mK-GmPG-7Fc90q58oK_wjcJ3IrkToggRYIAQTcQAtnxsa93zbfZEZV0f28OhiXL5Wp1OAyDHNI_x4ggRYINkQ8P9zGUvsVi3XbQ2bs6V_3kAiN8UNM6yPgeXfmArEgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwqHrYoUsNvSEaSShbW8barx0_ODXD5ZBEl9nKOdkNy_fBmGErE_C7ILbC91_fyZ7CZHRyZWWDAYIEWCB223o-sI97tc3LwJL3LRxQ4If6v_IvfC1fwIGYYQ9vroMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBgwJYIHszMLDS2VadioIaHajRY5iJzroqMs63lVrs_Uj42j0sggNAggRYICm0w_XxGEw4fDPoYcojCILEi0qdH4-4Zw7klzdaPNOC";
    const TEST_SIGNING_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const TEST_SEED: [u8; 32] = [
        142, 84, 220, 222, 130, 185, 65, 67, 145, 152, 171, 78, 191, 101, 41, 107, 108, 94, 2, 122,
        56, 7, 17, 80, 17, 183, 249, 81, 212, 200, 233, 231,
    ];
    const TEST_CREDENTIAL_JWT: &str = r#"{"iss":"https://employment.info/","nbf":1620328630,"jti":"https://employment.info/credentials/42","sub":"did:icp:igfpm-3fhrp-syqme-4i4xk-o4pgd-5xdh4-fbbgw-jnxm5-bvou4-ljt52-kqe","vc":{"@context":"https://www.w3.org/2018/credentials/v1","type":["VerifiableCredential","VerifiedEmployee"],"credentialSubject":{"employee_of":{"employerId":"did:web:dfinity.org","employerName":"DFINITY Foundation"}}}}"#;

    fn test_ic_root_pk_raw() -> Vec<u8> {
        let pk_der = decode_b64(TEST_IC_ROOT_PK_B64URL).expect("failure decoding canister pk");
        extract_raw_root_pk_from_der(pk_der.as_slice())
            .expect("failure extracting root pk from DER")
    }

    fn test_canister_sig_pk() -> CanisterSigPublicKey {
        CanisterSigPublicKey::new(
            Principal::from_text(TEST_SIGNING_CANISTER_ID).expect("wrong principal"),
            TEST_SEED.to_vec(),
        )
    }

    fn alias_principal() -> Principal {
        Principal::from_text(ALIAS_PRINCIPAL).expect("wrong principal")
    }

    fn dapp_principal() -> Principal {
        Principal::from_text(DAPP_PRINCIPAL).expect("wrong principal")
    }

    fn claims_from_jws(credential_jws: &str) -> JwtClaims<Value> {
        let decoder: Decoder = Decoder::new();
        let jws = decoder
            .decode_compact_serialization(credential_jws.as_ref(), None)
            .expect("failed JWS parsing");
        let claims: JwtClaims<Value> =
            serde_json::from_slice(jws.claims()).expect("failed parsing JSON JWT claims");
        claims
    }

    #[test]
    fn should_compute_domain_separated_signing_input_hash() {
        let signing_input = b"some bytes to sign";
        let signing_input_with_prefix = signing_input_with_prefix(signing_input.as_slice());
        assert_eq!(26, signing_input_with_prefix[0]);
        assert_eq!(
            b"iccs_verifiable_credential".as_slice(),
            signing_input_with_prefix[1..27].to_vec().as_slice()
        );
        let util_hash = vc_signing_input_hash(signing_input);
        let mut hasher = Sha256::new();
        hasher.update(signing_input_with_prefix);
        let manual_hash: Hash = hasher.finalize().into();
        assert_eq!(util_hash, manual_hash);
    }

    #[test]
    fn should_construct_correct_jws() {
        let canister_id = Principal::from_text(TEST_SIGNING_CANISTER_ID).expect("wrong principal");
        let canister_sig_pk = CanisterSigPublicKey::new(canister_id, TEST_SEED.to_vec());
        let dummy_sig: &str = "some signature";
        let credential_jwt = String::from_utf8(TEST_CREDENTIAL_JWT.into()).expect("wrong JWT");
        let credential_jws = vc_jwt_to_jws(&credential_jwt, &canister_sig_pk, dummy_sig.as_bytes())
            .expect("failed constructing JWS");
        let decoder: Decoder = Decoder::new();
        let jws = decoder
            .decode_compact_serialization(credential_jws.as_ref(), None)
            .expect("Failed parsing constructed JWS");
        assert_eq!(dummy_sig.as_bytes(), jws.decoded_signature());
        let jws_header = jws.protected_header().expect("JWS without header");
        let canister_sig_pk_from_jws =
            get_canister_sig_pk_raw(jws_header).expect("JWS header without pk");
        let canister_sig_pk_raw =
            extract_raw_canister_sig_pk_from_der(canister_sig_pk.to_der().as_slice())
                .expect("wrong canister sig pk");
        assert_eq!(canister_sig_pk_from_jws, canister_sig_pk_raw);
        assert_eq!(jws.claims(), TEST_CREDENTIAL_JWT.as_bytes());
    }

    #[test]
    fn should_compute_icp_did() {
        let principal = dapp_principal();
        let did = did_for_principal(principal);
        assert!(did.starts_with("did:icp:"));
        assert!(did.ends_with(&principal.to_string()));
        assert_eq!(did.len(), "did:icp:".len() + principal.to_string().len());
    }

    #[test]
    fn should_validate_id_alias_claims() {
        validate_id_alias_claims(
            claims_from_jws(ID_ALIAS_CREDENTIAL_JWS),
            &AliasTuple {
                id_alias: alias_principal(),
                id_dapp: dapp_principal(),
            },
        )
        .expect("Failed validating id_alias claims");
    }

    #[test]
    fn should_fail_validate_id_alias_claims_if_wrong_id_alias() {
        let result = validate_id_alias_claims(
            claims_from_jws(ID_ALIAS_CREDENTIAL_JWS),
            &AliasTuple {
                id_alias: dapp_principal(),
                id_dapp: dapp_principal(),
            },
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("wrong id_alias"));
    }

    #[test]
    fn should_fail_validate_id_alias_claims_if_wrong_id_dapp() {
        let result = validate_id_alias_claims(
            claims_from_jws(ID_ALIAS_CREDENTIAL_JWS),
            &AliasTuple {
                id_alias: dapp_principal(),
                id_dapp: dapp_principal(),
            },
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("wrong id_alias"));
    }

    #[test]
    fn should_verify_credential_jws() {
        verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS,
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
        )
        .expect("JWS verification failed");
    }

    #[test]
    fn should_fail_verify_credential_jws_without_canister_pk() {
        let result = verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS_NO_JWK,
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if e.to_string().contains("missing JWK in JWS header"));
    }

    #[test]
    fn should_fail_verify_credential_jws_with_wrong_canister_sig_pk() {
        let wrong_canister_sig_pk = CanisterSigPublicKey::new(alias_principal(), vec![1, 2, 3]);
        let result = verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS,
            &wrong_canister_sig_pk.canister_id,
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if e.to_string().contains("canister sig canister id does not match provided canister id"));
    }

    #[test]
    fn should_fail_verify_credential_jws_with_wrong_root_pk() {
        let mut ic_root_pk = test_ic_root_pk_raw();
        ic_root_pk[IC_ROOT_PK_DER_PREFIX.len()] += 1; // change the root pk value
        let result = verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS,
            &test_canister_sig_pk().canister_id,
            &ic_root_pk,
        );
        assert_matches!(result, Err(e) if  { let err_msg = e.to_string();
            err_msg.contains("invalid signature") &&
            err_msg.contains("Malformed ThresBls12_381 public key") });
    }

    #[test]
    fn should_verify_id_alias_credential_jws() {
        verify_id_alias_credential_jws(
            ID_ALIAS_CREDENTIAL_JWS,
            &AliasTuple {
                id_alias: alias_principal(),
                id_dapp: dapp_principal(),
            },
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
        )
        .expect("JWS verification failed");
    }
}
