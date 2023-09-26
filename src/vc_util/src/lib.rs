use candid::Principal;
use canister_sig_util::{verify_root_signature, CanisterSig, CanisterSigPublicKey};
use ic_certification::{Certificate, LookupResult};
use ic_certified_map::Hash;
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
use sha2::{Digest, Sha256};
use std::ops::{Add, Deref, DerefMut};

use serde_json::Value;

pub const II_CREDENTIAL_URL_PREFIX: &str =
    "https://internetcomputer.org/credential/internet-identity/";
pub const II_ISSUER_URL: &str = "https://internetcomputer.org/issuers/internet-identity";

pub struct AliasTuple {
    pub id_alias: Principal,
    pub id_dapp: Principal,
}

#[derive(Debug)]
pub enum CredentialVerificationError {
    InvalidJws(SignatureVerificationError),
    InvalidClaims(JwtValidationError),
}

pub fn vc_signing_input(
    credential_jwt: &str,
    canister_sig_pk_der: &[u8],
    canister_id: Principal,
) -> Vec<u8> {
    let encoder = jws_encoder(credential_jwt, canister_sig_pk_der, canister_id);
    encoder.signing_input().to_vec()
}

pub fn vc_jwt_to_jws(
    credential_jwt: &str,
    canister_sig_pk_der: &[u8],
    sig: &[u8],
    canister_id: Principal,
) -> String {
    let encoder = jws_encoder(credential_jwt, canister_sig_pk_der, canister_id);
    encoder.into_jws(sig)
}

pub fn vc_signing_input_hash(signing_input: &[u8]) -> Hash {
    let sep = b"iccs_verifiable_credential";
    let mut hasher = Sha256::new();
    let buf = [sep.len() as u8];
    hasher.update(buf);
    hasher.update(sep);
    hasher.update(signing_input);
    hasher.finalize().into()
}

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
) -> Result<(), CredentialVerificationError> {
    let claims =
        verify_credential_jws(credential_jws).map_err(CredentialVerificationError::InvalidJws)?;
    validate_id_alias_claims(claims, alias_tuple)
        .map_err(CredentialVerificationError::InvalidClaims)
}

/// Validates that the given claims are consistent with id_alias-credential
/// for the given alias tuple.
pub fn validate_id_alias_claims(
    claims: JwtClaims<Value>,
    alias_tuple: &AliasTuple,
) -> Result<(), JwtValidationError> {
    validate_claim("sub", did_for_principal(alias_tuple.id_dapp), claims.sub())?;
    validate_claim("iss", II_ISSUER_URL, claims.iss())?;
    let jti = claims.jti().ok_or(inconsistent_jwt_claims(
        "missing jti in id_alias JWT claims",
    ))?;
    if !jti.starts_with(II_CREDENTIAL_URL_PREFIX) {
        return Err(inconsistent_jwt_claims("wrong jti in id_alias JWT claims"));
    }
    let vc = claims
        .vc()
        .ok_or(inconsistent_jwt_claims("missing vc in id_alias JWT claims"))?;
    let subject_value = vc.get("credentialSubject").ok_or(inconsistent_jwt_claims(
        "missing credentialSubject in id_alias JWT vc",
    ))?;
    validate_credential_subject(subject_value, alias_tuple)?;
    Ok(())
}

/// Verifies the specified JWS credential cryptographically.
/// DOES NOT perform semantic validation of the claims in the credential.
pub fn verify_credential_jws(
    credential_jws: &str,
) -> Result<JwtClaims<Value>, SignatureVerificationError> {
    ///// Decode JWS.
    let decoder: Decoder = Decoder::new();
    let jws = decoder
        .decode_compact_serialization(credential_jws.as_ref(), None)
        .map_err(|e| invalid_signature_err(&format!("credential JWS parsing error: {}", e)))?;
    let canister_sig: CanisterSig = serde_cbor::from_slice(jws.decoded_signature())
        .map_err(|e| invalid_signature_err(&format!("signature parsing error: {}", e)))?;
    let ic_certificate: Certificate = serde_cbor::from_slice(canister_sig.certificate.as_ref())
        .map_err(|e| invalid_signature_err(&format!("certificate parsing error: {}", e)))?;
    let jws_header = jws
        .protected_header()
        .ok_or(invalid_signature_err("missing JWS header"))?;
    let canister_sig_pk = get_canister_sig_pk(jws_header)?;
    let claims: JwtClaims<Value> = serde_json::from_slice(jws.claims())
        .map_err(|e| invalid_signature_err(&format!("failed parsing JSON JWT claims: {}", e)))?;

    ///// Check if root hash of the signatures hash tree matches the certified data in the certificate
    let certified_data_path = [
        b"canister",
        canister_sig_pk.signing_canister_id.as_slice(),
        b"certified_data",
    ];
    // Get value of the certified data in the certificate
    let witness = match ic_certificate.tree.lookup_path(&certified_data_path) {
        LookupResult::Found(witness) => witness,
        _ => {
            return Err(invalid_signature_err(&format!(
                "certificate tree has no certified data witness for canister {} (0x{})",
                canister_sig_pk.signing_canister_id.to_text(),
                hex::encode(canister_sig_pk.signing_canister_id.as_slice())
            )))
        }
    };
    // Recompute the root hash of the signatures hash tree
    let digest = canister_sig.tree.digest();

    if witness != digest {
        return Err(invalid_signature_err(
            "certificate tree witness doesn't match signature tree digest",
        ));
    }

    ///// Check the certification path.
    let seed_hash = hash_bytes_sha256(&canister_sig_pk.seed);
    let signing_input_hash = verifiable_credential_signing_input_hash(jws.signing_input());
    let cert_sig_path = [b"sig", &seed_hash[..], &signing_input_hash[..]];
    match canister_sig.tree.lookup_path(&cert_sig_path) {
        LookupResult::Found(_) => {}
        _ => {
            return Err(invalid_signature_err(
                "missing signature path in canister's certified data",
            ))
        }
    }

    ///// Verify root signature (with delegation, if any).
    verify_root_signature(&ic_certificate, canister_sig_pk.signing_canister_id)
        .map_err(|e| invalid_signature_err(&e.to_string()))?;
    Ok(claims)
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
            println!(
                "inconsistent claim [{}] in id_alias VC::  expected: {}, actual: {}",
                label, expected, actual
            );
            Err(inconsistent_jwt_claims("inconsistent claim in id_alias VC"))
        }
    } else {
        println!("missing claim [{}] in id_alias VC", label);
        Err(inconsistent_jwt_claims("missing claim in id_alias VC"))
    }
}

// Per https://datatracker.ietf.org/doc/html/rfc7518#section-6.4,
// JwkParamsOct are for symmetric keys or another key whose value is a single octet sequence.
fn canister_sig_pk_jwk(canister_sig_pk_der: &[u8]) -> Jwk {
    let mut cspk_jwk = Jwk::new(JwkType::Oct);
    cspk_jwk.set_alg("IcCs");
    cspk_jwk
        .set_params(JwkParams::Oct(JwkParamsOct {
            k: encode_b64(canister_sig_pk_der),
        }))
        .expect("internal: failed setting JwkParams");
    cspk_jwk
}

fn inconsistent_jwt_claims(custom_message: &'static str) -> JwtValidationError {
    JwtValidationError::CredentialStructure(JwtVcError::InconsistentCredentialJwtClaims(
        custom_message,
    ))
}

fn jws_encoder<'a>(
    credential_jwt: &'a str,
    canister_sig_pk_der: &[u8],
    canister_id: Principal,
) -> CompactJwsEncoder<'a> {
    let mut header: JwsHeader = JwsHeader::new();
    header.set_alg(JwsAlgorithm::IcCs);
    let kid = did_for_principal(canister_id);
    header.set_kid(kid);
    header
        .deref_mut()
        .set_jwk(canister_sig_pk_jwk(canister_sig_pk_der));

    let encoder: CompactJwsEncoder = CompactJwsEncoder::new(credential_jwt.as_ref(), &header)
        .expect("internal error: JWS encoder failed");
    encoder
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

fn hash_bytes_sha256(bytes: &[u8]) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hasher.finalize().into()
}

fn verifiable_credential_signing_input_hash(signing_input: &[u8]) -> Hash {
    let sep = b"iccs_verifiable_credential";
    let mut hasher = Sha256::new();
    let buf = [sep.len() as u8];
    hasher.update(buf);
    hasher.update(sep);
    hasher.update(signing_input);
    hasher.finalize().into()
}

fn get_canister_sig_pk(
    jws_header: &JwsHeader,
) -> Result<CanisterSigPublicKey, SignatureVerificationError> {
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
    CanisterSigPublicKey::try_from(pk_der.as_slice()).map_err(|e| key_decoding_err(&e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use canister_sig_util::{
        set_ic_root_public_key_for_testing, CanisterSigVerificationError, IC_ROOT_KEY_DER_PREFIX,
    };
    use ic_cbor::CertificateToCbor;
    use ic_certification_testing::CertificateBuilder;
    use ic_response_verification_test_utils::AssetTree;
    use serial_test::serial;

    const TEST_SIGNING_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const TEST_IC_ROOT_PK_B64URL: &str = "MIGCMB0GDSsGAQQBgtx8BQMBAgEGDCsGAQQBgtx8BQMCAQNhAK32VjilMFayIiyRuyRXsCdLypUZilrL2t_n_XIXjwab3qjZnpR52Ah6Job8gb88SxH-J1Vw1IHxaY951Giv4OV6zB4pj4tpeY2nqJG77Blwk-xfR1kJkj1Iv-1oQ9vtHw";
    const ALIAS_PRINCIPAL: &str = "s33qc-ctnp5-ubyz4-kubqo-p2tem-he4ls-6j23j-hwwba-37zbl-t2lv3-pae";
    const DAPP_PRINCIPAL: &str = "cpehq-54hef-odjjt-bockl-3ldtg-jqle4-ysi5r-6bfah-v6lsa-xprdv-pqe";
    const ID_ALIAS_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCamxUYzNvSzVRVU9SbUt0T3YyVXBhMnhlQW5vNEJ4RlFFYmY1VWRUSTZlYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbnRpdHkiLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaW50ZXJuZXRjb21wdXRlci5vcmcvY3JlZGVudGlhbC9pbnRlcm5ldC1pZGVudGl0eS8xNjIwMzI4NjMwMDAwMDAwMDAwIiwic3ViIjoiZGlkOmljcDpjcGVocS01NGhlZi1vZGpqdC1ib2NrbC0zbGR0Zy1qcWxlNC15c2k1ci02YmZhaC12NmxzYS14cHJkdi1wcWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIkludGVybmV0SWRlbnRpdHlJZEFsaWFzIl0sImNyZWRlbnRpYWxTdWJqZWN0Ijp7Imhhc19pZF9hbGlhcyI6ImRpZDppY3A6czMzcWMtY3RucDUtdWJ5ejQta3VicW8tcDJ0ZW0taGU0bHMtNmoyM2otaHd3YmEtMzd6YmwtdDJsdjMtcGFlIn19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIMlBo1U8rvfGFEAvZoEFZX0uFlOGZLwgNjaBiyazTjUsggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIBfmGXVF1WCWPapsKI5MoFLJ55x11hQqSb_sRnrp5hFVggRYIBNvlU5ah4f5OsbVrHPPSsAhJo91Mf_fFkAE813rttVaggRYIEfscybxhCV3H9WVS6yOWyrSfnoAvrk5mr3vaKZOwOZiggRYID_qjkmyX1ydMvjuG7MS1E4grrzurjpGjbvYR7-YHMXwgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwogHXOX8bnQxCHy8TOkAG2Xak_qb2Yx22j5c9R5WC-8ResKLfeGgkSWbadE92xqRRZHRyZWWDAYIEWCB1hhWALXUuzFwXrojqFkaSB6Yejid7LwgvbIj61MjIN4MCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBggRYIOxGloHVCCWojZGGGusUYkg0Q-v_podIYMUM-jvDBF62gwJYIP28vIabsWSa5kTsn1ypw6vHamcQnTcYFNsiRlZMNcbAggNA";
    const ID_ALIAS_CREDENTIAL_JWS_NO_JWK: &str = "eyJraWQiOiJkaWQ6aWM6aWktY2FuaXN0ZXIiLCJhbGciOiJJY0NzIn0.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbml0eSIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9pbnRlcm5ldGNvbXB1dGVyLm9yZy9jcmVkZW50aWFsL2ludGVybmV0LWlkZW5pdHkiLCJzdWIiOiJkaWQ6d2ViOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOndlYjpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIG3uU_jutBtXB-of0uEA3RkCrcunK6D8QFPtX-gDSwDeggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIMULjwe1N6XomH10SEyc2r_uc7mGf1aSadeDaid9cUrkggRYIDw__VW2PgWMFp6mK-GmPG-7Fc90q58oK_wjcJ3IrkToggRYIAQTcQAtnxsa93zbfZEZV0f28OhiXL5Wp1OAyDHNI_x4ggRYINkQ8P9zGUvsVi3XbQ2bs6V_3kAiN8UNM6yPgeXfmArEgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwqHrYoUsNvSEaSShbW8barx0_ODXD5ZBEl9nKOdkNy_fBmGErE_C7ILbC91_fyZ7CZHRyZWWDAYIEWCB223o-sI97tc3LwJL3LRxQ4If6v_IvfC1fwIGYYQ9vroMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBgwJYIHszMLDS2VadioIaHajRY5iJzroqMs63lVrs_Uj42j0sggNAggRYICm0w_XxGEw4fDPoYcojCILEi0qdH4-4Zw7klzdaPNOC";

    fn test_ic_root_pk_der() -> Vec<u8> {
        decode_b64(TEST_IC_ROOT_PK_B64URL).expect("failure decoding canister pk")
    }

    fn alias_principal() -> Principal {
        Principal::from_text(ALIAS_PRINCIPAL).expect("wrong principal")
    }

    fn dapp_principal() -> Principal {
        Principal::from_text(DAPP_PRINCIPAL).expect("wrong principal")
    }

    fn principal_from_u64(i: u64) -> Principal {
        let mut bytes: Vec<u8> = i.to_be_bytes().to_vec();
        // Append 0x01 twice, to be compatible with CanisterId::from_u64() used by response_verification
        bytes.push(0x01);
        bytes.push(0x01);
        Principal::from_slice(&bytes)
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
    #[serial]
    fn should_verify_id_alias_vc_jws() {
        set_ic_root_public_key_for_testing(test_ic_root_pk_der());
        verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS).expect("JWS verification failed");
    }

    #[test]
    fn should_fail_verify_id_alias_vc_jws_without_canister_pk() {
        let result = verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS_NO_JWK);
        assert_matches!(result, Err(e) if e.to_string().contains("missing JWK in JWS header"));
    }

    #[test]
    #[serial]
    fn should_fail_verify_id_alias_vc_jws_with_wrong_root_pk() {
        let mut ic_root_pk_der = test_ic_root_pk_der();
        ic_root_pk_der[IC_ROOT_KEY_DER_PREFIX.len()] =
            ic_root_pk_der[IC_ROOT_KEY_DER_PREFIX.len()] + 1; // change the root pk value
        set_ic_root_public_key_for_testing(ic_root_pk_der);
        let result = verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS);
        assert_matches!(result, Err(e) if e.to_string().contains("InvalidBlsSignature"));
    }

    #[test]
    #[serial]
    fn should_verify_root_signature_without_delegation() {
        let signing_canister_id =
            Principal::from_text(TEST_SIGNING_CANISTER_ID).expect("failed parsing canister id");

        let ic_cert_data = CertificateBuilder::new(
            &signing_canister_id.to_string(),
            &AssetTree::new().get_certified_data(),
        )
        .expect("CertificateBuilder creation failed")
        .build()
        .expect("Certificate creation failed");
        set_ic_root_public_key_for_testing(ic_cert_data.root_key);
        let ic_certificate = Certificate::from_cbor(&ic_cert_data.cbor_encoded_certificate)
            .expect("CBOR cert parsing failed");

        verify_root_signature(&ic_certificate, signing_canister_id)
            .expect("Verification without delegation failed");
    }

    #[test]
    #[serial]
    fn should_verify_root_signature_with_delegation() {
        let signing_canister_id = principal_from_u64(5);
        let subnet_id = 123u64;
        let ic_cert_data = CertificateBuilder::new(
            &signing_canister_id.to_string(),
            &AssetTree::new().get_certified_data(),
        )
        .expect("CertificateBuilder creation failed")
        .with_delegation(subnet_id, vec![(0, 10)])
        .build()
        .expect("Certificate creation failed");
        set_ic_root_public_key_for_testing(ic_cert_data.root_key);
        let ic_certificate = Certificate::from_cbor(&ic_cert_data.cbor_encoded_certificate)
            .expect("CBOR cert parsing failed");
        verify_root_signature(&ic_certificate, signing_canister_id)
            .expect("Verification with delegation failed");
    }

    #[test]
    #[serial]
    fn should_fail_verify_root_signature_with_delegation_if_canister_not_in_range() {
        let signing_canister_id = principal_from_u64(42);
        let subnet_id = 123u64;
        let ic_cert_data = CertificateBuilder::new(
            &signing_canister_id.to_string(),
            &AssetTree::new().get_certified_data(),
        )
        .expect("CertificateBuilder creation failed")
        .with_delegation(subnet_id, vec![(0, 10)])
        .build()
        .expect("Certificate creation failed");
        set_ic_root_public_key_for_testing(ic_cert_data.root_key);
        let ic_certificate = Certificate::from_cbor(&ic_cert_data.cbor_encoded_certificate)
            .expect("CBOR cert parsing failed");

        let result = verify_root_signature(&ic_certificate, signing_canister_id);
        assert_matches!(
            result,
            Err(CanisterSigVerificationError::CertificateNotAuthorized)
        );
    }

    #[test]
    #[serial]
    fn should_fail_verify_root_signature_with_delegation_if_invalid_signature() {
        let signing_canister_id = principal_from_u64(5);
        let subnet_id = 123u64;
        let ic_cert_data = CertificateBuilder::new(
            &signing_canister_id.to_string(),
            &AssetTree::new().get_certified_data(),
        )
        .expect("CertificateBuilder creation failed")
        .with_delegation(subnet_id, vec![(0, 10)])
        .with_invalid_signature()
        .build()
        .expect("Certificate creation failed");
        set_ic_root_public_key_for_testing(ic_cert_data.root_key);
        let ic_certificate = Certificate::from_cbor(&ic_cert_data.cbor_encoded_certificate)
            .expect("CBOR cert parsing failed");

        let result = verify_root_signature(&ic_certificate, signing_canister_id);
        assert_matches!(
            result,
            Err(CanisterSigVerificationError::InvalidBlsSignature)
        );
    }

    #[test]
    #[serial]
    fn should_verify_id_alias_credential_jws() {
        set_ic_root_public_key_for_testing(test_ic_root_pk_der());
        verify_id_alias_credential_jws(
            ID_ALIAS_CREDENTIAL_JWS,
            &AliasTuple {
                id_alias: alias_principal(),
                id_dapp: dapp_principal(),
            },
        )
        .expect("JWS verification failed");
    }
}
