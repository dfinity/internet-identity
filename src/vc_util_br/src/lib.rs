use candid::Principal;
use canister_sig_util::extract_raw_canister_sig_pk_from_der;
use ic_certified_map::Hash;
use ic_crypto_standalone_sig_verifier::verify_canister_sig;
use ic_types::crypto::threshold_sig::IcRootOfTrust;
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Jwt, Subject};
use identity_credential::error::Error as JwtVcError;
use identity_credential::presentation::{
    JwtPresentationOptions, Presentation, PresentationBuilder, PresentationJwtClaims,
};
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

pub struct AliasTuple {
    pub id_alias: Principal,
    pub id_dapp: Principal,
}

#[derive(Debug)]
pub enum PresentationVerificationError {
    InvalidPresentationJwt(String),
    InvalidIdAliasCredential(CredentialVerificationError),
    InvalidRequestedCredential(CredentialVerificationError),
    Unknown(String),
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
    let mut hasher = Sha256::new();
    let buf = [VC_SIGNING_INPUT_DOMAIN.len() as u8];
    hasher.update(buf);
    hasher.update(VC_SIGNING_INPUT_DOMAIN);
    hasher.update(signing_input);
    hasher.finalize().into()
}

pub fn signing_input_with_prefix(signing_input: &[u8]) -> Vec<u8> {
    let mut result = Vec::from([VC_SIGNING_INPUT_DOMAIN.len() as u8]);
    result.extend_from_slice(VC_SIGNING_INPUT_DOMAIN);
    result.extend_from_slice(signing_input);
    result
}

pub fn did_for_principal(principal: Principal) -> String {
    let prefix = String::from("did:icp:");
    prefix.add(&principal.to_string())
}

pub fn verify_idp_presentation_jwt(
    vp_jwt: &str,
    alias_tuple: &AliasTuple,
    root_pk_raw: &[u8],
) -> Result<(), PresentationVerificationError> {
    let presentation = parse_verifiable_presentation_jwt(vp_jwt)
        .map_err(PresentationVerificationError::InvalidPresentationJwt)?;
    let expected_holder = Url::parse(did_for_principal(alias_tuple.id_dapp)).map_err(|_| {
        PresentationVerificationError::Unknown("internal: bad id_dapp principal".to_string())
    })?;
    if presentation.holder != expected_holder {
        return Err(PresentationVerificationError::InvalidPresentationJwt(
            "incompatible holder".to_string(),
        ));
    }
    if presentation.verifiable_credential.len() != 2 {
        return Err(PresentationVerificationError::InvalidPresentationJwt(
            "expected exactly two verifiable credentials".to_string(),
        ));
    }
    let id_alias_vc_jws =
        presentation
            .verifiable_credential
            .get(0)
            .ok_or(PresentationVerificationError::Unknown(
                "missing id_alias vc".to_string(),
            ))?;
    verify_id_alias_credential_jws(id_alias_vc_jws.as_str(), alias_tuple, root_pk_raw)
        .map_err(PresentationVerificationError::InvalidIdAliasCredential)?;
    let requested_vc_jws =
        presentation
            .verifiable_credential
            .get(1)
            .ok_or(PresentationVerificationError::Unknown(
                "missing requested vc".to_string(),
            ))?;
    let _claims = verify_credential_jws(requested_vc_jws.as_str(), root_pk_raw).map_err(|e| {
        PresentationVerificationError::InvalidRequestedCredential(
            CredentialVerificationError::InvalidJws(e),
        )
    })?;
    Ok(())
    // TODO: add verification of _claims
}

/// Verifies the given JWS-credential as an id_alias-VC for the specified alias tuple.
/// Performs both the cryptographic verification of the credential, and the semantic
/// validation of the id_alias-claims.
pub fn verify_id_alias_credential_jws(
    credential_jws: &str,
    alias_tuple: &AliasTuple,
    root_pk_raw: &[u8],
) -> Result<(), CredentialVerificationError> {
    let claims = verify_credential_jws(credential_jws, root_pk_raw)
        .map_err(CredentialVerificationError::InvalidJws)?;
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
    let canister_sig_pk = get_canister_sig_pk_bytes(jws_header)?;
    let root_pk_bytes: [u8; 96] = root_pk_raw
        .try_into()
        .map_err(|e| key_decoding_err(&format!("invalid root public key: {}", e)))?;
    let root_pk = IcRootOfTrust::from(root_pk_bytes);
    verify_canister_sig(&message, signature, canister_sig_pk.as_slice(), root_pk)
        .map_err(|e| invalid_signature_err(&format!("signature verification error: {}", e)))?;

    let claims: JwtClaims<Value> = serde_json::from_slice(jws.claims())
        .map_err(|e| invalid_signature_err(&format!("failed parsing JSON JWT claims: {}", e)))?;

    Ok(claims)
}
#[derive(Clone, Debug)]
pub struct PresentationParams {
    pub id_alias_vc_jws: String,
    pub requested_vc_jws: String,
}

fn presentation_to_compact_jwt(presentation: &Presentation<Jwt>) -> Result<String, String> {
    let mut header: JwsHeader = JwsHeader::new();
    header.set_typ("JWT");
    header.set_alg(JwsAlgorithm::NONE);
    let vp_jwt = presentation
        .serialize_jwt(&JwtPresentationOptions {
            expiration_date: None,
            issuance_date: None,
            audience: None,
        })
        .map_err(|_| "failed serializing presentation")?;
    let encoder: CompactJwsEncoder = CompactJwsEncoder::new(vp_jwt.as_ref(), &header)
        .map_err(|_| "internal error: JWS encoder failed")?;
    Ok(encoder.into_jws(&[]))
}

pub fn create_verifiable_presentation_jwt(
    params: PresentationParams,
    alias_tuple: &AliasTuple,
) -> Result<String, String> {
    let holder =
        Url::parse(did_for_principal(alias_tuple.id_dapp)).map_err(|_| "Invalid holder")?;
    let presentation: Presentation<Jwt> = PresentationBuilder::new(holder, Default::default())
        .credential(Jwt::from(params.id_alias_vc_jws))
        .credential(Jwt::from(params.requested_vc_jws))
        .build()
        .map_err(|_| "failed building presentation")?;
    presentation_to_compact_jwt(&presentation)
}

pub fn parse_verifiable_presentation_jwt(vp_jwt: &str) -> Result<Presentation<Jwt>, String> {
    let decoder = Decoder::new();
    let jwt = decoder
        .decode_compact_serialization(vp_jwt.as_ref(), None)
        .map_err(|_| "failed decoding compact jwt serialization")?;
    let presentation_claims = PresentationJwtClaims::from_json_slice(&jwt.claims())
        .map_err(|_| "failed parsing presentation claims")?;
    Ok(presentation_claims
        .try_into_presentation()
        .map_err(|_| "failed exporting presentation")?)
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

fn get_canister_sig_pk_bytes(
    jws_header: &JwsHeader,
) -> Result<Vec<u8>, SignatureVerificationError> {
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
    const REQUESTED_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCRW5ydW55RDN6Ty1pc29wNlRBOERkZWxxTEJmdkhLX1ZUeG5YZEl4bi1RZyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJpc3MiOiJodHRwczovL2lkZW50aXR5LmljMC5hcHAvIiwibmJmIjoxNjIwMzI4NjMwLCJqdGkiOiJodHRwczovL2V4YW1wbGUuZWR1L2NyZWRlbnRpYWxzLzM3MzIiLCJzdWIiOiJkaWQ6aWNwOm10cHBiLWtzY3ZhLTRoZHBsLXR4bWl0LWo3M3QzLWViZ2RpLWlybzRkLWtjZGZnLXVicm5oLW51bmE1LXpxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiVW5pdmVyc2l0eURlZ3JlZUNyZWRlbnRpYWwiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiR1BBIjoiNC4wIiwiZGVncmVlIjp7Im5hbWUiOiJCYWNoZWxvciBvZiBTY2llbmNlIGFuZCBBcnRzIiwidHlwZSI6IkJhY2hlbG9yRGVncmVlIn0sIm5hbWUiOiJBbGljZSJ9fX0.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggwsKW3qX8hPPdjOrA0R9YJgnTgdAn1nrgxIZZ_ebjZgeCBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFgglF8fe1owx2ekH6b_EzBZgU9L181hDdG2QX9bSIrGk7WCBFggiAQjCWS4p8OQjNKzyg73kXdTrJjDkDaF0g44WMAUruqCBFggV_MuZ9jXfsXICrVI40YLz5oaISN0_AX7bRJz6ZD5-3GCBFggpRekBB9l8QYhw2iKm1zHJJEmidbvuCTiajiT8KW4ci6CBFggkKkltZ_w0G1kjUDkcFOPeiQ8vdbJMRP03xe8a1r0ug6DAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCX6F5eY5WIa6_M7URZ816ANTNIqLgUWQi_lpnaNaEks67cL1luT2P-TxNCQB7n5uBkdHJlZYMCQ3NpZ4MCWCC64yx6oN_cInVY4FG60mk166oCBahlGepSE7IgGwes5YMCWCAMlZQJ9smkQJwvDgHDSxs-uV5XzdyOQsMMst2oKgRfKIIDQA";
    const ID_ALIAS_CREDENTIAL_JWS_NO_JWK: &str = "eyJraWQiOiJkaWQ6aWM6aWktY2FuaXN0ZXIiLCJhbGciOiJJY0NzIn0.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbml0eSIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9pbnRlcm5ldGNvbXB1dGVyLm9yZy9jcmVkZW50aWFsL2ludGVybmV0LWlkZW5pdHkiLCJzdWIiOiJkaWQ6d2ViOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOndlYjpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIG3uU_jutBtXB-of0uEA3RkCrcunK6D8QFPtX-gDSwDeggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIMULjwe1N6XomH10SEyc2r_uc7mGf1aSadeDaid9cUrkggRYIDw__VW2PgWMFp6mK-GmPG-7Fc90q58oK_wjcJ3IrkToggRYIAQTcQAtnxsa93zbfZEZV0f28OhiXL5Wp1OAyDHNI_x4ggRYINkQ8P9zGUvsVi3XbQ2bs6V_3kAiN8UNM6yPgeXfmArEgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwqHrYoUsNvSEaSShbW8barx0_ODXD5ZBEl9nKOdkNy_fBmGErE_C7ILbC91_fyZ7CZHRyZWWDAYIEWCB223o-sI97tc3LwJL3LRxQ4If6v_IvfC1fwIGYYQ9vroMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBgwJYIHszMLDS2VadioIaHajRY5iJzroqMs63lVrs_Uj42j0sggNAggRYICm0w_XxGEw4fDPoYcojCILEi0qdH4-4Zw7klzdaPNOC";

    fn test_ic_root_pk_raw() -> Vec<u8> {
        let pk_der = decode_b64(TEST_IC_ROOT_PK_B64URL).expect("failure decoding canister pk");
        extract_raw_root_pk_from_der(pk_der.as_slice())
            .expect("failure extracting root pk from DER")
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
    fn should_verify_id_alias_vc_jws() {
        verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS, &test_ic_root_pk_raw())
            .expect("JWS verification failed");
    }

    #[test]
    fn should_fail_verify_id_alias_vc_jws_without_canister_pk() {
        let result = verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS_NO_JWK, &test_ic_root_pk_raw());
        assert_matches!(result, Err(e) if e.to_string().contains("missing JWK in JWS header"));
    }

    #[test]
    fn should_fail_verify_id_alias_vc_jws_with_wrong_root_pk() {
        let mut ic_root_pk = test_ic_root_pk_raw();
        ic_root_pk[IC_ROOT_PK_DER_PREFIX.len()] += 1; // change the root pk value
        let result = verify_credential_jws(ID_ALIAS_CREDENTIAL_JWS, &ic_root_pk);
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
            &test_ic_root_pk_raw(),
        )
        .expect("JWS verification failed");
    }

    #[test]
    fn should_create_and_parse_verifiable_presentation() {
        let params = PresentationParams {
            id_alias_vc_jws: "a dummy id_alias_vc_jws".to_string(),
            requested_vc_jws: "a dummy requested_vc_jws".to_string(),
        };
        let alias_tuple = AliasTuple {
            id_alias: principal_from_u64(1),
            id_dapp: principal_from_u64(2),
        };
        let vp_jwt = create_verifiable_presentation_jwt(params.clone(), &alias_tuple)
            .expect("vp-creation failed");
        let presentation: Presentation<Jwt> =
            parse_verifiable_presentation_jwt(&vp_jwt).expect("failed jwt parsing");

        assert!(presentation
            .verifiable_credential
            .contains(&Jwt::from(params.id_alias_vc_jws)));
        assert!(presentation
            .verifiable_credential
            .contains(&Jwt::from(params.requested_vc_jws)));
        assert_eq!(
            Url::parse(did_for_principal(alias_tuple.id_dapp)).expect("bad url"),
            presentation.holder
        );
    }

    #[test]
    fn should_verify_verifiable_presentation() {
        let alias_tuple = AliasTuple {
            id_alias: alias_principal(),
            id_dapp: dapp_principal(),
        };
        let params = PresentationParams {
            id_alias_vc_jws: ID_ALIAS_CREDENTIAL_JWS.to_string(),
            requested_vc_jws: REQUESTED_CREDENTIAL_JWS.to_string(),
        };
        let vp_jwt =
            create_verifiable_presentation_jwt(params, &alias_tuple).expect("vp creation failed");
        verify_idp_presentation_jwt(&vp_jwt, &alias_tuple, &test_ic_root_pk_raw())
            .expect("vp verification failed");
    }

    #[test]
    fn should_fail_verifying_verifiable_presentation_with_bad_jwt() {
        let alias_tuple = AliasTuple {
            id_alias: alias_principal(),
            id_dapp: dapp_principal(),
        };
        let bad_vp_jwt = "some badly formatted string";
        let result = verify_idp_presentation_jwt(bad_vp_jwt, &alias_tuple, &test_ic_root_pk_raw());
        assert_matches!(
            result,
            Err(PresentationVerificationError::InvalidPresentationJwt(_))
        );
    }

    #[test]
    fn should_fail_verifying_verifiable_presentation_with_wrong_holder() {
        let alias_tuple = AliasTuple {
            id_alias: alias_principal(),
            id_dapp: dapp_principal(),
        };
        let params = PresentationParams {
            id_alias_vc_jws: ID_ALIAS_CREDENTIAL_JWS.to_string(),
            requested_vc_jws: REQUESTED_CREDENTIAL_JWS.to_string(),
        };
        let vp_jwt =
            create_verifiable_presentation_jwt(params, &alias_tuple).expect("vp creation failed");
        let wrong_alias_tuple = AliasTuple {
            id_alias: alias_principal(),
            id_dapp: alias_principal(),
        };
        let result =
            verify_idp_presentation_jwt(&vp_jwt, &wrong_alias_tuple, &test_ic_root_pk_raw());
        assert_matches!(
            result,
            Err(PresentationVerificationError::InvalidPresentationJwt(_))
        );
    }
}
