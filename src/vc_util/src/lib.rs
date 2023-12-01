use candid::Principal;
use canister_sig_util::{extract_raw_canister_sig_pk_from_der, CanisterSigPublicKey};
use ic_certification::Hash;
use ic_crypto_standalone_sig_verifier::verify_canister_sig;
use ic_types::crypto::threshold_sig::IcRootOfTrust;
use identity_core::convert::FromJson;
use identity_credential::credential::{Jwt, Subject};
use identity_credential::error::Error as JwtVcError;
use identity_credential::presentation::{Presentation, PresentationJwtClaims};
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

pub mod issuer_api;

pub const II_CREDENTIAL_URL_PREFIX: &str = "https://identity.ic0.app/credential/";
pub const II_ISSUER_URL: &str = "https://identity.ic0.app/";
pub const VC_SIGNING_INPUT_DOMAIN: &[u8; 26] = b"iccs_verifiable_credential";
pub const DID_ICP_PREFIX: &str = "did:icp:";

/// A pair of identities, that denote the same user.
/// Used in attribute sharing flow to maintain II's unlinkability of identities.
#[derive(Debug, Eq, PartialEq)]
pub struct AliasTuple {
    /// A temporary identity, used in attribute sharing flow.
    pub id_alias: Principal,
    /// An identity under which a user is known to a dapp.
    pub id_dapp: Principal,
}

#[derive(Debug, Eq, PartialEq)]
pub struct VcFlowParties {
    /// Ids of canisters that issued credentials contained in a verifiable presentation.
    pub ii_canister_id: Principal,
    pub issuer_canister_id: Principal,
}

#[derive(Debug)]
pub enum CredentialVerificationError {
    InvalidJws(SignatureVerificationError),
    InvalidClaims(JwtValidationError),
}

#[derive(Debug)]
pub enum PresentationVerificationError {
    InvalidPresentationJwt(String),
    InvalidIdAliasCredential(CredentialVerificationError),
    InvalidRequestedCredential(CredentialVerificationError),
    Unknown(String),
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
    DID_ICP_PREFIX.to_string().add(&principal.to_string())
}

/// Returns a `principal` for the given DID.
pub fn principal_for_did(did: &str) -> Result<Principal, String> {
    if !did.starts_with(DID_ICP_PREFIX) {
        return Err(format!(
            "invalid DID: {}, expected prefix {}",
            did, DID_ICP_PREFIX
        ));
    }
    Principal::from_text(did.trim_start_matches(DID_ICP_PREFIX))
        .map_err(|e| format!("failed to parse DID: {}", e))
}

/// Verifies the given JWS-credential as an id_alias-VC and extracts the alias tuple.
/// Performs both the cryptographic verification of the credential.
pub fn get_verified_id_alias_from_jws(
    credential_jws: &str,
    signing_canister_id: &Principal,
    root_pk_raw: &[u8],
) -> Result<AliasTuple, CredentialVerificationError> {
    let claims =
        verify_credential_jws_with_canister_id(credential_jws, signing_canister_id, root_pk_raw)
            .map_err(CredentialVerificationError::InvalidJws)?;
    validate_claim("iss", II_ISSUER_URL, claims.iss())
        .map_err(CredentialVerificationError::InvalidClaims)?;
    extract_id_alias(&claims).map_err(CredentialVerificationError::InvalidClaims)
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

fn parse_verifiable_presentation_jwt(vp_jwt: &str) -> Result<Presentation<Jwt>, String> {
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

/// Verifies the specified JWT presentation cryptographically, which should contain exactly
/// two verifiable credentials (in the order specifed):
///   1. An id_alias credential which links the holder of the VP to a temporary id_alias.
///      This credential should be signed by canister vc_flow_parties.ii_canister_id.
///   2. An actual credential requested by a user.  The subject of this credential is id_alias,
///      and it should be signed by canister vc_flow_parties.issuer_canister_id
/// Returns holder's identity together with id_alias, and the claims from the requested credential.
/// DOES NOT perform semantic validation of the returned claims.
pub fn verify_ii_presentation_jwt_with_canister_ids(
    vp_jwt: &str,
    vc_flow_parties: &VcFlowParties,
    root_pk_raw: &[u8],
) -> Result<(AliasTuple, JwtClaims<Value>), PresentationVerificationError> {
    let presentation = parse_verifiable_presentation_jwt(vp_jwt)
        .map_err(PresentationVerificationError::InvalidPresentationJwt)?;
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
    let alias_tuple = get_verified_id_alias_from_jws(
        id_alias_vc_jws.as_str(),
        &vc_flow_parties.ii_canister_id,
        root_pk_raw,
    )
    .map_err(PresentationVerificationError::InvalidIdAliasCredential)?;
    let holder = principal_for_did(&presentation.holder.to_string()).map_err(|e| {
        PresentationVerificationError::Unknown(format!("error parsing holder: {}", e))
    })?;
    if holder != alias_tuple.id_dapp {
        return Err(PresentationVerificationError::InvalidPresentationJwt(
            format!(
                "holder does not match subject: expected {}, got {}",
                holder, alias_tuple.id_dapp
            )
            .to_string(),
        ));
    }
    let requested_vc_jws =
        presentation
            .verifiable_credential
            .get(1)
            .ok_or(PresentationVerificationError::Unknown(
                "missing requested vc".to_string(),
            ))?;
    let claims = verify_credential_jws_with_canister_id(
        requested_vc_jws.as_str(),
        &vc_flow_parties.issuer_canister_id,
        root_pk_raw,
    )
    .map_err(|e| {
        PresentationVerificationError::InvalidRequestedCredential(
            CredentialVerificationError::InvalidJws(e),
        )
    })?;
    let requested_vc_subject = extract_subject(&claims).map_err(|e| {
        PresentationVerificationError::InvalidRequestedCredential(
            CredentialVerificationError::InvalidClaims(e),
        )
    })?;
    if requested_vc_subject != alias_tuple.id_alias {
        return Err(PresentationVerificationError::InvalidPresentationJwt(
            format!(
                "subject does not match id_alias: expected {}, got {}",
                alias_tuple.id_alias, requested_vc_subject
            )
            .to_string(),
        ));
    }
    Ok((alias_tuple, claims))
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

fn extract_subject(claims: &JwtClaims<Value>) -> Result<Principal, JwtValidationError> {
    let Some(sub) = claims.sub() else {
        return Err(JwtValidationError::CredentialStructure(
            JwtVcError::MissingSubject,
        ));
    };
    let subject = principal_for_did(sub)
        .map_err(|_| JwtValidationError::CredentialStructure(JwtVcError::InvalidSubject))?;
    Ok(subject)
}

fn extract_id_alias(claims: &JwtClaims<Value>) -> Result<AliasTuple, JwtValidationError> {
    let id_dapp = extract_subject(claims)?;
    let vc = claims.vc().ok_or(inconsistent_jwt_claims(
        "missing \"vc\" claim in id_alias JWT claims",
    ))?;
    let subject_value = vc.get("credentialSubject").ok_or(inconsistent_jwt_claims(
        "missing \"credentialSubject\" claim in id_alias JWT vc",
    ))?;
    let subject = Subject::from_json_value(subject_value.clone()).map_err(|_| {
        inconsistent_jwt_claims("malformed \"credentialSubject\" claim in id_alias JWT vc")
    })?;
    let Value::String(ref alias) = subject.properties["has_id_alias"] else {
        return Err(inconsistent_jwt_claims(
            "missing \"has_id_alias\" claim in id_alias JWT vc",
        ));
    };
    let id_alias = principal_for_did(alias).map_err(|_| {
        inconsistent_jwt_claims("malformed \"has_id_alias\" claim in id_alias JWT vc")
    })?;
    println!(
        "*** id_alias tuple from claims: id_alias: {}, id_dapp: {}",
        id_alias, id_dapp
    );
    Ok(AliasTuple { id_alias, id_dapp })
}

fn validate_claim<T: PartialEq<S> + std::fmt::Display, S: std::fmt::Display>(
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
    use identity_core::common::Url;
    use identity_credential::presentation::{JwtPresentationOptions, PresentationBuilder};

    const TEST_IC_ROOT_PK_B64URL: &str = "MIGCMB0GDSsGAQQBgtx8BQMBAgEGDCsGAQQBgtx8BQMCAQNhAK32VjilMFayIiyRuyRXsCdLypUZilrL2t_n_XIXjwab3qjZnpR52Ah6Job8gb88SxH-J1Vw1IHxaY951Giv4OV6zB4pj4tpeY2nqJG77Blwk-xfR1kJkj1Iv-1oQ9vtHw";
    const ALIAS_PRINCIPAL: &str = "s33qc-ctnp5-ubyz4-kubqo-p2tem-he4ls-6j23j-hwwba-37zbl-t2lv3-pae";
    const DAPP_PRINCIPAL: &str = "cpehq-54hef-odjjt-bockl-3ldtg-jqle4-ysi5r-6bfah-v6lsa-xprdv-pqe";
    const ID_ALIAS_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCamxUYzNvSzVRVU9SbUt0T3YyVXBhMnhlQW5vNEJ4RlFFYmY1VWRUSTZlYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIPJBKvDetPwyft_h5oQ0nnxjTE_PNV3PrJy1qwKUrh0LggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIGgDAfUw2gIV3kD2PAwglKvdL6iSQ6qQwFcFPtFnQPTpggRYIBTRAqxOalCVKAHk5WxJ9MVl6VO8uaE3dqwFQzy-KkV_ggRYIPzx8nwybmJ4amD88KZwujDMw22rOmhXTr6RRf38pPV4ggRYIARJcosVL7413JPlfHGUgZQyu2CZ2XFVRnJBCkZSRjROgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwr8xRG17_yzlaO2dIWFTQXGwPVPaAkD4P3hdjJyNmSRKDehzvWAShX03w9q_tZGlCZHRyZWWDAYIEWCAqgobdT9VjwJT-e8ou6LaCrL32vpKdLcqyN9RHVjBthIMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBggRYIFdVX_z6VIe33hrXKtxlLy8EZsv4_GNMMUXVF64TMzDqgwJYIF8bQCq3kGPfDU8PEiRnEmqgwwtHm3iGJeuwTLlxBocCggNA";
    const ID_ALIAS_CREDENTIAL_JWS_NO_JWK: &str = "eyJraWQiOiJkaWQ6aWM6aWktY2FuaXN0ZXIiLCJhbGciOiJJY0NzIn0.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbml0eSIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9pbnRlcm5ldGNvbXB1dGVyLm9yZy9jcmVkZW50aWFsL2ludGVybmV0LWlkZW5pdHkiLCJzdWIiOiJkaWQ6d2ViOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOndlYjpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIG3uU_jutBtXB-of0uEA3RkCrcunK6D8QFPtX-gDSwDeggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIMULjwe1N6XomH10SEyc2r_uc7mGf1aSadeDaid9cUrkggRYIDw__VW2PgWMFp6mK-GmPG-7Fc90q58oK_wjcJ3IrkToggRYIAQTcQAtnxsa93zbfZEZV0f28OhiXL5Wp1OAyDHNI_x4ggRYINkQ8P9zGUvsVi3XbQ2bs6V_3kAiN8UNM6yPgeXfmArEgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwqHrYoUsNvSEaSShbW8barx0_ODXD5ZBEl9nKOdkNy_fBmGErE_C7ILbC91_fyZ7CZHRyZWWDAYIEWCB223o-sI97tc3LwJL3LRxQ4If6v_IvfC1fwIGYYQ9vroMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBgwJYIHszMLDS2VadioIaHajRY5iJzroqMs63lVrs_Uj42j0sggNAggRYICm0w_XxGEw4fDPoYcojCILEi0qdH4-4Zw7klzdaPNOC";
    const TEST_SIGNING_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const TEST_SEED: [u8; 32] = [
        142, 84, 220, 222, 130, 185, 65, 67, 145, 152, 171, 78, 191, 101, 41, 107, 108, 94, 2, 122,
        56, 7, 17, 80, 17, 183, 249, 81, 212, 200, 233, 231,
    ];
    const TEST_CREDENTIAL_JWT: &str = r#"{"iss":"https://employment.info/","nbf":1620328630,"jti":"https://employment.info/credentials/42","sub":"did:icp:igfpm-3fhrp-syqme-4i4xk-o4pgd-5xdh4-fbbgw-jnxm5-bvou4-ljt52-kqe","vc":{"@context":"https://www.w3.org/2018/credentials/v1","type":["VerifiableCredential","VerifiedEmployee"],"credentialSubject":{"employee_of":{"employerId":"did:web:dfinity.org","employerName":"DFINITY Foundation"}}}}"#;

    // Test data used for verifiable presentation tests.
    const TEST_ISSUER_SIGNING_CANISTER_ID: &str = "rrkah-fqaaa-aaaaa-aaaaq-cai";
    const ID_ALIAS_FOR_VP: &str = "vhbib-m4hm6-hpvyc-7prd2-siivo-nbd7r-67o5x-n3awh-qsmqz-wznjf-tqe";
    const ID_RP_FOR_VP: &str = "p2nlc-3s5ul-lcu74-t6pn2-ui5im-i4a5f-a4tga-e6znf-tnvlh-wkmjs-dqe";
    const ID_ALIAS_VC_FOR_VP_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCRVNzWHp2bTEzd1BkRTVZSndvLTBCYkdBTHdCN0J2bW1LZUxramFUUTdkQSJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOnAybmxjLTNzNXVsLWxjdTc0LXQ2cG4yLXVpNWltLWk0YTVmLWE0dGdhLWU2em5mLXRudmxoLXdrbWpzLWRxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDp2aGJpYi1tNGhtNi1ocHZ5Yy03cHJkMi1zaWl2by1uYmQ3ci02N281eC1uM2F3aC1xc21xei13em5qZi10cWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1gg_I-cV8ZPkoVyh_WcilMFKcpT5cH2-oyCoRTh7llUZeyCBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggkzS0s8W3-LMHUd6MNFx8vCZvWiTSFRorIEMWhTwPnnmCBFggnrchOFTbl4uvnyde_cSNSJyYA1bRSyy00Wc9euCSodyCBFggIujS7mOdXhfJUJZBqbtLm22ZyONdyVFbAnpVPc30ZBCCBFggQkuzJEh6pJ0g-QNG7IbT7mnxW4XJSKdeWGVTxAw3jiaCBFggwQi-CzFufoFsQD0tmyYIhiaRNWLbADbGQmT6CchuXqSDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDC5hBVS08Sf6LMHt1w84qtUQ_ELlYt494xOwPQ9_a45cQ2YbpXovT2OohCzC0Rtfk5kdHJlZYMBggRYIDUbro13tGgB_Yqf9JhMW8pvpzCvCBJY_J0CfFMPNBthgwJDc2lngwJYIFcsa4eb-HMrTnmGWNje_RfErQYi0wNCJvGDrzqazq0OgwGDAlggltI_YbE4mukz0q2BJuH-cIbYTjmBOZbpwqiCPgURrdqCA0CCBFggq2BLsFnxNQd_gswt0oE2oBJO-Cwaey9RhEs4XhsPT2w";
    const REQUESTED_VC_FOR_VP_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCeUk3dlEyOGVybHFnVjVMck03dTNIOUlaeGVwcUxzQkdnSjFyTldaX0tfQSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vZW1wbG95bWVudC5pbmZvLyIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9lbXBsb3ltZW50LmluZm8vY3JlZGVudGlhbHMvNDIiLCJzdWIiOiJkaWQ6aWNwOnZoYmliLW00aG02LWhwdnljLTdwcmQyLXNpaXZvLW5iZDdyLTY3bzV4LW4zYXdoLXFzbXF6LXd6bmpmLXRxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiVmVyaWZpZWRFbXBsb3llZSJdLCJjcmVkZW50aWFsU3ViamVjdCI6eyJlbXBsb3llZV9vZiI6eyJlbXBsb3llcklkIjoiZGlkOndlYjpkZmluaXR5Lm9yZyIsImVtcGxveWVyTmFtZSI6IkRGSU5JVFkgRm91bmRhdGlvbiJ9fX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggqJgFhbOSOgOY-buXQFiOROH05grMrWl8vT4tcEIPiq6DAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggQ4oRC4U7Lky7wzOfBg9VDx119qT8VxNIS_--J9EL4o6CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggxZtR4JD8cPLlnq01PLx8CEHFa79R67oIdK_dBGpq6mmCBFggdUc130tu5B34n_1cMDNiJCbEamgKPeRmzOso7AmvmWqCBFggs_FSiRZluL8IU_a-voSJLAO8z2nvX1MmJCq_Tz0e1amCBFggqSI6U4ChzjO4YJ_NvOqGukyHIfX-3UYcYgkyTLgF9D6DAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDC0pP0klDiTh8fvUkKDDcrCjECDHalcBWLYKVzYy6Sm2rMZ2tsXvCbtf2fraClWVipkdHJlZYMBggRYICk5fdPgBGT68fKT0kj59h1zod2tut7mB0DAKhosc6-igwJDc2lngwJYIHGZW4y0kE1oq6oGYkhXj36h1sNPmG2jwFX6tPGiRkfXgwJYIOHTIYnxMIa4fJPR4CTYus-9YPlhsgAYJwpj0j9Jx3NcggNA";

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

    fn test_issuer_canister_sig_pk() -> CanisterSigPublicKey {
        CanisterSigPublicKey::new(
            Principal::from_text(TEST_ISSUER_SIGNING_CANISTER_ID).expect("wrong principal"),
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

    fn create_verifiable_presentation_jwt_for_test(
        holder: Principal,
        vcs_jws: Vec<String>,
    ) -> Result<String, String> {
        let holder_url = Url::parse(did_for_principal(holder)).map_err(|_| "Invalid holder")?;
        let mut builder = PresentationBuilder::new(holder_url, Default::default());
        for vc in vcs_jws {
            builder = builder.credential(Jwt::from(vc));
        }
        let presentation: Presentation<Jwt> = builder
            .build()
            .map_err(|_| "failed building presentation")?;
        presentation_to_compact_jwt(&presentation)
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
        let claims = claims_from_jws(ID_ALIAS_CREDENTIAL_JWS);
        validate_claim("iss", II_ISSUER_URL, claims.iss())
            .expect("Failed validating id_alias claims");
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
    fn should_verify_and_extract_id_alias_credential_jws() {
        let alias_tuple = get_verified_id_alias_from_jws(
            ID_ALIAS_CREDENTIAL_JWS,
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
        )
        .expect("JWS verification failed");
        assert_eq!(
            alias_tuple,
            AliasTuple {
                id_alias: alias_principal(),
                id_dapp: dapp_principal(),
            }
        )
    }

    #[test]
    fn should_parse_verifiable_presentation() {
        let id_alias_vc_jws = "a dummy id_alias_vc_jws".to_string();
        let requested_vc_jws = "a dummy requested_vc_jws".to_string();
        let holder = dapp_principal();
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![id_alias_vc_jws.clone(), requested_vc_jws.clone()],
        )
        .expect("vp-creation failed");
        let presentation: Presentation<Jwt> =
            parse_verifiable_presentation_jwt(&vp_jwt).expect("failed jwt parsing");

        assert!(presentation
            .verifiable_credential
            .contains(&Jwt::from(id_alias_vc_jws)));
        assert!(presentation
            .verifiable_credential
            .contains(&Jwt::from(requested_vc_jws)));
        assert_eq!(
            Url::parse(did_for_principal(holder)).expect("bad url"),
            presentation.holder
        );
    }

    #[test]
    fn should_verify_ii_presentation() {
        let id_alias = Principal::from_text(ID_ALIAS_FOR_VP).expect("wrong principal");
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let (alias_tuple_from_jws, _claims) = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        )
        .expect("vp verification failed");
        assert_eq!(id_alias, alias_tuple_from_jws.id_alias);
        assert_eq!(id_dapp, alias_tuple_from_jws.id_dapp);
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_extra_vc() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
                "an extra vc".to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("expected exactly two verifiable credentials"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_missing_vc() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![ID_ALIAS_VC_FOR_VP_JWS.to_string()],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("expected exactly two verifiable credentials"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_holder() {
        let wrong_holder = dapp_principal(); // does not match ID_ALIAS_VC_FOR_VP_JWS
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            wrong_holder,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("holder does not match subject"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_non_matching_id_alias_in_vcs() {
        let holder = dapp_principal(); // does match ID_ALIAS_CREDENTIAL_JWS

        // ID_ALIAS_CREDENTIAL_JWS does not match REQUESTED_VC_FOR_VP_JWS
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("subject does not match id_alias"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_invalid_id_alias_vc() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let mut bad_id_alias_vc = ID_ALIAS_VC_FOR_VP_JWS.to_string();
        bad_id_alias_vc.insert(42, 'a');
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![bad_id_alias_vc, REQUESTED_VC_FOR_VP_JWS.to_string()],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("InvalidSignature"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_invalid_requested_vc() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let mut bad_requested_vc = REQUESTED_VC_FOR_VP_JWS.to_string();
        bad_requested_vc.insert(42, 'a');
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![ID_ALIAS_VC_FOR_VP_JWS.to_string(), bad_requested_vc],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("InvalidSignature"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_ii_canister_id() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_issuer_canister_sig_pk().canister_id,
                issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("canister id does not match"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_issuer_canister_id() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                REQUESTED_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                ii_canister_id: test_canister_sig_pk().canister_id,
                issuer_canister_id: test_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("canister id does not match"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_order_of_vcs() {
        let holder = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        // Swap the order of the VCs
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            holder,
            vec![
                REQUESTED_VC_FOR_VP_JWS.to_string(),
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            &VcFlowParties {
                // Swap also the order of the canister ids, so that they match the VCs
                ii_canister_id: test_issuer_canister_sig_pk().canister_id,
                issuer_canister_id: test_canister_sig_pk().canister_id,
            },
            &test_ic_root_pk_raw(),
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("inconsistent claim in id_alias VC"));
    }
}
