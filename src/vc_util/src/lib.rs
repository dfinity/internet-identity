use crate::issuer_api::CredentialSpec;
use candid::Principal;
use canister_sig_util::{extract_raw_canister_sig_pk_from_der, CanisterSigPublicKey};
use ic_certification::Hash;
use ic_crypto_standalone_sig_verifier::verify_canister_sig;
use ic_types::crypto::threshold_sig::IcRootOfTrust;
use identity_core::common::{Timestamp, Url};
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Jwt, Subject};
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
use serde_json::{json, Map, Value};
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
/// Parties that signed credentials contained in a verifiable presentation.
pub struct VcFlowSigners {
    pub ii_canister_id: Principal,
    pub ii_origin: String,
    pub issuer_canister_id: Principal,
    pub issuer_origin: String,
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
/// Performs both the cryptographic verification of the credential, and the semantic
/// validation of the claims in the VC.
pub fn get_verified_id_alias_from_jws(
    credential_jws: &str,
    expected_vc_subject: &Principal,
    signing_canister_id: &Principal,
    root_pk_raw: &[u8],
    current_time_ns: u128,
) -> Result<AliasTuple, CredentialVerificationError> {
    let claims = verify_credential_jws_with_canister_id(
        credential_jws,
        signing_canister_id,
        root_pk_raw,
        current_time_ns,
    )
    .map_err(CredentialVerificationError::InvalidJws)?;
    validate_claim("iss", II_ISSUER_URL, claims.iss())
        .map_err(CredentialVerificationError::InvalidClaims)?;
    let alias_tuple =
        extract_id_alias(&claims).map_err(CredentialVerificationError::InvalidClaims)?;
    if *expected_vc_subject != alias_tuple.id_dapp {
        return Err(CredentialVerificationError::InvalidClaims(
            inconsistent_jwt_claims("unexpected vc subject"),
        ));
    }
    Ok(alias_tuple)
}

/// Verifies the specified JWS credential cryptographically and checks that the signature was
/// created by the provided canister.
/// DOES NOT perform semantic validation of the claims in the credential.
pub fn verify_credential_jws_with_canister_id(
    credential_jws: &str,
    signing_canister_id: &Principal,
    root_pk_raw: &[u8],
    current_time_ns: u128,
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
    validate_expiration(claims.exp(), current_time_ns)
        .map_err(|e| invalid_signature_err(&format!("credential expired: {}", e)))?;
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
///   1. An "Id alias" credential which links the effective subject of the VP to a temporary id_alias.
///      This credential should be signed by canister vc_flow_parties.ii_canister_id.
///   2. An actual credential requested by a user.  The subject of this credential is id_alias,
///      and it should be signed by canister vc_flow_parties.issuer_canister_id
/// Verifies that the subject of the first credential matches `effective_vc_subject`.
/// Returns the verified `effective_vc_subject` with id_alias, and the claims from the requested credential.
/// DOES NOT perform semantic validation of the returned claims.
pub fn verify_ii_presentation_jwt_with_canister_ids(
    vp_jwt: &str,
    effective_vc_subject: Principal,
    vc_flow_signers: &VcFlowSigners,
    root_pk_raw: &[u8],
    current_time_ns: u128,
) -> Result<(AliasTuple, JwtClaims<Value>), PresentationVerificationError> {
    let presentation = parse_verifiable_presentation_jwt(vp_jwt)
        .map_err(PresentationVerificationError::InvalidPresentationJwt)?;
    if presentation.verifiable_credential.len() != 2 {
        return Err(PresentationVerificationError::InvalidPresentationJwt(
            "expected exactly two verifiable credentials".to_string(),
        ));
    }
    let id_alias_vc_jws = presentation.verifiable_credential.first().ok_or(
        PresentationVerificationError::Unknown("missing id_alias vc".to_string()),
    )?;
    let alias_tuple = get_verified_id_alias_from_jws(
        id_alias_vc_jws.as_str(),
        &effective_vc_subject,
        &vc_flow_signers.ii_canister_id,
        root_pk_raw,
        current_time_ns,
    )
    .map_err(PresentationVerificationError::InvalidIdAliasCredential)?;
    let requested_vc_jws =
        presentation
            .verifiable_credential
            .get(1)
            .ok_or(PresentationVerificationError::Unknown(
                "missing requested vc".to_string(),
            ))?;
    let claims = verify_credential_jws_with_canister_id(
        requested_vc_jws.as_str(),
        &vc_flow_signers.issuer_canister_id,
        root_pk_raw,
        current_time_ns,
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

/// Validates the provided presentation `vp_jwt`, both cryptographically and semantically:
///  - verifies the cryptographic consistency via `verify_ii_presentation_jwt_with_canister_ids(...)`.
///  - checks that the claims from the presentation match the credential spec `vc_spec`.
pub fn validate_ii_presentation_and_claims(
    vp_jwt: &str,
    effective_vc_subject: Principal,
    vc_flow_signers: &VcFlowSigners,
    vc_spec: &CredentialSpec,
    root_pk_raw: &[u8],
    current_time_ns: u128,
) -> Result<(), PresentationVerificationError> {
    let (_alias_tuple, claims) = verify_ii_presentation_jwt_with_canister_ids(
        vp_jwt,
        effective_vc_subject,
        vc_flow_signers,
        root_pk_raw,
        current_time_ns,
    )?;
    validate_claim("iss", &vc_flow_signers.issuer_origin, claims.iss())
        .map_err(invalid_requested_vc)?;
    let vc_claims = claims
        .vc()
        .ok_or(invalid_requested_vc(inconsistent_jwt_claims(
            "missing vc in id_alias JWT claims",
        )))?;
    validate_claims_match_spec(vc_claims, vc_spec).map_err(invalid_requested_vc)?;
    Ok(())
}

pub struct CredentialParams {
    pub spec: CredentialSpec,
    pub subject_id: String,
    pub credential_id_url: String,
    pub issuer_url: String,
    pub expiration_timestamp_s: u32,
}

/// Builds a verifiable credential with the given parameters and returns the credential as a JWT-string.
pub fn build_credential_jwt(params: CredentialParams) -> String {
    let mut subject_json = json!({"id": params.subject_id});
    subject_json.as_object_mut().unwrap().insert(
        params.spec.credential_type.clone(),
        credential_spec_args_to_json(&params.spec),
    );
    let subject = Subject::from_json_value(subject_json).unwrap();
    let expiration_date = Timestamp::from_unix(params.expiration_timestamp_s as i64)
        .expect("internal: failed computing expiration timestamp");
    let credential: Credential = CredentialBuilder::default()
        .id(Url::parse(params.credential_id_url).unwrap())
        .issuer(Url::parse(params.issuer_url).unwrap())
        .type_(params.spec.credential_type)
        .subject(subject)
        .expiration_date(expiration_date)
        .build()
        .unwrap();
    credential.serialize_jwt().unwrap()
}

fn credential_spec_args_to_json(spec: &CredentialSpec) -> serde_json::Value {
    let mut args_map = serde_json::Map::new();
    if let Some(args) = spec.arguments.as_ref() {
        for arg in args {
            args_map.insert(arg.0.clone(), arg.1.clone().into());
        }
    }
    serde_json::Value::Object(args_map)
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

fn invalid_requested_vc(e: JwtValidationError) -> PresentationVerificationError {
    PresentationVerificationError::InvalidRequestedCredential(
        CredentialVerificationError::InvalidClaims(e),
    )
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
            println!(
                "inconsistent claim [{}] in VC::  expected: {}, actual: {}",
                label, expected, actual
            );
            Err(inconsistent_jwt_claims("inconsistent claim in VC"))
        }
    } else {
        println!("missing claim [{}] in VC", label);
        Err(inconsistent_jwt_claims("missing claim in VC"))
    }
}

fn validate_expiration(
    maybe_expiration_s: Option<i64>,
    current_time_ns: u128,
) -> Result<(), JwtValidationError> {
    if let Some(expiration_s) = maybe_expiration_s {
        let expiration_ns: u128 = (expiration_s * 1_000_000_000)
            .try_into()
            .map_err(|_| JwtValidationError::ExpirationDate)?;
        if expiration_ns > current_time_ns {
            Ok(())
        } else {
            Err(JwtValidationError::ExpirationDate)
        }
    } else {
        Err(JwtValidationError::CredentialStructure(
            JwtVcError::MissingExpirationDate,
        ))
    }
}

// Validates that provided `vc_claims` are consistent and match the given `spec`:
//  - `vc_claims` contain "type"-claim that contains `spec.credential_type`
//  - `vc_claims` contain claim named `spec.credential_type` with arguments that match `spec.arguments`,
//     cf. a convention at https://github.com/dfinity/internet-identity/blob/main/docs/vc-spec.md#recommended-convention-connecting-credential-specification-with-the-returned-credentials
pub fn validate_claims_match_spec(
    vc_claims: &Map<String, Value>,
    spec: &CredentialSpec,
) -> Result<(), JwtValidationError> {
    let credential_type = &spec.credential_type;

    // Check that type-claim contains spec.credential_type.
    let vc_type_entry = vc_claims
        .get("type")
        .ok_or(inconsistent_jwt_claims("missing type-claim"))?;
    let types = vc_type_entry
        .as_array()
        .ok_or(inconsistent_jwt_claims("malformed types-claim"))?;
    if !types.contains(&Value::String(credential_type.clone())) {
        return Err(inconsistent_jwt_claims(
            "missing credential_type in type-claim",
        ));
    };

    // Check that credentialSubject-claim contains spec.credential_type entry with matching arguments.
    let credential_subject = vc_claims
        .get("credentialSubject")
        .ok_or(inconsistent_jwt_claims("missing credentialSubject-claim"))?;
    let subject = Subject::from_json_value(credential_subject.clone())
        .map_err(|_| inconsistent_jwt_claims("malformed credentialSubject-claim"))?;
    let verified_claim_arguments = subject
        .properties
        .get(credential_type)
        .ok_or(inconsistent_jwt_claims("missing credential_type claim"))?
        .as_object()
        .ok_or(inconsistent_jwt_claims(
            "malformed credential_type arguments",
        ))?;
    let spec_arguments_count = spec.arguments.as_ref().map_or(0, |args| args.len());
    if spec_arguments_count != verified_claim_arguments.len() {
        return Err(inconsistent_jwt_claims(
            "wrong number of credential_type arguments",
        ));
    }
    if let Some(spec_arguments) = spec.arguments.as_ref() {
        for (key, value) in spec_arguments.iter() {
            if let Some(v) = verified_claim_arguments.get(key) {
                if value != v {
                    return Err(inconsistent_jwt_claims(
                        "wrong value in credential_type argument",
                    ));
                }
            } else {
                return Err(inconsistent_jwt_claims(
                    "missing key in credential_type arguments",
                ));
            }
        }
    }
    Ok(())
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

/// Extracts and returns raw canister sig public key (without DER-prefix) from the given header.
pub fn get_canister_sig_pk_raw(
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
    use crate::issuer_api::ArgumentValue;
    use assert_matches::assert_matches;
    use canister_sig_util::{extract_raw_root_pk_from_der, IC_ROOT_PK_DER_PREFIX};
    use identity_core::common::Url;
    use identity_credential::presentation::{JwtPresentationOptions, PresentationBuilder};
    use std::collections::HashMap;

    const TEST_IC_ROOT_PK_B64URL: &str = "MIGCMB0GDSsGAQQBgtx8BQMBAgEGDCsGAQQBgtx8BQMCAQNhAK32VjilMFayIiyRuyRXsCdLypUZilrL2t_n_XIXjwab3qjZnpR52Ah6Job8gb88SxH-J1Vw1IHxaY951Giv4OV6zB4pj4tpeY2nqJG77Blwk-xfR1kJkj1Iv-1oQ9vtHw";
    const ALIAS_PRINCIPAL: &str = "s33qc-ctnp5-ubyz4-kubqo-p2tem-he4ls-6j23j-hwwba-37zbl-t2lv3-pae";
    const DAPP_PRINCIPAL: &str = "cpehq-54hef-odjjt-bockl-3ldtg-jqle4-ysi5r-6bfah-v6lsa-xprdv-pqe";
    const ID_ALIAS_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCamxUYzNvSzVRVU9SbUt0T3YyVXBhMnhlQW5vNEJ4RlFFYmY1VWRUSTZlYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIPJBKvDetPwyft_h5oQ0nnxjTE_PNV3PrJy1qwKUrh0LggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIGgDAfUw2gIV3kD2PAwglKvdL6iSQ6qQwFcFPtFnQPTpggRYIBTRAqxOalCVKAHk5WxJ9MVl6VO8uaE3dqwFQzy-KkV_ggRYIPzx8nwybmJ4amD88KZwujDMw22rOmhXTr6RRf38pPV4ggRYIARJcosVL7413JPlfHGUgZQyu2CZ2XFVRnJBCkZSRjROgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwr8xRG17_yzlaO2dIWFTQXGwPVPaAkD4P3hdjJyNmSRKDehzvWAShX03w9q_tZGlCZHRyZWWDAYIEWCAqgobdT9VjwJT-e8ou6LaCrL32vpKdLcqyN9RHVjBthIMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBggRYIFdVX_z6VIe33hrXKtxlLy8EZsv4_GNMMUXVF64TMzDqgwJYIF8bQCq3kGPfDU8PEiRnEmqgwwtHm3iGJeuwTLlxBocCggNA";
    const EXPIRY_NS: u128 = 1620329530 * 1_000_000_000; // from ID_ALIAS_CREDENTIAL_JWS
    const MINUTE_NS: u128 = 60 * 1_000_000_000;
    const CURRENT_TIME_AFTER_EXPIRY_NS: u128 = EXPIRY_NS + MINUTE_NS;
    const CURRENT_TIME_BEFORE_EXPIRY_NS: u128 = EXPIRY_NS - MINUTE_NS;

    const ID_ALIAS_CREDENTIAL_JWS_NO_JWK: &str = "eyJraWQiOiJkaWQ6aWM6aWktY2FuaXN0ZXIiLCJhbGciOiJJY0NzIn0.eyJpc3MiOiJodHRwczovL2ludGVybmV0Y29tcHV0ZXIub3JnL2lzc3VlcnMvaW50ZXJuZXQtaWRlbml0eSIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9pbnRlcm5ldGNvbXB1dGVyLm9yZy9jcmVkZW50aWFsL2ludGVybmV0LWlkZW5pdHkiLCJzdWIiOiJkaWQ6d2ViOmNwZWhxLTU0aGVmLW9kamp0LWJvY2tsLTNsZHRnLWpxbGU0LXlzaTVyLTZiZmFoLXY2bHNhLXhwcmR2LXBxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOndlYjpzMzNxYy1jdG5wNS11Ynl6NC1rdWJxby1wMnRlbS1oZTRscy02ajIzai1od3diYS0zN3pibC10Mmx2My1wYWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBi9nZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwJKAAAAAAAAAAABAYMBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIG3uU_jutBtXB-of0uEA3RkCrcunK6D8QFPtX-gDSwDeggRYINLM_z_MXakw3sDoSiVB5lhRa0uxUB5w6LQQ5phqBX1gggRYIMULjwe1N6XomH10SEyc2r_uc7mGf1aSadeDaid9cUrkggRYIDw__VW2PgWMFp6mK-GmPG-7Fc90q58oK_wjcJ3IrkToggRYIAQTcQAtnxsa93zbfZEZV0f28OhiXL5Wp1OAyDHNI_x4ggRYINkQ8P9zGUvsVi3XbQ2bs6V_3kAiN8UNM6yPgeXfmArEgwGCBFggNVP2WB1Ts90nZG9hyLDaCww4gbhXxtw8R-poiMET62uDAkR0aW1lggNJgLiu1N2JpL4WaXNpZ25hdHVyZVgwqHrYoUsNvSEaSShbW8barx0_ODXD5ZBEl9nKOdkNy_fBmGErE_C7ILbC91_fyZ7CZHRyZWWDAYIEWCB223o-sI97tc3LwJL3LRxQ4If6v_IvfC1fwIGYYQ9vroMCQ3NpZ4MCWCA6UuW6rWVPRqQn_k-pP9kMNe6RKs1gj7QVCsaG4Bx2OYMBgwJYIHszMLDS2VadioIaHajRY5iJzroqMs63lVrs_Uj42j0sggNAggRYICm0w_XxGEw4fDPoYcojCILEi0qdH4-4Zw7klzdaPNOC";
    const TEST_CREDENTIAL_JWS_NO_EXPIRY: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCeUk3dlEyOGVybHFnVjVMck03dTNIOUlaeGVwcUxzQkdnSjFyTldaX0tfQSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJpc3MiOiJodHRwczovL2VtcGxveW1lbnQuaW5mby8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vZW1wbG95bWVudC5pbmZvL2NyZWRlbnRpYWxzLzQyIiwic3ViIjoiZGlkOmljcDp2aGJpYi1tNGhtNi1ocHZ5Yy03cHJkMi1zaWl2by1uYmQ3ci02N281eC1uM2F3aC1xc21xei13em5qZi10cWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIlZlcmlmaWVkRW1wbG95ZWUiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiZW1wbG95ZWVfb2YiOnsiZW1wbG95ZXJJZCI6ImRpZDp3ZWI6ZGZpbml0eS5vcmciLCJlbXBsb3llck5hbWUiOiJERklOSVRZIEZvdW5kYXRpb24ifX19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggq7DruGSK9j0nNpVYlgkE4OtYMHWfxzrqB0D-tTp77umDAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggc8y0K3LKbNnsixDTg2Ux51vwu6b9Kqm2NFykuHVtd06CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggTwA0M58_LFASzZLk1ju6zhwQ6qzeDSZsYyc8Ak-WWGCCBFgg7bPsepWtwANz_eF2pBaMOy-a-UEVj8ojdMRGhxyIODqCBFggEflcBBzJzouB9GoAqyMJiiexVT1w7LIv72CbckA15-SCBFggFtwxSFgot33A2BgPFXCOTj9gM8Z0ORDn-YD1tYNW2wmDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCisy0ljDwwuPOxJn72Y8qqxgxDRgP0srKPvFkEgygNfVHoEGnwseMBdMMrYzIStrNkdHJlZYMBggRYIAvQZNP5TRQHV7AavT2jNGPPLcQBzfQvva5hEybHvbw8gwJDc2lngwJYIHGZW4y0kE1oq6oGYkhXj36h1sNPmG2jwFX6tPGiRkfXgwJYICslyEcSADtGlWLKMBsBJAlXe8en4eGCuE9yuAnuqRBOggNA";
    const TEST_SIGNING_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const TEST_SEED: [u8; 32] = [
        142, 84, 220, 222, 130, 185, 65, 67, 145, 152, 171, 78, 191, 101, 41, 107, 108, 94, 2, 122,
        56, 7, 17, 80, 17, 183, 249, 81, 212, 200, 233, 231,
    ];
    const TEST_CREDENTIAL_JWT: &str = r#"{"iss":"https://employment.info/","nbf":1620328630,"jti":"https://employment.info/credentials/42","sub":"did:icp:igfpm-3fhrp-syqme-4i4xk-o4pgd-5xdh4-fbbgw-jnxm5-bvou4-ljt52-kqe","vc":{"@context":"https://www.w3.org/2018/credentials/v1","type":["VerifiableCredential","VerifiedEmployee"],"credentialSubject":{"employee_of":{"employerId":"did:web:dfinity.org","employerName":"DFINITY Foundation"}}}}"#;

    // Test data used for verifiable presentation tests.
    const AGE_VERIFIER_URL: &str = "https://age_verifier.info/";
    const TEST_ISSUER_SIGNING_CANISTER_ID: &str = "rrkah-fqaaa-aaaaa-aaaaq-cai";
    const ID_ALIAS_FOR_VP: &str = "jkk22-zqdxc-kgpez-6sv2m-5pby4-wi4t2-prmoq-gf2ih-i2qtc-v37ac-5ae";
    const ID_RP_FOR_VP: &str = "p2nlc-3s5ul-lcu74-t6pn2-ui5im-i4a5f-a4tga-e6znf-tnvlh-wkmjs-dqe";
    const ID_ALIAS_VC_FOR_VP_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCMGd6TTVJeXFMYUhyMDhtQTRWd2J5SmRxQTFyRVFUX2xNQnVVbmN5UDVVYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOnAybmxjLTNzNXVsLWxjdTc0LXQ2cG4yLXVpNWltLWk0YTVmLWE0dGdhLWU2em5mLXRudmxoLXdrbWpzLWRxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDpqa2syMi16cWR4Yy1rZ3Blei02c3YybS01cGJ5NC13aTR0Mi1wcm1vcS1nZjJpaC1pMnF0Yy12MzdhYy01YWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1gg4TNRYHd-8aiik7eFIFlUffIauc26_S_Cbfu5lrH6WG-CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFgg_KZ0TVqubo_EGWoMUPA35BYZ4B5ZRkR_zDfNIQCwa46CBFggQ32YxfhviZ3hMsTqcHLG7NvrNFYPeR86ayjF9kSIHR6CBFgg9MK-22EcX2HLbVDFBwSSVfBEi0MJrir_V8D4mSrs8W2CBFgguVqxODxxmD13JWUJh9h0fLIZ5knKseqhi6RzHTwFOeCCBFgggnfiF6BqnRcJrN4b5QOatcBHi7CoNcEnqJQpdgTFewWDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCjAL-A0R8pOvIRouUEI_zmS1PElQ1g7sJKgMb--60Tw399DUME-pKPFCEWQSpqEWFkdHJlZYMBggRYICk50o-r4OkMNAjh7eU61FNy0ARr-X3mLJdhUWMQAMr7gwJDc2lngwJYIIOQR7wl3Ws9Jb8VP4rhIb37XKLMkkZ2P7WaZ5we60WGgwGCBFggsvWfw_9l6MdknYq3zHvk8WyFan5Rxsb-dfI-p_IA_g6DAlgg_OG2VoT11h2BQfYTnf83Y9blXTdGnmKGRe3SJAvV8h2CA0A";
    const VERIFIED_ADULT_VC_FOR_VP_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCMzdLQ29yYjQ2OUVFZ19uYzVvTFI5RHNoSy1SOEhXUUNMN05VMDcwa1R0WSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vYWdlX3ZlcmlmaWVyLmluZm8vIiwibmJmIjoxNjIwMzI4NjMwLCJqdGkiOiJodHRwczovL2FnZV92ZXJpZmllci5pbmZvL2NyZWRlbnRpYWxzLzQyIiwic3ViIjoiZGlkOmljcDpqa2syMi16cWR4Yy1rZ3Blei02c3YybS01cGJ5NC13aTR0Mi1wcm1vcS1nZjJpaC1pMnF0Yy12MzdhYy01YWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIlZlcmlmaWVkQWR1bHQiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiVmVyaWZpZWRBZHVsdCI6eyJtaW5BZ2UiOjE4fX19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggt424dJpC6sjdhsBc8wD1SOiv5OM8gAKL00RP3F_1Wz6DAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggZDN-5FgR1a-r1vFcUM-_aSwM9WBkrfG-PjnkpDfd_a6CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFgg7KwVkmHXh_bkpjrNamLjBaVHnx8ygOov12v70RCCnauCBFgg4JfM61UN0ijoPEaD4V4vo3MD34QmoPRJ7NgRMR94NAKCBFggsSTfdsBa0-yHxDCqvaUlOO6rC4ipHUqkr1P3AAvz1KmCBFggACrZEhXi64qQOi0WG2dobr3wU-nz1EYrzkHUnKwm2euDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCDsLi8fdzb5mhTyUFJZUrsWcnfsriPBHFpPemY723yTxgIEExP76d4fzMu7xWXtqZkdHJlZYMBggRYIF8C4f7jFXIgbB8sfB9qSnjcDPIah96xJMJ-lkUV3es5gwJDc2lngwJYIPL9DVTa-kaJmcs0Khp5uNdi4aZ5Dnt_kCORxF48UNPjgwGCBFggH7DURumpKFxQSDZEQz_Z4OgXTAqhI4PT_qJtGsQwcL-DAYMCWCA87aI24qiaIQsjHtFMJRESwnEV9Ukj8lAJ3b1CaT84joIDQIIEWCAkNVdgMuWn2EpNNTPe7jaX3h93D4ijH2Sg8oRYhXBwDQ";
    const VERIFIED_EMPLOYEE_VC_FOR_VP_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCMzdLQ29yYjQ2OUVFZ19uYzVvTFI5RHNoSy1SOEhXUUNMN05VMDcwa1R0WSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vZW1wbG95bWVudC5pbmZvLyIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9lbXBsb3ltZW50LmluZm8vY3JlZGVudGlhbHMvNDIiLCJzdWIiOiJkaWQ6aWNwOmprazIyLXpxZHhjLWtncGV6LTZzdjJtLTVwYnk0LXdpNHQyLXBybW9xLWdmMmloLWkycXRjLXYzN2FjLTVhZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiVmVyaWZpZWRFbXBsb3llZSJdLCJjcmVkZW50aWFsU3ViamVjdCI6eyJWZXJpZmllZEVtcGxveWVlIjp7ImVtcGxveWVySWQiOiJkaWQ6d2ViOmRmaW5pdHkub3JnIiwiZW1wbG95ZXJOYW1lIjoiREZJTklUWSBGb3VuZGF0aW9uIn19fX0.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggt424dJpC6sjdhsBc8wD1SOiv5OM8gAKL00RP3F_1Wz6DAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggD8f87lse4wbqTJzVzzHL0KLYglxk_3Bqu_L9BYW1_82CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFgg7KwVkmHXh_bkpjrNamLjBaVHnx8ygOov12v70RCCnauCBFgg4JfM61UN0ijoPEaD4V4vo3MD34QmoPRJ7NgRMR94NAKCBFggldPzhEqB2ndq8dydYB14vltPlqh94grYUzSUEDT8-jeCBFgg4EA8lx4d8URjaaYFi_87OeQ1dcAcz4fOFXAjLqWWs22DAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCPZahVmdM4-1Y-c-Pzn701gIIVShPPcWD3_ZFJggWwiShD5Fq8i0ZPSC5YJqZ8d5xkdHJlZYMBggRYIF8C4f7jFXIgbB8sfB9qSnjcDPIah96xJMJ-lkUV3es5gwJDc2lngwJYIPL9DVTa-kaJmcs0Khp5uNdi4aZ5Dnt_kCORxF48UNPjgwJYIOotDCfm4T5b3qxy9vMUmKVHMantmISW7lOuF2fwoO9tggNA";

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

    pub(crate) fn create_verifiable_presentation_jwt_for_test(
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
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        )
        .expect("JWS verification failed");
    }

    #[test]
    fn should_fail_verify_credential_jws_if_expired() {
        let result = verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS,
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
            CURRENT_TIME_AFTER_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if e.to_string().contains("credential expired"));
    }

    #[test]
    fn should_fail_verify_credential_jws_if_no_expiry() {
        let result = verify_credential_jws_with_canister_id(
            TEST_CREDENTIAL_JWS_NO_EXPIRY,
            &test_issuer_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
            CURRENT_TIME_AFTER_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if e.to_string().contains("structure is not semantically correct"));
    }

    #[test]
    fn should_fail_verify_credential_jws_without_canister_pk() {
        let result = verify_credential_jws_with_canister_id(
            ID_ALIAS_CREDENTIAL_JWS_NO_JWK,
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
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
            CURRENT_TIME_BEFORE_EXPIRY_NS,
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
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if  { let err_msg = e.to_string();
            err_msg.contains("invalid signature") &&
            err_msg.contains("Malformed ThresBls12_381 public key") });
    }

    #[test]
    fn should_verify_and_extract_id_alias_credential_jws() {
        let alias_tuple = get_verified_id_alias_from_jws(
            ID_ALIAS_CREDENTIAL_JWS,
            &dapp_principal(),
            &test_canister_sig_pk().canister_id,
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
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

    fn default_test_vc_flow_signers() -> VcFlowSigners {
        VcFlowSigners {
            ii_canister_id: test_canister_sig_pk().canister_id,
            ii_origin: II_ISSUER_URL.to_string(),
            issuer_canister_id: test_issuer_canister_sig_pk().canister_id,
            issuer_origin: AGE_VERIFIER_URL.to_string(),
        }
    }

    #[test]
    fn should_verify_ii_presentation() {
        let id_alias = Principal::from_text(ID_ALIAS_FOR_VP).expect("wrong principal");
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let (alias_tuple_from_jws, _claims) = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        )
        .expect("vp verification failed");
        assert_eq!(id_alias, alias_tuple_from_jws.id_alias);
        assert_eq!(id_dapp, alias_tuple_from_jws.id_dapp);
    }

    #[test]
    fn should_fail_verify_ii_presentation_if_expired() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_AFTER_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("credential expired"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_extra_vc() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
                "an extra vc".to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("expected exactly two verifiable credentials"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_missing_vc() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![ID_ALIAS_VC_FOR_VP_JWS.to_string()],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("expected exactly two verifiable credentials"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_effective_subject() {
        let wrong_subject = dapp_principal(); // does not match ID_ALIAS_VC_FOR_VP_JWS
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            wrong_subject,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            wrong_subject,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("unexpected vc subject"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_non_matching_id_alias_in_vcs() {
        let id_dapp = dapp_principal(); // does match ID_ALIAS_CREDENTIAL_JWS

        // ID_ALIAS_CREDENTIAL_JWS does not match REQUESTED_VC_FOR_VP_JWS
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("subject does not match id_alias"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_invalid_id_alias_vc() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let mut bad_id_alias_vc = ID_ALIAS_VC_FOR_VP_JWS.to_string();
        bad_id_alias_vc.insert(42, 'a');
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![bad_id_alias_vc, VERIFIED_ADULT_VC_FOR_VP_JWS.to_string()],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("InvalidSignature"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_invalid_requested_vc() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let mut bad_requested_vc = VERIFIED_ADULT_VC_FOR_VP_JWS.to_string();
        bad_requested_vc.insert(42, 'a');
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![ID_ALIAS_VC_FOR_VP_JWS.to_string(), bad_requested_vc],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("InvalidSignature"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_ii_canister_id() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                ii_canister_id: test_issuer_canister_sig_pk().canister_id,
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("canister id does not match"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_issuer_canister_id() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                issuer_canister_id: test_canister_sig_pk().canister_id,
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("canister id does not match"));
    }

    #[test]
    fn should_fail_verify_ii_presentation_with_wrong_order_of_vcs() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");

        // Swap the order of the VCs
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp creation failed");
        let result = verify_ii_presentation_jwt_with_canister_ids(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                // Swap also the order of the canister ids, so that they match the VCs
                ii_canister_id: test_issuer_canister_sig_pk().canister_id,
                issuer_canister_id: test_canister_sig_pk().canister_id,
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).contains("inconsistent claim in VC"));
    }

    fn credential_spec_with_0_args() -> CredentialSpec {
        CredentialSpec {
            credential_type: "vcWithoutArgs".to_string(),
            arguments: None,
        }
    }

    fn credential_spec_with_1_arg() -> CredentialSpec {
        let mut args = HashMap::new();
        args.insert(
            "firstArg".to_string(),
            ArgumentValue::String("string arg value".to_string()),
        );
        CredentialSpec {
            credential_type: "vcWithOneArg".to_string(),
            arguments: Some(args),
        }
    }

    fn credential_spec_with_2_args() -> CredentialSpec {
        let mut args = HashMap::new();
        args.insert(
            "anotherFirstArg".to_string(),
            ArgumentValue::String("string arg value".to_string()),
        );
        args.insert("secondArg".to_string(), ArgumentValue::Int(42));
        CredentialSpec {
            credential_type: "vcWithTwoArgs".to_string(),
            arguments: Some(args),
        }
    }

    fn credential_specs_for_test() -> Vec<CredentialSpec> {
        vec![
            credential_spec_with_0_args(),
            credential_spec_with_1_arg(),
            credential_spec_with_2_args(),
        ]
    }

    fn vc_claims_for_spec(spec: &CredentialSpec) -> Map<String, Value> {
        let mut claims = Map::new();
        let types = vec![
            Value::String("VerifiableCredential".to_string()),
            Value::String(spec.credential_type.to_string()),
        ];
        claims.insert("type".to_string(), Value::Array(types));
        let mut arguments = Map::new();
        if let Some(args) = spec.arguments.as_ref() {
            for arg in args {
                arguments.insert(arg.0.clone(), arg.1.clone().into());
            }
        }
        let mut subject = Map::new();
        subject.insert(spec.credential_type.clone(), Value::Object(arguments));
        claims.insert("credentialSubject".to_string(), Value::Object(subject));
        claims
    }

    #[test]
    fn should_validate_claims_match_spec() {
        for spec in credential_specs_for_test() {
            let claims = vc_claims_for_spec(&spec);
            validate_claims_match_spec(&claims, &spec)
                .unwrap_or_else(|_| panic!("failed for spec: {:?}", spec));
        }
    }

    #[test]
    fn should_fail_validate_claims_match_spec_if_wrong_type() {
        for spec in credential_specs_for_test() {
            // Construct claims with wrong "type" entry.
            let mut claims = vc_claims_for_spec(&spec);
            claims.insert(
                "type".to_string(),
                Value::Array(vec![Value::String("WrongType".to_string())]),
            );
            let result = validate_claims_match_spec(&claims, &spec);
            assert_matches!(result, Err(e) if format!("{:?}", e).contains("missing credential_type in type-claim"));
        }
    }

    #[test]
    fn should_fail_validate_claims_match_spec_if_missing_credential_type_claim() {
        for spec in credential_specs_for_test() {
            // Construct claims without "credential_type"-claim.
            let mut claims = vc_claims_for_spec(&spec);
            claims
                .get_mut("credentialSubject")
                .expect("missing credentialSubject")
                .as_object_mut()
                .expect("wrong credentialSubject")
                .remove(&spec.credential_type)
                .expect("missing credential_type claim");
            let result = validate_claims_match_spec(&claims, &spec);
            assert_matches!(result, Err(e) if format!("{:?}", e).contains("missing credential_type claim"));
        }
    }

    #[test]
    fn should_fail_validate_claims_match_spec_with_extra_args_in_credential_type_claim() {
        for spec in credential_specs_for_test() {
            // Construct claims with extra arg in "credential_type"-claim.
            let mut claims = vc_claims_for_spec(&spec);
            claims
                .get_mut("credentialSubject")
                .expect("missing credentialSubject")
                .as_object_mut()
                .expect("wrong credentialSubject")
                .get_mut(&spec.credential_type)
                .expect("missing credential_type claim")
                .as_object_mut()
                .expect("wrong credential_type claim")
                .insert("extraArg".to_string(), Value::Null);
            let result = validate_claims_match_spec(&claims, &spec);
            assert_matches!(result, Err(e) if format!("{:?}", e).contains("wrong number of credential_type arguments"));
        }
    }

    #[test]
    fn should_fail_validate_claims_match_spec_with_missing_args_in_credential_type_claim() {
        for spec in [credential_spec_with_1_arg(), credential_spec_with_2_args()] {
            // Construct claims with extra arg in "credential_type"-claim.
            let mut claims = vc_claims_for_spec(&spec);
            let arg_name = spec.arguments.as_ref().unwrap().keys().last().unwrap();
            claims
                .get_mut("credentialSubject")
                .expect("missing credentialSubject")
                .as_object_mut()
                .expect("wrong credentialSubject")
                .get_mut(&spec.credential_type)
                .expect("missing credential_type claim")
                .as_object_mut()
                .expect("wrong credential_type claim")
                .remove(arg_name);
            let result = validate_claims_match_spec(&claims, &spec);
            assert_matches!(result, Err(e) if format!("{:?}", e).contains("wrong number of credential_type arguments"));
        }
    }

    #[test]
    fn should_fail_validate_claims_match_spec_with_wrong_arg_value_in_credential_type_claim() {
        for spec in [credential_spec_with_1_arg(), credential_spec_with_2_args()] {
            // Construct claims with extra arg in "credential_type"-claim.
            let mut claims = vc_claims_for_spec(&spec);
            let arg_name = spec.arguments.as_ref().unwrap().keys().last().unwrap();
            claims
                .get_mut("credentialSubject")
                .expect("missing credentialSubject")
                .as_object_mut()
                .expect("wrong credentialSubject")
                .get_mut(&spec.credential_type)
                .expect("missing credential_type claim")
                .as_object_mut()
                .expect("wrong credential_type claim")
                .insert(arg_name.clone(), Value::String("a wrong value".to_string()));
            let result = validate_claims_match_spec(&claims, &spec);
            assert_matches!(result, Err(e) if format!("{:?}", e).contains("wrong value in credential_type argument"));
        }
    }

    fn verified_adult_vc_spec() -> CredentialSpec {
        let mut args = HashMap::new();
        args.insert("minAge".to_string(), ArgumentValue::Int(18));
        CredentialSpec {
            credential_type: "VerifiedAdult".to_string(),
            arguments: Some(args),
        }
    }

    #[test]
    fn should_validate_ii_presentation_and_claims() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        )
        .expect("VP verification failed");
    }

    #[test]
    fn should_fail_validate_ii_presentation_and_claims_if_wrong_vc_flow_signers() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");

        // wrong ii_canister_id
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                ii_canister_id: test_issuer_canister_sig_pk().canister_id,
                ..default_test_vc_flow_signers()
            },
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InvalidSignature"));

        // wrong issuer_canister_id
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                issuer_canister_id: test_canister_sig_pk().canister_id,
                ..default_test_vc_flow_signers()
            },
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InvalidSignature"));

        // wrong issuer_origin
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &VcFlowSigners {
                issuer_origin: "https://wrong.origin.com".to_string(),
                ..default_test_vc_flow_signers()
            },
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InconsistentCredentialJwtClaims"));
    }

    #[test]
    fn should_fail_validate_ii_presentation_and_claims_if_wrong_effective_subject() {
        let id_alias = Principal::from_text(ID_ALIAS_FOR_VP).expect("wrong principal");
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_alias, // wrong effective subject
            &default_test_vc_flow_signers(),
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("unexpected vc subject"));
    }

    #[test]
    fn should_fail_validate_ii_presentation_and_claims_if_expired() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_ADULT_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_AFTER_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("credential expired"));
    }

    #[test]
    fn should_fail_validate_ii_presentation_and_claims_if_wrong_vcs() {
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            id_dapp,
            vec![
                ID_ALIAS_VC_FOR_VP_JWS.to_string(),
                VERIFIED_EMPLOYEE_VC_FOR_VP_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        let result = validate_ii_presentation_and_claims(
            &vp_jwt,
            id_dapp,
            &default_test_vc_flow_signers(),
            &verified_adult_vc_spec(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InconsistentCredentialJwtClaims"));
    }

    // Removes nbf-entry from the given VC-JWT.
    fn remove_nbf(vc_jwt: &str) -> String {
        let mut ret = vc_jwt.to_string();
        let nbf_start = vc_jwt.find("\"nbf\"").unwrap();
        let nbf_end = vc_jwt.find("\"jti\"").unwrap();
        ret.replace_range(nbf_start..nbf_end, "");
        ret
    }

    #[test]
    fn should_build_credential_jwt() {
        let example_jwt = "{\"exp\":1620329470,\"iss\":\"https://age_verifier.info/\",\"nbf\":1707817485,\"jti\":\"https://age_verifier.info/credentials/42\",\"sub\":\"did:icp:p2nlc-3s5ul-lcu74-t6pn2-ui5im-i4a5f-a4tga-e6znf-tnvlh-wkmjs-dqe\",\"vc\":{\"@context\":\"https://www.w3.org/2018/credentials/v1\",\"type\":[\"VerifiableCredential\",\"VerifiedAdult\"],\"credentialSubject\":{\"VerifiedAdult\":{\"minAge\":18}}}}";
        let example_jwt_without_nbf = "{\"exp\":1620329470,\"iss\":\"https://age_verifier.info/\",\"jti\":\"https://age_verifier.info/credentials/42\",\"sub\":\"did:icp:p2nlc-3s5ul-lcu74-t6pn2-ui5im-i4a5f-a4tga-e6znf-tnvlh-wkmjs-dqe\",\"vc\":{\"@context\":\"https://www.w3.org/2018/credentials/v1\",\"type\":[\"VerifiableCredential\",\"VerifiedAdult\"],\"credentialSubject\":{\"VerifiedAdult\":{\"minAge\":18}}}}";
        let id_dapp = Principal::from_text(ID_RP_FOR_VP).expect("wrong principal");
        let params = CredentialParams {
            spec: verified_adult_vc_spec(),
            subject_id: did_for_principal(id_dapp),
            credential_id_url: "https://age_verifier.info/credentials/42".to_string(),
            issuer_url: "https://age_verifier.info".to_string(),
            expiration_timestamp_s: (CURRENT_TIME_BEFORE_EXPIRY_NS / 1_000_000_000) as u32,
        };
        let credential = build_credential_jwt(params);
        assert_eq!(credential.len(), example_jwt.len());
        assert_ne!(credential, example_jwt);
        // build credential should differ from the expected one only by the nbf-entry.
        assert_eq!(remove_nbf(credential.as_str()), example_jwt_without_nbf);
    }
}
