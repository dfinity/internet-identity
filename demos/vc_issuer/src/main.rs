use candid::{candid_method, Principal};
use ic_cdk_macros::{query, update};
use ic_certified_map::Hash;
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use identity_jose::jwk::{Jwk, JwkParams, JwkParamsOct, JwkType};
use identity_jose::jws::{CompactJwsEncoder, JwsAlgorithm, JwsHeader};
use identity_jose::jwu::encode_b64;
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ConsentInfo, ConsentMessageRequest, ConsentMessageResponse, GetCredentialRequest,
    GetCredentialResponse, IssuedCredentialData, PrepareCredentialRequest,
    PrepareCredentialResponse, PreparedCredentialData,
};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::fmt::Error;
use std::ops::{Add, DerefMut};

#[update]
#[candid_method]
async fn prepare_credential(req: PrepareCredentialRequest) -> PrepareCredentialResponse {
    if let Err(err) = verify_prepare_credential_request(&req) {
        return PrepareCredentialResponse::Err(err.to_string());
    }
    let user = req.signed_id_alias.id_dapp;
    let seed = calculate_seed(&user);
    let canister_sig_pk_der = der_encoded_canister_sig_pk(&seed);
    let credential = new_credential(&user);
    let credential_jwt = credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure");
    let encoder = jws_encoder(&credential_jwt, &canister_sig_pk_der);
    let _msg_hash = vc_signing_input_hash(encoder.signing_input());
    // TODO: add the (seed, msg_hash) to the canister sigs in certified data.
    PrepareCredentialResponse::Ok(PreparedCredentialData {
        vc_jwt: credential_jwt,
    })
}

#[query]
#[candid_method(query)]
fn get_credential(req: GetCredentialRequest) -> GetCredentialResponse {
    if let Err(err) = verify_get_credential_request(&req) {
        return GetCredentialResponse::Err(err.to_string());
    }
    // TODO: retrieve the actual signature and add it to the JWS.
    let user = req.signed_id_alias.id_dapp;
    let seed = calculate_seed(&user);
    let canister_sig_pk_der = der_encoded_canister_sig_pk(&seed);
    let encoder = jws_encoder(&req.vc_jwt, &canister_sig_pk_der);
    let sig = b"dummy sig";
    let vc_jws = encoder.into_jws(sig);
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[query]
#[candid_method]
async fn consent_message(req: ConsentMessageRequest) -> ConsentMessageResponse {
    ConsentMessageResponse::Valid(ConsentInfo {
        consent_message: "Do you want to get a verifiable credential?".to_string(),
        language: req.preferences.language,
    })
}

fn main() {}

// TODO: move the helpers to an util-crate.
pub fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
}

fn calculate_seed(principal: &Principal) -> Hash {
    let dummy_salt = [5u8; 32];

    let mut bytes: Vec<u8> = vec![];
    bytes.push(dummy_salt.len() as u8);
    bytes.extend_from_slice(&dummy_salt);

    let principal_bytes = principal.as_slice();
    bytes.push(principal_bytes.len() as u8);
    bytes.extend(principal_bytes);
    hash_bytes(bytes)
}

fn vc_signing_input_hash(signing_input: &[u8]) -> Hash {
    let sep = b"iccs_verifiable_credential";
    let mut hasher = Sha256::new();
    let buf = [sep.len() as u8];
    hasher.update(buf);
    hasher.update(sep);
    hasher.update(signing_input);
    hasher.finalize().into()
}

pub fn der_encoded_canister_sig_pk(seed: &[u8]) -> Vec<u8> {
    let my_canister_id: Vec<u8> = ic_cdk::id().as_ref().to_vec();

    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(my_canister_id.len() as u8);
    bitstring.extend(my_canister_id);
    bitstring.extend(seed);

    let mut der: Vec<u8> = vec![];
    // sequence of length 17 + the bit string length
    der.push(0x30);
    der.push(17 + bitstring.len() as u8);
    der.extend(vec![
        // sequence of length 12 for the OID
        0x30, 0x0C, // OID 1.3.6.1.4.1.56387.1.2
        0x06, 0x0A, 0x2B, 0x06, 0x01, 0x04, 0x01, 0x83, 0xB8, 0x43, 0x01, 0x02,
    ]);
    // BIT string of given length
    der.push(0x03);
    der.push(1 + bitstring.len() as u8);
    der.push(0x00);
    der.extend(bitstring);
    der
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

fn jws_encoder<'a>(credential_jwt: &'a str, canister_sig_pk_der: &[u8]) -> CompactJwsEncoder<'a> {
    let mut header: JwsHeader = JwsHeader::new();
    header.set_alg(JwsAlgorithm::IcCs);
    let kid = did_from_principal(&ic_cdk::id());
    header.set_kid(kid);
    header
        .deref_mut()
        .set_jwk(canister_sig_pk_jwk(canister_sig_pk_der));

    let encoder: CompactJwsEncoder = CompactJwsEncoder::new(credential_jwt.as_ref(), &header)
        .expect("internal error: JWS encoder failed");
    encoder
}

fn did_from_principal(principal: &Principal) -> String {
    let prefix = String::from("did:icp:");
    prefix.add(&*principal.to_string())
}

fn new_credential(principal: &Principal) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_from_principal(principal),
      "name": "Alice",
      "degree": {
        "type": "BachelorDegree",
        "name": "Bachelor of Science and Arts",
      },
      "GPA": "4.0",
    }))
    .unwrap();

    // Build credential using subject above and issuer.
    CredentialBuilder::default()
        .id(Url::parse("https://example.edu/credentials/3732").unwrap())
        .issuer(Url::parse("https://identity.ic0.app").unwrap())
        .type_("UniversityDegreeCredential")
        .subject(subject)
        .build()
        .unwrap()
}

fn verify_prepare_credential_request(_req: &PrepareCredentialRequest) -> Result<(), Error> {
    Ok(())
}

fn verify_get_credential_request(_req: &GetCredentialRequest) -> Result<(), Error> {
    Ok(())
}

// Order dependent: do not move above any function annotated with #[candid_method]!
candid::export_service!();

#[cfg(test)]
mod test {
    use crate::__export_service;

    #[test]
    fn print_candid_interface() {
        let canister_interface = __export_service();
        println!("{}", canister_interface);
    }
}
