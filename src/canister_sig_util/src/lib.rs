use candid::Principal;
use ic_certified_map::{Hash, HashTree};
use identity_jose::jwk::{Jwk, JwkParams, JwkParamsOct, JwkType};
use identity_jose::jws::{CompactJwsEncoder, JwsAlgorithm, JwsHeader};
use identity_jose::jwu::encode_b64;
use serde::Serialize;
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::ops::{Add, DerefMut};

pub mod signature_map;
pub use identity_jose::jws::verify_credential_jws;

#[derive(Serialize)]
pub struct CanisterSig<'a> {
    pub certificate: ByteBuf,
    pub tree: HashTree<'a>,
}

pub fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
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

pub fn der_encoded_canister_sig_pk(canister_id: Principal, seed: &[u8]) -> Vec<u8> {
    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(canister_id.as_ref().len() as u8);
    bitstring.extend(canister_id.as_ref());
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

pub fn did_for_principal(principal: Principal) -> String {
    let prefix = String::from("did:icp:");
    prefix.add(&principal.to_string())
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
