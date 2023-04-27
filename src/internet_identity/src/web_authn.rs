use base64::engine::general_purpose::URL_SAFE_NO_PAD as BASE64;
use base64::Engine;
use der::{
    asn1::{AnyRef, BitStringRef, ObjectIdentifier},
    Decode, Sequence,
};
use ecdsa::signature::Verifier;
use ecdsa::VerifyingKey;
use ecdsa::{EncodedPoint, Signature};
use ic_cdk::api::instruction_counter;
use ic_cdk::trap;
use internet_identity_interface::internet_identity::types::PublicKey;
use p256::elliptic_curve::generic_array::GenericArray;
use p256::NistP256;
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use sha2::Digest;
use sha2::Sha256;

// see https://tools.ietf.org/html/rfc8152 section 8.1
#[allow(unused)]
const COSE_PARAM_KTY: serde_cbor::Value = serde_cbor::Value::Integer(1);
#[allow(unused)]
const COSE_PARAM_ALG: serde_cbor::Value = serde_cbor::Value::Integer(3);
#[allow(unused)]
const COSE_PARAM_KEY_OPS: serde_cbor::Value = serde_cbor::Value::Integer(4);

// see https://datatracker.ietf.org/doc/html/rfc8152#section-13
#[allow(unused)]
const COSE_KTY_EC2: serde_cbor::Value = serde_cbor::Value::Integer(2);
const COSE_PARAM_EC2_CRV: serde_cbor::Value = serde_cbor::Value::Integer(-1);
const COSE_PARAM_EC2_X: serde_cbor::Value = serde_cbor::Value::Integer(-2);
const COSE_PARAM_EC2_Y: serde_cbor::Value = serde_cbor::Value::Integer(-3);

// see https://datatracker.ietf.org/doc/html/rfc8152#section-8.1 and
// https://datatracker.ietf.org/doc/html/rfc8152#section-13.1
#[allow(unused)]
const COSE_ALG_ES256: serde_cbor::Value = serde_cbor::Value::Integer(-7);
const COSE_EC2_CRV_P256: serde_cbor::Value = serde_cbor::Value::Integer(1);

// https://datatracker.ietf.org/doc/html/rfc8812#section-2
#[allow(unused)]
const COSE_ALG_RS256: serde_cbor::Value = serde_cbor::Value::Integer(-257);

// https://datatracker.ietf.org/doc/html/rfc8230#section-4
#[allow(unused)]
const COSE_KTY_RSA: serde_cbor::Value = serde_cbor::Value::Integer(3);
#[allow(unused)]
const COSE_PARAM_RSA_N: serde_cbor::Value = serde_cbor::Value::Integer(-1);
#[allow(unused)]
const COSE_PARAM_RSA_E: serde_cbor::Value = serde_cbor::Value::Integer(-2);

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
struct ClientData {
    r#type: String,
    challenge: String,
    origin: String,
}

/// Verification of a signature that was generated with web authentication
/// requires as auxiliary information the AuthenticatorData and the
/// ClientDataJSON objects returned by the call to the authenticator. A
/// WebAuthnSignature contains both the actual cryptographic signature
/// and this auxiliary information.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WebAuthnSignature {
    authenticator_data: ByteBuf,
    client_data_json: ByteBuf,
    signature: ByteBuf,
}

impl WebAuthnSignature {
    pub fn signed_bytes(&self) -> Vec<u8> {
        let mut signed_bytes: Vec<u8> = self.authenticator_data.clone().into_vec();

        let mut hasher = Sha256::new();
        hasher.update(&self.client_data_json);
        let hash: [u8; 32] = hasher.finalize().into();
        signed_bytes.extend_from_slice(&hash);

        signed_bytes
    }
}

/// X.509 `AlgorithmIdentifier`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Sequence)]
pub struct AlgorithmIdentifier<'a> {
    /// This field contains an ASN.1 `OBJECT IDENTIFIER`, a.k.a. OID.
    pub algorithm: ObjectIdentifier,

    /// This field is `OPTIONAL` and contains the ASN.1 `ANY` type, which
    /// in this example allows arbitrary algorithm-defined parameters.
    pub parameters: Option<AnyRef<'a>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Sequence)]
pub struct SubjectPublicKeyInfo<'a> {
    /// X.509 `AlgorithmIdentifier`
    pub algorithm: AlgorithmIdentifier<'a>,

    /// Public key data
    pub subject_public_key: BitStringRef<'a>,
}

pub fn check_webauthn_signature(
    challenge: &[u8],
    signature_bytes: &[u8],
    public_key_bytes: &PublicKey,
) -> Result<(), ()> {
    let start_counter = instruction_counter();
    let webauthn_sig: WebAuthnSignature =
        serde_cbor::from_slice(signature_bytes).map_err(|_| ())?;

    verify_challenge(challenge, &webauthn_sig);
    verify_signature(public_key_bytes, &webauthn_sig);

    let end_counter = instruction_counter();
    ic_cdk::println!(
        "signature verified in {} cycles",
        end_counter - start_counter
    );
    Ok(())
}

fn verify_challenge(challenge: &[u8], webauthn_sig: &WebAuthnSignature) {
    let client_data: ClientData = serde_json::from_slice(&webauthn_sig.client_data_json).unwrap();
    let challenge_bytes = BASE64.decode(&client_data.challenge).unwrap();

    if challenge != challenge_bytes {
        trap(&format!(
            "invalid challenge: expected {:?}, got {:?}",
            BASE64.encode(challenge),
            client_data.challenge
        ));
    }
}

fn verify_signature(public_key_bytes: &PublicKey, webauthn_sig: &WebAuthnSignature) {
    let signature: Signature<NistP256> =
        Signature::<NistP256>::from_der(&webauthn_sig.signature).unwrap();

    let verifying_key = verifying_key_from_der(public_key_bytes);
    verifying_key
        .verify(&webauthn_sig.signed_bytes(), &signature)
        .expect("invalid signature!");
}

fn verifying_key_from_der(public_key_bytes: &PublicKey) -> VerifyingKey<NistP256> {
    let pubkey_info = SubjectPublicKeyInfo::from_der(public_key_bytes).unwrap();
    let parsed_value: serde_cbor::value::Value =
        serde_cbor::from_slice(pubkey_info.subject_public_key.raw_bytes()).unwrap();
    let serde_cbor::Value::Map(fields) = parsed_value else { trap("unsupported public key") };

    let crv = fields.get(&COSE_PARAM_EC2_CRV).unwrap();

    if *crv != COSE_EC2_CRV_P256 {
        // Some ECDSA we don't support
        trap("unsupported public key");
    }

    let x = fields.get(&COSE_PARAM_EC2_X).unwrap();
    let serde_cbor::Value::Bytes(x_bytes) = x else {
        trap("unsupported public key");
    };
    let y = fields.get(&COSE_PARAM_EC2_Y).unwrap();
    let serde_cbor::Value::Bytes(y_bytes) = y else {
        trap("unsupported public key");
    };
    let point: EncodedPoint<NistP256> = EncodedPoint::<NistP256>::from_affine_coordinates(
        GenericArray::from_slice(x_bytes),
        GenericArray::from_slice(y_bytes),
        false,
    );
    VerifyingKey::from_encoded_point(&point).unwrap()
}
