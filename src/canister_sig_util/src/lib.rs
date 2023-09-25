use candid::Principal;
use ic_certification::{Certificate, Delegation, HashTree, LookupResult};
use ic_certified_map::Hash;
use ic_verify_bls_signature::verify_bls_signature;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::fmt::{Display, Formatter};
use std::sync::RwLock;

pub mod signature_map;

pub const IC_STATE_ROOT_DOMAIN_SEPARATOR: &[u8; 14] = b"\x0Dic-state-root";
pub const IC_ROOT_KEY_DER_PREFIX: &[u8; 37] = b"\x30\x81\x82\x30\x1d\x06\x0d\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x01\x02\x01\x06\x0c\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x02\x01\x03\x61\x00";
pub const IC_ROOT_KEY_DER: &[u8; 133] = b"\x30\x81\x82\x30\x1d\x06\x0d\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x01\x02\x01\x06\x0c\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x02\x01\x03\x61\x00\x81\x4c\x0e\x6e\xc7\x1f\xab\x58\x3b\x08\xbd\x81\x37\x3c\x25\x5c\x3c\x37\x1b\x2e\x84\x86\x3c\x98\xa4\xf1\xe0\x8b\x74\x23\x5d\x14\xfb\x5d\x9c\x0c\xd5\x46\xd9\x68\x5f\x91\x3a\x0c\x0b\x2c\xc5\x34\x15\x83\xbf\x4b\x43\x92\xe4\x67\xdb\x96\xd6\x5b\x9b\xb4\xcb\x71\x71\x12\xf8\x47\x2e\x0d\x5a\x4d\x14\x50\x5f\xfd\x74\x84\xb0\x12\x91\x09\x1c\x5f\x87\xb9\x88\x83\x46\x3f\x98\x09\x1a\x0b\xaa\xae";
pub const IC_ROOT_KEY_LENGTH: usize = 96;

pub const CANISTER_KEY_DER_PREFIX_LENGTH: usize = 19;
// Canister signatures' public key OID is 1.3.6.1.4.1.56387.1.2,
// cf. https://internetcomputer.org/docs/current/references/ic-interface-spec#canister-signatures
pub const CANISTER_KEY_DER_OID: &[u8; 14] =
    b"\x30\x0C\x06\x0A\x2B\x06\x01\x04\x01\x83\xB8\x43\x01\x02";

lazy_static! {
    static ref IC_ROOT_PUBLIC_KEY: RwLock<Vec<u8>> = RwLock::new(
        extract_ic_root_key_from_der(IC_ROOT_KEY_DER).expect("Failed decoding IC root key.")
    );
}

fn extract_ic_root_key_from_der(buf: &[u8]) -> Result<Vec<u8>, CanisterSigVerificationError> {
    let expected_length = IC_ROOT_KEY_DER_PREFIX.len() + IC_ROOT_KEY_LENGTH;
    if buf.len() != expected_length {
        return Err(CanisterSigVerificationError::Unknown(String::from(
            "invalid root pk length",
        )));
    }

    let prefix = &buf[0..IC_ROOT_KEY_DER_PREFIX.len()];
    if prefix[..] != IC_ROOT_KEY_DER_PREFIX[..] {
        return Err(CanisterSigVerificationError::Unknown(String::from(
            "invalid root pk prefix",
        )));
    }

    let key = &buf[IC_ROOT_KEY_DER_PREFIX.len()..];
    Ok(key.to_vec())
}

pub fn set_ic_root_public_key_for_testing(pk_der: Vec<u8>) {
    let mut root_pk = IC_ROOT_PUBLIC_KEY.write().unwrap();
    *root_pk = pk_der;
}

#[derive(Serialize, Deserialize)]
pub struct CanisterSig {
    pub certificate: ByteBuf,
    pub tree: HashTree,
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct CanisterSigPublicKey {
    pub signing_canister_id: Principal,
    #[serde(with = "serde_bytes")]
    pub seed: Vec<u8>,
}

fn key_decoding_err(err_msg: &str) -> CanisterSigVerificationError {
    CanisterSigVerificationError::InvalidPublicKey(String::from(err_msg))
}

impl TryFrom<&[u8]> for CanisterSigPublicKey {
    type Error = CanisterSigVerificationError;

    fn try_from(der_pubkey_bytes: &[u8]) -> Result<Self, Self::Error> {
        // TODO: check the entire DER-structure.
        let oid_part = &der_pubkey_bytes[2..(CANISTER_KEY_DER_OID.len() + 2)];
        if oid_part[..] != CANISTER_KEY_DER_OID[..] {
            return Err(key_decoding_err("invalid OID of canister key"));
        }
        let bitstring_offset: usize = CANISTER_KEY_DER_PREFIX_LENGTH;
        let canister_id_len: usize = if der_pubkey_bytes.len() > bitstring_offset {
            usize::from(der_pubkey_bytes[bitstring_offset])
        } else {
            return Err(key_decoding_err("canister key shorter than DER prefix"));
        };
        if der_pubkey_bytes.len() < (bitstring_offset + 1 + canister_id_len) {
            return Err(key_decoding_err("canister key too short"));
        }
        let canister_id_raw =
            &der_pubkey_bytes[(bitstring_offset + 1)..(bitstring_offset + 1 + canister_id_len)];
        let seed = &der_pubkey_bytes[bitstring_offset + canister_id_len + 1..];

        let canister_id = Principal::try_from_slice(canister_id_raw)
            .map_err(|e| key_decoding_err(&format!("invalid canister id in canister pk: {}", e)))?;
        Ok(CanisterSigPublicKey {
            signing_canister_id: canister_id,
            seed: seed.to_vec(),
        })
    }
}

#[derive(Debug)]
pub enum CanisterSigVerificationError {
    InvalidPublicKey(String),
    InvalidRootPublicKey,
    InvalidBlsSignature,
    InvalidDelegation,
    CertificateNotAuthorized,
    Unknown(String),
}

impl Display for CanisterSigVerificationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

pub fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
}

pub fn canister_sig_pk_der(canister_id: Principal, seed: &[u8]) -> Vec<u8> {
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

pub fn verify_root_signature(
    ic_certificate: &Certificate,
    signing_canister_id: Principal,
) -> Result<(), CanisterSigVerificationError> {
    let signing_pk_der = validate_delegation(&ic_certificate.delegation, signing_canister_id)?;
    let signing_pk = extract_ic_root_key_from_der(&signing_pk_der)?;
    let root_hash = ic_certificate.tree.digest();
    let mut msg = vec![];
    msg.extend_from_slice(IC_STATE_ROOT_DOMAIN_SEPARATOR);
    msg.extend_from_slice(&root_hash);
    if verify_bls_signature(&ic_certificate.signature, &msg, &signing_pk).is_err() {
        return Err(CanisterSigVerificationError::InvalidBlsSignature);
    }
    Ok(())
}

fn validate_delegation(
    delegation: &Option<Delegation>,
    signing_canister_id: Principal,
) -> Result<Vec<u8>, CanisterSigVerificationError> {
    match delegation {
        None => {
            let root_pk = IC_ROOT_PUBLIC_KEY.read().map_err(|_| {
                CanisterSigVerificationError::Unknown(String::from(
                    "Internal error accessing IC root public key",
                ))
            })?;
            Ok(root_pk.to_owned())
        }
        Some(delegation) => {
            let cert: Certificate = serde_cbor::from_slice(&delegation.certificate).unwrap();
            let _ = verify_root_signature(&cert, signing_canister_id);
            let canister_range_path = [
                b"subnet",
                delegation.subnet_id.as_slice(),
                b"canister_ranges",
            ];
            let LookupResult::Found(canister_range) = cert.tree.lookup_path(&canister_range_path)
            else {
                return Err(CanisterSigVerificationError::InvalidDelegation);
            };
            let ranges: Vec<(Principal, Principal)> =
                serde_cbor::from_slice(canister_range).unwrap();
            if !principal_is_within_ranges(&signing_canister_id, &ranges[..]) {
                return Err(CanisterSigVerificationError::CertificateNotAuthorized);
            }

            let public_key_path = [b"subnet", delegation.subnet_id.as_slice(), b"public_key"];
            let LookupResult::Found(pk) = cert.tree.lookup_path(&public_key_path) else {
                return Err(CanisterSigVerificationError::InvalidDelegation);
            };
            Ok(pk.to_vec())
        }
    }
}

// Checks if a principal is contained within a list of principal ranges
// A range is a tuple: (low: Principal, high: Principal), as described here: https://docs.dfinity.systems/spec/public/#state-tree-subnet
// Taken from https://github.com/dfinity/agent-rs/blob/60f7a0db21688ca423dee0bb150e142a03e925c6/ic-agent/src/agent/mod.rs#L784
fn principal_is_within_ranges(principal: &Principal, ranges: &[(Principal, Principal)]) -> bool {
    ranges
        .iter()
        .any(|r| principal >= &r.0 && principal <= &r.1)
}
