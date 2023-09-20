use candid::Principal;
use ic_certified_map::{Hash, HashTree};
use serde::Serialize;
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};

pub mod signature_map;

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
