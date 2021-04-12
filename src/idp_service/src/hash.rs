//! Provides helper functions to calculate the representation independent hash of structured data.
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

#[derive(Clone, Serialize, Deserialize)]
enum Value {
    Bytes(#[serde(with = "serde_bytes")] Vec<u8>),
    String(String),
    U64(u64),
    Array(Vec<Value>),
}

fn hash_of_map<S: ToString>(map: &BTreeMap<S, Value>) -> [u8; 32] {
    let mut hashes: Vec<Vec<u8>> = Vec::new();
    for (key, val) in map.iter() {
        hashes.push(hash_key_val(key.to_string(), val.clone()));
    }

    // Computes hash by first sorting by "field name" hash, which is the
    // same as sorting by concatenation of H(field name) · H(field value)
    // (although in practice it's actually more stable in the presence of
    // duplicated field names). Then concatenate all the hashes.
    hashes.sort();

    let mut hasher = Sha256::new();
    for hash in hashes {
        hasher.update(&hash);
    }

    hasher.finalize().into()
}

fn hash_key_val(key: String, val: Value) -> Vec<u8> {
    let mut key_hash = hash_string(key);
    let mut val_hash = hash_val(val);
    key_hash.append(&mut val_hash);
    key_hash
}

fn hash_string(value: String) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(value.as_bytes());
    hasher.finalize().to_vec()
}

fn hash_bytes(value: Vec<u8>) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(&value);
    hasher.finalize().to_vec()
}

fn hash_u64(value: u64) -> Vec<u8> {
    // We need at most ⌈ 64 / 7 ⌉ = 10 bytes to encode a 64 bit
    // integer in LEB128.
    let mut buf = [0u8; 10];
    let mut n = value;
    let mut i = 0;

    loop {
        let byte = (n & 0x7f) as u8;
        n >>= 7;

        if n == 0 {
            buf[i] = byte;
            break;
        } else {
            buf[i] = byte | 0x80;
            i += 1;
        }
    }

    hash_bytes(buf[..=i].to_vec())
}

// Arrays encoded as the concatenation of the hashes of the encodings of the
// array elements.
fn hash_array(elements: Vec<Value>) -> Vec<u8> {
    let mut hasher = Sha256::new();
    elements
        .into_iter()
        // Hash the encoding of all the array elements.
        .for_each(|e| hasher.update(hash_val(e).as_slice()));
    hasher.finalize().to_vec() // hash the concatenation of the hashes.
}

fn hash_val(val: Value) -> Vec<u8> {
    match val {
        Value::String(string) => hash_string(string),
        Value::Bytes(bytes) => hash_bytes(bytes),
        Value::U64(integer) => hash_u64(integer),
        Value::Array(elements) => hash_array(elements),
    }
}
