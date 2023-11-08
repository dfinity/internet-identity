use candid::Principal;
use lazy_static::lazy_static;

pub mod signature_map;

pub const IC_ROOT_PK_DER_PREFIX: &[u8; 37] = b"\x30\x81\x82\x30\x1d\x06\x0d\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x01\x02\x01\x06\x0c\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x02\x01\x03\x61\x00";
pub const IC_ROOT_PK_DER: &[u8; 133] = b"\x30\x81\x82\x30\x1d\x06\x0d\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x01\x02\x01\x06\x0c\x2b\x06\x01\x04\x01\x82\xdc\x7c\x05\x03\x02\x01\x03\x61\x00\x81\x4c\x0e\x6e\xc7\x1f\xab\x58\x3b\x08\xbd\x81\x37\x3c\x25\x5c\x3c\x37\x1b\x2e\x84\x86\x3c\x98\xa4\xf1\xe0\x8b\x74\x23\x5d\x14\xfb\x5d\x9c\x0c\xd5\x46\xd9\x68\x5f\x91\x3a\x0c\x0b\x2c\xc5\x34\x15\x83\xbf\x4b\x43\x92\xe4\x67\xdb\x96\xd6\x5b\x9b\xb4\xcb\x71\x71\x12\xf8\x47\x2e\x0d\x5a\x4d\x14\x50\x5f\xfd\x74\x84\xb0\x12\x91\x09\x1c\x5f\x87\xb9\x88\x83\x46\x3f\x98\x09\x1a\x0b\xaa\xae";
pub const IC_ROOT_PK_LENGTH: usize = 96;

pub const CANISTER_SIG_PK_DER_PREFIX_LENGTH: usize = 19;
// Canister signatures' public key OID is 1.3.6.1.4.1.56387.1.2,
// cf. https://internetcomputer.org/docs/current/references/ic-interface-spec#canister-signatures
pub const CANISTER_SIG_PK_DER_OID: &[u8; 14] =
    b"\x30\x0C\x06\x0A\x2B\x06\x01\x04\x01\x83\xB8\x43\x01\x02";

lazy_static! {
    /// The IC root public key used when verifying canister signatures.
    static ref IC_ROOT_PUBLIC_KEY: Vec<u8> =
        extract_raw_root_pk_from_der(IC_ROOT_PK_DER).expect("Failed decoding IC root key.");
}

/// A public key of canister signatures,
/// see https://internetcomputer.org/docs/current/references/ic-interface-spec#canister-signatures
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CanisterSigPublicKey {
    pub canister_id: Principal,
    pub seed: Vec<u8>,
}

impl TryFrom<&[u8]> for CanisterSigPublicKey {
    type Error = String;

    fn try_from(pk_der: &[u8]) -> Result<Self, Self::Error> {
        let pk_raw = extract_raw_canister_sig_pk_from_der(pk_der)?;
        let canister_id_len: usize = if !pk_raw.is_empty() {
            usize::from(pk_der[0])
        } else {
            return Err("empty raw canister sig pk".to_string());
        };
        if pk_raw.len() < (1 + canister_id_len) {
            return Err("canister sig pk too short".to_string());
        }
        let canister_id_raw = &pk_der[1..(1 + canister_id_len)];
        let seed = &pk_der[canister_id_len + 1..];
        let canister_id = Principal::try_from_slice(canister_id_raw)
            .map_err(|e| format!("invalid canister id in canister sig pk: {}", e))?;
        Ok(CanisterSigPublicKey {
            canister_id,
            seed: seed.to_vec(),
        })
    }
}

impl CanisterSigPublicKey {
    /// Constructs a new canister signatures public key.
    pub fn new(canister_id: Principal, seed: Vec<u8>) -> Self {
        CanisterSigPublicKey { canister_id, seed }
    }

    /// Returns a byte vector with DER-encoding of this key.
    pub fn to_der(&self) -> Vec<u8> {
        get_canister_sig_pk_der(self.canister_id, &self.seed)
    }
}

/// Returns (DER-encoded) public key of the canister signatures for the given canister_id and seed.
/// (cf. https://internetcomputer.org/docs/current/references/ic-interface-spec#canister-signatures))
pub fn get_canister_sig_pk_der(canister_id: Principal, seed: &[u8]) -> Vec<u8> {
    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(canister_id.as_ref().len() as u8);
    bitstring.extend(canister_id.as_ref());
    bitstring.extend(seed);

    let mut der: Vec<u8> = vec![];
    // sequence of length 17 + the bit string length
    der.push(0x30);
    der.push(17 + bitstring.len() as u8);
    der.extend(CANISTER_SIG_PK_DER_OID);
    // BIT string of given length
    der.push(0x03);
    der.push(1 + bitstring.len() as u8);
    der.push(0x00);
    der.extend(bitstring);
    der
}

/// Verifies the structure given public key in DER-format, and returns raw bytes of the key.
pub fn extract_raw_root_pk_from_der(pk_der: &[u8]) -> Result<Vec<u8>, String> {
    let expected_length = IC_ROOT_PK_DER_PREFIX.len() + IC_ROOT_PK_LENGTH;
    if pk_der.len() != expected_length {
        return Err(String::from("invalid root pk length"));
    }

    let prefix = &pk_der[0..IC_ROOT_PK_DER_PREFIX.len()];
    if prefix[..] != IC_ROOT_PK_DER_PREFIX[..] {
        return Err(String::from("invalid OID"));
    }

    let key = &pk_der[IC_ROOT_PK_DER_PREFIX.len()..];
    Ok(key.to_vec())
}

/// Verifies the structure given public key in DER-format, and returns raw bytes of the key.
pub fn extract_raw_canister_sig_pk_from_der(pk_der: &[u8]) -> Result<Vec<u8>, String> {
    let oid_part = &pk_der[2..(CANISTER_SIG_PK_DER_OID.len() + 2)];
    if oid_part[..] != CANISTER_SIG_PK_DER_OID[..] {
        return Err(String::from("invalid OID of canister sig pk"));
    }
    let bitstring_offset: usize = CANISTER_SIG_PK_DER_PREFIX_LENGTH;
    let canister_id_len: usize = if pk_der.len() > bitstring_offset {
        usize::from(pk_der[bitstring_offset])
    } else {
        return Err(String::from("canister sig pk shorter than DER prefix"));
    };
    if pk_der.len() < (bitstring_offset + 1 + canister_id_len) {
        return Err(String::from("canister sig pk too short"));
    }
    Ok(pk_der[(bitstring_offset)..].to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    const TEST_SIGNING_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const TEST_SEED: [u8; 3] = [42, 72, 44];

    const CANISTER_SIG_PK_DER: &[u8; 33] = b"\x30\x1f\x30\x0c\x06\x0a\x2b\x06\x01\x04\x01\x83\xb8\x43\x01\x02\x03\x0f\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01\x2a\x48\x2c";

    #[test]
    fn should_der_encode_canister_sig_pk() {
        let canister_id = Principal::from_text(TEST_SIGNING_CANISTER_ID).expect("wrong principal");
        let cs_pk_der = get_canister_sig_pk_der(canister_id, &TEST_SEED);
        assert_eq!(CANISTER_SIG_PK_DER.as_slice(), cs_pk_der.as_slice());
    }

    #[test]
    fn should_extract_raw_canister_sig_pk_from_der() {
        let raw_pk = extract_raw_canister_sig_pk_from_der(CANISTER_SIG_PK_DER)
            .expect("Wrong DER canister sig pk");
        assert_eq!(
            raw_pk.as_slice(),
            &(*CANISTER_SIG_PK_DER)[CANISTER_SIG_PK_DER_PREFIX_LENGTH..]
        )
    }

    #[test]
    fn should_fail_extract_raw_canister_sig_pk_from_bad_oid_der() {
        let mut bad_oid_der = *CANISTER_SIG_PK_DER;
        bad_oid_der[2] += 42;
        let result = extract_raw_canister_sig_pk_from_der(&bad_oid_der);
        assert_matches!(result, Err(e) if e.contains("invalid OID"));
    }

    #[test]
    fn should_fail_extract_raw_canister_sig_pk_from_short_der() {
        let result = extract_raw_canister_sig_pk_from_der(&CANISTER_SIG_PK_DER[..25]);
        assert_matches!(result, Err(e) if e.contains("pk too short"));
    }

    #[test]
    fn should_extract_raw_root_pk_from_der() {
        let raw_pk =
            extract_raw_root_pk_from_der(IC_ROOT_PK_DER).expect("Failed decoding IC root key.");
        assert_eq!(IC_ROOT_PK_LENGTH, raw_pk.len());
        assert_eq!(
            raw_pk.as_slice(),
            &(*IC_ROOT_PK_DER)[IC_ROOT_PK_DER_PREFIX.len()..]
        )
    }

    #[test]
    fn should_fail_extract_raw_root_pk_from_bad_oid_der() {
        let mut bad_oid_der = *IC_ROOT_PK_DER;
        bad_oid_der[2] += 42;
        let result = extract_raw_root_pk_from_der(&bad_oid_der);
        assert_matches!(result, Err(e) if e.contains("invalid OID"));
    }

    #[test]
    fn should_fail_extract_raw_root_pk_from_short_der() {
        let result = extract_raw_root_pk_from_der(&IC_ROOT_PK_DER[..42]);
        assert_matches!(result, Err(e) if e.contains("invalid root pk length"));
    }
}
