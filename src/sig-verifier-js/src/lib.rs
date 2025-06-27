use candid::Principal;
use ic_canister_sig_creation::{
    delegation_signature_msg, hash_bytes, CanisterSigPublicKey, DELEGATION_SIG_DOMAIN,
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::wasm_bindgen;

/// Verifies an IC canister signature.
/// More details: https://internetcomputer.org/docs/current/references/ic-interface-spec/#canister-signatures
///
/// Throws an error if the signature verification fails.
#[wasm_bindgen(js_name = verifyCanisterSignature)]
pub fn verify_canister_sig(
    message: &[u8],
    signature: &[u8],
    public_key: &[u8],
    ic_root_public_key_raw: &[u8],
) -> Result<(), String> {
    ic_signature_verification::verify_canister_sig(
        message,
        signature,
        public_key,
        ic_root_public_key_raw,
    )
    .map_err(|e| e.to_string())
}

/// Verifies the validity of the given signed delegation chain wrt. the challenge, and the other parameters.
/// Specifically:
///  * `signed_delegation_chain` contains exactly one delegation, denoted below as `delegations[0]`
///  * `delegations[0].pubkey` equals `challenge` (i.e. challenge is the "session key")
///  * `signed_delegation_chain.publicKey` is a public key for canister signatures of `ii_canister_id`
///  * `current_time_ns` denotes point in time before `delegations[0].expiration`
///  *  TODO: `current_time_ns` denotes point in time that is not more than 5min after signature creation time
///     (as specified in the certified tree of the Certificate embedded in the signature)
///  * `delegations[0].signature` is a valid canister signature on a representation-independent hash of `delegations[0]`,
///    wrt. `signed_delegation_chain.publicKey` and `ic_root_public_key_raw`
///
/// On success returns textual representation of the self-authenticating Principal determined by
/// public key `signed_delegation_chain.publicKey` (which identifies the user).
#[wasm_bindgen(js_name = validateDelegationAndGetPrincipal)]
pub fn validate_delegation_and_get_principal(
    challenge: &[u8],
    signed_delegation_chain_json: &str,
    current_time_ns: u64,
    ii_canister_id: &str, // textural representation of the principal
    ic_root_public_key_raw: &[u8],
) -> Result<String, String> {
    // Signed delegation chain contains exactly one delegation.
    let signed_delegation_chain: DelegationChain =
        serde_json::from_str(signed_delegation_chain_json)
            .map_err(|e| format!("Error parsing delegation_chain: {e}"))?;
    if signed_delegation_chain.delegations.len() != 1 {
        return Err("Expected exactly one signed delegation".to_string());
    }

    // `delegation[0].pubkey` equals `challenge`
    let signed_delegation = &signed_delegation_chain.delegations[0];
    let delegation = &signed_delegation.delegation;
    if delegation.pubkey != challenge {
        return Err(format!(
            "delegation.pubkey {} does not match the challenge",
            hex::encode(delegation.pubkey.clone())
        )
        .to_string());
    }

    // `signed_delegation_chain.publicKey` is a public key for canister signatures of `ii_canister_id`
    let cs_pk = CanisterSigPublicKey::try_from(signed_delegation_chain.publicKey.as_slice())
        .map_err(|e| format!("Invalid publicKey in delegation chain: {e}"))?;
    let expected_ii_canister_id =
        Principal::from_text(ii_canister_id).map_err(|e| format!("Invalid ii_canister_id: {e}"))?;
    if cs_pk.canister_id != expected_ii_canister_id {
        return Err(format!(
            "Delegation's signing canister {} does not match II canister id {}",
            cs_pk.canister_id, expected_ii_canister_id
        ));
    }

    // `current_time_ns` denotes point in time before `delegations[0].expiration`
    if delegation.expiration() < current_time_ns {
        return Err(format!("delegation expired at {}", delegation.expiration()));
    };

    // `current_time_ns` denotes point in time that is not more than 5min after signature creation time
    // (as specified in the certified tree of the Certificate embedded in the signature)
    // TODO

    // `delegations[0].signature` is a valid canister signature on a representation-independent hash of `delegations[0]`,
    //  wrt. `signed_delegation_chain.publicKey` and `ic_root_public_key_raw`.
    let message = msg_with_domain(
        DELEGATION_SIG_DOMAIN,
        &delegation_signature_msg(
            delegation.pubkey.as_slice(),
            delegation.expiration(),
            delegation.targets.as_ref(),
        ),
    );
    println!("signed bytes: {}", hex::encode(message.as_slice()));
    println!(
        "hash of signed bytes: {}",
        hex::encode(hash_bytes(message.as_slice()).as_slice())
    );
    ic_signature_verification::verify_canister_sig(
        message.as_slice(),
        signed_delegation.signature.as_slice(),
        &cs_pk.to_der(),
        ic_root_public_key_raw,
    )
    .map_err(|e| format!("Invalid canister signature: {e}"))?;

    Ok(Principal::self_authenticating(signed_delegation_chain.publicKey.as_slice()).to_text())
}

fn msg_with_domain(sep: &[u8], bytes: &[u8]) -> Vec<u8> {
    let mut msg = vec![sep.len() as u8];
    msg.append(&mut sep.to_vec());
    msg.append(&mut bytes.to_vec());
    msg
}

// A custom definition of Delegation, as we need to parse from hex-values provided  in JSON from JS.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Delegation {
    #[serde(with = "hex::serde")]
    pub pubkey: Vec<u8>,
    #[serde(with = "hex::serde")]
    pub expiration: Vec<u8>, // Unix timestamp ns, u46 as BE-hex (that's how browser encodes it)
    pub targets: Option<Vec<Vec<u8>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
struct SignedDelegation {
    pub delegation: Delegation,
    #[serde(with = "hex::serde")]
    pub signature: Vec<u8>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[allow(non_snake_case)]
struct DelegationChain {
    delegations: Vec<SignedDelegation>,
    #[serde(with = "hex::serde")]
    publicKey: Vec<u8>,
}

impl Delegation {
    fn expiration(&self) -> u64 {
        let expiration_bytes: [u8; 8] = <[u8; 8]>::try_from(self.expiration.as_slice()).unwrap();
        u64::from_be_bytes(expiration_bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use ic_canister_sig_creation::IC_ROOT_PUBLIC_KEY;
    const CHALLENGE_HEX: &str =
        "e7875e69ce7beda6fc7b6dfbd9b75be1c6f6d5debae3ae1ed7c7f873de1b6f9f75e9e7dcddcf37efaddcdf6f7b69a7b57377b5ddaef87dee386ddd75e39e9cd39d7d77debc79df1b7b469df36eb8e7cef47b4d5cefa7f5df67dbefc73debdf5c";
    const DELEGATION_CHAIN_JSON: &str = r#"{
        "delegations": [
          {
            "delegation":{
              "expiration":"17b5b384762bfd21",
              "pubkey":"e7875e69ce7beda6fc7b6dfbd9b75be1c6f6d5debae3ae1ed7c7f873de1b6f9f75e9e7dcddcf37efaddcdf6f7b69a7b57377b5ddaef87dee386ddd75e39e9cd39d7d77debc79df1b7b469df36eb8e7cef47b4d5cefa7f5df67dbefc73debdf5c"
            },
            "signature":"d9d9f7a26b6365727469666963617465590547d9d9f7a3647472656583018301830183024863616e697374657283018301830183018301830182045820640c48458731be868c750243066312f4e06b2bfde48309a3cfd0617ee3c8f3448301820458204042fb2844db206e1724a248eef393f5cb1d22280f298d948fc18e0a408533438301820458208d3dbc5b1ac807eb4f313b91712db94fdf4a50068207719f1cba37771b2ac8ef83024a000000000060002701018301830183024e6365727469666965645f6461746182035820a61cee2397ab0f006060d4a7bf4a9bef463d5b2381c502a6c66a26b6d088b64d820458206ccd6bb31a54761d4a56e9cfd8cba384d5b8fb47184e8ca13cb70e04f2209ace82045820c64354fe1474e905acdcf09f6569cfb29c305d0b06806908f2da5ee9404726bf820458203de781de0811f5a8469166c594f9433d966f686f4f4065ad9395e30bfac153e282045820cb2a94057004ae336fb52ba39117cf90aaadefe02ddfe9205bcc13c8f6150a0282045820bc1f9b4c54f66eb8fc25381e90641ae59ef87c590186355162a52cb4875242cb8204582001f9f57686d9eb1af846b6ee42c48b02289fe9cf134f84d527a000e65e4d7443820458201c1f10e2904ed9819f3cf7e051c473151700ea5b8038bf1413ba894b3afac4608204582045c96fb30bf784be7d9da2f7e41a2fa93f728bf07829da23acad05006286c269820458204ffce0d4d1e2124180daef5447fe496bbec7ef22b53786138b4acf523453fa75830182045820d5523abdfb2963caffc236cfe5a7f30a832b152c2f827d6acdf79ed5bb9a690e83024474696d65820349a1fa9b83afaae6da17697369676e6174757265583092eaf174a665a296e8968d910ab5a6130fb7deca606a68f5903d8e6a4b64a0fc609b7b7f6a68146e6c51b35e367deb8b6a64656c65676174696f6ea2697375626e65745f6964581d2c55b347ecf2686c83781d6c59d1b43e7b4cba8deb6c1b376107f2cd026b636572746966696361746559026ed9d9f7a2647472656583018204582075d2df1ca388b2596be5564ca726dbcadf77bbc535811734b704a8846153be1383018302467375626e657483018301830183018204582035bc207266aa1f9a1b4eea393efe91ae33ed4ce77069ed8e881d86716adf7b6b830182045820f8c3eae0377ee00859223bf1c6202f5885c4dcdc8fd13b1d48c3c838688919bc83018302581d2c55b347ecf2686c83781d6c59d1b43e7b4cba8deb6c1b376107f2cd02830183024f63616e69737465725f72616e67657382035832d9d9f782824a000000000060000001014a00000000006000ae0101824a00000000006000b001014a00000000006fffff010183024a7075626c69635f6b657982035885308182301d060d2b0601040182dc7c0503010201060c2b0601040182dc7c0503020103610090075120778eb21a530a02bcc763e7f4a192933506966af7b54c10a4d2b24de6a86b200e3440bae6267bf4c488d9a11d0472c38c1b6221198f98e4e6882ba38a5a4e3aa5afce899b7f825ed95adfa12629688073556f2747527213e8d73e40ce8204582036f3cd257d90fb38e42597f193a5e031dbd585b6292793bb04db4794803ce06e82045820028fc5e5f70868254e7215e7fc630dbd29eefc3619af17ce231909e1faf97e9582045820ef8995c410ed405731c9b913f67879e3b6a6b4d659d2746db9a6b47d7e70d3d582045820f9a6810df003d2188a807e8370076bd94a996877ec8bd11aa2c4e1358c01c6ab83024474696d65820349e2c9c9e480f6edd917697369676e61747572655830833724e450e6e1c8848118e82b04c5db3964f0869b6fb52af9bdbf3876435a19c798c03b41d5eb5fd39535c4ab24e70464747265658301820458209a7cc9ffcec2242e2e15b45a4e1fb9983c87c5b7e8badb7b92a891b40382f73683024373696783025820c9f3b4b781360e36240c549029e4b0857a6cc31e7230a680e551cab71aae0df38301820458203e26edaf16f66c93c238503a3d2077176e9ce6f0438940679b22cb31a636bfee83025820f49c0d7056981c0f2fdfaf02d219db038e2c448193bbf19642fbf118a8f4739a820340"
          }
        ],
        "publicKey":"303c300c060a2b0601040183b8430102032c000a00000000006000270101f3ffab2278616508ad5ebfa0cb79a21e08dbb7132f6875b95f81e72067f31302"
    }"#;
    const EXPIRATION: u64 = 1708469015156620577;
    const II_CANISTER_ID: &str = "fgte5-ciaaa-aaaad-aaatq-cai";
    const USER_PRINCIPAL_ID: &str =
        "hf7wk-a35mp-bc6eb-ntvr2-aeu3d-naglw-n6ea3-qn5ps-jcanu-p2vro-5ae";

    fn challenge() -> Vec<u8> {
        hex::decode(CHALLENGE_HEX)
            .expect("hex-decoding failed")
            .to_vec()
    }

    #[test]
    fn should_validate_delegation_and_get_principal() {
        let principal = validate_delegation_and_get_principal(
            challenge().as_slice(),
            DELEGATION_CHAIN_JSON,
            EXPIRATION - 42,
            II_CANISTER_ID,
            IC_ROOT_PUBLIC_KEY.as_slice(),
        )
        .expect("failed");
        assert_eq!(principal, USER_PRINCIPAL_ID);
    }

    #[test]
    fn should_fail_validate_delegation_and_get_principal_with_wrong_challenge() {
        let result = validate_delegation_and_get_principal(
            [1, 2, 3].as_slice(),
            DELEGATION_CHAIN_JSON,
            EXPIRATION - 42,
            II_CANISTER_ID,
            IC_ROOT_PUBLIC_KEY.as_slice(),
        );
        assert_matches!(result, Err(msg) if msg.contains("does not match the challenge"));
    }

    #[test]
    fn should_fail_validate_delegation_and_get_principal_with_wrong_delegation_chain() {
        let wrong_json: &str = r#"{
        "delegations": [
          {
            "delegation":{
              "expiration":"17b5b384762bfd21",
              "pubkey":"010101"
            },
            "signature":"deadbeef"
          }
        ]
        }"#; // missing "publicKey"-entry
        let result = validate_delegation_and_get_principal(
            challenge().as_slice(),
            wrong_json,
            EXPIRATION - 42,
            II_CANISTER_ID,
            IC_ROOT_PUBLIC_KEY.as_slice(),
        );
        assert_matches!(result, Err(msg) if msg.contains("Error parsing delegation_chain"));
    }

    #[test]
    fn should_fail_validate_delegation_and_get_principal_with_expired_delegation() {
        let result = validate_delegation_and_get_principal(
            challenge().as_slice(),
            DELEGATION_CHAIN_JSON,
            EXPIRATION + 42, // past expiration
            II_CANISTER_ID,
            IC_ROOT_PUBLIC_KEY.as_slice(),
        );
        assert_matches!(result, Err(msg) if msg.contains("delegation expired"));
    }

    #[test]
    fn should_fail_validate_delegation_and_get_principal_with_wrong_ii_canister_id() {
        let result = validate_delegation_and_get_principal(
            challenge().as_slice(),
            DELEGATION_CHAIN_JSON,
            EXPIRATION - 42,
            "jqajs-xiaaa-aaaad-aab5q-cai", // wrong canister id
            IC_ROOT_PUBLIC_KEY.as_slice(),
        );
        assert_matches!(result, Err(msg) if msg.contains("does not match II canister id"));
    }

    #[test]
    fn should_fail_validate_delegation_and_get_principal_with_wrong_ic_root_pk() {
        let mut wrong_ic_root_pk = IC_ROOT_PUBLIC_KEY.clone();
        wrong_ic_root_pk[IC_ROOT_PUBLIC_KEY.len() - 42] += 1; // change the root pk value
        let result = validate_delegation_and_get_principal(
            challenge().as_slice(),
            DELEGATION_CHAIN_JSON,
            EXPIRATION - 42,
            II_CANISTER_ID,
            wrong_ic_root_pk.as_slice(),
        );
        assert_matches!(result, Err(msg) if msg.contains("Invalid canister signature"));
    }
}
