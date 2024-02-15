use ic_crypto_standalone_sig_verifier as ic_sig_ver;
use ic_crypto_standalone_sig_verifier::KeyBytesContentType;
use ic_types::crypto::threshold_sig::IcRootOfTrust;
use wasm_bindgen::prelude::wasm_bindgen;
use KeyBytesContentType::IcCanisterSignatureAlgPublicKeyDer;

/// Verifies a basic (i.e. not a canister signature) IC supported signature.
/// Supported signature schemes: https://internetcomputer.org/docs/current/references/ic-interface-spec/#signatures
///
/// Throws an error if the signature verification fails.
#[wasm_bindgen(js_name = verifyBasicSignature)]
pub fn verify_basic_sig_by_public_key(
    msg: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<(), String> {
    let (public_key, _) =
        ic_sig_ver::user_public_key_from_bytes(public_key).map_err(|e| e.to_string())?;
    ic_sig_ver::verify_basic_sig_by_public_key(
        public_key.algorithm_id,
        msg,
        signature,
        &public_key.key,
    )
    .map_err(|e| e.to_string())
}

/// Verifies an IC canister signature.
/// More details: https://internetcomputer.org/docs/current/references/ic-interface-spec/#canister-signatures
///
/// Throws an error if the signature verification fails.
#[wasm_bindgen(js_name = verifyCanisterSignature)]
pub fn verify_canister_sig(
    message: &[u8],
    signature: &[u8],
    public_key: &[u8],
    ic_root_public_key: &[u8],
) -> Result<(), String> {
    let root_of_trust = root_public_key_from_bytes(ic_root_public_key)?;
    ic_sig_ver::verify_canister_sig(message, signature, public_key, root_of_trust)
        .map_err(|e| e.to_string())
}

/// Verifies any IC supported signature.
/// Supported signature schemes: https://internetcomputer.org/docs/current/references/ic-interface-spec/#signatures
///
/// Throws an error if the signature verification fails.
#[wasm_bindgen(js_name = verifyIcSignature)]
pub fn verify_ic_signature(
    message: &[u8],
    signature: &[u8],
    public_key: &[u8],
    ic_root_public_key: &[u8],
) -> Result<(), String> {
    let (public_key, content_type) =
        ic_sig_ver::user_public_key_from_bytes(public_key).map_err(|e| e.to_string())?;
    let result = match content_type {
        IcCanisterSignatureAlgPublicKeyDer => {
            let root_of_trust = root_public_key_from_bytes(ic_root_public_key)?;
            ic_sig_ver::verify_canister_sig(message, signature, &public_key.key, root_of_trust)
        }
        _ => ic_sig_ver::verify_basic_sig_by_public_key(
            public_key.algorithm_id,
            message,
            signature,
            &public_key.key,
        ),
    };
    result.map_err(|e| e.to_string())
}

fn root_public_key_from_bytes(ic_root_public_key: &[u8]) -> Result<IcRootOfTrust, String> {
    let root_key_bytes: [u8; 96] = ic_root_public_key.try_into().map_err(|_| {
        format!(
            "Invalid length of ic root public key: expected 96 bytes, got {}",
            ic_root_public_key.len()
        )
    })?;
    Ok(IcRootOfTrust::from(root_key_bytes))
}
