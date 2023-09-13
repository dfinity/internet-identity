use candid::{candid_method, Principal};
use canister_sig_util::{
    der_encoded_canister_sig_pk, did_for_principal, hash_bytes, vc_jwt_to_jws, vc_signing_input,
    vc_signing_input_hash, CanisterSig,
};
use ic_cdk_macros::{query, update};
use ic_certified_map::Hash;
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    GetCredentialRequest, GetCredentialResponse, Icrc21ConsentInfo, Icrc21ConsentMessageRequest,
    Icrc21ConsentMessageResponse, IssueCredentialError, IssuedCredentialData,
    PrepareCredentialRequest, PrepareCredentialResponse, PreparedCredentialData,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use std::cell::RefCell;

use canister_sig_util::signature_map::{SignatureMap, LABEL_SIG};
use ic_cdk::api::{data_certificate, set_certified_data, time};
use ic_cdk::trap;

const MINUTE_NS: u64 = 60 * 1_000_000_000;
const CERTIFICATE_VALIDITY_PERIOD_NS: u64 = 5 * MINUTE_NS;

thread_local! {
    static SIGNATURES : RefCell<SignatureMap> = RefCell::new(SignatureMap::default());
}

#[cfg(target_arch = "wasm32")]
use ic_cdk::println;

#[update]
#[candid_method]
async fn prepare_credential(req: PrepareCredentialRequest) -> PrepareCredentialResponse {
    if let Err(err) = verify_prepare_credential_request(&req) {
        return PrepareCredentialResponse::Err(err);
    }
    let subject_principal = req.signed_id_alias.id_alias;
    let seed = calculate_seed(&subject_principal);
    let canister_id = ic_cdk::id();
    let canister_sig_pk_der = der_encoded_canister_sig_pk(canister_id, &seed);
    let credential = new_credential(subject_principal);
    let credential_jwt = credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure");
    let signing_input = vc_signing_input(&credential_jwt, &canister_sig_pk_der, canister_id);
    let msg_hash = vc_signing_input_hash(&signing_input);

    SIGNATURES.with(|sigs| {
        let mut sigs = sigs.borrow_mut();
        add_signature(&mut sigs, msg_hash, seed);
    });
    update_root_hash();
    PrepareCredentialResponse::Ok(PreparedCredentialData {
        vc_jwt: credential_jwt,
    })
}

fn update_root_hash() {
    use ic_certified_map::labeled_hash;
    SIGNATURES.with(|sigs| {
        let sigs = sigs.borrow();
        let prefixed_root_hash = &labeled_hash(LABEL_SIG, &sigs.root_hash());
        set_certified_data(&prefixed_root_hash[..]);
    })
}

#[query]
#[candid_method(query)]
fn get_credential(req: GetCredentialRequest) -> GetCredentialResponse {
    if let Err(err) = verify_get_credential_request(&req) {
        return GetCredentialResponse::Err(err);
    }
    let subject_principal = req.signed_id_alias.id_alias;
    let seed = calculate_seed(&subject_principal);
    let canister_id = ic_cdk::id();
    let canister_sig_pk_der = der_encoded_canister_sig_pk(canister_id, &seed);
    let signing_input = vc_signing_input(&req.vc_jwt, &canister_sig_pk_der, canister_id);
    let msg_hash = vc_signing_input_hash(&signing_input);
    let maybe_sig = SIGNATURES.with(|sigs| {
        let sigs = sigs.borrow();
        get_signature(&sigs, seed, msg_hash)
    });
    let sig = if let Some(sig) = maybe_sig {
        sig
    } else {
        return GetCredentialResponse::Err(IssueCredentialError::SignatureNotFound);
    };
    let vc_jws = vc_jwt_to_jws(&req.vc_jwt, &canister_sig_pk_der, &sig, canister_id);
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[update]
#[candid_method]
async fn consent_message(req: Icrc21ConsentMessageRequest) -> Icrc21ConsentMessageResponse {
    Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
        consent_message: "Do you want to get a verifiable credential?".to_string(),
        language: req.preferences.language,
    })
}

fn main() {}

fn add_signature(sigs: &mut SignatureMap, msg_hash: Hash, seed: Hash) {
    let signature_expires_at = time().saturating_add(CERTIFICATE_VALIDITY_PERIOD_NS);
    sigs.put(hash_bytes(seed), msg_hash, signature_expires_at);
}

fn get_signature(sigs: &SignatureMap, seed: Hash, msg_hash: Hash) -> Option<Vec<u8>> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let witness = sigs.witness(hash_bytes(seed), msg_hash)?;

    let witness_hash = witness.reconstruct();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(witness_hash),
            hex::encode(root_hash)
        ));
    }
    let tree = ic_certified_map::labeled(LABEL_SIG, witness);
    let sig = CanisterSig {
        certificate: ByteBuf::from(certificate),
        tree,
    };

    let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
    cbor.self_describe().unwrap();
    sig.serialize(&mut cbor).unwrap();
    Some(cbor.into_inner())
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

fn new_credential(subject_principal: Principal) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_for_principal(subject_principal),
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

fn verify_prepare_credential_request(
    _req: &PrepareCredentialRequest,
) -> Result<(), IssueCredentialError> {
    Ok(())
}

fn verify_get_credential_request(_req: &GetCredentialRequest) -> Result<(), IssueCredentialError> {
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
