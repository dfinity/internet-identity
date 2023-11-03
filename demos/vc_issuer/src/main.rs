use candid::{candid_method, Principal};
use canister_sig_util::get_canister_sig_pk_der;
use ic_cdk_macros::{query, update};
use ic_certified_map::{Hash, HashTree};
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use internet_identity_interface::internet_identity::types::vc_mvp::issuer::{
    ArgumentValue, CredentialSpec, GetCredentialRequest, GetCredentialResponse, Icrc21ConsentInfo,
    Icrc21ConsentMessageResponse, Icrc21Error, Icrc21ErrorInfo, Icrc21VcConsentMessageRequest,
    IssueCredentialError, IssuedCredentialData, PrepareCredentialRequest,
    PrepareCredentialResponse, PreparedCredentialData,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use std::cell::RefCell;

use canister_sig_util::signature_map::{SignatureMap, LABEL_SIG};
use ic_cdk::api::{data_certificate, set_certified_data, time};
use ic_cdk::trap;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use vc_util::{did_for_principal, vc_jwt_to_jws, vc_signing_input, vc_signing_input_hash};

const MINUTE_NS: u64 = 60 * 1_000_000_000;
const CERTIFICATE_VALIDITY_PERIOD_NS: u64 = 5 * MINUTE_NS;

thread_local! {
    static SIGNATURES : RefCell<SignatureMap> = RefCell::new(SignatureMap::default());
    static EMPLOYEES : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());
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
    let canister_sig_pk_der = get_canister_sig_pk_der(canister_id, &seed);
    let credential = new_credential_dfinity_employment(subject_principal);
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
        prepared_context: Some(ByteBuf::from(credential_jwt.as_bytes())),
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
    let canister_sig_pk_der = get_canister_sig_pk_der(canister_id, &seed);
    let prepared_context = match req.prepared_context {
        Some(context) => context,
        None => return GetCredentialResponse::Err(internal_error("missing prepared_context")),
    };
    let credential_jwt = match String::from_utf8(prepared_context.into_vec()) {
        Ok(s) => s,
        Err(_) => return GetCredentialResponse::Err(internal_error("invalid prepared_context")),
    };
    let signing_input = vc_signing_input(&credential_jwt, &canister_sig_pk_der, canister_id);
    let msg_hash = vc_signing_input_hash(&signing_input);
    let maybe_sig = SIGNATURES.with(|sigs| {
        let sigs = sigs.borrow();
        get_signature(&sigs, seed, msg_hash)
    });
    let sig = if let Some(sig) = maybe_sig {
        sig
    } else {
        return GetCredentialResponse::Err(IssueCredentialError::SignatureNotFound(String::from(
            "signature not prepared or expired",
        )));
    };
    let vc_jws = vc_jwt_to_jws(&credential_jwt, &canister_sig_pk_der, &sig, canister_id);
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[update]
#[candid_method]
async fn vc_consent_message(req: Icrc21VcConsentMessageRequest) -> Icrc21ConsentMessageResponse {
    if let Err(err) = verify_credential_spec(&req.credential_spec) {
        return Icrc21ConsentMessageResponse::Err(err);
    }
    Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
        consent_message: get_vc_consent_message(&req),
        language: "en-US".to_string(),
    })
}

fn verify_credential_spec(spec: &CredentialSpec) -> Result<(), Icrc21Error> {
    if spec.credential_name != "VerifiedEmployee" {
        return Err(Icrc21Error::NotSupported(Icrc21ErrorInfo {
            error_code: 0,
            description: format!("Credential {} is not supported", spec.credential_name),
        }));
    }
    let arg_key = "employerName";
    if spec.arguments.is_none() || !spec.arguments.as_ref().unwrap().contains_key(arg_key) {
        return Err(Icrc21Error::MalformedCall(Icrc21ErrorInfo {
            error_code: 0,
            description: format!(
                "Missing argument '{}' for credential {}",
                arg_key, spec.credential_name
            ),
        }));
    }
    Ok(())
}
fn get_vc_consent_message(req: &Icrc21VcConsentMessageRequest) -> String {
    format!(
        "Issue credential '{}' with arguments:{}",
        req.credential_spec.credential_name,
        arguments_as_string(&req.credential_spec.arguments)
    )
}
fn arguments_as_string(maybe_args: &Option<HashMap<String, ArgumentValue>>) -> String {
    let mut arg_str = String::new();
    let empty_args = HashMap::new();
    let args = maybe_args.as_ref().unwrap_or(&empty_args);
    for (key, value) in args {
        arg_str.push_str(&format!("\n\t{}: {}", key, value));
    }
    arg_str
}

#[update]
#[candid_method]
fn add_employee(employee_id: Principal) -> String {
    EMPLOYEES.with(|employees| employees.borrow_mut().insert(employee_id));
    format!("Added employee {}", employee_id)
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
    #[derive(Serialize)]
    struct Sig<'a> {
        certificate: ByteBuf,
        tree: HashTree<'a>,
    }

    let sig = Sig {
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

#[allow(dead_code)]
fn new_credential_bachelor_degree(subject_principal: Principal) -> Credential {
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

fn new_credential_dfinity_employment(subject_principal: Principal) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_for_principal(subject_principal),
      "employee_of": {
            "employerId" : "did:web:dfinity.org",
            "employerName": "DFINITY Foundation",
      },
    }))
    .unwrap();

    // Build credential using subject above and issuer.
    CredentialBuilder::default()
        .id(Url::parse("https://employment.info/credentials/42").unwrap())
        .issuer(Url::parse("https://employment.info").unwrap())
        .type_("VerifiedEmployee")
        .subject(subject)
        .build()
        .unwrap()
}

fn verify_prepare_credential_request(
    req: &PrepareCredentialRequest,
) -> Result<(), IssueCredentialError> {
    let is_employee =
        EMPLOYEES.with(|employees| employees.borrow().contains(&req.signed_id_alias.id_dapp));
    if is_employee {
        Ok(())
    } else {
        println!(
            "*** it is unknown whether {} is an employee",
            req.signed_id_alias.id_dapp
        );
        Err(IssueCredentialError::UnauthorizedSubject(format!(
            "unauthorized principal {}",
            req.signed_id_alias.id_dapp
        )))
    }
}

fn verify_get_credential_request(_req: &GetCredentialRequest) -> Result<(), IssueCredentialError> {
    Ok(())
}

fn internal_error(msg: &str) -> IssueCredentialError {
    IssueCredentialError::Internal(String::from(msg))
}

fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
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
