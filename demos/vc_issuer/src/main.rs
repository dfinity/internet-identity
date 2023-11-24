use candid::{candid_method, CandidType, Deserialize, Principal};
use canister_sig_util::signature_map::{SignatureMap, LABEL_SIG};
use canister_sig_util::{extract_raw_root_pk_from_der, CanisterSigPublicKey, IC_ROOT_PK_DER};
use ic_cdk::api::{caller, data_certificate, set_certified_data, time};
use ic_cdk::trap;
use ic_cdk_macros::{init, query, update};
use ic_certification::{Hash, HashTree};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::{DefaultMemoryImpl, RestrictedMemory, StableCell, Storable};
use identity_core::common::{Timestamp, Url};
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use serde::Serialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use sha2::{Digest, Sha256};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use vc_util::issuer_api::{
    ArgumentValue, CredentialSpec, GetCredentialRequest, GetCredentialResponse, Icrc21ConsentInfo,
    Icrc21ConsentMessageResponse, Icrc21Error, Icrc21ErrorInfo, Icrc21VcConsentMessageRequest,
    IssueCredentialError, IssuedCredentialData, PrepareCredentialRequest,
    PrepareCredentialResponse, PreparedCredentialData, SignedIdAlias,
};
use vc_util::{
    did_for_principal, get_verified_id_alias_from_jws, vc_jwt_to_jws, vc_signing_input,
    vc_signing_input_hash, AliasTuple,
};
use SupportedCredentialType::{UniversityDegreeCredential, VerifiedEmployee};

/// We use restricted memory in order to ensure the separation between non-managed config memory (first page)
/// and the managed memory for potential other data of the canister.
type Memory = RestrictedMemory<DefaultMemoryImpl>;
type ConfigCell = StableCell<IssuerConfig, Memory>;

const MINUTE_NS: u64 = 60 * 1_000_000_000;
const CERTIFICATE_VALIDITY_PERIOD_NS: u64 = 5 * MINUTE_NS;
const PROD_II_CANISTER_ID: &str = "rdmx6-jaaaa-aaaaa-aaadq-cai";
// The expiration of issued verifiable credentials.
const VC_EXPIRATION_PERIOD_NS: u64 = 15 * MINUTE_NS;
const VC_EMPLOYER_NAME: &str = "DFINITY Foundation";
const VC_INSTITUTION_NAME: &str = "DFINITY College of Engineering";

#[derive(Debug)]
enum SupportedCredentialType {
    VerifiedEmployee(String),
    UniversityDegreeCredential(String),
}

thread_local! {
    /// Static configuration of the canister set by init() or post_upgrade().
    static CONFIG: RefCell<ConfigCell> = RefCell::new(ConfigCell::init(config_memory(), IssuerConfig::default()).expect("failed to initialize stable cell"));
    static SIGNATURES : RefCell<SignatureMap> = RefCell::new(SignatureMap::default());
    static EMPLOYEES : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());
    static GRADUATES : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());
}

/// Reserve the first stable memory page for the configuration stable cell.
fn config_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 0..1)
}

#[cfg(target_arch = "wasm32")]
use ic_cdk::println;

#[derive(CandidType, Deserialize)]
struct IssuerConfig {
    /// Root of trust for checking canister signatures.
    ic_root_key_raw: Vec<u8>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
}

impl Storable for IssuerConfig {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to encode IssuerConfig"))
    }
    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to decode IssuerConfig")
    }
    const BOUND: Bound = Bound::Unbounded;
}

impl Default for IssuerConfig {
    fn default() -> Self {
        Self {
            ic_root_key_raw: extract_raw_root_pk_from_der(IC_ROOT_PK_DER)
                .expect("failed to extract raw root pk from der"),
            idp_canister_ids: vec![Principal::from_text(PROD_II_CANISTER_ID).unwrap()],
        }
    }
}

impl From<IssuerInit> for IssuerConfig {
    fn from(init: IssuerInit) -> Self {
        Self {
            ic_root_key_raw: extract_raw_root_pk_from_der(&init.ic_root_key_der)
                .expect("failed to extract raw root pk from der"),
            idp_canister_ids: init.idp_canister_ids,
        }
    }
}

#[derive(CandidType, Deserialize)]
struct IssuerInit {
    /// Root of trust for checking canister signatures.
    ic_root_key_der: Vec<u8>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
}

#[init]
#[candid_method(init)]
fn init(init_arg: Option<IssuerInit>) {
    let Some(init) = init_arg else {
        // nothing to do
        return;
    };
    apply_config(init);
}

#[update]
#[candid_method]
fn configure(config: IssuerInit) {
    apply_config(config);
}

fn apply_config(init: IssuerInit) {
    CONFIG
        .with_borrow_mut(|config_cell| config_cell.set(IssuerConfig::from(init)))
        .expect("failed to apply issuer config");
}

fn authorize_vc_request(alias: &SignedIdAlias) -> Result<AliasTuple, IssueCredentialError> {
    let alias_tuple = extract_id_alias(alias)?;

    if caller() != alias_tuple.id_dapp {
        return Err(IssueCredentialError::UnauthorizedSubject(format!(
            "Caller {} does not match id alias dapp principal {}.",
            caller(),
            alias_tuple.id_dapp
        )));
    }
    Ok(alias_tuple)
}

fn extract_id_alias(alias: &SignedIdAlias) -> Result<AliasTuple, IssueCredentialError> {
    CONFIG.with_borrow(|config| {
        let config = config.get();

        for idp_canister_id in &config.idp_canister_ids {
            if let Ok(alias_tuple) = get_verified_id_alias_from_jws(
                &alias.credential_jws,
                idp_canister_id,
                &config.ic_root_key_raw,
            ) {
                return Ok(alias_tuple);
            }
        }
        Err(IssueCredentialError::InvalidIdAlias(
            "id alias could not be verified".to_string(),
        ))
    })
}

#[update]
#[candid_method]
async fn prepare_credential(req: PrepareCredentialRequest) -> PrepareCredentialResponse {
    let alias_tuple = match authorize_vc_request(&req.signed_id_alias) {
        Ok(alias_tuple) => alias_tuple,
        Err(err) => return PrepareCredentialResponse::Err(err),
    };
    if let Err(err) = verify_credential_spec(&req.credential_spec) {
        return PrepareCredentialResponse::Err(IssueCredentialError::UnsupportedCredentialSpec(
            err,
        ));
    }
    let credential_type = match verify_credential_spec(&req.credential_spec) {
        Ok(credential_type) => credential_type,
        Err(err) => {
            return PrepareCredentialResponse::Err(
                IssueCredentialError::UnsupportedCredentialSpec(err),
            );
        }
    };

    let credential = match prepare_credential_payload(&credential_type, &alias_tuple) {
        Ok(credential) => credential,
        Err(err) => return PrepareCredentialResponse::Err(err),
    };
    let seed = calculate_seed(&alias_tuple.id_alias);
    let canister_id = ic_cdk::id();
    let canister_sig_pk = CanisterSigPublicKey::new(canister_id, seed.to_vec());
    let credential_jwt = credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure");
    let signing_input =
        vc_signing_input(&credential_jwt, &canister_sig_pk).expect("failed getting signing_input");
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
    use ic_certification::labeled_hash;
    SIGNATURES.with(|sigs| {
        let sigs = sigs.borrow();
        let prefixed_root_hash = &labeled_hash(LABEL_SIG, &sigs.root_hash());
        set_certified_data(&prefixed_root_hash[..]);
    })
}

#[query]
#[candid_method(query)]
fn get_credential(req: GetCredentialRequest) -> GetCredentialResponse {
    let alias_tuple = match authorize_vc_request(&req.signed_id_alias) {
        Ok(alias_tuple) => alias_tuple,
        Err(err) => return GetCredentialResponse::Err(err),
    };
    if let Err(err) = verify_credential_spec(&req.credential_spec) {
        return GetCredentialResponse::Err(IssueCredentialError::UnsupportedCredentialSpec(err));
    }
    let subject_principal = alias_tuple.id_alias;
    let seed = calculate_seed(&subject_principal);
    let canister_id = ic_cdk::id();
    let canister_sig_pk = CanisterSigPublicKey::new(canister_id, seed.to_vec());
    let prepared_context = match req.prepared_context {
        Some(context) => context,
        None => return GetCredentialResponse::Err(internal_error("missing prepared_context")),
    };
    let credential_jwt = match String::from_utf8(prepared_context.into_vec()) {
        Ok(s) => s,
        Err(_) => return GetCredentialResponse::Err(internal_error("invalid prepared_context")),
    };
    let signing_input =
        vc_signing_input(&credential_jwt, &canister_sig_pk).expect("failed getting signing_input");
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
    let vc_jws =
        vc_jwt_to_jws(&credential_jwt, &canister_sig_pk, &sig).expect("failed constructing JWS");
    GetCredentialResponse::Ok(IssuedCredentialData { vc_jws })
}

#[update]
#[candid_method]
async fn vc_consent_message(req: Icrc21VcConsentMessageRequest) -> Icrc21ConsentMessageResponse {
    if let Err(err) = verify_credential_spec(&req.credential_spec) {
        return Icrc21ConsentMessageResponse::Err(Icrc21Error::NotSupported(Icrc21ErrorInfo {
            description: err,
            error_code: 0,
        }));
    }
    Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
        consent_message: get_vc_consent_message(&req),
        language: "en-US".to_string(),
    })
}

fn verify_credential_spec(spec: &CredentialSpec) -> Result<SupportedCredentialType, String> {
    match spec.credential_name.as_str() {
        "VerifiedEmployee" => {
            verify_single_argument(
                spec,
                "employerName",
                ArgumentValue::String(VC_EMPLOYER_NAME.to_string()),
            )?;
            Ok(VerifiedEmployee(VC_EMPLOYER_NAME.to_string()))
        }
        "UniversityDegreeCredential" => {
            verify_single_argument(
                spec,
                "institutionName",
                ArgumentValue::String(VC_INSTITUTION_NAME.to_string()),
            )?;
            Ok(UniversityDegreeCredential(VC_INSTITUTION_NAME.to_string()))
        }
        other => Err(format!("Credential {} is not supported", other)),
    }
}

// Verifies that the credential spec `spec` contains an argument `expected_argument`
// with the value `expected_value`.
fn verify_single_argument(
    spec: &CredentialSpec,
    expected_argument: &str,
    expected_value: ArgumentValue,
) -> Result<(), String> {
    fn missing_argument_error(
        spec: &CredentialSpec,
        expected_argument: &str,
    ) -> Result<(), String> {
        Err(format!(
            "Missing argument '{}' for credential {}",
            expected_argument, spec.credential_name
        ))
    }

    let Some(arguments) = &spec.arguments else {
        return missing_argument_error(spec, expected_argument);
    };
    let Some(value) = arguments.get(expected_argument) else {
        return missing_argument_error(spec, expected_argument);
    };

    if value != &expected_value {
        return Err(format!(
            "Unsupported value for argument '{}': expected '{}', got '{}'",
            expected_argument, expected_value, value
        ));
    }

    let unexpected_arguments: Vec<&String> = arguments
        .keys()
        .filter(|k| k.as_str() != expected_argument)
        .collect();
    if !unexpected_arguments.is_empty() {
        return Err(format!(
            "Unexpected arguments for credential {}: {:?}",
            spec.credential_name, unexpected_arguments
        ));
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
    EMPLOYEES.with_borrow_mut(|employees| employees.insert(employee_id));
    format!("Added employee {}", employee_id)
}

#[update]
#[candid_method]
fn add_graduate(graduate_id: Principal) -> String {
    GRADUATES.with_borrow_mut(|graduates| graduates.insert(graduate_id));
    format!("Added graduate {}", graduate_id)
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

    let witness_hash = witness.digest();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(witness_hash),
            hex::encode(root_hash)
        ));
    }
    let tree = ic_certification::labeled(LABEL_SIG, witness);
    #[derive(Serialize)]
    struct Sig {
        certificate: ByteBuf,
        tree: HashTree,
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
    // IMPORTANT: In a real dapp the salt should be set to a random value.
    let dummy_salt = [5u8; 32];

    let mut bytes: Vec<u8> = vec![];
    bytes.push(dummy_salt.len() as u8);
    bytes.extend_from_slice(&dummy_salt);

    let principal_bytes = principal.as_slice();
    bytes.push(principal_bytes.len() as u8);
    bytes.extend(principal_bytes);
    hash_bytes(bytes)
}

fn bachelor_degree_credential(subject_principal: Principal, institution_name: &str) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_for_principal(subject_principal),
      "degree": {
        "type": "BachelorDegree",
        "name": "Bachelor of Engineering",
        "institutionName": institution_name,
      },
    }))
    .unwrap();

    // Build credential using subject above and issuer.
    CredentialBuilder::default()
        .id(Url::parse("https://example.edu/credentials/3732").unwrap())
        .issuer(Url::parse("https://example.edu").unwrap())
        .type_("UniversityDegreeCredential")
        .subject(subject)
        .expiration_date(exp_timestamp())
        .build()
        .unwrap()
}

fn dfinity_employment_credential(subject_principal: Principal, employer_name: &str) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
      "id": did_for_principal(subject_principal),
      "employee_of": {
            "employerId" : "did:web:dfinity.org",
            "employerName": employer_name,
      },
    }))
    .unwrap();

    // Build credential using subject above and issuer.
    CredentialBuilder::default()
        .id(Url::parse("https://employment.info/credentials/42").unwrap())
        .issuer(Url::parse("https://employment.info").unwrap())
        .type_("VerifiedEmployee")
        .subject(subject)
        .expiration_date(exp_timestamp())
        .build()
        .unwrap()
}

fn exp_timestamp() -> Timestamp {
    Timestamp::from_unix(((time() + VC_EXPIRATION_PERIOD_NS) / 1_000_000_000) as i64)
        .expect("internal: failed computing expiration timestamp")
}

fn prepare_credential_payload(
    credential_type: &SupportedCredentialType,
    alias_tuple: &AliasTuple,
) -> Result<Credential, IssueCredentialError> {
    match credential_type {
        VerifiedEmployee(employer_name) => {
            EMPLOYEES.with_borrow(|employees| {
                verify_authorized_principal(credential_type, alias_tuple, employees)
            })?;
            Ok(dfinity_employment_credential(
                alias_tuple.id_alias,
                employer_name.as_str(),
            ))
        }
        UniversityDegreeCredential(institution_name) => {
            GRADUATES.with_borrow(|graduates| {
                verify_authorized_principal(credential_type, alias_tuple, graduates)
            })?;
            Ok(bachelor_degree_credential(
                alias_tuple.id_alias,
                institution_name.as_str(),
            ))
        }
    }
}

fn verify_authorized_principal(
    credential_type: &SupportedCredentialType,
    alias_tuple: &AliasTuple,
    authorized_principals: &HashSet<Principal>,
) -> Result<(), IssueCredentialError> {
    if authorized_principals.contains(&alias_tuple.id_dapp) {
        Ok(())
    } else {
        println!(
            "*** principal {} it is not authorized for credential type {:?}",
            alias_tuple.id_dapp.to_text(),
            credential_type
        );
        Err(IssueCredentialError::UnauthorizedSubject(format!(
            "unauthorized principal {}",
            alias_tuple.id_dapp.to_text()
        )))
    }
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
    use candid::utils::{service_equal, CandidSource};
    use std::path::Path;

    /// Checks candid interface type equality by making sure that the service in the did file is
    /// equal to the generated interface.
    #[test]
    fn check_candid_interface_compatibility() {
        let canister_interface = __export_service();
        service_equal(
            CandidSource::Text(&canister_interface),
            CandidSource::File(Path::new("vc_issuer.did")),
        )
        .unwrap_or_else(|e| {
            panic!(
                "the canister code interface is not equal to the did file: {:?}",
                e
            )
        });
    }
}
