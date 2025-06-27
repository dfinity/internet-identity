use crate::consent_message::{get_vc_consent_message, SupportedLanguage};
use candid::{CandidType, Deserialize, Principal};
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap, LABEL_SIG};
use ic_canister_sig_creation::{
    extract_raw_root_pk_from_der, CanisterSigPublicKey, IC_ROOT_PUBLIC_KEY,
};
use ic_cdk::api::{caller, set_certified_data, time};
use ic_cdk_macros::{init, query, update};
use ic_certification::{fork_hash, labeled_hash, pruned, Hash};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::{DefaultMemoryImpl, RestrictedMemory, StableCell, Storable};
use ic_verifiable_credentials::issuer_api::{
    ArgumentValue, CredentialSpec, DerivationOriginData, DerivationOriginError,
    DerivationOriginRequest, GetCredentialRequest, Icrc21ConsentInfo, Icrc21Error,
    Icrc21VcConsentMessageRequest, IssueCredentialError, IssuedCredentialData,
    PrepareCredentialRequest, PreparedCredentialData, SignedIdAlias,
};
use ic_verifiable_credentials::{
    build_credential_jwt, did_for_principal, get_verified_id_alias_from_jws, vc_jwt_to_jws,
    vc_signing_input, AliasTuple, CredentialParams, VC_SIGNING_INPUT_DOMAIN,
};
use include_dir::{include_dir, Dir};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use SupportedCredentialType::{UniversityDegree, VerifiedAdult, VerifiedEmployee};

use asset_util::{collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType};
use ic_cdk::api;
use ic_cdk_macros::post_upgrade;
use lazy_static::lazy_static;

mod consent_message;

/// We use restricted memory in order to ensure the separation between non-managed config memory (first page)
/// and the managed memory for potential other data of the canister.
type Memory = RestrictedMemory<DefaultMemoryImpl>;
type ConfigCell = StableCell<IssuerConfig, Memory>;

const MINUTE_NS: u64 = 60 * 1_000_000_000;
const PROD_II_CANISTER_ID: &str = "rdmx6-jaaaa-aaaaa-aaadq-cai";
// The expiration of issued verifiable credentials.
const VC_EXPIRATION_PERIOD_NS: u64 = 15 * MINUTE_NS;
const VC_EMPLOYER_NAME: &str = "DFINITY Foundation";
const VC_INSTITUTION_NAME: &str = "DFINITY College of Engineering";

#[derive(Debug)]
pub enum SupportedCredentialType {
    VerifiedEmployee(String),
    UniversityDegree(String),
    VerifiedAdult(u16),
}

thread_local! {
    /// Static configuration of the canister set by init() or post_upgrade().
    static CONFIG: RefCell<ConfigCell> = RefCell::new(ConfigCell::init(config_memory(), IssuerConfig::default()).expect("failed to initialize stable cell"));
    static SIGNATURES : RefCell<SignatureMap> = RefCell::new(SignatureMap::default());
    static EMPLOYEES : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());
    static GRADUATES : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());
    static ADULTS : RefCell<HashSet<Principal>> = RefCell::new(HashSet::new());

    // Assets for the management app
    static ASSETS: RefCell<CertifiedAssets> = RefCell::new(CertifiedAssets::default());
}

lazy_static! {
    // Seed and public key used for signing the credentials.
    static ref CANISTER_SIG_SEED: Vec<u8> = hash_bytes("vc_demo_issuer").to_vec();
    static ref CANISTER_SIG_PK: CanisterSigPublicKey = CanisterSigPublicKey::new(ic_cdk::id(), CANISTER_SIG_SEED.clone());
}

/// Reserve the first stable memory page for the configuration stable cell.
fn config_memory() -> Memory {
    RestrictedMemory::new(DefaultMemoryImpl::default(), 0..1)
}

#[cfg(target_arch = "wasm32")]
use ic_cdk::println;

#[derive(CandidType, Deserialize, Clone)]
struct IssuerConfig {
    /// Root of trust for checking canister signatures.
    ic_root_key_raw: Vec<u8>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
    /// The derivation origin to be used by the issuer.
    derivation_origin: String,
    /// Frontend hostname to be used by the issuer.
    frontend_hostname: String,
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
        let derivation_origin = format!("https://{}.icp0.io", ic_cdk::id().to_text());
        Self {
            ic_root_key_raw: IC_ROOT_PUBLIC_KEY.clone(),
            idp_canister_ids: vec![Principal::from_text(PROD_II_CANISTER_ID).unwrap()],
            derivation_origin: derivation_origin.clone(),
            frontend_hostname: derivation_origin, // by default, use DERIVATION_ORIGIN as frontend-hostname
        }
    }
}

impl From<IssuerInit> for IssuerConfig {
    fn from(init: IssuerInit) -> Self {
        Self {
            ic_root_key_raw: if let Some(custom_root_pk) = init.ic_root_key_der {
                extract_raw_root_pk_from_der(&custom_root_pk)
                    .expect("failed to extract raw root pk from der")
            } else {
                IC_ROOT_PUBLIC_KEY.clone()
            },
            idp_canister_ids: init.idp_canister_ids,
            derivation_origin: init.derivation_origin,
            frontend_hostname: init.frontend_hostname,
        }
    }
}

#[derive(CandidType, Deserialize)]
struct IssuerInit {
    /// Root of trust for checking canister signatures.
    ic_root_key_der: Option<Vec<u8>>,
    /// List of canister ids that are allowed to provide id alias credentials.
    idp_canister_ids: Vec<Principal>,
    /// The derivation origin to be used by the issuer.
    derivation_origin: String,
    /// Frontend hostname be used by the issuer.
    frontend_hostname: String,
}

#[init]
fn init(init_arg: Option<IssuerInit>) {
    if let Some(init) = init_arg {
        apply_config(IssuerConfig::from(init));
    };

    init_assets();
}

#[post_upgrade]
fn post_upgrade(init_arg: Option<IssuerInit>) {
    init(init_arg);
}

#[update]
fn configure(init: IssuerInit) {
    apply_config(IssuerConfig::from(init));
}

#[update]
fn set_derivation_origin(frontend_hostname: String, derivation_origin: String) {
    let mut config: IssuerConfig = CONFIG.with_borrow(|config| config.get().clone());
    config.derivation_origin = derivation_origin;
    config.frontend_hostname = frontend_hostname;
    apply_config(config);
}

fn apply_config(config: IssuerConfig) {
    CONFIG
        .with_borrow_mut(|config_cell| config_cell.set(config))
        .expect("failed to apply issuer config");
}

fn authorize_vc_request(
    alias: &SignedIdAlias,
    expected_vc_subject: &Principal,
    current_time_ns: u128,
) -> Result<AliasTuple, IssueCredentialError> {
    CONFIG.with_borrow(|config| {
        let config = config.get();

        for idp_canister_id in &config.idp_canister_ids {
            if let Ok(alias_tuple) = get_verified_id_alias_from_jws(
                &alias.credential_jws,
                expected_vc_subject,
                idp_canister_id,
                &config.ic_root_key_raw,
                current_time_ns,
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
async fn prepare_credential(
    req: PrepareCredentialRequest,
) -> Result<PreparedCredentialData, IssueCredentialError> {
    let alias_tuple = match authorize_vc_request(&req.signed_id_alias, &caller(), time().into()) {
        Ok(alias_tuple) => alias_tuple,
        Err(err) => return Err(err),
    };

    let credential_jwt = match prepare_credential_jwt(&req.credential_spec, &alias_tuple) {
        Ok(credential) => credential,
        Err(err) => return Result::<PreparedCredentialData, IssueCredentialError>::Err(err),
    };
    let signing_input =
        vc_signing_input(&credential_jwt, &CANISTER_SIG_PK).expect("failed getting signing_input");

    SIGNATURES.with(|sigs| {
        let mut sigs = sigs.borrow_mut();
        sigs.add_signature(&CanisterSigInputs {
            domain: VC_SIGNING_INPUT_DOMAIN,
            seed: &CANISTER_SIG_SEED,
            message: &signing_input,
        });
    });
    update_root_hash();
    Ok(PreparedCredentialData {
        prepared_context: Some(ByteBuf::from(credential_jwt.as_bytes())),
    })
}

fn update_root_hash() {
    SIGNATURES.with_borrow(|sigs| {
        ASSETS.with_borrow(|assets| {
            let prefixed_root_hash = fork_hash(
                // NB: Labels added in lexicographic order.
                &assets.root_hash(),
                &labeled_hash(LABEL_SIG, &sigs.root_hash()),
            );

            set_certified_data(&prefixed_root_hash[..]);
        })
    })
}

#[query]
fn get_credential(req: GetCredentialRequest) -> Result<IssuedCredentialData, IssueCredentialError> {
    if let Err(err) = authorize_vc_request(&req.signed_id_alias, &caller(), time().into()) {
        return Result::<IssuedCredentialData, IssueCredentialError>::Err(err);
    };
    if let Err(err) = verify_credential_spec(&req.credential_spec) {
        return Result::<IssuedCredentialData, IssueCredentialError>::Err(
            IssueCredentialError::UnsupportedCredentialSpec(err),
        );
    }
    let prepared_context = match req.prepared_context {
        Some(context) => context,
        None => {
            return Result::<IssuedCredentialData, IssueCredentialError>::Err(internal_error(
                "missing prepared_context",
            ))
        }
    };
    let credential_jwt = match String::from_utf8(prepared_context.into_vec()) {
        Ok(s) => s,
        Err(_) => {
            return Result::<IssuedCredentialData, IssueCredentialError>::Err(internal_error(
                "invalid prepared_context",
            ))
        }
    };
    let signing_input =
        vc_signing_input(&credential_jwt, &CANISTER_SIG_PK).expect("failed getting signing_input");
    let sig_result = SIGNATURES.with(|sigs| {
        let sig_map = sigs.borrow();
        let certified_assets_root_hash = ASSETS.with_borrow(|assets| assets.root_hash());
        sig_map.get_signature_as_cbor(
            &CanisterSigInputs {
                domain: VC_SIGNING_INPUT_DOMAIN,
                seed: &CANISTER_SIG_SEED,
                message: &signing_input,
            },
            Some(certified_assets_root_hash),
        )
    });
    let sig = match sig_result {
        Ok(sig) => sig,
        Err(e) => {
            return Result::<IssuedCredentialData, IssueCredentialError>::Err(
                IssueCredentialError::SignatureNotFound(format!(
                    "signature not prepared or expired: {e}"
                )),
            );
        }
    };
    let vc_jws =
        vc_jwt_to_jws(&credential_jwt, &CANISTER_SIG_PK, &sig).expect("failed constructing JWS");
    Result::<IssuedCredentialData, IssueCredentialError>::Ok(IssuedCredentialData { vc_jws })
}

#[update]
async fn vc_consent_message(
    req: Icrc21VcConsentMessageRequest,
) -> Result<Icrc21ConsentInfo, Icrc21Error> {
    get_vc_consent_message(
        &req.credential_spec,
        &SupportedLanguage::from(req.preferences),
    )
}

#[update]
async fn derivation_origin(
    req: DerivationOriginRequest,
) -> Result<DerivationOriginData, DerivationOriginError> {
    get_derivation_origin(&req.frontend_hostname)
}

fn get_derivation_origin(hostname: &str) -> Result<DerivationOriginData, DerivationOriginError> {
    CONFIG.with_borrow(|config| {
        let config = config.get();

        // We don't currently rely on the value provided, so if it doesn't match
        // we just print a warning
        if hostname != config.frontend_hostname {
            println!("*** achtung! bad frontend hostname {hostname}");
        }

        Ok(DerivationOriginData {
            origin: config.derivation_origin.clone(),
        })
    })
}

fn verify_credential_spec(spec: &CredentialSpec) -> Result<SupportedCredentialType, String> {
    match spec.credential_type.as_str() {
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
            Ok(UniversityDegree(VC_INSTITUTION_NAME.to_string()))
        }
        "VerifiedAdult" => {
            verify_single_argument(spec, "minAge", ArgumentValue::Int(18))?;
            Ok(VerifiedAdult(18))
        }
        other => Err(format!("Credential {other} is not supported")),
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
            expected_argument, spec.credential_type
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
            "Unsupported value for argument '{expected_argument}': expected '{expected_value}', got '{value}'"
        ));
    }

    let unexpected_arguments: Vec<&String> = arguments
        .keys()
        .filter(|k| k.as_str() != expected_argument)
        .collect();
    if !unexpected_arguments.is_empty() {
        return Err(format!(
            "Unexpected arguments for credential {}: {:?}",
            spec.credential_type, unexpected_arguments
        ));
    }
    Ok(())
}

#[update]
fn add_employee(employee_id: Principal) -> String {
    EMPLOYEES.with_borrow_mut(|employees| employees.insert(employee_id));
    format!("Added employee {employee_id}")
}

#[update]
fn add_graduate(graduate_id: Principal) -> String {
    GRADUATES.with_borrow_mut(|graduates| graduates.insert(graduate_id));
    format!("Added graduate {graduate_id}")
}

#[update]
fn add_adult(adult_id: Principal) -> String {
    ADULTS.with_borrow_mut(|adults| adults.insert(adult_id));
    format!("Added adult {adult_id}")
}

#[query]
pub fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split('?').collect();
    let path = parts[0];
    let sigs_root_hash =
        SIGNATURES.with_borrow(|sigs| pruned(labeled_hash(LABEL_SIG, &sigs.root_hash())));
    let maybe_asset = ASSETS.with_borrow(|assets| {
        assets.get_certified_asset(path, req.certificate_version, Some(sigs_root_hash))
    });

    let mut headers = static_headers();
    match maybe_asset {
        Some(asset) => {
            headers.extend(asset.headers);
            HttpResponse {
                status_code: 200,
                body: ByteBuf::from(asset.content),
                headers,
            }
        }
        None => HttpResponse {
            status_code: 404,
            headers,
            body: ByteBuf::from(format!("Asset {path} not found.")),
        },
    }
}

fn static_headers() -> Vec<HeaderField> {
    vec![("Access-Control-Allow-Origin".to_string(), "*".to_string())]
}

#[update]
fn set_alternative_origins(alternative_origins: String) {
    const ALTERNATIVE_ORIGINS_PATH: &str = "/.well-known/ii-alternative-origins";
    ASSETS.with_borrow_mut(|assets| {
        let asset = Asset {
            url_path: ALTERNATIVE_ORIGINS_PATH.to_string(),
            content: alternative_origins.as_bytes().to_vec(),
            encoding: ContentEncoding::Identity,
            content_type: ContentType::JSON,
        };
        assets.certify_asset(asset, &static_headers())
    });
    update_root_hash()
}

fn main() {}

fn bachelor_degree_credential(
    subject_principal: Principal,
    credential_spec: &CredentialSpec,
) -> String {
    let params = CredentialParams {
        spec: credential_spec.clone(),
        subject_id: did_for_principal(subject_principal),
        credential_id_url: "https://example.edu/credentials/3732".to_string(),
        issuer_url: "https://example.edu".to_string(),
        expiration_timestamp_s: exp_timestamp_s(),
    };
    build_credential_jwt(params)
}

fn dfinity_employment_credential(
    subject_principal: Principal,
    credential_spec: &CredentialSpec,
) -> String {
    let params = CredentialParams {
        spec: credential_spec.clone(),
        subject_id: did_for_principal(subject_principal),
        credential_id_url: "https://employment.info/credentials/42".to_string(),
        issuer_url: "https://employment.info".to_string(),
        expiration_timestamp_s: exp_timestamp_s(),
    };
    build_credential_jwt(params)
}

fn verified_adult_credential(
    subject_principal: Principal,
    credential_spec: &CredentialSpec,
) -> String {
    let params = CredentialParams {
        spec: credential_spec.clone(),
        subject_id: did_for_principal(subject_principal),
        credential_id_url: "https://age_verifier.info/credentials/42".to_string(),
        issuer_url: "https://age_verifier.info".to_string(),
        expiration_timestamp_s: exp_timestamp_s(),
    };
    build_credential_jwt(params)
}

fn exp_timestamp_s() -> u32 {
    ((time() + VC_EXPIRATION_PERIOD_NS) / 1_000_000_000) as u32
}

fn prepare_credential_jwt(
    credential_spec: &CredentialSpec,
    alias_tuple: &AliasTuple,
) -> Result<String, IssueCredentialError> {
    let credential_type = match verify_credential_spec(credential_spec) {
        Ok(credential_type) => credential_type,
        Err(err) => {
            return Err(IssueCredentialError::UnsupportedCredentialSpec(err));
        }
    };
    match credential_type {
        VerifiedEmployee(_) => {
            EMPLOYEES.with_borrow(|employees| {
                verify_authorized_principal(credential_type, alias_tuple, employees)
            })?;
            Ok(dfinity_employment_credential(
                alias_tuple.id_alias,
                credential_spec,
            ))
        }
        UniversityDegree(_) => {
            GRADUATES.with_borrow(|graduates| {
                verify_authorized_principal(credential_type, alias_tuple, graduates)
            })?;
            Ok(bachelor_degree_credential(
                alias_tuple.id_alias,
                credential_spec,
            ))
        }
        VerifiedAdult(_) => {
            ADULTS.with_borrow(|adults| {
                verify_authorized_principal(credential_type, alias_tuple, adults)
            })?;
            Ok(verified_adult_credential(
                alias_tuple.id_alias,
                credential_spec,
            ))
        }
    }
}

fn verify_authorized_principal(
    credential_type: SupportedCredentialType,
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

// Order dependent: do not move above any exposed canister method!
candid::export_service!();

// Assets
static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/dist");
pub fn init_assets() {
    ASSETS.with_borrow_mut(|assets| {
        *assets = CertifiedAssets::certify_assets(
            collect_assets(&ASSET_DIR, Some(fixup_html)),
            &static_headers(),
        );
    });

    update_root_hash()
}
pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub certificate_version: Option<u16>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
}

fn fixup_html(html: &str) -> String {
    let canister_id = api::id();

    // the string we are replacing here is inserted by vite during the front-end build
    html.replace(
            r#"<script type="module" crossorigin src="/index.js"></script>"#,
            &format!(r#"<script data-canister-id="{canister_id}" type="module" crossorigin src="/index.js"></script>"#).to_string()
        )
}

#[cfg(test)]
mod test {
    use crate::__export_service;
    use candid_parser::utils::{service_equal, CandidSource};
    use std::path::Path;

    /// Checks candid interface type equality by making sure that the service in the did file is
    /// equal to the generated interface.
    #[test]
    fn check_candid_interface_compatibility() {
        let canister_interface = __export_service();
        service_equal(
            CandidSource::Text(&canister_interface),
            CandidSource::File(Path::new("vc_demo_issuer.did")),
        )
        .unwrap_or_else(|e| {
            panic!(
                "the canister interface is not equal to the did file: {e:?} \n--- current interface:\n{canister_interface}\n"
            )
        });
    }
}
