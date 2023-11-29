use crate::anchor_management::{post_operation_bookkeeping, tentative_device_registration};
use crate::archive::ArchiveState;
use crate::assets::init_assets;
use crate::ii_domain::IIDomain;
use crate::storage::anchor::Anchor;
use candid::{candid_method, Principal};
use canister_sig_util::signature_map::LABEL_SIG;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use internet_identity_interface::archive::types::{BufferedEntry, Operation};
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasRequest, GetIdAliasResponse, PrepareIdAliasRequest, PrepareIdAliasResponse,
};
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use storage::{Salt, Storage};

mod activity_stats;
mod anchor_management;
mod archive;
mod assets;
mod delegation;
mod hash;
mod http;
mod ii_domain;
mod state;
mod storage;
mod vc_mvp;

// Some time helpers
const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}
const MINUTE_NS: u64 = secs_to_nanos(60);
const HOUR_NS: u64 = 60 * MINUTE_NS;
const DAY_NS: u64 = 24 * HOUR_NS;

// Note: concatenating const &str is a hassle in rust. It seemed easiest to just repeat.
const IC0_APP_DOMAIN: &str = "identity.ic0.app";
const IC0_APP_ORIGIN: &str = "https://identity.ic0.app";
const INTERNETCOMPUTER_ORG_DOMAIN: &str = "identity.internetcomputer.org";
const INTERNETCOMPUTER_ORG_ORIGIN: &str = "https://identity.internetcomputer.org";

#[update]
#[candid_method]
async fn init_salt() {
    state::init_salt().await;
}

#[update]
#[candid_method]
fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    authenticate_and_record_activity(anchor_number);
    tentative_device_registration::enter_device_registration_mode(anchor_number)
}

#[update]
#[candid_method]
fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    authenticate_and_record_activity(anchor_number);
    tentative_device_registration::exit_device_registration_mode(anchor_number)
}

#[update]
#[candid_method]
async fn add_tentative_device(
    anchor_number: AnchorNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    tentative_device_registration::add_tentative_device(anchor_number, device_data).await
}

#[update]
#[candid_method]
fn verify_tentative_device(
    anchor_number: AnchorNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    authenticated_anchor_operation(anchor_number, |anchor| {
        tentative_device_registration::verify_tentative_device(
            anchor,
            anchor_number,
            user_verification_code,
        )
    })
}

#[update]
#[candid_method]
async fn create_challenge() -> Challenge {
    anchor_management::registration::create_challenge().await
}

#[update]
#[candid_method]
fn register(
    device_data: DeviceData,
    challenge_result: ChallengeAttempt,
    temp_key: Option<Principal>,
) -> RegisterResponse {
    anchor_management::registration::register(device_data, challenge_result, temp_key)
}

#[update]
#[candid_method]
fn add(anchor_number: AnchorNumber, device_data: DeviceData) {
    authenticated_anchor_operation(anchor_number, |anchor| {
        Ok(((), anchor_management::add(anchor, device_data)))
    })
}

#[update]
#[candid_method]
fn update(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    authenticated_anchor_operation(anchor_number, |anchor| {
        Ok((
            (),
            anchor_management::update(anchor, device_key, device_data),
        ))
    })
}

#[update]
#[candid_method]
fn replace(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    authenticated_anchor_operation(anchor_number, |anchor| {
        Ok((
            (),
            anchor_management::replace(anchor_number, anchor, device_key, device_data),
        ))
    })
}

#[update]
#[candid_method]
fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    authenticated_anchor_operation(anchor_number, |anchor| {
        Ok((
            (),
            anchor_management::remove(anchor_number, anchor, device_key),
        ))
    })
}

/// Returns all devices of the anchor (authentication and recovery) but no information about device registrations.
/// Deprecated: use [get_anchor_credentials] instead
#[query]
#[candid_method(query)]
fn lookup(anchor_number: AnchorNumber) -> Vec<DeviceData> {
    state::storage_borrow(|storage| {
        storage
            .read(anchor_number)
            .unwrap_or_default()
            .into_devices()
            .into_iter()
            .map(DeviceData::from)
            .map(|mut d| {
                // Remove non-public fields.
                d.alias = "".to_string();
                d
            })
            .collect()
    })
}

#[query]
#[candid_method(query)]
fn get_anchor_credentials(anchor_number: AnchorNumber) -> AnchorCredentials {
    let anchor = state::storage_borrow(|storage| storage.read(anchor_number).unwrap_or_default());

    anchor.into_devices().into_iter().fold(
        AnchorCredentials {
            credentials: vec![],
            recovery_credentials: vec![],
            recovery_phrases: vec![],
        },
        |mut credentials, device| {
            if device.key_type == KeyType::SeedPhrase {
                credentials.recovery_phrases.push(device.pubkey);
            } else if let Some(credential_id) = device.credential_id {
                let credential = WebAuthnCredential {
                    pubkey: device.pubkey,
                    credential_id,
                };
                if device.purpose == Purpose::Recovery {
                    credentials.recovery_credentials.push(credential);
                } else {
                    credentials.credentials.push(credential);
                }
            }
            credentials
        },
    )
}

#[update] // this is an update call because queries are not (yet) certified
#[candid_method]
fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    authenticate_and_record_activity(anchor_number);
    anchor_management::get_anchor_info(anchor_number)
}

#[query]
#[candid_method(query)]
fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    let Ok(_) = check_authentication(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    delegation::get_principal(anchor_number, frontend)
}

#[update]
#[candid_method]
async fn prepare_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    let ii_domain = authenticate_and_record_activity(anchor_number);
    delegation::prepare_delegation(
        anchor_number,
        frontend,
        session_key,
        max_time_to_live,
        &ii_domain,
    )
    .await
}

#[query]
#[candid_method(query)]
fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    let Ok(_) = check_authentication(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    delegation::get_delegation(anchor_number, frontend, session_key, expiration)
}

#[query]
#[candid_method(query)]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[update]
#[candid_method]
fn http_request_update(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[query]
#[candid_method(query)]
fn stats() -> InternetIdentityStats {
    let archive_info = match state::archive_state() {
        ArchiveState::NotConfigured => ArchiveInfo {
            archive_canister: None,
            archive_config: None,
        },
        ArchiveState::Configured { config } | ArchiveState::CreationInProgress { config, .. } => {
            ArchiveInfo {
                archive_canister: None,
                archive_config: Some(config),
            }
        }
        ArchiveState::Created { data, config } => ArchiveInfo {
            archive_canister: Some(data.archive_canister),
            archive_config: Some(config),
        },
    };

    let canister_creation_cycles_cost =
        state::persistent_state(|persistent_state| persistent_state.canister_creation_cycles_cost);

    let (latest_delegation_origins, max_num_latest_delegation_origins) =
        state::persistent_state(|persistent_state| {
            let origins = persistent_state
                .latest_delegation_origins
                .as_ref()
                .map(|latest_delegation_origins| {
                    latest_delegation_origins.keys().cloned().collect()
                })
                .unwrap_or_default();
            (
                origins,
                persistent_state.max_num_latest_delegation_origins.unwrap(),
            )
        });

    state::storage_borrow(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_anchor_number_range(),
        users_registered: storage.anchor_count() as u64,
        archive_info,
        canister_creation_cycles_cost,
        storage_layout_version: storage.version(),
        max_num_latest_delegation_origins,
        latest_delegation_origins,
    })
}

#[update]
#[candid_method]
async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    archive::deploy_archive(wasm).await
}

/// Returns a batch of entries _sorted by sequence number_ to be archived.
/// This is an update call because the archive information _must_ be certified.
/// Only callable by this IIs archive canister.
#[update]
#[candid_method]
fn fetch_entries() -> Vec<BufferedEntry> {
    archive::fetch_entries()
}

/// Removes all buffered archive entries up to sequence number.
/// Only callable by this IIs archive canister.
#[update]
#[candid_method]
fn acknowledge_entries(sequence_number: u64) {
    archive::acknowledge_entries(sequence_number)
}

#[init]
#[candid_method(init)]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    state::init_new();

    apply_install_arg(maybe_arg);

    // make sure the fully initialized storage configuration is written to stable memory
    state::storage_borrow_mut(|storage| storage.flush());
    update_root_hash();
}

#[post_upgrade]
fn post_upgrade(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    state::init_from_stable_memory();

    // We drop all the signatures on upgrade, users will
    // re-request them if needed.
    update_root_hash();
    // load the persistent state after initializing storage, otherwise the memory address to load it from cannot be calculated
    state::load_persistent_state();

    apply_install_arg(maybe_arg);
}

fn apply_install_arg(maybe_arg: Option<InternetIdentityInit>) {
    if let Some(arg) = maybe_arg {
        if let Some(range) = arg.assigned_user_number_range {
            state::storage_borrow_mut(|storage| {
                storage.set_anchor_number_range(range);
            });
        }
        if let Some(new_config) = arg.archive_config {
            update_archive_config(new_config);
        }
        if let Some(cost) = arg.canister_creation_cycles_cost {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.canister_creation_cycles_cost = cost;
            })
        }
        if let Some(rate_limit) = arg.register_rate_limit {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.registration_rate_limit = Some(rate_limit);
            })
        }
        if let Some(limit) = arg.max_num_latest_delegation_origins {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.max_num_latest_delegation_origins = Some(limit);
            })
        }
        if let Some(limit) = arg.max_inflight_captchas {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.max_inflight_captchas = Some(limit);
            })
        }
    }
}

fn update_archive_config(new_config: ArchiveConfig) {
    state::persistent_state_mut(|persistent_state| match persistent_state.archive_state {
        ArchiveState::NotConfigured => {
            persistent_state.archive_state = ArchiveState::Configured { config: new_config }
        }
        ArchiveState::Configured { ref mut config }
        | ArchiveState::CreationInProgress { ref mut config, .. }
        | ArchiveState::Created { ref mut config, .. } => {
            *config = new_config;
        }
    })
}

#[pre_upgrade]
fn save_persistent_state() {
    state::save_persistent_state();
}

fn update_root_hash() {
    use ic_certification::{fork_hash, labeled_hash};
    state::assets_and_signatures(|assets, sigs| {
        let prefixed_root_hash = fork_hash(
            &assets.root_hash(),
            // NB: sigs have to be added last due to lexicographic order of labels
            &labeled_hash(LABEL_SIG, &sigs.root_hash()),
        );
        set_certified_data(&prefixed_root_hash[..]);
    })
}

/// Authenticates the caller (traps if not authenticated) and updates the device used to authenticate
/// reflecting the current activity. Also updates the aggregated stats on daily and monthly active users.
///
/// Note: this function reads / writes the anchor from / to stable memory. It is intended to be used by functions that
/// do not further modify the anchor.
fn authenticate_and_record_activity(anchor_number: AnchorNumber) -> Option<IIDomain> {
    let Ok((mut anchor, device_key)) = check_authentication(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    let domain = anchor.device(&device_key).unwrap().ii_domain();
    anchor_management::activity_bookkeeping(&mut anchor, &device_key);
    state::storage_borrow_mut(|storage| storage.write(anchor_number, anchor)).unwrap_or_else(
        |err| panic!("last_usage_timestamp update: unable to update anchor {anchor_number}: {err}"),
    );
    domain
}

/// Authenticates the caller (traps if not authenticated) calls the provided function and handles all
/// the necessary bookkeeping for anchor operations.
///
/// * anchor_number: indicates the anchor to be provided op should be called on
/// * op: Function that modifies an anchor and returns a value `R` wrapped in a [Result] indicating
///       success or failure which determines whether additional bookkeeping (on success) is required.
///       On success, the function must also return an [Operation] which is used for archiving purposes.
///       The type `R` is usually bound to an interface type specified in the candid file. This type
///       is either unit or a variant unifying success and error cases (which is why the [Result] has
///       `R` in both success and error positions).
fn authenticated_anchor_operation<R>(
    anchor_number: AnchorNumber,
    op: impl FnOnce(&mut Anchor) -> Result<(R, Operation), R>,
) -> R {
    let Ok((mut anchor, device_key)) = check_authentication(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    anchor_management::activity_bookkeeping(&mut anchor, &device_key);

    let result = op(&mut anchor);

    // write back anchor
    state::storage_borrow_mut(|storage| storage.write(anchor_number, anchor)).unwrap_or_else(
        |err| panic!("unable to update anchor {anchor_number} in stable memory: {err}"),
    );

    match result {
        Ok((ret, operation)) => {
            post_operation_bookkeeping(anchor_number, operation);
            ret
        }
        Err(err) => err,
    }
}

/// Checks if the caller is authenticated against the anchor provided and returns a reference to the device used.
/// Returns an error if the caller cannot be authenticated.
fn check_authentication(anchor_number: AnchorNumber) -> Result<(Anchor, DeviceKey), ()> {
    let anchor = state::anchor(anchor_number);
    let caller = caller();

    for device in anchor.devices() {
        if caller == Principal::self_authenticating(&device.pubkey)
            || state::with_temp_keys_mut(|temp_keys| {
                temp_keys
                    .check_temp_key(&caller, &device.pubkey, anchor_number)
                    .is_ok()
            })
        {
            return Ok((anchor.clone(), device.pubkey.clone()));
        }
    }
    Err(())
}

/// New v2 API that aims to eventually replace the current API.
/// The v2 API:
/// * uses terminology more aligned with the front-end and is more consistent in its naming.
/// * uses opt variant return types consistently in order to by able to extend / change return types
///   in the future without breaking changes.
///
/// TODO, API v2 rollout plan:
/// 1. Develop API v2 far enough so that front-ends can switch to it.
/// 2. Deprecate the old API.
/// 3. Add additional errors to the API v2 return type. The canister should no longer trap on invalid
///    input.
/// 4. Add additional features to the API v2, that were not possible with the old API.
mod v2_api {
    use super::*;

    #[update]
    #[candid_method]
    async fn captcha_create() -> Option<CaptchaCreateResponse> {
        let challenge = anchor_management::registration::create_challenge().await;
        Some(CaptchaCreateResponse::Ok(challenge))
    }

    #[update]
    #[candid_method]
    fn identity_register(
        authn_method: AuthnMethodData,
        challenge_result: ChallengeAttempt,
        temp_key: Option<Principal>,
    ) -> Option<IdentityRegisterResponse> {
        let result = match DeviceWithUsage::try_from(authn_method)
            .map_err(|err| IdentityRegisterResponse::InvalidMetadata(err.to_string()))
        {
            Ok(device) => IdentityRegisterResponse::from(register(
                DeviceData::from(device),
                challenge_result,
                temp_key,
            )),
            Err(err) => err,
        };
        Some(result)
    }

    #[update]
    #[candid_method]
    fn identity_info(identity_number: IdentityNumber) -> Option<IdentityInfoResponse> {
        authenticate_and_record_activity(identity_number);
        let anchor_info = anchor_management::get_anchor_info(identity_number);
        let metadata = state::anchor(identity_number)
            .identity_metadata()
            .clone()
            .unwrap_or_default();

        let identity_info = IdentityInfo {
            authn_methods: anchor_info
                .devices
                .into_iter()
                .map(AuthnMethodData::from)
                .collect(),
            authn_method_registration: anchor_info
                .device_registration
                .map(AuthnMethodRegistration::from),
            metadata,
        };
        Some(IdentityInfoResponse::Ok(identity_info))
    }

    #[update]
    #[candid_method]
    fn authn_method_add(
        identity_number: IdentityNumber,
        authn_method: AuthnMethodData,
    ) -> Option<AuthnMethodAddResponse> {
        let result = match DeviceWithUsage::try_from(authn_method)
            .map_err(|err| AuthnMethodAddResponse::InvalidMetadata(err.to_string()))
        {
            Ok(device) => {
                add(identity_number, DeviceData::from(device));
                AuthnMethodAddResponse::Ok
            }
            Err(err) => err,
        };
        Some(result)
    }

    #[update]
    #[candid_method]
    fn authn_method_remove(
        identity_number: IdentityNumber,
        public_key: PublicKey,
    ) -> Option<AuthnMethodRemoveResponse> {
        remove(identity_number, public_key);
        Some(AuthnMethodRemoveResponse::Ok)
    }

    #[update]
    #[candid_method]
    fn identity_metadata_replace(
        identity_number: IdentityNumber,
        metadata: HashMap<String, MetadataEntry>,
    ) -> Option<IdentityMetadataReplaceResponse> {
        let result = authenticated_anchor_operation(identity_number, |anchor| {
            Ok((
                IdentityMetadataReplaceResponse::Ok,
                anchor_management::identity_metadata_replace(anchor, metadata),
            ))
        });
        Some(result)
    }
}

/// API for the attribute sharing mvp
mod attribute_sharing_mvp {
    use super::*;

    #[update]
    #[candid_method]
    async fn prepare_id_alias(req: PrepareIdAliasRequest) -> Option<PrepareIdAliasResponse> {
        let _maybe_ii_domain = authenticate_and_record_activity(req.identity_number);
        let prepared_id_alias = vc_mvp::prepare_id_alias(
            req.identity_number,
            vc_mvp::InvolvedDapps {
                relying_party: req.relying_party.clone(),
                issuer: req.issuer.clone(),
            },
        )
        .await;
        Some(PrepareIdAliasResponse::Ok(prepared_id_alias))
    }

    #[query]
    #[candid_method(query)]
    fn get_id_alias(req: GetIdAliasRequest) -> Option<GetIdAliasResponse> {
        let Ok(_) = check_authentication(req.identity_number) else {
            return Some(GetIdAliasResponse::AuthenticationFailed(format!(
                "{} could not be authenticated.",
                caller()
            )));
        };
        let response = vc_mvp::get_id_alias(
            req.identity_number,
            vc_mvp::InvolvedDapps {
                relying_party: req.relying_party,
                issuer: req.issuer,
            },
            &req.rp_id_alias_jwt,
            &req.issuer_id_alias_jwt,
        );
        Some(response)
    }
}

fn main() {}

// Order dependent: do not move above any function annotated with #[candid_method]!
candid::export_service!();

#[cfg(test)]
mod test {
    use crate::__export_service;
    use candid::utils::{service_equal, CandidSource};
    use std::path::Path;

    /// Checks candid interface type equality by making sure that the service in the did file is
    /// a subtype of the generated interface and vice versa.
    #[test]
    fn check_candid_interface_compatibility() {
        let canister_interface = __export_service();
        service_equal(
            CandidSource::Text(&canister_interface),
            CandidSource::File(Path::new("internet_identity.did")),
        )
        .unwrap_or_else(|e| {
            panic!(
                "the canister code interface is not equal to the did file: {:?}",
                e
            )
        });
    }
}
