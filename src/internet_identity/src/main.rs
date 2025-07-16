use crate::anchor_management::tentative_device_registration;
use crate::anchor_management::tentative_device_registration::{
    TentativeDeviceRegistrationError, TentativeRegistrationInfo, VerifyTentativeDeviceError,
};
use crate::archive::ArchiveState;
use crate::assets::init_assets;
use crate::state::persistent_state;
use crate::stats::event_stats::all_aggregations_top_n;
use anchor_management::registration;
use authz_utils::{
    anchor_operation_with_authz_check, check_authorization, check_authz_and_record_activity,
};
use candid::Principal;
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk::call;
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use internet_identity_interface::archive::types::BufferedEntry;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::openid::{
    OpenIdCredentialAddError, OpenIdCredentialRemoveError, OpenIdDelegationError,
    OpenIdPrepareDelegationResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, GetIdAliasRequest, IdAliasCredentials, PrepareIdAliasError,
    PrepareIdAliasRequest, PreparedIdAlias,
};
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use storage::account::{AccountDelegationError, PrepareAccountDelegation};
use storage::{Salt, Storage};

mod account_management;
mod anchor_management;
mod archive;
mod assets;
mod authz_utils;

/// Type conversions between internal and external types.
mod conversions;
mod delegation;
mod http;
mod ii_domain;
mod openid;
mod state;
mod stats;
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
async fn init_salt() {
    state::init_salt().await;
}

#[update]
fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::enter_device_registration_mode(anchor_number)
}

#[update]
fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::exit_device_registration_mode(anchor_number)
}

#[update]
async fn add_tentative_device(
    anchor_number: AnchorNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    let result =
        tentative_device_registration::add_tentative_device(anchor_number, device_data).await;
    match result {
        Ok(TentativeRegistrationInfo {
            verification_code,
            device_registration_timeout,
        }) => AddTentativeDeviceResponse::AddedTentatively {
            verification_code,
            device_registration_timeout,
        },
        Err(err) => match err {
            TentativeDeviceRegistrationError::DeviceRegistrationModeOff => {
                AddTentativeDeviceResponse::DeviceRegistrationModeOff
            }
            TentativeDeviceRegistrationError::AnotherDeviceTentativelyAdded => {
                AddTentativeDeviceResponse::AnotherDeviceTentativelyAdded
            }
            TentativeDeviceRegistrationError::IdentityUpdateError(err) => {
                // Legacy API traps instead of returning an error.
                trap(String::from(err).as_str())
            }
        },
    }
}

#[update]
fn verify_tentative_device(
    anchor_number: AnchorNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    let result = anchor_operation_with_authz_check(anchor_number, |anchor| {
        tentative_device_registration::verify_tentative_device(
            anchor,
            anchor_number,
            user_verification_code,
        )
    });
    match result {
        Ok(()) => VerifyTentativeDeviceResponse::Verified,
        Err(err) => match err {
            VerifyTentativeDeviceError::DeviceRegistrationModeOff => {
                VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
            }
            VerifyTentativeDeviceError::NoDeviceToVerify => {
                VerifyTentativeDeviceResponse::NoDeviceToVerify
            }
            VerifyTentativeDeviceError::WrongCode { retries_left } => {
                VerifyTentativeDeviceResponse::WrongCode { retries_left }
            }
            VerifyTentativeDeviceError::IdentityUpdateError(err) => {
                // Legacy API traps instead of returning an error.
                trap(String::from(err).as_str())
            }
        },
    }
}

#[update]
async fn create_challenge() -> Challenge {
    registration::create_challenge().await
}

#[update]
fn register(
    device_data: DeviceData,
    challenge_result: ChallengeAttempt,
    temp_key: Option<Principal>,
) -> RegisterResponse {
    anchor_management::registration::register(device_data, challenge_result, temp_key)
}

#[update]
fn add(anchor_number: AnchorNumber, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>(((), anchor_management::add_device(anchor, device_data)))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
fn update(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::update_device(anchor, device_key, device_data),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
fn replace(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::replace_device(anchor_number, anchor, device_key, device_data),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::remove_device(anchor_number, anchor, device_key),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

/// Returns all devices of the anchor (authentication and recovery) but no information about device registrations.
/// Deprecated: use [get_anchor_credentials] instead
#[query]
fn lookup(anchor_number: AnchorNumber) -> Vec<DeviceData> {
    let Ok(anchor) = state::storage_borrow(|storage| storage.read(anchor_number)) else {
        return vec![];
    };
    let mut devices = anchor.into_devices();
    devices.sort_by(|a, b| b.last_usage_timestamp.cmp(&a.last_usage_timestamp));
    devices
        .into_iter()
        .map(DeviceData::from)
        .map(|mut d| {
            // Remove non-public fields.
            d.alias = String::new();
            d.metadata = None;
            d
        })
        .collect()
}

#[query]
fn get_anchor_credentials(anchor_number: AnchorNumber) -> AnchorCredentials {
    let Ok(anchor) = state::storage_borrow(|storage| storage.read(anchor_number)) else {
        return AnchorCredentials::default();
    };

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

#[query]
fn lookup_device_key(credential_id: CredentialId) -> Option<DeviceKeyWithAnchor> {
    anchor_management::lookup_device_key_with_credential_id(&credential_id)
}

#[update] // this is an update call because queries are not (yet) certified
fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    anchor_management::get_anchor_info(anchor_number)
}

#[query]
fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    let Ok(_) = check_authorization(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    delegation::get_principal(anchor_number, frontend)
}

#[update]
async fn prepare_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    let ii_domain = check_authz_and_record_activity(anchor_number)
        .unwrap_or_else(|err| trap(&format!("{err}")));

    account_management::prepare_account_delegation(
        anchor_number,
        frontend,
        None,
        session_key,
        max_time_to_live,
        &ii_domain,
    )
    .await
    .map(
        |PrepareAccountDelegation {
             user_key,
             expiration,
         }| (user_key, expiration),
    )
    .unwrap_or_else(|err| trap(&format!("{err:?}")))
}

#[query]
fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    let Ok(_) = check_authorization(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    account_management::get_account_delegation(
        anchor_number,
        &frontend,
        None,
        session_key,
        expiration,
    )
    .map(GetDelegationResponse::SignedDelegation)
    .unwrap_or(GetDelegationResponse::NoSuchDelegation)
}

#[query]
fn get_accounts(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<Vec<AccountInfo>, GetAccountsError> {
    match check_authorization(anchor_number) {
        Ok(_) => Ok(
            account_management::get_accounts_for_origin(anchor_number, &origin)
                .iter()
                .map(|acc| acc.to_info())
                .collect(),
        ),
        Err(err) => Err(GetAccountsError::Unauthorized(err.principal)),
    }
}

#[update]
fn create_account(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    name: String,
) -> Result<AccountInfo, CreateAccountError> {
    match check_authorization(anchor_number) {
        Ok(_) => {
            // check if this anchor and acc are actually linked
            account_management::create_account_for_origin(anchor_number, origin, name)
                .map(|acc| acc.to_info())
        }
        Err(err) => Err(CreateAccountError::Unauthorized(err.principal)),
    }
}

#[update]
fn update_account(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    update: AccountUpdate,
) -> Result<AccountInfo, UpdateAccountError> {
    match check_authorization(anchor_number) {
        Ok(_) => account_management::update_account_for_origin(
            anchor_number,
            account_number,
            origin,
            update,
        )
        .map(|acc| acc.to_info()),
        Err(err) => Err(UpdateAccountError::Unauthorized(err.principal)),
    }
}

#[update]
async fn prepare_account_delegation(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<PrepareAccountDelegation, AccountDelegationError> {
    match check_authz_and_record_activity(anchor_number) {
        Ok(ii_domain) => {
            account_management::prepare_account_delegation(
                anchor_number,
                origin,
                account_number,
                session_key,
                max_ttl,
                &ii_domain,
            )
            .await
        }
        Err(err) => Err(err.into()),
    }
}

#[query]
fn get_account_delegation(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    match check_authorization(anchor_number) {
        Ok(_) => account_management::get_account_delegation(
            anchor_number,
            &origin,
            account_number,
            session_key,
            expiration,
        ),
        Err(err) => Err(err.into()),
    }
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[update]
fn http_request_update(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[query]
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

    let event_aggregations = all_aggregations_top_n(100);

    state::storage_borrow(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_anchor_number_range(),
        users_registered: storage.anchor_count() as u64,
        archive_info,
        canister_creation_cycles_cost,
        storage_layout_version: storage.version(),
        event_aggregations,
    })
}

#[query]
fn config() -> InternetIdentityInit {
    let archive_config = match state::archive_state() {
        ArchiveState::NotConfigured => None,
        ArchiveState::Configured { config } | ArchiveState::CreationInProgress { config, .. } => {
            Some(config)
        }
        ArchiveState::Created { config, .. } => Some(config),
    };
    let user_range = state::storage_borrow(|s| s.assigned_anchor_number_range());
    persistent_state(|persistent_state| InternetIdentityInit {
        assigned_user_number_range: Some(user_range),
        archive_config,
        canister_creation_cycles_cost: Some(persistent_state.canister_creation_cycles_cost),
        register_rate_limit: Some(persistent_state.registration_rate_limit.clone()),
        captcha_config: Some(persistent_state.captcha_config.clone()),
        related_origins: persistent_state.related_origins.clone(),
        new_flow_origins: persistent_state.new_flow_origins.clone(),
        openid_google: Some(persistent_state.openid_google.clone()),
        analytics_config: Some(persistent_state.analytics_config.clone()),
        fetch_root_key: persistent_state.fetch_root_key,
        enable_dapps_explorer: persistent_state.enable_dapps_explorer,
        is_production: persistent_state.is_production,
        dummy_auth: Some(persistent_state.dummy_auth.clone()),
    })
}

#[update]
async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    archive::deploy_archive(wasm).await
}

/// Returns a batch of entries _sorted by sequence number_ to be archived.
/// This is an update call because the archive information _must_ be certified.
/// Only callable by this IIs archive canister.
#[update]
fn fetch_entries() -> Vec<BufferedEntry> {
    archive::fetch_entries()
}

/// Removes all buffered archive entries up to sequence number.
/// Only callable by this IIs archive canister.
#[update]
fn acknowledge_entries(sequence_number: u64) {
    archive::acknowledge_entries(sequence_number)
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    state::init_new();
    initialize(maybe_arg);
}

#[post_upgrade]
fn post_upgrade(maybe_arg: Option<InternetIdentityInit>) {
    state::init_from_stable_memory();
    // load the persistent state after initializing storage as it manages the respective stable cell
    state::load_persistent_state();

    initialize(maybe_arg);
}

fn initialize(maybe_arg: Option<InternetIdentityInit>) {
    // Apply arguments
    apply_install_arg(maybe_arg);

    // Get config that possibly has been updated above
    let config = config();

    // Initiate assets and OpenID providers
    init_assets(&config);
    update_root_hash();
    if let Some(Some(openid_config)) = config.openid_google {
        openid::setup_google(openid_config);
    }
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
                persistent_state.registration_rate_limit = rate_limit;
            })
        }
        if let Some(captcha_config) = arg.captcha_config {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.captcha_config = captcha_config;
            })
        }
        if let Some(related_origins) = arg.related_origins {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.related_origins = Some(related_origins);
            })
        }
        if let Some(new_flow_origins) = arg.new_flow_origins {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.new_flow_origins = Some(new_flow_origins);
            })
        }
        if let Some(openid_google) = arg.openid_google {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.openid_google = openid_google;
            })
        }
        if let Some(analytics_config) = arg.analytics_config {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.analytics_config = analytics_config;
            })
        }
        if let Some(fetch_root_key) = arg.fetch_root_key {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.fetch_root_key = Some(fetch_root_key);
            })
        }
        if let Some(enable_dapps_explorer) = arg.enable_dapps_explorer {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.enable_dapps_explorer = Some(enable_dapps_explorer);
            })
        }
        if let Some(is_production) = arg.is_production {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.is_production = Some(is_production);
            })
        }
        if let Some(dummy_auth) = arg.dummy_auth {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.dummy_auth = dummy_auth;
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

/// Calls raw rand to retrieve a random salt (32 bytes).
async fn random_salt() -> Salt {
    let res: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get salt: {err}")),
    };
    let salt: Salt = res[..].try_into().unwrap_or_else(|_| {
        trap(&format!(
            "expected raw randomness to be of length 32, got {}",
            res.len()
        ));
    });
    salt
}

/// New v2 API that aims to eventually replace the current API.
/// The v2 API:
/// * uses terminology more aligned with the front-end and is more consistent in its naming.
/// * uses [Result] return types consistently.
///
/// TODO, API v2 rollout plan:
/// 1. Develop API v2 far enough so that front-ends can switch to it.
/// 2. Deprecate the old API.
/// 3. Add additional errors to the API v2 return type. The canister should no longer trap on invalid
///    input.
/// 4. Add additional features to the API v2, that were not possible with the old API.
mod v2_api {
    use crate::{
        anchor_management::tentative_device_registration::ValidatedRegistrationId,
        state::get_identity_number_by_registration_id,
    };

    use super::*;

    #[query]
    fn identity_authn_info(identity_number: IdentityNumber) -> Result<IdentityAuthnInfo, ()> {
        let Ok(anchor) = state::storage_borrow(|storage| storage.read(identity_number)) else {
            return Ok(IdentityAuthnInfo {
                authn_methods: vec![],
                recovery_authn_methods: vec![],
            });
        };

        let authn_info = anchor.into_devices().into_iter().fold(
            IdentityAuthnInfo {
                authn_methods: vec![],
                recovery_authn_methods: vec![],
            },
            |mut authn_info, device| {
                let purpose = device.purpose;

                let authn_method = if let Some(credential_id) = device.credential_id {
                    AuthnMethod::WebAuthn(WebAuthn {
                        credential_id,
                        pubkey: device.pubkey,
                    })
                } else {
                    AuthnMethod::PubKey(PublicKeyAuthn {
                        pubkey: device.pubkey,
                    })
                };
                match purpose {
                    Purpose::Authentication => authn_info.authn_methods.push(authn_method),
                    Purpose::Recovery => authn_info.recovery_authn_methods.push(authn_method),
                }
                authn_info
            },
        );
        Ok(authn_info)
    }

    #[update]
    async fn identity_registration_start() -> Result<IdRegNextStepResult, IdRegStartError> {
        registration::registration_flow_v2::identity_registration_start().await
    }

    #[update]
    async fn check_captcha(arg: CheckCaptchaArg) -> Result<IdRegNextStepResult, CheckCaptchaError> {
        registration::registration_flow_v2::check_captcha(arg).await
    }

    #[update]
    fn identity_registration_finish(
        arg: IdRegFinishArg,
    ) -> Result<IdRegFinishResult, IdRegFinishError> {
        registration::registration_flow_v2::identity_registration_finish(
            CreateIdentityData::PubkeyAuthn(arg),
        )
    }

    #[update]
    fn identity_info(identity_number: IdentityNumber) -> Result<IdentityInfo, IdentityInfoError> {
        check_authz_and_record_activity(identity_number).map_err(IdentityInfoError::from)?;
        let anchor_info = anchor_management::get_anchor_info(identity_number);

        let metadata = state::anchor(identity_number)
            .identity_metadata()
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(|(k, v)| (k, MetadataEntryV2::from(v)))
            .collect();

        let identity_info = IdentityInfo {
            authn_methods: anchor_info
                .devices
                .into_iter()
                .map(AuthnMethodData::from)
                .collect(),
            authn_method_registration: anchor_info
                .device_registration
                .map(AuthnMethodRegistration::from),
            openid_credentials: anchor_info.openid_credentials,
            metadata,
            name: anchor_info.name,
        };
        Ok(identity_info)
    }

    #[update]
    fn authn_method_add(
        identity_number: IdentityNumber,
        authn_method: AuthnMethodData,
    ) -> Result<(), AuthnMethodAddError> {
        DeviceWithUsage::try_from(authn_method)
            .map(|device| add(identity_number, DeviceData::from(device)))
            .map_err(|err| AuthnMethodAddError::InvalidMetadata(err.to_string()))
    }

    #[update]
    fn authn_method_remove(
        identity_number: IdentityNumber,
        public_key: PublicKey,
    ) -> Result<(), ()> {
        remove(identity_number, public_key);
        Ok(())
    }

    #[update]
    fn authn_method_replace(
        identity_number: IdentityNumber,
        authn_method_pk: PublicKey,
        new_authn_method: AuthnMethodData,
    ) -> Result<(), AuthnMethodReplaceError> {
        let new_device = DeviceWithUsage::try_from(new_authn_method)
            .map(DeviceData::from)
            .map_err(|err| AuthnMethodReplaceError::InvalidMetadata(err.to_string()))?;
        replace(identity_number, authn_method_pk, new_device);
        Ok(())
    }

    #[update]
    fn authn_method_metadata_replace(
        identity_number: IdentityNumber,
        authn_method_pk: PublicKey,
        new_metadata: HashMap<String, MetadataEntryV2>,
    ) -> Result<(), AuthnMethodMetadataReplaceError> {
        let anchor_info = get_anchor_info(identity_number);
        let Some(device) = anchor_info
            .into_device_data()
            .into_iter()
            .find(|d| d.pubkey == authn_method_pk)
        else {
            return Err(AuthnMethodMetadataReplaceError::AuthnMethodNotFound);
        };

        // We need to assign the metadata to an AuthnMethodData and then convert that to DeviceData in order to get
        // the correct metadata validation with respect to reserved keys, etc.
        let mut authn_method = AuthnMethodData::from(device);
        authn_method.metadata = new_metadata;
        let device = DeviceWithUsage::try_from(authn_method)
            .map(DeviceData::from)
            .map_err(|err| AuthnMethodMetadataReplaceError::InvalidMetadata(err.to_string()))?;
        update(identity_number, authn_method_pk, device);
        Ok(())
    }

    #[update]
    fn authn_method_security_settings_replace(
        identity_number: IdentityNumber,
        authn_method_pk: PublicKey,
        new_security_settings: AuthnMethodSecuritySettings,
    ) -> Result<(), AuthnMethodSecuritySettingsReplaceError> {
        let anchor_info = get_anchor_info(identity_number);
        let Some(mut device) = anchor_info
            .into_device_data()
            .into_iter()
            .find(|d| d.pubkey == authn_method_pk)
        else {
            return Err(AuthnMethodSecuritySettingsReplaceError::AuthnMethodNotFound);
        };

        device.protection = DeviceProtection::from(new_security_settings.protection);
        device.purpose = Purpose::from(new_security_settings.purpose);
        update(identity_number, authn_method_pk, device);
        Ok(())
    }

    #[update]
    fn identity_metadata_replace(
        identity_number: IdentityNumber,
        metadata: HashMap<String, MetadataEntryV2>,
    ) -> Result<(), IdentityMetadataReplaceError> {
        anchor_operation_with_authz_check(identity_number, |anchor| {
            let metadata = metadata
                .into_iter()
                .map(|(k, v)| (k, MetadataEntry::from(v)))
                .collect();
            Ok::<_, IdentityMetadataReplaceError>((
                (),
                anchor_management::identity_metadata_replace(anchor, metadata)
                    .map_err(IdentityMetadataReplaceError::from)?,
            ))
        })
    }

    #[update]
    fn identity_properties_replace(
        identity_number: IdentityNumber,
        properties: IdentityPropertiesReplace,
    ) -> Result<(), IdentityPropertiesReplaceError> {
        anchor_operation_with_authz_check(identity_number, |anchor| {
            Ok::<_, IdentityPropertiesReplaceError>((
                (),
                anchor_management::identity_properties_replace(anchor, properties)
                    .map_err(IdentityPropertiesReplaceError::from)?,
            ))
        })
    }

    #[update]
    fn authn_method_registration_mode_enter(
        identity_number: IdentityNumber,
        id: Option<RegistrationId>,
    ) -> Result<RegistrationModeInfo, AuthnMethodRegistrationModeEnterError> {
        check_authz_and_record_activity(identity_number)
            .map_err(AuthnMethodRegistrationModeEnterError::from)?;
        match id {
            Some(reg_id) => {
                let expiration = tentative_device_registration::enter_device_registration_mode_v2(
                    identity_number,
                    ValidatedRegistrationId::try_new(reg_id)
                        .map_err(AuthnMethodRegistrationModeEnterError::InvalidRegistrationId)?,
                )?;
                Ok(RegistrationModeInfo { expiration })
            }
            None => {
                let timeout =
                    tentative_device_registration::enter_device_registration_mode(identity_number);
                Ok(RegistrationModeInfo {
                    expiration: timeout,
                })
            }
        }
    }

    #[update]
    fn authn_method_registration_mode_exit(identity_number: IdentityNumber) -> Result<(), ()> {
        exit_device_registration_mode(identity_number);
        Ok(())
    }

    #[update]
    async fn authn_method_register(
        identity_number: IdentityNumber,
        authn_method: AuthnMethodData,
    ) -> Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError> {
        let device = DeviceWithUsage::try_from(authn_method)
            .map_err(|err| AuthnMethodRegisterError::InvalidMetadata(err.to_string()))?;
        let result = add_tentative_device(identity_number, DeviceData::from(device)).await;
        match result {
            AddTentativeDeviceResponse::AddedTentatively {
                device_registration_timeout,
                verification_code,
            } => Ok(AuthnMethodConfirmationCode {
                expiration: device_registration_timeout,
                confirmation_code: verification_code,
            }),
            AddTentativeDeviceResponse::DeviceRegistrationModeOff => {
                Err(AuthnMethodRegisterError::RegistrationModeOff)
            }
            AddTentativeDeviceResponse::AnotherDeviceTentativelyAdded => {
                Err(AuthnMethodRegisterError::RegistrationAlreadyInProgress)
            }
        }
    }

    #[update]
    fn authn_method_confirm(
        identity_number: IdentityNumber,
        confirmation_code: String,
    ) -> Result<(), AuthnMethodConfirmationError> {
        let response = verify_tentative_device(identity_number, confirmation_code);
        match response {
            VerifyTentativeDeviceResponse::Verified => Ok(()),
            VerifyTentativeDeviceResponse::WrongCode { retries_left } => {
                Err(AuthnMethodConfirmationError::WrongCode { retries_left })
            }
            VerifyTentativeDeviceResponse::DeviceRegistrationModeOff => {
                Err(AuthnMethodConfirmationError::RegistrationModeOff)
            }
            VerifyTentativeDeviceResponse::NoDeviceToVerify => {
                Err(AuthnMethodConfirmationError::NoAuthnMethodToConfirm)
            }
        }
    }

    #[query]
    fn lookup_by_registration_mode_id(id: String) -> Option<IdentityNumber> {
        let validated_id = match ValidatedRegistrationId::try_new(id) {
            Ok(id) => id,
            Err(_) => return None,
        };
        get_identity_number_by_registration_id(&validated_id)
    }
}

/// API for OpenID credentials
mod openid_api {
    use crate::anchor_management::{
        add_openid_credential, lookup_anchor_with_openid_credential, registration,
        remove_openid_credential, update_openid_credential,
    };
    use crate::authz_utils::{anchor_operation_with_authz_check, IdentityUpdateError};
    use crate::openid::{self, OpenIdCredentialKey};
    use crate::storage::anchor::AnchorError;
    use crate::{
        state, IdentityNumber, OpenIdCredentialAddError, OpenIdCredentialRemoveError,
        OpenIdDelegationError, OpenIdPrepareDelegationResponse, SessionKey, Timestamp,
    };
    use ic_cdk::caller;
    use ic_cdk_macros::{query, update};
    use internet_identity_interface::internet_identity::types::{
        CreateIdentityData, IdRegFinishError, IdRegFinishResult, OpenIDRegFinishArg,
        SignedDelegation,
    };

    impl From<IdentityUpdateError> for OpenIdCredentialAddError {
        fn from(_: IdentityUpdateError) -> Self {
            OpenIdCredentialAddError::Unauthorized(caller())
        }
    }
    impl From<IdentityUpdateError> for OpenIdCredentialRemoveError {
        fn from(_: IdentityUpdateError) -> Self {
            OpenIdCredentialRemoveError::Unauthorized(caller())
        }
    }

    #[update]
    fn openid_identity_registration_finish(
        arg: OpenIDRegFinishArg,
    ) -> Result<IdRegFinishResult, IdRegFinishError> {
        openid::with_provider(&arg.jwt, |provider| provider.verify(&arg.jwt, &arg.salt))?;
        registration::registration_flow_v2::identity_registration_finish(
            CreateIdentityData::OpenID(arg),
        )
    }

    #[update]
    fn openid_credential_add(
        identity_number: IdentityNumber,
        jwt: String,
        salt: [u8; 32],
    ) -> Result<(), OpenIdCredentialAddError> {
        anchor_operation_with_authz_check(identity_number, |anchor| {
            let openid_credential =
                openid::with_provider(&jwt, |provider| provider.verify(&jwt, &salt))?;
            add_openid_credential(anchor, openid_credential)
                .map(|operation| ((), operation))
                .map_err(|err| match err {
                    AnchorError::OpenIdCredentialAlreadyRegistered => {
                        OpenIdCredentialAddError::OpenIdCredentialAlreadyRegistered
                    }
                    err => OpenIdCredentialAddError::InternalCanisterError(err.to_string()),
                })
        })
    }

    #[update]
    fn openid_credential_remove(
        identity_number: IdentityNumber,
        openid_credential_key: OpenIdCredentialKey,
    ) -> Result<(), OpenIdCredentialRemoveError> {
        anchor_operation_with_authz_check(identity_number, |anchor| {
            remove_openid_credential(anchor, &openid_credential_key)
                .map(|operation| ((), operation))
                .map_err(|err| match err {
                    AnchorError::OpenIdCredentialNotFound => {
                        OpenIdCredentialRemoveError::OpenIdCredentialNotFound
                    }
                    err => OpenIdCredentialRemoveError::InternalCanisterError(err.to_string()),
                })
        })
    }

    #[update]
    async fn openid_prepare_delegation(
        jwt: String,
        salt: [u8; 32],
        session_key: SessionKey,
    ) -> Result<OpenIdPrepareDelegationResponse, OpenIdDelegationError> {
        let openid_credential =
            openid::with_provider(&jwt, |provider| provider.verify(&jwt, &salt))
                .map_err(|_| OpenIdDelegationError::JwtVerificationFailed)?;

        let anchor_number = lookup_anchor_with_openid_credential(&openid_credential.key())
            .ok_or(OpenIdDelegationError::NoSuchAnchor)?;

        // Update anchor with latest OpenID credential from JWT so latest metadata is stored,
        // this means all data except the `last_used_timestamp` e.g. `name`, `email` and `picture`.
        let mut anchor = state::anchor(anchor_number);
        update_openid_credential(&mut anchor, openid_credential.clone())
            .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;
        state::storage_borrow_mut(|storage| storage.update(anchor))
            .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;

        let (user_key, expiration) = openid_credential
            .prepare_jwt_delegation(session_key, anchor_number)
            .await;

        // Checking again because the association could've changed during the .await
        let still_anchor_number = lookup_anchor_with_openid_credential(&openid_credential.key())
            .ok_or(OpenIdDelegationError::NoSuchAnchor)?;

        if anchor_number != still_anchor_number {
            return Err(OpenIdDelegationError::NoSuchAnchor);
        }

        Ok(OpenIdPrepareDelegationResponse {
            user_key,
            expiration,
            anchor_number,
        })
    }

    #[query]
    fn openid_get_delegation(
        jwt: String,
        salt: [u8; 32],
        session_key: SessionKey,
        expiration: Timestamp,
    ) -> Result<SignedDelegation, OpenIdDelegationError> {
        let openid_credential =
            openid::with_provider(&jwt, |provider| provider.verify(&jwt, &salt))
                .map_err(|_| OpenIdDelegationError::JwtVerificationFailed)?;

        match lookup_anchor_with_openid_credential(&openid_credential.key()) {
            Some(anchor_number) => {
                openid_credential.get_jwt_delegation(session_key, expiration, anchor_number)
            }
            None => Err(OpenIdDelegationError::NoSuchAnchor),
        }
    }
}

/// API for the attribute sharing mvp
mod attribute_sharing_mvp {
    use super::*;

    #[update]
    async fn prepare_id_alias(
        req: PrepareIdAliasRequest,
    ) -> Result<PreparedIdAlias, PrepareIdAliasError> {
        check_authz_and_record_activity(req.identity_number).map_err(PrepareIdAliasError::from)?;
        let prepared_id_alias = vc_mvp::prepare_id_alias(
            req.identity_number,
            vc_mvp::InvolvedDapps {
                relying_party: req.relying_party.clone(),
                issuer: req.issuer.clone(),
            },
        )
        .await;
        Ok(prepared_id_alias)
    }

    #[query]
    fn get_id_alias(req: GetIdAliasRequest) -> Result<IdAliasCredentials, GetIdAliasError> {
        check_authorization(req.identity_number).map_err(GetIdAliasError::from)?;
        vc_mvp::get_id_alias(
            req.identity_number,
            vc_mvp::InvolvedDapps {
                relying_party: req.relying_party,
                issuer: req.issuer,
            },
            &req.rp_id_alias_jwt,
            &req.issuer_id_alias_jwt,
        )
    }
}

fn main() {}

// Order dependent: do not move above any exposed canister method!
candid::export_service!();

#[cfg(test)]
mod test {
    use crate::__export_service;
    use candid_parser::utils::{service_equal, CandidSource};
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
            panic!("the canister code interface is not equal to the did file: {e:?}")
        });
    }
}
