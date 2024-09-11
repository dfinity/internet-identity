use crate::anchor_management::tentative_device_registration;
use crate::anchor_management::tentative_device_registration::{
    TentativeDeviceRegistrationError, TentativeRegistrationInfo, VerifyTentativeDeviceError,
};
use crate::archive::ArchiveState;
use crate::assets::init_assets;
use crate::stats::event_stats::all_aggregations_top_n;
use authz_utils::{
    anchor_operation_with_authz_check, check_authorization, check_authz_and_record_activity,
};
use candid::{candid_method, Principal};
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk::call;
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use internet_identity_interface::archive::types::BufferedEntry;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, GetIdAliasRequest, IdAliasCredentials, PrepareIdAliasError,
    PrepareIdAliasRequest, PreparedIdAlias,
};
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use storage::{Salt, Storage};

mod anchor_management;
mod archive;
mod assets;
mod authz_utils;

/// Type conversions between internal and external types.
mod conversions;
mod delegation;
mod http;
mod ii_domain;
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
#[candid_method]
async fn init_salt() {
    state::init_salt().await;
}

#[update]
#[candid_method]
fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::enter_device_registration_mode(anchor_number)
}

#[update]
#[candid_method]
fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::exit_device_registration_mode(anchor_number)
}

#[update]
#[candid_method]
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
#[candid_method]
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
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>(((), anchor_management::add(anchor, device_data)))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
#[candid_method]
fn update(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::update(anchor, device_key, device_data),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
#[candid_method]
fn replace(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::replace(anchor_number, anchor, device_key, device_data),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
#[candid_method]
fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        Ok::<_, String>((
            (),
            anchor_management::remove(anchor_number, anchor, device_key),
        ))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

/// Returns all devices of the anchor (authentication and recovery) but no information about device registrations.
/// Deprecated: use [get_anchor_credentials] instead
#[query]
#[candid_method(query)]
fn lookup(anchor_number: AnchorNumber) -> Vec<DeviceData> {
    let Ok(anchor) = state::storage_borrow(|storage| storage.read(anchor_number)) else {
        return vec![];
    };
    anchor
        .into_devices()
        .into_iter()
        .map(DeviceData::from)
        .map(|mut d| {
            // Remove non-public fields.
            d.alias = "".to_string();
            d.metadata = None;
            d
        })
        .collect()
}

#[query]
#[candid_method(query)]
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

#[update] // this is an update call because queries are not (yet) certified
#[candid_method]
fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    anchor_management::get_anchor_info(anchor_number)
}

#[query]
#[candid_method(query)]
fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    let Ok(_) = check_authorization(anchor_number) else {
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
    let ii_domain = check_authz_and_record_activity(anchor_number)
        .unwrap_or_else(|err| trap(&format!("{err}")));
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
    let Ok(_) = check_authorization(anchor_number) else {
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
    init_assets();
    apply_install_arg(maybe_arg);
    update_root_hash();
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
        if let Some(limit) = arg.max_inflight_captchas {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.max_inflight_captchas = limit;
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
    use super::*;

    #[query]
    #[candid_method(query)]
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
    #[candid_method]
    async fn captcha_create() -> Result<Challenge, ()> {
        let challenge = anchor_management::registration::create_challenge().await;
        Ok(challenge)
    }

    #[update]
    #[candid_method]
    fn identity_register(
        authn_method: AuthnMethodData,
        challenge_result: ChallengeAttempt,
        temp_key: Option<Principal>,
    ) -> Result<IdentityNumber, IdentityRegisterError> {
        let device = DeviceWithUsage::try_from(authn_method)
            .map_err(|err| IdentityRegisterError::InvalidMetadata(err.to_string()))?;

        match register(DeviceData::from(device), challenge_result, temp_key) {
            RegisterResponse::Registered { user_number } => Ok(user_number),
            RegisterResponse::CanisterFull => Err(IdentityRegisterError::CanisterFull),
            RegisterResponse::BadChallenge => Err(IdentityRegisterError::BadCaptcha),
        }
    }

    #[update]
    #[candid_method]
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
            metadata,
        };
        Ok(identity_info)
    }

    #[update]
    #[candid_method]
    fn authn_method_add(
        identity_number: IdentityNumber,
        authn_method: AuthnMethodData,
    ) -> Result<(), AuthnMethodAddError> {
        DeviceWithUsage::try_from(authn_method)
            .map(|device| add(identity_number, DeviceData::from(device)))
            .map_err(|err| AuthnMethodAddError::InvalidMetadata(err.to_string()))
    }

    #[update]
    #[candid_method]
    fn authn_method_remove(
        identity_number: IdentityNumber,
        public_key: PublicKey,
    ) -> Result<(), ()> {
        remove(identity_number, public_key);
        Ok(())
    }

    #[update]
    #[candid_method]
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
    #[candid_method]
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
    #[candid_method]
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
    #[candid_method]
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
    #[candid_method]
    fn authn_method_registration_mode_enter(
        identity_number: IdentityNumber,
    ) -> Result<RegistrationModeInfo, ()> {
        let timeout = enter_device_registration_mode(identity_number);
        Ok(RegistrationModeInfo {
            expiration: timeout,
        })
    }

    #[update]
    #[candid_method]
    fn authn_method_registration_mode_exit(identity_number: IdentityNumber) -> Result<(), ()> {
        exit_device_registration_mode(identity_number);
        Ok(())
    }

    #[update]
    #[candid_method]
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
    #[candid_method]
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
}

/// API for the attribute sharing mvp
mod attribute_sharing_mvp {
    use super::*;

    #[update]
    #[candid_method]
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
    #[candid_method(query)]
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

// Order dependent: do not move above any function annotated with #[candid_method]!
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
            panic!(
                "the canister code interface is not equal to the did file: {:?}",
                e
            )
        });
    }
}
