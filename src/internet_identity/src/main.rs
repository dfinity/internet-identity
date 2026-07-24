use crate::anchor_management::tentative_device_registration;
use crate::archive::ArchiveState;
use crate::assets::init_assets;
use crate::authz_utils::IdentityUpdateError;
use crate::delegation::DelegationAccess;
use crate::state::persistent_state;
use crate::stats::event_stats::all_aggregations_top_n;
use anchor_management::registration;
use authz_utils::check_session_authorization;
use authz_utils::{
    anchor_operation_with_authz_check, check_authorization, check_authz_and_record_activity,
};
use candid::Principal;
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk::call;
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};

use ic_cdk_timers::TimerId;
use internet_identity_interface::archive::types::{BufferedEntry, Operation};
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};
use internet_identity_interface::internet_identity::types::attributes::{
    CertifiedAttributes, GetAttributesError, GetAttributesRequest, GetIcrc3AttributeError,
    GetIcrc3AttributeRequest, GetIcrc3AttributeResponse, ListAvailableAttributesError,
    ListAvailableAttributesRequest, PrepareAttributeError, PrepareAttributeRequest,
    PrepareAttributeResponse, PrepareIcrc3AttributeError, PrepareIcrc3AttributeRequest,
    PrepareIcrc3AttributeResponse, ValidatedGetIcrc3AttributeRequest,
    ValidatedListAvailableAttributesRequest, ValidatedPrepareAttributeRequest,
    ValidatedPrepareIcrc3AttributeRequest,
};
use internet_identity_interface::internet_identity::types::openid::{
    OpenIdCredentialAddError, OpenIdCredentialRemoveError, OpenIdDelegationError,
    OpenIdPrepareDelegationResponse, OpenIdResult, SsoGetDelegationResponse,
    SsoPrepareDelegationResponse,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, GetIdAliasRequest, IdAliasCredentials, PrepareIdAliasError,
    PrepareIdAliasRequest, PreparedIdAlias,
};
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use std::collections::HashMap;
use std::time::Duration;
use storage::account::{AccountDelegationError, PrepareAccountDelegation};
use storage::storable::openid_credential_key::StorableOpenIdCredentialKey;
use storage::{Salt, Storage};

mod account_management;
mod anchor_management;
mod archive;
mod assets;
mod authz_utils;

mod attributes;
/// Type conversions between internal and external types.
mod conversions;
mod delegation;
mod dkim;
mod dmarc;
mod dnssec;
mod doh;
mod email_inbound;
mod email_recovery;
mod http;
mod ii_domain;
mod mcp;
mod mcp_registration;

mod openid;
mod session_delegation;
mod single_flight_cache;
mod state;
mod stats;
mod storage;
mod utils;
mod vc_mvp;
mod verified_emails;

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
const ID_AI_DOMAIN: &str = "id.ai";
const ID_AI_ORIGIN: &str = "https://id.ai";

// ---- SSO credential migration (temporary, see PR-M of
// `docs/ongoing/openid-sso-prod-readiness.md` §8.6) ----
//
// Stored `OpenIdCredential`s grew `sso_domain` / `sso_name` fields. New
// credentials are stamped at verification time, but existing SSO credentials
// in stable storage must be backfilled from the `sso_credential_migration`
// upgrade arg. Backfilling all credentials synchronously in `post_upgrade`
// would blow the instruction limit on II's production canister, so we batch
// the work via an interval timer using the same convention as the prior
// OpenID credential key migration (#3784) and anchor migration (#3713).

/// How long to wait between migration batches.
const SSO_CREDENTIAL_MIGRATION_BATCH_BACKOFF_SECONDS: Duration = Duration::from_secs(1);

/// Maximum number of credential index keys to examine per batch (= per
/// ingress message). Matches the 2 000-per-batch convention used for
/// previous migrations.
const SSO_CREDENTIAL_MIGRATION_BATCH_SIZE: u64 = 2_000;

thread_local! {
    // TODO: Remove these after the data migration is complete.
    static SSO_CREDENTIAL_MIGRATION_ENTRIES: RefCell<Vec<SsoCredentialMigrationEntry>> = const { RefCell::new(Vec::new()) };
    static SSO_CREDENTIAL_MIGRATION_CURSOR: RefCell<Option<StorableOpenIdCredentialKey>> = const { RefCell::new(None) };
    static SSO_CREDENTIAL_MIGRATION_DONE: RefCell<bool> = const { RefCell::new(false) };
    static SSO_CREDENTIAL_MIGRATION_STAMPED: RefCell<u64> = const { RefCell::new(0) };
    static SSO_CREDENTIAL_MIGRATION_ERRORS: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    static SSO_CREDENTIAL_MIGRATION_TIMER_ID: RefCell<Option<TimerId>> = const { RefCell::new(None) };
}

/// Temporary hidden endpoint: returns any per-entry errors encountered
/// during the SSO credential migration.
#[update(hidden = true)]
fn list_sso_credential_migration_errors() -> Vec<String> {
    SSO_CREDENTIAL_MIGRATION_ERRORS.with_borrow(|errors| errors.clone())
}

/// Temporary hidden endpoint: returns `(stamped_credentials, is_done)` so
/// monitoring can track migration progress.
#[query(hidden = true)]
fn sso_credential_migration_status() -> (u64, bool) {
    (
        SSO_CREDENTIAL_MIGRATION_STAMPED.with_borrow(|c| *c),
        SSO_CREDENTIAL_MIGRATION_DONE.with_borrow(|d| *d),
    )
}

/// Process one batch of the SSO credential migration. Bound to the interval
/// timer set up in [`init_sso_credential_migration_timer`]; clears the timer
/// once the migration signals completion.
fn run_sso_credential_migration_batch() {
    if SSO_CREDENTIAL_MIGRATION_DONE.with_borrow(|done| *done) {
        return;
    }

    let entries = SSO_CREDENTIAL_MIGRATION_ENTRIES.with_borrow(|entries| entries.clone());
    let cursor = SSO_CREDENTIAL_MIGRATION_CURSOR.with_borrow(|cursor| cursor.clone());
    let outcome = state::storage_borrow_mut(|storage| {
        storage.migrate_sso_credentials_batch(&entries, cursor, SSO_CREDENTIAL_MIGRATION_BATCH_SIZE)
    });

    SSO_CREDENTIAL_MIGRATION_STAMPED.with_borrow_mut(|count| {
        *count = count.saturating_add(outcome.stamped);
    });
    if !outcome.errors.is_empty() {
        SSO_CREDENTIAL_MIGRATION_ERRORS.with_borrow_mut(|errors| errors.extend(outcome.errors));
    }
    if let Some(next_cursor) = outcome.next_cursor {
        SSO_CREDENTIAL_MIGRATION_CURSOR.replace(Some(next_cursor));
    }

    if outcome.is_done {
        SSO_CREDENTIAL_MIGRATION_DONE.replace(true);
        SSO_CREDENTIAL_MIGRATION_TIMER_ID.with_borrow_mut(|id_slot| {
            if let Some(timer_id) = id_slot.take() {
                ic_cdk_timers::clear_timer(timer_id);
            }
        });
        let stamped = SSO_CREDENTIAL_MIGRATION_STAMPED.with_borrow(|c| *c);
        ic_cdk::println!("SSO credential migration COMPLETED ({stamped} credentials stamped).");
    }
}

/// Start the interval timer driving [`run_sso_credential_migration_batch`].
/// Safe to call from both `init` (the first batch will immediately see an
/// empty entry list or index and mark the migration done) and `post_upgrade`.
fn init_sso_credential_migration_timer() {
    let timer_id = ic_cdk_timers::set_timer_interval(
        SSO_CREDENTIAL_MIGRATION_BATCH_BACKOFF_SECONDS,
        run_sso_credential_migration_batch,
    );
    SSO_CREDENTIAL_MIGRATION_TIMER_ID.with_borrow_mut(|id_slot| {
        if let Some(old_id) = id_slot.replace(timer_id) {
            ic_cdk_timers::clear_timer(old_id);
        }
    });
}

#[update]
async fn init_salt() {
    state::init_salt().await;
}

#[update]
fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::enter_device_registration_mode(anchor_number, None)
        // Legacy API traps instead of returning an error.
        .unwrap_or_else(|err| match err {
            AuthnMethodRegistrationModeEnterError::AlreadyInProgress => trap("Already in progress"),
            AuthnMethodRegistrationModeEnterError::InternalCanisterError(message) => trap(&message),
            AuthnMethodRegistrationModeEnterError::Unauthorized(_)
            | AuthnMethodRegistrationModeEnterError::InvalidRegistrationId(_) => {
                trap("Unreachable error")
            }
        })
}

#[update]
fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    check_authz_and_record_activity(anchor_number).unwrap_or_else(|err| trap(&format!("{err}")));
    tentative_device_registration::exit_device_registration_mode(anchor_number);
}

#[update]
async fn add_tentative_device(
    anchor_number: AnchorNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    let result =
        tentative_device_registration::add_tentative_device(anchor_number, device_data).await;
    match result {
        Ok(AuthnMethodConfirmationCode {
            confirmation_code,
            expiration,
        }) => AddTentativeDeviceResponse::AddedTentatively {
            verification_code: confirmation_code,
            device_registration_timeout: expiration,
        },
        Err(err) => match err {
            AuthnMethodRegisterError::RegistrationModeOff => {
                AddTentativeDeviceResponse::DeviceRegistrationModeOff
            }
            AuthnMethodRegisterError::RegistrationAlreadyInProgress => {
                AddTentativeDeviceResponse::AnotherDeviceTentativelyAdded
            }
            AuthnMethodRegisterError::PasskeyWithThisPublicKeyIsAlreadyUsed => {
                AddTentativeDeviceResponse::PasskeyWithThisPublicKeyIsAlreadyUsed
            }
            AuthnMethodRegisterError::InvalidMetadata(_) => {
                trap("Unreachable error");
            }
            AuthnMethodRegisterError::NotSelfAuthenticating(_) => {
                trap("Unreachable error");
            }
        },
    }
}

#[update]
fn verify_tentative_device(
    anchor_number: AnchorNumber,
    user_confirmation_code: DeviceConfirmationCode,
) -> VerifyTentativeDeviceResponse {
    let (mut anchor, authorization_key) = check_authorization(anchor_number)
        .unwrap_or_else(|err| trap(&format!("{} could not be authenticated.", err.principal)));
    match tentative_device_registration::confirm_tentative_device_or_session(
        anchor_number,
        user_confirmation_code,
    ) {
        Ok(maybe_confirmed_device) => {
            if let Some(confirmed_device) = maybe_confirmed_device {
                // Add device to anchor with bookkeeping if it has been confirmed
                anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);
                let operation = anchor_management::add_device(&mut anchor, confirmed_device);
                if let Err(err) = state::storage_borrow_mut(|storage| storage.write(anchor)) {
                    trap(&format!("{err}"));
                }
                anchor_management::post_operation_bookkeeping(anchor_number, operation);
            }
            VerifyTentativeDeviceResponse::Verified
        }
        Err(err) => match err {
            AuthnMethodConfirmationError::RegistrationModeOff => {
                VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
            }
            AuthnMethodConfirmationError::NoAuthnMethodToConfirm => {
                VerifyTentativeDeviceResponse::NoDeviceToVerify
            }
            AuthnMethodConfirmationError::WrongCode { retries_left } => {
                VerifyTentativeDeviceResponse::WrongCode { retries_left }
            }
            // Unreachable since these two errors already result in a trap in this legacy method.
            AuthnMethodConfirmationError::Unauthorized(principal) => {
                trap(&format!("{principal} could not be authenticated."))
            }
            AuthnMethodConfirmationError::InternalCanisterError(err) => {
                trap(&err.to_string());
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
        anchor_management::check_passkey_pubkey_is_not_used(&device_data.pubkey)?;

        Ok::<_, String>(((), anchor_management::add_device(anchor, device_data)))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
fn update(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        if device_key != device_data.pubkey {
            anchor_management::check_passkey_pubkey_is_not_used(&device_data.pubkey)?;
        }

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
        if device_key != device_data.pubkey {
            anchor_management::check_passkey_pubkey_is_not_used(&device_data.pubkey)?;
        }

        let operation =
            anchor_management::replace_device(anchor_number, anchor, device_key, device_data);
        Ok::<_, String>(((), operation))
    })
    .unwrap_or_else(|err| trap(err.as_str()))
}

#[update]
fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    anchor_operation_with_authz_check(anchor_number, |anchor| {
        let operation = anchor_management::remove_device(anchor_number, anchor, device_key);
        Ok::<_, String>(((), operation))
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

#[update]
fn lookup_caller_identity_by_recovery_phrase() -> Option<IdentityNumber> {
    let caller = caller();
    anchor_management::lookup_caller_identity_by_recovery_phrase(caller)
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
        None,
        // The legacy endpoint has no read-only option.
        DelegationAccess::Unrestricted,
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
        // The legacy endpoint has no read-only option.
        DelegationAccess::Unrestricted,
    )
    .map(GetDelegationResponse::SignedDelegation)
    .unwrap_or(GetDelegationResponse::NoSuchDelegation)
}

#[query]
fn get_accounts(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<Vec<AccountInfo>, GetAccountsError> {
    match check_session_authorization(anchor_number) {
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

#[query]
fn get_default_account(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<AccountInfo, GetDefaultAccountError> {
    check_session_authorization(anchor_number)
        .map_err(|err| GetDefaultAccountError::Unauthorized(err.principal))?;

    let default_account_info =
        account_management::get_default_account_for_origin(anchor_number, origin)?;

    Ok(default_account_info)
}

impl From<IdentityUpdateError> for SetDefaultAccountError {
    fn from(src: IdentityUpdateError) -> Self {
        match src {
            IdentityUpdateError::Unauthorized(principal) => Self::Unauthorized(principal),
            IdentityUpdateError::StorageError(anchor_number, storage_error) => {
                Self::InternalCanisterError(format!(
                    "Identity: {}, Error: {}",
                    anchor_number, storage_error
                ))
            }
        }
    }
}

#[update]
fn set_default_account(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
) -> Result<AccountInfo, SetDefaultAccountError> {
    check_authz_and_record_activity(anchor_number).map_err(SetDefaultAccountError::from)?;

    let result =
        account_management::set_default_account_for_origin(anchor_number, origin, account_number)?;
    anchor_management::post_operation_bookkeeping(anchor_number, Operation::SetDefaultAccount);
    Ok(result)
}

#[update]
async fn prepare_account_delegation(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
    permissions: Option<Permissions>,
) -> Result<PrepareAccountDelegation, AccountDelegationError> {
    match check_authz_and_record_activity(anchor_number) {
        Ok(ii_domain) => {
            account_management::prepare_account_delegation(
                anchor_number,
                origin,
                account_number,
                session_key,
                max_ttl,
                None,
                // An omitted `permissions` argument means unrestricted (see
                // `impl From<Option<Permissions>> for DelegationAccess`): this
                // preserves the original behavior for callers of the
                // (pre-feature) form. First-party callers always pass an explicit
                // value (queries-only by default in the CLI and MCP flows).
                DelegationAccess::from(permissions),
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
    permissions: Option<Permissions>,
) -> Result<SignedDelegation, AccountDelegationError> {
    match check_authorization(anchor_number) {
        Ok(_) => account_management::get_account_delegation(
            anchor_number,
            &origin,
            account_number,
            session_key,
            expiration,
            // See `prepare_account_delegation`: an omitted `permissions`
            // argument means an unrestricted delegation (backwards-compatible
            // with the original form).
            DelegationAccess::from(permissions),
        ),
        Err(err) => Err(err.into()),
    }
}

/// Read `anchor_number`'s synced trusted-MCP-server config (master toggle +
/// trusted server URL). Persisted on-chain, so it follows the identity across
/// devices. Read by the Settings UI and by the `/mcp` connect flow, which
/// verifies the connecting origin against it at connect time. Returns the
/// disabled, no-server default for an unauthorized caller or an anchor that
/// never wrote a config.
#[query]
fn mcp_get_config(anchor_number: AnchorNumber) -> McpConfig {
    if check_session_authorization(anchor_number).is_err() {
        return McpConfig::default();
    }
    mcp::get_mcp_config(anchor_number)
}

/// Persist `anchor_number`'s trusted-MCP-server config so it syncs across the
/// identity's devices. Authenticated as the identity (full authorization), so
/// only the user — never a page that initiates a connect request — can change
/// what their identity trusts. Disabling MCP or changing the trusted server
/// URL revokes the identity's active MCP session in the same message.
#[update]
fn mcp_set_config(anchor_number: AnchorNumber, config: McpConfig) -> Result<(), String> {
    check_authz_and_record_activity(anchor_number).map_err(|err| format!("Unauthorized: {err}"))?;
    mcp::set_mcp_config(anchor_number, config)
}

/// Called by the MCP server, signed with its registered session key: prepare a
/// per-app delegation at `target_origin` for `account_number` — one of the
/// anchor's accounts there (discover them with `mcp_get_accounts`), or the
/// anchor's default account when `None`. `max_ttl` is the requested lifetime in
/// ns, defaulting to and capped at 1 hour, and never outliving the session
/// grant. The resolved `account_number` is returned in `McpPrepareDelegation`
/// to thread into `mcp_get_delegation`.
///
/// `mcp::authorize_mcp_session_for_update` is the authorization gate: it
/// admits only the caller holding a live session grant (the registered session
/// key) and hands back the `McpSession` the operation runs on, so there is no
/// way to reach `prepare_delegation` without it.
#[update]
async fn mcp_prepare_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<McpPrepareDelegation, AccountDelegationError> {
    mcp::authorize_mcp_session_for_update()?
        .prepare_delegation(target_origin, account_number, session_key, max_ttl)
        .await
}

/// Fetch the delegation prepared by `mcp_prepare_delegation`. The anchor is
/// recovered from `caller()`'s grant; `account_number` and `expiration` must
/// be the values returned by the matching prepare call, or this returns
/// `NoSuchDelegation`. Gated by `mcp::authorize_mcp_session` (the registered
/// session key), like the other server-facing methods.
#[query]
fn mcp_get_delegation(
    target_origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    mcp::authorize_mcp_session()?.get_delegation(
        target_origin,
        account_number,
        session_key,
        expiration,
    )
}

/// Called by the MCP server (anchor recovered from `caller()`): list the anchor's
/// accounts at `target_origin` so the agent can pick which `account_number` to
/// request a delegation for. Gated by `mcp::authorize_mcp_session` (the
/// registered session key), like the other server-facing methods.
#[query]
fn mcp_get_accounts(
    target_origin: FrontendHostname,
) -> Result<Vec<AccountInfo>, AccountDelegationError> {
    mcp::authorize_mcp_session()?.get_accounts(target_origin)
}

#[update]
async fn prepare_session_delegation(
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<
    internet_identity_interface::internet_identity::types::PrepareSessionDelegation,
    internet_identity_interface::internet_identity::types::SessionDelegationError,
> {
    session_delegation::prepare_session_delegation(anchor_number, session_key, max_ttl).await
}

#[query]
fn get_session_delegation(
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<
    SignedDelegation,
    internet_identity_interface::internet_identity::types::SessionDelegationError,
> {
    session_delegation::get_session_delegation(anchor_number, session_key, expiration)
}

/// Mint an MCP *registration* delegation `P_reg -> registration_key` (see
/// [`mcp_registration`]). Authenticated as the identity (full authorization):
/// only the consenting user can create one. `registration_key` is an ephemeral
/// key the II frontend generates for this connect (browser-held — never a key
/// taken from the connect link; the frontend extends the chain to the MCP
/// server's key browser-side). `P_reg` is derived from a fresh random nonce,
/// and the whole consent — the anchor, `permissions` (read-only choice),
/// `max_ttl` (session-grant lifetime), and the trusted server URL from the
/// synced config — is recorded on an index entry keyed by `P_reg`, so
/// `mcp_register_v2` recovers it server-side and the server cannot alter any
/// of it.
#[update]
async fn prepare_mcp_registration_delegation(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    permissions: Option<Permissions>,
    max_ttl: Option<u64>,
) -> Result<PrepareMcpRegistrationDelegation, String> {
    mcp_registration::prepare(anchor_number, registration_key, permissions, max_ttl).await
}

/// Fetch the signed registration delegation prepared above, to deliver to the
/// trusted MCP server. `user_key` is the value the prepare call returned; the
/// seed is recovered from it, so no consent parameters need re-passing.
/// Authenticated as the identity, like the prepare call.
#[query]
fn get_mcp_registration_delegation(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    user_key: UserKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, String> {
    mcp_registration::get(anchor_number, registration_key, user_key, expiration)
}

/// Called by the trusted MCP server, authenticated by the registration
/// delegation chain (so `caller()` is the registration principal): bind the
/// server's long-lived `session_key` to the consenting anchor. The entire
/// consent — anchor, read-only choice, grant lifetime — is recovered from the
/// index entry keyed by `caller()`, so the server passes only `session_key`:
/// it cannot name a different anchor, upgrade the access level, or stretch the
/// grant, and never learns the anchor number. A trusted-server switch or
/// disable since consent invalidates the delegation. Returns the grant
/// expiration and the access level.
#[update]
fn mcp_register_v2(session_key: SessionKey) -> Result<McpRegistrationV2, String> {
    mcp_registration::register_v2(session_key)
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
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
fn whoami() -> Principal {
    caller()
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
        openid_configs: persistent_state.openid_configs.clone(),
        sso_allow_insecure_discovery: persistent_state.sso_allow_insecure_discovery,
        // One-shot upgrade arg driving the SSO credential backfill; not
        // persisted as config, so there is nothing to report back here.
        sso_credential_migration: None,
        analytics_config: Some(persistent_state.analytics_config.clone()),
        enable_dapps_explorer: persistent_state.enable_dapps_explorer,
        is_production: persistent_state.is_production,
        dummy_auth: Some(persistent_state.dummy_auth.clone()),
        backend_canister_id: Some(ic_cdk::api::id()),
        backend_origin: persistent_state.backend_origin.clone(),
        enable_dnssec_email_recovery: persistent_state.enable_dnssec_email_recovery,
        dnssec_config: Some(persistent_state.dnssec_config.clone()),
        doh_config: Some(persistent_state.doh_config.clone()),
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
    if let Some(openid_configs) = config.openid_configs {
        openid::setup(openid_configs);
    }

    // Kick off the SSO credential batch migration. Examines at most
    // `SSO_CREDENTIAL_MIGRATION_BATCH_SIZE` index keys per tick so each batch
    // fits in one ingress message; timer self-clears once the migration
    // signals done (immediately, when no `sso_credential_migration` arg was
    // supplied).
    init_sso_credential_migration_timer();
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
        if let Some(backend_origin) = arg.backend_origin {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.backend_origin = Some(backend_origin);
            })
        }
        if let Some(openid_configs) = arg.openid_configs {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.openid_configs = Some(openid_configs);
            })
        }
        if let Some(sso_allow_insecure_discovery) = arg.sso_allow_insecure_discovery {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.sso_allow_insecure_discovery = Some(sso_allow_insecure_discovery);
            })
        }
        if let Some(entries) = arg.sso_credential_migration {
            // One-shot arg, not persisted: the entries only need to live
            // until the batch migration kicked off in `initialize()` has
            // walked the credential index once.
            SSO_CREDENTIAL_MIGRATION_ENTRIES.replace(entries);
        }
        if let Some(new_flow_origins) = arg.new_flow_origins {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.new_flow_origins = Some(new_flow_origins);
            })
        }
        if let Some(analytics_config) = arg.analytics_config {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.analytics_config = analytics_config;
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
        if let Some(enable_dnssec_email_recovery) = arg.enable_dnssec_email_recovery {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.enable_dnssec_email_recovery = Some(enable_dnssec_email_recovery);
            })
        }
        if let Some(dnssec_config) = arg.dnssec_config {
            // Outer Some -> apply: inner None clears, inner Some replaces.
            state::persistent_state_mut(|persistent_state| {
                persistent_state.dnssec_config = dnssec_config;
            })
        }
        if let Some(doh_config) = arg.doh_config {
            // Outer Some -> apply: inner None clears, inner Some replaces.
            state::persistent_state_mut(|persistent_state| {
                persistent_state.doh_config = doh_config;
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

/// Calls raw rand to retrieve 32 fresh random bytes. Named for its original
/// use (the canister salt), but also reused by `mcp_registration::prepare` as a
/// per-connect nonce — each call is an independent `raw_rand` draw.
pub(crate) async fn random_salt() -> Salt {
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
        authz_utils::is_self_authenticating, state::get_identity_number_by_registration_id,
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
                        aaguid: None,
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
            None,
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

        // Anchor stores email recoveries as a Vec; the candid surface
        // exposes the full list as an `opt vec`, with `None` when no
        // recovery email is configured. The canister API currently
        // enforces ≤1 entry, so the FE picks the first to render the
        // recovery-email card — but a future bump to N>1 won't need a
        // candid schema change.
        let stored_email_recovery = state::anchor(identity_number).email_recovery.clone();
        let email_recovery = if stored_email_recovery.is_empty() {
            None
        } else {
            Some(stored_email_recovery)
        };

        let stored_verified_emails = state::anchor(identity_number).verified_emails.clone();
        let verified_emails = if stored_verified_emails.is_empty() {
            None
        } else {
            Some(stored_verified_emails)
        };

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
            created_at: anchor_info.created_at,
            email_recovery,
            verified_emails,
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

        let expiration = tentative_device_registration::enter_device_registration_mode(
            identity_number,
            id.map(|reg_id| {
                ValidatedRegistrationId::try_new(reg_id)
                    .map_err(AuthnMethodRegistrationModeEnterError::InvalidRegistrationId)
            })
            .transpose()?,
        )?;
        Ok(RegistrationModeInfo { expiration })
    }

    #[update]
    fn authn_method_registration_mode_exit(
        identity_number: IdentityNumber,
        authn_method: Option<AuthnMethodData>,
    ) -> Result<(), AuthnMethodRegistrationModeExitError> {
        if let (Some(authn_method), Some(confirmed_session)) = (
            authn_method,
            tentative_device_registration::get_confirmed_session(identity_number),
        ) {
            // Verify that caller matches confirmed session
            if confirmed_session != caller() {
                return Err(AuthnMethodRegistrationModeExitError::Unauthorized(caller()));
            }
            // Map authn method to device
            let device_data = DeviceWithUsage::try_from(authn_method.clone())
                .map(DeviceData::from)
                .map_err(|err| {
                    AuthnMethodRegistrationModeExitError::InvalidMetadata(err.to_string())
                })?;

            anchor_management::check_passkey_pubkey_is_not_used(&device_data.pubkey).map_err(
                |_| AuthnMethodRegistrationModeExitError::PasskeyWithThisPublicKeyIsAlreadyUsed,
            )?;

            // Add device to anchor with bookkeeping
            let mut anchor = state::anchor(identity_number);
            let operation = anchor_management::add_device(&mut anchor, device_data.clone());
            state::storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| {
                AuthnMethodRegistrationModeExitError::InternalCanisterError(err.to_string())
            })?;
            anchor_management::post_operation_bookkeeping(identity_number, operation);
            // Add temp key for device as a workaround for WebAuthn needing two users interactions:
            // one for "create" and one for "sign". So instead we only "create" and instead
            // authenticate with a temporary key for their first visit from their new device.
            state::with_temp_keys_mut(|temp_keys| {
                temp_keys.add_temp_key(&device_data.pubkey, identity_number, confirmed_session);
            });
        } else {
            // Else if there's no confirmed session, verify that caller is authenticated
            check_authz_and_record_activity(identity_number)
                .map_err(AuthnMethodRegistrationModeExitError::from)?;
        }

        // Exit registration mode and return
        tentative_device_registration::exit_device_registration_mode(identity_number);
        Ok(())
    }

    #[update]
    async fn authn_method_register(
        identity_number: IdentityNumber,
        authn_method: AuthnMethodData,
    ) -> Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError> {
        let device = DeviceWithUsage::try_from(authn_method)
            .map_err(|err| AuthnMethodRegisterError::InvalidMetadata(err.to_string()))?;

        tentative_device_registration::add_tentative_device(
            identity_number,
            DeviceData::from(device),
        )
        .await
    }

    #[update]
    async fn authn_method_session_register(
        identity_number: IdentityNumber,
    ) -> Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError> {
        let caller = caller();

        if !is_self_authenticating(caller) {
            return Err(AuthnMethodRegisterError::NotSelfAuthenticating(caller));
        }

        tentative_device_registration::add_tentative_session(identity_number, caller).await
    }

    #[query]
    fn authn_method_session_info(
        identity_number: IdentityNumber,
    ) -> Option<AuthnMethodSessionInfo> {
        // Return session info if caller matches confirmed session
        tentative_device_registration::get_confirmed_session(identity_number)
            .is_some_and(|confirmed_session| confirmed_session == caller())
            .then(|| {
                let anchor = state::anchor(identity_number);
                AuthnMethodSessionInfo {
                    name: anchor.name(),
                    created_at: anchor.created_at(),
                }
            })
    }

    #[update]
    fn authn_method_confirm(
        identity_number: IdentityNumber,
        confirmation_code: String,
    ) -> Result<(), AuthnMethodConfirmationError> {
        let (mut anchor, authorization_key) = check_authorization(identity_number)
            .map_err(|err| AuthnMethodConfirmationError::Unauthorized(err.principal))?;

        let maybe_confirmed_device =
            tentative_device_registration::confirm_tentative_device_or_session(
                identity_number,
                confirmation_code,
            )?;

        if let Some(confirmed_device) = maybe_confirmed_device {
            // Add device to anchor with bookkeeping if it has been confirmed
            anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);
            let operation = anchor_management::add_device(&mut anchor, confirmed_device);
            state::storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| {
                AuthnMethodConfirmationError::InternalCanisterError(err.to_string())
            })?;
            anchor_management::post_operation_bookkeeping(identity_number, operation);
        }

        Ok(())
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
        add_openid_credential, registration, remove_openid_credential, update_openid_credential,
    };
    use crate::authz_utils::{anchor_operation_with_authz_check, IdentityUpdateError};
    use crate::openid::{self, OpenIdCredentialKey};
    use crate::storage::anchor::AnchorError;
    use crate::{
        state, IdentityNumber, OpenIdCredentialAddError, OpenIdCredentialRemoveError,
        OpenIdDelegationError, OpenIdPrepareDelegationResponse, OpenIdResult, SessionKey,
        SsoGetDelegationRequest, SsoGetDelegationResponse, SsoPrepareDelegationRequest,
        SsoPrepareDelegationResponse, Timestamp,
    };
    use ic_cdk::caller;
    use ic_cdk_macros::{query, update};
    use internet_identity_interface::internet_identity::types::{
        CreateIdentityData, IdRegFinishError, IdRegFinishResult, OpenIDRegFinishArg,
        SignedDelegation,
    };
    use serde_bytes::ByteBuf;

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
        mut arg: OpenIDRegFinishArg,
    ) -> OpenIdResult<IdRegFinishResult, IdRegFinishError> {
        // Canonicalize the untrusted discovery domain at the boundary so both
        // verification and the credential stored from `arg` see the same value.
        arg.discovery_domain = openid::canonical_discovery_domain_opt(arg.discovery_domain);
        // Verify the JWT up front (driving any SSO discovery/JWKS fetches): a
        // cold or evicted cache surfaces as the `Pending` retry arm instead of a
        // terminal error, and the verified credential is then handed to the
        // shared flow so it isn't re-verified. A gated SSO login (discovery
        // domain + origin) goes through the SSO gate; everything else (a direct
        // provider or an ungated SSO login) through the plain OpenID verify —
        // mirroring the `sso_`/`openid_prepare_delegation` split on the auth side.
        let credential = match (arg.discovery_domain.as_deref(), arg.origin.as_deref()) {
            (Some(domain), Some(origin)) => {
                openid::prefetch_sso(Some(domain));
                match openid::verify_sso_for_registration(&arg.jwt, &arg.salt, domain, origin) {
                    Ok(openid::Cached::Ready(credential)) => credential,
                    Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
                    Err(err) => return OpenIdResult::Err(err),
                }
            }
            _ => match registration::registration_flow_v2::verify_openid_for_registration(&arg) {
                Ok(openid::Cached::Ready(credential)) => credential,
                Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
                Err(err) => return OpenIdResult::Err(err),
            },
        };
        // Config issuer for the authorization key / operation log: the
        // configured provider's (template) issuer, or the concrete JWT issuer for
        // an SSO credential, which carries its own `sso_domain` for scope routing.
        let config_iss = credential
            .config_issuer()
            .unwrap_or_else(|| credential.iss.clone());
        match registration::registration_flow_v2::identity_registration_finish(
            CreateIdentityData::OpenID(arg),
            Some((credential, config_iss)),
        ) {
            Ok(result) => OpenIdResult::Ok(result),
            Err(err) => OpenIdResult::Err(err),
        }
    }

    #[update]
    fn openid_credential_add(
        identity_number: IdentityNumber,
        jwt: String,
        salt: [u8; 32],
        discovery_domain: Option<String>,
    ) -> OpenIdResult<(), OpenIdCredentialAddError> {
        let discovery_domain = openid::canonical_discovery_domain_opt(discovery_domain);
        openid::prefetch_sso(discovery_domain.as_deref());
        let openid_credential = match openid::verify_jwt(&jwt, &salt, discovery_domain.as_deref()) {
            Ok(openid::Cached::Ready(credential)) => credential,
            // SSO discovery/JWKS isn't cached yet; the fetch is in flight. The
            // frontend retries the call.
            Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
            Err(err) => return OpenIdResult::Err(err.into()),
        };
        let now_ns = ic_cdk::api::time();
        let outcome = anchor_operation_with_authz_check(identity_number, |anchor| {
            add_openid_credential(anchor, openid_credential, now_ns)
                .map(|operation| ((), operation))
                .map_err(|err| match err {
                    AnchorError::OpenIdCredentialAlreadyRegistered => {
                        OpenIdCredentialAddError::OpenIdCredentialAlreadyRegistered
                    }
                    err => OpenIdCredentialAddError::InternalCanisterError(err.to_string()),
                })
        });
        match outcome {
            Ok(()) => OpenIdResult::Ok(()),
            Err(err) => OpenIdResult::Err(err),
        }
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
        discovery_domain: Option<String>,
    ) -> OpenIdResult<OpenIdPrepareDelegationResponse, OpenIdDelegationError> {
        // Drive the SSO discovery/JWKS fetches (if any) on this update, then
        // read the result. A cold cache reads `Pending`; the frontend polls
        // `openid_get_delegation` and re-calls this until the delegation is
        // ready.
        let discovery_domain = openid::canonical_discovery_domain_opt(discovery_domain);
        openid::prefetch_sso(discovery_domain.as_deref());
        let openid_credential = match openid::verify_jwt(&jwt, &salt, discovery_domain.as_deref()) {
            Ok(openid::Cached::Ready(credential)) => credential,
            Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
            Err(err) => return OpenIdResult::Err(err.into()),
        };
        // The verified credential already carries the SSO stable identifier, so
        // the anchor write below reconciles the stable-id index — an existing
        // anchor self-heals on a normal sign-in.
        let prepared: Result<OpenIdPrepareDelegationResponse, OpenIdDelegationError> = async {
            let anchor_number = state::storage_borrow(|storage| {
                storage.lookup_anchor_with_openid_credential(&openid_credential.key())
            })
            .ok_or(OpenIdDelegationError::NoSuchAnchor)?;

            // Update anchor with latest OpenID credential from JWT so latest information is stored,
            // this means all data except the `last_used_timestamp` e.g. `name`, `email` and `picture`.
            let mut anchor = state::anchor(anchor_number);
            update_openid_credential(&mut anchor, openid_credential.clone())
                .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;
            state::storage_borrow_mut(|storage| storage.write(anchor))
                .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;

            let (user_key, expiration) = openid_credential
                .prepare_jwt_delegation(session_key, anchor_number)
                .await;

            // Checking again because the association could've changed during the .await
            let still_anchor_number = state::storage_borrow(|storage| {
                storage.lookup_anchor_with_openid_credential(&openid_credential.key())
            })
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
        .await;

        match prepared {
            Ok(response) => OpenIdResult::Ok(response),
            Err(err) => OpenIdResult::Err(err),
        }
    }

    #[query]
    fn openid_get_delegation(
        jwt: String,
        salt: [u8; 32],
        session_key: SessionKey,
        expiration: Timestamp,
        discovery_domain: Option<String>,
    ) -> OpenIdResult<SignedDelegation, OpenIdDelegationError> {
        // A query can't drive the SSO fetches, so `verify_jwt` only reads the
        // caches. A `Pending` means discovery/JWKS isn't cached yet — the
        // frontend re-calls `openid_prepare_delegation` (an update, which drives
        // the fetch) and polls this again.
        let discovery_domain = openid::canonical_discovery_domain_opt(discovery_domain);
        let openid_credential = match openid::verify_jwt(&jwt, &salt, discovery_domain.as_deref()) {
            Ok(openid::Cached::Ready(credential)) => credential,
            Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
            Err(err) => return OpenIdResult::Err(err.into()),
        };

        let delegation = match state::storage_borrow(|storage| {
            storage.lookup_anchor_with_openid_credential(&openid_credential.key())
        }) {
            Some(anchor_number) => {
                openid_credential.get_jwt_delegation(session_key, expiration, anchor_number)
            }
            None => Err(OpenIdDelegationError::NoSuchAnchor),
        };

        match delegation {
            Ok(signed) => OpenIdResult::Ok(signed),
            Err(err) => OpenIdResult::Err(err),
        }
    }

    /// Drive the two-hop SSO discovery fetch for `domain`. The frontend calls
    /// this when `get_sso_discovery_status` reads `Pending`, then keeps polling
    /// the query until it returns `Resolved`.
    #[update]
    fn discover_sso(domain: String) {
        openid::discover_sso(&openid::canonical_discovery_domain(&domain))
    }

    /// Read the status of `org_domain`'s SSO discovery: `Resolved` with the
    /// config, or `Pending` while the fetch is in flight.
    ///
    /// With `target_app_origin`, the resolved `resolved_client_id` is the client
    /// that origin must use.
    #[query]
    fn get_sso_discovery_status(
        request: internet_identity_interface::internet_identity::types::GetSsoDiscoveryStatusRequest,
    ) -> internet_identity_interface::internet_identity::types::SsoDiscoveryStatus {
        openid::get_sso_discovery_status(
            &openid::canonical_discovery_domain(&request.org_domain),
            request.target_app_origin.as_deref(),
        )
    }

    /// SSO sign-in: verify the JWT, enforce the per-app gate, resolve the II-client
    /// identity, and mint the openid delegation plus a certified SSO attribute bundle.
    #[update]
    async fn sso_prepare_delegation(
        request: SsoPrepareDelegationRequest,
    ) -> OpenIdResult<SsoPrepareDelegationResponse, OpenIdDelegationError> {
        let SsoPrepareDelegationRequest {
            jwt,
            salt,
            session_key,
            org_domain,
            target_app_origin: origin,
        } = request;
        let discovery_domain = openid::canonical_discovery_domain(&org_domain);
        openid::prefetch_sso(Some(&discovery_domain));

        let verification = match openid::verify_sso_jwt(&jwt, &salt, &discovery_domain, &origin) {
            Ok(openid::Cached::Ready(v)) => v,
            Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
            Err(err) => return OpenIdResult::Err(err.into()),
        };

        let identity = match openid::resolve_ii_client_identity(&verification) {
            Ok(identity) => identity,
            Err(err) => return OpenIdResult::Err(err),
        };

        let prepared: Result<SsoPrepareDelegationResponse, OpenIdDelegationError> = async {
            let key = identity.credential.key();
            let anchor_number =
                state::storage_borrow(|storage| storage.lookup_anchor_with_openid_credential(&key))
                    .ok_or(OpenIdDelegationError::NoSuchAnchor)?;

            // Refresh the II-client credential's metadata from the token; never adds a per-app credential.
            let mut anchor = state::anchor(anchor_number);
            update_openid_credential(&mut anchor, identity.credential.clone())
                .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;
            state::storage_borrow_mut(|storage| storage.write(anchor))
                .map_err(|_| OpenIdDelegationError::NoSuchAnchor)?;

            let (user_key, expiration) = identity
                .credential
                .prepare_jwt_delegation(session_key, anchor_number)
                .await;

            let (sso_attr_bundle, _bundle_expiration) = openid::prepare_sso_attr_bundle(
                &identity.credential.iss,
                &identity.credential.sub,
                &identity.credential.aud,
                anchor_number,
                &discovery_domain,
                &origin,
            );

            // The association could change during the `.await`.
            let still_anchor_number =
                state::storage_borrow(|storage| storage.lookup_anchor_with_openid_credential(&key))
                    .ok_or(OpenIdDelegationError::NoSuchAnchor)?;
            if anchor_number != still_anchor_number {
                // The credential re-associated to a different anchor during the
                // `.await` (a concurrent account change). Deliberately reported
                // as the same coarse error as "no anchor": the client has no
                // distinct recovery, and we don't surface backend
                // state-transition detail on the wire.
                return Err(OpenIdDelegationError::NoSuchAnchor);
            }

            Ok(SsoPrepareDelegationResponse {
                user_key,
                expiration,
                anchor_number,
                sso_attr_bundle: ByteBuf::from(sso_attr_bundle),
            })
        }
        .await;

        prepared.into()
    }

    /// Fetch the delegation and SSO attribute bundle signature prepared by `sso_prepare_delegation`.
    #[query]
    fn sso_get_delegation(
        request: SsoGetDelegationRequest,
    ) -> OpenIdResult<SsoGetDelegationResponse, OpenIdDelegationError> {
        let SsoGetDelegationRequest {
            jwt,
            salt,
            session_key,
            expiration,
            org_domain,
            target_app_origin: origin,
            sso_attr_bundle,
        } = request;
        let discovery_domain = openid::canonical_discovery_domain(&org_domain);
        let verification = match openid::verify_sso_jwt(&jwt, &salt, &discovery_domain, &origin) {
            Ok(openid::Cached::Ready(v)) => v,
            Ok(openid::Cached::Pending) => return OpenIdResult::Pending,
            Err(err) => return OpenIdResult::Err(err.into()),
        };
        // Query context: `resolve_ii_client_identity` only reads the stable-id index.
        let identity = match openid::resolve_ii_client_identity(&verification) {
            Ok(identity) => identity,
            Err(err) => return OpenIdResult::Err(err),
        };
        let key = identity.credential.key();
        let Some(anchor_number) =
            state::storage_borrow(|storage| storage.lookup_anchor_with_openid_credential(&key))
        else {
            return OpenIdResult::Err(OpenIdDelegationError::NoSuchAnchor);
        };
        let signed_delegation =
            match identity
                .credential
                .get_jwt_delegation(session_key, expiration, anchor_number)
            {
                Ok(signed) => signed,
                Err(err) => return OpenIdResult::Err(err),
            };
        let sso_attr_bundle_signature = match openid::get_sso_attr_bundle_signature(
            &identity.credential.iss,
            &identity.credential.sub,
            &identity.credential.aud,
            anchor_number,
            &sso_attr_bundle,
        ) {
            Ok(signature) => signature,
            Err(err) => return OpenIdResult::Err(err),
        };
        let sso_attr_bundle_signature = ByteBuf::from(sso_attr_bundle_signature);
        OpenIdResult::Ok(SsoGetDelegationResponse {
            signed_delegation,
            sso_attr_bundle_signature,
        })
    }
}

/// Email-based identity recovery — both halves of the flow.
///
/// See `docs/ongoing/email-recovery.md` (PR #3836) for the full
/// design, and `crate::email_recovery` for the per-flow logic.
///
/// - **Setup** (binding): `prepare_add` (authenticated) issues a
///   nonce, `smtp_request` for `register@id.ai` consumes the
///   verified email and binds the credential to the anchor, and
///   `credential_remove` (authenticated) detaches it.
/// - **Recovery** (delegation): `prepare_delegation` (anonymous)
///   issues a nonce bound to a session_key, `smtp_request` for
///   `recover@id.ai` consumes the verified email and stamps a
///   canister-signed delegation seed, and
///   `email_recovery_get_delegation` (anonymous query) hands the
///   signed delegation to the FE.
///
/// The canister methods below are thin wrappers handling
/// authorization, wall-clock injection (`now_secs` lifted out of
/// `ic_cdk::api::time` so the inner functions stay testable), and
/// the FE-facing `Result` shape. `status` is anonymous for both
/// flows since the nonce is the only secret needed to poll.
mod email_challenge_api {
    use super::*;
    use crate::email_inbound;
    use internet_identity_interface::internet_identity::types::email_challenge::{
        EmailChallengeDiagnostics, EmailChallengeError, EmailChallengeResolveViaDohArg,
        EmailChallengeStatus, EmailChallengeSubmitDkimLeafArg,
    };

    /// Open update. Called by the off-chain SMTP gateway for every
    /// inbound message. The canister verifies DKIM + DMARC, looks
    /// up the pending challenge by the nonce in the `Subject:`
    /// header, and on success binds the credential to the anchor
    /// from the pending entry.
    ///
    /// We always return `Ok` regardless of the verification verdict
    /// — the gateway gets no useful signal from per-message
    /// "verification failed" answers, and emitting one would let it
    /// probe the canister for which nonces exist. The FE sees the
    /// outcome via its `email_challenge_status(nonce)` poll.
    ///
    /// **Security:** no-oracle on the response *shape* — always
    /// returns `SmtpResponse::Ok {}` regardless of whether the
    /// nonce is known, expired, or terminal, so a caller can't
    /// learn the verification outcome from the response. (Response
    /// *latency* does differ — an unknown nonce silent-drops in
    /// microseconds, a valid one on the DoH path runs DKIM/DMARC
    /// fetches first — but with 64 bits of nonce entropy and a
    /// 30-minute TTL, timing-based existence probing isn't a
    /// threat this property is meant to block.) Recipient dispatch
    /// matches the full `user@domain` against `related_origins`,
    /// defense-in-depth against a direct caller spoofing just the
    /// user-part.
    #[update]
    fn smtp_request(
        request: internet_identity_interface::internet_identity::types::smtp::SmtpRequest,
    ) -> internet_identity_interface::internet_identity::types::smtp::SmtpResponse {
        // Synchronous accept: the DoH path detaches verification and the FE
        // polls `email_challenge_status` for the outcome, so the gateway
        // isn't held for the outcall round trip.
        email_inbound::handle_smtp_request(request)
    }

    /// Open query — the off-chain SMTP gateway calls this at
    /// `RCPT TO` time to decide whether to accept the connection
    /// before pulling the message body. Returns `Ok` for the two
    /// recipients we handle (`register@id.ai`, `recover@id.ai`) and
    /// 550 (mailbox unavailable) for everything else. Without this
    /// the gateway has no way to know which recipients we accept,
    /// and falls back to whatever default policy it was deployed
    /// with — which on the existing II gateway is the postbox PoC's
    /// "user-part must be a numeric anchor number" rule, rejecting
    /// `register@id.ai` and `recover@id.ai` outright.
    #[query]
    fn smtp_request_validate(
        request: internet_identity_interface::internet_identity::types::smtp::SmtpRequest,
    ) -> internet_identity_interface::internet_identity::types::smtp::SmtpResponse {
        email_inbound::handle_smtp_request_validate(request)
    }

    /// Anonymous. The FE polls this with the nonce returned from
    /// `prepare_add` to drive its "waiting for your email" spinner.
    /// Polling at 1–5 s cadence is the FE's responsibility; this
    /// query is cheap.
    ///
    /// Returns `EmailChallengeStatus::Expired` for unknown nonces —
    /// observably indistinguishable from "the canister forgot it",
    /// which is exactly what we want (the FE shows "timed out, try
    /// again" in either case).
    ///
    /// **Security:** unknown-nonce → `Expired` is deliberate: a
    /// caller without the original `prepare_add` nonce can't
    /// distinguish "never issued" from "evicted" from "expired".
    #[query]
    fn email_challenge_status(nonce: String) -> EmailChallengeStatus {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::pending_status(&nonce, now_secs)
    }

    /// Anonymous. Returns strictly-public, user-copyable diagnostics for
    /// a pending challenge — the gateway `message_id`, a coarse reason
    /// code, the verification path, and the challenge creation time — so
    /// a user who hits a recovery-email failure can paste them into a
    /// support ticket and let support line the case up across the SMTP
    /// gateway and canister logs.
    ///
    /// Returns `None` for an unknown/expired nonce, the same observable
    /// collapse `email_challenge_status` makes to `Expired`.
    ///
    /// **Security:** strictly non-sensitive — NO email address, anchor,
    /// principal, delegation/seed, or inner error string (the
    /// `reason_code` is the failing variant's name only). Readable only
    /// by the holder of the 64-bit nonce, exactly like
    /// `email_challenge_status`; it adds no pre-verification distinction a
    /// prober could use as an existence/linkage oracle.
    #[query]
    fn email_challenge_diagnostics(nonce: String) -> Option<EmailChallengeDiagnostics> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::pending_diagnostics(&nonce, now_secs)
    }

    /// Anonymous. Phase 2 of the DNSSEC path: once
    /// `email_challenge_status` returns `NeedDkimLeaf { selector }`,
    /// the FE walks DNSSEC for `<selector>._domainkey.<domain>` and
    /// submits the signed RRset here. The canister validates the leaf
    /// against the chain it cached at prepare time, completes the DKIM
    /// signature check using the partial-verification record stashed
    /// at email-arrival time, runs DMARC alignment, and binds the
    /// credential. Returns the post-call status — typically
    /// `RegistrationSucceeded` — saving the FE one extra
    /// `email_challenge_status` round-trip.
    ///
    /// Anonymous because this is the only DNSSEC-path move that
    /// happens before the address-binding finishes. The pending
    /// challenge nonce is the only authentication; it's a 64-bit
    /// canister-issued secret that lives only inside one pending
    /// entry, and the leaf submission is a no-op against any other
    /// entry.
    #[update]
    async fn email_challenge_submit_dkim_leaf(
        arg: EmailChallengeSubmitDkimLeafArg,
    ) -> Result<(), EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        // `Ok` = accepted; the verdict (Succeeded / Failed / RecoveryReady)
        // is read from `email_challenge_status`. `Err` is a call-level
        // rejection (unknown nonce / wrong state) only.
        email_inbound::submit_dkim_leaf(arg, now_secs).await
    }

    /// Anonymous. Resolves the DKIM key over the canister's own
    /// allowlist-gated DoH path, reusing the partial-verification record
    /// stashed at email-arrival time, then runs the same DMARC alignment +
    /// binding. Used for the pure-DoH (Gmail) case and as the fallback for
    /// the DNSSEC path when the FE can't walk a fully-signed resolution
    /// because the DKIM record CNAMEs into an unsigned zone (`outlook.com`
    /// -> `outbound.protection.outlook.com`, `live.com`, …).
    ///
    /// **Polled.** The FE calls this repeatedly while `email_challenge_status`
    /// reports `ResolvingDoh`. Each call reads the DoH cache: a cache miss
    /// spawns the fetch and returns `Ok(())` with the status left at
    /// `ResolvingDoh` (poll again); a cache hit verifies and stamps the
    /// terminal verdict. Idempotent — completing more than once is a no-op. A
    /// domain the operator hasn't enabled lands as `DomainNotAllowlisted`.
    ///
    /// Anonymous for the same reason as `email_challenge_submit_dkim_leaf`:
    /// the 64-bit nonce is the only authentication, and the call is a no-op
    /// against any other entry.
    #[update]
    fn email_challenge_resolve_via_doh(
        arg: EmailChallengeResolveViaDohArg,
    ) -> Result<(), EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        // `Ok` = accepted; the verdict is read by polling
        // `email_challenge_status` (the single source of truth).
        email_inbound::resolve_via_doh(arg.nonce, now_secs)
    }

    // =================================================================
    // DEPRECATED — remove in a follow-up PR
    // =================================================================
    // Legacy aliases for the four `email_challenge_*` methods above.
    // Kept so a stale FE bundle in a browser cache — or any FE build
    // that lands before this canister's renamed methods — can still
    // drive the inbound-DKIM flow without a "method not found" break
    // mid-verification. The wire bytes are identical to the new
    // methods (Candid is structurally typed; the renamed return types
    // match the old types' shapes field-for-field), so old clients
    // with bindings against the old type names deserialize
    // successfully.
    //
    // **All four wrappers below must be removed together in a single
    // follow-up `chore(be): remove deprecated email_recovery_* method
    // aliases` PR**, once every deployed FE has refreshed to the
    // `email_challenge_*` names. See TASKS.md for the tracked
    // follow-up. The .did file has a matching DEPRECATED section
    // immediately after `verified_email_remove` — drop both together.
    // =================================================================

    #[query]
    fn email_recovery_status(nonce: String) -> EmailChallengeStatus {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::pending_status(&nonce, now_secs)
    }

    #[query]
    fn email_recovery_diagnostics(nonce: String) -> Option<EmailChallengeDiagnostics> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::pending_diagnostics(&nonce, now_secs)
    }

    #[update]
    async fn email_recovery_submit_dkim_leaf(
        arg: EmailChallengeSubmitDkimLeafArg,
    ) -> Result<(), EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::submit_dkim_leaf(arg, now_secs).await
    }

    #[update]
    fn email_recovery_resolve_via_doh(
        arg: EmailChallengeResolveViaDohArg,
    ) -> Result<(), EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_inbound::resolve_via_doh(arg.nonce, now_secs)
    }
}

mod email_recovery_api {
    use super::*;
    use crate::authz_utils::check_authorization;
    use crate::email_recovery;
    use ic_canister_sig_creation::delegation_signature_msg;
    use ic_canister_sig_creation::signature_map::CanisterSigInputs;
    use ic_canister_sig_creation::DELEGATION_SIG_DOMAIN;
    use internet_identity_interface::internet_identity::types::email_challenge::{
        EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
    };
    use internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryGetDelegationArgs;
    use internet_identity_interface::internet_identity::types::SessionKey;

    /// Authenticated. Validates the caller owns `identity_number`,
    /// validates the DNS input shape (DNSSEC bundle if supplied,
    /// otherwise DoH allowlist gate), and issues a fresh nonce. The
    /// nonce is stored alongside the claimed address + selector +
    /// anchor in the heap pending-challenge map; an inbound email
    /// with that nonce in `Subject:` completes the binding via
    /// `smtp_request`.
    ///
    /// **Security:** caller must be the anchor controller — enforced
    /// by `check_authorization(identity_number)`. Resource exposure
    /// is bounded by the 10 k pending-map cap (see
    /// `email_inbound::MAX_PENDING_CHALLENGES`) with 30-minute TTL
    /// and oldest-first eviction.
    #[update]
    async fn email_recovery_credential_prepare_add(
        identity_number: IdentityNumber,
        dns_input: EmailChallengeDnsInput,
    ) -> Result<EmailChallenge, EmailChallengeError> {
        check_authorization(identity_number)
            .map_err(|err| EmailChallengeError::Unauthorized(err.principal))?;

        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_recovery::prepare_add(identity_number, dns_input, now_secs).await
    }

    /// Anonymous. The FE-side counterpart of `prepare_add` for the
    /// recovery flow: validates the same DNSSEC skeleton chain or
    /// DoH allowlist, parks a pending challenge bound to the
    /// FE-supplied `session_key`, and returns the nonce +
    /// `recover@id.ai` recipient. The anchor isn't resolved here —
    /// that happens at submit-leaf (or DoH-path smtp_request) time
    /// from the verified `From:` of the inbound email.
    ///
    /// Anonymous because by the time the user reaches for recovery
    /// they may have lost every authn method on their anchor; the
    /// DKIM-verified email itself is the credential. The
    /// nonce-rotation + Subject-visibility defenses (design §3.2)
    /// stand in for caller authentication.
    #[update]
    async fn email_recovery_prepare_delegation(
        dns_input: EmailChallengeDnsInput,
        session_key: SessionKey,
    ) -> Result<EmailChallenge, EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        email_recovery::prepare_delegation(dns_input, session_key, now_secs).await
    }

    /// **Anonymous query.** Final step of the recovery flow: after
    /// `email_challenge_status` reports `RecoveryReady { user_key,
    /// expiration, anchor_number }`, the FE calls this to retrieve
    /// the actual `SignedDelegation`. Mirrors `openid_get_delegation`
    /// in shape — the args (`session_key`, `expiration`) must match
    /// exactly what was stamped at submit-leaf time, otherwise the
    /// canister-signature store has no entry to return.
    #[query]
    fn email_recovery_get_delegation(
        args: EmailRecoveryGetDelegationArgs,
    ) -> Result<SignedDelegation, EmailChallengeError> {
        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        let seed = email_recovery::recovery_seed_for_nonce(&args.nonce, now_secs)
            .ok_or(EmailChallengeError::NonceUnknown)?;

        crate::state::assets_and_signatures(|certified_assets, sigs| {
            let inputs = CanisterSigInputs {
                domain: DELEGATION_SIG_DOMAIN,
                seed: &seed,
                message: &delegation_signature_msg(&args.session_key, args.expiration, None),
            };
            sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
                .map(|signature| SignedDelegation {
                    delegation: Delegation {
                        pubkey: args.session_key,
                        expiration: args.expiration,
                        targets: None,
                        permissions: None,
                    },
                    signature: serde_bytes::ByteBuf::from(signature),
                })
                .map_err(|_| {
                    EmailChallengeError::InternalCanisterError(
                        "no canister signature for the supplied (nonce, session_key, expiration); \
                         either the recovery flow hasn't completed yet, the args don't match \
                         what was stamped, or the signature has been evicted"
                            .into(),
                    )
                })
        })
    }

    /// Authenticated. Detaches the recovery email from the anchor.
    /// The FE's "Remove" action calls this once; on success the
    /// management page flips back to the inactive card. Removing a
    /// recovery email the anchor doesn't have is surfaced as
    /// `AddressNotRegistered` so the FE can show a meaningful error
    /// if it got into an inconsistent state.
    ///
    /// **Security:** caller must be the anchor controller — enforced
    /// by `authz_utils::check_authorization` (same path the other
    /// authenticated anchor mutators use).
    #[update]
    fn email_recovery_credential_remove(
        identity_number: IdentityNumber,
        address: String,
    ) -> Result<(), EmailChallengeError> {
        let (mut anchor, authz_key) = crate::authz_utils::check_authorization(identity_number)
            .map_err(|err| EmailChallengeError::Unauthorized(err.principal))?;
        crate::anchor_management::activity_bookkeeping(&mut anchor, &authz_key);

        let operation =
            email_recovery::remove_credential(&mut anchor, &address).map_err(|err| match err {
                crate::email_recovery::RemoveError::NotRegistered => {
                    EmailChallengeError::AddressNotRegistered
                }
            })?;

        crate::state::storage_borrow_mut(|storage| storage.write(anchor))
            .map_err(|err| EmailChallengeError::InternalCanisterError(format!("{err:?}")))?;

        crate::anchor_management::post_operation_bookkeeping(identity_number, operation);
        Ok(())
    }
}

mod verified_email_api {
    use super::*;
    use crate::authz_utils::check_authorization;
    use internet_identity_interface::internet_identity::types::email_challenge::{
        EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
    };

    #[update]
    async fn verified_email_prepare_add(
        identity_number: IdentityNumber,
        dns_input: EmailChallengeDnsInput,
    ) -> Result<EmailChallenge, EmailChallengeError> {
        check_authorization(identity_number)
            .map_err(|err| EmailChallengeError::Unauthorized(err.principal))?;

        let now_secs = ic_cdk::api::time() / 1_000_000_000;
        crate::verified_emails::prepare_add_verified_email(identity_number, dns_input, now_secs)
            .await
    }

    #[update]
    fn verified_email_remove(
        identity_number: IdentityNumber,
        address: String,
    ) -> Result<(), EmailChallengeError> {
        let (mut anchor, authz_key) = crate::authz_utils::check_authorization(identity_number)
            .map_err(|err| EmailChallengeError::Unauthorized(err.principal))?;
        crate::anchor_management::activity_bookkeeping(&mut anchor, &authz_key);

        let operation =
            crate::verified_emails::remove(&mut anchor, &address).map_err(|err| match err {
                crate::verified_emails::RemoveError::NotRegistered => {
                    EmailChallengeError::AddressNotRegistered
                }
            })?;

        crate::state::storage_borrow_mut(|storage| storage.write(anchor))
            .map_err(|err| EmailChallengeError::InternalCanisterError(format!("{err:?}")))?;

        crate::anchor_management::post_operation_bookkeeping(identity_number, operation);
        Ok(())
    }
}

mod attribute_sharing {
    use internet_identity_interface::internet_identity::types::attributes::{
        Attribute, CertifiedAttributes, GetAttributesError, GetAttributesRequest,
        ValidatedGetAttributesRequest,
    };

    use super::*;
    use crate::{account_management::get_account_for_origin, authz_utils::AuthorizationError};

    #[update]
    async fn prepare_attributes(
        request: PrepareAttributeRequest,
    ) -> Result<PrepareAttributeResponse, PrepareAttributeError> {
        // Parse and validate API request into internal types.
        let ValidatedPrepareAttributeRequest {
            // Arguments for computing the seed
            identity_number,
            origin,
            account_number,

            // Which attributes to prepare
            attribute_keys,
        } = request.try_into()?;

        let (anchor, _) =
            check_authorization(identity_number).map_err(|AuthorizationError { principal }| {
                PrepareAttributeError::AuthorizationError(principal)
            })?;

        let account = get_account_for_origin(anchor.anchor_number(), origin, account_number)
            .map_err(PrepareAttributeError::GetAccountError)?;

        // This is the only async operation, so we do it first, call operations that depend on
        // the time. TODO: refactor to avoid asynchronicity here.
        state::ensure_salt_set().await;
        let issued_at_timestamp_ns = ic_cdk::api::time();

        let attributes = anchor.prepare_attributes(attribute_keys, account, issued_at_timestamp_ns);

        // Convert response to API types.
        let attributes = attributes
            .into_iter()
            .map(|Attribute { key, value }| (key.to_string(), value))
            .collect();

        Ok(PrepareAttributeResponse {
            issued_at_timestamp_ns,
            attributes,
        })
    }

    #[query]
    fn get_attributes(
        request: GetAttributesRequest,
    ) -> Result<CertifiedAttributes, GetAttributesError> {
        // Parse and validate API request into internal types.
        let ValidatedGetAttributesRequest {
            // Arguments for computing the seed
            identity_number,
            origin,
            account_number,

            // Which attributes to prepare
            attributes,

            // When were the attribute certificates issued
            issued_at_timestamp_ns,
        } = request.try_into()?;

        let (anchor, _) =
            check_authorization(identity_number).map_err(|AuthorizationError { principal }| {
                GetAttributesError::AuthorizationError(principal)
            })?;

        let account = get_account_for_origin(anchor.anchor_number(), origin, account_number)
            .map_err(GetAttributesError::GetAccountError)?;

        let certified_attributes =
            anchor.get_attributes(attributes, account, issued_at_timestamp_ns);

        Ok(certified_attributes)
    }

    #[update]
    async fn prepare_icrc3_attributes(
        request: PrepareIcrc3AttributeRequest,
    ) -> Result<PrepareIcrc3AttributeResponse, PrepareIcrc3AttributeError> {
        let ValidatedPrepareIcrc3AttributeRequest {
            identity_number,
            origin,
            unmapped_origin,
            account_number,
            attributes,
            nonce,
        } = request.try_into()?;

        // SSO session iff a certified bundle for this origin is attached; gates `sso:<domain>` attributes.
        let sso_session_domain = openid::read_certified_sso_bundle()
            .filter(|bundle| bundle.origin == origin)
            .map(|bundle| bundle.sso_domain);
        let (anchor, _) =
            check_authorization(identity_number).map_err(|AuthorizationError { principal }| {
                PrepareIcrc3AttributeError::AuthorizationError(principal)
            })?;

        let account =
            get_account_for_origin(anchor.anchor_number(), origin.clone(), account_number)
                .map_err(PrepareIcrc3AttributeError::GetAccountError)?;

        state::ensure_salt_set().await;

        let issued_at_timestamp_ns = ic_cdk::api::time();
        let message = anchor.prepare_icrc3_attributes(
            attributes,
            nonce,
            origin,
            unmapped_origin,
            issued_at_timestamp_ns,
            account,
            sso_session_domain,
        )?;

        Ok(PrepareIcrc3AttributeResponse { message })
    }

    #[query]
    fn get_icrc3_attributes(
        request: GetIcrc3AttributeRequest,
    ) -> Result<GetIcrc3AttributeResponse, GetIcrc3AttributeError> {
        let ValidatedGetIcrc3AttributeRequest {
            identity_number,
            origin,
            account_number,
            message,
        } = request.try_into()?;

        let (anchor, _) =
            check_authorization(identity_number).map_err(|AuthorizationError { principal }| {
                GetIcrc3AttributeError::AuthorizationError(principal)
            })?;

        let account = get_account_for_origin(anchor.anchor_number(), origin, account_number)
            .map_err(GetIcrc3AttributeError::GetAccountError)?;

        let response = anchor.get_icrc3_attributes(account, &message)?;

        Ok(response)
    }

    #[query]
    fn list_available_attributes(
        request: ListAvailableAttributesRequest,
    ) -> Result<Vec<(String, Vec<u8>)>, ListAvailableAttributesError> {
        let ValidatedListAvailableAttributesRequest {
            identity_number,
            attributes,
        } = request.try_into()?;

        // Gate `sso:<domain>` rows by the certified bundle; the bundle is the trusted source of the SSO domain.
        let sso_session_domain =
            openid::read_certified_sso_bundle().map(|bundle| bundle.sso_domain);
        let (anchor, _) =
            check_authorization(identity_number).map_err(|AuthorizationError { principal }| {
                ListAvailableAttributesError::AuthorizationError(principal)
            })?;

        Ok(anchor.list_available_attributes(attributes, sso_session_domain))
    }
}

/// Legacy API for the original attribute sharing MVP (VC-based).
/// The current attribute sharing protocol endpoints live in `mod attribute_sharing`.
mod attribute_sharing_old_vc {
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
