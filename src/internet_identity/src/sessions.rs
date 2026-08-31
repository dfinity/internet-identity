pub mod device_key;

use crate::anchor_management::post_operation_bookkeeping;
use crate::authz_utils::{
    check_authorization, check_authz_and_record_activity, AuthorizationError, IdentityUpdateError,
};
use crate::delegation::{
    add_delegation_signature, calculate_session_seed_with_salt, canister_sig_principal,
    check_frontend_length, der_encode_canister_sig_key, DelegationAccess,
};
use crate::sessions::device_key::verify_device_keys;
use crate::state::{self, storage_borrow, storage_borrow_mut};
use crate::storage::account::{Account, ReadAccountParams, SessionRecord};
use crate::storage::storable::account_locator::StorableAccountLocator;
use crate::storage::{CreateSessionParams, StorageError};
use crate::{update_root_hash, DAY_NS, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::CanisterSigInputs;
use ic_canister_sig_creation::DELEGATION_SIG_DOMAIN;
use ic_cdk::api::time;
use ic_cdk::caller;
use ic_certification::Hash;
use internet_identity_interface::archive::types::{Operation, Private};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AccountSessionError, AnchorNumber, AppGetDelegationRequest,
    AppPrepareDelegationRequest, AppPrepareDelegationResponse, AppSessionError, ApplicationNumber,
    Delegation, FrontendHostname, GetAccountSessionRequest, GetAccountSessionResponse,
    PrepareAccountSessionRequest, PrepareAccountSessionResponse, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;

pub const DEFAULT_SESSION_TTL_NS: u64 = 30 * DAY_NS;
pub const MAX_SESSION_TTL_NS: u64 = 30 * DAY_NS;
const MIN_SESSION_TTL_NS: u64 = 10 * MINUTE_NS;

/// The device name is a label the user reads, never anything the canister acts on.
const MAX_DEVICE_NAME_BYTES: usize = 128;

impl From<AuthorizationError> for AccountSessionError {
    fn from(err: AuthorizationError) -> Self {
        AccountSessionError::Unauthorized(err.principal)
    }
}

impl From<IdentityUpdateError> for AccountSessionError {
    fn from(err: IdentityUpdateError) -> Self {
        match err {
            IdentityUpdateError::Unauthorized(principal) => {
                AccountSessionError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(_, storage_error) => storage_error.into(),
        }
    }
}

impl From<StorageError> for AccountSessionError {
    fn from(err: StorageError) -> Self {
        match err {
            StorageError::MissingAccount { .. } | StorageError::ApplicationNotFound { .. } => {
                AccountSessionError::NoSuchAccount
            }
            other => AccountSessionError::InternalCanisterError(other.to_string()),
        }
    }
}

pub async fn prepare_account_session(
    request: PrepareAccountSessionRequest,
) -> Result<PrepareAccountSessionResponse, AccountSessionError> {
    let PrepareAccountSessionRequest {
        identity_number,
        origin,
        account_number,
        session_key,
        device_name,
        device_key,
        next_device_key,
        device_key_signature,
        next_device_key_signature,
        permissions,
        valid_for,
    } = request;

    check_authz_and_record_activity(identity_number)?;
    check_frontend_length(&origin);
    if device_name.len() > MAX_DEVICE_NAME_BYTES {
        return Err(AccountSessionError::InternalCanisterError(
            "device name exceeds the limit".to_string(),
        ));
    }
    if !verify_device_keys(
        &device_key,
        &device_key_signature,
        &next_device_key,
        &next_device_key_signature,
        &session_key,
    ) {
        return Err(AccountSessionError::InvalidDeviceKey);
    }
    state::ensure_salt_set().await;

    let now = time();
    let valid_till = now.saturating_add(
        valid_for
            .unwrap_or(DEFAULT_SESSION_TTL_NS)
            .clamp(MIN_SESSION_TTL_NS, MAX_SESSION_TTL_NS),
    );
    let access = DelegationAccess::from(permissions);
    let read_only = access == DelegationAccess::ReadOnly;

    // Checked before anything is written. An account this identity does not hold is the
    // one failure a caller can provoke, and returning it after the writes below would
    // leave a browser registered for a sign-in that never happened.
    if storage_borrow(|storage| {
        storage.read_account(ReadAccountParams {
            account_number,
            anchor_number: identity_number,
            origin: &origin,
            known_app_num: None,
        })
    })
    .is_none()
    {
        return Err(AccountSessionError::NoSuchAccount);
    }

    let mut anchor = state::anchor(identity_number);
    // A rotating browser presents the successor it announced, so both values are known.
    let known_device = anchor
        .session_devices()
        .iter()
        .any(|device| device.key == device_key || device.pending == device_key);
    let (device_id, dropped_devices) = anchor
        .resolve_session_device(device_key, next_device_key, device_name, now)
        .map_err(|_| AccountSessionError::InvalidDeviceKey)?;
    storage_borrow_mut(|storage| storage.write(anchor))
        .expect("failed to write the anchor while registering a browser");

    if !known_device {
        post_operation_bookkeeping(
            identity_number,
            Operation::RegisterSessionDevice {
                name: Private::Redacted,
            },
        );
    }

    for dropped in dropped_devices {
        storage_borrow_mut(|storage| storage.revoke_device_sessions(identity_number, dropped))
            .expect("failed to end the sessions of a browser the registry dropped");
    }

    // The account was checked above, so anything left is a broken storage invariant
    // rather than a request this caller could have got wrong. Trapping rolls the whole
    // message back, including the browser registration.
    let session = storage_borrow_mut(|storage| {
        storage.create_session(CreateSessionParams {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
            device_id,
            valid_till_ns: valid_till,
            max_idle_ns: None,
            read_only,
            now_ns: now,
        })
    })
    .expect("failed to create a session for an account that was just read");

    let (seed, application_number) =
        session_identity(identity_number, &origin, account_number, &session)
            .expect("failed to derive the identity of a session that was just created");
    let account_principal = account_principal(identity_number, application_number, account_number)
        .expect("failed to derive the principal of an account that was just read");

    state::signature_map_mut(|sigs| {
        add_delegation_signature(
            sigs,
            session_key,
            seed.as_ref(),
            session.valid_till_ns,
            None,
        );
    });
    update_root_hash();

    Ok(PrepareAccountSessionResponse {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration: session.valid_till_ns,
        created_at: session.created_at_ns,
        device_id,
        account_principal,
    })
}

pub fn get_account_session(
    request: GetAccountSessionRequest,
) -> Result<GetAccountSessionResponse, AccountSessionError> {
    let GetAccountSessionRequest {
        identity_number,
        origin,
        account_number,
        session_key,
        expiration,
    } = request;

    check_authorization(identity_number)?;
    check_frontend_length(&origin);

    let sessions = storage_borrow(|storage| {
        storage.account_sessions(identity_number, &origin, account_number)
    })
    .ok_or(AccountSessionError::NoSuchAccount)?;

    sessions
        .into_iter()
        .filter(|session| session.valid_till_ns == expiration)
        .find_map(|session| {
            let (seed, _) =
                session_identity(identity_number, &origin, account_number, &session).ok()?;
            let signed_delegation = witness_session_delegation(&seed, &session_key, expiration)?;
            Some(GetAccountSessionResponse { signed_delegation })
        })
        .ok_or(AccountSessionError::NoSuchSession)
}

fn witness_session_delegation(
    seed: &Hash,
    session_key: &[u8],
    expiration: Timestamp,
) -> Option<SignedDelegation> {
    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed,
            message: &crate::delegation::delegation_signature_msg_with_permissions(
                session_key,
                expiration,
                None,
                None,
            ),
        };
        sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
            .ok()
    })
    .map(|signature| SignedDelegation {
        delegation: Delegation {
            pubkey: ByteBuf::from(session_key.to_vec()),
            expiration,
            targets: None,
            permissions: None,
        },
        signature: ByteBuf::from(signature),
    })
}

fn session_identity(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
    session: &SessionRecord,
) -> Result<(Hash, ApplicationNumber), AccountSessionError> {
    let (salt, application_number) = storage_borrow(|storage| {
        (
            storage.salt().copied(),
            storage.lookup_application_number_with_origin(origin),
        )
    });
    let salt = salt.ok_or_else(|| {
        AccountSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;
    let application_number: ApplicationNumber =
        application_number.ok_or(AccountSessionError::NoSuchAccount)?;

    let (account, _) = storage_borrow(|storage| {
        storage.account_with_sessions(anchor_number, application_number, account_number)
    })
    .ok_or(AccountSessionError::NoSuchAccount)?;
    let seed = calculate_session_seed_with_salt(
        &salt,
        &account.calculate_seed_with_salt(&salt),
        session.created_at_ns,
        session.device_id,
    );
    Ok((seed, application_number))
}

/// The principal an app sees for this account. A session handle names the account by this
/// and never by the numbers behind it, which are II's alone.
///
/// The account is read rather than reconstructed: a materialized default derives from
/// `seed_from_anchor`, which only the stored row carries.
fn account_principal(
    anchor_number: AnchorNumber,
    application_number: ApplicationNumber,
    account_number: Option<AccountNumber>,
) -> Result<Principal, AccountSessionError> {
    let salt = storage_borrow(|storage| storage.salt().copied()).ok_or_else(|| {
        AccountSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;
    let (account, _) = storage_borrow(|storage| {
        storage.account_with_sessions(anchor_number, application_number, account_number)
    })
    .ok_or(AccountSessionError::NoSuchAccount)?;
    Ok(canister_sig_principal(
        ic_cdk::id(),
        account.calculate_seed_with_salt(&salt).to_vec(),
    ))
}

/// The one window revocation cannot reach. Matches what MCP mints, and is not
/// requestable by the app.
pub const APP_DELEGATION_TTL_NS: u64 = 5 * MINUTE_NS;

pub fn app_prepare_delegation(
    request: AppPrepareDelegationRequest,
) -> Result<AppPrepareDelegationResponse, AppSessionError> {
    let now = time();
    let (locator, account, session) = authorize_session(now)?;

    storage_borrow_mut(|storage| {
        storage.stamp_session_refresh(
            locator.anchor_number,
            locator.application_number,
            locator.account_number,
            session.created_at_ns,
            session.device_id,
            now,
        )
    })
    .map_err(|err| AppSessionError::InternalCanisterError(err.to_string()))?;

    let expiration = u64::min(
        now.saturating_add(APP_DELEGATION_TTL_NS),
        session.valid_till_ns,
    );
    let seed = account_seed(&account)?;
    let access = DelegationAccess::from_read_only(session.read_only);

    state::signature_map_mut(|sigs| {
        add_delegation_signature(
            sigs,
            request.session_key,
            seed.as_ref(),
            expiration,
            access.permissions(),
        );
    });
    update_root_hash();

    Ok(AppPrepareDelegationResponse {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    })
}

pub fn app_get_delegation(
    request: AppGetDelegationRequest,
) -> Result<SignedDelegation, AppSessionError> {
    let now = time();
    let (_, account, session) = authorize_session(now)?;

    if request.expiration > now.saturating_add(APP_DELEGATION_TTL_NS)
        || request.expiration > session.valid_till_ns
    {
        return Err(AppSessionError::NoMatchingSession);
    }

    let seed = account_seed(&account)?;
    let access = DelegationAccess::from_read_only(session.read_only);
    let permissions = access.permissions();

    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed: &seed,
            message: &crate::delegation::delegation_signature_msg_with_permissions(
                &request.session_key,
                request.expiration,
                None,
                permissions,
            ),
        };
        sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
    })
    .map(|signature| SignedDelegation {
        delegation: Delegation {
            pubkey: request.session_key,
            expiration: request.expiration,
            targets: None,
            permissions: permissions.map(str::to_string),
        },
        signature: ByteBuf::from(signature),
    })
    .map_err(|_| AppSessionError::NoMatchingSession)
}

/// Authenticates a refresh from `caller()` alone.
///
/// The session index is keyed by the principal a session's chain is rooted at, so a hit is
/// itself the proof that the caller is that session: nothing is named in the request and
/// nothing is attached to it.
fn authorize_session(
    now: Timestamp,
) -> Result<(StorableAccountLocator, Account, SessionRecord), AppSessionError> {
    let matched = match_session()?;
    if matched.2.is_expired(now) {
        return Err(AppSessionError::NoMatchingSession);
    }
    Ok(matched)
}

/// Signs the caller's own session out. A caller cannot produce another session's
/// principal, so the seed match is the whole authorization. Always succeeds.
pub fn app_revoke_session() {
    let Ok((locator, _, session)) = match_session() else {
        return;
    };
    // Trapping rather than reporting success: the caller is told nothing either way, so a
    // storage failure that left the session live would end as a silent no-op. A trap rolls
    // the message back and reaches the caller as a reject.
    storage_borrow_mut(|storage| {
        storage.remove_session(
            locator.anchor_number,
            locator.application_number,
            locator.account_number,
            session.created_at_ns,
            session.device_id,
        )
    })
    .expect("failed to remove a session that was just matched");
}

fn match_session() -> Result<(StorableAccountLocator, Account, SessionRecord), AppSessionError> {
    let handle = storage_borrow(|storage| storage.lookup_session_with_principal(caller()))
        .ok_or(AppSessionError::NoMatchingSession)?;
    let locator = storage_borrow(|storage| storage.lookup_account_with_principal(handle.account()))
        .ok_or(AppSessionError::NoMatchingSession)?;

    let (account, sessions) = storage_borrow(|storage| {
        storage.account_with_sessions(
            locator.anchor_number,
            locator.application_number,
            locator.account_number,
        )
    })
    .ok_or(AppSessionError::NoMatchingSession)?;

    // The browser and the creation time together, because a browser keeps its id across
    // sign-ins: on the browser alone, an index entry that outlived its session would
    // authenticate its holder as whatever that browser created next.
    let session = sessions
        .into_iter()
        .find(|session| {
            session.device_id == handle.device_id && session.created_at_ns == handle.created_at
        })
        .ok_or(AppSessionError::NoMatchingSession)?;

    Ok((locator, account, session))
}

fn account_seed(account: &Account) -> Result<Hash, AppSessionError> {
    let salt = storage_borrow(|storage| storage.salt().copied()).ok_or_else(|| {
        AppSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;
    Ok(account.calculate_seed_with_salt(&salt))
}
