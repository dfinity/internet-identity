pub mod browser_key;

use crate::authz_utils::{
    check_authorization, check_authz_and_record_activity, AuthorizationError, IdentityUpdateError,
};
use crate::delegation::{
    add_delegation_signature, calculate_session_seed_with_salt, canister_sig_principal,
    check_frontend_length, der_encode_canister_sig_key, DelegationAccess,
};
use crate::sessions::browser_key::verify_browser_keys;
use crate::state::{self, storage_borrow, storage_borrow_mut};
use crate::storage::account::{Account, AccountKey, SessionRecord, SessionRecordKey};
use crate::storage::anchor::BrowserError;
use crate::storage::{CreateSessionParams, StorageError};
use crate::{update_root_hash, DAY_NS, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::CanisterSigInputs;
use ic_canister_sig_creation::DELEGATION_SIG_DOMAIN;
use ic_cdk::api::time;
use ic_cdk::caller;
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AccountSessionError, AnchorNumber, AppGetDelegationRequest,
    AppPrepareDelegationRequest, AppPrepareDelegationResponse, AppSessionError, Delegation,
    FrontendHostname, GetAccountSessionRequest, GetAccountSessionResponse,
    PrepareAccountSessionRequest, PrepareAccountSessionResponse, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;

pub const DEFAULT_SESSION_TTL_NS: u64 = 30 * DAY_NS;
pub const MAX_SESSION_TTL_NS: u64 = 30 * DAY_NS;
const MIN_SESSION_TTL_NS: u64 = 10 * MINUTE_NS;

/// The browser name is a label the user reads, never anything the canister acts on.
const MAX_BROWSER_NAME_BYTES: usize = 128;

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

pub fn prepare_account_session(
    request: PrepareAccountSessionRequest,
) -> Result<PrepareAccountSessionResponse, AccountSessionError> {
    let PrepareAccountSessionRequest {
        identity_number,
        origin,
        account_number,
        session_key,
        browser_name,
        current_browser_key,
        next_browser_key,
        current_browser_key_signature,
        next_browser_key_signature,
        permissions,
        valid_for,
        max_idle,
    } = request;

    check_authz_and_record_activity(identity_number)?;
    check_frontend_length(&origin);
    if browser_name.len() > MAX_BROWSER_NAME_BYTES {
        return Err(AccountSessionError::InternalCanisterError(
            "browser name exceeds the limit".to_string(),
        ));
    }
    if !verify_browser_keys(
        &current_browser_key,
        &current_browser_key_signature,
        &next_browser_key,
        &next_browser_key_signature,
        &session_key,
    ) {
        return Err(AccountSessionError::InvalidBrowserKey);
    }

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
        storage.read_account(&AccountKey {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
        })
    })
    .is_none()
    {
        return Err(AccountSessionError::NoSuchAccount);
    }

    // The browser is resolved by the write, not here: registering it can put the registry
    // over its cap, and the browser that gives way takes its sessions with it. That is one
    // change with the session created below, so it is one write, and working any of it out
    // here would be working out something the write has to be told.
    let (_, session) = storage_borrow_mut(|storage| {
        storage.create_session(CreateSessionParams {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
            current_browser_key,
            next_browser_key,
            browser_name,
            valid_till_ns: valid_till,
            max_idle_ns: max_idle,
            read_only,
            now_ns: now,
        })
    })
    .map_err(|err| match err {
        // Told apart from the rest because the browser can act on it: it is the only
        // party holding the successor that does resolve.
        StorageError::Browser(BrowserError::StaleBrowserKey) => {
            AccountSessionError::StaleBrowserKey
        }
        StorageError::Browser(_) => AccountSessionError::InvalidBrowserKey,
        // The account was checked above, so anything left is a broken storage invariant
        // rather than a request this caller could have got wrong.
        err => AccountSessionError::InternalCanisterError(err.to_string()),
    })?;

    let seed = session_identity(identity_number, &origin, account_number, &session)
        .expect("failed to derive the identity of a session that was just created");
    let account_principal = account_principal(identity_number, &origin, account_number)
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
        session_id: session.session_id,
        browser_id: session.browser_id,
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
        session_id,
    } = request;

    check_authorization(identity_number)?;
    check_frontend_length(&origin);

    // `prepare_account_session` handed the browser this id, so the session is named
    // exactly rather than searched for. An id naming a session that was replaced since
    // finds nothing, which is the honest answer: the delegation this call is collecting
    // was signed for the session that is gone.
    let session = storage_borrow(|storage| {
        storage.read_session(&SessionRecordKey {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
            session_id,
        })
    })
    .ok_or(AccountSessionError::NoSuchSession)?;

    let seed = session_identity(identity_number, &origin, account_number, &session)
        .map_err(|_| AccountSessionError::NoSuchSession)?;
    let signed_delegation = witness_session_delegation(&seed, &session_key, expiration)
        .ok_or(AccountSessionError::NoSuchSession)?;

    Ok(GetAccountSessionResponse { signed_delegation })
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
) -> Result<Hash, AccountSessionError> {
    let salt = storage_borrow(|storage| storage.salt().copied()).ok_or_else(|| {
        AccountSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;

    let account = storage_borrow(|storage| {
        storage.read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number,
        })
    })
    .ok_or(AccountSessionError::NoSuchAccount)?;
    let seed = calculate_session_seed_with_salt(
        &salt,
        &account.calculate_seed_with_salt(&salt),
        session.session_id,
    );
    Ok(seed)
}

/// The principal an app sees for this account. A session handle names the account by this
/// and never by the numbers behind it, which are II's alone.
///
/// The account is read rather than reconstructed: a materialized default derives from
/// `seed_from_anchor`, which only the stored list carries.
fn account_principal(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
) -> Result<Principal, AccountSessionError> {
    let salt = storage_borrow(|storage| storage.salt().copied()).ok_or_else(|| {
        AccountSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;
    let account = storage_borrow(|storage| {
        storage.read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number,
        })
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
    let (account, session) = authorize_session(now)?;

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
    let (account, session) = authorize_session(now)?;

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
fn authorize_session(now: Timestamp) -> Result<(Account, SessionRecord), AppSessionError> {
    let key = storage_borrow(|storage| storage.lookup_session_with_principal(caller()))
        .ok_or(AppSessionError::NoMatchingSession)?;

    let (account, session) = storage_borrow(|storage| {
        Some((
            storage.read_account(&key.account())?,
            storage.read_session(&key)?,
        ))
    })
    .ok_or(AppSessionError::NoMatchingSession)?;

    if session.is_over(now) {
        return Err(AppSessionError::NoMatchingSession);
    }
    Ok((account, session))
}

fn account_seed(account: &Account) -> Result<Hash, AppSessionError> {
    let salt = storage_borrow(|storage| storage.salt().copied()).ok_or_else(|| {
        AppSessionError::InternalCanisterError(StorageError::SaltNotSet.to_string())
    })?;
    Ok(account.calculate_seed_with_salt(&salt))
}
