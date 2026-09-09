use crate::authz_utils::{
    check_authorization, check_authz_and_record_activity, AuthorizationError, IdentityUpdateError,
};
use crate::browser_key::verify_browser_keys;
use crate::delegation::{
    add_delegation_signature, calculate_session_seed_with_salt, canister_sig_principal,
    der_encode_canister_sig_key, frontend_length_within_limit, DelegationAccess,
};
use crate::state::{self, storage_borrow, storage_borrow_mut};
use crate::storage::account::{Account, AccountKey, Session, SessionLocator};
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
    AppPrepareDelegationRequest, AppPrepareDelegationResponse, AppSessionError, BrowserBrand,
    BrowserDescription, Delegation, FrontendHostname, GetAccountSessionRequest,
    GetAccountSessionResponse, OperatingSystem, PrepareAccountSessionRequest,
    PrepareAccountSessionResponse, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;

pub const DEFAULT_SESSION_TTL_NS: u64 = 30 * DAY_NS;
pub const MAX_SESSION_TTL_NS: u64 = 30 * DAY_NS;
const MIN_SESSION_TTL_NS: u64 = 10 * MINUTE_NS;

/// A bound on each token a browser reports about itself. Refused rather than truncated:
/// a client sending something this long is sending something wrong, and a cut-off token
/// would put a value in the record that no parser ever produced.
const MAX_BROWSER_TOKEN_BYTES: usize = 64;

/// Whether every token in a description is short enough to store.
///
/// Only the tokens a client writes itself can be too long. The named variants carry no
/// text, so a description of nothing but those is within the limit whatever it says.
fn browser_description_within_limits(description: &BrowserDescription) -> bool {
    let within = |token: &str| token.len() <= MAX_BROWSER_TOKEN_BYTES;
    let brand = match &description.brand {
        BrowserBrand::Other(token) => within(token),
        _ => true,
    };
    let os = match &description.os {
        OperatingSystem::Other(token) => within(token),
        _ => true,
    };
    brand && os && description.model.as_deref().is_none_or(within)
}

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
        browser_description,
        current_browser_key,
        next_browser_key,
        current_browser_key_signature,
        next_browser_key_signature,
        permissions,
        valid_for,
        max_idle,
    } = request;

    check_authz_and_record_activity(identity_number)?;
    frontend_length_within_limit(&origin).map_err(AccountSessionError::InternalCanisterError)?;
    if !browser_description_within_limits(&browser_description) {
        return Err(AccountSessionError::InternalCanisterError(
            "browser description exceeds the limit".to_string(),
        ));
    }

    // Everything that can refuse, before anything is stored — and among the refusals, the
    // cheap one first. An account this identity does not hold is the likeliest legitimate
    // failure, so a request that was never going to succeed does not pay for two P-256
    // verifications on the way to being told so. Nothing is revealed by the order:
    // `check_authz_and_record_activity` above is the auth guard, so only the identity's
    // own holder gets this far.
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

    let browser_keys = verify_browser_keys(
        &current_browser_key,
        &current_browser_key_signature,
        &next_browser_key,
        &next_browser_key_signature,
        &session_key,
    )
    .ok_or(AccountSessionError::InvalidBrowserKey)?;

    let now = time();
    let valid_till = now.saturating_add(
        valid_for
            .unwrap_or(DEFAULT_SESSION_TTL_NS)
            .clamp(MIN_SESSION_TTL_NS, MAX_SESSION_TTL_NS),
    );
    let access = DelegationAccess::from(permissions);
    let read_only = access == DelegationAccess::ReadOnly;

    // The browser is resolved by the write, not here: registering it can put the registry
    // over its cap, and the browser that gives way takes its sessions with it. That is one
    // change with the session created below, so it is one write, and working any of it out
    // here would be working out something the write has to be told.
    let (_, session) = storage_borrow_mut(|storage| {
        storage.create_session(CreateSessionParams {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
            browser_keys,
            browser_description,
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
    let account_principal =
        get_account_principal_for_origin(identity_number, &origin, account_number)
            .expect("failed to derive the principal of an account that was just read");

    state::signature_map_mut(|sigs| {
        add_delegation_signature(
            sigs,
            session_key,
            seed.as_ref(),
            session.valid_till_ns,
            Some(&session_delegation_targets()),
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
    frontend_length_within_limit(&origin).map_err(AccountSessionError::InternalCanisterError)?;

    // `prepare_account_session` handed the browser this id, so the session is named
    // exactly rather than searched for. An id naming a session that was replaced since
    // finds nothing, which is the honest answer: the delegation this call is collecting
    // was signed for the session that is gone.
    let session = storage_borrow(|storage| {
        storage.read_session(&SessionLocator {
            anchor_number: identity_number,
            origin: origin.clone(),
            account_number,
            session_id,
        })
    })
    .ok_or(AccountSessionError::NoSuchSession)?;

    // Not `NoSuchSession`: the session was just read. A seed that will not derive means
    // the salt is unset, which is a canister that never initialised rather than anything
    // this caller named.
    let seed = session_identity(identity_number, &origin, account_number, &session)?;

    // The session is there and its seed derives, so what is missing is the signature for
    // this `(session_key, expiration)` — a delegation, not a session. Saying
    // `NoSuchSession` sent the frontend to sign in again when the answer is to ask with
    // the parameters `prepare_account_session` signed.
    let signed_delegation = get_session_delegation(&seed, &session_key, expiration)
        .ok_or(AccountSessionError::NoSuchDelegation)?;

    Ok(GetAccountSessionResponse { signed_delegation })
}

/// The one canister a session credential may call.
///
/// It exists to mint app delegations, which is an update call on this canister and
/// nothing else. Scoping it here rather than leaving it to the frontend to add is the
/// difference between a restriction and a request: the party holding the session key is
/// the party that would have to add it, and can decline to.
///
/// `permissions` stays absent for the same reason it is set on app delegations: minting
/// is an update call, so a read-only session that could not make one could not sign in
/// to an app at all. Read-only travels on the app delegation this credential mints.
fn session_delegation_targets() -> Vec<Principal> {
    vec![ic_cdk::id()]
}

/// The signature `prepare_account_session` added, or `None` where it never signed for
/// this `(session_key, expiration)`.
///
/// The message must match the one signed byte for byte, targets included, which is why
/// both sides take them from [`session_delegation_targets`].
fn get_session_delegation(
    seed: &Hash,
    session_key: &[u8],
    expiration: Timestamp,
) -> Option<SignedDelegation> {
    let targets = session_delegation_targets();
    let signed_targets = crate::delegation::target_bytes(&targets);
    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed,
            message: &crate::delegation::delegation_signature_msg_with_permissions(
                session_key,
                expiration,
                Some(&signed_targets),
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
            targets: Some(targets),
            permissions: None,
        },
        signature: ByteBuf::from(signature),
    })
}

fn session_identity(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
    session: &Session,
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
fn get_account_principal_for_origin(
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
    let AuthorizedSession {
        locator,
        account,
        session,
    } = authorize_session(now)?;

    // Everything that can refuse, before the stamp. Returning `Err` on the IC commits
    // whatever was written before it — only a trap rolls back — so a stamp above this
    // would leave the session recorded as used while the caller is told the call failed.
    // All three depend on values already in hand, so there is nothing to gain by
    // computing them later.
    let expiration = u64::min(
        now.saturating_add(APP_DELEGATION_TTL_NS),
        session.valid_till_ns,
    );
    let seed = account_seed(&account)?;
    let access = DelegationAccess::from_read_only(session.read_only);

    storage_borrow_mut(|storage| storage.record_session_use(&locator, now))
        .map_err(|err| AppSessionError::InternalCanisterError(err.to_string()))?;

    state::signature_map_mut(|sigs| {
        add_delegation_signature(
            sigs,
            request.session_key,
            seed.as_ref(),
            expiration,
            // Unscoped on purpose: an app calls whatever canisters it likes. The session
            // credential this was minted from is the scoped one.
            None,
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
    let AuthorizedSession {
        account, session, ..
    } = authorize_session(now)?;

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

/// A live session the caller has been proved to be, and where it lives.
///
/// Only [`authorize_session`] constructs one, so holding it is the evidence rather than
/// three values a caller gathered: the principal lookup and the liveness check have both
/// happened, and no field can be here without them.
struct AuthorizedSession {
    locator: SessionLocator,
    account: Account,
    session: Session,
}

/// Authenticates a refresh from `caller()` alone.
///
/// The session index is keyed by the principal a session's chain is rooted at, so a hit is
/// itself the proof that the caller is that session: nothing is named in the request and
/// nothing is attached to it.
fn authorize_session(now: Timestamp) -> Result<AuthorizedSession, AppSessionError> {
    let (locator, account, session) = find_caller_session()?;
    // Either bound: a session past its lifetime and one nobody has used for longer
    // than it was allowed are equally gone, and a refresh is the thing that finds out.
    if session.is_expired_or_idle(now) {
        return Err(AppSessionError::NoMatchingSession);
    }
    Ok(AuthorizedSession {
        locator,
        account,
        session,
    })
}

/// Signs the caller's own session out. A caller cannot produce another session's
/// principal, so the seed match is the whole authorization.
///
/// Idempotent: a session that is not there is `Ok`, because the caller wanted it gone and
/// it is gone, and they could not tell a pruned session from one that never existed
/// anyway. Only a storage failure comes back as an error — an app is never handed a
/// refusal whose only sane response is to do nothing.
pub fn app_revoke_session(now: Timestamp) -> Result<(), AppSessionError> {
    // Found rather than authorized, so a session that is present and past its bounds is
    // not refused: it is still the caller's to sign out, and refusing would leave its
    // record and index entry behind.
    let Ok((locator, _, _)) = find_caller_session() else {
        return Ok(());
    };
    match storage_borrow_mut(|storage| storage.revoke_session(&locator, now)) {
        Ok(()) | Err(StorageError::SessionNotFound { .. }) => Ok(()),
        Err(err) => Err(AppSessionError::InternalCanisterError(err.to_string())),
    }
}

/// The caller's session as stored, without asking whether it is still live.
fn find_caller_session() -> Result<(SessionLocator, Account, Session), AppSessionError> {
    let locator = storage_borrow(|storage| storage.lookup_session_with_principal(caller()))
        .ok_or(AppSessionError::NoMatchingSession)?;

    let (account, session) = storage_borrow(|storage| {
        Some((
            storage.read_account(&locator.account_key())?,
            storage.read_session(&locator)?,
        ))
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

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::FormFactor;

    fn description(
        brand: BrowserBrand,
        os: OperatingSystem,
        model: Option<&str>,
    ) -> BrowserDescription {
        BrowserDescription {
            brand,
            os,
            form_factor: FormFactor::Desktop,
            model: model.map(str::to_string),
        }
    }

    /// The named variants carry no text of their own, so nothing about them can be too
    /// long however many of them there are.
    #[test]
    fn a_description_of_named_tokens_is_always_within_the_limit() {
        assert!(browser_description_within_limits(&description(
            BrowserBrand::Safari,
            OperatingSystem::Ipados,
            None
        )));
    }

    #[test]
    fn each_token_a_client_writes_is_bounded() {
        let long = "x".repeat(MAX_BROWSER_TOKEN_BYTES + 1);
        let at_limit = "x".repeat(MAX_BROWSER_TOKEN_BYTES);

        assert!(browser_description_within_limits(&description(
            BrowserBrand::Other(at_limit.clone()),
            OperatingSystem::Other(at_limit.clone()),
            Some(&at_limit)
        )));

        for over in [
            description(
                BrowserBrand::Other(long.clone()),
                OperatingSystem::Macos,
                None,
            ),
            description(
                BrowserBrand::Chrome,
                OperatingSystem::Other(long.clone()),
                None,
            ),
            description(BrowserBrand::Chrome, OperatingSystem::Macos, Some(&long)),
        ] {
            assert!(
                !browser_description_within_limits(&over),
                "a token of {} bytes should be refused: {over:?}",
                long.len()
            );
        }
    }
}
