use candid::{CandidType, Principal};

use crate::{
    authz_utils::{AuthorizationError, IdentityUpdateError},
    delegation,
};
use ic_cdk::trap;
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNameValidationError, AccountNumber, AnchorNumber, FrontendHostname,
    SessionDeviceId, SessionId, Timestamp, UserKey,
};
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests;

/// An account's address: the identity, the origin, and which of that identity's
/// accounts there, `None` being the tracked default.
///
/// Carries no capability. The seed an account signs with lives on [`Account`], which
/// only [`crate::storage::Storage::read_account`] hands out, and only after checking
/// that the identity holds a reference to it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AccountKey {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
}

// Types used internally to encapsulate business logic and data.

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct AccountsCounter {
    pub stored_accounts: u64,
    pub stored_account_references: u64,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is the unreserved synthetic account
    pub last_used: Option<Timestamp>,
    pub sessions: Vec<SessionRecord>,
}

impl AccountReference {
    pub fn new(account_number: Option<AccountNumber>, last_used: Option<Timestamp>) -> Self {
        Self {
            account_number,
            last_used,
            sessions: vec![],
        }
    }
}

/// The shortest idle bound a session may be given.
///
/// An app delegation lasts five minutes and an active application replaces it a
/// little before it expires, so a bound anywhere near that would end sessions
/// plainly in use. Ten minutes is already the floor on a session's own length,
/// so this shares that range rather than introducing a second one.
pub const MIN_SESSION_IDLE_NS: u64 = 10 * crate::MINUTE_NS;

/// What a session gets when its ceremony asks for no bound of its own.
///
/// Seven days of nobody touching an application ends the sign-in, well inside the
/// thirty days a session may otherwise live. It is the length of an absence rather
/// than of a session: coming back inside a week keeps you signed in indefinitely,
/// and a machine walked away from stops being signed in within one.
pub const DEFAULT_SESSION_IDLE_NS: u64 = 7 * crate::DAY_NS;

/// Where one session is stored, and which session it is.
///
/// The account addresses the row; `session_id` picks the record out of it. The id is
/// unique on its own, so every operation is compare-and-act: a key for a session that
/// was replaced reads as `None` and revokes nothing, instead of landing on its
/// successor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionRecordKey {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_id: SessionId,
}

impl SessionRecordKey {
    /// The account this session is at.
    pub fn account(&self) -> AccountKey {
        AccountKey {
            anchor_number: self.anchor_number,
            origin: self.origin.clone(),
            account_number: self.account_number,
        }
    }
}

/// A revocable session at one account.
///
/// `session_id` is what the seed binds, so the identity this session signs with is
/// tied to the one record that was allocated that id. Every other field describes the
/// session and can be rewritten without changing who it signs as.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct SessionRecord {
    pub created_at_ns: Timestamp,
    pub valid_till_ns: Timestamp,
    pub max_idle_ns: u64,
    pub last_refreshed_ns: Option<Timestamp>,
    pub device_id: SessionDeviceId,
    pub read_only: bool,
    pub session_id: SessionId,
}

impl SessionRecord {
    /// Whether this session is finished, on either bound.
    ///
    /// One question rather than two, because a caller has no use for the halves
    /// apart: a session past its lifetime and one nobody has used for longer than
    /// it was allowed are equally over. Asking separately is how a caller ends up
    /// checking one and forgetting the other.
    ///
    /// Idleness is measured from the last mint, or from creation where nothing has
    /// minted yet, so a session abandoned immediately after sign-in is bounded like
    /// any other.
    pub fn is_over(&self, now: Timestamp) -> bool {
        if self.valid_till_ns <= now {
            return true;
        }
        let last_used = self.last_refreshed_ns.unwrap_or(self.created_at_ns);
        now.saturating_sub(last_used) >= self.max_idle_ns
    }

    /// How long this session stayed in service: the span from its creation to the last time
    /// its app asked for a delegation. Bounded by the session's own lifetime.
    pub fn demonstrated_use(&self) -> u64 {
        self.last_refreshed_ns
            .map_or(0, |refreshed| refreshed.saturating_sub(self.created_at_ns))
    }

    /// What the caps reclaim on, ascending: dead sessions first, then live ones by how
    /// recently used, extended by how long they stayed in service.
    ///
    /// The extension is what separates an app in weekly use from one opened once and
    /// abandoned, which recency alone gets backwards — the abandoned one was touched more
    /// recently. `session_id` only makes the order total, which it can because no two
    /// sessions share one.
    pub fn reclaim_order(&self, now: Timestamp) -> (bool, Timestamp, SessionId) {
        let last_used = self.last_refreshed_ns.unwrap_or(self.created_at_ns);
        (
            !self.is_over(now),
            last_used.saturating_add(self.demonstrated_use()),
            self.session_id,
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Account {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
    seed_from_anchor: Option<AnchorNumber>,
}

impl Account {
    /// An identity's default account at an origin, derived rather than stored.
    ///
    /// Test-only. In production every account comes out of
    /// [`crate::storage::Storage::read_account`], which builds this one only where the
    /// identity's row still names it — a derived default handed out without that check
    /// would sign for an origin the identity may have moved every account away from.
    #[cfg(test)]
    pub fn synthetic(anchor_number: AnchorNumber, origin: FrontendHostname) -> Self {
        Self {
            anchor_number,
            origin,
            account_number: None,
            last_used: None,
            name: None,
            seed_from_anchor: None,
        }
    }

    pub fn new(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
        account_number: Option<AccountNumber>,
    ) -> Account {
        Self {
            account_number,
            anchor_number,
            origin,
            name,
            last_used: None,
            seed_from_anchor: None,
        }
    }

    pub fn new_with_last_used(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
        account_number: Option<AccountNumber>,
        last_used: Option<Timestamp>,
    ) -> Account {
        Self {
            account_number,
            anchor_number,
            origin,
            last_used,
            name,
            seed_from_anchor: None,
        }
    }

    pub fn new_full(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
        account_number: Option<AccountNumber>,
        last_used: Option<Timestamp>,
        seed_from_anchor: Option<AnchorNumber>,
    ) -> Account {
        Self {
            account_number,
            anchor_number,
            origin,
            last_used,
            name,
            seed_from_anchor,
        }
    }

    fn get_seed_anchor(&self) -> Option<AnchorNumber> {
        self.seed_from_anchor
    }

    // Used in tests (for now)
    #[allow(dead_code)]
    pub fn to_reference(&self) -> AccountReference {
        AccountReference::new(self.account_number, self.last_used)
    }

    pub fn to_info(&self) -> AccountInfo {
        AccountInfo {
            account_number: self.account_number,
            origin: self.origin.clone(),
            last_used: self.last_used,
            name: self.name.clone(),
        }
    }

    /// Create `Hash` used for a delegation that can make calls on behalf of an `Account`.
    /// If the `Account` is a non-stored default account or has a `seed_from_anchor` (and thus is a stored default account),
    /// the respective anchor number will be used as a seed input. Otherwise, the `AccountNumber` is used.
    ///
    /// # Arguments
    ///
    /// * `account` is the `Account` we're using for this delegation
    pub fn calculate_seed(&self) -> Hash {
        self.calculate_seed_with_salt(&crate::state::salt())
    }

    pub fn calculate_seed_with_salt(&self, salt: &[u8; 32]) -> Hash {
        // If this is a non-stored default account, we derive from frontend and anchor
        if self.account_number.is_none() {
            return delegation::calculate_anchor_seed_with_salt(
                salt,
                self.anchor_number,
                &self.origin,
            );
        }

        match (self.get_seed_anchor(), self.account_number) {
            (Some(seed_from_anchor), _) => {
                // If this is a stored default account, we derive from frontend and anchor
                delegation::calculate_anchor_seed_with_salt(salt, seed_from_anchor, &self.origin)
            }
            (None, Some(account_number)) => {
                // If this is an added account, we derive from the account number and origin.
                delegation::calculate_account_seed_with_salt(salt, account_number, &self.origin)
            }
            (None, None) => trap("Attempted to calculate an account seed from an account without seed anchor or anchor number - this should never happen!")
        }
    }
}

#[derive(CandidType, Debug, Serialize, Deserialize)]
pub enum AccountDelegationError {
    Unauthorized(Principal),
    InternalCanisterError(String),
    NoSuchDelegation,
}

impl From<AuthorizationError> for AccountDelegationError {
    fn from(err: AuthorizationError) -> Self {
        AccountDelegationError::Unauthorized(err.principal)
    }
}

impl From<IdentityUpdateError> for AccountDelegationError {
    fn from(err: IdentityUpdateError) -> Self {
        match err {
            IdentityUpdateError::Unauthorized(principal) => {
                AccountDelegationError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(_identity_number, storage_error) => {
                AccountDelegationError::InternalCanisterError(storage_error.to_string())
            }
        }
    }
}

#[derive(CandidType, Serialize)]
pub struct PrepareAccountDelegation {
    pub user_key: UserKey,
    pub expiration: Timestamp,
}

pub(crate) fn validate_account_name(name: &str) -> Result<(), AccountNameValidationError> {
    const ACCOUNT_NAME_LIMIT: usize = 255;

    if name.len() > ACCOUNT_NAME_LIMIT {
        return Err(AccountNameValidationError::NameTooLong);
    }

    Ok(())
}
