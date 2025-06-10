use candid::{CandidType, Principal};

use crate::{
    authz_utils::{AuthorizationError, IdentityUpdateError},
    delegation,
};
use ic_cdk::trap;
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNameValidationError, AccountNumber, AnchorNumber, ApplicationNumber,
    FrontendHostname, Timestamp, UserKey,
};
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests;

// API to manage accounts.
pub struct CreateAccountParams {
    pub anchor_number: AnchorNumber,
    pub name: String,
    pub origin: FrontendHostname,
}

pub struct UpdateAccountParams {
    pub account_number: Option<AccountNumber>,
    pub anchor_number: AnchorNumber,
    pub name: String,
    pub origin: FrontendHostname,
}

pub struct UpdateExistingAccountParams {
    pub account_number: AccountNumber,
    pub anchor_number: AnchorNumber,
    pub name: String,
    pub origin: FrontendHostname,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReadAccountParams<'a> {
    pub account_number: Option<AccountNumber>,
    pub anchor_number: AnchorNumber,
    pub origin: &'a FrontendHostname,
    pub known_app_num: Option<ApplicationNumber>,
}

// Types used internally to encapsulate business logic and data.

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct AccountsCounter {
    pub stored_accounts: u64,
    pub stored_account_references: u64,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is the unreserved default account
    pub last_used: Option<Timestamp>,
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
            last_used: None,
            name,
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
        AccountReference {
            account_number: self.account_number,
            last_used: self.last_used,
        }
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
        // If this is a non-stored default account, we derive from frontend and anchor
        if self.account_number.is_none() {
            return delegation::calculate_anchor_seed(self.anchor_number, &self.origin);
        }

        match (self.get_seed_anchor(), self.account_number) {
            (Some(seed_from_anchor), _) => {
                // If this is a stored default account, we derive from frontend and anchor
                delegation::calculate_anchor_seed(seed_from_anchor, &self.origin)
            }
            (None, Some(account_number)) => {
                // If this is an added account, we derive from the account number and origin.
                delegation::calculate_account_seed(account_number, &self.origin)
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
