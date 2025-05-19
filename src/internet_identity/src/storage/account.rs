use candid::{CandidType, Principal};

use ic_canister_sig_creation::hash_bytes;
use ic_certification::Hash;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNumber, AnchorNumber, FrontendHostname, Timestamp, UserKey,
};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

use crate::{authz_utils::IdentityUpdateError, delegation, state};

use super::anchor;

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

pub struct UpdateExistinAccountParams {
    pub account_number: AccountNumber,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReadAccountParams<'a> {
    pub account_number: Option<AccountNumber>,
    pub anchor_number: AnchorNumber,
    pub origin: &'a FrontendHostname,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AccountType {
    AccountReference,
    Account,
}

// Types stored in memory.

#[derive(Default, Clone, Debug, Deserialize, CandidType, serde::Serialize, PartialEq)]
pub struct AccountsCounter {
    pub stored_accounts: u64,
    pub stored_account_references: u64,
}

impl AccountsCounter {
    pub fn increment(&self, account_type: &AccountType) -> Self {
        match account_type {
            AccountType::AccountReference => Self {
                stored_account_references: self.stored_account_references + 1,
                stored_accounts: self.stored_accounts,
            },
            AccountType::Account => Self {
                stored_accounts: self.stored_accounts + 1,
                stored_account_references: self.stored_account_references,
            },
        }
    }
}

impl Storable for AccountsCounter {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(
    Clone, Debug, Deserialize, CandidType, serde::Serialize, Eq, PartialEq, Ord, PartialOrd,
)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is the unreserved default account
    pub last_used: Option<Timestamp>,
}

impl Storable for AccountReference {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq)]
pub struct StorableAccount {
    pub name: String,
    // Set if this is a default account
    pub seed_from_anchor: Option<AnchorNumber>,
}

impl Storable for StorableAccount {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

// Types used internally to encapsulate business logic and data.

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
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
            return delegation::calculate_seed(self.anchor_number, &self.origin);
        }

        match self.get_seed_anchor() {
            Some(seed_from_anchor) => {
                // If this is a stored default account, we derive from frontend and anchor
                delegation::calculate_seed(seed_from_anchor, &self.origin)
            }
            None => {
                // If this is an added account, we derive from the account number.
                let salt = state::salt();

                let mut blob: Vec<u8> = vec![];
                blob.push(salt.len() as u8);
                blob.extend_from_slice(&salt);

                let account_number_str = self.account_number.unwrap().to_string(); // XXX: this should be safe because an account without a seed_from_anchor must always have an account_number
                let account_number_blob = account_number_str.bytes();
                blob.push(account_number_blob.len() as u8);
                blob.extend(account_number_blob);

                hash_bytes(blob)
            }
        }
    }
}

#[derive(CandidType, Debug, Serialize, Deserialize)]
pub enum AccountDelegationError {
    Unauthorized(Principal),
    InternalCanisterError(String),
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
    pub timestamp: Timestamp,
}
