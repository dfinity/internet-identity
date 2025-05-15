use candid::{CandidType, Principal};

use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNumber, AnchorNumber, FrontendHostname, Timestamp,
};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

use crate::authz_utils::IdentityUpdateError;

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

    // right now only used in tests
    #[allow(dead_code)]
    pub fn new_with_seed_anchor(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
        account_number: Option<AccountNumber>,
        seed_from_anchor: Option<AnchorNumber>,
    ) -> Account {
        Self {
            account_number,
            anchor_number,
            origin,
            last_used: None,
            name,
            seed_from_anchor,
        }
    }

    pub fn get_seed_anchor(&self) -> Option<AnchorNumber> {
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
}

#[derive(CandidType, Serialize, Deserialize)]
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
