use internet_identity_interface::internet_identity::types::{
    AccountInfo, AccountNumber, AnchorNumber, FrontendHostname, Timestamp,
};

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

// Types used internally to encapsulate business logic and data.

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
        }
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
