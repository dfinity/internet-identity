use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
#[cbor(map)]
pub struct StorableAccountsCounter {
    #[n(0)]
    pub stored_accounts: u64,
    #[n(1)]
    pub stored_account_references: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AccountType {
    AccountReference,
    Account,
}

impl StorableAccountsCounter {
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

impl Storable for StorableAccountsCounter {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountsCounter");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountsCounter")
    }

    const BOUND: Bound = Bound::Unbounded;
}
