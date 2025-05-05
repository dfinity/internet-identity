use candid::CandidType;
use ic_canister_sig_creation::hash_bytes;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, Timestamp,
};
use serde::Deserialize;
use std::{
    borrow::Cow,
    hash::{DefaultHasher, Hash, Hasher},
};

use crate::state;

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub last_used: Option<Timestamp>,
}

impl AccountReference {
    pub fn to_storable(&self) -> StorableAccountReference {
        StorableAccountReference {
            account_number: self.account_number,
            last_used: self.last_used,
        }
    }
}

impl From<(&AnchorNumber, &StorableAccountReference)> for AccountReference {
    fn from(value: (&AnchorNumber, &StorableAccountReference)) -> Self {
        let anchor_number = value.0;
        let storable_acc_ref = value.1;

        Self {
            account_number: storable_acc_ref.account_number,
            anchor_number: anchor_number.clone(),
            last_used: storable_acc_ref.last_used,
        }
    }
}

#[derive(Clone, Debug, Deserialize, CandidType, serde::Serialize, Eq, PartialEq)]
pub struct StorableAccountReference {
    pub account_number: Option<AccountNumber>,
    pub last_used: Option<Timestamp>,
}

impl Storable for StorableAccountReference {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded; //TODO: figure out the size of two options wrapping two u64s
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct Account {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
    seed_from_anchor: AnchorNumber,
}

impl Account {
    pub fn seed(&self) -> [u8; 32] {
        let salt = state::salt();

        let mut blob: Vec<u8> = vec![];
        blob.push(salt.len() as u8);
        blob.extend_from_slice(&salt);

        let anchor_number_str = self.anchor_number.to_string();
        let anchor_number_blob = anchor_number_str.bytes();
        blob.push(anchor_number_blob.len() as u8);
        blob.extend(anchor_number_blob);

        blob.push(self.origin.len() as u8);
        blob.extend(self.origin.bytes());

        // If default account, we should match the original principal.
        if self.account_number.is_some() {
            let account_number_str = self.account_number.expect("unreachable").to_string();
            let account_number_blob = account_number_str.bytes();
            blob.push(account_number_blob.len() as u8);
            blob.extend(account_number_blob);
        }

        hash_bytes(blob)
    }

    pub fn new(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
    ) -> Account {
        Self {
            account_number: None, // TODO: this needs to depend on pre-existing accounts
            anchor_number,
            origin,
            last_used: None,
            name,
            seed_from_anchor: anchor_number,
        }
    }

    /// Reconstructs an Account from its constituent parts, including the seed.
    /// Used when reading from storage where all fields are known.
    pub fn reconstruct(
        account_number: Option<AccountNumber>,
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        last_used: Option<Timestamp>,
        name: Option<String>,
        seed_from_anchor: AnchorNumber,
    ) -> Self {
        Self {
            account_number,
            anchor_number,
            origin,
            last_used,
            name,
            seed_from_anchor, // Set the private field
        }
    }

    pub fn to_reference(&self) -> AccountReference {
        AccountReference {
            account_number: self.account_number.clone(),
            anchor_number: self.anchor_number.clone(),
            last_used: self.last_used.clone(),
        }
    }

    pub fn to_storable(&self) -> StorableAccount {
        StorableAccount {
            name: self.name.clone(),
            seed_from_anchor: self.seed_from_anchor.clone(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq)]
pub struct StorableAccount {
    pub name: Option<String>,
    seed_from_anchor: AnchorNumber,
}

impl StorableAccount {
    pub fn seed_from_anchor(&self) -> AnchorNumber {
        self.seed_from_anchor
    }
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

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq, Ord, PartialOrd)]
pub struct Application {
    pub origin: FrontendHostname,
    pub total_accounts: u64,
}

impl Storable for Application {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(serde_cbor::to_vec(&self).unwrap())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        serde_cbor::from_slice(&bytes).unwrap()
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct OriginHash {
    hash: [u8; 8],
}

impl Storable for OriginHash {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(self.hash.to_vec())
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        Self {
            hash: bytes.as_ref().try_into().unwrap(),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 8,
        is_fixed_size: true,
    };
}

impl OriginHash {
    pub fn from_origin(origin: &FrontendHostname) -> Self {
        let mut hasher = DefaultHasher::new();
        origin.hash(&mut hasher);
        let hash_u64 = hasher.finish();
        Self {
            hash: hash_u64.to_le_bytes(),
        }
    }

    pub fn get_hash(&self) -> &[u8; 8] {
        &self.hash
    }
}
