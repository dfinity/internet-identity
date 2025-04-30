use candid::CandidType;
use ic_canister_sig_creation::hash_bytes;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationNumber, FrontendHostname, Timestamp,
};
use serde::Deserialize;
use std::{
    borrow::Cow,
    hash::{DefaultHasher, Hash, Hasher},
};

use crate::state::{self, storage_borrow, storage_borrow_mut};

use super::{stable_anchor::StableAnchor, storable_anchor::StorableAnchor};

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
}

impl AccountReference {
    pub fn to_storable(&self) -> StorableAccountReference {
        StorableAccountReference {
            account_number: self.account_number,
            last_used: self.last_used,
            application_number: storage_borrow_mut(|storage| {
                storage.lookup_or_insert_application_number_with_origin(&self.origin)
            }),
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
            origin: storage_borrow(|storage| {
                storage.lookup_application_with_application_number(
                    &storable_acc_ref.application_number,
                )
            })
            .and_then(|app| Some(app.origin))
            .expect("Help! Could not find an origin!"), // XXX: This might be handled more elegantly at some point.
            last_used: storable_acc_ref.last_used,
        }
    }
}

#[derive(Clone, Debug, Deserialize, CandidType, serde::Serialize, Eq, PartialEq)]
pub struct StorableAccountReference {
    account_number: Option<AccountNumber>,
    last_used: Option<Timestamp>,
    // XXX: I know the design doc doesn't include this but not having it makes conversion rather tedious (feel free to refactor)
    application_number: ApplicationNumber,
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
    pub fn to_reference(&self) -> AccountReference {
        AccountReference {
            account_number: self.account_number.clone(),
            anchor_number: self.anchor_number.clone(),
            origin: self.origin.clone(),
            last_used: self.last_used.clone(),
        }
    }

    pub fn to_storable(&self) -> StorableAccount {
        StorableAccount {
            name: self.name.clone(),
            seed_from_anchor: self.seed_from_anchor.clone(),
        }
    }

    // pub fn from_storable_and_anchor(
    //     storable_acc: StorableAccount,
    //     anchor_number: AnchorNumber,
    // ) -> Self {
    //     let storable_ref = storage_borrow(|storage| {
    //         storage.
    //     });
    //     Self {
    //         account_number: (),
    //         anchor_number: (),
    //         origin: (),
    //         last_used: (),
    //         name: storable_acc.name,
    //         seed_from_anchor: storable_acc.seed_from_anchor,
    //     }
    // }
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
