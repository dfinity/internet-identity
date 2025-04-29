use candid::CandidType;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationNumber, FrontendHostname, Timestamp,
};
use serde::Deserialize;
use std::{
    borrow::Cow,
    hash::{DefaultHasher, Hash, Hasher},
};

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
struct Account {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
    seed_from_anchor: AnchorNumber,
}

impl Account {
    pub fn seed() {}
    pub fn new(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
    ) -> Account {
        // TODO lookup existing accounts for this anchor and origin

        Self {
            account_number: todo!(),
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
        // TODO: handle origin (lookup origin, if exists get number, otherwise create and get number)

        StorableAccount {
            account_number: self.account_number,
            anchor_number: self.anchor_number,
            origin_number: todo!(),
            last_used: self.last_used,
            name: self.name,
            seed_from_anchor: self.seed_from_anchor,
        }
    }
}

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq)]
pub struct StorableAccount {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin_number: ApplicationNumber,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
    seed_from_anchor: AnchorNumber,
}

impl StorableAccount {
    pub fn to_account(&self) -> Account {
        // TODO: handle origin (lookup origin, get number)
        Account {
            account_number: self.account_number,
            anchor_number: self.anchor_number,
            origin: todo!(),
            last_used: self.last_used,
            name: self.name,
            seed_from_anchor: self.seed_from_anchor,
        }
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

#[derive(Clone, Debug, Deserialize, serde::Serialize, Eq, PartialEq)]
pub struct StorableApplication {
    pub origin: FrontendHostname,
    pub total_accounts: u64,
}

impl Storable for StorableApplication {
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
    pub fn from_origin(origin: FrontendHostname) -> Self {
        let mut hasher = DefaultHasher::new();
        origin.hash(&mut hasher);
        let hash_u64 = hasher.finish();
        Self {
            hash: hash_u64.to_le_bytes(),
        }
    }
}
