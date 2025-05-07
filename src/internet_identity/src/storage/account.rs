use candid::CandidType;
use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, Timestamp,
};
use serde::Deserialize;
use std::{
    borrow::Cow,
    hash::{DefaultHasher, Hash, Hasher},
};

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AccountReference {
    pub account_number: Option<AccountNumber>, // None is the unreserved default account
    pub last_used: Option<Timestamp>,
}

impl From<(&AnchorNumber, &StorableAccountReference)> for AccountReference {
    fn from(value: (&AnchorNumber, &StorableAccountReference)) -> Self {
        let storable_acc_ref = value.1;

        Self {
            account_number: storable_acc_ref.account_number,
            last_used: storable_acc_ref.last_used,
        }
    }
}

#[derive(
    Clone, Debug, Deserialize, CandidType, serde::Serialize, Eq, PartialEq, Ord, PartialOrd,
)]
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
pub struct InternalAccount {
    pub account_number: Option<AccountNumber>, // None is unreserved default account
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
}

impl InternalAccount {
    pub fn new(
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
        name: Option<String>,
        account_number: Option<AccountNumber>,
    ) -> InternalAccount {
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
}
