use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::smtp::{
    DkimVerificationStatus, MAX_BODY_BYTES, MAX_EMAILS_PER_USER, MAX_EMAIL_DOMAIN_BYTES,
    MAX_EMAIL_USER_BYTES, MAX_SUBJECT_BYTES,
};
use std::borrow::Cow;

/// Max serialized size of a single email address key.
/// user (64) + "@" (1) + domain (255) + Candid overhead (~20 bytes).
const STORABLE_EMAIL_ADDRESS_MAX_SIZE: u32 =
    (MAX_EMAIL_USER_BYTES + 1 + MAX_EMAIL_DOMAIN_BYTES + 20) as u32;

/// Max serialized size of a single email.
/// sender (320) + recipient (320) + subject (256) + body (5_000)
/// + DKIM checks (7 × ~140 bytes) + Candid overhead (~120 bytes).
const STORABLE_EMAIL_MAX_SIZE: u32 = (MAX_EMAIL_USER_BYTES + 1 + MAX_EMAIL_DOMAIN_BYTES) as u32 * 2
    + MAX_SUBJECT_BYTES as u32
    + MAX_BODY_BYTES as u32
    + 1100;

/// Max serialized size of the email list value.
const STORABLE_EMAIL_LIST_MAX_SIZE: u32 =
    STORABLE_EMAIL_MAX_SIZE * MAX_EMAILS_PER_USER as u32 + 100;

#[derive(Clone, Debug, CandidType, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableEmailAddress(pub String);

impl Storable for StorableEmailAddress {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to encode StorableEmailAddress"))
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to decode StorableEmailAddress")
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: STORABLE_EMAIL_ADDRESS_MAX_SIZE,
        is_fixed_size: false,
    };
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct StorableEmail {
    pub sender: String,
    pub recipient: String,
    pub subject: String,
    pub body: String,
    pub dkim_status: Option<DkimVerificationStatus>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct StorableEmailList {
    pub emails: Vec<StorableEmail>,
}

impl Storable for StorableEmailList {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to encode StorableEmailList"))
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to decode StorableEmailList")
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: STORABLE_EMAIL_LIST_MAX_SIZE,
        is_fixed_size: false,
    };
}
