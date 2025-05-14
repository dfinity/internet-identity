use super::account::AccountReference;
use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use serde::Deserialize;
use std::borrow::Cow;

/// Vectors are not supported yet in ic-stable-structures, this file
/// implements a struct to wrap this vector so it can be stored.
#[derive(Deserialize, CandidType, Clone, Ord, Eq, PartialEq, PartialOrd, Default)]
pub struct StorableAccountReferenceList(Vec<AccountReference>);

impl StorableAccountReferenceList {
    pub fn to_acc_ref_vec(self) -> Vec<AccountReference> {
        self.0
    }
}

impl From<StorableAccountReferenceList> for Vec<AccountReference> {
    fn from(value: StorableAccountReferenceList) -> Self {
        value.0
    }
}

impl From<Vec<AccountReference>> for StorableAccountReferenceList {
    fn from(value: Vec<AccountReference>) -> Self {
        StorableAccountReferenceList(value)
    }
}

impl Storable for StorableAccountReferenceList {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut candid = candid::encode_one(self)
            .expect("Failed to serialize StorableAccountReferenceList to candid");
        let mut buf = (candid.len() as u16).to_le_bytes().to_vec(); // 2 bytes for length
        buf.append(&mut candid);
        Cow::Owned(buf)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let length = u16::from_le_bytes(bytes[..2].try_into().unwrap()) as usize;

        candid::decode_one(&bytes[2..length + 2])
            .expect("Failed to deserialize StorableAccountReferenceList from candid")
    }

    const BOUND: Bound = Bound::Unbounded;
}
