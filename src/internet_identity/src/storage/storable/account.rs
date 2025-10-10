use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableAccount {
    #[n(0)]
    pub name: String,
    #[n(1)]
    pub seed_from_anchor: Option<StorableAnchorNumber>, // Set if this is a default account
    #[n(2)]
    pub last_used: Option<Timestamp>,
}

impl Storable for StorableAccount {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccount");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccount")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Encode, Decode, Clone, PartialEq, Eq, Debug)]
    #[cbor(map)]
    pub struct StorableAccountV1 {
        #[n(0)]
        pub name: String,
        #[n(1)]
        pub seed_from_anchor: Option<StorableAnchorNumber>,
    }

    #[derive(Encode, Decode, Clone, PartialEq, Eq, Debug)]
    #[cbor(map)]
    pub struct StorableAccountV2 {
        #[n(0)]
        pub name: String,
        #[n(1)]
        pub seed_from_anchor: Option<StorableAnchorNumber>,
        #[n(2)]
        pub last_used: Option<Timestamp>,
    }

    fn to_bytes(x: &StorableAccountV1) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(x, &mut buffer).expect("failed to encode StorableAccount");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> StorableAccountV2 {
        minicbor::decode(&bytes).expect("failed to decode StorableAccount")
    }

    #[test]
    fn test_v1_to_v2() {
        let v1 = StorableAccountV1 {
            name: "John".to_string(),
            seed_from_anchor: Some(123),
        };

        let bytes = to_bytes(&v1);
        let v2: StorableAccountV2 = from_bytes(bytes);
        assert_eq!(
            v2,
            StorableAccountV2 {
                name: "John".to_string(),
                seed_from_anchor: Some(123),
                last_used: None,
            }
        );
    }
}
