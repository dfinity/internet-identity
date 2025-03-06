use crate::openid::OpenIdCredential;
use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use serde::Deserialize;
use std::borrow::Cow;

#[derive(CandidType, Deserialize, Clone)]
pub struct StableAnchor {
    pub openid_credentials: Vec<OpenIdCredential>,
}

impl Storable for StableAnchor {
    fn to_bytes(&self) -> Cow<[u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to encode StableAnchor"))
    }
    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to decode StableAnchor")
    }

    const BOUND: Bound = Bound::Unbounded;
}
