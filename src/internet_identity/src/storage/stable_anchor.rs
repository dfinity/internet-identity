use crate::openid::OpenIdCredential;
use candid::CandidType;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::ApplicationNumber;
use serde::Deserialize;
use std::borrow::Cow;
use std::collections::HashMap;

use super::account::StorableAccountReference;

#[derive(CandidType, Deserialize, Clone, Debug)]
pub struct StableAnchor {
    pub name: Option<String>,
    pub openid_credentials: Vec<OpenIdCredential>,
    pub application_accounts:
        Option<HashMap<ApplicationNumber, Vec<StorableAccountReference>>>,
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
