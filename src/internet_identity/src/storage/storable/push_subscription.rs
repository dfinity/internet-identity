use candid::{CandidType, Deserialize};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::push::{
    PushSubscription, MAX_PUSH_ENDPOINT_BYTES, MAX_PUSH_SUBSCRIPTIONS_PER_USER,
    PUSH_AUTH_SECRET_BYTES, PUSH_P256DH_KEY_BYTES,
};
use std::borrow::Cow;

/// Max serialized size of a single push subscription:
/// endpoint (2048) + p256dh (65) + auth (16) + Candid overhead (~50 bytes).
const STORABLE_PUSH_SUBSCRIPTION_MAX_SIZE: u32 = MAX_PUSH_ENDPOINT_BYTES as u32
    + PUSH_P256DH_KEY_BYTES as u32
    + PUSH_AUTH_SECRET_BYTES as u32
    + 50;

/// Max serialized size of the push subscription list value.
const STORABLE_PUSH_SUBSCRIPTION_LIST_MAX_SIZE: u32 =
    STORABLE_PUSH_SUBSCRIPTION_MAX_SIZE * MAX_PUSH_SUBSCRIPTIONS_PER_USER as u32 + 100;

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct StorablePushSubscriptionList {
    pub subscriptions: Vec<PushSubscription>,
}

impl Storable for StorablePushSubscriptionList {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(candid::encode_one(self).expect("failed to encode StorablePushSubscriptionList"))
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        candid::decode_one(&bytes).expect("failed to decode StorablePushSubscriptionList")
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: STORABLE_PUSH_SUBSCRIPTION_LIST_MAX_SIZE,
        is_fixed_size: false,
    };
}
