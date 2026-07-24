use crate::storage::storable::anchor_number::StorableAnchorNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Value stored in `push_principal_index_memory`: everything
/// `notify_user` needs to locate the target subscription without having
/// to iterate over the anchor's rows.
///
/// `origin_hash` is the SHA-256 of the consenting dApp's origin (same
/// hash that keys the subscription and consent maps). We store the hash,
/// not the origin string, because the origin isn't needed for outcall
/// construction — only the subscription's endpoint is — and the
/// fixed-size hash is what the subscription/consent maps key on anyway.
#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorablePushSenderInfo {
    #[n(0)]
    pub anchor: StorableAnchorNumber,
    #[cbor(n(1), with = "minicbor::bytes")]
    pub origin_hash: [u8; 32],
}

impl Storable for StorablePushSenderInfo {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorablePushSenderInfo");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePushSenderInfo")
    }

    const BOUND: Bound = Bound::Unbounded;
}
