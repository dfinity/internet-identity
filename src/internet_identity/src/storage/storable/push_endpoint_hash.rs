use crate::utils::{sha256sum, slice_to_bounded_32};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use std::borrow::Cow;

/// SHA-256 of a push relay endpoint URL. Keyed alongside `AnchorNumber`
/// in `push_subscriptions_memory` under the Option-A design: subscriptions
/// belong to a `(anchor, device)` pair (where "device" is a specific
/// browser's relay endpoint), independent of any dApp origin.
///
/// Fixed 32-byte encoding — total per-row overhead stays bounded even
/// for very long relay URLs.
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableEndpointSha256 {
    hash: [u8; 32],
}

impl StorableEndpointSha256 {
    pub fn from_endpoint(endpoint: &str) -> Self {
        Self {
            hash: sha256sum(endpoint.as_bytes()),
        }
    }
}

impl Storable for StorableEndpointSha256 {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(self.hash.to_vec())
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        Self {
            hash: slice_to_bounded_32(bytes.as_ref()),
        }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 32,
        is_fixed_size: true,
    };
}
