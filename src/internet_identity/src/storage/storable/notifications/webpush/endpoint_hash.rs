use crate::utils::{sha256sum, slice_to_bounded_32};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use std::borrow::Cow;

/// SHA-256 of a push relay endpoint URL — a fixed 32-byte key so a long relay
/// URL doesn't bloat the subscription row's key.
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

    /// Range-scan lower bound for a per-anchor prefix over the subscriptions map.
    pub const MIN: Self = Self { hash: [0u8; 32] };
    /// Range-scan upper bound.
    pub const MAX: Self = Self { hash: [0xffu8; 32] };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_endpoint_hashes_equal() {
        let a = StorableEndpointSha256::from_endpoint("https://example.com/push/abc");
        let b = StorableEndpointSha256::from_endpoint("https://example.com/push/abc");
        assert_eq!(a, b);
    }

    #[test]
    fn min_and_max_bound_every_hash() {
        let hash = StorableEndpointSha256::from_endpoint("https://example.com/push/abc");
        assert!(StorableEndpointSha256::MIN <= hash);
        assert!(hash <= StorableEndpointSha256::MAX);
    }
}
