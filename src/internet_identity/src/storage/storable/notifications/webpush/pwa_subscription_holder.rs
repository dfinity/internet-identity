use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::application_number::StorableApplicationNumber;
use crate::storage::storable::browser_id::StorableBrowserId;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The installed app subscription holding a push endpoint: the identity, the application,
/// and the browser entry and frontend origin hash it is keyed by on that identity's
/// application config.
#[derive(Encode, Decode, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
// An array rather than a map: a derived index is rebuilt from the configs it mirrors, so it
// has no rows written before a field existed to decode.
#[cbor(array)]
pub struct StorablePwaSubscriptionHolder {
    #[n(0)]
    pub anchor_number: StorableAnchorNumber,
    #[n(1)]
    pub application_number: StorableApplicationNumber,
    #[n(2)]
    pub browser_id: StorableBrowserId,
    #[n(3)]
    pub frontend_origin_hash: StorableOriginSha256,
}

impl Storable for StorablePwaSubscriptionHolder {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer)
            .expect("failed to encode StorablePwaSubscriptionHolder");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePwaSubscriptionHolder")
    }

    /// The array header, two maximal `u64`s at nine bytes, a maximal `u32` at five, and a
    /// 32-byte string with its two-byte header.
    const BOUND: Bound = Bound::Bounded {
        max_size: 58,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_largest_holder_fits_the_bound() {
        let holder = StorablePwaSubscriptionHolder {
            anchor_number: u64::MAX,
            application_number: u64::MAX,
            browser_id: u32::MAX,
            frontend_origin_hash: StorableOriginSha256::from_origin(&"https://a.example".into()),
        };
        let bytes = holder.to_bytes();
        assert_eq!(bytes.len(), 58);
        assert_eq!(StorablePwaSubscriptionHolder::from_bytes(bytes), holder);
    }
}
