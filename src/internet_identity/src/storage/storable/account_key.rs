use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::anchor_number::StorableAnchorNumber;
use crate::storage::storable::application_number::StorableApplicationNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The stored form of an [`crate::storage::account::AccountKey`], with the origin
/// interned to an application number.
///
/// The number rather than the origin, because a list per principal would otherwise
/// carry a copy of the origin string, and interning it is what application numbers are
/// for. Which is also why the two types stay apart: the number is storage's own, and
/// what leaves is the `AccountKey` it maps to. Absent account number means the tracked
/// default.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
// An array rather than a map: a map's integer keys buy the ability to add a field and still
// decode records written before it, which is what authoritative state needs. This is a
// derived index — every entry is reconstructible from the account reference lists that hold
// the truth, and the backfill sweep does exactly that — so a shape change here is a rebuild,
// and the three key bytes are not worth paying for.
#[cbor(array)]
pub struct StorableAccountKey {
    #[n(0)]
    pub anchor_number: StorableAnchorNumber,
    #[n(1)]
    pub application_number: StorableApplicationNumber,
    #[n(2)]
    pub account_number: Option<StorableAccountNumber>,
}

impl Storable for StorableAccountKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountKey")
    }

    /// The array header, then three maximal `u64`s at nine bytes each. Declared rather
    /// than left unbounded because a bounded key and value are together what let the map
    /// size its pages to what it stores, instead of falling back to a default four
    /// hundred bytes larger — and because the bound can only be declared before the map
    /// holds anything, since it is what the page size was computed from.
    ///
    /// `is_fixed_size` is false: a small number encodes shorter. It would not help anyway,
    /// being read for keys alone, and this is a value.
    const BOUND: Bound = Bound::Bounded {
        max_size: 28,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use ic_stable_structures::storable::Bound;
    use pretty_assertions::assert_eq;

    /// A declared bound is a promise the derive does not keep: exceed it and the write
    /// panics rather than the type failing to compile. So the maximum is asserted here,
    /// where adding a field breaks a test instead of a canister.
    #[test]
    fn the_largest_key_fits_the_declared_bound() {
        let largest = StorableAccountKey {
            anchor_number: u64::MAX,
            application_number: u64::MAX,
            account_number: Some(u64::MAX),
        };

        let Bound::Bounded { max_size, .. } = StorableAccountKey::BOUND else {
            panic!("the bound is what this test is about");
        };
        assert_eq!(largest.to_bytes().len() as u32, max_size);
    }

    /// The page size a map derives from the bound is only a saving while the bound is
    /// tight, so an absent account number must not be paid for like a present one.
    #[test]
    fn a_tracked_default_encodes_shorter_than_the_bound() {
        let tracked_default = StorableAccountKey {
            anchor_number: 10_000,
            application_number: 1,
            account_number: None,
        };

        assert!(tracked_default.to_bytes().len() < 28);
    }
}
