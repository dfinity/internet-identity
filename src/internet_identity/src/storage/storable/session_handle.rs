use crate::storage::storable::session_id::StorableSessionId;
use candid::Principal;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Where the session a caller authenticates as is stored.
///
/// The account is named by its principal rather than by its locator because materialising a
/// default account changes the locator and leaves the principal alone, so a rename touches
/// one entry in the principal index instead of every session of that account.
///
/// The session itself is named by its id, which is an input to the session seed, so an
/// entry can only ever resolve to the one session whose principal is its own key.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
// An array rather than a map, for the reason given on `StorableAccountKey`: a map's keys buy
// decoding records written before a field existed, which a derived index does not need.
// Nothing sweeps this one today, but the sessions themselves are held on the account
// references, so it can be rebuilt from them if a shape change ever calls for it.
#[cbor(array)]
pub struct StorableSessionHandle {
    #[cbor(n(0), with = "minicbor::bytes")]
    pub account_principal: Vec<u8>,
    #[n(1)]
    pub session_id: StorableSessionId,
}

impl StorableSessionHandle {
    pub fn account(&self) -> Principal {
        Principal::from_slice(&self.account_principal)
    }
}

impl Storable for StorableSessionHandle {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSessionHandle");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSessionHandle")
    }

    /// The array header, then the principal as a byte string — twenty-nine bytes and a
    /// two-byte header, a self-authenticating principal being a hash and a tag — then a
    /// maximal `u64` at nine. Declared so the map sizes its pages to what it stores rather
    /// than to a default, which is only possible before it holds anything.
    ///
    /// `is_fixed_size` is false: the session id encodes shorter when it is small, and the
    /// flag is read for keys alone in any case.
    const BOUND: Bound = Bound::Bounded {
        max_size: 41,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use candid::Principal;
    use ic_stable_structures::storable::Bound;
    use pretty_assertions::assert_eq;

    /// A declared bound is a promise the derive does not keep: exceed it and the write
    /// panics rather than the type failing to compile. So the maximum is asserted here,
    /// where adding a field breaks a test instead of a canister.
    #[test]
    fn the_largest_handle_fits_the_declared_bound() {
        let largest = StorableSessionHandle {
            account_principal: vec![0xff; Principal::MAX_LENGTH_IN_BYTES],
            session_id: u64::MAX,
        };

        let Bound::Bounded { max_size, .. } = StorableSessionHandle::BOUND else {
            panic!("the bound is what this test is about");
        };
        assert_eq!(largest.to_bytes().len() as u32, max_size);
    }

    /// What the index actually holds: `canister_sig_principal` is self-authenticating, so
    /// the principal is always the full twenty-nine bytes and only the id varies.
    #[test]
    fn a_real_handle_is_shorter_than_the_bound() {
        let handle = StorableSessionHandle {
            account_principal: vec![0x01; Principal::MAX_LENGTH_IN_BYTES],
            session_id: 1,
        };

        assert!(handle.to_bytes().len() < 41);
    }
}
