use ic_stable_structures::{storable::Bound, Storable};
use internet_identity_interface::internet_identity::types::FrontendHostname;
use minicbor::{Decode, Encode};
use std::{
    borrow::Cow,
    fmt::{self, Display},
};

use crate::utils::{sha256sum, slice_to_bounded_32};

#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableApplication {
    #[n(0)]
    pub origin: FrontendHostname,
    #[n(1)]
    pub stored_accounts: u64,
    #[n(2)]
    pub stored_account_references: u64,
    /// Rows that exist here while holding no reference at all.
    ///
    /// A row holding nothing is a tombstone: it says every account an identity had at
    /// this origin was moved away and its default must never be derived again. It
    /// contributes nothing to `stored_account_references`, so without counting it
    /// separately this application would look unreferenced and be retired — and the next
    /// visit would mint a fresh application number the tombstone no longer applies to,
    /// handing the identity back the default it had moved away from.
    ///
    /// Absent from every application stored before this field existed, and `0` is the
    /// truth for those: nothing could write an empty reference list when they were
    /// written. `default` is what makes that absence decode rather than trap.
    #[n(3)]
    #[cbor(default)]
    pub tombstones: u64,
}

impl Storable for StorableApplication {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableApplication");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableApplication")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableOriginSha256 {
    hash: [u8; 32],
}

impl Display for StorableOriginSha256 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex::encode(self.hash))
    }
}

impl StorableOriginSha256 {
    pub fn from_origin(origin: &FrontendHostname) -> Self {
        let hash = sha256sum(origin.as_bytes());
        Self { hash }
    }
}

impl Storable for StorableOriginSha256 {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(self.hash.to_vec())
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        let hash = slice_to_bounded_32(bytes.as_ref());
        Self { hash }
    }

    const BOUND: Bound = Bound::Bounded {
        max_size: 32,
        is_fixed_size: true,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    /// The shape every application stored before the tombstone count existed has on
    /// disk. Decoding one has to keep working, and has to read as no tombstones: nothing
    /// could write an empty reference list when these were written.
    #[test]
    fn an_application_stored_without_a_tombstone_count_decodes_as_zero() {
        #[derive(Encode)]
        #[cbor(map)]
        struct BeforeTombstones {
            #[n(0)]
            origin: FrontendHostname,
            #[n(1)]
            stored_accounts: u64,
            #[n(2)]
            stored_account_references: u64,
        }

        let mut bytes = Vec::new();
        minicbor::encode(
            &BeforeTombstones {
                origin: "https://example.com".to_string(),
                stored_accounts: 3,
                stored_account_references: 4,
            },
            &mut bytes,
        )
        .unwrap();

        let decoded = StorableApplication::from_bytes(Cow::Owned(bytes));

        assert_eq!(
            decoded,
            StorableApplication {
                origin: "https://example.com".to_string(),
                stored_accounts: 3,
                stored_account_references: 4,
                tombstones: 0,
            }
        );
    }

    #[test]
    fn a_tombstone_count_survives_the_round_trip() {
        let application = StorableApplication {
            origin: "https://example.com".to_string(),
            stored_accounts: 1,
            stored_account_references: 2,
            tombstones: 5,
        };

        assert_eq!(
            StorableApplication::from_bytes(application.to_bytes()),
            application
        );
    }
}
