use crate::storage::account::AccountReference;
use crate::storage::storable::account_reference::StorableAccountReference;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::fmt;

/// Vectors are not supported yet in ic-stable-structures, this file
/// implements a struct to wrap this vector so it can be stored.
#[derive(Encode, Decode, Clone, Debug, Ord, Eq, PartialEq, PartialOrd)]
#[cbor(transparent)]
pub struct StorableAccountReferenceList(#[n(0)] Vec<StorableAccountReference>);

impl Storable for StorableAccountReferenceList {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAccountReferenceList");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAccountReferenceList")
    }

    const BOUND: Bound = Bound::Unbounded;
}

/// Why a list of account references cannot be stored.
///
/// Only ever raised on the way in. Decoding stays infallible, so a rule added here
/// applies to every future write of an existing row as well as to new ones — a stored
/// row that broke one would become unwritable, and for a reference list that means an
/// identity locked out of the origin. So the rules here are limited to states nothing
/// has ever written.
#[derive(Debug, Eq, PartialEq)]
pub enum StorableAccountReferenceListError {
    /// An empty list is a tombstone: it says every reference at this origin was moved
    /// away and its default account must never be derived again. Only a move may
    /// create one, and nothing moves accounts yet, so an empty list here is a bug
    /// rather than an intent — storing it would deny an identity its default account
    /// for good.
    Empty,
}

impl fmt::Display for StorableAccountReferenceListError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Empty => write!(
                f,
                "refusing to store an empty account reference list, which would be a tombstone"
            ),
        }
    }
}

impl StorableAccountReferenceList {
    pub fn into_vec(self) -> Vec<StorableAccountReference> {
        self.0
    }

    /// The row a future account move will leave behind, for tests that need one to
    /// exist. Test-only because [`Self::try_from`] refuses it, which is the point.
    #[cfg(test)]
    pub fn tombstone_for_testing() -> Self {
        Self(vec![])
    }
}

impl From<StorableAccountReferenceList> for Vec<AccountReference> {
    fn from(value: StorableAccountReferenceList) -> Self {
        value
            .0
            .iter()
            .cloned()
            .map(AccountReference::from)
            .collect()
    }
}

/// The only way to build a list to be stored, so every write is checked. Deliberately
/// `TryFrom` rather than `From`: an infallible conversion existed here before and the
/// checks it lacked had to be remembered at each of the write sites instead.
impl TryFrom<Vec<AccountReference>> for StorableAccountReferenceList {
    type Error = StorableAccountReferenceListError;

    fn try_from(value: Vec<AccountReference>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(StorableAccountReferenceListError::Empty);
        }

        Ok(StorableAccountReferenceList(
            value
                .iter()
                .cloned()
                .map(StorableAccountReference::from)
                .collect(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn reference(account_number: Option<u64>) -> AccountReference {
        AccountReference::new(account_number, None)
    }

    #[test]
    fn refuses_an_empty_list() {
        assert_eq!(
            StorableAccountReferenceList::try_from(vec![]).err(),
            Some(StorableAccountReferenceListError::Empty)
        );
    }

    #[test]
    fn keeps_the_references_in_the_order_they_were_given() {
        let references = vec![reference(None), reference(Some(7)), reference(Some(3))];

        let stored = StorableAccountReferenceList::try_from(references.clone()).unwrap();

        assert_eq!(Vec::<AccountReference>::from(stored), references);
    }

    #[test]
    fn a_list_without_a_tracked_default_is_storable() {
        // Not a tombstone: the default was named, so the row legitimately holds only
        // numbered references.
        assert!(StorableAccountReferenceList::try_from(vec![reference(Some(7))]).is_ok());
    }
}
