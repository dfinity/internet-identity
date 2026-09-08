use crate::storage::account::AccountReference;
use crate::storage::storable::account_reference::StorableAccountReference;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::AccountNumber;
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::BTreeSet;
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
/// applies to every future write of an existing list as well as to new ones — a stored
/// list that broke one would become unwritable, and for an account reference list that
/// means an
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
    /// One account cannot be held twice at one origin. Both references would derive the
    /// same principal, so the index would hold one entry for two slots while the stored
    /// account count claimed both — and nothing downstream could tell which reference a
    /// read had answered from.
    RepeatedAccountNumber(AccountNumber),
    /// An identity has one tracked default at an origin. A second numberless reference
    /// derives the same principal as the first and is counted as another default, which
    /// inflates the identity's evictable-default count against its cap and leaves a list
    /// no eviction can ever pick, because eviction only picks a list whose *single*
    /// reference is a tracked default.
    RepeatedTrackedDefault,
}

impl fmt::Display for StorableAccountReferenceListError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Empty => write!(
                f,
                "refusing to store an empty account reference list, which would be a tombstone"
            ),
            Self::RepeatedAccountNumber(account_number) => write!(
                f,
                "refusing to store an account reference list holding account {account_number} twice"
            ),
            Self::RepeatedTrackedDefault => write!(
                f,
                "refusing to store an account reference list holding more than one tracked default"
            ),
        }
    }
}

impl StorableAccountReferenceList {
    pub fn into_vec(self) -> Vec<StorableAccountReference> {
        self.0
    }

    /// The list a future account move will leave behind, for tests that need one to
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

        // A reference is identified by the account it names, and the numberless one names
        // the tracked default — so one pass over `Option<AccountNumber>` catches a repeated
        // number and a second tracked default alike.
        let mut seen = BTreeSet::new();
        for reference in &value {
            if !seen.insert(reference.account_number) {
                return Err(match reference.account_number {
                    Some(account_number) => {
                        StorableAccountReferenceListError::RepeatedAccountNumber(account_number)
                    }
                    None => StorableAccountReferenceListError::RepeatedTrackedDefault,
                });
            }
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
        AccountReference {
            account_number,
            last_used: None,
        }
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
    fn refuses_the_same_account_twice() {
        assert_eq!(
            StorableAccountReferenceList::try_from(vec![
                reference(None),
                reference(Some(7)),
                reference(Some(7)),
            ])
            .err(),
            Some(StorableAccountReferenceListError::RepeatedAccountNumber(7))
        );
    }

    #[test]
    fn refuses_a_second_tracked_default() {
        assert_eq!(
            StorableAccountReferenceList::try_from(vec![reference(None), reference(None)]).err(),
            Some(StorableAccountReferenceListError::RepeatedTrackedDefault)
        );
    }

    #[test]
    fn a_list_without_a_tracked_default_is_storable() {
        // Not a tombstone: the default was named, so the list legitimately holds only
        // numbered references.
        assert!(StorableAccountReferenceList::try_from(vec![reference(Some(7))]).is_ok());
    }
}
