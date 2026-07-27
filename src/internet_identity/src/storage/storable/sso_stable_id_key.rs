use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use sha2::{Digest, Sha256};
use std::borrow::Cow;

/// Fixed-length lookup key for the SSO stable-id index:
/// `SHA-256(sso_domain, iss, ii_client_id, stable_id) -> AnchorNumber`.
///
/// The index is only ever probed by exact match, so the key needs to be
/// *distinguishing*, not *readable*. Storing the 32-byte digest instead of the
/// four caller-influenced strings bounds every entry to a fixed size — no
/// unbounded key growth from a long domain, issuer, or claim value — and folds
/// `sso_domain` into the key so a login discovered through one domain can never
/// resolve to an entry established through another (the domain-scoping that
/// makes cross-domain injection into a foreign `(iss, ii_client_id)`
/// namespace impossible).
///
/// Each field is length-prefixed before hashing so distinct field boundaries
/// can't alias — e.g. `("ab", "c", …)` and `("a", "bc", …)` hash differently.
/// The value is maintained by [`crate::storage::Storage::write`], reconciled
/// from the anchors' stored credentials.
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableSsoStableIdKey([u8; 32]);

impl StorableSsoStableIdKey {
    pub fn new(sso_domain: &str, iss: &str, ii_client_id: &str, stable_id: &str) -> Self {
        let mut hasher = Sha256::new();
        for field in [sso_domain, iss, ii_client_id, stable_id] {
            hasher.update((field.len() as u64).to_be_bytes());
            hasher.update(field.as_bytes());
        }
        Self(hasher.finalize().into())
    }
}

impl Storable for StorableSsoStableIdKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Borrowed(&self.0)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        Self(
            <[u8; 32]>::try_from(bytes.as_ref())
                .expect("StorableSsoStableIdKey must be exactly 32 bytes"),
        )
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
    fn should_encode_and_decode_roundtrip() {
        let key = StorableSsoStableIdKey::new(
            "acme.example",
            "https://login.microsoftonline.com/tenant/v2.0",
            "ii-client",
            "entra-oid-0001",
        );
        let bytes = key.to_bytes();
        assert_eq!(bytes.len(), 32);
        let decoded = StorableSsoStableIdKey::from_bytes(bytes);
        assert_eq!(key, decoded);
    }

    #[test]
    fn different_ii_client_ids_produce_distinct_keys() {
        let a = StorableSsoStableIdKey::new("acme.example", "iss", "client-a", "oid-1");
        let b = StorableSsoStableIdKey::new("acme.example", "iss", "client-b", "oid-1");
        assert_ne!(a, b);
    }

    #[test]
    fn different_domains_produce_distinct_keys() {
        // The whole point of the index fix: the same (iss, client, stable_id)
        // discovered through a different domain must not collide.
        let legit = StorableSsoStableIdKey::new("acme.example", "iss", "client", "oid-1");
        let attacker = StorableSsoStableIdKey::new("attacker.example", "iss", "client", "oid-1");
        assert_ne!(legit, attacker);
    }

    #[test]
    fn field_boundaries_do_not_alias() {
        // Length-prefixing means concatenation-ambiguous inputs stay distinct.
        let a = StorableSsoStableIdKey::new("ab", "c", "client", "oid");
        let b = StorableSsoStableIdKey::new("a", "bc", "client", "oid");
        assert_ne!(a, b);
    }
}
