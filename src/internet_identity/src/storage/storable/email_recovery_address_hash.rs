//! Stable-map key for the `address → AnchorNumber` reverse index used
//! by the email-recovery flow.
//!
//! See `docs/ongoing/email-recovery.md` §8.2: we hash the address
//! before storing it as a key, both to keep entry size fixed (32
//! bytes) regardless of the underlying address length, and because
//! the address itself already lives on the anchor — there is no
//! reason to store it again in the index. Lookup recomputes the hash
//! from the verified `From:` of the inbound email.

use crate::utils::slice_to_bounded_32;
use ic_stable_structures::{storable::Bound, Storable};
use std::borrow::Cow;

/// SHA-256 of the lowercased canonical address.
///
/// Always produced via [`StorableEmailRecoveryAddressHash::of`] —
/// don't construct directly with raw bytes from outside the module so
/// the "address must be lowercased before hashing" invariant stays
/// inside one place.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct StorableEmailRecoveryAddressHash([u8; 32]);

impl StorableEmailRecoveryAddressHash {
    /// Hash the lowercased canonical form of `address`. Callers that
    /// have already normalised the address can rely on the
    /// `to_ascii_lowercase` here as a no-op for ASCII (which is the
    /// only form we accept — IDN inputs are rejected at the Candid
    /// boundary; see `email_recovery::prepare`).
    pub fn of(address: &str) -> Self {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(address.to_ascii_lowercase().as_bytes());
        Self(hasher.finalize().into())
    }
}

impl Storable for StorableEmailRecoveryAddressHash {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Borrowed(&self.0)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        // `BOUND` says 32 bytes fixed-size, so this should always be
        // exactly 32 bytes. Use the same defensive helper as
        // `StorableApplication`'s hash key — copy up to 32, zero-pad
        // any short input — so corrupted stable memory never makes
        // the canister trap mid-call.
        Self(slice_to_bounded_32(bytes.as_ref()))
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
    fn lowercases_before_hashing() {
        let lower = StorableEmailRecoveryAddressHash::of("alice@example.com");
        let mixed = StorableEmailRecoveryAddressHash::of("Alice@Example.COM");
        assert_eq!(lower, mixed);
    }

    #[test]
    fn distinct_addresses_distinct_hashes() {
        let a = StorableEmailRecoveryAddressHash::of("alice@example.com");
        let b = StorableEmailRecoveryAddressHash::of("bob@example.com");
        assert_ne!(a, b);
    }

    #[test]
    fn round_trip_storable() {
        let original = StorableEmailRecoveryAddressHash::of("alice@example.com");
        let bytes = original.to_bytes();
        let parsed = StorableEmailRecoveryAddressHash::from_bytes(bytes);
        assert_eq!(original, parsed);
    }
}
