//! Notification ids, which Internet Identity scopes to `(origin, recipient)`.
//!
//! A keyed id is derived, not remembered: the same `(recipient, key)` always
//! yields the same id, so an update supersedes its predecessor even after an
//! upgrade or a sweep.

use candid::Principal;
use sha2::{Digest, Sha256};

/// Separates derived ids from counter ids, so the two spaces cannot collide.
const KEYED: u64 = 1 << 63;

pub fn keyed(recipient: Principal, key: &str) -> u64 {
    let mut hasher = Sha256::new();
    let who = recipient.as_slice();
    // Length-prefixed, so no pair of (recipient, key) can be spelled two ways
    // and land on one id.
    hasher.update([who.len() as u8]);
    hasher.update(who);
    hasher.update(key.as_bytes());

    let digest = hasher.finalize();
    let leading = u64::from_be_bytes(digest[..8].try_into().expect("sha256 is 32 bytes"));
    KEYED | (leading & (KEYED - 1))
}

/// Counter ids start from a random offset, so two canisters sending for one
/// origin do not both hand out the same low numbers.
pub fn keyless(seed: u64, counter: u64) -> u64 {
    seed.wrapping_add(counter) & (KEYED - 1)
}

/// The first eight bytes of anything random, as an offset.
pub fn seed_from(randomness: &[u8]) -> u64 {
    let mut bytes = [0u8; 8];
    let take = randomness.len().min(8);
    bytes[..take].copy_from_slice(&randomness[..take]);
    u64::from_be_bytes(bytes) & (KEYED - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn alice() -> Principal {
        Principal::from_text("un4fu-tqaaa-aaaab-qadjq-cai").unwrap()
    }

    fn bob() -> Principal {
        Principal::from_text("ryjl3-tyaaa-aaaaa-aaaba-cai").unwrap()
    }

    #[test]
    fn the_same_key_is_the_same_notification() {
        assert_eq!(keyed(alice(), "chat-7"), keyed(alice(), "chat-7"));
    }

    #[test]
    fn keys_and_recipients_are_distinct() {
        assert_ne!(keyed(alice(), "chat-7"), keyed(alice(), "chat-8"));
        assert_ne!(keyed(alice(), "chat-7"), keyed(bob(), "chat-7"));
    }

    #[test]
    fn a_length_prefix_stops_two_spellings_meeting() {
        assert_ne!(keyed(alice(), "b-chat"), keyed(alice(), "bchat"));
    }

    #[test]
    fn derived_and_counter_ids_never_collide() {
        assert_eq!(keyed(alice(), "chat-7") & KEYED, KEYED);
        assert_eq!(keyless(0, 0) & KEYED, 0);
        assert_eq!(keyless(0, u64::MAX) & KEYED, 0);
        assert_eq!(keyless(u64::MAX, u64::MAX) & KEYED, 0);
    }

    #[test]
    fn a_seed_offsets_the_counter_space() {
        let seed = seed_from(&[0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x99, 0x88]);
        assert_ne!(keyless(seed, 1), keyless(0, 1));
        assert_eq!(keyless(seed, 1) & KEYED, 0);
    }

    #[test]
    fn short_randomness_still_seeds() {
        assert_ne!(seed_from(&[1, 2, 3]), 0);
    }
}
