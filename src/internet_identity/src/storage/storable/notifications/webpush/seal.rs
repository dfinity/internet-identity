use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// The RFC 8291 sealed Web Push payload for one (device, origin), computed once
/// at consent and reused for every send. `blob` is the full aes128gcm wire
/// format the relay POSTs verbatim.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorableWebPushSeal {
    #[cbor(n(0), with = "minicbor::bytes")]
    pub blob: Vec<u8>,
    #[n(1)]
    pub created_at_ns: Timestamp,
}

impl Storable for StorableWebPushSeal {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableWebPushSeal");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableWebPushSeal")
    }

    // The blob is `notification_payload` (`{"o":"<origin>"}`, and a JSON-escaped
    // origin can reach 6 bytes per input byte) sealed by `rfc8291::encrypt`,
    // which adds an 86-byte header, the padding delimiter and the GCM tag.
    const BOUND: Bound = Bound::Bounded {
        max_size: 1700,
        is_fixed_size: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_through_storable() {
        let seal = StorableWebPushSeal {
            blob: vec![7u8; 200],
            created_at_ns: 1_234_567_890,
        };
        assert_eq!(StorableWebPushSeal::from_bytes(seal.to_bytes()), seal);
    }

    #[test]
    fn the_largest_possible_seal_fits_the_bound() {
        let Bound::Bounded { max_size, .. } = StorableWebPushSeal::BOUND else {
            panic!("StorableWebPushSeal must stay bounded");
        };
        // A 255-byte origin whose every non-scheme byte JSON-escapes to 6 bytes,
        // wrapped in `{"o":"…"}` and sealed: an 86-byte header, the padding
        // delimiter and the GCM tag.
        let escaped_origin = "https://".len() + (255 - "https://".len()) * 6;
        let plaintext = r#"{"o":""#.len() + escaped_origin + r#""}"#.len();
        // High-entropy bytes (a real seal is aes128gcm ciphertext): as a CBOR
        // int-array these would ~double and blow the bound; as a byte string
        // they don't. Guards against dropping the `minicbor::bytes` annotation.
        let seal = StorableWebPushSeal {
            blob: vec![0xABu8; 86 + plaintext + 1 + 16],
            created_at_ns: u64::MAX,
        };
        assert!(seal.to_bytes().len() <= max_size as usize);
    }
}
