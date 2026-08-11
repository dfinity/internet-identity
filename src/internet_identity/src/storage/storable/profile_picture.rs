use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::profile_picture::{
    ProfilePicture, ProfilePictureMetadata,
};
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use serde_bytes::ByteBuf;
use std::borrow::Cow;

/// An identity's profile picture, persisted in its own stable map keyed by
/// anchor number (see [`crate::storage`]) rather than inside the anchor
/// record.
///
/// The anchor record is deserialized on every authenticated call and is sized
/// against a 4 KB budget (`DEFAULT_ENTRY_SIZE`); a picture is up to 100 KB.
/// Keeping it out of anchor serialization means the cost of having a picture
/// is paid only by the calls that actually read one — the same reasoning as
/// [`crate::storage::storable::mcp_config::StorableMcpConfig`].
///
/// No media type is stored: every picture is WebP. Should a second format ever
/// be supported, it takes a fresh CBOR key as an `Option` — `#[cbor(map)]`
/// decodes an absent key as `None`, so existing records keep loading.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableProfilePicture {
    /// The raw WebP image, at most `PROFILE_PICTURE_MAX_BYTES`.
    #[cbor(n(0), with = "minicbor::bytes")]
    pub bytes: Vec<u8>,
    /// Nanoseconds since the Unix epoch.
    #[n(1)]
    pub uploaded_at: Timestamp,
}

impl StorableProfilePicture {
    /// The API shape.
    pub fn to_profile_picture(&self) -> ProfilePicture {
        ProfilePicture {
            bytes: ByteBuf::from(self.bytes.clone()),
            uploaded_at: self.uploaded_at,
        }
    }

    /// The summary shape, read straight off the stored record.
    ///
    /// Exists so `identity_info` — which runs on every manage-screen load —
    /// can report the picture's size and age without cloning up to 100 KiB of
    /// image bytes it would immediately discard.
    pub fn metadata(&self) -> ProfilePictureMetadata {
        ProfilePictureMetadata {
            size_bytes: self.bytes.len() as u64,
            uploaded_at: self.uploaded_at,
        }
    }
}

impl From<ProfilePicture> for StorableProfilePicture {
    fn from(value: ProfilePicture) -> Self {
        Self {
            bytes: value.bytes.into_vec(),
            uploaded_at: value.uploaded_at,
        }
    }
}

impl Storable for StorableProfilePicture {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableProfilePicture");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableProfilePicture")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::profile_picture::PROFILE_PICTURE_MAX_BYTES;

    fn sample(len: usize) -> ProfilePicture {
        ProfilePicture {
            bytes: ByteBuf::from(vec![7u8; len]),
            uploaded_at: 1_700_000_000_000_000_000,
        }
    }

    #[test]
    fn should_roundtrip_through_storable() {
        let picture = sample(1024);
        let storable = StorableProfilePicture::from(picture.clone());
        let decoded = StorableProfilePicture::from_bytes(storable.to_bytes());
        assert_eq!(decoded, storable);
        assert_eq!(decoded.to_profile_picture(), picture);
    }

    #[test]
    fn should_roundtrip_a_max_size_picture() {
        let picture = sample(PROFILE_PICTURE_MAX_BYTES);
        let storable = StorableProfilePicture::from(picture.clone());
        let decoded = StorableProfilePicture::from_bytes(storable.to_bytes());
        assert_eq!(decoded.to_profile_picture(), picture);
    }

    /// `metadata()` skips the byte clone, so it must still report exactly what
    /// the full round trip would — otherwise `identity_info` and
    /// `profile_picture_get` could disagree about the same stored picture.
    #[test]
    fn metadata_agrees_with_the_full_round_trip() {
        let storable = StorableProfilePicture::from(sample(1024));
        assert_eq!(
            storable.metadata(),
            storable.to_profile_picture().metadata()
        );
    }

    /// A key added by a future wasm (e.g. a media type, once a second format
    /// exists) must not trap this one — `#[cbor(map)]` skips unknown keys.
    #[test]
    fn should_ignore_an_unknown_future_key() {
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(3).unwrap();
        encoder.u8(0).unwrap().bytes(&[1, 2, 3]).unwrap();
        encoder.u8(1).unwrap().u64(42).unwrap();
        // A key this wasm has never heard of.
        encoder.u8(7).unwrap().u8(9).unwrap();

        let decoded = StorableProfilePicture::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.bytes, vec![1, 2, 3]);
        assert_eq!(decoded.uploaded_at, 42);
    }
}
