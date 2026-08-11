use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::profile_picture::{
    ProfilePicture, ProfilePictureMediaType, ProfilePictureMetadata,
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
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableProfilePicture {
    /// Discriminant of [`ProfilePictureMediaType`], sniffed from the bytes
    /// when the picture was accepted. Encoded as a small integer rather than
    /// the string form so a rename of the IANA type never rewrites stored
    /// records; [`StorableProfilePicture::media_type`] maps it back.
    #[n(0)]
    pub media_type: u8,
    /// The raw picture, at most `PROFILE_PICTURE_MAX_BYTES`.
    #[cbor(n(1), with = "minicbor::bytes")]
    pub bytes: Vec<u8>,
    /// Nanoseconds since the Unix epoch.
    #[n(2)]
    pub uploaded_at: Timestamp,
}

/// Stored discriminants for [`ProfilePictureMediaType`]. Append-only: an
/// existing value must never be reused for a different media type, or stored
/// pictures would start being labelled with the wrong one.
const MEDIA_TYPE_PNG: u8 = 0;
const MEDIA_TYPE_JPEG: u8 = 1;
const MEDIA_TYPE_WEBP: u8 = 2;

impl StorableProfilePicture {
    /// The stored discriminant as a media type.
    ///
    /// `None` for a discriminant this wasm doesn't know — which can only
    /// happen on a rollback to a version that predates a newly added format.
    /// The caller treats it as "no picture" rather than trapping, so a
    /// rollback degrades to a missing avatar instead of a broken identity.
    pub fn media_type(&self) -> Option<ProfilePictureMediaType> {
        match self.media_type {
            MEDIA_TYPE_PNG => Some(ProfilePictureMediaType::Png),
            MEDIA_TYPE_JPEG => Some(ProfilePictureMediaType::Jpeg),
            MEDIA_TYPE_WEBP => Some(ProfilePictureMediaType::Webp),
            _ => None,
        }
    }

    /// The summary shape, read straight off the stored record.
    ///
    /// Exists so `identity_info` — which runs on every manage-screen load —
    /// can report the picture's media type, size and age without cloning up to
    /// 100 KiB of image bytes it would immediately discard.
    pub fn metadata(&self) -> Option<ProfilePictureMetadata> {
        Some(ProfilePictureMetadata {
            media_type: self.media_type()?,
            size_bytes: self.bytes.len() as u64,
            uploaded_at: self.uploaded_at,
        })
    }

    /// The API shape, or `None` when the stored media type is unknown to this
    /// wasm (see [`StorableProfilePicture::media_type`]).
    pub fn to_profile_picture(&self) -> Option<ProfilePicture> {
        Some(ProfilePicture {
            media_type: self.media_type()?,
            bytes: ByteBuf::from(self.bytes.clone()),
            uploaded_at: self.uploaded_at,
        })
    }
}

impl From<ProfilePicture> for StorableProfilePicture {
    fn from(value: ProfilePicture) -> Self {
        Self {
            media_type: match value.media_type {
                ProfilePictureMediaType::Png => MEDIA_TYPE_PNG,
                ProfilePictureMediaType::Jpeg => MEDIA_TYPE_JPEG,
                ProfilePictureMediaType::Webp => MEDIA_TYPE_WEBP,
            },
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

    fn sample(media_type: ProfilePictureMediaType) -> ProfilePicture {
        ProfilePicture {
            media_type,
            bytes: ByteBuf::from(vec![7u8; 1024]),
            uploaded_at: 1_700_000_000_000_000_000,
        }
    }

    #[test]
    fn should_roundtrip_every_media_type_through_storable() {
        for media_type in [
            ProfilePictureMediaType::Png,
            ProfilePictureMediaType::Jpeg,
            ProfilePictureMediaType::Webp,
        ] {
            let picture = sample(media_type);
            let storable = StorableProfilePicture::from(picture.clone());
            let decoded = StorableProfilePicture::from_bytes(storable.to_bytes());
            assert_eq!(decoded, storable);
            assert_eq!(
                decoded.to_profile_picture(),
                Some(picture),
                "round-trip lost data for {:?}",
                media_type
            );
        }
    }

    #[test]
    fn should_roundtrip_a_max_size_picture() {
        let picture = ProfilePicture {
            media_type: ProfilePictureMediaType::Png,
            bytes: ByteBuf::from(vec![0xabu8; PROFILE_PICTURE_MAX_BYTES]),
            uploaded_at: 1,
        };
        let storable = StorableProfilePicture::from(picture.clone());
        let decoded = StorableProfilePicture::from_bytes(storable.to_bytes());
        assert_eq!(decoded.to_profile_picture(), Some(picture));
    }

    /// `metadata()` skips the byte clone, so it must still report exactly what
    /// the full round trip would — otherwise `identity_info` and
    /// `profile_picture_get` could disagree about the same stored picture.
    #[test]
    fn metadata_agrees_with_the_full_round_trip() {
        for media_type in [
            ProfilePictureMediaType::Png,
            ProfilePictureMediaType::Jpeg,
            ProfilePictureMediaType::Webp,
        ] {
            let storable = StorableProfilePicture::from(sample(media_type));
            assert_eq!(
                storable.metadata(),
                storable.to_profile_picture().map(|p| p.metadata()),
                "metadata diverged from the round trip for {:?}",
                media_type
            );
        }
    }

    /// A media-type discriminant written by a newer wasm must not trap this
    /// one — it reads as "no picture" instead.
    #[test]
    fn should_treat_an_unknown_media_type_as_absent() {
        let forward = StorableProfilePicture {
            media_type: 200,
            bytes: vec![1, 2, 3],
            uploaded_at: 5,
        };
        let decoded = StorableProfilePicture::from_bytes(forward.to_bytes());
        assert_eq!(decoded.media_type(), None);
        assert_eq!(decoded.to_profile_picture(), None);
        assert_eq!(decoded.metadata(), None);
    }
}
