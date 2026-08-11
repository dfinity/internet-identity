//! Profile pictures — the one piece of shareable identity info that is
//! binary rather than textual.
//!
//! An identity carries zero or one picture. It is stored raw (the bytes the
//! user uploaded, plus the sniffed media type) and rendered into a `data:`
//! URL at certification time, which is what a relying party receives for the
//! `profile_picture` attribute key. See
//! [`crate::internet_identity::types::attributes`] for the attribute plumbing,
//! and the `profile_picture_*` section of `internet_identity.did` — which
//! `docs/ii-spec.mdx` embeds verbatim — for the caller-facing contract.

use crate::internet_identity::types::Timestamp;
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::{CandidType, Principal};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;

/// Upper bound on the raw picture bytes an identity may store.
///
/// The user-facing promise is "up to 100 KB", and we read KB as the binary
/// kibibyte the rest of this canister counts in.
pub const PROFILE_PICTURE_MAX_BYTES: usize = 100 * 1024;

/// Lower bound, purely a sanity check: no accepted media type has a valid
/// encoding shorter than its own magic number, so anything this small is a
/// truncated upload rather than a picture.
pub const PROFILE_PICTURE_MIN_BYTES: usize = 16;

/// The picture formats an identity may store.
///
/// Deliberately short: every entry is a format that every browser can both
/// produce (via `canvas.toBlob`) and render, and whose magic number we can
/// check without a decoder. Storing a format we can't sniff would mean
/// trusting a client-supplied media type, which then lands in a `data:` URL
/// that relying parties feed to an `<img>` tag — so the media type is
/// something we derive, never something we accept.
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, CandidType, Serialize, Deserialize,
)]
pub enum ProfilePictureMediaType {
    Png,
    Jpeg,
    Webp,
}

impl ProfilePictureMediaType {
    /// The IANA media type, as it appears in the `data:` URL handed to
    /// relying parties.
    pub const fn as_str(self) -> &'static str {
        match self {
            ProfilePictureMediaType::Png => "image/png",
            ProfilePictureMediaType::Jpeg => "image/jpeg",
            ProfilePictureMediaType::Webp => "image/webp",
        }
    }

    /// Identifies the format from `bytes`' magic number, or `None` when the
    /// bytes are not one of the accepted formats.
    ///
    /// This is a format check, not a validity check: it establishes which
    /// media type to label the bytes with, and rejects anything that isn't an
    /// image at all (an HTML page, an SVG with a script in it, a PDF). We
    /// deliberately do not decode the image — a canister has no business
    /// running an image parser over user input, and a relying party's
    /// `<img>` tag is what ultimately decides whether the pixels are
    /// renderable.
    pub fn sniff(bytes: &[u8]) -> Option<Self> {
        const PNG: &[u8] = &[0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a];
        const JPEG: &[u8] = &[0xff, 0xd8, 0xff];

        if bytes.starts_with(PNG) {
            return Some(ProfilePictureMediaType::Png);
        }
        if bytes.starts_with(JPEG) {
            return Some(ProfilePictureMediaType::Jpeg);
        }
        // RIFF container: `RIFF<u32 le size>WEBP`.
        if bytes.len() >= 12 && bytes.starts_with(b"RIFF") && &bytes[8..12] == b"WEBP" {
            return Some(ProfilePictureMediaType::Webp);
        }
        None
    }
}

/// An identity's stored picture, as it crosses the Candid boundary.
#[derive(Clone, Debug, PartialEq, Eq, CandidType, Serialize, Deserialize)]
pub struct ProfilePicture {
    /// Derived from the bytes by [`ProfilePictureMediaType::sniff`] — never
    /// taken from the caller.
    pub media_type: ProfilePictureMediaType,
    /// The raw picture, at most [`PROFILE_PICTURE_MAX_BYTES`].
    pub bytes: ByteBuf,
    /// Nanoseconds since the Unix epoch.
    pub uploaded_at: Timestamp,
}

impl ProfilePicture {
    /// The `data:` URL a relying party receives as the `profile_picture`
    /// attribute value.
    ///
    /// A self-describing URL rather than the raw bytes: it carries its own
    /// media type, so the certified bundle needs no second key for it, and it
    /// drops straight into an `<img src>` on the consuming side. The cost is
    /// base64's 4/3 inflation, which
    /// [`crate::internet_identity::types::attributes::PROFILE_PICTURE_ATTRIBUTE_VALUE_MAX_BYTES`]
    /// budgets for.
    pub fn to_data_url(&self) -> String {
        format!(
            "data:{};base64,{}",
            self.media_type.as_str(),
            BASE64.encode(self.bytes.as_ref())
        )
    }

    /// The cheap summary the manage UI lists without pulling the bytes down.
    pub fn metadata(&self) -> ProfilePictureMetadata {
        ProfilePictureMetadata {
            media_type: self.media_type,
            size_bytes: self.bytes.len() as u64,
            uploaded_at: self.uploaded_at,
        }
    }
}

/// What `identity_info` reports about the picture.
///
/// The bytes are deliberately absent: `identity_info` is fetched on every
/// manage-screen load, and a picture is up to three orders of magnitude
/// larger than everything else in that response put together. Callers that
/// want to render the picture ask for it with `profile_picture_get`.
#[derive(Clone, Debug, PartialEq, Eq, CandidType, Serialize, Deserialize)]
pub struct ProfilePictureMetadata {
    pub media_type: ProfilePictureMediaType,
    pub size_bytes: u64,
    pub uploaded_at: Timestamp,
}

/// What the caller sends to `profile_picture_set`.
///
/// Only the bytes: the media type is sniffed, and the timestamp is the
/// canister's.
#[derive(Clone, Debug, PartialEq, Eq, CandidType, Serialize, Deserialize)]
pub struct ProfilePictureSetArg {
    pub bytes: ByteBuf,
}

#[derive(Clone, Debug, PartialEq, Eq, CandidType, Serialize, Deserialize)]
pub enum ProfilePictureError {
    Unauthorized(Principal),
    /// The bytes are larger than [`PROFILE_PICTURE_MAX_BYTES`].
    TooLarge {
        size_bytes: u64,
        max_bytes: u64,
    },
    /// The bytes are too short to be any accepted format.
    TooSmall {
        size_bytes: u64,
        min_bytes: u64,
    },
    /// The bytes' magic number matches none of the accepted formats.
    UnsupportedMediaType,
    /// No picture is set on this identity (returned by `profile_picture_remove`).
    NotSet,
    InternalCanisterError(String),
}

/// Validates raw upload bytes and stamps them into a [`ProfilePicture`].
///
/// The single place the "what is an acceptable picture" rule lives, so the
/// endpoint, the storage layer and the tests can't drift apart on it.
pub fn validate_profile_picture(
    bytes: ByteBuf,
    now_ns: Timestamp,
) -> Result<ProfilePicture, ProfilePictureError> {
    if bytes.len() > PROFILE_PICTURE_MAX_BYTES {
        return Err(ProfilePictureError::TooLarge {
            size_bytes: bytes.len() as u64,
            max_bytes: PROFILE_PICTURE_MAX_BYTES as u64,
        });
    }
    if bytes.len() < PROFILE_PICTURE_MIN_BYTES {
        return Err(ProfilePictureError::TooSmall {
            size_bytes: bytes.len() as u64,
            min_bytes: PROFILE_PICTURE_MIN_BYTES as u64,
        });
    }
    let media_type = ProfilePictureMediaType::sniff(bytes.as_ref())
        .ok_or(ProfilePictureError::UnsupportedMediaType)?;

    Ok(ProfilePicture {
        media_type,
        bytes,
        uploaded_at: now_ns,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq as pretty_assert_eq;

    /// A byte string that starts like `media_type` and is `len` bytes long.
    fn sample(media_type: ProfilePictureMediaType, len: usize) -> ByteBuf {
        let mut bytes = match media_type {
            ProfilePictureMediaType::Png => {
                vec![0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a]
            }
            ProfilePictureMediaType::Jpeg => vec![0xff, 0xd8, 0xff, 0xe0],
            ProfilePictureMediaType::Webp => {
                let mut riff = b"RIFF".to_vec();
                riff.extend_from_slice(&[0, 0, 0, 0]);
                riff.extend_from_slice(b"WEBP");
                riff
            }
        };
        bytes.resize(len.max(bytes.len()), 0);
        ByteBuf::from(bytes)
    }

    #[test]
    fn sniffs_every_accepted_format() {
        for media_type in [
            ProfilePictureMediaType::Png,
            ProfilePictureMediaType::Jpeg,
            ProfilePictureMediaType::Webp,
        ] {
            let bytes = sample(media_type, 64);
            pretty_assert_eq!(
                ProfilePictureMediaType::sniff(bytes.as_ref()),
                Some(media_type),
                "failed to sniff {:?}",
                media_type
            );
        }
    }

    #[test]
    fn rejects_bytes_that_are_not_an_accepted_image() {
        let test_cases: Vec<(&str, &[u8])> = vec![
            ("empty", b""),
            ("html", b"<!DOCTYPE html><html><body>hi</body></html>"),
            // SVG is a script-execution vector in an `<img>` tag on some
            // consumers; it must not sniff as an image.
            ("svg", b"<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"),
            ("pdf", b"%PDF-1.7\n"),
            ("gif", b"GIF89a\0\0\0\0"),
            // A RIFF container that isn't WebP (e.g. a WAV file).
            ("riff but not webp", b"RIFF\0\0\0\0WAVEfmt "),
            ("truncated png magic", &[0x89, b'P', b'N']),
        ];

        for (label, bytes) in test_cases {
            pretty_assert_eq!(
                ProfilePictureMediaType::sniff(bytes),
                None,
                "should not have sniffed as an image: {}",
                label
            );
        }
    }

    #[test]
    fn validates_size_bounds() {
        let now = 1_700_000_000_000_000_000;

        let at_max = sample(ProfilePictureMediaType::Png, PROFILE_PICTURE_MAX_BYTES);
        assert!(
            validate_profile_picture(at_max, now).is_ok(),
            "a picture of exactly the maximum size must be accepted"
        );

        let over_max = sample(ProfilePictureMediaType::Png, PROFILE_PICTURE_MAX_BYTES + 1);
        pretty_assert_eq!(
            validate_profile_picture(over_max, now),
            Err(ProfilePictureError::TooLarge {
                size_bytes: PROFILE_PICTURE_MAX_BYTES as u64 + 1,
                max_bytes: PROFILE_PICTURE_MAX_BYTES as u64,
            })
        );

        // Short enough to be a truncated upload, but still carrying valid
        // PNG magic — so this exercises the size check, not the sniff.
        let under_min = sample(ProfilePictureMediaType::Png, 8);
        pretty_assert_eq!(
            validate_profile_picture(under_min, now),
            Err(ProfilePictureError::TooSmall {
                size_bytes: 8,
                min_bytes: PROFILE_PICTURE_MIN_BYTES as u64,
            })
        );
    }

    #[test]
    fn validate_stamps_media_type_and_timestamp() {
        let now = 1_700_000_000_000_000_000;
        let picture = validate_profile_picture(sample(ProfilePictureMediaType::Jpeg, 64), now)
            .expect("a well-formed JPEG must validate");
        pretty_assert_eq!(picture.media_type, ProfilePictureMediaType::Jpeg);
        pretty_assert_eq!(picture.uploaded_at, now);
        pretty_assert_eq!(picture.bytes.len(), 64);
    }

    #[test]
    fn data_url_is_self_describing_and_base64() {
        let picture = ProfilePicture {
            media_type: ProfilePictureMediaType::Png,
            bytes: ByteBuf::from(vec![0x89, b'P', b'N', b'G']),
            uploaded_at: 1,
        };
        pretty_assert_eq!(picture.to_data_url(), "data:image/png;base64,iVBORw==");
    }

    #[test]
    fn metadata_omits_the_bytes_but_reports_their_size() {
        let picture = ProfilePicture {
            media_type: ProfilePictureMediaType::Webp,
            bytes: ByteBuf::from(vec![0; 4096]),
            uploaded_at: 42,
        };
        pretty_assert_eq!(
            picture.metadata(),
            ProfilePictureMetadata {
                media_type: ProfilePictureMediaType::Webp,
                size_bytes: 4096,
                uploaded_at: 42,
            }
        );
    }
}
