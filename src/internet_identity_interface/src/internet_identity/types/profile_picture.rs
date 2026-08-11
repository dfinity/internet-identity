//! Profile pictures — the one piece of shareable identity info that is
//! binary rather than textual.
//!
//! An identity carries zero or one picture, always WebP. It is stored raw and
//! rendered into a `data:` URL at certification time, which is what a relying
//! party receives for the `profile_picture` attribute key. See
//! [`crate::internet_identity::types::attributes`] for the attribute plumbing,
//! and the `profile_picture_*` section of `internet_identity.did` — which
//! `docs/ii-spec.mdx` embeds verbatim — for the caller-facing contract.
//!
//! WebP is the only stored format because it subsumes the alternatives: it
//! encodes both lossily and losslessly, and carries alpha in either mode, so
//! it covers everything PNG and JPEG would have been kept around for while
//! being smaller than both at equivalent quality — which buys real picture
//! quality under a 100 KiB cap. One format also means the media type is a
//! constant rather than something to record, derive and keep in sync.
//!
//! Clients may still let a user pick a PNG or a JPEG; converting is the
//! client's job, and the frontend's `prepareProfilePicture` does exactly that.

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

/// Lower bound, purely a sanity check: a WebP header alone is 16 bytes (RIFF
/// magic, size, `WEBP`, and the first chunk's FourCC), so anything shorter is
/// a truncated upload rather than a picture.
pub const PROFILE_PICTURE_MIN_BYTES: usize = 16;

/// The IANA media type of every stored picture, as it appears in the `data:`
/// URL handed to relying parties.
pub const PROFILE_PICTURE_MEDIA_TYPE: &str = "image/webp";

/// Whether `bytes` are a WebP image, by their container header.
///
/// A WebP file is a RIFF container — `RIFF`, a little-endian u32 size, `WEBP`
/// — followed by a chunk whose FourCC says which coding was used: `VP8 `
/// lossy, `VP8L` lossless, `VP8X` extended (the form that carries alpha or
/// animation). All three are accepted: the point of standardising on WebP is
/// that one format covers both compressed and uncompressed pictures.
///
/// This is a format check, not a validity check. It establishes that the bytes
/// are the format we claim when we label them, and rejects anything that isn't
/// an image at all (an HTML page, an SVG with a script in it, a PDF) — which
/// matters because the label ends up in a `data:` URL that relying parties
/// feed to an `<img>` tag. We deliberately do not decode the image: a canister
/// has no business running an image parser over user input, and the consumer's
/// `<img>` tag is what ultimately decides whether the pixels are renderable.
pub fn is_webp(bytes: &[u8]) -> bool {
    const HEADER_BYTES: usize = 16;
    const CHUNK_FOURCCS: &[&[u8; 4]] = &[b"VP8 ", b"VP8L", b"VP8X"];

    if bytes.len() < HEADER_BYTES {
        return false;
    }
    if !bytes.starts_with(b"RIFF") || &bytes[8..12] != b"WEBP" {
        return false;
    }
    CHUNK_FOURCCS
        .iter()
        .any(|fourcc| &bytes[12..16] == fourcc.as_slice())
}

/// An identity's stored picture, as it crosses the Candid boundary.
///
/// Carries no media type: every picture is WebP
/// ([`PROFILE_PICTURE_MEDIA_TYPE`]). Should a second format ever be
/// supported, the field to add is a `media_type` on this record and a fresh
/// CBOR key on the storable — both additive.
#[derive(Clone, Debug, PartialEq, Eq, CandidType, Serialize, Deserialize)]
pub struct ProfilePicture {
    /// The raw WebP image, at most [`PROFILE_PICTURE_MAX_BYTES`].
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
            PROFILE_PICTURE_MEDIA_TYPE,
            BASE64.encode(self.bytes.as_ref())
        )
    }

    /// The cheap summary the manage UI lists without pulling the bytes down.
    pub fn metadata(&self) -> ProfilePictureMetadata {
        ProfilePictureMetadata {
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
    pub size_bytes: u64,
    pub uploaded_at: Timestamp,
}

/// What the caller sends to `profile_picture_set`.
///
/// Only the bytes: the format is checked rather than declared, and the
/// timestamp is the canister's.
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
    /// The bytes are too short to be a WebP image.
    TooSmall {
        size_bytes: u64,
        min_bytes: u64,
    },
    /// The bytes are not a WebP image. Clients converting from another format
    /// should encode to WebP before calling.
    NotWebp,
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
    if !is_webp(bytes.as_ref()) {
        return Err(ProfilePictureError::NotWebp);
    }

    Ok(ProfilePicture {
        bytes,
        uploaded_at: now_ns,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq as pretty_assert_eq;

    /// A WebP header with the given chunk FourCC, padded to `len`.
    fn webp_with(fourcc: &[u8; 4], len: usize) -> ByteBuf {
        let mut bytes = b"RIFF".to_vec();
        bytes.extend_from_slice(&[0, 0, 0, 0]);
        bytes.extend_from_slice(b"WEBP");
        bytes.extend_from_slice(fourcc);
        bytes.resize(len.max(bytes.len()), 0);
        ByteBuf::from(bytes)
    }

    fn webp(len: usize) -> ByteBuf {
        webp_with(b"VP8 ", len)
    }

    /// Lossy, lossless and extended (alpha/animation) WebP must all be
    /// accepted — one format covering both compressed and uncompressed
    /// pictures is the whole reason for standardising on it.
    #[test]
    fn accepts_every_webp_coding() {
        for fourcc in [b"VP8 ", b"VP8L", b"VP8X"] {
            let bytes = webp_with(fourcc, 64);
            assert!(
                is_webp(bytes.as_ref()),
                "should have accepted chunk {:?}",
                std::str::from_utf8(fourcc).unwrap()
            );
        }
    }

    #[test]
    fn rejects_bytes_that_are_not_webp() {
        let png = {
            let mut b = vec![0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a];
            b.resize(64, 0);
            b
        };
        let jpeg = {
            let mut b = vec![0xff, 0xd8, 0xff, 0xe0];
            b.resize(64, 0);
            b
        };

        let test_cases: Vec<(&str, Vec<u8>)> = vec![
            ("empty", vec![]),
            // PNG and JPEG are no longer stored — a client must convert.
            ("png", png),
            ("jpeg", jpeg),
            (
                "html",
                b"<!DOCTYPE html><html><body>hi</body></html>".to_vec(),
            ),
            // SVG is a script-execution vector in an `<img>` tag on some
            // consumers; it must never sniff as an image.
            (
                "svg",
                b"<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_vec(),
            ),
            ("pdf", b"%PDF-1.7\n".to_vec()),
            ("gif", b"GIF89a\0\0\0\0\0\0\0\0\0\0".to_vec()),
            // A RIFF container that isn't WebP at all (e.g. a WAV file).
            ("riff but not webp", b"RIFF\0\0\0\0WAVEfmt ".to_vec()),
            // RIFF/WEBP but an unknown coding chunk — not a WebP we can vouch
            // for, so not something to label as one.
            (
                "riff webp with unknown chunk",
                webp_with(b"XXXX", 64).into_vec(),
            ),
            ("truncated webp header", b"RIFF\0\0\0\0WEBP".to_vec()),
        ];

        for (label, bytes) in test_cases {
            assert!(
                !is_webp(&bytes),
                "should not have accepted as WebP: {}",
                label
            );
        }
    }

    #[test]
    fn validates_size_bounds() {
        let now = 1_700_000_000_000_000_000;

        assert!(
            validate_profile_picture(webp(PROFILE_PICTURE_MAX_BYTES), now).is_ok(),
            "a picture of exactly the maximum size must be accepted"
        );

        pretty_assert_eq!(
            validate_profile_picture(webp(PROFILE_PICTURE_MAX_BYTES + 1), now),
            Err(ProfilePictureError::TooLarge {
                size_bytes: PROFILE_PICTURE_MAX_BYTES as u64 + 1,
                max_bytes: PROFILE_PICTURE_MAX_BYTES as u64,
            })
        );

        // Short enough to be a truncated upload. The size check must fire
        // before the format check, so this is not reported as `NotWebp`.
        pretty_assert_eq!(
            validate_profile_picture(ByteBuf::from(b"RIFF\0\0\0\0WEBP".to_vec()), now),
            Err(ProfilePictureError::TooSmall {
                size_bytes: 12,
                min_bytes: PROFILE_PICTURE_MIN_BYTES as u64,
            })
        );
    }

    #[test]
    fn validate_rejects_a_well_sized_non_webp() {
        let mut png = vec![0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a];
        png.resize(4096, 0);
        pretty_assert_eq!(
            validate_profile_picture(ByteBuf::from(png), 1),
            Err(ProfilePictureError::NotWebp)
        );
    }

    #[test]
    fn validate_stamps_the_timestamp() {
        let now = 1_700_000_000_000_000_000;
        let picture = validate_profile_picture(webp(64), now).expect("a WebP must validate");
        pretty_assert_eq!(picture.uploaded_at, now);
        pretty_assert_eq!(picture.bytes.len(), 64);
    }

    #[test]
    fn data_url_is_self_describing_and_base64() {
        let picture = ProfilePicture {
            bytes: ByteBuf::from(b"RIFF".to_vec()),
            uploaded_at: 1,
        };
        pretty_assert_eq!(picture.to_data_url(), "data:image/webp;base64,UklGRg==");
    }

    #[test]
    fn metadata_omits_the_bytes_but_reports_their_size() {
        let picture = ProfilePicture {
            bytes: ByteBuf::from(vec![0; 4096]),
            uploaded_at: 42,
        };
        pretty_assert_eq!(
            picture.metadata(),
            ProfilePictureMetadata {
                size_bytes: 4096,
                uploaded_at: 42,
            }
        );
    }
}
