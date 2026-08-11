//! Profile pictures — an identity's zero-or-one shareable avatar.
//!
//! Parallel to `verified_emails` in role (user-owned shareable info, offered
//! on the manage screen and in the continue-to-app consent flow) but not in
//! storage: the picture lives in its own stable map keyed by anchor number
//! rather than on the anchor record, because it is up to 100 KB and the
//! anchor is read on every authenticated call. See
//! [`crate::storage::PROFILE_PICTURE_MEMORY_ID`].
//!
//! The bytes are never trusted: [`validate_profile_picture`] bounds their size
//! and derives the media type from their magic number, so the `data:` URL a
//! relying party receives can only ever claim a format we actually
//! recognised.

use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::profile_picture::StorableProfilePicture;
use internet_identity_interface::archive::types::Operation;
use internet_identity_interface::internet_identity::types::profile_picture::{
    validate_profile_picture, ProfilePicture, ProfilePictureError, ProfilePictureMetadata,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};
use serde_bytes::ByteBuf;

/// `anchor_number`'s picture, or `None` when it has none.
pub fn get(anchor_number: AnchorNumber) -> Option<ProfilePicture> {
    storage_borrow(|storage| storage.lookup_profile_picture(anchor_number))
        .as_ref()
        .and_then(StorableProfilePicture::to_profile_picture)
}

/// The cheap summary `identity_info` reports, without the bytes.
pub fn get_metadata(anchor_number: AnchorNumber) -> Option<ProfilePictureMetadata> {
    get(anchor_number).map(|picture| picture.metadata())
}

/// Validate `bytes` and store them as `anchor_number`'s picture, replacing any
/// previous one.
///
/// Returns the operation to archive on success.
pub fn set(
    anchor_number: AnchorNumber,
    bytes: ByteBuf,
    now_ns: Timestamp,
) -> Result<Operation, ProfilePictureError> {
    let picture = validate_profile_picture(bytes, now_ns)?;

    storage_borrow_mut(|storage| {
        storage.write_profile_picture(anchor_number, StorableProfilePicture::from(picture))
    });

    Ok(Operation::SetProfilePicture)
}

/// Drop `anchor_number`'s picture.
///
/// [`ProfilePictureError::NotSet`] when there was none, so a double-remove is
/// reported rather than silently succeeding — mirroring
/// `verified_emails::remove`'s `NotRegistered`.
pub fn remove(anchor_number: AnchorNumber) -> Result<Operation, ProfilePictureError> {
    let removed = storage_borrow_mut(|storage| storage.remove_profile_picture(anchor_number));
    match removed {
        Some(_) => Ok(Operation::RemoveProfilePicture),
        None => Err(ProfilePictureError::NotSet),
    }
}
