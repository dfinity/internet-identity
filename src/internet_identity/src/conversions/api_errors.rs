use crate::authz_utils::IdentityUpdateError;
use crate::storage::anchor::AnchorError;
use crate::storage::StorageError;
use internet_identity_interface::internet_identity::types::IdentityMetadataReplaceError;

impl From<IdentityUpdateError> for IdentityMetadataReplaceError {
    fn from(value: IdentityUpdateError) -> Self {
        let storage_err = match value {
            IdentityUpdateError::Unauthorized(principal) => {
                return IdentityMetadataReplaceError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(_, storage_err) => storage_err,
        };

        match storage_err {
            StorageError::EntrySizeLimitExceeded {
                space_available,
                space_required,
            } => IdentityMetadataReplaceError::StorageSpaceExceeded {
                space_available,
                space_required,
            },
            err => IdentityMetadataReplaceError::InternalCanisterError(err.to_string()),
        }
    }
}

impl From<AnchorError> for IdentityMetadataReplaceError {
    fn from(value: AnchorError) -> Self {
        match value {
            AnchorError::CumulativeDataLimitExceeded { limit, length } => {
                IdentityMetadataReplaceError::StorageSpaceExceeded {
                    space_available: limit as u64,
                    space_required: length as u64,
                }
            }
            err => IdentityMetadataReplaceError::InternalCanisterError(err.to_string()),
        }
    }
}
