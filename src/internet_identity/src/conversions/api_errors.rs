use crate::authz_utils::{AuthorizationError, IdentityUpdateError};
use crate::storage::anchor::AnchorError;
use crate::storage::StorageError;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, PrepareIdAliasError,
};
use internet_identity_interface::internet_identity::types::{
    IdentityInfoError, IdentityMetadataReplaceError, IdentityPropertiesReplaceError,
};

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

impl From<IdentityUpdateError> for PrepareIdAliasError {
    fn from(value: IdentityUpdateError) -> Self {
        match value {
            IdentityUpdateError::Unauthorized(principal) => {
                PrepareIdAliasError::Unauthorized(principal)
            }
            err => PrepareIdAliasError::InternalCanisterError(err.to_string()),
        }
    }
}

impl From<IdentityUpdateError> for IdentityInfoError {
    fn from(value: IdentityUpdateError) -> Self {
        match value {
            IdentityUpdateError::Unauthorized(principal) => {
                IdentityInfoError::Unauthorized(principal)
            }
            err => IdentityInfoError::InternalCanisterError(err.to_string()),
        }
    }
}

impl From<IdentityUpdateError> for IdentityPropertiesReplaceError {
    fn from(value: IdentityUpdateError) -> Self {
        let storage_err = match value {
            IdentityUpdateError::Unauthorized(principal) => {
                return IdentityPropertiesReplaceError::Unauthorized(principal)
            }
            IdentityUpdateError::StorageError(_, storage_err) => storage_err,
        };

        match storage_err {
            StorageError::EntrySizeLimitExceeded {
                space_available,
                space_required,
            } => IdentityPropertiesReplaceError::StorageSpaceExceeded {
                space_available,
                space_required,
            },
            err => IdentityPropertiesReplaceError::InternalCanisterError(err.to_string()),
        }
    }
}

impl From<AnchorError> for IdentityPropertiesReplaceError {
    fn from(value: AnchorError) -> Self {
        match value {
            AnchorError::NameTooLong { limit } => IdentityPropertiesReplaceError::NameTooLong {
                limit: limit.try_into().unwrap(),
            },
            err => IdentityPropertiesReplaceError::InternalCanisterError(err.to_string()),
        }
    }
}

impl From<AuthorizationError> for GetIdAliasError {
    fn from(value: AuthorizationError) -> Self {
        Self::Unauthorized(value.principal)
    }
}
