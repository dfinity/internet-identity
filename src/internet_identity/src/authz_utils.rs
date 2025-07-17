use crate::anchor_management::post_operation_bookkeeping;
use crate::ii_domain::IIDomain;
use crate::storage::anchor::Anchor;
use crate::storage::StorageError;
use crate::{anchor_management, state};
use candid::Principal;
use ic_cdk::caller;
use internet_identity_interface::archive::types::Operation;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, AuthorizationKey, IdentityNumber,
};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum IdentityUpdateError {
    Unauthorized(Principal),
    StorageError(IdentityNumber, StorageError),
}

#[derive(Debug)]
pub struct AuthorizationError {
    pub principal: Principal,
}

impl From<Principal> for AuthorizationError {
    fn from(principal: Principal) -> Self {
        Self { principal }
    }
}

impl From<AuthorizationError> for IdentityUpdateError {
    fn from(err: AuthorizationError) -> Self {
        IdentityUpdateError::Unauthorized(err.principal)
    }
}

impl Display for IdentityUpdateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentityUpdateError::Unauthorized(principal) => {
                // This error message is used by the legacy API and should not be changed, even though
                // it is confusing authentication with authorization.
                f.write_str(&format!("{principal} could not be authenticated."))
            }
            IdentityUpdateError::StorageError(identity_nr, err) => {
                // This error message is used by the legacy API and should not be changed
                f.write_str(&format!(
                    "unable to update anchor {identity_nr} in stable memory: {err}"
                ))
            }
        }
    }
}

impl From<IdentityUpdateError> for String {
    fn from(err: IdentityUpdateError) -> Self {
        format!("{err}")
    }
}

/// Checks the authorization of the caller for the given anchor and calls the provided function, if successful.
/// Handles all the necessary bookkeeping for anchor operations.
/// Returns an error if the caller is not authorized or the anchor cannot be written to stable memory.
///
/// * anchor_number: indicates the anchor to be provided op should be called on
/// * op: Function that modifies an anchor and returns a [Result] indicating
///   success or failure which determines whether additional bookkeeping (on success) is required.
///
///  On success, the function must also return an [Operation] which is used for archiving purposes.
pub fn anchor_operation_with_authz_check<R, E>(
    anchor_number: AnchorNumber,
    op: impl FnOnce(&mut Anchor) -> Result<(R, Operation), E>,
) -> Result<R, E>
where
    E: From<IdentityUpdateError>,
{
    let (mut anchor, authorization_key) = check_authorization(anchor_number)
        .map_err(|err| E::from(IdentityUpdateError::from(err)))?;

    anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);

    let result = op(&mut anchor);

    // write back anchor
    state::storage_borrow_mut(|storage| storage.update(anchor))
        .map_err(|err| E::from(IdentityUpdateError::StorageError(anchor_number, err)))?;

    match result {
        Ok((ret, operation)) => {
            post_operation_bookkeeping(anchor_number, operation);
            Ok(ret)
        }
        Err(err) => Err(err),
    }
}

/// Checks if the caller is authorized to operate on the anchor provided and returns a reference to the public key of the authentication method used (device or openid).
/// Returns an error if the caller is not authorized.
pub fn check_authorization(
    anchor_number: AnchorNumber,
) -> Result<(Anchor, AuthorizationKey), AuthorizationError> {
    let anchor = state::anchor(anchor_number);
    let caller = caller();

    // First check device (passkey or recovery) authorization
    for device in anchor.devices() {
        if caller == Principal::self_authenticating(&device.pubkey)
            || state::with_temp_keys_mut(|temp_keys| {
                temp_keys
                    .check_temp_key(&caller, &device.pubkey, anchor_number)
                    .is_ok()
            })
        {
            return Ok((
                anchor.clone(),
                AuthorizationKey::DeviceKey(device.pubkey.clone()),
            ));
        }
    }
    // Else check OpenID authorization
    for credential in anchor.openid_credentials() {
        if caller == credential.principal(anchor_number) {
            return Ok((
                anchor.clone(),
                AuthorizationKey::OpenIdCredentialKey(credential.key()),
            ));
        }
    }

    Err(AuthorizationError::from(caller))
}

/// Checks that the caller is authorized to operate on the given anchor_number and updates the authorization method used to
/// reflect the current activity.
/// Also updates the aggregated stats on daily and monthly active users.
/// Returns an error if the caller is not authorized or the anchor cannot be written to stable memory.
///
/// Note: this function reads / writes the anchor from / to stable memory. It is intended to be used by functions that
/// do not further modify the anchor.
pub fn check_authz_and_record_activity(
    anchor_number: AnchorNumber,
) -> Result<Option<IIDomain>, IdentityUpdateError> {
    let (mut anchor, authorization_key) =
        check_authorization(anchor_number).map_err(IdentityUpdateError::from)?;

    let maybe_domain = match &authorization_key {
        AuthorizationKey::DeviceKey(device_key) => anchor.device(device_key).unwrap().ii_domain(),
        _ => None,
    };
    anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);
    state::storage_borrow_mut(|storage| storage.update(anchor))
        .map_err(|err| IdentityUpdateError::StorageError(anchor_number, err))?;
    Ok(maybe_domain)
}
