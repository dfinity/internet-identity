use crate::anchor_management::post_operation_bookkeeping;
use crate::ii_domain::IIDomain;
use crate::storage::anchor::Anchor;
use crate::storage::StorageError;
use crate::{anchor_management, state};
use candid::Principal;
use ic_cdk::{caller, trap};
use internet_identity_interface::archive::types::Operation;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DeviceKey, IdentityNumber,
};

#[derive(Debug)]
pub enum IdentityUpdateError {
    Unauthorized(Principal),
    StorageError(IdentityNumber, StorageError),
}

/// Authenticates the caller (traps if not authenticated) calls the provided function and handles all
/// the necessary bookkeeping for anchor operations.
///
/// * anchor_number: indicates the anchor to be provided op should be called on
/// * op: Function that modifies an anchor and returns a [Result] indicating
///       success or failure which determines whether additional bookkeeping (on success) is required.
///       On success, the function must also return an [Operation] which is used for archiving purposes.
pub fn authenticated_anchor_operation<R, E>(
    anchor_number: AnchorNumber,
    op: impl FnOnce(&mut Anchor) -> Result<(R, Operation), E>,
) -> Result<R, E>
where
    E: From<IdentityUpdateError>,
{
    let Ok((mut anchor, device_key)) = check_authentication(anchor_number) else {
        return Err(E::from(IdentityUpdateError::Unauthorized(caller())));
    };
    anchor_management::activity_bookkeeping(&mut anchor, &device_key);

    let result = op(&mut anchor);

    // write back anchor
    state::storage_borrow_mut(|storage| storage.write(anchor_number, anchor))
        .map_err(|err| E::from(IdentityUpdateError::StorageError(anchor_number, err)))?;

    match result {
        Ok((ret, operation)) => {
            post_operation_bookkeeping(anchor_number, operation);
            Ok(ret)
        }
        Err(err) => Err(err),
    }
}

/// Checks if the caller is authenticated against the anchor provided and returns a reference to the device used.
/// Returns an error if the caller cannot be authenticated.
pub fn check_authentication(anchor_number: AnchorNumber) -> Result<(Anchor, DeviceKey), ()> {
    let anchor = state::anchor(anchor_number);
    let caller = caller();

    for device in anchor.devices() {
        if caller == Principal::self_authenticating(&device.pubkey)
            || state::with_temp_keys_mut(|temp_keys| {
                temp_keys
                    .check_temp_key(&caller, &device.pubkey, anchor_number)
                    .is_ok()
            })
        {
            return Ok((anchor.clone(), device.pubkey.clone()));
        }
    }
    Err(())
}

/// Authenticates the caller (traps if not authenticated) and updates the device used to authenticate
/// reflecting the current activity. Also updates the aggregated stats on daily and monthly active users.
///
/// Note: this function reads / writes the anchor from / to stable memory. It is intended to be used by functions that
/// do not further modify the anchor.
pub fn authenticate_and_record_activity(anchor_number: AnchorNumber) -> Option<IIDomain> {
    let Ok((mut anchor, device_key)) = check_authentication(anchor_number) else {
        trap(&format!("{} could not be authenticated.", caller()));
    };
    let domain = anchor.device(&device_key).unwrap().ii_domain();
    anchor_management::activity_bookkeeping(&mut anchor, &device_key);
    state::storage_borrow_mut(|storage| storage.write(anchor_number, anchor)).unwrap_or_else(
        |err| panic!("last_usage_timestamp update: unable to update anchor {anchor_number}: {err}"),
    );
    domain
}

impl From<IdentityUpdateError> for String {
    fn from(err: IdentityUpdateError) -> Self {
        match err {
            IdentityUpdateError::Unauthorized(principal) => {
                // This error message is used by the legacy API and should not be changed, even though
                // it is confusing authentication with authorization.
                format!("{} could not be authenticated.", principal)
            }
            IdentityUpdateError::StorageError(identity_nr, err) => {
                // This error message is used by the legacy API and should not be changed
                format!(
                    "unable to update anchor {} in stable memory: {}",
                    identity_nr, err
                )
            }
        }
    }
}
